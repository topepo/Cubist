# Direct data transfer to the C code
#
# Cubist was written as a command-line program that reads a `.names` file
# (attribute definitions) and a `.data` file (one comma-separated line per
# case). Versions up to 0.6.0 formatted R data into those two text formats
# and the C code parsed them back. That round trip needs escaping, loses
# whitespace in values, gives special meaning to values such as "?" and
# "N/A", and reserves some column names.
#
# Now R passes plain vectors to `C_cubist_fit()` and `C_cubist_predict()`
# (src/top.c), and src/rdata.c fills the C data structures directly:
#
# * `cubist_data_spec()` describes the attributes once, at fit time. The
#   spec is stored in the fitted object and reused for prediction.
# * `cubist_data_columns()` converts a data frame to the list of columns
#   that the C code expects: doubles for numeric columns and integer codes
#   (1..k, matching `spec$levels`) for factor and character columns.

cubist_data_spec <- function(
  x,
  label = "outcome",
  has_weights = FALSE,
  call = caller_env()
) {
  check_string(label, call = call)
  check_column_names(x, label, has_weights, call = call)

  is_discrete <- vapply(x, is_discrete_column, logical(1))
  is_continuous <- vapply(x, is.numeric, logical(1))
  bad <- !is_discrete & !is_continuous
  if (any(bad)) {
    bad_cols <- names(x)[bad]
    bad_types <- vapply(x[bad], function(col) class(col)[1], character(1))
    bad_desc <- paste0(bad_cols, " (", bad_types, ")")
    cli_abort(
      c(
        "Predictors must be numeric, factor, or character.",
        "x" = "Unsupported column{?s}: {.field {bad_desc}}."
      ),
      call = call
    )
  }

  lvls <- lapply(x, column_levels)
  ordered <- vapply(x, is.ordered, logical(1))

  list(
    names = names(x),
    discrete = unname(is_discrete),
    ordered = unname(ordered),
    levels = unname(lvls),
    target = label,
    has_weights = has_weights
  )
}

cubist_data_columns <- function(
  x,
  spec,
  new_data = FALSE,
  call = caller_env()
) {
  if (!identical(names(x), spec$names)) {
    cli_abort(
      "The columns do not match the attribute spec.",
      .internal = TRUE
    )
  }
  unseen <- list()
  cols <- vector("list", length(spec$names))
  for (j in seq_along(spec$names)) {
    col <- x[[j]]
    if (spec$discrete[j]) {
      values <- as.character(col)
      codes <- match(values, spec$levels[[j]])
      new_values <- !is.na(values) & is.na(codes)
      if (any(new_values)) {
        unseen[[spec$names[j]]] <- unique(values[new_values])
      }
      cols[[j]] <- as.integer(codes)
    } else {
      if (new_data) {
        col <- coerce_new_numeric(col)
      }
      col_name <- spec$names[j]
      if (!is.numeric(col)) {
        col_class <- class(col)[1]
        cli_abort(
          "Column {.field {col_name}} was numeric in the training set but is
           {.cls {col_class}} in the new data.",
          call = call
        )
      }
      if (any(is.infinite(col))) {
        cli_abort(
          c(
            "Column {.field {col_name}} has infinite values.",
            "i" = "Use {.code NA} for missing values instead."
          ),
          call = call
        )
      }
      cols[[j]] <- as.double(col)
    }
  }
  if (length(unseen) > 0) {
    if (!new_data) {
      cli_abort(
        "The training set has values that are not in the attribute spec.",
        .internal = TRUE
      )
    }
    warn_unseen_levels(unseen)
  }
  cols
}

# Rebuild a data frame from coded columns, e.g. to write the original
# `.data` file format in `exportCubistFiles()`
cubist_data_decode <- function(cols, spec) {
  out <- vector("list", length(spec$names))
  for (j in seq_along(spec$names)) {
    if (spec$discrete[j]) {
      out[[j]] <- factor(
        spec$levels[[j]][cols[[j]]],
        levels = spec$levels[[j]],
        ordered = spec$ordered[j]
      )
    } else {
      out[[j]] <- cols[[j]]
    }
  }
  names(out) <- spec$names
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

# Write a `.names` file for a fitted model.
#
# With `escape_values = TRUE` (the default), names and values are escaped as
# the command-line version of Cubist requires; `exportCubistFiles()` uses this.
#
# With `escape_values = FALSE`, the text has the same layout as the `names`
# element of fitted objects from Cubist 0.6.0 and earlier: predictor names
# are escaped, but the outcome label and the factor levels are not. The C code
# does not use this text any more, but `cubist.default()` still stores it as
# `names` because other packages parse it to get the predictor types and
# levels (e.g. `tidyrules:::varSpec()`, which removes the backslashes from
# the names only, and finds the outcome with the pattern "^outcome:").
cubist_names_string <- function(spec, comments = TRUE, escape_values = TRUE) {
  escape_value <- identity
  if (escape_values) {
    escape_value <- escapes
  }
  header <- ""
  if (comments) {
    header <- paste0(
      "| Generated using ",
      R.version.string,
      "\n",
      "| on ",
      format(Sys.time(), "%a %b %d %H:%M:%S %Y")
    )
  }
  types <- rep("continuous.", length(spec$names))
  for (j in which(spec$discrete)) {
    prefix <- ""
    if (spec$ordered[j]) {
      prefix <- "[ordered]"
    }
    types[j] <- paste0(
      prefix,
      paste(escape_value(spec$levels[[j]]), collapse = ","),
      "."
    )
  }
  att_lines <- paste0(escapes(spec$names), ": ", types)
  if (spec$has_weights) {
    att_lines <- c(att_lines, "case weight: continuous.")
  }
  target <- escape_value(spec$target)
  paste0(
    header,
    "\n",
    target,
    ".\n\n",
    target,
    ": continuous.\n",
    paste(att_lines, collapse = "\n"),
    "\n"
  )
}

# Call `C_cubist_fit()` or `C_cubist_predict()` and turn their failures into
# cli errors:
#
# * The C wrappers check their inputs with `Rf_error()`. The R code above
#   prepares those inputs, so a failure there is a bug in this package.
# * When the Cubist code itself stops (e.g. "No cases with known target
#   values"), it writes the reason to its output and returns a non-zero
#   `status`. Without this check, a fit would continue with an empty model
#   and fail later with an unrelated error, and predictions would all be `NA`.
#
# `expr` is the `.Call()` itself (rather than the routine name and arguments)
# so that R CMD check can see which registered routine is called. It is
# evaluated lazily, inside `try_fetch()`.
call_cubist <- function(expr, call = caller_env()) {
  res <- try_fetch(
    expr,
    error = function(cnd) {
      # the call would be `withCallingHandlers()` from `try_fetch()`
      cnd$call <- NULL
      cli_abort(
        "The data passed to the C code are not valid.",
        parent = cnd,
        .internal = TRUE,
        call = call
      )
    }
  )
  if (res$status != 0) {
    reasons <- cubist_error_lines(res$output)
    cli_abort(
      c("The Cubist C code stopped with an error.", reasons),
      call = call
    )
  }
  res
}

# The Cubist code starts its error and warning lines with "***"
cubist_error_lines <- function(output) {
  lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines[grepl("^\\s*\\*\\*\\*", lines)])
  lines <- trimws(sub("^\\*\\*\\*", "", lines))
  if (length(lines) == 0) {
    return(character(0))
  }
  # The lines can contain column names and values; double any braces so that
  # cli prints them as is
  lines <- gsub("([{}])", "\\1\\1", lines)
  names(lines) <- rep("x", length(lines))
  lines
}

# ------------------------------------------------------------------------------

is_discrete_column <- function(col) {
  is.factor(col) || is.character(col)
}

column_levels <- function(col) {
  if (is.factor(col)) {
    return(levels(col))
  }
  if (is.character(col)) {
    # radix sorting is locale independent, so the level order (and thus the
    # model) is the same on every platform
    return(sort(unique(col[!is.na(col)]), method = "radix"))
  }
  NULL
}

# Prepare a column of `newdata` for a predictor that was numeric in the
# training set. Up to version 0.6.0, all of the data were written to text, so
# any column class was accepted, and two cases are still used in practice:
#
# * A column that has only missing values. `data.frame(x = NA)` makes this a
#   logical column, and other packages (e.g. dendroTools) predict on such
#   columns. Treat it as missing whatever its class, as before.
# * A logical column with values. Use 0/1 as R does elsewhere (e.g.
#   `model.matrix()`). Before, the C code could not read "TRUE"/"FALSE" as
#   numbers and gave wrong predictions without a warning.
#
# Other classes (e.g. character) are left alone, so the caller gives an error.
coerce_new_numeric <- function(col) {
  if (all(is.na(col))) {
    return(rep(NA_real_, length(col)))
  }
  if (is.logical(col)) {
    return(as.double(col))
  }
  col
}

# The C code stops (and returns no model) when it sees an infinite value, so
# catch these here (predictor columns are checked in `cubist_data_columns()`).
# Missing values (NA, NaN) are allowed.
check_finite <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (any(is.infinite(x))) {
    cli_abort(
      c(
        "{.arg {arg}} has infinite values.",
        "i" = "Use {.code NA} for missing values instead."
      ),
      call = call
    )
  }
  invisible(NULL)
}

check_column_names <- function(x, label, has_weights, call = caller_env()) {
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    cli_abort("All columns of {.arg x} must have non-empty names.", call = call)
  }
  dups <- unique(nms[duplicated(nms)])
  if (length(dups) > 0) {
    cli_abort(
      c(
        "Column names of {.arg x} must be unique.",
        "x" = "Duplicated: {.field {dups}}."
      ),
      call = call
    )
  }
  if (label %in% nms) {
    cli_abort(
      c(
        "The outcome label {.val {label}} is also a column name in {.arg x}.",
        "i" = "Use {.code cubistControl(label = )} to choose a different
               label."
      ),
      call = call
    )
  }
  if (has_weights && "case weight" %in% nms) {
    cli_abort(
      "{.val case weight} is reserved for the case weights and cannot be a
       column name in {.arg x} when {.arg weights} are used.",
      call = call
    )
  }
  invisible(NULL)
}

warn_unseen_levels <- function(unseen) {
  details <- vapply(
    names(unseen),
    function(nm) {
      cli::format_inline("{.field {nm}}: {.val {unseen[[nm]]}}")
    },
    character(1)
  )
  # The details are already formatted; double any braces in the values so
  # that cli does not interpolate them a second time
  details <- gsub("([{}])", "\\1\\1", details)
  names(details) <- rep("*", length(details))
  cli_warn(
    c(
      "{.arg newdata} has values that were not in the training set; they are
       treated as missing:",
      details
    )
  )
}
