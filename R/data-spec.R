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

cubist_data_spec <- function(x, label = "outcome", has_weights = FALSE) {
  if (!is.character(label) || length(label) != 1 || is.na(label)) {
    stop("`label` must be a single character string", call. = FALSE)
  }
  check_column_names(x, label, has_weights)

  is_discrete <- vapply(x, is_discrete_column, logical(1))
  is_continuous <- vapply(x, is.numeric, logical(1))
  bad <- !is_discrete & !is_continuous
  if (any(bad)) {
    bad_cols <- names(x)[bad]
    bad_types <- vapply(x[bad], function(col) class(col)[1], character(1))
    stop(
      "Predictors must be numeric, factor, or character. ",
      "Unsupported column(s): ",
      paste0("'", bad_cols, "' (", bad_types, ")", collapse = ", "),
      call. = FALSE
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

cubist_data_columns <- function(x, spec, new_data = FALSE) {
  if (!identical(names(x), spec$names)) {
    stop(
      "internal error: columns do not match the attribute spec",
      call. = FALSE
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
      if (!is.numeric(col)) {
        stop(
          "Column '",
          spec$names[j],
          "' was numeric in the training set but is ",
          class(col)[1],
          " in the new data",
          call. = FALSE
        )
      }
      check_finite(col, paste0("Column '", spec$names[j], "'"))
      cols[[j]] <- as.double(col)
    }
  }
  if (length(unseen) > 0) {
    if (!new_data) {
      stop(
        "internal error: training data has values that are not in the spec",
        call. = FALSE
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
# catch these here. Missing values (NA, NaN) are allowed.
check_finite <- function(x, what) {
  if (any(is.infinite(x))) {
    stop(
      what,
      " has infinite values; use NA for missing values instead",
      call. = FALSE
    )
  }
  invisible(NULL)
}

check_column_names <- function(x, label, has_weights) {
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    stop("All columns of `x` must have non-empty names", call. = FALSE)
  }
  dups <- unique(nms[duplicated(nms)])
  if (length(dups) > 0) {
    stop(
      "Column names of `x` must be unique. Duplicated: ",
      paste0("'", dups, "'", collapse = ", "),
      call. = FALSE
    )
  }
  if (label %in% nms) {
    stop(
      "The outcome label '",
      label,
      "' is also a column name in `x`. ",
      "Use `cubistControl(label = )` to choose a different label.",
      call. = FALSE
    )
  }
  if (has_weights && "case weight" %in% nms) {
    stop(
      "'case weight' is reserved for the case weights and cannot be ",
      "a column name in `x` when `weights` are used",
      call. = FALSE
    )
  }
  invisible(NULL)
}

warn_unseen_levels <- function(unseen) {
  details <- vapply(
    names(unseen),
    function(nm) {
      paste0("'", nm, "': ", paste0("'", unseen[[nm]], "'", collapse = ", "))
    },
    character(1)
  )
  warning(
    "`newdata` has values that were not in the training set; ",
    "they are treated as missing:\n",
    paste0("  ", details, collapse = "\n"),
    call. = FALSE
  )
}
