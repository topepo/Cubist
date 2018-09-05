#' @name tidy_rules
#' @title Obtain rules as a tidy tibble
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @return A tibble where each row corresponds to a rule
#' @export
tidy_rules <- function(object, ...){
  
  UseMethod("tidy_rules", object)
  
}

#' @name tidy_rules.cubist
#' @title Obtain rules as a tidy tibble from a cubist model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @return A tibble where each row corresponds to a rule. The columns are:
#'   support, mean, min, max, error, lhs, rhs and committee
#' @examples
#' data("attrition", package = "rsample")
#' attrition <- tibble::as_tibble(attrition)
#' # lets predict monthly income
#' cubist_model <- 
#'   Cubist::cubist(
#'     x   = attrition %>% dplyr::select(-MonthlyIncome, -Attrition)
#'     , y = attrition %>% dplyr::select(MonthlyIncome) %>% unlist()
#'     )
#' summary(cubist_model)
#' tidy_rules(cubist_model)
#' 
#' cubist_model_commitees <- 
#'   Cubist::cubist(x   = attrition %>% dplyr::select(-MonthlyIncome, -Attrition)
#'                  , y = attrition %>% dplyr::select(MonthlyIncome) %>% unlist()
#'                  , committees = 7
#'                  )
#' summary(cubist_model_commitees)
#' tidy_rules(cubist_model_commitees)
#' 
#' # column names with spaces are handled with adding '`' quotes to it in the rules
#' ames <- AmesHousing::make_ames()
#' 
#' ames2 <- 
#'   ames %>%
#'   dplyr::rename(`Gr Liv Area` = Gr_Liv_Area) %>%
#'   dplyr::rename(`Gr Liv` = Latitude) %>% 
#'   dplyr::mutate(
#'     Overall_Qual = gsub("_", " ", as.character(Overall_Qual)),
#'     MS_SubClass = gsub("_", " ", as.character(MS_SubClass))
#'     )
#' 
#' 
#' colnames(ames2)
#' 
#' cb_mod <- 
#'   cubist(
#'     x = ames2 %>% dplyr::select(-Sale_Price),
#'     y = log10(ames2$Sale_Price),
#'     committees = 3
#'     ) 
#' 
#' tr <- tidy_rules(cb_mod)
#' tr
#' tr$rhs[[1]]
#' @export
tidy_rules.cubist <- function(object, ...){
  
  remove_empty_lines <- function(strings){
    strings[!(strings == "")]
  }
  
  # get column names
  columnNames <- object[["names"]] %>% 
    stringr::str_split("\\n") %>% 
    unlist() %>% 
    utils::tail(-5) %>% 
    lapply(function(string) stringr::str_split(string, ":")[[1]][[1]]) %>% 
    unlist() %>% 
    stringr::str_replace_all("\\\\", "") %>% 
    remove_empty_lines()
  
  # handle column names with spaces
  namesWithSpace <- columnNames[(stringr::str_detect(columnNames, "\\s"))]
  
  # ordering is required because we do not want to replace smaller strings
  # ex: suppose 'hello world' and 'hello world india' are two columns
  # First replacement of 'hello world' by 'hello_world' will prevent
  # 'hello_world_india' from replacing 'hello world india'
  if(length(namesWithSpace) > 0){
    namesWithSpace  <- namesWithSpace[order(stringr::str_length(namesWithSpace)
                                            , decreasing = TRUE)]
    namesWithSpace_ <- stringr::str_replace_all(namesWithSpace, "\\s", "_")
  }
  
  # split by newline and remove emptylines
  lev_1 <- object$output %>% 
    stringr::str_split("\n") %>% 
    unlist() %>% 
    remove_empty_lines()
  
  # remove everything from 'Evaluation on training data' onwards
  evalLine <- stringr::str_detect(lev_1
                                  , "^Evaluation on training data"
                                  ) %>% 
    which()
  lev_2    <- lev_1[-(evalLine:length(lev_1))] 
  
  
  # detect starts and ends of rules
  rule_starts <- stringr::str_detect(stringr::str_trim(lev_2), "^Rule\\s") %>% 
    which()
  rule_ends   <- c(utils::tail(rule_starts, -1) - 1, length(lev_2))
  
  # create a rule list for cubist
  get_rules_cubist <- function(single_raw_rule){
    
    res <- list()
    
    # locate the position of square bracket and collect stats
    firstLine <- stringr::str_trim(single_raw_rule[1])
    openingSquareBracketPosition <- stringr::str_split(firstLine, "")[[1]] %>% 
      stringr::str_detect("\\[") %>% 
      which()
    
    stat <- stringr::str_sub(firstLine
                    , openingSquareBracketPosition + 1
                    , nchar(firstLine) - 1
                    ) %>% 
      stringr::str_split(",") %>% 
      unlist() %>% 
      stringr::str_trim()
    
    res[["support"]] <- stat[1] %>% 
      stringr::str_split(" ") %>% 
      unlist() %>% 
      `[`(1) %>% 
      as.integer()
    
    res[["mean"]] <- stat[2] %>% 
      stringr::str_split(" ") %>% 
      unlist() %>% 
      `[`(2) %>% 
      as.numeric()
    
    res[["min"]] <- stat[3] %>% 
      stringr::str_split(" ") %>% 
      unlist() %>% 
      `[`(2) %>% 
      as.numeric()
    
    res[["max"]] <- stat[3] %>% 
      strsplit(" ") %>% 
      unlist() %>% 
      `[`(4) %>% 
      as.numeric()
    
    res[["error"]] <- stat[4] %>% 
      stringr::str_split(" ") %>% 
      unlist() %>% 
      `[`(3) %>% 
      as.numeric()
    
    # get LHS
    btwIfThen <- seq(which(stringr::str_trim(single_raw_rule) == "if") + 1
                     , which(stringr::str_trim(single_raw_rule) == "then") - 1
                     )
    lhsStrings <-  single_raw_rule[btwIfThen] %>% 
      stringr::str_replace("\\t", "\\\\n") %>% 
      stringr::str_trim() %>% 
      stringr::str_c(collapse = " ") %>% 
      stringr::str_split("\\\\n") %>% 
      unlist() %>% 
      remove_empty_lines() %>% 
      stringr::str_trim()
    
    # function to get the one rule string
    getRuleString <- function(string){
      
      # if  there is ' in {' in the string
      if(stringr::str_detect(string, "\\sin\\s\\{")){
        
        # split with ' in {'
        var_lvls <- stringr::str_split(string, "\\sin\\s\\{")[[1]]
        
        # get the contents inside curly braces
        lvls <- var_lvls[2] %>% 
          stringr::str_sub(1, stringr::str_length(var_lvls[2]) - 1) %>% 
          stringr::str_split(", ") %>% 
          `[[`(1) %>% 
          stringr::str_trim() %>% 
          sapply(function(x) stringr::str_c("'", x, "'")) %>% 
          stringr::str_c(collapse = ", ")
        lvls <- stringr::str_c("c(", lvls, ")")
        
        # get the variable
        var <- var_lvls[1] %>% stringr::str_trim()
        
        rs <- stringr::str_c(var, " %in% ", lvls)  
        
      } else {
        
        rs <- string # no change as it is R parsable
        
      }
      
      return(rs)
      
    }
  
    res[["lhs"]] <- stringr::str_c(
      sapply(lhsStrings, getRuleString), collapse = " & ")
  
    # get RHS
    afterThen <- seq(which(trimws(single_raw_rule) == "then") + 1
                     , length(single_raw_rule)
                     )
    if(length(namesWithSpace) > 0){
      for(i in 1:length(namesWithSpace)){
        single_raw_rule[afterThen] <- 
          stringr::str_replace_all(single_raw_rule[afterThen]
                                   , namesWithSpace[i]
                                   , namesWithSpace_[i]
                                   )
      }
    }

    res[["rhs"]] <- single_raw_rule[afterThen] %>% 
      stringr::str_trim() %>% 
      stringr::str_c(collapse = " ") %>% 
      stringr::str_replace_all("\\s\\s+", " ") %>% 
      stringr::str_replace("outcome = ", "") %>% 
      stringr::str_replace_all("\\s\\+\\s", "+") %>% 
      stringr::str_replace_all("\\s\\-\\s", "-") %>% 
      stringr::str_replace_all("\\s", " * ") %>% 
      stringr::str_replace_all("\\+", ") + (") %>% 
      stringr::str_replace_all("\\-", ") - (")
    
    res[["rhs"]] <- stringr::str_c("(", res[["rhs"]], ")") %>% 
      stringr::str_replace("\\(\\)\\s\\-\\s\\(", "(-")
    
    if(length(namesWithSpace) > 0){
      for(i in 1:length(namesWithSpace_)){
        res[["rhs"]] <- 
          stringr::str_replace_all(res[["rhs"]]
                                   , namesWithSpace_[i]
                                   , stringr::str_c("`", namesWithSpace[i], "`")
                                   )
      }  
    }
    
    return(res)
}
  
  # see if rules have commitees
  rule_number_splits <- 
    stringr::str_split(stringr::str_trim(lev_2)[rule_starts], ":") %>% 
    vapply(function(x) x[[1]], "character") %>% 
    stringr::str_split("\\s") %>% 
    vapply(function(x) x[[2]], "character") %>% 
    stringr::str_split("/") %>% 
    simplify2array() %>% 
    as.integer()
  
  if(length(rule_number_splits) > length(rule_starts)){
    committees <- rule_number_splits[seq(1
                                         , by = 2
                                         , length.out = length(rule_starts)
                                         )]
  } else {
    committees <- rep(1L, length(rule_starts))
  }
  
  # create multiline rules
  rules_raw   <- lapply(1:length(rule_starts)
                        , function(i) lev_2[rule_starts[i]:rule_ends[i]]
                        )
  
  tidydf <- rules_raw %>% 
    lapply(get_rules_cubist) %>% 
    lapply(tibble::as_tibble) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(committee = committees)
  
  return(tidydf)
}
