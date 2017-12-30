library(plyr)
library(purrr)


####################
# Characterization #
####################

#' Vizualize and Quantify Data Frame Missingness Patterns
#'
#' Displays a cohort missingness heatmap and prints the % of missing data
#'  and complete cases
#'
#' @param df data.frame with missing data
#' @param ... additional arguments to heatmap, like \code{main} title
#'
#' @return NULL; called for side effect
#' @export
#'
#' @examples
#' DF.assess_missingness(mtcars)
DF.assess_missingness <- function(df, ..., title='Patterns of Data Missingness') {
    library(mice)
    if (any(df %>% is.na)){
        options(stringsAsFactors = TRUE)
        df %<>% data.frame
        df %<>% DF.chrs2factors()
        md_pat <- md.pattern(df)
        heatmap(t(md_pat[1:(nrow(md_pat)-1), 1:(ncol(md_pat)-1)]),
                Rowv = NA, Colv = NA, scale = "none",
                margins = c(10, 10), xlab = 'cohorts (sizes)', ylab='features',
                ..., main=title)
        .DF.perct_missing(df)
        message(nrow(df %>% na.omit), ' of ', nrow(df), ' cases are complete')
        options(stringsAsFactors = FALSE)
    }
}

.DF.perct_missing <- function(df){
    n_missing <- df %>%
        is.na %>%
        sum
    n_total <- nrow(df) * ncol(df)
    perct_missing = n_missing / n_total * 100
    message(perct_missing %>% sprintf('%.1f', .),
            '% of data is missing from data.frame ',
            substitute(df))
    return(perct_missing)
}


# Assess whether table is indexed by a unique and never missing key col
#' Title
#'
#' @param df data.frame
#' @param keyCol a character or integer column index
#'
#' @return data.frame
#' @export
#'
#' @examples
#' seqs_df %>% DF.assess_key('sequence') %>% summary
DF.assess_key <- function(df, keyCol=1L) {
    # Check Args
    if (is.numeric(keyCol) && (keyCol %% 1 == 0)) keyCol %<>% as.integer
    assert_is_data.frame(df); assert(Or(is.integer, is.character)(keyCol))
    if (is.character(keyCol) && (keyCol %ni% names(df)))
        stop(paste0('keyCol `', keyCol, '` not found in data.frame `', substitute(df), '`.'))
    if (is.integer(keyCol) && keyCol > ncol(df))
        stop(paste0('keyCol `', keyCol ,'` is out of range of data.frame `', substitute(df), '` which has ', nrow(df), 'rows.'))

    # Check key
    if (any(is.na(df[[keyCol]])))
        stop(paste0('data.frame `', substitute(df), '` has ', sum(is.na(df[[keyCol]])), ' missing values in keyCol ', substitute(keyCol)))
    if (!n_distinct(df[[keyCol]]) == nrow(df))
        stop(paste0('data.frame `', substitute(df), '` has ', nrow(df), 'rows but only ', n_distinct(df[[keyCol]]), ' distinct values in keyCol ', substitute(keyCol)))
    message('keyCol `', substitute(keyCol), '` is suitable for indexing data.frame.')
    return(df)
}


#############
# Filtering #
#############
DF.remove_all_NA_cols <- function(df){
    all_na_cols = colSums(is.na(df)) %>% equals(nrow(df)) %>% which %>% names
    message(len(all_na_cols), ' columns in ', substitute(df), ' contained all NAs and were removed:\n',
            all_na_cols)
    df <- df[setdiff(names(df), all_na_cols)]
    return(df)
}


################
# Manipulation #
################
DF.chrs2factors <- function(df) {
    df.is_char_col <- df %>% map_lgl(is.character)
    df.char_cols <- df[df.is_char_col]
    message('converting data.frame `', substitute(df), "`'s character columns ",
            "to factor columns: \n", df.is_char_col %>% which %>% names %>% paste(collapse = ', '))
    df.factors <- map_df(df.char_cols, factor)
    df[df.is_char_col] <- df.factors
    return(df)
}

#' Map Names Flexibly in a Data.Frame or Vector
#' A wrapper for `mapvalues` (which only supports vectors, not data frames),
#'  with the same mapping rules.
#' `from` and `to` must have equal lengths.
#'
#' @param df : a data frame whose names need changed
#' @param from : a character vector with old col names.
#' @param to : a character vector with new col names.
#'
#' @return df : a dataframe with the same values as input, but possibly replaced colnames
#' @export
#'
#' @examples
#' # DF Operand
#' mapnames(mtcars,
#'          from = c("cyl", "hp"),
#'          to   = c("cylinders", "horsepower"))
#' # Nmd Vector Operand
#' mapnames(mtcars$cyl %>% set_names(),
#'          from = c(4, 6),
#'          to   = c("four", "six"))
mapnames <- function(df, from, to){
    # A `mapvalues` wrapper to rename columns of a
    # data.frame, named vector, or named list
    old_df_nms <- names(df)
    new_df_nms <- mapvalues(x = old_df_nms, from=from, to=to)
    names(df) <- new_df_nms
    return(df)
}

#' Transform a vector of dates into a numeric vector of days since the first date.
#'
#' Helpful for masking absolute dates and getting a continuous predictor/target
#'
#' @param x_date a vector of absolute dates to be transformed
#'
#' @return delta_date, a numeric vector of days since the first date
#' @export
#'
#' @examples
#' past_101_days <- Sys.Date() - seq(from = 0, to=100)
#' assert(all(zero_dates(past_101_days) == 100:0))
zero_dates <- function(x_date){
    stopifnot(is.date(x_date))
    first_date <- min(x_date)
    delta_date <- x_date - first_date
    delta_date %<>% as.numeric()
    if (interactive())
        hist(delta_date)
    return(delta_date)
}
