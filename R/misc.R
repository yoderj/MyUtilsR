#' Helper to generate a character with the current date and time
#'
#' Useful for stamping a filename or log entry.
#'
#' @return s_chr: a scalar character with the current <date>_<time>
#'
#' @export
date_time_stamp <- function(){format(Sys.time(), "%Y%m%d_%H%M")}
