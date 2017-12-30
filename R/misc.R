#' Helper to generate a character with the current date and time
#'
#' Useful for stamping a filename or log entry.
#'
#' @return s_chr: a scalar character with the current <date>_<time>
#'
#' @export
date_time_stamp <- function(){format(Sys.time(), "%Y%m%d_%H%M")}


#' Extract the 'stem' from a file path (the filename without the extension).
#'
#' Useful for providing unique names to filepath vectors
#'
#' @param x_chr character vector of file paths
#' @return x_chr same length as x_chr of file path stems
#'
#' @import rebus
#' @export
stem <- function(x_chr) {
    bn <- basename(x_chr)
    stem <- str_replace(bn,
                        pattern=DOT %R% one_or_more(ALNUM) %R% END,
                        replacement="")
    stem
}


