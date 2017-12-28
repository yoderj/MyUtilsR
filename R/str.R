#' Convert string to title case.
#'
#' Transform a character vector into a title case representation.
#' First, separate camelCase and alphanumeric boundaries into spaces
#' Second, split pieces by symbols or spaces
#' Third, use \code{\link[stringr]{str_to_title}} to glue pieces into title.
#'
#' @param x_chr character vector
#'
#' @return x_title character vector with same length as x_chr, but title format
#'
#' @import stringr purrr
#' @export
#'
#' @family str_case
#'
#' @examples
#' str_case_title(rownames(mtcars))
str_case_title <- function(x_chr) {
    pieces <- split_pieces(x_chr)
    nms <- names(pieces)

    x_title <- pieces %>%
        map_chr(str_c, collapse=" ") %>%
        stringr::str_to_title()

    names(x_title) <- nms
    x_title
}


#' Convert string to snake_case.
#'
#' @param x_chr character vector
#'
#' @return A character vector the same len as x_chr, with CamelCaseFormatting
#' @export
# str_case_snake <- function(x_chr) {
#     x_chr %>%
#         stringr::str_replace_all(
#             pattern = "_",
#             replacement= " ") %>%
#         stringr::str_to_title() %>%
#         stringr::str_replace_all(
#             pattern = " ",
#             replacement= "")
# }


#' Convert snake case to camel case by removing "_" and making initial letters cap
#'
#' @param x_chr A character vector with snake_case_formatting
#'
#' @return A character vector the same len as x_chr, with CamelCaseFormatting
#'
#' @importFrom magrittr "%>%"
#' @import rebus
#' @export
#' @example
#' str_case_camel(x_chr = c("from_snake_case", "From Title Case"))
# str_case_camel <- function(x_chr, upper=FALSE) {
#     SPACE_PAT <- capture()
#     DELIMITER_IN <- rebus::or("[:space:]", "[:punct:]")
#
#     x_spaced <-
#     x_pieces <- x_chr %>%
#
#     x_chr %>%
#         stringr::str_replace_all(
#             pattern = "_",
#             replacement= " ") %>%
#         stringr::str_to_title() %>%
#         stringr::str_replace_all(
#             pattern = " ",
#             replacement= "")
# }
#
