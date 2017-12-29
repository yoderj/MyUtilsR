# Helpers ----

if (interactive()) {
    test_chrs <<- MyUtils::gen_test_suite()
}

#' Split a string vector into a list of string piece vectors.
#'
#' First, a replacement is performed at alphanumeric piece edges to create
#' pieces that are delimited by a space or punctuation character.
#' Second, the string is split by split_pat into pieces
#'
#' @param x_chr character vector of strings to be split
#' @param split_pat regex pattern to delimit pieces
#'
#' @return pieces list with same length as x_chr, whose i-th element contains a
#' character vector of split products of x_chr[i]
#'
#' @import rebus purrr stringr assertive.strings
#' @export
split_pieces <- function(x_chr, split_pat = character()) {
    if (is_empty(split_pat)) {
        split_pat <- one_or_more(rebus::or("[[:space:]]", "[[:punct:]]"))
    }
    nms <- names(x_chr) %||% x_chr

    pieces <- x_chr %>%
        str_replace_all(pattern=capture(LOWER) %R% capture(UPPER),
                        replacement = "\\1 \\2") %>%
        str_replace_all(pattern = capture(ALPHA) %R% capture(DIGIT),
                        replacement = "\\1 \\2") %>%
        str_replace_all(pattern = capture(DIGIT) %R% capture(ALPHA),
                        replacement = "\\1 \\2") %>%
        str_split(pattern=split_pat)

    # Remove empty strings pieces
    pieces %<>% lapply(FUN = discard, .p=is_empty_character)

    names(pieces) <- nms
    pieces
}


# Case Transformations ----

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
#' Transform a character vector into a snake case representation.
#' First, separate camelCase and alphanumeric boundaries into spaces
#' Second, split pieces by symbols or spaces
#' Third, convert all alpha to lower with \code{\link[stringr]{str_to_lower}}
#' Finally, collapse with "_"
#'
#' @param x_chr character vector
#'
#' @return x_snake character vector with same length as x_chr, but snake format
#'
#' @import stringr
#' @export
#'
#' @family str_case
str_case_snake <- function(x_chr) {
    pieces <- split_pieces(x_chr)

    pieces %>%
        map(stringr::str_to_lower) %>%
        map_chr(str_c, collapse="_")
}


#' Convert string to camel case.
#'
#' Transform a character vector into a camel case representation.
#' First, convert `x_chr` to title case with \code{\link{str_case_title}}
#' Second, remove spaces
#' Third, lower first character of each str if `lower`
#'
#' @param x_chr A character vector with snake_case_formatting
#'
#' @return A character vector the same len as x_chr, with CamelCaseFormatting
#'
#' @importFrom magrittr "%>%"
#' @import rebus
#' @export
#' @examples
#' str_case_camel(x_chr = c("from_snake_case", "From Title Case"))
#' str_case_camel(x_chr = c("from_snake_case", "From Title Case"), lower=FALSE)
str_case_camel <- function(x_chr, lower=TRUE) {
    nms <- names(x_chr)

    x_UpperCamelCase <- x_chr %>%
        str_case_title() %>%
        str_replace_all(pattern = " ", replacement = "")
    names(x_UpperCamelCase) <- nms
    if (lower) {
        return(
            (function(.x)
            {
                str_c(substr(.x, 0, 1) %>% tolower(),
                      substr(.x, 2, nchar(.x)))
            })(x_UpperCamelCase)
        )
    } else {
        return(x_UpperCamelCase)
    }
}