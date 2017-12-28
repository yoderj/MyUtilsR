library(magrittr)

# Generate character test suite ----

#' Generate Test Suite.
#'
#' @return an env with several named character vectors
#'
#' @family testing_toolkit
#' @export
gen_test_suite <- function() {
    test_suite <- list(
        x_chr_lowers = stringr::words %>% str_to_lower() %>% purrr::set_names(., .),
        x_chr_uppers = stringr::words %>% str_to_upper() %>% purrr::set_names(., .),
        x_chr_sentences = stringr::sentences %>% purrr::set_names(., .),
        x_chr = mtcars %>% rownames() %>% purrr::set_names(., .)
    )
    test_suite %<>% map(~sample(.x, size = 10))
}


# Helpers ----

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