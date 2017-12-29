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
