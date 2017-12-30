# Generate test suite ----

test_suite <- list(
    x_chr_lowers = stringr::words %>% str_to_lower() %>% purrr::set_names(., .),
    x_chr_uppers = stringr::words %>% str_to_upper() %>% purrr::set_names(., .),
    x_chr_sentences = stringr::sentences %>% purrr::set_names(., .),
    x_chr = mtcars %>% rownames() %>% purrr::set_names(., .)
)
test_suite %<>% map(~sample(.x, size = 10))

test_suite$l_chrs <- replicate(n=5, sample(test_suite$x_chr, 5), simplify = FALSE) %>%
    set_names(str_c("x_chr", 1:5))

test_suite %<>% list2env()
# devtools::use_data(test_suite, overwrite = TRUE)
