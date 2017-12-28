library(MyUtils)
library(magrittr)
library(purrr)
context("String methods")


# Test Blocks ----
test_that("test string case 2 title transformations", {
    expect_title <- function(x_str, expect_str) {
        eval(bquote(
            expect_equivalent(object = str_case_title(.(x_str)), .(expect_str))
        ))
    }

    #! Issue #53
    # test suite of vectorized objects against cached reference
    # expect_equal_to_reference(object = map(test_chrs, str_case_title),
    #                           file = system.file("tests", "testthat", package = "MyUtils") %>%
    #                               file.path("test-str-testcache.rds"))

    # test edge case scalar strings
    expect_title("Aa_(w", "Aa W")
    expect_title(" aA.", "A A")
})

test_that("test string case 2 title transformations", {
    expect_title <- function(x_str, expect_str) {
        eval(bquote(
            expect_equivalent(object = str_case_title(.(x_str)), .(expect_str))
        ))
    }

    #! Issue #53
    # test suite of vectorized objects against cached reference
    # expect_equal_to_reference(object = map(test_chrs, str_case_title),
    #                           file = system.file("tests", "testthat", package = "MyUtils") %>%
    #                               file.path("test-str-testcache.rds"))

    # test edge case scalar strings
    expect_title("Aa_(w", "Aa W")
    expect_title(" aA.", "A A")
})
