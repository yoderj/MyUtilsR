library(MyUtils)
library(magrittr)
library(purrr)
context("String methods")


# Test Blocks ----
test_that("test string case 2 Title Case transformations", {
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

test_that("test string case 2 snake_case transformations", {
    expect_snake <- function(x_str, expect_str) {
        eval(bquote(
            expect_equivalent(object = str_case_snake(.(x_str)), .(expect_str))
        ))
    }

    expect_snake("Aa_(w", "aa_w")
    expect_snake(" aA.", "a_a")
})

test_that("test string case 2 camelCase transformations", {
    expect_camel <- function(x_str, expect_str, lower=TRUE) {
        eval(bquote(
            expect_equivalent(object=str_case_camel(.(x_str), .(lower)), .(expect_str))
        ))
    }

    expect_camel("Aa_(w", "aaW")
    expect_camel(" aA.", "aA")
    expect_camel("Aa_(w", "AaW", lower=FALSE)
    expect_camel(" aA.", "AA", lower=FALSE)
})
