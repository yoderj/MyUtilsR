#! TODO: make an S3 generic fxn inspect()


#' Characterize and inspect contents of packages
#'
#' @param pkg character(1)
#' @return None, called for side effects.
#'
#' @export
#'
#' @examples
#' inspect_pkg("base")
inspect_pkg <- function(pkg) {
    stopifnot(is.character(pkg))
    requireNamespace(pkg, character.only = TRUE)
    data(package=pkg)
}
