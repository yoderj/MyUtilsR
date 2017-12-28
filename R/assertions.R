#' @export
assert_fs_exists <- function() {
    objects(pattern="IN_PTH", name = .GlobalEnv) %>%
        purrr::map_chr(get) %>%
        assertive::assert_all_are_existing_files()
}
