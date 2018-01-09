#' Query the available aesthetics for a ggplot geom
#'
#' @param geom
#'
#' @return data frame
#' @export
#' @family gg_aes
#'
#' @examples
#' gg_find_aes()
gg_find_aes <- function(geom="point"){
    tryCatch({
        Geom <- getFromNamespace(paste("Geom", ggplot2:::firstUpper(geom), sep=""),
                                 "ggplot2")

        tmp <- unclass(Geom$default_aes)
        tmp[is.na(tmp)] <- "yes"
        data.frame(tmp, stringsAsFactors=FALSE)
    }, error = function(e) {})
}


#' Build lLUT to associate ggplot2 layer with aesthetics.
#'
#' @return data.frame
#' @export
#' @family gg_aes
#'
#' @examples
#' geoms_aesthetics_mtx()
gg_geoms_aesthetics_mtx <- function() {
    find_aes <- function(geom="point"){
        tryCatch({
            Geom <- getFromNamespace(paste("Geom", ggplot2:::firstUpper(geom), sep=""),
                                     "ggplot2")

            tmp <- unclass(Geom$default_aes)
            tmp[is.na(tmp)] <- "yes"
            data.frame(tmp, stringsAsFactors=FALSE)
        }, error = function(e) {})
    }

    funs <- grep("^geom_", ls("package:ggplot2"),val=T)

    geoms <- gsub("^geom_", "", funs)

    all <- lapply(geoms, find_aes)
    names(all) <- geoms
    relevant <- sapply(all, function(x) !is.null(x) && nrow(x) > 0)
    library(plyr)
    results = do.call("rbind.fill",all)
    rownames(results) <- names(relevant[relevant])
    results[is.na(results)] <- "--"
    results
}
