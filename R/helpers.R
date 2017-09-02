#' @export
soundex <- function(x) {
    stringdist::phonetic(trimws(tolower(x)), method = "soundex", useBytes = FALSE)
}

#' @export
`%+%` <- function(x, y) {
    paste0(trimws(x), trimws(y))
}
