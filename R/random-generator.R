make_string <- function(alphabet = LETTERS[1:20], n = 10, nchar = 7:25) {
    if (length(nchar) == 1L) lens <- rep(nchar, n)
    else lens <- sample(nchar, n, replace = TRUE)
    vapply(lens,
           function(len) paste0(sample(alphabet, len, replace = TRUE), collapse = ""),
           character(1))
}

make_df <- function(nrow, ncol, ...) {
    args <- c(list(...), n = nrow)
    colnames <- paste0("var", seq_len(ncol))

    random_data <- replicate(
        ncol,
        do.call("make_string", args = args),
        simplify = FALSE)

    names(random_data) <- colnames
    tibble::as_data_frame(
        c(list(id = seq_len(nrow)), random_data)
    )
}
