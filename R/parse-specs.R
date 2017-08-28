parse_comparison <- function(a, b, comparison) {
    if (!rlang::is_formula(comparison))
        stop("enter comparison as a formula, eg: x ~ y")

    compa <- dplyr::transmute(a, !!comparison[[2]])[[1]]
    compb <- dplyr::transmute(b, !!comparison[[3]])[[1]]

    list(compa = compa, compb = compb)
}

parse_output_spec <- function(a, b, output) {
    if (is.null(output)) {
        idcols_a <- grepl("id$", names(a), ignore.case = TRUE)
        idcols_b <- grepl("id$", names(b), ignore.case = TRUE)
        if (sum(idcols_a) == 1L && sum(idcols_b) == 1L) {
            return(list(
                a_out = a[, idcols_a, drop = FALSE],
                b_out = b[, idcols_b, drop = FALSE]
            ))
        }
        else stop("Need output specification")
    }

    if (rlang::is_formula(output)) {
        return(list(
            a_out = dplyr::transmute(a, !!output[[2]]),
            b_out = dplyr::transmute(b, !!output[[3]])
        ))
    }

    stop("Need output specification")
}
