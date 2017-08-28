#' @useDynLib stringcandidates
#' @importFrom Rcpp sourceCpp
NULL

#' @export
substring_candidates <- function(a, b, comparison, k = 6,
                                 output = NULL) {
    if (!rlang::is_formula(comparison))
        stop("enter comparison as a formula, eg: x ~ y")

    compa <- transmute(a, !!comparison[[2]])[[1]]
    compb <- transmute(b, !!comparison[[3]])[[1]]

    if (is.null(output)) {
        idcols_a <- grepl("id$", names(a), ignore.case = TRUE)
        idcols_b <- grepl("id$", names(b), ignore.case = TRUE)
        if (sum(idcols_a) == 1L && sum(idcols_b) == 1L) {
            a_out <- a[, idcols_a, drop = FALSE]
            b_out <- b[, idcols_b, drop = FALSE]
        }
        else stop("Need output specification")
    }

    if (rlang::is_formula(output)) {
        a_out <- dplyr::transmute(a, !!output[[2]])
        b_out <- dplyr::transmute(b, !!output[[3]])
    }

    if (!is.null(output) && !rlang::is_formula(output))
        stop("Need output specification")

    matches <- match_substrings(
        compa, compb,
        k = k
    )

    a_ind <- which(
        purrr::map_lgl(matches, ~length(.) > 0)
    )

    if (length(a_ind) == 0L) return(
        tibble::data_frame(id = integer(), b_id = integer())
    )

    a_out[a_ind, ] %>%
        dplyr::mutate(..b_id = matches[a_ind]) %>%
        tidyr::unnest() %>%
        bind_cols(b_out[.$..b_id, , drop = FALSE]) %>%
        dplyr::select(-..b_id) %>%
        dplyr::distinct()
}
