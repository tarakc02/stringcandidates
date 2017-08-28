#' @useDynLib stringcandidates
#' @importFrom Rcpp sourceCpp
NULL

#' @export
substring_candidates <- function(a, b, comparison, k = 6,
                                 output = NULL) {
    comp <- parse_comparison(a, b, comparison)
    compa <- comp$compa
    compb <- comp$compb

    output <- parse_output_spec(a, b, output)
    a_out <- output$a_out
    b_out <- output$b_out

    matches <- match_substrings(
        compa, compb,
        k = k
    )

    a_ind <- which(
        purrr::map_lgl(matches, ~length(.) > 0)
    )

    if (length(a_ind) == 0L) return(
        dplyr::bind_cols(a_out[0, , drop = FALSE],
                         b_out[0, , drop = FALSE])
    )

    a_out[a_ind, ] %>%
        dplyr::mutate(..b_id = matches[a_ind]) %>%
        tidyr::unnest() %>%
        bind_cols(b_out[.$..b_id, , drop = FALSE]) %>%
        dplyr::select(-..b_id) %>%
        dplyr::distinct()
}

substring_inner_join <- function(a, b, by, k) {
    compa <- a[, names(by), drop = FALSE]
    compb <- b[, by, drop = FALSE]
    matches <- lapply(
        seq_along(by),
        function(i) match_substrings(compa[[i]], compb[[i]], k = k)
    )

    intersections <- function(l1, l2) {
        purrr::map2(l1, l2, base::intersect)
    }

    matches <- purrr::reduce(matches, intersections)
    match_ind <- which(purrr::map_lgl(matches, ~length(.) > 0L))
    if (length(match_ind) == 0L) return(
        dplyr::bind_cols(a[0, , drop = FALSE],
                         b[0, , drop = FALSE])
    )

    match_df <- tibble::data_frame(a = match_ind, b = matches[match_ind])
    match_df <- tidyr::unnest(match_df)

    dplyr::bind_cols(
        a[match_df$a, , drop = FALSE],
        b[match_df$b, , drop = FALSE]
    )
}
