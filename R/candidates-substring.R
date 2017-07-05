#' @useDynLib stringcandidates
#' @importFrom Rcpp sourceCpp
NULL

#' @export
substring_candidates <- function(a, b, comparison,
                                 output_a = names(a),
                                 output_b = names(b),
                                 k) {
    comp_a <- a[[names(comparison)]]
    comp_b <- b[[unname(comparison)]]

    ind <- match_substrings(comp_a, comp_b, k = k)
    result <- a[, output_a, drop = FALSE]
    result <- result %>%
        mutate(.MATCHINDEX = ind) %>%
        unnest
    resb <- b[result$.MATCHINDEX, output_b, drop = FALSE]
    bind_cols(result, resb) %>% select(-.MATCHINDEX)
}
