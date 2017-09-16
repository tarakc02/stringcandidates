#' @export
ngram_candidates <- function(a, b, comparison, n = 2L, output = NULL) {
    comp <- parse_comparison(a, b, comparison)
    compa <- comp$compa
    compb <- comp$compb

    output <- parse_output_spec(a, b, output)
    a_out <- output$a_out
    b_out <- output$b_out

    compa <- tokenizers::tokenize_ngrams(compa, lowercase = FALSE, n = n)
    compb <- tokenizers::tokenize_ngrams(compb, lowercase = FALSE, n = n)

    dplyr::bind_cols(a_out, tibble::data_frame(compa = compa)) %>%
        tidyr::unnest() %>%
        dplyr::inner_join(dplyr::bind_cols(b_out, tibble::data_frame(compb = compb)) %>%
                                               tidyr::unnest(),
                          by = c("compa" = "compb")) %>%
        dplyr::select(-compa) %>%
        dplyr::distinct()
}
