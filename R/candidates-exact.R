exact_candidates <- function(a, b, comparison,
                             output = NULL) {

    comp <- parse_comparison(a, b, comparison)
    output <- parse_output_spec(a, b, output)

    a_out <- output$a_out
    b_out <- output$b_out

    a_out %>%
        dplyr::mutate(..match = comp$compa) %>%
        dplyr::inner_join(
            b_out %>%
                dplyr::mutate(..match = comp$compb),
            by = "..match") %>%
        dplyr::select(-..match)
}
