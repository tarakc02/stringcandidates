conditional_distribution <- function(context_size = 3L) {
    param_n <- context_size + 1
    pad <- paste(rep(" ", context_size), collapse = "")

    nm <- name_data %>%
        dplyr::mutate(name = paste0(pad, name, "#"))

    # the n-grams are the "context" along with the following letter
    ngrams <- nm %>%
        dplyr::mutate(ngram = tokenizers::tokenize_character_shingles(
            name, n = param_n, strip_non_alphanum = FALSE
        )) %>% tidyr::unnest() %>%
        dplyr::group_by(sex, ngram) %>%
        dplyr::summarise(cnt = sum(n))

    # finally, i calculate the distribution over the last letter,
    # given the context_size letters that came before
    conditional_dist <- ngrams %>%
        dplyr::mutate(prev = stringr::str_sub(ngram, 1, context_size),
                      nxt = stringr::str_sub(ngram, param_n, param_n)) %>%
        dplyr::group_by(sex, prev, nxt) %>%
        dplyr::summarise(joint_count = sum(cnt)) %>%
        dplyr::mutate(pct = joint_count / sum(joint_count)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-joint_count)

    conditional_dist %>%
        dplyr::group_by(sex, prev) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            data = purrr::map(data,
                              ~list2env(structure(as.list(.$pct), names = .$nxt)))) %>%
        split(.$sex) %>%
        purrr::map(~split(., .$prev)) %>%
        purrr::map(list2env, hash = TRUE)
}

#' @export
generator <- function(context_size, weight) {
    nm <- purrr::map(context_size, conditional_distribution)
    gen_name <- function(sex, n = 1) {
        res <- replicate(
            n,
            interp(nm, sex, starter = paste0(rep(" ", max(context_size)), collapse = ""),
                   context_sizes = context_size,
                   weights = weight)
        )
        stringr::str_trim(res)
    }
    structure(gen_name, class = "name_generator")
}
