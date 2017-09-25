conditional_distribution <- function(count_data, context_size = 3L) {
    if (!is.data.frame(count_data))
        stop("count_data must be dataframe")
    if (!setequal(names(count_data), c("name", "count")))
        stop("count_data must only have columns 'name' and 'count'")

    count_data <- count_data %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(count = sum(count))

    param_n <- context_size + 1
    pad <- paste(rep(" ", context_size), collapse = "")

    nm <- dplyr::mutate(count_data, name = paste0(pad, name, "#"))

    # the n-grams are the "context" along with the following letter
    ngrams <- nm %>%
        dplyr::mutate(ngram = tokenizers::tokenize_character_shingles(
            name, n = param_n, strip_non_alphanum = FALSE
        )) %>% tidyr::unnest() %>%
        dplyr::group_by(ngram) %>%
        dplyr::summarise(cnt = sum(count))

    # finally, i calculate the distribution over the last letter,
    # given the context_size letters that came before
    conditional_dist <- ngrams %>%
        dplyr::mutate(prev = stringr::str_sub(ngram, 1, context_size),
                      nxt = stringr::str_sub(ngram, param_n, param_n)) %>%
        dplyr::group_by(prev, nxt) %>%
        dplyr::summarise(joint_count = sum(cnt)) %>%
        dplyr::mutate(pct = joint_count / sum(joint_count)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-joint_count)

    conditional_dist %>%
        dplyr::group_by(prev) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            data = purrr::map(data,
                              ~list2env(structure(as.list(.$pct), names = .$nxt)))) %>%
        split(.$prev) %>%
        list2env(., hash = TRUE)
}


gen <- function(count_data, context_size, weight) {
    if (length(context_size) != length(weight))
        stop("context_size and weight must be same length")
    if (sum(weight) != 1)
        stop("weights must sum to 1")
    nm <- purrr::map(context_size, conditional_distribution, count_data = count_data)
    gen_name <- function(n = 1) {
        res <- interp(
            nm,
            starter = paste0(rep(" ", max(context_size)), collapse = ""),
            context_sizes = context_size,
            weights = weight,
            n = n)
        stringr::str_to_title(stringr::str_trim(res))
    }
    structure(gen_name, class = "name_generator")
}

#' @export
generator <- function(count_data, context_size, weight) {
    groupings <- setdiff(names(count_data), c("name", "count"))
    if (length(groupings) > 1)
        stop("Can only handle one additional grouping at this time, but got: ",
             paste(groupings, collapse = ", "))
    if (length(groupings) == 0)
        return(gen(count_data, context_size = context_size, weight = weight))
    splitter <- count_data[[groupings]]
    count_data <- count_data[, c("name", "count"), drop = FALSE]
    res <- split(count_data, splitter)
    purrr::map(res, gen, context_size = context_size, weight = weight)
}
