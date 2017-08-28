context("match_substring")

test_that("match_substrings finds matching substrings", {
    expect_equal(
        match_substrings(
            c("abcdef", "ghijkl", "mnopq"),
            c("efaijklb", "hijkasd", "qqxymnbbcdefhnopq"),
            k = 4
        ),
        list(3, c(1, 2), 3)
    )
})

test_that("match substrings handles missing values", {
    expect_equal(
        match_substrings(
            c(NA, NA, NA, "abc"),
            c(NA, "ab"),
            k = 2
        ),
        list(integer(0), integer(0), integer(0), 2)
    )
})
