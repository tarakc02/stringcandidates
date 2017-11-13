#' Hypocoristics for common English given names
#'
#' Dataset containing the shortened/alternate versions of names
#' that are common in English
#'
#' @format A data frame with 755 rows and 2 variables:
#' \describe{
#'   \item{long}{long version of name}
#'   \item{short}{shortened version of name}
#' }
#' @source \url{https://en.wiktionary.org/wiki/Appendix:English_given_names}
"name_dictionary"

#' Frequently occurring surnames from the 2010 Census
#'
#' Tabulations of all surnames occurring 100 or more times in the 2010 Census.
#'
#' @format A data frame with 162,254 rows and 11 variables:
#' \describe{
#'   \item{name}{name, in caps}
#'   \item{rank}{rank}
#'   \item{count}{raw count of name occurrence}
#'   \item{prop100k}{prevalence of name per 100,000}
#'   \item{cum_prop100k}{proportion of population, per 100k, having this name or a more common one}
#'   \item{pctwhite}{percent of people with this surname identified as White}
#'   \item{pctblack}{percent of people with this surname identified as Black}
#'   \item{pctapi}{percent of people with this surname identified as Asian, Native Hawaiian, and other Pacific Islander}
#'   \item{pctaian}{percent of people with this surname identified as American Indian and Alaska Native}
#'   \item{pct2prace}{percent of people with this surname identified as two or more races}
#'   \item{pcthispanic}{percent of people with this surname identified as hispanic/latino}
#' }
#'
#' @source \url{https://www.census.gov/topics/population/genealogy/data/2010_surnames.html}
"census_surnames"

#' Surname counts by race, derived from Census data
#'
#' Counts derived from \code{\link{census_surnames}} and names converted
#' to lower case.
#'
#' @format A data frame with 162,254 rows and 11 variables:
#' \describe{
#'   \item{race}{race}
#'   \item{name}{name}
#'   \item{count}{calculated count based on overall name count and proportion by race}
#' }
#'
#' @source \url{https://www.census.gov/topics/population/genealogy/data/2010_surnames.html}
"surname_counts"

#' Counts for single-edit spelling correction edits
#'
#' Common typos
#'
#' @format A data framw ith 1,587 rows and 3 variables:
#' \describe{
#'   \item{from}{original character (single or two-character sequence)}
#'   \item{to}{replacement character (1- or 2-char sequence) after typo}
#'   \item{count}{frequency of this replacement in the typo corpus}
#' }
#'
#' @source \url{http://norvig.com/ngrams/}
"edit_counts"
