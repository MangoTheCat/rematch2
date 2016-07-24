
#' Match Regular Expressions with a Nicer 'API'
#'
#' A small wrapper on 'regexpr' to extract the matches and captured
#' groups from the match of a regular expression to a character vector.
#' See \code{\link{re_match}}.
#'
#' @docType package
#' @name rematch2
#' @importFrom tibble tibble
NULL

#' Match a regular expression to a character vector, return a data frame
#'
#' \code{re_match} wraps \code{\link[base]{regexpr}} and returns the
#' match results in a convenient data frame. The data frame has one
#' column for each capture group, and one final columns called \code{.match}
#' for the matching (sub)string. The columns of the capture groups are
#' named if the groups themselves are named.
#'
#' Note that \code{re_match} always matches PCRE compatible regular
#' epxressions, i.e. \code{perl = TRUE} is set for
#' \code{\link[base]{regexpr}}.
#'
#' @param text Character vector.
#' @param pattern A PCRE regular expression. See \code{\link[base]{regex}}
#'   for more about regular expressions.
#' @param ... Additional arguments to pass to
#'   \code{\link[base]{regexpr}}.
#' @return A data frame of character vectors: one column per capture
#'   group, named if the group was named, and an additional column for
#'   the first matching (sub)string. Each row corresponds to an element
#'   in the \code{text} vector.
#'
#' @export
#' @family tidy regular expression matching
#' @examples
#' dates <- c("2016-04-20", "1977-08-08", "not a date", "2016",
#'   "76-03-02", "2012-06-30", "2015-01-21 19:58")
#' isodate <- "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
#' re_match(text = dates, pattern = isodate)
#'
#' # The same with named groups
#' isodaten <- "(?<year>[0-9]{4})-(?<month>[0-1][0-9])-(?<day>[0-3][0-9])"
#' re_match(text = dates, pattern = isodaten)

re_match <- function(text, pattern, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = TRUE, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".match")
  class(res) <- c("tbl_df", "tbl", class(res))
  res
}
