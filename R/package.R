
#' Match Regular Expressions with a Nicer 'API'
#'
#' A small wrapper on 'regexpr' to extract the matches and captured
#' groups from the match of a regular expression to a character vector.
#' See \code{\link{re_match}}.
#'
#' @docType package
#' @name rematch2
NULL

#' Match a regular expression to a character vector
#'
#' This function is a small wrapper on the \code{\link[base]{regexpr}}
#' base R function, to provide an API that is easier to use.
#'
#' Currently only the first occurence of the pattern is used.
#'
#' @param pattern Regular expression, defaults to be a PCRE
#'   expression. See \code{\link[base]{regex}} for more about
#'   regular expressions.
#' @param text Character vector.
#' @param ... Additional arguments to pass to
#'   \code{\link[base]{regexpr}}.
#'
#' @export
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
  res
}
