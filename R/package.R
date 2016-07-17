
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
#' @return A character matrix of the matched (sub)strings.
#'   The first column is always the full match. This column is
#'   named \code{.match}. The result of the columns are capture groups,
#'   with appropriate column names, if the groups are named.
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

  ## Full matches
  res <- cbind(as.character(
    ifelse(
      match == -1,
      NA_character_,
      substr(text, match, match + attr(match, "match.length") - 1)
    )
  ))

  if (!is.null(attr(match, "capture.start"))) {

    res <- cbind(
      res,
      rbind(vapply(
        seq_len(NCOL(attr(match, "capture.start"))),
        function(i) {
          start <- attr(match, "capture.start")[,i]
          len <- attr(match, "capture.length")[,i]
          end <- start + len - 1
          res <- substr(text, start, end)
          res[ start == -1 ] <- NA_character_
          res
        },
        character(length(match))
      ))
    )
  }

  colnames(res) <- c(".match", attr(match, "capture.names"))
  res
}

#' Extract all matches of a regular expression
#'
#' This function is a thin wrapper on the \code{\link[base]{gregexpr}}
#' base R function, to provide an API that is easier to use. It is
#' similar to \code{\link{re_match}}, but extracts all matches, including
#' potentially named capture groups.
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[base]{regexpr}}.
#' @inheritParams re_match
#' @return A list of character matrices. Each list element contains the
#'   matches of one string in the input character vector. Each matrix
#'   has a \code{.match} column that contains the matching part of the
#'   string. Additional columns are added for capture groups. For named
#'   capture groups, the columns are named.
#'
#' @export

re_match_all <- function(text, pattern, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- gregexpr(pattern, text, perl = TRUE, ...)

  mapply(re_match_all1, text, match, SIMPLIFY = FALSE)
}

re_match_all1 <- function(text, match) {

  match_len <- attr(match, "match.length")
  capt_start <- attr(match, "capture.start")
  capt_len <- attr(match, "capture.length")
  capt_names <- attr(match, "capture.names")
  match <- as.vector(match)

  if (identical(match, -1L)) {
    return(matrix(
      character(),
      nrow = 0,
      ncol = length(capt_names) + 1,
      dimnames = list(character(), c(".match", capt_names))
    ))
  }

  res <- cbind(as.character(substring(text, match, match + match_len - 1)))

  if (!is.null(capt_start)) {
    res <- cbind(
      res,
      rbind(vapply(
        seq_len(NCOL(capt_start)),
        function(i) {
          substring(text, capt_start[,i], capt_start[,i] + capt_len[,i] - 1)
        },
        character(length(match))
      ))
    )
  }

  colnames(res) <- c(".match", capt_names)

  res
}
