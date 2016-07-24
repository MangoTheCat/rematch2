
#' First regular expression match and positions
#'
#' Match a regular expression to a string, and return the first match,
#' match positions, and also capture groups, if any.
#'
#' The results are returned in a data frame with list columns. The strings
#' of the character vector correspond to the rows of the data frame.
#' The columns correspond to capture groups and the first matching
#' (sub)string. The columns of named capture groups are named accordingly,
#' and the column of the full match if the last column and it is named
#' \code{.match}.
#'
#' Each column of the result is a list, containing match records.
#' A match record is a named list, with entries \code{match}, \code{start}
#' and \code{end}; the matching (sub) string, the start and end positions
#' (using one based indexing).
#'
#' Non-matching strings contain NAs in their corresponding rows, for the
#' matches and the positions as well.
#'
#' @inheritParams re_match
#' @return A data frame with list columns. See the details below.
#'
#' @family tidy regular expression matching
#' @export
#' @examples
#' TODO

re_exec <- function(text, pattern, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = TRUE, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_
  end     [ start == -1 ] <- NA_integer_
  start   [ start == -1 ] <- NA_integer_

  names <- c("match", "start", "end")

  matchlist <- lapply(seq_along(text), function(i) {
    structure(list(matchstr[i], start[i], end[i]), names = names)
  })

  res <- structure(
    list(matchlist),
    names = ".match",
    row.names = seq_along(text),
    class = "data.frame"
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    gend    [ gstart == -1 ] <- NA_integer_
    gstart  [ gstart == -1 ] <- NA_integer_
    dim(groupstr) <- dim(gstart)

    grouplists <- lapply(
      seq_along(attr(match, "capture.names")),
      function(g) {
        lapply(seq_along(text), function(i) {
          structure(
            list(groupstr[i, g], gstart[i, g], gend[i, g]),
            names = names
          )
        })
      }
    )

    res <- structure(
      c(grouplists, res),
      names = c(attr(match, "capture.names"), ".match"),
      row.names = seq_along(text),
      class = "data.frame"
    )
  }

  res
}
