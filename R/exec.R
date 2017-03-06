
#' Regular Expression Matches and Match Positions
#'
#' Match a regular expression to a string, and return matches, match positions,
#' and capture groups.
#'
#' \code{re_exec} returns the data from the first match, while
#' \code{re_exec_all} returns the data from all matches. The functions use
#' \code{\link[base]{regexpr}} and \code{\link[base]{gregexpr}} respectively to
#' extract matching substring(s) for a regular expression.
#'
#' The return value is a tidy data frame where each row corresponds to an
#' element of the input character vector \code{text}.  The values from
#' \code{text} appear for reference in the \code{.text} character column.  All
#' other columns are list columns containing the match data.  The \code{.match}
#' column contains the match information for full regular expression matches
#' while other columns correspond to capture groups if there are any and PCRE
#' matches are enabled with \code{perl = TRUE} (this is on by default).  If
#' capture groups are named the corresponding columns will bear those names.
#'
#' Each match data list column contains match records, one for each element in
#' \code{text}.  A match record is a named list, with entries \code{match},
#' \code{start} and \code{end}; the matching (sub) string, the start and end
#' positions (using one based indexing).  For \code{re_exec} these entries will
#' be one length vectors set to NA if there is no match.  For \code{re_exec_all}
#' these entries will be as long as there are matches, with length zero if there
#' are no matches.
#'
#' To make it easier to extract matching substrings or positions, a
#' special \code{$} operator is defined on match columns, both for the
#' \code{.match} column and the columns corresponding to the capture groups.
#'
#' See example below.
#'
#' @inheritParams re_match_all
#' @see also \code{\link{re_match}}, \code{\link{re_match_all}}
#' @param x Object returned by \code{re_exec} or \code{re_exec_all}.
#' @param name \code{match}, \code{start} or \code{end}.
#' @return A data frame with list columns, see details below.
#'
#' @family tidy regular expression matching
#' @export
#' @examples
#' name_rex <- paste0(
#'   "(?<first>[[:upper:]][[:lower:]]+) ",
#'   "(?<last>[[:upper:]][[:lower:]]+)"
#' )
#' notables <- c(
#'   "  Ben Franklin and Jefferson Davis",
#'   "\tMillard Fillmore"
#' )
#' # Match first occurrence
#' pos <- re_exec(notables, name_rex)
#' pos
#'
#' # All occurrences
#' allpos <- re_exec_all(notables, name_rex)
#' allpos
#'
#' # Custom $ to extract matches and positions
#' pos$first$match
#' pos$first$start
#' pos$first$end
#' allpos$first$match
#' allpos$first$start
#' allpos$first$end

re_exec <- function(text, pattern, perl=TRUE, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_
  end     [ start == -1 ] <- NA_integer_
  start   [ start == -1 ] <- NA_integer_

  names <- c("match", "start", "end")

  matchlist <- structure(
    lapply(seq_along(text), function(i) {
      structure(list(matchstr[i], start[i], end[i]), names = names)
    }),
    class = "rematch_records"
  )

  res <- structure(
    list(text, matchlist),
    names = c(".text", ".match"),
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- unname(attr(match, "capture.start"))
    glength <- unname(attr(match, "capture.length"))
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    gend    [ gstart == -1 ] <- NA_integer_
    gstart  [ gstart == -1 ] <- NA_integer_
    dim(groupstr) <- dim(gstart)

    grouplists <- lapply(
      seq_along(attr(match, "capture.names")),
      function(g) {
        structure(
          lapply(seq_along(text), function(i) {
            structure(
              list(groupstr[i, g], gstart[i, g], gend[i, g]),
              names = names
            )
          }),
          class = "rematch_records"
        )
      }
    )

    res <- structure(
      c(grouplists, res),
      names = c(attr(match, "capture.names"), ".text", ".match"),
      row.names = seq_along(text),
      class = c("tbl_df", "tbl", "data.frame")
    )
  }

  res
}
