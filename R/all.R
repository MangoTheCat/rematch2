
#' Extract all matches of a PCRE regular expression, as a data frame
#'
#' This function is a thin wrapper on the \code{\link[base]{gregexpr}}
#' base R function, to extract the matching (sub)strings as a data frame.
#' It extracts all matches, and potentially their capture groups as well.
#'
#' Note: if the input text character vector has length zero,
#' \code{\link[base]{regexpr}} is called instead of
#' \code{\link[base]{gregexpr}}, because the latter cannot extract the
#' number and names of the capture groups in this case.
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[base]{gregexpr}} (or \code{\link[base]{regexpr}} if
#'   \code{text} is of length zero).
#' @inheritParams re_match
#' @return A data frame with all matching (sub)strings and capture groups
#'   for all matches in all input strings. The rows of the data frame
#'   correspond to the input strings in \code{text}. The columns correspond
#'   to the capture groups plus the last column called \code{.match}
#'   corresponds to the complete matches. Since all matches are returned
#'   all columns are list columns containing character vectors.
#'   The columns of named groups are named as well.
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
#' re_match_all(notables, name_rex)

re_match_all <- function(text, pattern, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  ## Need to handle this case separately, as gregexpr effectively
  ## does not work for this.
  if (length(text) == 0) return(empty_result(text, pattern, ...))

  match <- gregexpr(pattern, text, perl = TRUE, ...)

  num_groups <- length(attr(match[[1]], "capture.names"))

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  res[non] <- list(replicate(num_groups + 1, character(), simplify = FALSE))
  res[yes] <- mapply(match1, text[yes], match[yes], SIMPLIFY = FALSE)

  ## Need to assemble the final data frame "manually".
  ## There is apparently no function for this. rbind() is almost
  ## good, but simplifies to a matrix if the dimensions allow it....
  res <- lapply(seq_along(res[[1]]), function(i) {
    lapply(res, "[[", i)
  })

  structure(
    res,
    names = c(attr(match[[1]], "capture.names"), ".match"),
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

match1 <- function(text1, match1) {

  matchstr <- substring(
    text1,
    match1,
    match1 + attr(match1, "match.length") - 1
  )

  ## substring fails if the index is length zero,
  ## need to handle special case
  if (is.null(attr(match1, "capture.start"))) {
    list(.match = matchstr)

  } else {
    gstart  <- attr(match1, "capture.start")
    glength <- attr(match1, "capture.length")
    gend    <- gstart + glength - 1

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(lapply(seq_len(ncol(groupstr)), function(i) groupstr[, i]),
      list(.match = matchstr)
      )
  }
}
