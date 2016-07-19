
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
#'
#' @export

re_match_all <- function(text, pattern, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  ## Need to handle this case separately, as gregexpr effectively
  ## does not work for this.
  if (length(text) == 0) {
    match <- regexpr(pattern, text, perl = TRUE, ...)
    num_groups <- length(attr(match, "capture.names"))
    return(
      structure(
        replicate(num_groups + 1, list(), simplify = FALSE),
        names = c(attr(match, "capture.names"), ".match"),
        row.names = integer(0),
        class = "data.frame"
      )
    )
  }

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
    class = "data.frame"
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
