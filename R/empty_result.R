
empty_result <- function(text, pattern, ...) {
  match <- regexpr(pattern, text, perl = TRUE, ...)
  num_groups <- length(attr(match, "capture.names"))
  structure(
    replicate(num_groups + 1, list(), simplify = FALSE),
    names = c(attr(match, "capture.names"), ".match"),
    row.names = integer(0),
    class = "data.frame"
  )
}
