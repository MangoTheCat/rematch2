
df <- function(...) {
  args <- list(...)
  structure(
    args,
    names = names(args),
    row.names = seq_along(args[[1]]),
    class = "data.frame"
  )
}
