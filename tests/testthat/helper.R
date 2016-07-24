
df <- function(...) {
  args <- list(...)
  structure(
    args,
    names = names(args),
    row.names = seq_along(args[[1]]),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

asdf <- function(...) {
  as.data.frame(df(...))
}
