# 2.0.0.9000

* Add `perl` argument to `re_match` and `re_match_all` for compatibility with
  functions that may pass that argument as part of `...`

# 2.0.0

* Add `re_match_all` to extract all matches.

* Removed the `perl` options, we always use PERL compatible regular
  expressions.

# 1.0.1

* Make `R CMD check` work when `testthat` is not available.

* Fixed a bug with group capture when `text` is a scalar.

# 1.0.0

First public release.
