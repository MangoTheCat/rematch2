
context("re_match_all")

test_that("corner cases", {

  res <- re_match_all(c("foo", "bar"), "")
  expect_equal(res, df(.match = list(c("", "", ""), c("", "", ""))))

  res <- re_match_all(c("", "bar"), "")
  expect_equal(res, df(.match = list("", c("", "", ""))))

  res <- re_match_all(character(), "")
  expect_equal(res, df(.match = list()))

  res <- re_match_all(character(), "foo")
  expect_equal(res, df(.match = list()))

  res <- re_match_all("not", "foo")
  expect_equal(res, df(.match = list(character())))
})


test_that("capture groups", {

  pattern <- "([0-9]+)"

  res <- re_match_all(c("123xxxx456", "", "xxx", "1", "123"), pattern)
  expect_equal(
    res,
    df(
      .match = list(c("123", "456"), character(), character(), "1", "123"),
      list(c("123", "456"), character(), character(), "1", "123")
    )
  )

})


test_that("scalar text with capure groups", {

  res <- re_match_all("foo bar", "\\b(\\w+)\\b")
  expect_equal(
    res,
    df(.match = list(c("foo", "bar")), list(c("foo", "bar")))
  )

  res <- re_match_all("foo bar", "\\b(?<word>\\w+)\\b")
  expect_equal(
    res,
    df(.match = list(c("foo", "bar")), word = list(c("foo", "bar")))
  )
})
