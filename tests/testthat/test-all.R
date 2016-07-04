
context("re_match_all")

test_that("corner cases", {

  res <- re_match_all(c("foo", "bar"), "")
  expect_equal(
    res,
    list(
      cbind(.match = c("", "", "")),
      cbind(.match = c("", "", ""))
    )
  )

  res <- re_match_all(c("", "bar"), "")
  expect_equal(
    res,
    list(
      cbind(.match = ""),
      cbind(.match = c("", "", ""))
    )
  )

  res <- re_match_all(character(), "")
  expect_equal(res, list())

  res <- re_match_all(character(), "foo")
  expect_equal(res, list())

  res <- re_match_all("not", "foo")
  expect_equal(res, list(cbind(.match = character())))
})


test_that("capture groups", {

  pattern <- "([0-9]+)"

  res <- re_match_all(c("123xxxx456", "", "xxx", "1", "123"), pattern)
  expect_equal(
    res,
    list(
      cbind(.match = c("123", "456"), c("123", "456")),
      cbind(.match = character(), character()),
      cbind(.match = character(), character()),
      cbind(.match = "1", "1"),
      cbind(.match = "123", "123")
    )
  )

})


test_that("scalar text with capure groups", {

  res <- re_match_all("foo bar", "\\b(\\w+)\\b")
  expect_equal(res, list(cbind(.match = c("foo", "bar"), c("foo", "bar"))))

  res <- re_match_all("foo bar", "\\b(?<word>\\w+)\\b")
  expect_equal(
    res,
    list(
      cbind(.match = c("foo", "bar"), word = c("foo", "bar"))
    )
  )
})
