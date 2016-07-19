
context("rematch2")

test_that("corner cases", {

  res <- re_match(c("foo", "bar"), "")
  expect_equal(res, df(.match = c("", "")))

  res <- re_match(c("foo", "", "bar"), "")
  expect_equal(res, df(.match = c("", "", "")))

  res <- re_match(character(), "")
  expect_equal(res, df(.match = character()))

  res <- re_match(character(), "foo")
  expect_equal(res, df(.match = character()))

  res <- re_match(character(), "foo (g1) (g2)")
  expect_equal(res, df(.match = character(), character(), character()))

  res <- re_match(character(), "foo (g1) (?<name>g2)")
  expect_equal(
    res,
    df(.match = character(), character(), name = character())
  )

  res <- re_match("not", "foo")
  expect_equal(res, df(.match = NA_character_))
})


test_that("not so corner cases", {

  dates <- c("2016-04-20", "1977-08-08", "not a date", "2016",
             "76-03-02", "2012-06-30", "2015-01-21 19:58")
  isodate <- "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
  expect_equal(
    re_match(text = dates, pattern = isodate),
    df(
      .match = c(dates[1:2], NA, NA, NA, "2012-06-30", "2015-01-21"),
      c("2016", "1977", NA, NA, NA, "2012", "2015"),
      c("04", "08", NA, NA, NA, "06", "01"),
      c("20", "08", NA, NA, NA, "30", "21")
    )
  )

  isodaten <- "(?<year>[0-9]{4})-(?<month>[0-1][0-9])-(?<day>[0-3][0-9])"
  expect_equal(
    re_match(text = dates, pattern = isodaten),
    df(
      .match = c(dates[1:2], NA, NA, NA, "2012-06-30", "2015-01-21"),
      year = c("2016", "1977", NA, NA, NA, "2012", "2015"),
      month = c("04", "08", NA, NA, NA, "06", "01"),
      day = c("20", "08", NA, NA, NA, "30", "21")
    )
  )
})


test_that("UTF8", {

  res <- re_match("Gábor Csárdi", "Gábor")
  expect_equal(res, df(.match = "Gábor"))

})


test_that("text is scalar & capture groups", {

  res <- re_match("foo bar", "(\\w+) (\\w+)")
  expect_equal(res, df(.match = "foo bar", "foo", "bar"))

  res <- re_match("foo bar", "(?<g1>\\w+) (?<g2>\\w+)")
  expect_equal(res, df(.match = "foo bar", g1 = "foo", g2 = "bar"))

})
