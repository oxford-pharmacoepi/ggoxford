# Test
test_that("check forest plot", {
  # Loading mock data
  df <- data.frame(
    label = paste0("X", 1:6),
    mean = c(1.30, 0.62, 2.00, 1.10, 0.83, 1.4),
    lower = c(0.80, 0.49, 1.50, 1.04, 0.72, 1.11),
    upper = c(2.01, 0.97, 3.00, 1.25, 1.08, 1.56)
  )


  result <- forestPlot(df, "label", "mean", "lower", "upper")

  # has the result a "gg, ggplot" class?
  expect_true(all(class(result) == c("gg", "ggplot")))

  expect_false(all(class(result) == c("not gg", "not ggplot")))

  expect_false(!any(class(result) == c("gg", "not ggplot")))


  # is data a data frame?
  expect_true(all(class(result) == c("gg", "ggplot")))


  # is outcome labels a column in data?

  # is estimate a column in data?

  # are lowCi and highCi values?
})
