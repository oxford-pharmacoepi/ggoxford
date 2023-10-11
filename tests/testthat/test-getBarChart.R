test_that("result is a ggplot object", {
  p <- getBarChart(data.frame(tab=floor(runif(10, min=0, max=5))),"a")
  expect_true(ggplot2::is.ggplot(p))
  expect_error(getBarChart(tab=floor(runif(10, min=0, max=5)),1))
})
