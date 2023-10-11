test_that("result is a ggplot object", {
  p <- getBarChart(data.frame(a=floor(runif(10, min=0, max=5))),a)
  expect_true(ggplot2::is.ggplot(p))

  p <- getBarChart(tab=data.frame(a = 1),"b")


  getBarChart(tab = data,var = "group_level")


})
