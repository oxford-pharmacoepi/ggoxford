test_that("box_plot check", {
expect_error(box_plot(data=1))
expect_true(ggplot2::is.ggplot(box_plot(mockSummarisedResult(seed = 1))))
})
