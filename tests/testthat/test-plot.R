test_that("input plot", {
  plot <-  mockSummarisedResult()%>%
    dplyr::filter(variable=="cohort_start_date") %>%
    ggplot2::ggplot(ggplot2::aes(x=estimate)) +
    themeOxfor()

  expect_true(ggplot2::is.ggplot(plot))
})

