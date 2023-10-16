test_that("input plot", {
  plot <- mockSummarisedResult() %>%
    dplyr::filter(variable == "cohort_start_date") %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate)) +
    ggplot2::geom_bar()

  plot <- plot %>%
    themeOxford()

  expect_true(ggplot2::is.ggplot(plot))
})

test_that("input base size value", {
  plot <- mockSummarisedResult() %>%
    dplyr::filter(variable == "cohort_start_date") %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate)) +
    ggplot2::geom_bar()

  expect_error(plot %>%
    themeOxford(baseSize = 299))
})
