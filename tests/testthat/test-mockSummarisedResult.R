
test_that("test date", {
  result <- mockSummarisedResult(seed = 1,as.Date("2021-01-01"),as.Date("2021-12-31"))
  out_of_range <- result %>%
    dplyr::filter(.data$variable %in% c("cohort_start_date", "cohort_end_date")) %>%
    dplyr::filter((as.Date("2021-01-01") >= .data$estimate) | (as.Date("2021-12-31") <= .data$estimate)) %>%
    dplyr::tally()
  expect_true(out_of_range == 0)
})
