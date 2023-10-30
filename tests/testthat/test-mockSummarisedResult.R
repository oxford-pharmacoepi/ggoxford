
test_that("test date", {
  result <- mockSummarisedResult(seed = 1,as.Date("2021-01-01"),as.Date("2021-12-31"))
  out_of_range <- result %>%
    dplyr::filter(.data$variable %in% c("cohort_start_date", "cohort_end_date")) %>%
    dplyr::filter((as.Date("2021-01-01") >= .data$estimate) | (as.Date("2021-12-31") <= .data$estimate)) %>%
    dplyr::tally()
  expect_true(out_of_range == 0)

  result <-mockSummarisedResult(populationSize = 10)
  resultA <-mockSummarisedResult(populationSize = 10,seed=2)
  expect_equal(
    result %>%
      dplyr::filter(
        group_name == "Overall",
        strata_name == "Overall",
        variable == "number subjects",
      ) %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    10
  )
  expect_error(mockSummarisedResult(populationSize = "10"))
  expect_s3_class(mockSummarisedResult(), class = "tbl")
  suppressWarnings(expect_false(isTRUE(dplyr::all_equal(result, resultA))))
})
