test_that("hello works", {
  result <- mockSummarisedResult(populationSize = 10)
  resultA <- mockSummarisedResult(populationSize = 10, seed = 2)
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
