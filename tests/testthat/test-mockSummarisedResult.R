test_that("hello works", {
  result <-mockSummarisedResult(populationSize = 10)
  resultA <-mockSummarisedResult(populationSize = 10,seed=2)
  expect_true(result %>% dplyr::filter(strata_name=="Overall" & variable=="number subjects") %>% dplyr::summarise(Freq = sum(as.numeric(estimate))) == 10)
  expect_error(mockSummarisedResult(populationSize = "10"))
  expect_s3_class(mockSummarisedResult(), class = "tbl")
  suppressWarnings(expect_false(isTRUE(dplyr::all_equal(result,resultA))))
})


