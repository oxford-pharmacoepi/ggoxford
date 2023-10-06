test_that("hello works", {
  expect_no_error(mockSummarisedResult())
  expect_s3_class(mockSummarisedResult(), class = "tbl")
})
