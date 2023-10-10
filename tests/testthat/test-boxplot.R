test_that("make a boxplot, expect error",{

  data <- mockSummarisedResult()
  expect_error(boxplot(data, variablesToPlot = "sex"))
  expect_error(boxplot(data, variablesToPlot = "age_group"))
  expect_error(boxplot(data, variablesToPlot = "blood_type"))
  expect_error(boxplot(data, variablesToPlot = "blood_type"))

})
