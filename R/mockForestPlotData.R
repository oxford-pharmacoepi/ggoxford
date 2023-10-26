#' Function to create mock forest plot data.
#'
#' @param seed Seed value for the random mock data generated
#' @param numVars Number of variables
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(ggoxford)
#'
#' mockForestPlotData()
#' }
#'
mockForestPlotData <- function(seed = 1, numVars = 10) {
  set.seed(seed = seed)
  for (i in 1:numVars){
  row1 <- dplyr::tibble(
    group_name = "Cohort_name",
    group_level = "Cohort 1",
    strata_name = "Overall",
    strata_level = "Overall",
    variable = paste0("Variable ", i),
    variable_level = NA,
    variable_type = "numeric",
    estimate = round((stats::runif(n = 1, min = 0.51, max = 3.01)),2),
    estimate_type = "Central value"
  )
  row2 <- row1 |> dplyr::mutate(estimate = estimate - round((stats::runif(n = 1, min = 0.25, max = 0.75)),2))
  row2 <- row2 |> dplyr::mutate(estimate_type = "Lower 95% CI")
  row3 <- row1 |> dplyr::mutate(estimate = estimate + round((stats::runif(n = 1, min = 0.25, max = 0.75)),2))
  row3 <- row3 |> dplyr::mutate(estimate_type = "Higher 95% CI")

  if (i==1){
    result <- rbind(row1,row2,row3)}
  else{
    result <- rbind(result,row1,row2,row3)}
  }
  return(result)
}
