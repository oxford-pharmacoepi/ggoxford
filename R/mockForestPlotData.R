#' Function to create mock forest plot data.
#'
#' @param seed Seed value for the random mock data generated
#' @param numRows Number of rows
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
mockForestPlotData <- function(seed = 1, numRows = 10) {
  set.seed(seed = seed)
  res <- dplyr::tibble(
    group_name = paste0("Outcome ", 1:numRows),
    group_level = "Cohort 1",
    strata_name = "Overall",
    strata_level = "Overall",
    variable = paste0("Variable ", 1:numRows),
    variable_level = NA,
    variable_type = "numeric",
    estimate = paste0(round((stats::runif(n = numRows, min = 0.51, max = 3.01)),2)," ± ",(round((stats::runif(n = numRows, min = 0.25, max = 0.75)),2))),
    estimate_type = "95% confidence interval"
  )
  return(res)
}
