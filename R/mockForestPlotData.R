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

mockForestPlotData <- function(seed = 1, numVars = 8) {
  set.seed(seed = seed)
  rows1 <- dplyr::tibble(
    group_name = "Cohort_name",
    group_level = "Cohort 1",
    strata_name = "Overall",
    strata_level = "Overall",
    variable = paste0("Variable ", 1:numVars),
    variable_level = NA,
    variable_type = "numeric",
    estimate = round((stats::runif(n = numVars, min = 0.2, max = 0.6)),2),
    estimate_type = "Central value",
    se = round((stats::runif(n = numVars, min = 0.1, max = 0.25)),2)
  )
  rows2 <- rows1 |> dplyr::mutate(estimate = round(exp(estimate - 1.96*se),2)) |> dplyr::mutate(estimate_type = "Lower 95% CI")
  rows3 <- rows1 |> dplyr::mutate(estimate = round(exp(estimate + 1.96*se),2)) |> dplyr::mutate(estimate_type = "Higher 95% CI")
  rows1 <- rows1 |> dplyr::mutate(estimate = round(0.5*(exp(estimate + 1.96*se)+exp(estimate - 1.96*se)),2))
  result <- rbind(rows1,rows2,rows3)
  result <- result |> dplyr::select(-c("se")) |> dplyr::arrange(variable)
  return(result)
}
