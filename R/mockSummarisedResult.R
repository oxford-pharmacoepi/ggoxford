#' Function to create a mock summarised result tibble.
#'
#' @param seed Seed value for the random mock data generated
#' @param startDate Start date for the mock data
#' @param endDate End date for the mock data
#' @param populationSize choose size of the population
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(ggoxford)
#'
#' mockSummarisedResult(seed = 1,as.Date("2021-01-01"),as.Date("2021-12-31"))
#' }
#'
mockSummarisedResult <- function(seed = 1, startDate, endDate, populationSize=10) {
  errorMessage <- checkmate::makeAssertCollection()
  data_check <- checkmate::assertNumber(populationSize, add =errorMessage)
  seed_check <- checkmate::assertNumber(seed, add =errorMessage)
  checkmate::reportAssertions(collection = errorMessage)
  set.seed(seed = seed)
  res <- dplyr::tibble(
    subject_id = 1:populationSize,
    cohort_start_date = as.Date(
      stats::runif(n = populationSize, min = 0, max = as.numeric(.env$endDate - .env$startDate)), origin = as.Date(.env$startDate)
    ),
    age = sample(x = 0:80, size = populationSize, replace = TRUE),
    sex = sample(x = c("Male", "Female"), size = populationSize, replace = TRUE),
    cohort_name = sample(x = c("Cohort 1", "Cohort 2"), size = populationSize, replace = TRUE),
    blood_type = sample(x = c("0", "a", "b", "ab"), size = populationSize, replace = TRUE)
  ) |>
    dplyr::mutate(cohort_end_date = .data$cohort_start_date + stats::runif(
      n = populationSize, min = 0, max = as.numeric(.env$endDate - .data$cohort_start_date)
    )) |>
    PatientProfiles::addCategories(
      variable = "age",
      categories = list("age_group" = list(
        c(0, 19), c(20, 39), c(40, 59), c(60, 79), ">=80" = c(80, Inf)
      ))
    ) |>
    dplyr::select(
      "cohort_name", "subject_id", "cohort_start_date", "cohort_end_date",
      "age", "sex", "age_group", "blood_type"
    ) |>
    PatientProfiles::summariseResult(
      group = list("cohort_name"),
      includeOverallGroup = FALSE,
      strata = list("age_group", "sex"),
      includeOverallStrata = TRUE,
      minCellCount = 1
    )
  return(res)
}
