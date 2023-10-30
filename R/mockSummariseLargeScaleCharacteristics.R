#' Function to create mock summarised large-scale characteristics.
#'
#' @param patient_size Number of patients
#' @param window Set of temporal windows to consider
#' @param eventInWindow Table to characterise the events in the window
#' @param episodeInWindow Table to characterise the episodes in the window
#' @export
#'
#' @examples
#' \donttest{
#' library(ggoxford)
#'
#' mockSummariseLargeScaleCharacteristics()
#' }
#'
mockSummariseLargeScaleCharacteristics <- function(patient_size = 10,
                                                   window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(1, 30), c(31, 365), c(366, Inf)),
                                                   eventInWindow = "condition_occurrence",
                                                   episodeInWindow = "drug_exposure"){
  mock <- DrugUtilisation::mockDrugUtilisation()
  mock$cohort1 |>
    PatientProfiles::summariseLargeScaleCharacteristics(
      window = window,
      eventInWindow = eventInWindow,
      episodeInWindow = episodeInWindow,
      minCellCount = 0,
      minimumFrequency = 0
    )
}
