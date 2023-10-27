#' Function to create mock summarised large-scale characteristics.
#'
#' @param seed Seed value for the random mock data generated
#' @param table_name Name of table from cdm object
#' @param concept Set of concept ids
#' @export
#'
#' @examples
#' \donttest{
#' library(ggoxford)
#'
#' mockSummariseLargeScaleCharacteristics()
#' }
#'
mockSummariseLargeScaleCharacteristics <- function(seed=1,table_name="condition_occurrence", concept = c(317009,4266367,378253,1125315,1503328,1516978)) {
  set.seed(seed = seed)
  for (i in concept){
    row1 <- dplyr::tibble(
      result_type = "Summarised Large Scale Characteristics",
      cdm_name = "PP_MOCK",
      group_name = "Cohort_name",
      group_level = "Cohort 1",
      strata_name = "Overall",
      strata_level = "Overall",
      table_name = table_name,
      type = "event",
      analysis = "standard",
      concept = i,
      variable = paste0("concept: ", i),
      variable_level = sample(c("-inf to -366","-365 to -31","-30 to -1","0 to 30","31 to 365","366 to inf"),size=1),
      estimate_type = "count",
      estimate = round(sample.int(3,1),0)
    )
    row2 <- row1 |> dplyr::mutate(estimate_type = "percentage")
    row2 <- row2 |> dplyr::mutate(estimate = (round((estimate/3)*100,digits=2)))

    if (i==concept[1]){
      result <- rbind(row1,row2)}
    else{
      result <- rbind(result,row1,row2)}
  }
  return(result)
}
