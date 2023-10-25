#' getBarChart summary
#'
#' @param data Input data.
#' @param variable Variable to prepare the barchart.
#' @param groupName Group name to use.
#' @param groupLevel Group level to use.
#' @param strataName Strata name to use.
#' @param strataLevel Strata levels to use.
#'
#' @return plot
#' @export
#'
#' @examples
plotBarChart <- function(data,
                         variable,
                         groupName = "cohort_name",
                         groupLevel = "Cohort 1",
                         strataName = "Overall",
                         strataLevel = "Overall") {
  # errorMessage <- checkmate::makeAssertCollection()
  #
  # data_check <- any(class(tab) %in% c("tbl_df", "data.frame","tibble"))
  # checkmate::assertTRUE(data_check, add = errorMessage)
  # if (!isTRUE(data_check)) {
  #   errorMessage$push(glue::glue("- {tab} is not a tibble or dataframe"))
  # }
  #
  # cohort_check <- cohort %in% c('Cohort 1','Cohort 2')
  # checkmate::assertTRUE(cohort_check, add = errorMessage)
  # if (!isTRUE(cohort_check)){
  #   errorMessage$push(glue::glue("- {cohort} has an invalid value"))
  # }
  #
  # var_check1 <- var %in% c('age_group','sex')
  # checkmate::assertTRUE(var_check1, add = errorMessage)
  # if (!isTRUE(var_check1)){
  #   errorMessage$push(glue::glue("- {var_check1} has an invalid value"))
  # }
  #
  # checkmate::reportAssertions(collection = errorMessage)

  tab <- data %>%
    dplyr::filter(.data$group_name == .env$groupName) %>%
    dplyr::filter(.data$group_level == .env$groupLevel) %>%
    dplyr::filter(.data$strata_name == .env$strataName) %>%
    dplyr::filter(.data$strata_level == .env$strataLevel) %>%
    dplyr::filter(.data$estimate_type == "count") %>%
    dplyr::filter(.data$variable == .env$variable)


  p <- ggplot2::ggplot(tab, ggplot2::aes(x = .data$variable_level)) +
    ggplot2::geom_bar()

  return(p)
}
