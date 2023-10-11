#' getBarChart summary
#'
#' @param tab dataframe
#' @param cohort cohort
#' @param var variable that is going to be for the bar chart
#' @param color color
#'
#' @return plot
#' @export
#'
#' @examples
getBarChart <- function(data,
                        group_name   = 'cohort_name'
                        group_level  = 'Cohort 1'
                        strata_name  = 'Overall'
                        strata_level = 'Overall'
                        variable){

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
    dplyr::filter(.data$group_name == .env$group_name) %>%
    dplyr::filter(.data$group_level == .env$group_level) %>%
    dplyr::filter(.data$strata_name == .env$strata_name) %>%
    dplyr::filter(.data$strata_level == .env$strata_level) %>%
    dplyr::filter(.data$estimate_type == "count") %>%
    dplyr::filter(.data$variable == .env$var)


  p <- ggplot2::ggplot(tab, ggplot2::aes(x = .data$variable_level)) +
    ggplot2::geom_bar()

  return(p)
}
