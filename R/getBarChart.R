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
getBarChart <- function(tab, cohort, var = c('age_group'), color = 'grey'){
  errorMessage <- checkmate::makeAssertCollection()

  data_check <- any(class(tab) %in% c("tbl_df", "data.frame","tibble"))
  checkmate::assertTRUE(data_check, add = errorMessage)
  if (!isTRUE(data_check)) {
    errorMessage$push(glue::glue("- {tab} is not a tibble or dataframe"))
  }

  cohort_check <- cohort %in% c('Cohort 1','Cohort 2')
  checkmate::assertTRUE(cohort_check, add = errorMessage)
  if (!isTRUE(cohort_check)){
    errorMessage$push(glue::glue("- {cohort} has an invalid value"))
  }

  var_check1 <- var %in% c('age_group','sex')
  checkmate::assertTRUE(var_check1, add = errorMessage)
  if (!isTRUE(var_check1)){
    errorMessage$push(glue::glue("- {var_check1} has an invalid value"))
  }

  checkmate::reportAssertions(collection = errorMessage)


  tab <- tab %>% dplyr::filter(.data$group_level == cohort)
  if(var == 'age_group'){
    tab <- tab %>% dplyr::filter(.data$strata_name == 'age_group')
    p <- ggplot2::ggplot(tab, ggplot2::aes(.data$strata_level)) +
      ggplot2::geom_bar(fill = color) +
      ggplot2::xlab('Age group')
  }else if(var == 'sex'){
    tab <- tab %>% dplyr::filter(.data$strata_name == 'sex')
    p <- ggplot2::ggplot(tab, ggplot2::aes(.data$strata_level)) +
      ggplot2::geom_bar(fill = color) +
      ggplot2::xlab('Sex')
  }

  p <- ggplot2::ggplot(tab, ggplot2::aes(var)) +
    ggplot2::geom_bar()
  return(p)
}
