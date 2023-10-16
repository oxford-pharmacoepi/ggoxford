#' Boxplot ggOxford
#'
#' @param data dataset
#' @param group_level group_level in dataset
#' @param strata_name strata_name in dataset
#' @param strata_level strata_level in dataset
#' @param variable variable that the box plot should plot over
#'
#' @return a box plot
#' @export
#'
#' @examples
box_plot <- function(data, group_level = "Cohort 1", strata_name = "Overall", strata_level = "Overall", variable = "age") {
  errorStorage <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = errorStorage)
  checkmate::assert_character(group_level, add = errorStorage)
  checkmate::assert_character(strata_name, add = errorStorage)
  checkmate::assert_character(strata_level, add = errorStorage)
  checkmate::assert_character(variable, add = errorStorage)
  # checkmate::assert_true()
  checkmate::reportAssertions(errorStorage)
  subdata <- data %>%
    dplyr::filter(.data$group_level == .env$group_level) %>%
    dplyr::filter(.data$strata_name == .env$strata_name) %>%
    dplyr::filter(.data$strata_level == .env$strata_level) %>%
    dplyr::filter(.data$variable == .env$variable)

  subdata_min <- subdata %>%
    dplyr::filter(.data$estimate_type == "min") %>%
    dplyr::pull(.data$estimate)

  subdata_25 <- subdata %>%
    dplyr::filter(.data$estimate_type == "q25") %>%
    dplyr::pull(.data$estimate)

  subdata_med <- subdata %>%
    dplyr::filter(.data$estimate_type == "median") %>%
    dplyr::pull(.data$estimate)

  subdata_75 <- subdata %>%
    dplyr::filter(.data$estimate_type == "q75") %>%
    dplyr::pull(.data$estimate)

  subdata_max <- subdata %>%
    dplyr::filter(.data$estimate_type == "max") %>%
    dplyr::pull(.data$estimate)

  plot <- ggplot2::ggplot(subdata, ggplot2::aes(xmin = subdata_min, lower = subdata_25, middle = subdata_med, upper = subdata_75, xmax = subdata_max)) +
    ggplot2::geom_boxplot()
  return(plot)
}
