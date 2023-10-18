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
    dplyr::filter(.data$variable == .env$variable) %>%
    dplyr::select("variable", "estimate_type", "estimate") %>%
    dplyr::mutate(estimate = as.numeric(.data$estimate)) %>%
    tidyr::pivot_wider(names_from = "estimate_type", values_from = "estimate")

  plot <- ggplot2::ggplot(
    subdata,
    ggplot2::aes(
      ymin = .data$min, lower = .data$q25, middle = .data$median,
      upper = .data$q75, ymax = .data$max, x = .data$variable
    )
  ) +
    ggplot2::geom_boxplot(stat = "identity", width = 0.4, fill = "lightblue", color = "blue") +
    ggplot2::labs(
      title = "Boxplot",
      x = "Variable",
      y = "Values"
    )

  return(plot)
}
