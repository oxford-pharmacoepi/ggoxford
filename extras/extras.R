
data <- ggoxford::mockSummarisedResult()
library(dplyr)

group_name <- 'cohort_name'
group_level <- 'Cohort 1'
strata_name <- 'Overall'
strata_level <- 'Overall'
variable <- 'age_group'

tab <- data %>%
  dplyr::filter(.data$group_name == .env$group_name) %>%
  dplyr::filter(.data$group_level == .env$group_level) %>%
  dplyr::filter(.data$strata_name == .env$strata_name) %>%
  dplyr::filter(.data$strata_level == .env$strata_level) %>%
  dplyr::filter(.data$estimate_type == "count") %>%
  dplyr::filter(.data$variable == .env$var)


ggplot2::ggplot(tab, ggplot2::aes(x = .data$variable_level)) +
  ggplot2::geom_bar()
