
data <- ggoxford::mockSummarisedResult()
tab <- data
var <- "group_level"
strata_name <- "group_level"

ggplot2::ggplot(tab, ggplot2::aes(var)) +
  ggplot2::geom_bar(fill = "strata_name")
