#' Forest Plot - Oxford-Pharmacoepi style
#'
#' @param data: Include your data frame.
#' @param "outcome_labels": name of your column including the  labels. It has to go within " ".
#' @param "estimate": include name of the column with RR, IR, HR, etc. It has to go within " ".
#' @param "lowCI": include name of the column with the low 95% confidence interval value. It has to go within " ".
#' @param "highCI": include name of the column with the high 95% confidence interval value. It has to go within " ".
#'
#' @return
#' @export
#'
#' @examples
forestPlot <- function(data, outcomelabels, estimate, lowCi, highCi) {
  errorStorage <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = errorStorage)
  checkmate::assert_character(outcomelabels, add = errorStorage)
  checkmate::assert_character(lowCi, add = errorStorage)
  checkmate::assert_character(highCi, add = errorStorage)
  checkmate::assert_numeric(data[[lowCi]], add = errorStorage)
  checkmate::assert_numeric(data[[highCi]], add = errorStorage)
  checkmate::reportAssertions(errorStorage)

  data %>%
    ggplot2::ggplot(ggplot2::aes(y = .data[[outcomelabels]], x = .data[[estimate]], xmin = .data[[lowCi]], xmax = .data[[highCi]])) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data[[lowCi]], xmax = .data[[highCi]]), width = 0.35, cex = 0.5) + # Makes whiskers on the range (more aesthetically pleasing)
    ggplot2::geom_vline(xintercept = 1, lty = 2) + # add a dotted line at x
    ggplot2::ylab("Outcomes") +
    ggplot2::xlab("Value (95% CI)") +
    ggplot2::theme_bw()
}
