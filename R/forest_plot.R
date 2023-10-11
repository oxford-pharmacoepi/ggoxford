
#' Forest Plot - Oxford-Pharmacoepi style
#'
#' @param data: Inlcude your data frame.
#' @param "outcome_labels": name of your column including the  labels. It has to go within " ".
#' @param "estimate": include name of the column with RR, IR, HR, etc. It has to go within " ".
#' @param "lowCI": include name of the column with the low 95% confidence interval value. It has to go within " ".
#' @param "highCI": include name of the column with the high 95% confidence interval value. It has to go within " ".
#'
#' @return
#' @export
#'
#' @examples

forest_plot<-function(data, outcome_labels, estimate, lowCI, highCI){

data %>%
  ggplot2::ggplot(ggplot2::aes(y=.data[[outcome_labels]], x=.data[[estimate]], xmin=.data[[lowCI]], xmax=.data[[highCI]])) +
                  ggplot2::geom_pointrange() +
                  ggplot2::geom_errorbar(ggplot2::aes(xmin=.data[[lowCI]], xmax=.data[[highCI]]), width=0.35, cex=0.5)+ # Makes whiskers on the range (more aesthetically pleasing)
                  ggplot2::geom_vline(xintercept=1, lty=2) +  # add a dotted line at x
                  ggplot2::ylab("Outcomes") + ggplot2::xlab("Value (95% CI)") +
                  ggplot2::theme_bw()

}

