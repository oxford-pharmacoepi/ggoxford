
forest_plot<-function(data, outcome_labels, estimate, lowCI, highCI){

data %>%
  ggplot2::ggplot(ggplot2::aes(y=.data[[outcome_labels]], x=.data[[estimate]], xmin=.data[[lowCI]], xmax=.data[[highCI]])) +
                  ggplot2::geom_pointrange() +
                  ggplot2::geom_errorbar(ggplot2::aes(xmin=.data[[lowCI]], xmax=.data[[highCI]]), width=0.35, cex=0.5)+ # Makes whiskers on the range (more aesthetically pleasing)
                  ggplot2::geom_vline(xintercept=1, lty=2) +  # add a dotted line at x
                  ggplot2::ylab("Outcomes") + ggplot2::xlab("Value (95% CI)") +
                  ggplot2::theme_bw()

}

