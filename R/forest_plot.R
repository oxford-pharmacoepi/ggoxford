
forest_plot<-function(data, outcome_labels, estimate, lowCI, highCI){

data %>%
  ggplot2::ggplot(aes(y=.data[[outcome_labels]], x=.data[[estimate]], xmin=.data[[lowCI]], xmax=.data[[highCI]])) +
                  geom_pointrange() +
                  geom_vline(xintercept=1, lty=2) +  # add a dotted line at x
                  ylab("Outcomes") + xlab("Value (95% CI)") +
                  theme_bw()

}
