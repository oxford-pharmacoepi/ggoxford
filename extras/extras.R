
#data <- ggoxford::mockSummarisedResult()
library(dplyr)

tab <- data
cohort <- c('Cohort 1')
strata1 <- 'age_group'
color <- 'grey'

tab <- tab %>% filter(group_level == cohort)
if(strata1 == 'age_group'){
  tab <- tab %>% filter(strata_name == 'age_group')
  p <- ggplot2::ggplot(tab, ggplot2::aes(strata_level)) +
    ggplot2::geom_bar(fill = color) +
    ggplot2::xlab('Age group')
}else if(strata1 == 'sex'){
  tab <- tab %>% filter(strata_name == 'sex')
  p <- ggplot2::ggplot(tab, ggplot2::aes(strata_level)) +
    ggplot2::geom_bar(fill = color) +
    ggplot2::xlab('Sex')
}

p
