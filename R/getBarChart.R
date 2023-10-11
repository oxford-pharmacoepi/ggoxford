getBarChart <- function(tab, var){
  errorMessage <- checkmate::makeAssertCollection()
  data_check<-any(class(tab) %in%
                    c("tbl_df", "data.frame","tibble"))
  checkmate::assertTRUE(data_check, add =errorMessage)
  if (!isTRUE(data_check)) {
    errorMessage$push(glue::glue("- {tab} is not a tibble or dataframe"))
  }

  ggplot2::ggplot(tab, aes(var)) + geom_bar()
}
