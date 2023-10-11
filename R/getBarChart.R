#' getBarChart summary
#'
#' @param tab dataframe
#' @param var variable that is going to be for the bar chart
#'
#' @return plot
#' @export
#'
#' @examples
getBarChart <- function(tab, var){
  errorMessage <- checkmate::makeAssertCollection()
  data_check<-any(class(tab) %in%
                    c("tbl_df", "data.frame","tibble"))
  checkmate::assertTRUE(data_check, add =errorMessage)
  if (!isTRUE(data_check)) {
    errorMessage$push(glue::glue("- {tab} is not a tibble or dataframe"))
  }

  p <- ggplot2::ggplot(tab, ggplot2::aes(var)) +
    ggplot2::geom_bar()
  return(p)
}
