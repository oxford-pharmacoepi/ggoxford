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

  data_check <- any(class(tab) %in% c("tbl_df", "data.frame","tibble"))
  checkmate::assertTRUE(data_check, add = errorMessage)
  if (!isTRUE(data_check)) {
    errorMessage$push(glue::glue("- {tab} is not a tibble or dataframe"))
  }

  var_check <- c(var) %in% colnames(tab)
  checkmate::assertTRUE(var_check, add = errorMessage)
  if (!isTRUE(var_check)){
    errorMessage$push(glue::glue("- {var} is not a column of the dataframe"))
  }

  checkmate::reportAssertions(collection = errorMessage)

  p <- ggplot2::ggplot(tab, ggplot2::aes(var)) +
    ggplot2::geom_bar()
  return(p)
}
