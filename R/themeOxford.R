#' Function to create a ggplot theme
#'
#'
#' @param plot ggplot
#' @param base_size base font size, given in pts.
#'
#' @return ggplot
#'
#' @examples
#' \donttest{
#' library(ggoxford)
#'
#' plot +
#'  themeOxford()
#' }
#'
themeOxford <- function(plot,
                        base_size = 8) {

  errorMessage <- checkmate::makeAssertCollection()
  plot_check<-ggplot2::is.ggplot(plot)
  checkmate::assertTRUE(plot_check, add =errorMessage)
  if (!isTRUE(plot_check)) {
    errorMessage$push(glue::glue("- {data} is not a ggplot"))
  }

  base_size_check<-any(class(base_size) %in%
                    c("number"))
  checkmate::assertTRUE(data_check, add =errorMessage)
  if (!isTRUE(base_size_check)) {
    errorMessage$push(glue::glue("- Base size is not a number"))
  }

  plot <- plot+
          ggplot2::theme(
                plot.title=ggplot2::element_text(size= base_size+4),
                axis.title.x = ggplot2::element_text(size = base_size+2),
                axis.text.x = ggplot2::element_text(size = base_size),
                axis.title.y = ggplot2::element_text(size = base_size+2),
                panel.border = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                axis.line = ggplot2::element_line(colour = "black"))

  return(plot)
}



