#' plot.board
#'
#' @param x matrix
#'
#' @return visualize the matrix with 0 = black, 1 = white, 2 = ligtblue
#' @export
#' @import ggplot2
#' @import tidyr
#' @examples run plot(board())
plot.board <- function(x, grid = FALSE) {
  is_valid(x)
  n <- attr(x, "n")
  gen_board <-tidyr::gather(data.frame(row = 1:n, x), key = "column", value = "value", -row)
  gen_board$column <- as.numeric(substr(gen_board$column, 2, nchar(gen_board$column)))
  gen_board$value <- factor(gen_board$value, levels = c(0,1,2))
  plot_board <- ggplot(gen_board, aes(x = column, y = max(row)-row + 1))
  if(grid) {
    plot_board <- plot_board + geom_tile(aes(fill=value), color="black", linetype = "dashed" )
  } else {
    plot_board <- plot_board + geom_tile(aes(fill=value))
  }

  plot_board + scale_fill_manual(values = c("0" = "black", "1" = "white", "2" = "lightblue3")) +
    labs(y = "row", x="col", title = paste("size ", n)) +
    theme(legend.position = "none") + theme_void() + theme(plot.title = element_text(hjust = 0.5))
}
