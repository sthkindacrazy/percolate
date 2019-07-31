#' Title generate_board_mat
#'
#' @param n a positive integer denote the size of the board
#' @param p a number between 0 and 1 that denotes the fraction of the `n^2` squares are blocked 
#'
#' @return matrix with random (0, 1) (n^2 size with n^2 * p numbers of blocked(1), rest (0))
#' @export
#'
#' @examples
generate_board_mat <- function(n = 5, p = 0.25) {
  assert_that(is.numeric(n))
  assert_that(is.numeric(p))
  assert_that(n > 0)
  assert_that(p >= 0 & p <= 1)
  
  blocked <- floor(n^2 * p)
  non_blocked <- n^2 - blocked
  gen_blocks <- sample(c(rep(1, blocked), rep(0, non_blocked)))
  return(matrix(gen_blocks, nrow = n, ncol = n))
}