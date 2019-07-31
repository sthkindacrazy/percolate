library(assertthat)
library(testthat)
library(tidyverse)

#' Title generate_board_mat
#'
#' @param n a positive integer denote the size of the board
#' @param p a number between 0 and 1 that denotes the fraction of the `n^2` squares are blocked
#'
#' @return matrix with random (0, 1) (n^2 size with n^2 * p numbers of blocked(1), rest (0))
#' @export
#'
#' @examples 0 is blocked(black) 1 is non-blocked(white)
generate_board_mat <- function(n = 5, p = 0.25) {
  assert_that(is.numeric(n))
  assert_that(is.numeric(p))
  assert_that(n %% 1  == 0)
  assert_that(n > 0)
  assert_that(p >= 0 & p <= 1)

  blocked <- floor(n^2 * p)
  non_blocked <- n^2 - blocked
  gen_blocks <- sample(c(rep(0, blocked), rep(1, non_blocked)))
  return(matrix(gen_blocks, nrow = n, ncol = n))
}

#' Title
#'
#' @param mat matrix
#'
#' @return error if matrix does not qualifies, return TRUE if pass
#' @export
#'
#' @examples is_valid(generate_board_mat())
is_valid <- function(mat) {
  # check matrix
  assert_that(is.matrix(mat))
  # square matrix
  assert_that(dim(mat)[1] == dim(mat)[2])
  # values are all 0, 1, 2
  assert_that(sum(mat %in% c(0, 1, 2)) == dim(mat)[1]^2)
  return(TRUE)
}
