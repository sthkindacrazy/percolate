#' Title board
#'
#' @param mat takes matrix default NULL
#' @param n defines size of matrix when mat is NULL
#' @param p a number between 0 and 1 that denotes the fraction of the `n^2` squares are blocked
#'
#' @return board class objects, if given matrix is incorrect, it returns errors
#' @export
#'
#' @examples
board <- function(mat = NULL, n = 5, p = 0.25) {
  if (is.null(mat)) {
    object = list(mat = generate_board_mat(n, p),
                  n = n,
                  p = p)
  } else {
    is_valid(mat)
    object = list(mat = mat,
                  n = dim(mat)[1],
                  p = (sum(mat == 0) / dim(mat)[1]^2) )
  }
  class(object) <- "board"
  object
}
