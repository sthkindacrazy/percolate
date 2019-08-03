#' board
#'
#' @param mat takes matrix default NULL
#' @param n defines size of matrix when mat is NULL
#' @param p a number between 0 and 1 that denotes the fraction of the `n^2` squares are blocked
#'
#' @return board class objects, if given matrix is incorrect, it returns error
#' @export
#'
#' @examples board
#'      [,1] [,2] [,3] [,4] [,5]
#'[1,]    1    0    1    1    1
#'[2,]    1    0    1    1    1
#'[3,]    0    1    1    1    1
#'[4,]    1    0    0    1    1
#'[5,]    1    1    0    1    1
#'attr(,"class")
#'[1] "matrix" "board"
#'attr(,"n")
#'[1] 5
#'attr(,"p")
#'[1] 0.25
board <- function(mat = NULL, n = 5, p = 0.25) {
  if (is.null(mat)) {
    object <- generate_board_mat(n, p)
    class(object) <- c("matrix", "board")
    attr(object, "n") <- n
    attr(object, "p") <- p
  } else {
    is_valid(mat)
    object <- mat
    class(object) <- c("matrix", "board")
    attr(object, "n") <- dim(mat)[1]
    attr(object, "p") <- (sum(mat == 0) / dim(mat)[1]^2)
  }
  object
}
