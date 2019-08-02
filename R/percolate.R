#' Title percolate.board
#'
#' @param start_board board(matrix) of initial stage
#'
#' @return list of result_board(after the percolate end), and result whether it is percolate or not
#' @export
#'
#' @examples mat_example_list <- list(matrix(c(1,1,1,1,0,
#' 0,0,0,1,0,
#' 1,1,1,1,0,
#' 0,1,0,0,0,
#' 0,1,1,1,1), 5, 5),
#' matrix(c(1,1,1,1,0,
#'         0,0,0,1,0,
#'         0,1,1,1,0,
#'         0,1,0,0,0,
#'         0,1,1,1,1), 5, 5),
#' matrix(c(1,1,1,1,0,
#'         0,0,0,1,0,
#'         0,1,1,0,0,
#"         0,1,0,0,0,
#'         0,1,1,1,1), 5, 5))
#' ## turn matrices to board
#' example_boards <- lapply(mat_example_list, board)
#' ## apply boards to percolate.board
#' laply(example_boards, percolate.board)
percolate.board <- function(start_board) {
  is_valid(start_board)
  assert_that(all(start_board %in% c(0,1,2)))
  n <- attr(start_board, "n")
  result_board <- flow(start_board)
  # percolate if one of the bottom cell is 2
  result <- 2 %in% result_board[n, ]
  return(list(result_board = result_board, result = result))
}

#' Title percolate
#'
#' @param start_board
#'
#' @return percolate.board result (since there is no percolate for matrix,
#'         board inherited from matrix should call percolate instead)
#' @export
#'
#' @examples percolate(board())
percolate <- function(start_board) {
  UseMethod("percolate")
}

#' Title flow
#'
#' @param mat takes board(matrix) and start percolate from first row
#'
#' @return mat (class board, matrix) after the flow ends and reflect all the flooded cell
#' @export
#'
#' @examples flow(board())
flow <- function(mat) {
  n <- attr(mat, "n")
  for (i in 1:n) {
    mat <- flooded(mat,1,i)
  }
  return(mat)
}

#' Title flooded
#'
#' @param mat board(matrix)
#' @param nr  row index
#' @param nc  column index
#'
#' @return board(matrix) if cell of given row index, and column index percolates
#' @export
#'
#' @examples flooded(board(), 1, 1)
flooded <- function(mat, nr, nc) {
  n <- attr(mat, "n")
  if (nr < 1 | nr > n) {
    return(mat)
  }
  if (nc < 1 | nc > n) {
    return(mat)
  }
  if (mat[nr, nc] == 0) {
    return(mat)
  }
  if (mat[nr, nc] == 2) {
    return(mat)
  }
  mat[nr, nc] = 2
  mat <- flooded(mat, nr+1, nc)
  mat <- flooded(mat, nr, nc + 1)
  mat <- flooded(mat, nr, nc - 1)
  mat <- flooded(mat, nr-1, nc)
  return(mat)
}
