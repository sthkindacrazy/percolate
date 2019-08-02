#' generate_board_mat
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

#' is_valid
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

#' read_boards
#'
#' @param file denote the text file that contains our boards
#'
#' @return board class objects, if lines do not meet these specifications, return NA
#' @export
#'
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")
read_boards <- function(file) {
  lines <- readLines(file)
  # paste with arbitrary separator ','
  lines <- lines %>% trimws() %>% .[. != ""]  %>% paste(collapse = " ")
  boards <- extract_boards(lines)
  return(boards)
}

#' extract_boards
#'
#' @param lines lines extracted from the file
#'
#' @return board class objects, if lines do not meet these specifications, return NA
#' @export
#'
#' @examples
#' lines <- readLines(file)
#' lines <- lines %>% trimws() %>% .[. != ""]  %>% paste(collapse = ",")
#' extract_boards(lines)
extract_boards <- function(lines) {
  # split with ----
  n_board_lines <- lines %>% strsplit("----") %>% unlist() %>% .[. != ""]
  return (lapply(n_board_lines, create_board))
}

#' create_board, create single board from single_board_lines specifically between '----'
#'
#' @param single_board_lines
#'
#' @return single board objects
#' @export
#'
#' @examples
create_board <- function(single_board_lines) {
  single_board_lines <- single_board_lines %>% strsplit(" ") %>% unlist() %>% .[. != ""]
  size_n <- as.numeric(single_board_lines[1])
  board_elements <- single_board_lines[-1]
  if (!is_valid_board_lines(size_n, board_elements)) {
    return(NA)
  } else {
    board_elements <- ifelse(board_elements == "*", 0, 1)
    mat <- matrix(board_elements, nrow = size_n, byrow = TRUE)
    return(board(mat = mat))
  }
}

#' is_valid_board_lines, check whether lines between '----' are valid
#'
#' @param n first element of single_board_lines, supposed to be a size n of a board
#' @param board_elements characters
#'
#' @return TRUE if valid, FALSE if invalid
#' @export
#'
#' @examples
is_valid_board_lines <- function(n , board_elements) {
  if (is.na(n)) return(FALSE)
  if (is.null(n)) return(FALSE)
  if (!is.numeric(n)) return(FALSE)
  if (n <= 0) return(FALSE)
  if (n %% 1 != 0) return(FALSE)
  if (n^2 != length(board_elements)) return(FALSE)
  if (!all(board_elements %in% c("*", "."))) return(FALSE)
  TRUE
}

