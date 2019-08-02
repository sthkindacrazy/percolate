test_that("equivalent matrix", {
  t_board <- board()
  expect_equivalent(t_board, board(t_board))
  expect_equivalent(unclass(t_board), unclass(board(t_board)))
})

test_that("check empirical values are correct for default", {
  t_mat <- generate_board_mat()
  t_board <- board(mat = t_mat)
  expect_equal(attr(t_board, "n"), 5)
  expect_equal(attr(t_board, "p") * attr(t_board, "n")^2, floor(0.25 * 25))
})

test_that("check incorrect matrix errors", {
  # not matrix
  expect_error(board(mat = c(0,1)))
  # not square
  expect_error(board(mat = matrix(nrow = 12, ncol = 3)))
  # not values in 0,1,2
  e_mat <- matrix(1:36, nrow = 6, ncol = 6)
  expect_error(board(mat = e_mat))
})

test_that("check board class with some n, p", {
  t_mat <- generate_board_mat(n = 6)
  t_board <- board(mat = t_mat)
  expect_equal(attr(t_board, "n"), 6)
  expect_equal(attr(t_board, "p") * attr(t_board, "n")^2, floor(0.25 * 36))

  t_mat2 <- generate_board_mat(n = 6, p = 0.35)
  t_board2 <- board(mat = t_mat2)
  expect_equal(attr(t_board2, "n"), 6)
  expect_equal(attr(t_board2, "p") * attr(t_board, "n")^2, floor(0.35 * 36))
})
