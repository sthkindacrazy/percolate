# 1
test_that("all open sites", {
  my_board <- board(n = 10, p = 0)
  res <- percolate(my_board)
  test_board <- board(n = 10, p = 0)
  test_board[,] <- 2 # make all 2
  expect_equal(res$result_board, test_board)
  expect_equal(res$result , TRUE)
})

# 2
test_that("all blocked", {
  my_board <- board(n = 10, p = 1)
  res <- percolate(my_board)
  test_board <- board(n = 10, p = 1)
  test_board[,] <- 0 # make all 0
  expect_equal(res$result_board, test_board)
  expect_equal(res$result , FALSE)
})

# 3
test_that("top row blocked", {
  my_board <- board(n = 10)
  my_board[1,] <- 0 #block top row
  res <- percolate(my_board)
  expect_equal(res$result_board, my_board)
  expect_equal(res$result, FALSE)
})

# 4
test_that("bottom row blocked", {
  my_board <- board(n = 10, p = 0)
  my_board[10, ] <- 0 #block bottom row
  res <- percolate(my_board)
  my_board[my_board == 1] <- 2
  expect_equal(res$result_board, my_board)
  expect_equal(res$result, FALSE)
})

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))

  your_result_list <- lapply(board_list, percolate)

  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board

    identical(your_board, result_board) *
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })
  expect_true(all(bool_vec))
})
