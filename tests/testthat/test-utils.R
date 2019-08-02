test_that("default matrix is 5 by 5 with only 0, 1, 2", {
  mat <- generate_board_mat()
  expect_equal(dim(mat)[1], 5)
  expect_equal(dim(mat)[2], 5)
  expect_equal(sum(mat %in% c(0, 1, 2)), 25)
})

test_that("check with matrix n = 6", {
  mat <- generate_board_mat(n = 6)
  expect_equal(dim(mat)[1], 6)
  expect_equal(dim(mat)[2], 6)
  expect_equal(sum(mat %in% c(0, 1, 2)), 36)
})

test_that("p = 0 gives a board with all 1", {
  mat <- generate_board_mat(p = 0)
  expect_equal(sum(mat == 1), 25)
})

test_that("p = 1 gives a board with all 0", {
  mat <- generate_board_mat(p = 1)
  expect_equal(sum(mat), 0)
})

test_that("check possible errors", {
  expect_error(generate_board_mat(n = c(1,2)))
  expect_error(generate_board_mat(n = "asdf"))
  expect_error(generate_board_mat(n = 5.4))
  expect_error(generate_board_mat(n = -5))
})

test_that("check is_valid", {
  expect_error(is_valid(mat = c(1,2,3)))
  expect_error(is_valid(mat = matrix(ncol = 5, nrow = 11)))
  expect_error(is_valid(mat = matrix(1:25, nrow = 5, ncol = 5)))
})

test_that("check read_board function with loaded data", {
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))
  t_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")
  expect_equal(identical(attributes(board_list), attributes(t_boards)), TRUE)
})

test_that("check read board returns NA 1", {
  res <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test1.txt")
  expect_equal(res, list(NA))
})

test_that("check read board returns NA 2", {
  res <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test2.txt")
  expect_equal(res, list(NA))
})

test_that("check read board returns NA 3", {
  res <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test3.txt")
  expect_equal(res, list(NA))
})

test_that("check read board returns NA 4", {
  res <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test4.txt")
  expect_equal(res, list(NA))
})

test_that("check read board returns NA 5", {
  res <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test5.txt")
  expect_equal(res, list(NA))
})

test_that("check read board returns NA 6", {
  res <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test6.txt")
  expect_equal(res, list(NA))
})
