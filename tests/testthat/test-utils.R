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
