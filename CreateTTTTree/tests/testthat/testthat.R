library(CreateTTTTree)
context("CreateDataSetTTT")

test_that("CreateTTTTree returns a tree", {
  ttt <- CreateDataSetTTT()

  expect_equal(ttt$leafCount, 89796)

  expect_equal(ttt$totalCount, 203716)

  winnerOne <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 1)
  winnerTwo <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == -1)
  ties <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 0)
  # print(c(winnerOne = length(winnerOne), winnerTwo = length(winnerTwo), ties = length(ties)))

  expect_equal(length(winnerOne), 39588)
  expect_equal(length(winnerTwo), 21408)
  expect_equal(length(ties), 28800)
})
