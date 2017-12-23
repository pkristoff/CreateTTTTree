#' CreateTTTTree: A package for generating the tree structure representing all possible tic-tac-toe games.
#'
#' The CreateTTTTree package provides ? categories creating all possible games:
#' data.set, bar and baz.
#'
#' @section data.set:
#' This provides a function called CreateDataSetTTT
#'
#' @docType package
#' @name CreateTTTTree

# https://ipub.com/tic-tac-toe/
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#get
#

# In this post, we do a brute force solution of Tic-Tac-Toe, the well-known 3*3 game.
# You’ll learn how data.tree can be used to build a tree of game history, and how the resulting
# data.tree structure can be used to analyse the game.

# This post is based on data.tree 0.2.1, which you can get from CRAN.

# We want to set up the problem in a way such that each Node is a move of a player,
# and each path describes the entire history of a game.

# install.packages("data.tree")
library(data.tree)

#' Generates a data.tree representing all the potential moves in a tic-tac-toe game
#'
#' @return A data.tree representing all the potential moves in a tic-tac-toe game
#' @examples
#' CreateDataSetTTT(1, 1)
CreateDataSetTTT <- function () {
  #' Now we traverse the tree recursively, and add possible moves to the leaves along the way,
  #' growing it to eventually hold all possible games. To do this, we define a method which,
  #' based on a Node’s path, adds possible moves as children.
  #'
  #' @param node A data.tree node
  #'
  #' @return \code{node} with all its children filled in
  #'
  #' @examples
  #' AddPossibleMoves(node)
  #'
  AddPossibleMoves <- function(node) {
    t <- Traverse(node, traversal = "ancestor", filterFun = isNotRoot)
    available <-
      rownames(fields)[!rownames(fields) %in% Get(t, "f")]
    node$points1 <- 0
    node$points2 <- 0
    node$wp1 <- 0
    node$wp2 <- 0
    for (f in available) {
      child <- node$AddChild(paste0(fields[f, 1], fields[f, 2]))
      child$f <- as.numeric(f)
      # child$player <- ifelse(node$player == 1, 2, 1)
      child$player <- -node$player
      hasWon <- HasWon(child)
      if (!hasWon && child$level <= 10) {
        AddPossibleMoves(child)
      }
      if (hasWon) {
        child$result <- child$player
        # print(paste("Player ", child$player, "wins!"))
        # child$points1 <- ifelse(node$player == 1, 10, 0)
        # child$points2 <- ifelse(node$player == 2, 10, 0)
        child$points1 <- ifelse(node$player == 1, 10, 0)
        child$points2 <- ifelse(node$player == -1, -10, 0)
        child$wp1 <- ifelse(node$player == 1, 10, 0)
        child$wp2 <- ifelse(node$player == -1, -10, 0)
      } else if (child$level == 10) {
        # print("Tie!")
        child$result <- 0
        child$points1 <- 0
        child$points2 <- 0
        child$wp1 <- 0
        child$wp2 <- 0
      }
      node$points1 <- node$points1 + child$points1
      node$points2 <- node$points2 + child$points2
      node$wp1 <- node$wp1 + (child$wp1 * 0.5)
      node$wp2 <- node$wp2 + (child$wp2 * 0.5)
      print(paste("node$wp1=", node$wp1, "child$wp1=", child$wp1))
    }
    return (node)
  }

  #'
  #' Our algorithm stops whenever either player has won, or when all 9 fields are taken.
  #' Whether a player has won is determined by this function:
  #'
  #' @param node A data.tree node
  #'
  #' @return whether the node represents a winning game.
  #' @examples
  #' HasWon(node)
  #'
  HasWon <- function(node) {
    t <-
      Traverse(
        node,
        traversal = "ancestor",
        filterFun = function(x)
          ! x$isRoot && x$player == node$player
      )
    mine <- Get(t, "f")
    mineV <- rep(0, 9)
    mineV[mine] <- 1
    mineM <- matrix(mineV, 3, 3, byrow = TRUE)
    result <- any(rowSums(mineM) == 3) ||
      any(colSums(mineM) == 3) ||
      sum(diag(mineM)) == 3 ||
      sum(diag(t(mineM))) == 3
    return (result)
  }
  #'
  #' Prints a board representation of the node
  #'
  #' @param node A data.tree node
  #'
  #' @return whether the node represents a winning game.
  #'
  #' @examples
  #' PrintBoard(node)
  #'
  PrintBoard <- function(node) {
    mineV <- rep(0, 9)

    t <-
      Traverse(
        node,
        traversal = "ancestor",
        filterFun = function(x)
          ! x$isRoot && x$player == 1
      )
    field <- Get(t, "f")
    value <- Get(t, function(x)
      paste0("X", x$level - 1))
    mineV[field] <- value

    # t <- Traverse(node, traversal = "ancestor", filterFun = function(x) !x$isRoot && x$player == 2)
    t <-
      Traverse(
        node,
        traversal = "ancestor",
        filterFun = function(x)
          ! x$isRoot && x$player == -1
      )
    field <- Get(t, "f")
    value <- Get(t, function(x)
      paste0("O", x$level - 1))
    mineV[field] <- value

    mineM <- matrix(mineV, 3, 3, byrow = TRUE)
    rownames(mineM) <- letters[1:3]
    colnames(mineM) <- as.character(1:3)
    mineM
  }

  # We number the fields from 1 to 9. Additionally, for easy readability, we label the Nodes
  # in an Excel-like manner, such that field 9, say, is ‘c3’:

  fields <- expand.grid(letters[1:3], 1:3)
  fields
  ##   Var1 Var2
  ## 1    a    1
  ## 2    b    1
  ## 3    c    1
  ## 4    a    2
  ## 5    b    2
  ## 6    c    2
  ## 7    a    3
  ## 8    b    3
  ## 9    c    3

  ttt <- Node$new("ttt")

  # To speed up things a bit, we consider rotation, so that, say, the first move in a3 and a1 are considered equal,
  # because they could be achieved with a 90 degree rotation of the board. This leaves us with only a3, b3, and b2
  # for the first move of player 1:

  #consider rotation, so first move is explicit
  ttt$AddChild("a3")
  ttt$a3$f <- 7
  ttt$AddChild("b3")
  ttt$b3$f <- 8
  ttt$AddChild("b2")
  ttt$b2$f <- 5

  ttt$Set(player = 1, filterFun = isLeaf)

  # The following code plays all possible games. Depending on your computer, this might take a few minutes:
  #
  print(system.time(for (child in ttt$children)
    AddPossibleMoves(child)))
  ##    user  system elapsed
  ## 345.645   3.245 346.445
  ##

  # print(paste("ttt$leafCount=", ttt$leafCount))
  # print(paste("ttt$totalCount=", ttt$totalCount))
  # print(paste("What is the average length of a game? ", mean(ttt$Get(function(x) x$level - 1, filterFun = isLeaf))))
  # print(paste("What is the average branching factor? ", ttt$averageBranchingFactor))

  # winnerOne <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 1)
  # winnerTwo <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 2)
  # ties <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 0)
  # print(c(winnerOne = length(winnerOne), winnerTwo = length(winnerTwo), ties = length(ties)))
  #
  # print(PrintBoard(ties[[1]]))

  ttt

}

TTTTree <- function() {
  print(system.time(ttt <- CreateDataSetTTT()))
  ttt
}
