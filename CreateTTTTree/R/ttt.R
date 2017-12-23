#' A tree representing all possible moves in tic-tac-toe
#'
#' To recreate this run CreateDataSetTTT()
#'
#' ttt <- CreateDataSetTTT()
#' print(system.time(devtools::use_data(ttt,compress = 'bzip2', internal = TRUE, overwrite = TRUE)))
#' ttt <- TTTTree()
#'
#' @source https://ipub.com/tic-tac-toe/
#' @format Data tree with attributes
#' \describe{
#' \item{points1}{Analysis Points for winning tic-tac-toe: player 1}
#' \item{points2}{Analysis Points for winning tic-tac-toe: player -1}
#' }
#' @examples
#'   ttt
"ttt"
