#' Day 04: Giant Squid
#'
#' [Giant Squid](https://adventofcode.com/2021/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' You\'re already almost 1.5km (almost a mile) below the surface of the
#' ocean, already so deep that you can\'t see any sunlight. What you *can*
#' see, however, is a giant squid that has attached itself to the outside
#' of your submarine.
#' 
#' Maybe it wants to play
#' [bingo](https://en.wikipedia.org/wiki/Bingo_(American_version))?
#' 
#' Bingo is played on a set of boards each consisting of a 5x5 grid of
#' numbers. Numbers are chosen at random, and the chosen number is *marked*
#' on all boards on which it appears. (Numbers may not appear on all
#' boards.) If all numbers in any row or any column of a board are marked,
#' that board *wins*. (Diagonals don\'t count.)
#' 
#' The submarine has a *bingo subsystem* to help passengers (currently, you
#' and the giant squid) pass the time. It automatically generates a random
#' order in which to draw numbers and a random set of boards (your puzzle
#' input). For example:
#' 
#'     7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
#' 
#'     22 13 17 11  0
#'      8  2 23  4 24
#'     21  9 14 16  7
#'      6 10  3 18  5
#'      1 12 20 15 19
#' 
#'      3 15  0  2 22
#'      9 18 13 17  5
#'     19  8  7 25 23
#'     20 11 10 24  4
#'     14 21 16 12  6
#' 
#'     14 21 17 24  4
#'     10 16 15  9 19
#'     18  8 23 26 20
#'     22 11 13  6  5
#'      2  0 12  3  7
#' 
#' After the first five numbers are drawn (`7`, `4`, `9`, `5`, and `11`),
#' there are no winners, but the boards are marked as follows (shown here
#' adjacent to each other to save space):
#' 
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#' 
#' After the next six numbers are drawn (`17`, `23`, `2`, `0`, `14`, and
#' `21`), there are still no winners:
#' 
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#' 
#' Finally, `24` is drawn:
#' 
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#' 
#' At this point, the third board *wins* because it has at least one
#' complete row or column of marked numbers (in this case, the entire top
#' row is marked: `14 21 17 24  4`).
#' 
#' The *score* of the winning board can now be calculated. Start by finding
#' the *sum of all unmarked numbers* on that board; in this case, the sum
#' is `188`. Then, multiply that sum by *the number that was just called*
#' when the board won, `24`, to get the final score, `188 * 24 = 4512`.
#' 
#' To guarantee victory against the giant squid, figure out which board
#' will win first. *What will your final score be if you choose that
#' board?*
#'
#' **Part Two**
#' On the other hand, it might be wise to try a different strategy: [let
#' the giant squid
#' win]{title="That's 'cuz a submarine don't pull things' antennas out of their sockets when they lose. Giant squid are known to do that."}.
#' 
#' You aren\'t sure how many bingo boards a giant squid could play at once,
#' so rather than waste time counting its arms, the safe thing to do is to
#' *figure out which board will win last* and choose that one. That way, no
#' matter which boards it picks, it will win for sure.
#' 
#' In the above example, the second board is the last to win, which happens
#' after `13` is eventually called and its middle column is completely
#' marked. If you were to keep playing until this point, the second board
#' would have a sum of unmarked numbers equal to `148` for a final score of
#' `148 * 13 = 1924`.
#' 
#' Figure out which board will win last. *Once it wins, what would its
#' final score be?*

#'
#' @param x some data
#' @param board_id For us with part B - specify the losing board
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b()
library(tidyverse)
f04a <- function(x, board_id = NULL) {
  draws <- as.numeric(stringr::str_split(x[1], ",")[[1]])
  boards <- x[3:length(x)]
  # Remove the blank lines
  boards2 <- boards[-seq(6, length(boards), by = 6)]
  # Combine every 5 entries in the vector
  boards_i <- seq(1, length(boards2), by = 5)
  boards_list <- vector(mode = 'list', length = length(boards_i))
  track_list <- vector(mode = 'list', length = length(boards_list))
  for (i in 1:length(boards_i)) {
    board_num <- boards_i[i]
    board <- paste(boards2[board_num],
          boards2[board_num + 1],
          boards2[board_num + 2],
          boards2[board_num + 3],
          boards2[board_num + 4])
    board <- str_squish(board)
    board <- as.numeric(stringr::str_split(board, " ")[[1]])
    board_mat <- as.data.frame(matrix(board, nrow = 5, byrow = TRUE))
    boards_list[[i]] <- board_mat
    track_list[[i]] <- as.data.frame(matrix(rep(NA_integer_, 25), nrow = 5))
  }
  if (!is.null(board_id)) {
    boards_list <- boards_list[board_id]
    track_list <- track_list[board_id]
  }
  win <- 0
  winner_row <- NA
  winner_col <- NA
  winner_id <- NA
  for (i in 1:length(draws)) {
    draw_i <- draws[i]
    track_list <- map2(boards_list, track_list, function(.x, .y) {
      ind <- which(.x == draw_i, arr.ind = TRUE)
      if (!is.na(ind[1])) {
        .y[ind[1], ind[2]] <- 1
      }
      return(.y)
    })
    row_sums <- map(track_list, ~rowSums(.x, na.rm = TRUE))
    col_sums <- map(track_list, ~colSums(.x, na.rm = TRUE))
    row_bingo <- map(row_sums, function(.x) {
      any(.x == 5)
    }) %>% unlist()
    col_bingo <- map(col_sums, function(.x){
      any(.x == 5)
    }) %>% unlist()
    if (any(col_bingo == TRUE) == TRUE) {
        # Get the winning board number
        winner_id <- which(col_bingo == TRUE)
        # Remove the winning column
        winner_col <- which(col_sums[[winner_id]] == 5) 
        winner <- boards_list[[winner_id]][is.na(track_list[[winner_id]])]
        win <- sum(winner) * draw_i
      } 
    if (any(row_bingo == TRUE) == TRUE) {
        # Get the winning board number
        winner_id <- which(row_bingo == TRUE)
        # Remove the winning column
        winner_row <- which(row_sums[[winner_id]] == 5) 
        winner <- boards_list[[winner_id]][is.na(track_list[[winner_id]])]
        win <- sum(winner) * draw_i
      }
    if (win != 0) break 
  } # End draws
  
  return(win)  

}


#' @rdname day04
#' @export
f04b <- function(x) {
  draws <- as.numeric(stringr::str_split(x[1], ",")[[1]])
  boards <- x[3:length(x)]
  # Remove the blank lines
  boards2 <- boards[-seq(6, length(boards), by = 6)]
  # Combine every 5 entries in the vector
  boards_i <- seq(1, length(boards2), by =5)
  boards_list <- vector(mode = 'list', length = length(boards_i))
  track_list <- vector(mode = 'list', length = length(boards_i))
  for (i in 1:length(boards_i)) {
    board_num <- boards_i[i]
    board <- paste(boards2[board_num],
                   boards2[board_num + 1],
                   boards2[board_num + 2],
                   boards2[board_num + 3],
                   boards2[board_num + 4])
    board <- str_squish(board)
    board <- as.numeric(stringr::str_split(board, " ")[[1]])
    board_mat <- as.data.frame(matrix(board, nrow = 5, byrow = TRUE))
    boards_list[[i]] <- board_mat
    track_list[[i]] <- as.data.frame(matrix(rep(NA_integer_, 25), nrow = 5))
  }
  win <- 0
  winner_row <- NA
  winner_col <- NA
  winner_id <- NA
  win_tracker <- rep(0, length(boards_list))
  for (i in 1:length(draws)) {
    draw_i <- draws[i]
    track_list <- map2(boards_list, track_list, function(.x, .y) {
      ind <- which(.x == draw_i, arr.ind = TRUE)
      if (!is.na(ind[1])) {
        .y[ind[1], ind[2]] <- 1
      }
      return(.y)
    })
    row_sums <- map(track_list, ~rowSums(.x, na.rm = TRUE))
    col_sums <- map(track_list, ~colSums(.x, na.rm = TRUE))
    row_bingo <- map(row_sums, function(.x) {
      any(.x == 5)
    }) %>% unlist()
    col_bingo <- map(col_sums, function(.x){
      any(.x == 5)
    }) %>% unlist()
    if (any(col_bingo == TRUE) == TRUE) {
      # Get the winning board number
      winner_id <- which(col_bingo == TRUE)
      win_tracker[winner_id] <- 1
    } 
    if (any(row_bingo == TRUE) == TRUE) {
      # Get the winning board number
      winner_id <- which(row_bingo == TRUE)
      win_tracker[winner_id] <- 1
    }
    if (sum(win_tracker) == (length(win_tracker) - 1)) break
  } # End draws
  last_winning_index <- which(win_tracker == 0)
  f04a(x, board_id = last_winning_index)
}


f04_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = readLines('./inst/example04.txt', warn = FALSE)
  )
  l[[example]]
}
