library(adventofcode21)
x <- readLines("./inst/input06.txt")

p1 <- f06a(x, days = 80)
p2 <- f06a(x, days = 256)
options(scipen = 999) # Needed to get full numerical output

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
