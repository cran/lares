## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

# Enable ANSI color output in HTML vignettes
Sys.setenv(CLICOLOR_FORCE = "1")
options(crayon.enabled = TRUE)
library(fansi)
old_hooks <- fansi::set_knit_hooks(knitr::knit_hooks,
  which = c("output", "message", "error")
)

## -----------------------------------------------------------------------------
library(lares)

## ----eval=TRUE----------------------------------------------------------------
# Check your guess against a word
wordle_check("OPENS", "ABBEY")
wordle_check("BABES", "ABBEY")
wordle_check("ABBEY", "ABBEY")

## -----------------------------------------------------------------------------
# After OPENS: O is not in word, P/E/N/S are not in positions 2/3/4/5
hints <- scrabble_words(
  tiles = "abcdefghijklmrtuvwxyz", # Available letters
  exclude_here = list("2" = "p", "3" = "e", "4" = "n", "5" = "s"),
  force_exclude = c("o"),
  force_n = 5, # 5-letter words
  language = "en"
)
head(hints, 10)

## -----------------------------------------------------------------------------
# Simulate solving with different starting words
simulation <- wordle_simulation(
  input = "SAINT",
  word = "ABBEY",
  seed = 123
)
print(simulation)

## ----eval=FALSE---------------------------------------------------------------
# # Get English 5-letter words
# en_words <- wordle_dictionary("en")
# head(en_words, 20)
# 
# # Spanish words
# es_words <- wordle_dictionary("es")

## -----------------------------------------------------------------------------
# Find best words from your tiles
scrabble_words(
  tiles = "aeiourtn",
  force_max = 8, # Max 8 letters
  language = "en",
  scores = "en"
)

## -----------------------------------------------------------------------------
# Must contain specific letters
scrabble_words(
  tiles = "bernardo",
  force_str = "arn",
  force_max = 7,
  language = "en"
)

## -----------------------------------------------------------------------------
# Get point values for each letter
en_scores <- scrabble_points("en")
print(en_scores)

# Calculate scores for words
words <- c("QUEEN", "QUIZ", "HELLO")
scrabble_score(words, en_scores)

## ----eval=FALSE---------------------------------------------------------------
# # Spanish Scrabble
# scrabble_words(
#   tiles = "españa",
#   language = "es",
#   scores = "es"
# )
# 
# # French Scrabble
# scrabble_words(
#   tiles = "bonjour",
#   language = "fr",
#   scores = "fr"
# )

## -----------------------------------------------------------------------------
# Easy puzzle (0 represents empty cells)
trivial <- matrix(c(
  0, 9, 0, 7, 0, 0, 8, 6, 0,
  0, 3, 1, 0, 0, 5, 0, 2, 0,
  8, 0, 6, 0, 0, 0, 0, 0, 0,
  0, 0, 7, 0, 5, 0, 0, 0, 6,
  0, 0, 0, 3, 0, 7, 0, 0, 0,
  5, 0, 0, 0, 1, 0, 7, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 9,
  0, 2, 0, 6, 0, 0, 3, 5, 0,
  0, 5, 4, 0, 0, 8, 0, 7, 0
), nrow = 9, byrow = TRUE)

solution <- sudoku_solver(trivial)
print(solution)

## ----eval=FALSE---------------------------------------------------------------
# # Harder puzzle
# difficult <- matrix(c(
#   5, 3, 0, 0, 7, 0, 0, 0, 0,
#   6, 0, 0, 1, 9, 5, 0, 0, 0,
#   0, 9, 8, 0, 0, 0, 0, 6, 0,
#   8, 0, 0, 0, 6, 0, 0, 0, 3,
#   4, 0, 0, 8, 0, 3, 0, 0, 1,
#   7, 0, 0, 0, 2, 0, 0, 0, 6,
#   0, 6, 0, 0, 0, 0, 2, 8, 0,
#   0, 0, 0, 4, 1, 9, 0, 0, 5,
#   0, 0, 0, 0, 8, 0, 0, 7, 9
# ), nrow = 9, byrow = TRUE)
# 
# sudoku_solver(difficult)

## -----------------------------------------------------------------------------
# Create a simple maze (0 = path, 1 = wall)
simple_maze <- matrix(c(
  0, 1, 0, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 0, 1, 0,
  1, 1, 0, 0, 0,
  0, 0, 0, 1, 0
), nrow = 5, byrow = TRUE)

solution <- maze_solve(
  simple_maze,
  start = c(1, 1),
  end = c(5, 5)
)
print(solution)

## ----eval=FALSE---------------------------------------------------------------
# # Classic Micromouse-style maze
# micromouse <- matrix(c(
#   1, 1, 1, 1, 1, 1, 1, 1,
#   1, 0, 0, 0, 0, 0, 0, 1,
#   1, 0, 1, 1, 1, 1, 0, 1,
#   1, 0, 1, 0, 0, 0, 0, 1,
#   1, 0, 1, 0, 1, 1, 0, 1,
#   1, 0, 0, 0, 0, 1, 0, 1,
#   1, 0, 1, 1, 0, 0, 0, 1,
#   1, 1, 1, 1, 1, 1, 1, 1
# ), nrow = 8, byrow = TRUE)
# 
# maze_solve(
#   micromouse,
#   start = c(2, 2),
#   end = c(7, 7),
#   diagonal = FALSE
# )

## ----eval=FALSE---------------------------------------------------------------
# # With diagonal movement and aiming toward goal
# maze_solve(
#   micromouse,
#   start = c(2, 2),
#   end = c(7, 7),
#   diagonal = TRUE, # Allow diagonal moves
#   aim = TRUE, # Prefer directions toward goal
#   inertia = TRUE, # Prefer continuing in same direction
#   timeout = 5 # Max 5 seconds
# )

## ----eval=FALSE---------------------------------------------------------------
# # Find anagrams in your dataset
# words <- c("listen", "silent", "hello")
# scrabble_words(tiles = "listen", language = "en")

## ----eval=FALSE---------------------------------------------------------------
# # Demonstrate recursion with maze solving
# maze_solve(simple_maze, start = c(1, 1), end = c(5, 5))

## ----eval=FALSE---------------------------------------------------------------
# # Wordle simulations for optimal starting words
# seeds <- 1:100
# results <- lapply(seeds, function(s) {
#   wordle_simulation("SAINT", "ABBEY", seed = s, quiet = TRUE)
# })

