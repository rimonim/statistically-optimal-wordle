library(tidyverse)
library(tidytext)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Official Wordle dictionary (from https://www.powerlanguage.co.uk/wordle/main.e65ce0a5.js)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

guess_dictionary_vec <- scan(file = "/Users/louisteitelbaum/Documents/wordle_dictionary.txt", 
                        what = "character", 
                        sep = ",")
answer_dictionary_vec <- guess_dictionary_vec[1:grep("shave", guess_dictionary_vec, value = F)]

guess_dictionary <- strsplit(guess_dictionary_vec, "")
answer_dictionary <- strsplit(answer_dictionary_vec, "")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Letter frequencies in each of the five spaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

places <- data.frame(letter = answer_dictionary_vec) %>%
  separate(letter, into = c("blank", "1", "2", "3", "4", "5"), sep = "") %>%
  select(2:6) %>%
  pivot_longer(cols = 1:5, names_to = "place", values_to = "letter") %>%
  count(place, letter)

places %>%
  group_by(place) %>%
  arrange(desc(n), .by_group = T) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(letter = factor(paste(letter, place, sep = "__"), levels = rev(paste(letter, place, sep = "__")))) %>%
  ggplot(aes(letter, n)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~place, scales = "free") +
    coord_flip() +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    theme_minimal() +
    labs(title = "Letter Frequencies by Place", x = NULL, y = "Occurences")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function: Calculate new dictionary based on: 
# guess (5 item vector), 
# answer (5 item vector), and
# prior dictionary (list of 5 item vectors)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dictionary_update <- function(guess, answer, dictionary) {
  for (n in 1:5) {
    if (guess[n] %in% answer){
      if (guess[n] == answer[n]) {
        dictionary <- dictionary[sapply(dictionary, "[", n) == guess[n]]
      }else{
        dictionary <- dictionary[sapply(dictionary, "[", n) != guess[n]
                                 & sapply(dictionary, function(x) any(guess[n] %in% x))]
      }
    }else{
      dictionary <- dictionary[sapply(dictionary, function(x) !any(guess[n] %in% x))]
    }
  }
  dictionary
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How good is a particular first guess? (input string)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

guess_quality <- function(guess) {
  guess <- unlist(strsplit(guess, ""))
  distribution_table <- data.frame(answer = rep(NA, length(answer_dictionary)),
                                   posterior_dictionary_length = rep(NA, length(answer_dictionary)))
  for (n in 1:length(answer_dictionary)) {
    answer <- answer_dictionary[[n]]
    posterior_dictionary_length <- length(dictionary_update(guess, answer, answer_dictionary))
    distribution_table[n, 1] <- paste(answer, collapse = "")
    distribution_table[n, 2] <- posterior_dictionary_length
  }
  distribution_table
}

  # Let's graph it!
guess_quality("roate") %>%
  mutate(
    dictionary_reduction = 100-(100*(posterior_dictionary_length/length(answer_dictionary)))
  ) %>%
  ggplot(aes(x = dictionary_reduction)) +
  geom_bar() +
  theme_classic() +
  labs(title = "Expected Reduction in Possible Answers for First Guess 'roate'",
       x = "Percent Reduction")

  # How much does a guess narrow our possibilities down, on average?
guess <- guess_quality("tares") %>%
  mutate(dictionary_reduction = 100-(100*(posterior_dictionary_length/length(answer_dictionary))))
mean(guess$dictionary_reduction)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What's the best starting word?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Faster guess assessment
guess_quality_minimal_fast <- function(guess, dictionary = answer_dictionary) {
  guess <- unlist(strsplit(guess, ""))
  answer_sample <- answer_dictionary[sample(1:length(answer_dictionary), 100)]
  distribution <- rep(NA, length(answer_sample))
  for (n in 1:length(answer_sample)) {
    answer <- answer_sample[[n]]
    distribution[n] <- length(dictionary_update(guess, answer, dictionary))
  }
  mean(distribution)
}

  # Make table of expected answer narrowing
expected_reduction_table <- data.frame(guess = rep(NA, length(guess_dictionary)),
                                       expected_reduction = rep(NA, length(guess_dictionary)))
for (n in 1:length(guess_dictionary)) {
  guess <- paste(guess_dictionary[[n]], collapse = "")
  expected_reduction_table$guess[n] <- guess
  expected_reduction_table$expected_reduction[n] <- 100-(100*(guess_quality_minimal_fast/length(answer_dictionary)))
}

expected_reduction_table <- expected_reduction_table %>%
  arrange(desc(expected_reduction))

head(expected_reduction_table)

  # Exact numbers for the top 200
guess_dictionary_top <- strsplit(head(expected_reduction_table$guess, n = 200), "")

expected_reductions_exact <- data.frame(guess = rep(NA, 200),
                                        expected_reduction = rep(NA, 200))
for (n in 1:200) {
  guess <- paste(guess_dictionary_top[[n]], collapse = "")
  expected_reductions_exact$guess[n] <- guess
  distribution_table <- guess_quality(guess) %>%
    mutate(dictionary_reduction = 100-(100*(posterior_dictionary_length/length(answer_dictionary))))
  expected_reductions_exact$expected_reduction[n] <- mean(distribution_table$dictionary_reduction)
}

expected_reductions_exact <- expected_reductions_exact %>%
  arrange(desc(expected_reduction))
head(expected_reductions_exact, n = 10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some metrics for "roate"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

roate_outcomes <- guess_quality("roate") %>%
  mutate(
    dictionary_reduction = 100-(100*(posterior_dictionary_length/length(answer_dictionary)))
  )

roate_outcomes_summary <- as.data.frame(table(roate_outcomes$dictionary_reduction))
for (row in 1:nrow(roate_outcomes_summary)) {
  roate_outcomes_summary$example[row] <- (roate_outcomes$answer[roate_outcomes$dictionary_reduction == roate_outcomes_summary$Var1[row]])[1]
}
roate_outcomes_summary <- roate_outcomes_summary %>%
  mutate(rfreq = Freq/sum(Freq))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### WORDLE ENGINE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # This is the same as dictionary_update, but it prints the outcome of the guess with nice colors
dictionary_update_printed <- function(guess, answer, dictionary) {
  for (n in 1:5) {
    if (guess[n] %in% answer){
      if (guess[n] == answer[n]) {
        dictionary <- dictionary[sapply(dictionary, "[", n) == guess[n]]
        cat(paste0("\033[48;5;46m", guess[n]))
      }else{
        dictionary <- dictionary[sapply(dictionary, "[", n) != guess[n]
                                 & sapply(dictionary, function(x) any(guess[n] %in% x))]
        cat(paste0("\033[48;5;226m", guess[n]))
      }
    }else{
      dictionary <- dictionary[sapply(dictionary, function(x) !any(guess[n] %in% x))]
      cat(paste0("\033[48;5;249m", guess[n]))
    }
  }
  dictionary
}

    # Returns average posterior dictionary length
guess_quality_minimal <- function(guess, dictionary = answer_dictionary) {
  guess <- unlist(strsplit(guess, ""))
  distribution <- rep(NA, length(dictionary))
  for (n in 1:length(distribution)) {
    answer <- dictionary[[n]]
    distribution[n] <- length(dictionary_update(guess, answer, dictionary))
  }
  mean(distribution)
}

# THE BOT

  # Data frame of pre-computed common optimal second guesses, for ease of computation
common_seconds <- data.frame(green = c(0, 2, 0, 0, 0),
                             yellow = c(0, 2, 4, 3, 5),
                             optimal_guess = c("slimy", "bludy", "shunt", "lysin", "silen"))

play <- function(answer){
  stopifnot(length(answer) == 5)
  # Make the first guess (always "roate") and update the dictionary
  dictionary <- dictionary_update_printed(unlist(strsplit("roate", "")), answer, answer_dictionary)
  cat("\n")
  for (try in 1:5) {
    if (length(dictionary) == 1) {
      cat(paste0("\033[48;5;46m", paste(dictionary[[1]], collapse = "")))
      cat("\n")
      break
    }
    if (length(dictionary) == 2) {
      if (paste(guess_dictionary[[1]], collapse = "") == paste(answer, collapse = "")){
        dictionary <- dictionary_update_printed(dictionary[[1]], answer, dictionary)
        cat("\n")
        break
      }else{
        dictionary <- dictionary_update_printed(dictionary[[1]], answer, dictionary)
        dictionary <- dictionary_update_printed(dictionary[[1]], answer, dictionary)
        cat("\n")
        break
      }
    }
    if (try == 1) {
      green <- c(which(unlist(strsplit("roate", "")) == answer), 0)[1]
      yellow <- c(which(unlist(strsplit("roate", "")) %in% answer), 0)[1]
      dictionary <- dictionary_update_printed(unlist(strsplit(common_seconds$optimal_guess[common_seconds$green == green & common_seconds$yellow == yellow], "")), answer, dictionary)
      cat("\n")
    }
    guess_remainders <- rep(NA, length(guess_dictionary))
    for (n in 1:length(guess_dictionary)) {
      guess <- paste(guess_dictionary[[n]], collapse = "")
      guess_remainders[n] <- guess_quality_minimal(guess, dictionary)
    }
    dictionary <- dictionary_update_printed(guess_dictionary[[which.min(guess_remainders)]], answer, dictionary)
    cat("\n")
    if (paste(guess_dictionary[[which.min(guess_remainders)]], collapse = "") == paste(answer, collapse = "")) {
      break
    }
  }
}

  # Play a given game (good for retroactively seeing the optimally cautious solution to today's Wordle)
play(unlist(strsplit("skill", "")))

  # Play a random game
play(answer_dictionary[[sample(1:length(answer_dictionary), 1)]])

