#!/bin/env Rscript
#
# Based on
#
# False-positive psychology: Undisclosed flexibility in data collection
# and analysis allows presenting anything as significant
#

get_mean_woman_length <- function() { 171 }
get_sd_woman_length <- function() { 10 }
get_n_subjects <- function() { 200 }

#' Create a table of `n` subjects, with half of type A and half of type B
create_subjects <- function(n_subjects, seed) {
  testthat::expect_equal(n_subjects / 4, round(n_subjects / 4))
  set.seed(seed)
  t <- tibble::tibble(
    preference = rep(c("red", "green"), times = n_subjects / 2), 
    phone_number = rep(c("odd", "even"), each = n_subjects / 2), 
    length = rnorm(
      n = n_subjects, 
      mean = get_mean_woman_length(), 
      sd = get_sd_woman_length()
    )
  )
  t$preference <- as.factor(t$preference)
  t
}
testthat::expect_equal(create_subjects(12, 42), create_subjects(12, 42))

plot_subjects <- function(t) {
  n_subjects <- nrow(t)
  binwidth <- 1
  xs <- seq(
    from = get_mean_woman_length() - (3.0 * get_sd_woman_length()), 
    to = get_mean_woman_length() + (3.0 * get_sd_woman_length()), 
    by = binwidth
  )
  t_ideal <- tibble::tibble(
    x = xs
  )
  t_ideal$y <- dnorm(
    t_ideal$x, 
    mean = get_mean_woman_length(), 
    sd = get_sd_woman_length()
  ) * n_subjects * binwidth / 1
  ggplot2::ggplot(
    t, 
    ggplot2::aes(x = length)) + 
    ggplot2::geom_freqpoly(linewidth = 3, binwidth = binwidth) + 
    ggplot2::geom_line(data = t_ideal, mapping = ggplot2::aes(x = x, y = y), color = "black", lty = "dashed") +
    ggplot2::scale_x_continuous(name = "Längd (cm)") +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 13),
      name = "Antal kvinnor med en viss längd"
    ) +
    ggplot2::labs(
      caption = paste0(
        #"Number of women: ", n_subjects
      )
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}

plot_subjects_by_preference <- function(t) {
  n_subjects <- nrow(t)
  binwidth <- 1
  xs <- seq(
    from = get_mean_woman_length() - (3.0 * get_sd_woman_length()), 
    to = get_mean_woman_length() + (3.0 * get_sd_woman_length()), 
    by = binwidth
  )
  t_ideal <- tibble::tibble(
    x = xs
  )
  t_ideal$y <- dnorm(
    t_ideal$x, 
    mean = get_mean_woman_length(), 
    sd = get_sd_woman_length()
  ) * n_subjects * binwidth / 2
  statistics <- broom::tidy(
    t.test(
      x = t[t$preference == "red", ]$length, 
      y = t[t$preference == "green", ]$length
    )
  )
  
  ggplot2::ggplot(
    t, 
    ggplot2::aes(x = length, color = preference)) + 
    ggplot2::geom_freqpoly(linewidth = 3, binwidth = binwidth) + 
    ggplot2::geom_line(data = t_ideal, mapping = ggplot2::aes(x = x, y = y), color = "black", lty = "dashed") +
    ggplot2::scale_x_continuous(name = "Längd (cm)") +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 10),
      name = "Antal kvinnor med en viss längd"
    ) +
    ggplot2::labs(
      caption = paste0(
        # "Number of women: ", n_subjects, ", ",
        "Chansen att skillnaden bara är en slump: ", format(100.0 * statistics$p.value, digits = 3), "%"
      )
    ) +
    ggplot2::scale_color_manual(
      name = "Preferens",
      labels = c("red" = "röd", "green" = "grön"), 
      values = c("red" = "red", "green" = "green")
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}

t <- create_subjects(n_subjects = get_n_subjects(), seed = 1)
plot_subjects(t)
ggplot2::ggsave(paste0("distribution_", get_n_subjects(), "_no_preference.png"), width = 7, height = 7)
plot_subjects_by_preference(t)
ggplot2::ggsave(paste0("distribution_", get_n_subjects(), ".png"), width = 7, height = 7)

#' Determines if 'red' is significantly different than 'green'
create_experiment_results <- function(n_subjects, n_seeds) {
  t_result <- tibble::tibble(seed = seq_len(n_seeds), p_value = NA)
  for (i in seq_len(n_seeds)) {
    seed <- t_result$seed[i]
    t <- create_subjects(n_subjects, seed = seed)
    p_value <- t.test(
      x = t[t$preference == "red", ]$length, 
      y = t[t$preference == "green", ]$length
    )$p.value
    t_result$p_value[i] <- p_value
  }
  t_result
}

n_seeds <- 40
t <- create_experiment_results(n_subjects = get_n_subjects(), n_seeds = n_seeds)
n_significant <- sum(t$p_value < 0.05)
testthat::expect_true(n_significant >= 1)
percentage_significant <- 100.0 * n_significant / n_seeds

best_seed <- which(t$p_value == min(t$p_value))
t[best_seed, ]

plot_subjects_by_preference(create_subjects(n_subjects = get_n_subjects(), seed = best_seed))
ggplot2::ggsave(paste0("distribution_", get_n_subjects(), "_", best_seed, ".png"), width = 7, height = 7)
