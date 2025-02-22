# Based on
#
# False-positive psychology: Undisclosed flexibility in data collection and analysis allows presenting anything as significant
#

get_mean_woman_length <- function() { 171 }
get_sd_woman_length <- function() { 10 }

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
    ggplot2::scale_x_continuous(name = "Length (cm)") +
    ggplot2::scale_y_continuous(name = "Amount of women with that length") +
    ggplot2::labs(
      caption = paste0(
        #"Number of women: ", n_subjects
      )
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}
plot_subjects(create_subjects(n_subjects = 200, seed = 42))


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
    ggplot2::scale_x_continuous(name = "Length (cm)") +
    ggplot2::scale_y_continuous(name = "Amount of women with that length") +
    ggplot2::labs(
      caption = paste0(
        # "Number of women: ", n_subjects, ", ",
        "p-value: ", format(100.0 * statistics$p.value, digits = 3), "%"
      )
    ) +
    ggplot2::scale_color_manual(values = c("red" = "red", "green" = "green")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}

plot_subjects_by_preference(create_subjects(n_subjects = 1000, seed = 42))

# Create one example, 1000 women
for (n_subjects in c(200, 100, 20)) {
  t <- create_subjects(n_subjects = n_subjects, seed = 1)
  plot_subjects(t)
  ggplot2::ggsave(paste0("distribution_", n_subjects, "_no_preference.png"), width = 7, height = 7)
  plot_subjects_by_preference(t)
  ggplot2::ggsave(paste0("distribution_", n_subjects, ".png"), width = 7, height = 7)
}


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

n_subjects <- 200
n_seeds <- 1000
t <- create_experiment_results(n_subjects = n_subjects, n_seeds = n_seeds)
n_significant <- sum(t$p_value < 0.05)
percentage_significant <- 100.0 * n_significant / n_seeds

ggplot2::ggplot(
  t, 
  ggplot2::aes(x = p_value)
) + 
  ggplot2::geom_histogram(binwidth = 0.01) + 
  ggplot2::scale_x_continuous(limits = c(0, 1), minor_breaks = seq(0.0, 1.0, by = 0.01)) + 
  ggplot2::geom_vline(xintercept = 0.05, lty = "dashed") +
  ggplot2::labs(
    caption = paste0(
      "Number of experiments: ", n_seeds, "\n",
      "Number of subjects in an experiment: ", n_subjects, "\n",
      "Significant findings: ", percentage_significant, "%"
    )
  ) +
  ggplot2::theme(text = ggplot2::element_text(size = 20))
ggplot2::ggsave(paste0("n_significant_findings_", n_subjects, "_", n_seeds, ".png"), width = 7, height = 7)


best_seed <- which(t$p_value == min(t$p_value))
t[best_seed, ]

plot_subjects_by_preference(create_subjects(n_subjects = 200, seed = best_seed))
ggplot2::ggsave(paste0("distribution_", n_subjects, "_", best_seed, ".png"), width = 7, height = 7)


#' Determines if 'red' is significantly different than 'green', for a covariate
create_experiment_results_with_covariate <- function(n_subjects, n_seeds) {
  t_result <- tibble::tibble(
    seed = seq_len(n_seeds), 
    p_value_preference = NA,
    p_value_preference_for_odd = NA,
    p_value_preference_for_even = NA,
    p_value_phone_number = NA,
    p_value_phone_number_for_red = NA,
    p_value_phone_number_for_green = NA
  )
  for (i in seq_len(n_seeds)) {
    seed <- t_result$seed[i]
    t <- create_subjects(n_subjects, seed = seed)
    p_value_preference <- t.test(
      x = t[t$preference == "red", ]$length, 
      y = t[t$preference == "green", ]$length
    )$p.value
    p_value_preference_for_odd <- t.test(
      x = t[t$preference == "red" & t$phone_number == "odd", ]$length, 
      y = t[t$preference == "green" & t$phone_number == "odd", ]$length
    )$p.value
    p_value_preference_for_even <- t.test(
      x = t[t$preference == "red" & t$phone_number == "even", ]$length, 
      y = t[t$preference == "green" & t$phone_number == "even", ]$length
    )$p.value
    p_value_phone_number <- t.test(
      x = t[t$phone_number == "odd", ]$length, 
      y = t[t$phone_number == "even", ]$length
    )$p.value
    p_value_phone_number_for_red <- t.test(
      x = t[t$phone_number == "odd" & t$preference == "red", ]$length, 
      y = t[t$phone_number == "even" & t$preference == "red", ]$length
    )$p.value
    p_value_phone_number_for_green <- t.test(
      x = t[t$phone_number == "odd" & t$preference == "green", ]$length, 
      y = t[t$phone_number == "even" & t$preference == "green", ]$length
    )$p.value
    t_result$p_value_preference[i] <- p_value_preference
    t_result$p_value_preference_for_odd[i] <- p_value_preference_for_odd
    t_result$p_value_preference_for_even[i] <- p_value_preference_for_even
    t_result$p_value_phone_number[i] <- p_value_phone_number
    t_result$p_value_phone_number_for_red[i] <- p_value_phone_number_for_red
    t_result$p_value_phone_number_for_green[i] <- p_value_phone_number_for_green
  }
  t_result
}


n_subjects <- 1000
n_seeds <- 1000
t <- create_experiment_results_with_covariate(n_subjects = n_subjects, n_seeds = n_seeds)

t$p_value_max <- NA
for (i in seq_len(nrow(t))) {
  t$p_value_max[i] <- min(as.numeric(t[i, 2:7]))
}
sum(t$p_value_max < 0.05)
n_significant <- sum(t$p_value_preference < 0.05 | 
  t$p_value_preference_for_odd < 0.05 | 
  t$p_value_preference_for_even < 0.05 |  
  t$p_value_phone_number < 0.05 | 
  t$p_value_phone_number_for_red < 0.05 | 
  t$p_value_phone_number_for_green < 0.05)
percentage_significant <- 100.0 * n_significant / n_seeds
percentage_significant

ggplot2::ggplot(
  t, 
  ggplot2::aes(x = p_value_max)
) + 
  ggplot2::geom_histogram(binwidth = 0.01) + 
  ggplot2::scale_x_continuous("Lowest p-value", limits = c(0, 1), minor_breaks = seq(0.0, 1.0, by = 0.01)) + 
  ggplot2::geom_vline(xintercept = 0.05, lty = "dashed") +
  ggplot2::labs(
    caption = paste0(
      "Number of experiments: ", n_seeds, "\n",
      "Number of subjects in an experiment: ", n_subjects, "\n",
      "Number of things: ", 2, "\n",
      "Significant findings: ", percentage_significant, "%"
    )
  ) +
  ggplot2::theme(text = ggplot2::element_text(size = 20))
ggplot2::ggsave(paste0("n_significant_findings_", n_subjects, "_", n_seeds, "_2_things.png"), width = 7, height = 7)
