#' SmartHealthSimulator
#'
#' @description
#' Simulates a synthetic, multimodal time series dataset resembling wearable, environmental,
#' behavioral, and self-reported health data over time. Includes numeric, categorical, and text data.
#'
#' @details
#' The simulator (created alongside ChatGPT) generates realistic daily records including:
#' - Heart rate
#' - Steps
#' - Skin and ambient temperature
#' - Activity label (rest, walk, exercise)
#' - Mood score (1–5)
#' - Mood notes (short texts)
#' - Air quality index
#' - Sleep quality (dependent variable)
#'
#' Includes functions for plotting time series, distributions, relationships, and text-based visualizations.
#'
#' @examples
#' sim <- SmartHealthSimulator$new(days = 180, seed = 42)
#' head(sim$data)
#' sim$plot_time_series()
#' sim$plot_mood_sleep()
#' sim$plot_activity_distribution()
#' sim$plot_mood_wordcloud()
#'
#' @export
SmartHealthSimulator <- R6::R6Class("SmartHealthSimulator",
                                    public = list(

                                      #' @field data A data frame containing the simulated dataset
                                      data = NULL,

                                      #' @field seed The random seed used for reproducibility
                                      seed = NULL,

                                      #' @description
                                      #' Create a new simulator instance and generate synthetic data
                                      #'
                                      #' @param days Number of days to simulate (default: 180)
                                      #' @param seed Random seed for reproducibility (default: 123)
                                      initialize = function(days = 180, seed = 123) {
                                        self$seed <- seed
                                        private$n_days <- days
                                        private$generate_data()
                                      },

                                      #' @description
                                      #' Plot time series of selected numeric variables
                                      #'
                                      #' @param vars Character vector of column names to plot (default: c("hr_mean", "steps", "sleep_quality"))
                                      #' @return A ggplot2 object showing the time series
                                      plot_time_series = function(vars = c("hr_mean", "steps", "sleep_quality")) {
                                        df <- self$data %>% tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value")
                                        ggplot(df, aes(x = timestamp, y = Value, color = Variable)) +
                                          geom_line(size = 1) +
                                          facet_wrap(~Variable, scales = "free_y", ncol = 1) +
                                          theme_minimal(base_size = 13) +
                                          labs(title = "Time Series of Key Variables", x = "Date", y = NULL)
                                      },

                                      #' @description
                                      #' Plot the relationship between mood score and sleep quality
                                      #' @return A ggplot2 scatter plot with regression line
                                      plot_mood_sleep = function() {
                                        ggplot(self$data, aes(x = mood_score, y = sleep_quality)) +
                                          geom_jitter(width = 0.2, height = 0.5, alpha = 0.6, color = "steelblue") +
                                          geom_smooth(method = "lm", se = FALSE, color = "darkred") +
                                          theme_minimal(base_size = 13) +
                                          labs(title = "Sleep Quality vs. Mood Score", x = "Mood Score", y = "Sleep Quality")
                                      },

                                      #' @description
                                      #' Plot the distribution of activity labels
                                      #' @return A ggplot2 bar chart
                                      plot_activity_distribution = function() {
                                        ggplot(self$data, aes(x = activity_label, fill = activity_label)) +
                                          geom_bar() +
                                          scale_fill_brewer(palette = "Set2") +
                                          theme_minimal(base_size = 13) +
                                          labs(title = "Activity Label Distribution", x = "Activity", y = "Count") +
                                          theme(legend.position = "none")
                                      },

                                      #' @description
                                      #' Create a word cloud from the self-reported mood notes
                                      #' @return A ggplot2 word cloud visualization
                                      plot_mood_wordcloud = function() {
                                        text_data <- self$data$mood_note
                                        words <- unlist(strsplit(tolower(paste(text_data, collapse = " ")), "\\W+"))
                                        word_df <- as.data.frame(table(words)) %>%
                                          dplyr::filter(nchar(as.character(words)) > 3)

                                        ggplot(word_df, aes(label = words, size = Freq)) +
                                          ggwordcloud::geom_text_wordcloud_area() +
                                          theme_minimal() +
                                          labs(title = "Mood Notes Word Cloud")
                                      }
                                    ),

                                    private = list(

                                      #' @field n_days Internal: number of simulated days
                                      n_days = 180,

                                      #' @description Internal data generation function
                                      generate_data = function() {
                                        set.seed(self$seed)
                                        n <- private$n_days

                                        timestamps <- seq(as.Date("2025-01-01"), by = "day", length.out = n)

                                        hr_mean <- round(rnorm(n, mean = 70, sd = 10), 1)
                                        steps <- round(rnorm(n, mean = 7000, sd = 3000))
                                        skin_temp <- round(rnorm(n, mean = 36.5, sd = 0.4), 1)
                                        ambient_temp <- round(rnorm(n, mean = 23, sd = 3), 1)
                                        air_quality_index <- round(runif(n, 20, 120))

                                        activity_label <- cut(steps,
                                                              breaks = c(-Inf, 3000, 7000, Inf),
                                                              labels = c("rest", "walk", "exercise"))

                                        mood_score <- pmin(pmax(round(3 + 0.001 * (steps - 7000) -
                                                                        0.01 * (air_quality_index - 50) +
                                                                        rnorm(n, 0, 0.5)), 1), 5)

                                        mood_phrases <- c("Felt great today.", "Very tired.", "Worked out hard.",
                                                          "Anxious and stressed.", "Calm and productive day.",
                                                          "Slept poorly.", "Long day at work.")

                                        mood_note <- sapply(mood_score, function(ms) {
                                          if (ms >= 4) sample(mood_phrases[c(1, 3, 5)], 1)
                                          else if (ms <= 2) sample(mood_phrases[c(2, 4, 6)], 1)
                                          else sample(mood_phrases, 1)
                                        })

                                        activity_penalty <- ifelse(activity_label == "exercise", -10, 0)

                                        sleep_quality <- 100 -
                                          0.2 * hr_mean +
                                          0.01 * steps +
                                          5 * (mood_score - 3) -
                                          0.3 * air_quality_index +
                                          activity_penalty +
                                          rnorm(n, 0, 5)

                                        sleep_quality <- round(pmin(pmax(sleep_quality, 0), 100), 1)

                                        self$data <- data.frame(
                                          timestamp = timestamps,
                                          hr_mean = hr_mean,
                                          steps = steps,
                                          skin_temp = skin_temp,
                                          ambient_temp = ambient_temp,
                                          activity_label = activity_label,
                                          mood_score = mood_score,
                                          mood_note = mood_note,
                                          air_quality_index = air_quality_index,
                                          sleep_quality = sleep_quality
                                        )
                                      }
                                    )
)
