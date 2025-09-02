#' @title WellnessData R6 Class
#' @description A class (created alongside DeepSeek) for generating, managing, and visualizing a synthetic multimodal time series dataset
#' focused on personal wellness metrics, including wearable sensors, self-report, and environmental data.
#' @import R6
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import purrr
#' @importFrom stringr str_detect
#' @importFrom stats arima.sim runif rnorm sd
#' @export
WellnessData <- R6::R6Class(
  classname = "WellnessData",

  public = list(
    #' @field data A tibble containing the generated wellness dataset.
    data = NULL,

    #' @field seed The seed used for random number generation, ensuring reproducibility.
    seed = NULL,

    #' @description
    #' Create a new WellnessData object.
    #' @param seed An integer used to set the random seed for reproducible data generation. Default is 456.
    #' @return A new `WellnessData` object.
    initialize = function(seed = 456) {
      self$seed <- seed
      private$set_seed()
      message("WellnessData object initialized with seed: ", self$seed)
    },

    #' @description
    #' Generates the synthetic wellness dataset.
    #' @param n_participants Number of participants. Default is 5.
    #' @param start_date Start date for the time series (YYYY-MM-DD). Default is '2023-11-01'.
    #' @param n_days Number of days to generate data for. Default is 30.
    #' @param save_csv Logical. Should the data be saved to a CSV file? Default is FALSE.
    #' @param file_path Path for the CSV file if `save_csv = TRUE`.
    #' @return The generated dataset (invisibly) and assigns it to `self$data`.
    generate_data = function(n_participants = 5,
                             start_date = '2023-11-01',
                             n_days = 30,
                             save_csv = FALSE,
                             file_path = "WellnessSensors.csv") {

      private$set_seed() # Ensure seed is set before generation

      participants <- paste0("P", sprintf("%02d", 1:n_participants))
      dates <- seq(lubridate::ymd(start_date), by = 'days', length.out = n_days)

      # 1. Generate global environmental data (same for all participants each day)
      private$generate_global_env_data(dates)

      # 2. Create the base grid and join global data
      data_grid <- private$create_data_grid(participants, dates)

      # 3. Generate data for each participant
      self$data <- purrr::map_dfr(participants,
                                  ~private$generate_person_data(.x, data_grid),
                                  .id = NULL) %>%
        dplyr::filter(!is.na(.data$WellnessScore)) # Remove rows where target couldn't be calculated

      message("Data generation complete. Dataset dimensions: ",
              paste(dim(self$data), collapse = " x "))

      if(save_csv) {
        readr::write_csv(self$data, file = file_path)
        message("Data saved to: ", file_path)
      }

      invisible(self$data)
    },

    #' @description
    #' Prints a concise summary of the dataset.
    #' @param ... Additional arguments passed to `summary`.
    print = function(...) {
      if(is.null(self$data)) {
        message("No data available. Run $generate_data() first.")
      } else {
        cat("WellnessData Dataset Summary\n")
        cat("===========================\n")
        cat("Period     : ", as.character(min(self$data$DateTime)),
            "to", as.character(max(self$data$DateTime)), "\n")
        cat("Participants: ", length(unique(self$data$ParticipantID)), "\n")
        cat("Rows       : ", nrow(self$data), "\n")
        cat("Variables  : ", ncol(self$data), "\n\n")
        print(summary(self$data[, sapply(self$data, is.numeric)]), ...)
      }
    },

    #' @description
    #' Creates a comprehensive time series plot for a selected participant.
    #' @param participant_id The ID of the participant to plot (e.g., "P01").
    #' @param alpha Transparency level for the plot lines. Default is 0.7.
    #' @param base_size Base font size for the plot. Default is 12.
    #' @return A ggplot object.
    plot_participant_timeseries = function(participant_id, alpha = 0.7, base_size = 12) {
      private$validate_data()

      plot_data <- self$data %>%
        dplyr::filter(.data$ParticipantID == participant_id) %>%
        dplyr::select("DateTime", "ActivityLevel", "AvgHeartRate",
                      "SleepHours", "WorkHours", "WellnessScore") %>%
        tidyr::pivot_longer(cols = -"DateTime",
                            names_to = "Metric",
                            values_to = "Value")

      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$DateTime, y = .data$Value, color = .data$Metric)) +
        ggplot2::geom_line(alpha = alpha, linewidth = 0.8) +
        ggplot2::geom_point(alpha = alpha, size = 1) +
        ggplot2::facet_wrap(~ .data$Metric, scales = "free_y", ncol = 1) +
        ggplot2::labs(
          title = paste("Daily Metrics for Participant:", participant_id),
          x = "Date",
          y = "Value",
          color = "Metric"
        ) +
        ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
        )
    },

    #' @description
    #' Creates a scatterplot matrix to visualize relationships between numeric variables.
    #' @param variables Character vector of numeric variable names to include. If NULL, uses all numeric vars.
    #' @param alpha Transparency level for points. Default is 0.6.
    #' @param base_size Base font size for the plot. Default is 10.
    #' @return A GGally::ggpairs plot object.
    plot_correlation_matrix = function(variables = NULL, alpha = 0.6, base_size = 10) {
      private$validate_data()

      if (is.null(variables)) {
        num_data <- self$data %>%
          dplyr::select(where(is.numeric), -"DateTime") # Exclude DateTime
      } else {
        num_data <- self$data %>%
          dplyr::select(dplyr::any_of(variables))
      }

      # Use GGally if available, else fall back to a warning
      if(requireNamespace("GGally", quietly = TRUE)) {
        p <- GGally::ggpairs(num_data,
                             title = "Correlation Matrix of Numeric Variables",
                             progress = FALSE,
                             aes(alpha = alpha)) +
          ggplot2::theme_minimal(base_size = base_size) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
        return(p)
      } else {
        warning("Please install.packages('GGally') for correlation matrix plots.")
        return(invisible(NULL))
      }
    },

    #' @description
    #' Plots the average of a metric across participants over time.
    #' @param metric The name of the numeric metric to plot (e.g., "WellnessScore").
    #' @param ribbon Logical. Should a confidence interval ribbon be displayed? Default is TRUE.
    #' @param alpha Transparency level for the ribbon. Default is 0.2.
    #' @param base_size Base font size for the plot. Default is 12.
    #' @return A ggplot object.
    plot_metric_trend = function(metric, ribbon = TRUE, alpha = 0.2, base_size = 12) {
      private$validate_data()
      private$validate_numeric_metric(metric)

      trend_data <- self$data %>%
        dplyr::group_by(.data$DateTime) %>%
        dplyr::summarise(
          Mean = mean(.data[[metric]], na.rm = TRUE),
          SD = sd(.data[[metric]], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(
          Lower = .data$Mean - 1.96 * .data$SD / sqrt(nrow(self$data)/length(unique(self$data$DateTime))),
          Upper = .data$Mean + 1.96 * .data$SD / sqrt(nrow(self$data)/length(unique(self$data$DateTime)))
        )

      p <- ggplot2::ggplot(trend_data, ggplot2::aes(x = .data$DateTime, y = .data$Mean)) +
        ggplot2::geom_line(color = "steelblue", linewidth = 1) +
        ggplot2::geom_point(color = "steelblue", size = 2) +
        ggplot2::labs(
          title = paste("Daily Average Trend of", metric),
          x = "Date",
          y = metric
        ) +
        ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
        )

      if(ribbon) {
        p <- p +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper),
                               alpha = alpha, fill = "steelblue")
      }

      return(p)
    },

    #' @description
    #' Creates a bar plot showing the distribution of the environmental temperature variable.
    #' @param fill_color Color for the bars. Default is "coral".
    #' @param base_size Base font size for the plot. Default is 12.
    #' @return A ggplot object.
    plot_temperature_distribution = function(fill_color = "coral", base_size = 12) {
      private$validate_data()

      temp_data <- self$data %>%
        dplyr::count(.data$EnvTemperature) %>%
        dplyr::mutate(EnvTemperature = factor(.data$EnvTemperature,
                                              levels = c("Cold", "Mild", "Warm")))

      ggplot2::ggplot(temp_data, ggplot2::aes(x = .data$EnvTemperature, y = .data$n, fill = .data$EnvTemperature)) +
        ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = .data$n), vjust = -0.5, size = 4) +
        ggplot2::scale_fill_manual(values = c("Cold" = "lightblue", "Mild" = "lightgreen", "Warm" = fill_color)) +
        ggplot2::labs(
          title = "Distribution of Environmental Temperature",
          x = "Temperature Category",
          y = "Number of Observations"
        ) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
        )
    }
  ),

  private = list(
    global_env_data = NULL,

    set_seed = function() {
      set.seed(self$seed)
    },

    validate_data = function() {
      if(is.null(self$data)) {
        stop("No data available. Please run $generate_data() first.")
      }
    },

    validate_numeric_metric = function(metric) {
      if(!metric %in% names(self$data)) {
        stop("Metric '", metric, "' not found in the dataset.")
      }
      if(!is.numeric(self$data[[metric]])) {
        stop("Metric '", metric, "' is not numeric.")
      }
    },

    generate_global_env_data = function(dates) {
      n_days <- length(dates)
      temp_trend <- sin(2 * pi * (1:n_days) / 30) * 0.8
      temp_ar <- arima.sim(model = list(ar = 0.7), n = n_days, sd = 0.3)
      temp_continuous <- scale(temp_trend + temp_ar)

      temp_cut <- cut(temp_continuous,
                      breaks = c(-Inf, -0.5, 0.5, Inf),
                      labels = c("Cold", "Mild", "Warm"))

      private$global_env_data <- tibble::tibble(
        DateTime = dates,
        EnvTemperature = temp_cut
      )
    },

    create_data_grid = function(participants, dates) {
      base_grid <- expand.grid(
        DateTime = dates,
        ParticipantID = participants,
        stringsAsFactors = FALSE
      ) %>%
        dplyr::arrange(.data$ParticipantID, .data$DateTime)

      # Join the global environmental data
      dplyr::left_join(base_grid, private$global_env_data, by = "DateTime")
    },

    generate_person_data = function(id, global_df) {
      person_df <- global_df %>% dplyr::filter(.data$ParticipantID == id)
      n_days <- nrow(person_df)

      # Base values for this person (introducing heterogeneity)
      base_vals <- list(
        activity = rnorm(1, 3.0, 0.5),
        hr = rnorm(1, 72, 3),
        sleep = rnorm(1, 6.8, 0.5),
        work = rnorm(1, 7.5, 1.0)
      )

      # Generate autocorrelated data
      ar_sim <- function(n, base, phi, sd) {
        x <- numeric(n)
        x[1] <- base + rnorm(1, 0, sd)
        for (i in 2:n) {
          x[i] <- base + phi * (x[i-1] - base) + rnorm(1, 0, sd)
        }
        return(x)
      }

      activity <- ar_sim(n_days, base_vals$activity, phi = 0.6, sd = 0.4)
      heartrate <- ar_sim(n_days, base_vals$hr, phi = 0.5, sd = 2.5)
      sleep <- ar_sim(n_days, base_vals$sleep, phi = 0.7, sd = 0.5)
      work <- ar_sim(n_days, base_vals$work, phi = 0.5, sd = 0.8)

      # Apply realistic constraints and relationships
      sleep <- pmax(pmin(sleep, 10), 4)
      activity <- pmax(activity, 0)
      work <- pmax(pmin(work, 12), 0)

      # Introduce dependencies
      activity <- activity - (work - mean(work)) * 0.2
      heartrate <- heartrate + (work - mean(work)) * 0.5

      # Temperature effect
      temp_effect <- dplyr::case_when(
        person_df$EnvTemperature == "Cold" ~ -0.4,
        person_df$EnvTemperature == "Mild" ~ 0,
        person_df$EnvTemperature == "Warm" ~ 0.6
      )
      activity <- activity + temp_effect

      # Generate Mood Text
      mood_sentiment_score <- (sleep - mean(sleep)) / sd(sleep) +
        (activity - mean(activity)) / sd(activity) -
        (work - mean(work)) / sd(work) * 0.7

      mood_text <- private$generate_mood_text(mood_sentiment_score, person_df$EnvTemperature)

      # Generate Target: Wellness Score for next day
      wellness_next_day <- private$calculate_wellness_score(
        activity, heartrate, sleep, work, mood_text, n_days
      )

      # Return final tibble
      tibble::tibble(
        ParticipantID = id,
        DateTime = person_df$DateTime,
        ActivityLevel = round(activity, 1),
        AvgHeartRate = round(heartrate, 0),
        SleepHours = round(sleep, 1),
        WorkHours = round(work, 1),
        EnvTemperature = person_df$EnvTemperature,
        MoodText = mood_text,
        WellnessScore = dplyr::lag(round(wellness_next_day, 0))
      )
    },

    generate_mood_text = function(sentiment_score, temperature) {
      mood_options <- list(
        positive = c("Felt great today!", "Very productive and happy",
                     "Energetic and motivated", "Calm and relaxed"),
        neutral = c("Day was ok, nothing special", "Feeling average", "A bit tired"),
        negative = c("Stressed and overwhelmed", "Didn't sleep well, feeling groggy",
                     "Low energy, not very active", "Burnt out from work"),
        weather = c("Nice day outside", "Too cold to go out", "The heat was draining")
      )

      purrr::map2_chr(sentiment_score, temperature, function(score, temp) {
        weather_comment <- ""
        if (runif(1) < 0.2) {
          weather_comment <- dplyr::case_when(
            temp == "Warm" ~ " Nice day outside.",
            temp == "Cold" ~ " Too cold to go out.",
            TRUE ~ ""
          )
        }

        base_mood <- dplyr::case_when(
          score > 1 ~ paste0(sample(mood_options$positive, 1), weather_comment),
          score < -1 ~ paste0(sample(mood_options$negative, 1), weather_comment),
          TRUE ~ paste0(sample(mood_options$neutral, 1), weather_comment)
        )
        return(base_mood)
      })
    },

    calculate_wellness_score = function(activity, heartrate, sleep, work, mood_text, n_days) {
      beta <- list(
        activity = 3,
        sleep = 5,
        hr = -0.8,
        work = -2,
        mood_positive = 8,
        mood_negative = -6
      )

      base_score <- 70
      wellness <- numeric(n_days)

      for (i in 1:(n_days-1)) {
        mood_effect <- dplyr::case_when(
          stringr::str_detect(mood_text[i], "great|productive|happy|Energetic|relaxed|motivated") ~ beta$mood_positive,
          stringr::str_detect(mood_text[i], "tired|Stressed|groggy|Low energy|Burnt out") ~ beta$mood_negative,
          TRUE ~ 0
        )

        wellness[i] <- base_score +
          beta$activity * (activity[i] - 3.0) +
          beta$sleep * (sleep[i] - 6.8) +
          beta$hr * (heartrate[i] - 72) +
          beta$work * (work[i] - 7.5) +
          mood_effect +
          rnorm(1, 0, 3)
      }
      wellness <- pmin(pmax(wellness, 0), 100)
      wellness[n_days] <- NA

      return(wellness)
    }
  )
)
