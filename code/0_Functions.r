##################################################
# ASIP-Study: Functions ##########################
# Valentin Vetter, valentin.vetter@charite.de ####
# Last Modified: December 15, 2024 ###############
##################################################

# Wrangle REDCap Data ####
library(Hmisc)
fct_polish_REDCap_data <- function(data) {

  data$dem_sex_birth <- factor(data$dem_sex_birth, levels = c("Männlich", "Weiblich"))
  data$dem_health <- factor(data$dem_health, levels = c("Sehr gut", "Gut", "Mittelmäßig", "Schlecht", "Sehr schlecht"))
  data$lifest_smoking <- factor(data$lifest_smoking, levels = c("Ja, ich rauche", "Nein, nicht mehr, aber ich habe früher geraucht", "Ich habe noch nie geraucht"))
  data$lifest_alc_1_modified <- factor(data$lifest_alc_1_modified, levels = c("once per week or more", "less than once per week", "not in past 12 months or never"))
  data$work_place <- factor(data$work_place, levels = c("einer Praxis", "einer Klinik", "keiner der oben genannten Optionen"))
  
  return(data)
}

# TABLE 1 ####
library(tableone)
table1_desc_fct <- function(data, vec_var_table1){
  table_1 <- print(
    CreateTableOne(
      data = data,
      vars = vec_var_table1
    ),
    contDigits = 2
  ) |>
    data.frame()
  
  table_1
  
  table_1 <- cbind("variables" = rownames(table_1),
                   table_1)
  
  table_1 <- table_1[-1,]
  
  table_1$n <- NA
  table_1$n_missing <- NA
  table_1$min <- NA
  table_1$max <- NA
  for (h in 1:nrow(table_1)) {
    variable_data <- data[vec_var_table1[h]]
    table_1$n[h] <- sum(is.na(variable_data)==FALSE)
    table_1$n_missing[h] <- nrow(data) - table_1$n[h]
    
    variable_data <- sapply(variable_data, as.numeric)
    table_1$min[h] <- round(min(variable_data, na.rm=TRUE), 2)
    table_1$max[h] <- round(max(variable_data, na.rm=TRUE), 2)
  }
  
  overall_n <- c(paste("Overall: ", nrow(data), " samples", sep = ""),
                 NA,
                 NA,
                 NA,
                 NA,
                 NA)
  table_1 <- rbind(overall_n, table_1)
  rownames(table_1) <- NULL
  return(table_1)
}


# Functions that provide results from continuous and discrete variables that can be easily combined in one table.
## Continuously scales variables
fct_table1_cont <- function(data, name){
  results <- data.frame(cbind("Name" = name,
                              "Mean_perc" = mean(data, na.rm=TRUE),
                              "SD" = sd(data, na.rm=TRUE),
                              "Min" = min(data, na.rm=TRUE),
                              "Max" = max(data, na.rm=TRUE),
                              "n" = sum(is.na(data)==FALSE)))
  results[2:6] <- round(as.numeric(results[2:6]), 2)
  
  return(results)
}

## Discrete variables
fct_table1_discr <- function(data, name){
  data_2 <- data.frame("var" = data)
  results <- CreateTableOne(data = data_2,
                            vars = "var")
  results <- data.frame(print(results))
  names(results) <- "Mean_perc"
  results <- cbind("Name" = rownames(results), 
                   results,
                   "SD" = rep(NA, nrow(results)),
                   "Min" = rep(NA, nrow(results)),
                   "Max" = rep(NA, nrow(results)),
                   "n" = rep(NA, nrow(results)))
  results <- results[-1,]
  results$n[1] <- sum(is.na(data)==FALSE)
  results[1,1] <- name
  return(results)
}

# Table 1 ####
table_1_function <- function(data) {
  table_1 <- rbind(
    fct_table1_cont(data = data$Age, name = "Age (years)"),
    fct_table1_discr(data = data$dem_sex_birth, name = "Sex (female)"),
    fct_table1_discr(data = data$dem_health, name = "Overall Health"),
    fct_table1_cont(data = data$lifest_sport_1, name = "Sport (days/week)"),
    fct_table1_discr(data = data$lifest_alc_1_modified, name = "Alcohol"),
    fct_table1_discr(data = data$lifest_smoking, name = "Smoking"),
    fct_table1_cont(data = data$work_year, name = "Year of residency"),
    fct_table1_cont(data = data$work_hours_contract, name = "Workhours as per contract (h/week)"),
    fct_table1_cont(data = data$work_overtime, name = "Average overtime (h/week)"),
    fct_table1_discr(data = data$work_place, name = "Workplace"),
    fct_table1_discr(data = data$work_shift, name = "Shift work (no)"),
    fct_table1_cont(data = data$copsoq_score, name = "Privacy-Conflict-Scale"),
    fct_table1_cont(data = data$cbi_score, name = "Copenhagen Burnout Inventory"),
    fct_table1_cont(data = data$eri_score_ERratio, name = "Effort Reward Imbalance"),
    fct_table1_cont(data = data$oc_score, name = "Over-Commitment Scale"),
    fct_table1_cont(data = data$pss_score, name = "Perceived Stress Scale"),
    fct_table1_cont(data = data$swls_score, name = "Satisfaction With Life Scale")
  )

  table_1$Name <- c(
    "Age (years)",
    "Sex (female)",
    "Overall Health",
    "Very good",
    "Good",
    "Mediocre",
    "Bad",
    "Very Bad",
    "Sport (days/week)",
    "Alcohol",
    "once per week or more",
    "less than once per week",
    "not in past 12 months or never",
    "Smoking",
    "Yes, current",
    "No, former smoker",
    "No, never smoker",
    "Year of residency",
    "Workhours as per contract (h/week)",
    "Average overtime (h/week)",
    "Workplace",
    "Practice",
    "Hospital",
    "Other",
    "Shift work (no)",
    "Privacy-Conflict-Scale",
    "Copenhagen Burnout Inventory",
    "Effort Reward Imbalance",
    "Over-Commitment Scale",
    "Perceived Stress Scale",
    "Satisfaction With Life Scale"
  )
  table_1 <- select(table_1, -Min, -Max)
  return(table_1)
}


# Line Plots ####
fct_line_plot_all <- function(data, y_var, title, ylab) {
  if (data$sequence[1] == "ABAB") {
    block_1_3_name <- "Intervention"
    
    block_2_4_name <- "Control"
  } else {
    block_1_3_name <- "Control"
    
    block_2_4_name <- "Intervention"
  }
  
  data$current_day_of_study_harmonized_histogram <- data$current_day_of_study_harmonized-0.25
  
  alpha_background_periods <- 0.3
  alpha_background_Sat_Sun <- 0.5
  alpha_smooth <- 0.001
  color_smoth <- "darkblue"
  span <- 0.9
  
  plot <- ggplot() +
    # intervention/control periods
    geom_rect(
      aes(
        xmin = 1,
        xmax = 7.5,
        ymin = -Inf,
        ymax = Inf,
        fill = block_1_3_name
      ),
      alpha = alpha_background_periods
    ) +
    geom_rect(
      aes(
        xmin = 7.5,
        xmax = 14.5,
        ymin = -Inf,
        ymax = Inf,
        fill = block_2_4_name
      ),
      alpha = alpha_background_periods
    ) +
    geom_rect(
      aes(
        xmin = 14.5,
        xmax = 21.5,
        ymin = -Inf,
        ymax = Inf,
        fill = block_1_3_name
      ),
      alpha = alpha_background_periods
    ) +
    geom_rect(
      aes(
        xmin = 21.5,
        xmax = 28.5,
        ymin = -Inf,
        ymax = Inf,
        fill = block_2_4_name
      ),
      alpha = alpha_background_periods
    ) +
    
    # Saturday/Sunday
    geom_rect(
      aes(
        xmin = 5.5,
        xmax = 7.5,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey20",
      alpha = alpha_background_Sat_Sun
    ) +
    geom_rect(
      aes(
        xmin = 12.5,
        xmax = 14.5,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey20",
      alpha = alpha_background_Sat_Sun
    ) +
    geom_rect(
      aes(
        xmin = 19.5,
        xmax = 21.5,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey20",
      alpha = alpha_background_Sat_Sun
    ) +
    geom_rect(
      aes(
        xmin = 26.5,
        xmax = 28.5,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey20",
      alpha = alpha_background_Sat_Sun
    ) +
    # geom_point for marginal histogramms
    geom_point(
      data = data,
      aes_string(x = "current_day_of_study_harmonized_histogram", y = y_var),
      alpha = 0
    ) +
    # lines
    ## Period: 1
    geom_line(
      data = filter(data, current_period_of_study == 1),
      aes_string(x = "current_day_of_study_harmonized", y = y_var, color = "manuscript_ID"),
      alpha = 0.75
    ) +
    ## Period: 2
    geom_line(
      data = filter(data, current_period_of_study == 2),
      aes_string(x = "current_day_of_study_harmonized", y = y_var, color = "manuscript_ID"),
      alpha = 0.75
    ) +
    ## Period: 3
    geom_line(
      data = filter(data, current_period_of_study == 3),
      aes_string(x = "current_day_of_study_harmonized", y = y_var, color = "manuscript_ID"),
      alpha = 0.75
    ) +
    ## Period: 4
    geom_line(
      data = filter(data, current_period_of_study == 4),
      aes_string(x = "current_day_of_study_harmonized", y = y_var, color = "manuscript_ID"),
      alpha = 0.75
    ) +
    # Smooth Lines
    geom_smooth(
      data = filter(data, current_period_of_study == 1),
      aes_string(x = "current_day_of_study_harmonized", y = y_var),
      se = FALSE,
      color = color_smoth,
      alpha = alpha_smooth,
      span = span
    ) +
    geom_smooth(
      data = filter(data, current_period_of_study == 2),
      aes_string(x = "current_day_of_study_harmonized", y = y_var),
      se = FALSE,
      color = color_smoth,
      alpha = alpha_smooth,
      span = span
    ) +
    geom_smooth(
      data = filter(data, current_period_of_study == 3),
      aes_string(x = "current_day_of_study_harmonized", y = y_var),
      se = FALSE,
      color = color_smoth,
      alpha = alpha_smooth,
      span = span
    ) +
    geom_smooth(
      data = filter(data, current_period_of_study == 4),
      aes_string(x = "current_day_of_study_harmonized", y = y_var),
      se = FALSE,
      color = color_smoth,
      alpha = alpha_smooth,
      span = span
    ) +
    # Figure Properties
    ylim(0, 10) +
    scale_x_continuous(
      breaks = seq(0, 29, 1),
      label = c("", rep(
        c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        ),
        4
      ), ""),
      guide = guide_axis(angle = 90)
    ) +
    scale_fill_manual(values = c("grey90", "grey30"), name = "Period") +
    scale_colour_grey(
      start = 0.4,
      end = 0.9,
      guide = "none"
    ) +
    xlab("Day of study (1 - 28)") +
    ylab(ylab) +
    labs(title = title) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  plot2 <- ggMarginal(plot,
                      type = "histogram",
                      margins = "x",
                      binwidth = 0.99,
                      size = 4,
                      fill = "darkblue",
                      color = "white")
  
  return(plot2)
  
}


# Analysis: individual trials ####
fct_individual_trials <- function(data_StudyU, vec_ID, formula_bayes) {
  results_table <- c()
  
  for (i in 1:length(vec_ID)) {
    data <- filter(data_StudyU, manuscript_ID == vec_ID[i])
    
    data_intervention <-
      data$survey1_question1_response[data$intervention_period == 1]
    data_control <-
      data$survey1_question1_response[data$intervention_period == 0]
    
    
    if (length(data_intervention) > 1 &
        length(data_control) > 1) {
      # T-Test
      test_res <- t.test(data_intervention, data_control)
      
      # Bayesian Models
      model <- brm(
        formula = formula_bayes,
        data = data,
        warmup = warmup,
        iter = iter,
        cores = 2,
        chains = chains,
        seed = 123
      )
      
      results <- data.frame(
        cbind(
          "ID" = vec_ID[i],
          "Mean_Invt" = mean(data_intervention),
          "SD_Invt" = sd(data_intervention),
          "Min_Invt" = min(data_intervention),
          "Max_Invt" = max(data_intervention),
          "n_Invt" = length(data_intervention),
          "Mean_Ctrl" = mean(data_control),
          "SD_Ctrl" = sd(data_control),
          "Min_Ctrl" = min(data_control),
          "Max_Ctrl" = max(data_control),
          "n_Ctrl" = length(data_control),
          "Diff" = mean(data_intervention) - mean(data_control),
          "n_all" = length(data_intervention) + length(data_control),
          "p_value" = test_res$p.value,
          "Estimate" = summary(model)$fixed[2, 1],
          "Est.Error" = summary(model)$fixed[2, 2],
          "l_95%CI" = summary(model)$fixed[2, 3],
          "u_95%CI" = summary(model)$fixed[2, 4],
          "prob_clinc_effect" = mean(as_draws_df(model)$b_intervention_period < -0.5),
          "prob_below_0" = mean(as_draws_df(model)$b_intervention_period < 0),
          "Rhat" = summary(model)$fixed[2, 5],
          "Bulk_ESS" = summary(model)$fixed[2, 6],
          "Tail_ESS" = summary(model)$fixed[2, 6]
        )
      )
      results[, 2:13] <- sapply(results[, 2:13], as.numeric)
      results[, 2:13] <- round(results[, 2:13], 2)
      
      
      results_table <- rbind(results_table, results)
    }
    else {
      results <- data.frame(
        cbind(
          "ID" = vec_ID[i],
          "Mean_Invt" = NA,
          "SD_Invt" = NA,
          "Min_Invt" = NA,
          "Max_Invt" = NA,
          "n_Invt" = length(data_intervention),
          "Mean_Ctrl" = NA,
          "SD_Ctrl" = NA,
          "Min_Ctrl" = NA,
          "Max_Ctrl" = NA,
          "n_Ctrl" = length(data_control),
          "Diff" = NA,
          "n_all" = length(data_intervention) + length(data_control),
          "p_value" = NA,
          "Estimate" = NA,
          "Est.Error" = NA,
          "l_95%CI" = NA,
          "u_95%CI" = NA,
          "prob_clinc_effect" = NA,
          "prob_below_0" = NA,
          "Rhat" = NA,
          "Bulk_ESS" = NA,
          "Tail_ESS" = NA
        )
      )
      
      
      results_table <- rbind(results_table, results)
      
    }
    
    results_table[, 2:13] <- sapply(results_table[, 2:13], as.numeric)
    
  }
  
  return(results_table)
}


# Analysis: aggregated trials ####
## Calculate Models ####
fct_aggregated_trials <- function(data_StudyU, formula_bayes, outcome_var) {
  results <- c()
  data <- data_StudyU
  
  data_intervention <- filter(data, intervention_period == 1) |>
    select(any_of(outcome_var))
  data_intervention <- as.numeric(data_intervention[,1])
  
  data_control <- filter(data, intervention_period == 0) |>
    select(any_of(outcome_var))
  data_control <- as.numeric(data_control[,1])
  
  # data_intervention <-
  #   data$survey1_question1_response[data$intervention_period == 1]
  # data_control <-
  #   data$survey1_question1_response[data$intervention_period == 0]
  
  # T-Test
  test_res <- t.test(data_intervention, data_control)
  
  # Bayesian Models
  model <- brm(
    formula = formula_bayes,
    data = data,
    warmup = warmup,
    iter = iter,
    cores = 2,
    chains = chains,
    seed = 123
  )
  
  results <- data.frame(
    cbind(
      "Mean_Invt" = mean(data_intervention),
      "SD_Invt" = sd(data_intervention),
      "Min_Invt" = min(data_intervention),
      "Max_Invt" = max(data_intervention),
      "n_Invt" = length(data_intervention),
      "Mean_Ctrl" = mean(data_control),
      "SD_Ctrl" = sd(data_control),
      "Min_Ctrl" = min(data_control),
      "Max_Ctrl" = max(data_control),
      "n_Ctrl" = length(data_control),
      "Diff" = mean(data_intervention) - mean(data_control),
      "n_all" = length(data_intervention) + length(data_control),
      "p_value" = test_res$p.value,
      "Estimate" = summary(model)$fixed[2, 1],
      "Est.Error" = summary(model)$fixed[2, 2],
      "l_95%CI" = summary(model)$fixed[2, 3],
      "u_95%CI" = summary(model)$fixed[2, 4],
      "prob_clinc_effect" = mean(as_draws_df(model)$b_intervention_period < -0.5),
      "Rhat" = summary(model)$fixed[2, 5],
      "Bulk_ESS" = summary(model)$fixed[2, 6],
      "Tail_ESS" = summary(model)$fixed[2, 6]
    )
  )
  results[, 2:13] <- sapply(results[, 2:13], as.numeric)
  results[, 2:13] <- round(results[, 2:13], 2)
  
  
  
  
  results[, 2:13] <- sapply(results[, 2:13], as.numeric)
  
  result_list <- list()
  result_list[[1]] <- results
  result_list[[2]] <- model
  
  return(result_list)
}


## Wrapper ####
# calculate across interventions and outcomes
fct_aggregated_trials_all_together <- function(ASIP_data_Nof1_MB,
                                               ASIP_data_Nof1_BB,
                                               formula_bayes_daily_stress = formula_bayes_daily_stress,
                                               formula_bayes_next_day = formula_bayes_next_day) {
  
  # Daily Stress, Mindfulness Breathing
  results_aggregated_daily_stress_1 <- fct_aggregated_trials(
    data_StudyU = ASIP_data_Nof1_MB,
    formula_bayes = formula_bayes_daily_stress,
    outcome_var = "survey1_question1_response"
  )
  
  # Daily Stress, Box Breathing
  results_aggregated_daily_stress_2 <- fct_aggregated_trials(
    data_StudyU = ASIP_data_Nof1_BB,
    formula_bayes = formula_bayes_daily_stress,
    outcome_var = "survey1_question1_response"
  )
  
  # Next Day Stress, Mindfulness Breathing
  results_aggregated_next_day_1 <- fct_aggregated_trials(
    data_StudyU = ASIP_data_Nof1_MB,
    formula_bayes =
      formula_bayes_next_day,
    outcome_var = "survey1_question2_response"
  )
  
  # Next Day Stress, Box Breathing
  results_aggregated_next_day_2 <- fct_aggregated_trials(
    data_StudyU = ASIP_data_Nof1_BB,
    formula_bayes =
      formula_bayes_next_day,
    outcome_var = "survey1_question2_response"
  )
  
  result_list <- list()
  result_list <- c("results_aggregated_daily_stress_1"=results_aggregated_daily_stress_1,
                   "results_aggregated_daily_stress_2"=results_aggregated_daily_stress_2,
                   "results_aggregated_next_day_1"=results_aggregated_next_day_1,
                   "results_aggregated_next_day_2"=results_aggregated_next_day_2)
  
  return(result_list)
}

# Plot: Pointrange Plots ####
fct_indiv_trials_pointrange <- function(data, title) {
  data$Estimate <- as.numeric(data$Estimate)
  data$l_95.CI <- as.numeric(data$l_95.CI)
  data$u_95.CI <- as.numeric(data$u_95.CI)
  
  data$ID <- factor(seq(1, nrow(data)),
                    labels = data$ID)
  
  alpha_background <- 1
  
  plot <- ggplot(data = data, aes(
    x = ID,
    y = Estimate,
    ymin = l_95.CI,
    ymax = u_95.CI
  )) +
    geom_rect(aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = 0,
      ymax = Inf,
    ),
    fill = color_larger_0,
    alpha = alpha_background) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = cut_off_clinically_relevant,
        ymax = 0
      ),
      fill = color_0_to_clin_relevant,
      alpha = alpha_background
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = cut_off_clinically_relevant
      ),
      fill = color_smaller_clin_relevant,
      alpha = alpha_background
    ) +
    geom_errorbar() +
    geom_point() +
    labs(title = title) +
    ylim(-8, 6) +
    theme_classic() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 6
      ),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  return(plot)
}

# Plot: Barplots ####
fct_indiv_trials_barplot <- function(data, title) {
  data$prob_clinc_effect <- as.numeric(data$prob_clinc_effect)
  data$prob_below_0 <- as.numeric(data$prob_below_0)
  
  data <- rbind(
    cbind(
      "Var" = "Intervention Effect below -0.05",
      "ID" = data$ID,
      "prob_clinc_effect" = data$prob_clinc_effect
    ),
    cbind(
      "Var" = "Intervention Effect between 0 and -0.05",
      "ID" = data$ID,
      "prob_clinc_effect" = data$prob_below_0 - data$prob_clinc_effect
    ),
    cbind(
      "Var" = "Intervention Effect of 0 or higher",
      "ID" = data$ID,
      "prob_clinc_effect" = 1 - data$prob_below_0
    )
  ) |> data.frame()
  
  data$prob_clinc_effect <- as.numeric(data$prob_clinc_effect)
  
  data$ID <- factor(seq(1, nrow(data)), labels = data$ID)
  
  data$Var <- factor(
    data$Var,
    levels = c(
      "Intervention Effect of 0 or higher",
      "Intervention Effect between 0 and -0.05",
      "Intervention Effect below -0.05"
    )
  )
  
  
  plot <- ggplot(data = data, aes(x = ID, y = prob_clinc_effect, fill = Var)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = probability_responder, linetype = "dashed") +
    scale_fill_manual(values = c(
      color_larger_0,
      color_0_to_clin_relevant,
      color_smaller_clin_relevant
    )) +
    scale_y_continuous(breaks = c(seq(0, 1, 0.1))) +
    ylab("Probability") +
    labs(title = title) +
    theme_classic() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 6
      ),
      legend.position = "bottom",
      legend.title=element_blank()
    )
  
  return(plot)
}


# Plot: Aggregated Analyses ####
fct_aggregated_trials_cum_prob_plot <- function(model, title) {
  y_effect_smaller_0 <- mean(as_draws_df(model)$b_intervention_period < 0)
  y_effect_smaller_clin_rel <- mean(as_draws_df(model)$b_intervention_period < cut_off_clinically_relevant)
  
  # plot_dat <- data.frame("x" = as_draws_df(model)$b_intervention_period)
  
  beta  <- sort(as_draws_df(model)$b_intervention_period)
  plot_dat <- data.frame(x = beta, y = seq_along(beta) / length(beta))
  
  # if (min(plot_dat$x) > cut_off_clinically_relevant) {
  #   plot_dat <- data.frame(rbind(cbind("x" = -0.5, "y" = 0), plot_dat))
  #   plot_dat <- data.frame(rbind(cbind(
  #     "x" = -0.5001, "y" = 0
  #   ), plot_dat))
  # }
  
  # plot_dat <- data.frame(rbind(cbind("x" = -1, "y" = 0), plot_dat))
  
  plot_dat$Var <- c()
  plot_dat$Var[plot_dat$x >= 0] <- "Intervention Effect of 0 or higher"
  plot_dat$Var[plot_dat$x < 0 &
                 plot_dat$x >= cut_off_clinically_relevant] <- "Intervention Effect between 0 and -0.05"
  plot_dat$Var[plot_dat$x < cut_off_clinically_relevant] <- "Intervention Effect below -0.05"
  
  plot_dat$Var <- factor(plot_dat$Var,
                         levels = c("Intervention Effect of 0 or higher",
                                    "Intervention Effect between 0 and -0.05",
                                    "Intervention Effect below -0.05"))
  
  plot <- ggplot() +
    geom_area(data = plot_dat,
              aes(x = x, y = y, fill = Var),
              color = "black") +
    # stat_ecdf(data = plot_dat, aes(x, fill = Var)) +
    # effect smaller 0
    ##horizontal
    geom_segment(aes(
      x = -Inf,
      y = y_effect_smaller_0,
      xend = 0,
      yend = y_effect_smaller_0
    ),
    linetype = "dashed") +
    ##vertical
    geom_segment(aes(
      x = 0,
      y = -Inf,
      xend = 0,
      yend = y_effect_smaller_0
    ), linetype = "dashed") +
    ##annotate
    annotate(
      geom = "text",
      x = -0.925,
      y = y_effect_smaller_0 + 0.05,
      label = round(y_effect_smaller_0, 3),
      color = "black"
    ) +
    geom_segment(
      aes(
        x = -1,
        y = 0,
        xend = min(plot_dat$x)+0.13,
        yend = 0
      ),
      linetype = "solid"
    ) +
    # effect smaller clinically relevant
    ##horizontal
    geom_segment(
      aes(
        x = -Inf,
        y = y_effect_smaller_clin_rel,
        xend = cut_off_clinically_relevant,
        yend = y_effect_smaller_clin_rel
      ),
      linetype = "dashed"
    ) +
    ##vertical
    geom_segment(
      aes(
        x = cut_off_clinically_relevant,
        y = -Inf,
        xend = cut_off_clinically_relevant,
        yend = y_effect_smaller_clin_rel
      ),
      linetype = "dashed"
    ) +
    ##annotate
    annotate(
      geom = "text",
      x = -0.925,
      y = y_effect_smaller_clin_rel + 0.05,
      label = round(y_effect_smaller_clin_rel, 3),
      color = "black"
    ) +
    scale_fill_manual(values = c(
      color_larger_0,
      color_0_to_clin_relevant,
      color_smaller_clin_relevant
    )) +
    labs(title = title) +
    xlim(-1, 0.5) +
    ylab("Cumulative Probability") +
    xlab("Intervention Effect") +
    theme_classic() +
    theme(legend.title = element_blank())
  
  
  return(plot)
}
