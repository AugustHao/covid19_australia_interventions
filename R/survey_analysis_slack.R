# Code that produces plots and csvs for test-seeking behaviour to post on slack and upload to mediaflux

library(tidyverse)
#library(lubridate)
#library(ggplot2)
#library(cowplot)
library(zoo)
#library(vroom)
library(lemon)

# Set working directory
#setwd("~/M work stuff/covid19/survey data")

#### Functions ####
# Change quoted to yes/no
quoted_to_yesno <- function(x) {
  case_when(
    x == "quoted" ~ "Yes",
    x == "not quoted" ~ "No"
  )
}


# Function to load a single doh survey (modified from Nick G github)
parse_doh_survey_for_test_seeking <- function(filename) {
  
  # Abbreviate state names and add new column for state
  survey_doh <- read_csv(filename,
                         col_types = cols(.default = col_character())) %>%
    mutate(
      wave = wave_from_file(filename),
      state = abbreviate_states(S3),
      date = as.Date(StartDate, format = "%Y%m%d")
    )
  
 if (min(survey_doh$date) >= as.Date("2023-06-20")) { # start of flu questions being included in survey
   
   # Select columns of interest
   survey_doh %>%
     select(
       wave,
       state,
       age = S1,
       gender = S2,
       postcode = Q37,
       exp_cough = Q228_1,
       exp_fever = Q228_2,
       exp_breathing = Q228_3,
       exp_throat = Q228_4,
       exp_tired = Q228_5,
       exp_joint_ache = Q228_6,
       exp_headache = Q228_7,
       exp_nose = Q228_8,
       exp_taste_smell = Q228_9,
       exp_nausea = Q228_10,
       exp_chills = Q228_11,
       exp_none = Q228_99,
       test = Q229,
       test_symp = Q230_1,
       test_contact = Q230_2,
       test_job = Q230_3,
       test_other = Q230_98,
       pcr_test = Q231_1,
       rat_test = Q231_2,
       dontknow_test = Q231_3,
       pcr_result = Q231a,
       rat_result = Q231b,
       rat_report = Q232,
       date) %>%
     mutate_at(
       vars(starts_with("exp_")),
       ~quoted_to_yesno(.)
     ) %>%
     mutate_at(
       vars(starts_with("test_")),
       ~quoted_to_yesno(.)
     ) %>%
     mutate_at(
       vars(pcr_test, rat_test, dontknow_test),
       ~quoted_to_yesno(.)
     ) %>%
     mutate_at(
       vars(pcr_test, rat_test, dontknow_test),
       ~replace_na(.,"No")
     ) %>%
     # Responses different once flu added in - set test so it refer to yes - influenza as no and joint tests to yes

       mutate(
         test = case_when(
           test == "Yes - COVID-19" ~ "Yes",
           test == "Yes – COVID-19" ~ "Yes", 
           test == "Yes – Both COVID-19 and Influenza" ~ "Yes", 
           test == "Yes – Influenza" ~ "No", 
           test == "7" ~ "No", 
           test == "8" ~ "Yes",
           grepl("No", test) ~ "No",
          test == "Prefer not to say" ~ "No", 
          TRUE ~ test),
  
       age_groups = cut(as.numeric(age), 
                        breaks = c(0, 18, 30, 60, Inf), 
                        labels = c("0-17", "18-29", "30-59", "60+"),
                        include.lowest = TRUE, 
                        right = FALSE)
     )
  
  } else if ((min(survey_doh$date) >= as.Date("2022-02-08")) & (min(survey_doh$date) < as.Date("2023-06-20"))) {
  

    # Select columns of interest
    survey_doh %>%
      select(
        wave,
        state,
        age = S1,
        gender = S2,
        postcode = Q37,
        exp_cough = Q228_1,
        exp_fever = Q228_2,
        exp_breathing = Q228_3,
        exp_throat = Q228_4,
        exp_tired = Q228_5,
        exp_joint_ache = Q228_6,
        exp_headache = Q228_7,
        exp_nose = Q228_8,
        exp_taste_smell = Q228_9,
        exp_nausea = Q228_10,
        exp_chills = Q228_11,
        exp_none = Q228_99,
        test = Q229,
        test_symp = Q230_1,
        test_contact = Q230_2,
        test_job = Q230_3,
        test_other = Q230_98,
        pcr_test = Q231_1,
        rat_test = Q231_2,
        dontknow_test = Q231_3,
        pcr_result = Q231a,
        rat_result = Q231b,
        rat_report = Q232,
        date) %>%
      mutate_at(
        vars(starts_with("exp_")),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(starts_with("test_")),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(pcr_test, rat_test, dontknow_test),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(pcr_test, rat_test, dontknow_test),
        ~replace_na(.,"No")
      ) %>%
      # As the response options for the question about covid test is different in Weeks 84 and 85, instead of "Yes", "No", or "Prefer not to say", it is "Tested, blah", "Not tested" and "Prefer not to say", 
      # we have responses starting with "Tested" or "Yes" changed to "Yes", responses starting with "No" (which captures "Not tested") and "Prefer not to say" set to "No".
      mutate(
        test = case_when(
          grepl("Tested", test) ~ "Yes",
          test == "Yes" ~ "Yes",
          grepl("No", test) ~ "No",
          test == "Prefer not to say" ~ "No",
          TRUE ~ test),
        age_groups = cut(as.numeric(age), 
                         breaks = c(0, 18, 30, 60, Inf), 
                         labels = c("0-17", "18-29", "30-59", "60+"),
                         include.lowest = TRUE, 
                         right = FALSE)
      )
    
  } else if ((min(survey_doh$date) >= as.Date("2022-01-11")) & (min(survey_doh$date) < as.Date("2022-02-08"))) {
    
    # Select columns of interest
    survey_doh %>%
      select(
        wave,
        state,
        age = S1,
        gender = S2,
        postcode = Q37,
        exp_cough = Q228_1,
        exp_fever = Q228_2,
        exp_breathing = Q228_3,
        exp_throat = Q228_4,
        exp_tired = Q228_5,
        exp_joint_ache = Q228_6,
        exp_headache = Q228_7,
        exp_nose = Q228_8,
        exp_taste_smell = Q228_9,
        exp_nausea = Q228_10,
        exp_chills = Q228_11,
        exp_none = Q228_99,
        test = Q229,
        test_symp = Q230_1,
        test_contact = Q230_2,
        test_job = Q230_3,
        test_other = Q230_98,
        pcr_test = Q231_1,
        rat_test = Q231_2,
        dontknow_test = Q231_3,
        pcr_result = Q231a,
        rat_result = Q231b,
        date) %>%
      mutate_at(
        vars(starts_with("exp_")),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(starts_with("test_")),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(pcr_test, rat_test, dontknow_test),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(pcr_test, rat_test, dontknow_test),
        ~replace_na(.,"No")
      ) %>%
      # As the response options for the question about covid test is different in Weeks 84 and 85, instead of "Yes", "No", or "Prefer not to say", it is "Tested, blah", "Not tested" and "Prefer not to say", 
      # we have responses starting with "Tested" or "Yes" changed to "Yes", responses starting with "No" (which captures "Not tested") and "Prefer not to say" set to "No".
      mutate(
        test = case_when(
          grepl("Tested", test) ~ "Yes",
          test == "Yes" ~ "Yes",
          grepl("No", test) ~ "No",
          test == "Prefer not to say" ~ "No",
          TRUE ~ test),
        age_groups = cut(as.numeric(age), 
                         breaks = c(0, 18, 30, 60, Inf), 
                         labels = c("0-17", "18-29", "30-59", "60+"),
                         include.lowest = TRUE, 
                         right = FALSE)
      )
    
  } else {
    
    # Select columns of interest
    survey_doh %>%
      select(
        wave,
        state,
        age = S1,
        gender = S2,
        postcode = Q37,
        exp_cough = Q228_1,
        exp_fever = Q228_2,
        exp_breathing = Q228_3,
        exp_throat = Q228_4,
        exp_tired = Q228_5,
        exp_joint_ache = Q228_6,
        exp_headache = Q228_7,
        exp_nose = Q228_8,
        exp_taste_smell = Q228_9,
        exp_nausea = Q228_10,
        exp_chills = Q228_11,
        exp_none = Q228_99,
        test = Q229,
        tested_result = Q229, # So that we keep this field unmodified, used for later
        test_symp = Q230_1,
        test_contact = Q230_2,
        test_job = Q230_3,
        test_other = Q230_98,
        date) %>%
      mutate_at(
        vars(starts_with("exp_")),
        ~quoted_to_yesno(.)
      ) %>%
      mutate_at(
        vars(starts_with("test_")),
        ~quoted_to_yesno(.)
      ) %>%
      # As the response options for the question about covid test is different in Weeks 84 and 85, instead of "Yes", "No", or "Prefer not to say", it is "Tested, blah", "Not tested" and "Prefer not to say", 
      # we have responses starting with "Tested" or "Yes" changed to "Yes", responses starting with "No" (which captures "Not tested") and "Prefer not to say" set to "No".
      mutate(
        test = case_when(
          grepl("Tested", test) ~ "Yes",
          test == "Yes" ~ "Yes",
          grepl("No", test) ~ "No",
          test == "Prefer not to say" ~ "No",
          TRUE ~ test),
        age_groups = cut(as.numeric(age), 
                         breaks = c(0, 18, 30, 60, Inf), 
                         labels = c("0-17", "18-29", "30-59", "60+"),
                         include.lowest = TRUE, 
                         right = FALSE),
        pcr_test = test, # include this as all tests in this time assumed to be PCR
        rat_test = "No", # include this as all tests in this time assumed to be PCR
        dontknow_test = "No" # include this as all tests in this time assumed to be PCR
      )
  }
  
}

# Function to load all doh surveys in data folder (modified from Nick G github)
parse_all_doh_surveys_for_test_seeking <- function(dir = "data/survey_raw") {
  
  files <- dir %>%
    list.files(
      pattern = ".csv$",
      full.names = TRUE
    ) 
  
  #do a dodgy regex thing to select > 124 wave since the question wasn't asked
  #before
  files <- files[grep("[1][2][4-9][.] |[1][3-9][0-9][.] |[2][0-9][0-9][.] ",files)]
  
  files %>% 
    lapply(parse_doh_survey_for_test_seeking) %>%
    bind_rows() %>%
    remove_doh_duplicates() %>%
    mutate(
      weekend_fraction = weekend_weight(date)
    ) %>%
    group_by(wave) %>%
    mutate(
      wave_date = median(date),
      wave_duration = as.numeric(max(date) - min(date))
    ) %>%
    ungroup()
  
}


#### Load and basic look at data ####

## Number of weeks to aggregate data
aggregate_over <- 4

## Load survey data
survey_data <- parse_all_doh_surveys_for_test_seeking() %>%
  # Filter by state == NA
  filter(!is.na(state))

latest_survey_date <- survey_data$wave_date %>% max()
# Define core symptoms
core_symptoms <- c("exp_cough", "exp_fever", "exp_throat", "exp_taste_smell")

# Add columns for symptom categories to allow for filtering etc
# Add column for type of test
expanded_sym_data <- survey_data %>%
  mutate(
    what_test = case_when(
      pcr_test == "No" & rat_test == "No" & dontknow_test == "Yes" ~ "PCR only",
      pcr_test == "Yes" & rat_test == "No" & dontknow_test == "No" ~ "PCR only",
      pcr_test == "No" & rat_test == "Yes" & dontknow_test == "No" ~ "RAT only",
      pcr_test == "Yes" & rat_test == "Yes" & dontknow_test == "No" ~ "Both",
      TRUE ~ pcr_test)
  ) %>%
  mutate(
    fever_cough = case_when(
      if_all(c(exp_cough, exp_fever), ~.=='Yes') ~ "Yes",
      TRUE ~ "No"),
    one_core = case_when(
      if_any(all_of(core_symptoms), ~.=='Yes') ~ "Yes",
      TRUE ~ "No"),
    two_core = case_when(
      rowSums(across(all_of(core_symptoms), ~.=='Yes')) > 1 ~ "Yes",
      TRUE ~ "No")
  ) %>%
  pivot_longer(
    cols = c(exp_none, fever_cough, one_core, two_core),
    names_to = "symptoms",
    values_to = "response"
  ) %>%
  # rename no symptoms to at least one symptom
  mutate(
    symptoms = recode(
      symptoms,
      `exp_none` = "at least one symptom",
      `fever_cough` = "at least fever and cough",
      `one_core` = "at least one core symptom",
      `two_core` = "at least two core symptoms"
    )
  ) %>%
  # recode no symptoms = no to at least one symptom = yes
  mutate(
    response = case_when(
      symptoms == "at least one symptom" & response == "No" ~ "Yes",
      symptoms == "at least one symptom" & response == "Yes" ~ "No",
      TRUE ~ response)
  ) %>%
  filter(
    !is.na(response)
  ) 


#########################################################################
#### Proportion of respondents reporting symptom categories by state ####

prop_sym_breakdown_week_state <- function(data, sym_def){
  
  data %>%
    group_by(wave_date, state, symptoms, response) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(
      names_from = "response",
      values_from  = "count",
    ) %>%
    mutate(
      Yes = replace_na(Yes, 0),
      No = replace_na(No, 0),
      respondents = Yes + No
    ) %>%
    rename(count = Yes) %>%
    select(-No) %>%
    mutate(proportion = count / respondents) %>%
    filter(symptoms == sym_def)
  
}

prop_sym_breakdown_week_state_one_sym <- prop_sym_breakdown_week_state(expanded_sym_data, "at least one symptom")

prop_sym_breakdown_week_state_one_core <- prop_sym_breakdown_week_state(expanded_sym_data, "at least one core symptom")

# Aggregate over aggregate_over weeks
prop_sym_breakdown_week_state_roll <- function(data) {
  
  data %>% 
    group_by(state) %>%
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
}

prop_sym_breakdown_week_state_one_core_roll <- prop_sym_breakdown_week_state_roll(prop_sym_breakdown_week_state_one_core)


prop_sym_breakdown_week_state_one_sym_roll <- prop_sym_breakdown_week_state_roll(prop_sym_breakdown_week_state_one_sym)


prop_sym_weekly_data_state <- function(data, sym_def) {
  point_df <- data %>%
    group_by(wave_date, state) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date) %>%
    mutate(symptoms = sym_def)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  point_df
  
}

plot_prop_one_sym_state <- prop_sym_weekly_data_state(prop_sym_breakdown_week_state_one_sym_roll, "at least one symptom")

plot_prop_one_core_state <- prop_sym_weekly_data_state(prop_sym_breakdown_week_state_one_core_roll, "at least one core symptom")


# Plotting at least one symptom
p <- ggplot(plot_prop_one_sym_state) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = symptoms),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = symptoms),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = symptoms),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported at least one \nsymptom")
  ) +
  ylab(paste0("Estimate of percentage reporting symptoms"))

p
save_ggplot(paste0("prop_at_least_one_sym_states.png"))


# Plotting at least one core symptom
p <- ggplot(plot_prop_one_core_state) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = symptoms),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = symptoms),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = symptoms),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported at least one \ncore symptom")
  ) +
  ylab(paste0("Estimate of percentage reporting symptoms"))

p
save_ggplot(paste0("prop_at_least_one_core_states.png"))


############################################################################
#### Symptom breakdown for all tests and all of AUS combined ####
sym_breakdown_test_duetosymp_week <- function(data, sym_def){
  
  sym_breakdown_test_week <- data %>%
    # collate responses into respondents and yeses
    group_by(wave_date, symptoms, response, test) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes') %>%
    pivot_wider(
      names_from = "test",
      values_from  = "count",
    ) %>%
    mutate(
      Yes = replace_na(Yes, 0),
      No = replace_na(No, 0),
      respondents = Yes + No
    ) %>%
    rename(count = Yes) %>%
    select(-c(No, response)) %>%
    mutate(proportion = count / respondents) %>%
    filter(symptoms == sym_def)
  
  data %>% mutate(
    why_test_symp = case_when(
      (rowSums(across(c("test_contact", "test_job", "test_other"), ~.!='Yes')) > 1) & (test_symp == "Yes") ~ "Yes",
      TRUE ~ "No")
  ) %>% 
    group_by(wave_date, symptoms, response, test, why_test_symp) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes', test == 'Yes', why_test_symp == "Yes", symptoms == sym_def) %>%
    left_join(sym_breakdown_test_week %>% select(wave_date, symptoms, respondents), by=c("wave_date", "symptoms")) %>%
    select(c(wave_date, symptoms, count, respondents)) %>%
    mutate(proportion = count / respondents)
  
}

sym_breakdown_test_duetosymp_week_one_sym <- sym_breakdown_test_duetosymp_week(expanded_sym_data, "at least one symptom")

sym_breakdown_test_duetosymp_week_one_core <- sym_breakdown_test_duetosymp_week(expanded_sym_data, "at least one core symptom")

# Aggregate over aggregate_over weeks
sym_breakdown_test_duetosymp_week_roll <- function(data) {
  
  data %>% 
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
}

sym_breakdown_test_duetosymp_week_one_core_roll <- sym_breakdown_test_duetosymp_week_roll(sym_breakdown_test_duetosymp_week_one_core)

sym_breakdown_test_duetosymp_week_one_sym_roll <- sym_breakdown_test_duetosymp_week_roll(sym_breakdown_test_duetosymp_week_one_sym)


test_sym_weekly_data <- function(data, sym_def) {
  point_df <- data %>%
    group_by(wave_date) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date) %>%
    mutate(symptoms = sym_def)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  point_df
  
}

plot_one_sym <- test_sym_weekly_data(sym_breakdown_test_duetosymp_week_one_sym_roll, "at least one symptom")

plot_one_core <- test_sym_weekly_data(sym_breakdown_test_duetosymp_week_one_core_roll, "at least one core symptom")


p <- ggplot(plot_one_core) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = symptoms),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = symptoms),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = symptoms),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.5,0.9),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        # axis.text.x = element_text(size = 7)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a test due to symptoms, \n given at least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p

p <- ggplot(plot_one_sym) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = symptoms),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = symptoms),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = symptoms),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.5,0.9),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        # axis.text.x = element_text(size = 7)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a test due to symptoms, \n given at least one symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p


############################################################################
#### Breakdown further by test type and all of AUS combined ####
sym_breakdown_test_type_duetosymp_week <- function(data, sym_def) {
  
  sym_breakdown_test_week <- data %>%
    # collate responses into respondents and yeses
    group_by(wave_date, symptoms, response, what_test) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes') %>%
    pivot_wider(
      names_from = "what_test",
      values_from  = "count",
    ) %>%
    mutate(
      `PCR only` = replace_na(`PCR only`, 0),
      `RAT only` = replace_na(`RAT only`, 0),
      Both = replace_na(Both, 0),
      No = replace_na(No, 0),
      respondents = `PCR only` + `RAT only` + Both + No
    ) %>%
    mutate(p_pcr = `PCR only` / respondents,
           p_rat = `RAT only` / respondents,
           p_both = Both / respondents) %>%
    filter(symptoms == sym_def)
  
  data %>%
    mutate(
      why_test_symp = case_when(
        (rowSums(across(c("test_contact", "test_job", "test_other"), ~.!='Yes')) > 1) & (test_symp == "Yes") ~ "Yes",
        TRUE ~ "No")
    ) %>% 
    group_by(wave_date, symptoms, response, what_test, why_test_symp) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes', why_test_symp == 'Yes', symptoms == sym_def) %>%
    left_join(sym_breakdown_test_week %>% select(wave_date, symptoms, respondents), by=c("wave_date", "symptoms")) %>%
    select(c(wave_date, symptoms, what_test, count, respondents)) %>%
    mutate(proportion = count / respondents)
  
}

sym_breakdown_test_type_duetosymp_week_one_sym <- sym_breakdown_test_type_duetosymp_week(expanded_sym_data, "at least one symptom")

sym_breakdown_test_type_duetosymp_week_one_core <- sym_breakdown_test_type_duetosymp_week(expanded_sym_data, "at least one core symptom")


# Aggregate over aggregate_over weeks
sym_breakdown_test_type_duetosymp_week_roll <- function(data) {
  
  data %>% 
    group_by(what_test) %>%
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
}

sym_breakdown_test_type_duetosymp_week_one_core_roll <- sym_breakdown_test_type_duetosymp_week_roll(sym_breakdown_test_type_duetosymp_week_one_core)

sym_breakdown_test_type_duetosymp_week_one_sym_roll <- sym_breakdown_test_type_duetosymp_week_roll(sym_breakdown_test_type_duetosymp_week_one_sym)


test_sym_weekly_data_test_type <- function(data, sym_def) {
  point_df <- data %>%
    group_by(wave_date, what_test) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date) %>%
    mutate(symptoms = sym_def)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  point_df
  
}

plot_one_sym_type <- test_sym_weekly_data_test_type(sym_breakdown_test_type_duetosymp_week_one_sym_roll, "at least one symptom")

plot_one_core_type <- test_sym_weekly_data_test_type(sym_breakdown_test_type_duetosymp_week_one_core_roll, "at least one core symptom")


# Plotting for at least one core symptom
p <- ggplot(plot_one_core_type) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = what_test),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = what_test),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.7,0.9),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        # axis.text.x = element_text(size = 7)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a test due to symptoms \n split by type of test, given they reported at least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p

# Plotting for at least one symptom
p <- ggplot(plot_one_sym_type) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = what_test),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = what_test),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.7,0.9),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        # axis.text.x = element_text(size = 7)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a test due to symptoms \n split by type of test, given they reported at least one symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p


############################################################################
#### Breakdown further by state ####
# Due to symptoms only
sym_breakdown_test_duetosymp_week_state <- function(data, sym_def) {
  
  sym_breakdown_test_week <- data %>% 
    # collate responses into respondents and yeses
    group_by(wave_date, symptoms, state, response, test) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes') %>%
    pivot_wider(
      names_from = "test",
      values_from  = "count",
    ) %>%
    mutate(
      Yes = replace_na(Yes, 0),
      No = replace_na(No, 0),
      respondents = Yes + No
    ) %>%
    rename(count = Yes) %>%
    select(-c(No, response)) %>%
    mutate(proportion = count / respondents) %>%
    filter(symptoms == sym_def)
  
  data %>%
    mutate(
      why_test_symp = case_when(
        (rowSums(across(c("test_contact", "test_job", "test_other"), ~.!='Yes')) > 1) & (test_symp == "Yes") ~ "Yes",
        TRUE ~ "No")
    ) %>% 
    group_by(wave_date, symptoms, state, response, test, why_test_symp) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes', test == 'Yes', why_test_symp == 'Yes', symptoms == sym_def) %>%
    left_join(sym_breakdown_test_week %>% select(wave_date, symptoms, state, respondents), by=c("wave_date", "symptoms", "state")) %>%
    select(c(wave_date, symptoms, state, count, respondents)) %>%
    mutate(proportion = count / respondents)
  
}

sym_breakdown_test_duetosymp_week_state_one_core <- sym_breakdown_test_duetosymp_week_state(expanded_sym_data, "at least one core symptom")

sym_breakdown_test_duetosymp_week_state_one_sym <- sym_breakdown_test_duetosymp_week_state(expanded_sym_data, "at least one symptom")


# Aggregate over aggregate_over weeks
sym_breakdown_test_duetosymp_week_state_roll <- function(data) {
  
  data %>% 
    group_by(state) %>%
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
}

sym_breakdown_test_duetosymp_week_state_one_core_roll <- sym_breakdown_test_duetosymp_week_state_roll(sym_breakdown_test_duetosymp_week_state_one_core)

sym_breakdown_test_duetosymp_week_state_one_sym_roll <- sym_breakdown_test_duetosymp_week_state_roll(sym_breakdown_test_duetosymp_week_state_one_sym)


test_sym_weekly_data_state <- function(data, sym_def) {
  point_df <- data %>%
    group_by(wave_date, state) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date) %>%
    mutate(symptoms = sym_def)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  if (sym_def == "at least one symptom") {
    point_df %>% select(date, state, symptoms) %>% bind_cols(p_sims %>% as_tibble()) %>% write_csv(paste0("outputs/at_least_one_sym_states_simulated_values_", latest_survey_date, ".csv"))
  }
  
  point_df
  
}

plot_one_sym_state <- test_sym_weekly_data_state(sym_breakdown_test_duetosymp_week_state_one_sym_roll, "at least one symptom")

plot_one_core_state <- test_sym_weekly_data_state(sym_breakdown_test_duetosymp_week_state_one_core_roll, "at least one core symptom")


# Plotting at least one core symptom only
p_at_least_one_core_states <- ggplot(plot_one_core_state) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = symptoms),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = symptoms),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = symptoms),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a test due \nto symptoms, given at least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p_at_least_one_core_states
save_ggplot(paste0("at_least_one_core_states.png"))


# Plotting at least one symptom
p_at_least_one_sym_states <- ggplot(plot_one_sym_state) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = symptoms),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = symptoms),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = symptoms),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a test due \nto symptoms, given at least one symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p_at_least_one_sym_states
save_ggplot(paste0("at_least_one_sym_states.png"))


############################################################################
#### Combine plots of proportion of respondents reporting symptoms and plot of respondents who reported seeking a test due to symptoms ####

one_sym_comb <- bind_rows(plot_prop_one_sym_state %>% 
                            mutate(data="proportion"), 
                          plot_one_sym_state %>%
                            mutate(data="test"))


# Plotting at least one symptom
p <- ggplot(one_sym_comb) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = data),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = data),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = data),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.525,0.975),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("At least one symptom")
  ) +
  ylab(paste0("Estimate of percentage"))

p
#save_ggplot(paste0("combined_at_least_one_sym_states.png"))


one_core_comb <- bind_rows(plot_prop_one_core_state %>% 
                             mutate(data="proportion"), 
                           plot_one_core_state %>%
                             mutate(data="test"))


# Plotting at least one core symptom
p <- ggplot(one_core_comb) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = data),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = data),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = data),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.525,0.975),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("At least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage"))

p
#save_ggplot(paste0("combined_at_least_one_core_states.png"))



###########################################################################
#### Overlay cases on combined plots of proportion of respondents reporting symptoms and plot of respondents who reported seeking a test due to symptoms ####

# Read in data
cases <- read_csv(paste0("outputs/",get_local_cases_input()))

coeff <- cases %>%
  group_by(state) %>%
  filter(date_onset >= min(expanded_sym_data$date),
         date_onset <= max(expanded_sym_data$date)) %>%
  summarise(max(count))

# At least one core symptom
cases %>%
  filter(date_onset >= min(expanded_sym_data$date),
         date_onset <= max(expanded_sym_data$date)) %>%
  left_join(coeff, by="state") %>%
  mutate(count_mod = count/`max(count)`*100) %>%
  ggplot() +
  geom_col(aes(x=date_onset, y=count_mod)) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(data = one_core_comb,
             aes(date, proportion*100, colour = data),
             size = 2,
             shape = 4
  ) +
  
  # add empirical percentages
  geom_point(data = one_core_comb,
             aes(date, percentage, colour = data),
             size = 6,
             pch = "_"
  ) +
  
  geom_errorbar(data = one_core_comb,
                aes(date, percentage, ymin = lower, ymax = upper, colour = data),
                size = 5,
                alpha = 0.2,
                width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.525,0.975),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 8)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("At least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage"))

#save_ggplot("combined_one_core_overlay_cases.png")


# At least one symptom
cases %>%
  filter(date_onset >= min(expanded_sym_data$date),
         date_onset <= max(expanded_sym_data$date)) %>%
  left_join(coeff, by="state") %>%
  mutate(count_mod = count/`max(count)`*100) %>%
  ggplot() +
  geom_col(aes(x=date_onset, y=count_mod)) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(data = one_sym_comb,
             aes(date, proportion*100, colour = data),
             size = 2,
             shape = 4
  ) +
  
  # add empirical percentages
  geom_point(data = one_sym_comb,
             aes(date, percentage, colour = data),
             size = 6,
             pch = "_"
  ) +
  
  geom_errorbar(data = one_sym_comb,
                aes(date, percentage, ymin = lower, ymax = upper, colour = data),
                size = 5,
                alpha = 0.2,
                width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = c(0.525,0.975),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("At least one symptom")
  ) +
  ylab(paste0("Estimate of percentage"))

#save_ggplot("combined_one_sym_overlay_cases.png")




############################################################################
#### Breakdown further by test type and states ####
# Due to symptoms only
sym_breakdown_test_type_duetosymp_week_state <- function(data, sym_def) {
  
  sym_breakdown_test_week <- data %>%
    # collate responses into respondents and yeses
    group_by(wave_date, symptoms, state, response, what_test) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes') %>%
    pivot_wider(
      names_from = "what_test",
      values_from  = "count",
    ) %>%
    mutate(
      `PCR only` = replace_na(`PCR only`, 0),
      `RAT only` = replace_na(`RAT only`, 0),
      Both = replace_na(Both, 0),
      No = replace_na(No, 0),
      respondents = `PCR only` + `RAT only` + Both + No
    ) %>%
    mutate(p_pcr = `PCR only` / respondents,
           p_rat = `RAT only` / respondents,
           p_both = Both / respondents) %>%
    filter(symptoms == sym_def)
  
  data %>%
    mutate(
      why_test_symp = case_when(
        (rowSums(across(c("test_contact", "test_job", "test_other"), ~.!='Yes')) > 1) & (test_symp == "Yes") ~ "Yes",
        TRUE ~ "No")
    ) %>% 
    group_by(wave_date, symptoms, state, response, what_test, why_test_symp) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    filter(response == 'Yes', why_test_symp == 'Yes', symptoms == sym_def) %>%
    left_join(sym_breakdown_test_week %>% select(wave_date, symptoms, state, respondents), by=c("wave_date", "symptoms", "state")) %>%
    select(c(wave_date, symptoms, state, what_test, count, respondents)) %>%
    mutate(proportion = count / respondents)
  
}


sym_breakdown_test_type_duetosymp_week_state_one_core <- sym_breakdown_test_type_duetosymp_week_state(expanded_sym_data, "at least one core symptom")

sym_breakdown_test_type_duetosymp_week_state_one_sym <- sym_breakdown_test_type_duetosymp_week_state(expanded_sym_data, "at least one symptom")


# Aggregate over aggregate_over weeks
sym_breakdown_test_type_duetosymp_week_state_roll <- function(data) {
  
  data %>% 
    group_by(state, what_test) %>%
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
  
}

sym_breakdown_test_type_duetosymp_week_state_one_core_roll <- sym_breakdown_test_type_duetosymp_week_state_roll(sym_breakdown_test_type_duetosymp_week_state_one_core)

sym_breakdown_test_type_duetosymp_week_state_one_sym_roll <- sym_breakdown_test_type_duetosymp_week_state_roll(sym_breakdown_test_type_duetosymp_week_state_one_sym)


test_sym_weekly_data_state_type <- function(data, sym_def) {
  point_df <- data %>%
    group_by(wave_date, state, what_test) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date) %>%
    mutate(symptoms = sym_def)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  point_df
  
}


plot_one_sym_state_type <- test_sym_weekly_data_state_type(sym_breakdown_test_type_duetosymp_week_state_one_sym_roll, "at least one symptom")

plot_one_core_state_type <- test_sym_weekly_data_state_type(sym_breakdown_test_type_duetosymp_week_state_one_core_roll, "at least one core symptom")


# at least one core symptom - PCR only
p <- plot_one_core_state_type %>%
  filter(what_test == "PCR only") %>%
  ggplot() +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = what_test),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = what_test),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  #coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a PCR test \n due to symptoms, given at least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p
save_ggplot(paste0("at_least_one_core_states_testtype_PCR_only.png"))


# at least one symptom - PCR only
p <- plot_one_sym_state_type %>%
  filter(what_test == "PCR only") %>%
  ggplot() +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = what_test),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = what_test),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  #coord_cartesian(ylim = c(0, 30)) +
  scale_y_continuous(position = "right")+ #, breaks = seq(0,30,5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a PCR test \n due to symptoms, given at least one symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p
save_ggplot(paste0("at_least_one_sym_states_testtype_PCR_only.png"))


# # at least one core symptom - PCR only (zoomed)
# p <- plot_one_core_state_type %>%
#   filter(what_test == "PCR only") %>%
#   ggplot() +
#   # add calculated proportions converted to percentages
#   # add empirical percentages
#   geom_point(
#     aes(date, proportion*100, colour = what_test),
#     size = 2,
#     shape = 4
#   ) +
#   
#   # add empirical percentages
#   geom_point(
#     aes(date, percentage, colour = what_test),
#     size = 6,
#     pch = "_"
#   ) +
#   
#   geom_errorbar(
#     aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
#     size = 5,
#     alpha = 0.2,
#     width = 0
#   ) +
#   
#   xlab(element_blank()) +
#   
#   coord_cartesian(ylim = c(0, 30)) +
#   scale_y_continuous(position = "right") +
#   scale_x_date(date_breaks = "2 months", date_labels = "%d/%m") +
#   scale_alpha(range = c(0, 0.5)) +
#   
#   facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(hjust = 0, face = "bold"),
#         axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#         panel.spacing = unit(1.2, "lines"),
#         axis.text.x = element_text(size = 9)
#   ) +
#   
#   # and titles
#   ggtitle(
#     label = paste0("Percentage of respondents who reported seeking a PCR test \n due to symptoms, given at least one core symptom")
#   ) +
#   ylab(paste0("Estimate of percentage tested \n due to symptoms"))
# 
# p
# save_ggplot(paste0("at_least_one_core_states_testtype_PCR_only_zoomed.png"))
# 
# 
# # at least one symptom - PCR only (zoomed)
# p <- plot_one_sym_state_type %>%
#   filter(what_test == "PCR only") %>%
#   ggplot() +
#   # add calculated proportions converted to percentages
#   # add empirical percentages
#   geom_point(
#     aes(date, proportion*100, colour = what_test),
#     size = 2,
#     shape = 4
#   ) +
#   
#   # add empirical percentages
#   geom_point(
#     aes(date, percentage, colour = what_test),
#     size = 6,
#     pch = "_"
#   ) +
#   
#   geom_errorbar(
#     aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
#     size = 5,
#     alpha = 0.2,
#     width = 0
#   ) +
#   
#   xlab(element_blank()) +
#   
#   coord_cartesian(ylim = c(0, 20)) +
#   scale_y_continuous(position = "right")+ #, breaks = seq(0,30,5)) +
#   scale_x_date(date_breaks = "2 months", date_labels = "%d/%m") +
#   scale_alpha(range = c(0, 0.5)) +
#   
#   facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(hjust = 0, face = "bold"),
#         axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#         panel.spacing = unit(1.2, "lines"),
#         axis.text.x = element_text(size = 9)
#   ) +
#   
#   # and titles
#   ggtitle(
#     label = paste0("Percentage of respondents who reported seeking a PCR test \n due to symptoms, given at least one symptom")
#   ) +
#   ylab(paste0("Estimate of percentage tested \n due to symptoms"))
# 
# p
# save_ggplot(paste0("at_least_one_sym_states_testtype_PCR_only_zoomed.png"))


# at least one core symptom - RAT only
p <- plot_one_core_state_type %>%
  filter(what_test == "RAT only") %>%
  ggplot() +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = what_test),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = what_test),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a RAT test \n due to symptoms, given at least one core symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p
save_ggplot(paste0("at_least_one_core_states_testtype_RAT_only.png"))


# at least one symptom - RAT only
p <- plot_one_sym_state_type %>%
  filter(what_test == "RAT only") %>%
  ggplot() +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100, colour = what_test),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = what_test),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = what_test),
    size = 5,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(position = "right")+ #, breaks = seq(0,30,5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported seeking a RAT test \n due to symptoms, given at least one symptom")
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))

p
save_ggplot(paste0("at_least_one_sym_states_testtype_RAT_only.png"))


############################################################################
#### Smoothing ####
# Produce the central estimate as a smoothed time-series and output as a csv
# From: https://stackoverflow.com/questions/50163106/loess-regression-on-each-group-with-dplyrgroup-by

# # At least one core symptom
# models <- plot_one_core_state %>%
#   nest(data=-state) %>%
#   mutate(
#     # Perform loess calculation on each state with span = 0.3
#     m_point3 = map(data, loess,
#                    formula = percentage ~ as.numeric(date), span = .3),
#     # Retrieve the fitted values from each model
#     fitted_point3 = map(m_point3, `[[`, "fitted"),
#     # Perform loess calculation on each state with span = 0.4
#     m_point4 = map(data, loess,
#                    formula = percentage ~ as.numeric(date), span = .4),
#     # Retrieve the fitted values from each model
#     fitted_point4 = map(m_point4, `[[`, "fitted"),
#     # Perform loess calculation on each state with span = 0.5
#     m_point5 = map(data, loess,
#                    formula = percentage ~ as.numeric(date), span = .5),
#     # Retrieve the fitted values from each model
#     fitted_point5 = map(m_point5, `[[`, "fitted"),
#   )
# 
# # Apply fitted y's as a new column
# results <- models %>%
#   select(-c(m_point3, m_point4, m_point5)) %>%
#   unnest(cols = c(data, fitted_point3, fitted_point4, fitted_point5))
# 
# results %>% 
#   pivot_longer(cols=c(fitted_point3, fitted_point4, fitted_point5), 
#                names_to = "span",
#                values_to = "fitted") %>%
#   ggplot(aes(x = date, y = percentage)) +
#   geom_point() +
#   geom_line(aes(y = fitted, colour = span)) +
#   facet_wrap(~state, ncol=2, scales="free") +
#   xlab(element_blank()) +
#   coord_cartesian(ylim = c(0, 100)) +
#   scale_y_continuous(position = "right") +
#   scale_x_date(date_breaks = "2 months", date_labels = "%d/%m") +
#   facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(hjust = 0, face = "bold"),
#         axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#         panel.spacing = unit(1.2, "lines"),
#         axis.text.x = element_text(size = 10)
#   ) +
#   ylab(paste0("Estimate of percentage tested \n due to symptoms"))
# save_ggplot(paste0("at_least_one_core_states_central_smoothed.png"))
# 
# results %>%
#   select(state, date, percentage, fitted_point3, fitted_point4, fitted_point5) %>%
#   write_csv("output/at_least_one_core_states_central_smoothed.csv")



# At least one symptom split by state
models <- plot_one_sym_state %>%
  nest(data=-state) %>%
  mutate(
    # Perform loess calculation on each state with span = 0.3
    m_point3 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .3),
    # Retrieve the fitted values from each model
    fitted_point3 = map(m_point3, `[[`, "fitted"),
    # Perform loess calculation on each state with span = 0.4
    m_point4 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .4),
    # Retrieve the fitted values from each model
    fitted_point4 = map(m_point4, `[[`, "fitted"),
    # Perform loess calculation on each state with span = 0.5
    m_point5 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .5),
    # Retrieve the fitted values from each model
    fitted_point5 = map(m_point5, `[[`, "fitted"),
  )

# Apply fitted y's as a new column
results <- models %>%
  select(-c(m_point3, m_point4, m_point5)) %>%
  unnest(cols = c(data, fitted_point3, fitted_point4, fitted_point5))

results %>% 
  pivot_longer(cols=c(fitted_point3, fitted_point4, fitted_point5), 
               names_to = "span",
               values_to = "fitted") %>%
  ggplot(aes(x = date, y = percentage)) +
  geom_point() +
  geom_line(aes(y = fitted, colour = span)) +
  facet_wrap(~state, ncol=2, scales="free") +
  xlab(element_blank()) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))
save_ggplot(paste0("at_least_one_sym_states_central_smoothed.png"))

# For Rob Moss
results %>%
  select(state, date, percentage, fitted_point3, fitted_point4, fitted_point5) %>%
  write_csv(paste0("outputs/at_least_one_sym_states_central_smoothed_",latest_survey_date, ".csv"))


# For Dave Duncan
plot_one_sym_state %>% write_csv(paste0("outputs/at_least_one_sym_states_raw_", latest_survey_date, ".csv"))


# For August Hao

# At least one symptom national
results <- plot_one_sym %>%
  mutate(
    # Perform loess calculation with span = 0.3 and obtain fitted values from each model
    fitted_point3 = predict(loess(percentage ~ as.numeric(date), span = .3)),
    # Perform loess calculation with span = 0.4 and obtain fitted values from each model
    fitted_point4 = predict(loess(percentage ~ as.numeric(date), span = .4)),
    # Perform loess calculation with span = 0.5 and obtain fitted values from each model
    fitted_point5 = predict(loess(percentage ~ as.numeric(date), span = .5))
  )

results %>% 
  pivot_longer(cols=c(fitted_point3, fitted_point4, fitted_point5), 
               names_to = "span",
               values_to = "fitted") %>%
  ggplot(aes(x = date, y = percentage)) +
  geom_point() +
  geom_line(aes(y = fitted, colour = span)) +
  xlab(element_blank()) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))
save_ggplot(paste0("at_least_one_sym_states_central_smoothed_national.png"))

results %>%
  select(date, percentage, fitted_point3, fitted_point4, fitted_point5) %>%
  write_csv(paste0("outputs/at_least_one_sym_states_central_smoothed_national_",latest_survey_date, ".csv"))


# At least one symptom: RAT only national
results <- plot_one_sym_type %>%
  filter(what_test == "RAT only") %>%
  mutate(
    # Perform loess calculation with span = 0.3 and obtain fitted values from each model
    fitted_point3 = predict(loess(percentage ~ as.numeric(date), span = .3)),
    # Perform loess calculation with span = 0.4 and obtain fitted values from each model
    fitted_point4 = predict(loess(percentage ~ as.numeric(date), span = .4)),
    # Perform loess calculation with span = 0.5 and obtain fitted values from each model
    fitted_point5 = predict(loess(percentage ~ as.numeric(date), span = .5))
  )

results %>% 
  pivot_longer(cols=c(fitted_point3, fitted_point4, fitted_point5), 
               names_to = "span",
               values_to = "fitted") %>%
  ggplot(aes(x = date, y = percentage)) +
  geom_point() +
  geom_line(aes(y = fitted, colour = span)) +
  xlab(element_blank()) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))
save_ggplot(paste0("at_least_one_sym_states_central_smoothed_national_RAT_only.png"))

results %>%
  select(date, percentage, fitted_point3, fitted_point4, fitted_point5) %>%
  write_csv(paste0("outputs/at_least_one_sym_states_central_smoothed_national_RAT_only_",latest_survey_date, ".csv"))


# At least one symptom: PCR only split by state
models <- plot_one_sym_state_type %>%
  filter(what_test == "PCR only") %>%
  nest(data=-state) %>%
  mutate(
    # Perform loess calculation on each state with span = 0.3
    m_point3 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .3),
    # Retrieve the fitted values from each model
    fitted_point3 = map(m_point3, `[[`, "fitted"),
    # Perform loess calculation on each state with span = 0.4
    m_point4 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .4),
    # Retrieve the fitted values from each model
    fitted_point4 = map(m_point4, `[[`, "fitted"),
    # Perform loess calculation on each state with span = 0.5
    m_point5 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .5),
    # Retrieve the fitted values from each model
    fitted_point5 = map(m_point5, `[[`, "fitted"),
  )

# Apply fitted y's as a new column
results <- models %>%
  select(-c(m_point3, m_point4, m_point5)) %>%
  unnest(cols = c(data, fitted_point3, fitted_point4, fitted_point5))

results %>% 
  pivot_longer(cols=c(fitted_point3, fitted_point4, fitted_point5), 
               names_to = "span",
               values_to = "fitted") %>%
  ggplot(aes(x = date, y = percentage)) +
  geom_point() +
  geom_line(aes(y = fitted, colour = span)) +
  facet_wrap(~state, ncol=2, scales="free") +
  xlab(element_blank()) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))
save_ggplot(paste0("at_least_one_sym_states_central_smoothed_PCR_only.png"))

results %>%
  select(state, date, percentage, fitted_point3, fitted_point4, fitted_point5) %>%
  write_csv(paste0("outputs/at_least_one_sym_states_central_smoothed_PCR_only_",latest_survey_date, ".csv"))

# At least one symptom: RAT only split by state
models <- plot_one_sym_state_type %>%
  filter(what_test == "RAT only") %>%
  nest(data=-state) %>%
  mutate(
    # Perform loess calculation on each state with span = 0.3
    m_point3 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .3),
    # Retrieve the fitted values from each model
    fitted_point3 = map(m_point3, `[[`, "fitted"),
    # Perform loess calculation on each state with span = 0.4
    m_point4 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .4),
    # Retrieve the fitted values from each model
    fitted_point4 = map(m_point4, `[[`, "fitted"),
    # Perform loess calculation on each state with span = 0.5
    m_point5 = map(data, loess,
                   formula = percentage ~ as.numeric(date), span = .5),
    # Retrieve the fitted values from each model
    fitted_point5 = map(m_point5, `[[`, "fitted"),
  )

# Apply fitted y's as a new column
results <- models %>%
  select(-c(m_point3, m_point4, m_point5)) %>%
  unnest(cols = c(data, fitted_point3, fitted_point4, fitted_point5))

results %>% 
  pivot_longer(cols=c(fitted_point3, fitted_point4, fitted_point5), 
               names_to = "span",
               values_to = "fitted") %>%
  ggplot(aes(x = date, y = percentage)) +
  geom_point() +
  geom_line(aes(y = fitted, colour = span)) +
  facet_wrap(~state, ncol=2, scales="free") +
  xlab(element_blank()) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  facet_rep_wrap(~state, ncol = 2, scales = "free_y",repeat.tick.labels = TRUE) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 9)
  ) +
  ylab(paste0("Estimate of percentage tested \n due to symptoms"))
save_ggplot(paste0("at_least_one_sym_states_central_smoothed_RAT_only.png"))

results %>%
  select(state, date, percentage, fitted_point3, fitted_point4, fitted_point5) %>%
  write_csv(paste0("outputs/at_least_one_sym_states_central_smoothed_RAT_only_",latest_survey_date, ".csv"))



############################################################################
#### P(reported positive RAT test|tested positive on RAT) = n(reported positive RAT test AND tested positive on RAT) / n(tested positive on RAT) ####
# Due to symptoms only

report_positive_rat_week <- survey_data %>% #filter out states that no longer have rat reporting pipelines after it gets switched off
  filter(
    !(state == "VIC" & date > "2023-07-14")) %>%
  filter (
   !(state == "QLD" & date > "2023-09-14")) %>%
  filter (
    !(state == "NSW" & date > "2023-10-14")) %>%
  filter (
    !(state == "WA" & date > "2023-10-22")) %>%
  filter (
    !(state == "NT" & date > "2023-11-04")) %>%
  mutate(rat_result = case_when( 
    grepl("positive", rat_result) ~ "RAT Positive", 
    grepl("negative", rat_result) ~ "RAT Negative", 
    grepl("waiting", rat_result) ~ "RAT Waiting",
    grepl("inconclusive", rat_result) ~ "RAT Inconclusive", 
    TRUE ~ rat_result)) %>%
  filter(date >= as.Date("2022-02-08"), 
         rat_result == "RAT Positive") %>%
  group_by(wave_date, rat_report) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = "rat_report",
    values_from  = "count",
  ) %>%
  mutate(
    Yes = replace_na(Yes, 0),
    No = replace_na(No, 0),
    respondents = Yes + No
  ) %>%
  rename(count = Yes) %>%
  select(-No) %>%
  mutate(proportion = count / respondents)

# Aggregate over aggregate_over weeks
report_positive_rat_week_rolling <- function(data) {
  
  data %>% 
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
}

report_positive_rat_week_roll <- report_positive_rat_week_rolling(report_positive_rat_week)

report_positive_rat_weekly_data <- function(data) {
  point_df <- data %>%
    group_by(wave_date) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  point_df
  # save_ggplot(paste0("test_given_", symptoms, "_weekly.png"))
  
}

plot_report_positive_rat <- report_positive_rat_weekly_data(report_positive_rat_week_roll)

p <- ggplot(plot_report_positive_rat) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper),
    size = 3,
    alpha = 0.2,
    width = 0
  ) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  scale_alpha(range = c(0, 0.5)) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        # axis.text.x = element_text(size = 7)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported positive RAT result to health, \n given positive RAT test")
  ) +
  ylab(paste0("Estimate of percentage reported result, \n given positive RAT"))

p
filepath <- file.path("outputs/figures/", "report_positive_rat.png")
ggsave(filepath,
       width = 10,
       height = 4,
       bg = 'white')


# By state
report_positive_rat_week_state <- survey_data %>%
  mutate(rat_result = case_when( 
    grepl("positive", rat_result) ~ "RAT Positive", 
    grepl("negative", rat_result) ~ "RAT Negative", 
    grepl("waiting", rat_result) ~ "RAT Waiting",
    grepl("inconclusive", rat_result) ~ "RAT Inconclusive", 
    TRUE ~ rat_result)) %>%
  filter(date >= as.Date("2022-02-08"), 
         rat_result == "RAT Positive") %>%
  group_by(wave_date, state, rat_report) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = "rat_report",
    values_from  = "count",
  ) %>%
  mutate(
    Yes = replace_na(Yes, 0),
    No = replace_na(No, 0),
    respondents = Yes + No
  ) %>%
  rename(count = Yes) %>%
  select(-No) %>%
  mutate(proportion = count / respondents)

#check if all states are present for last week of data and if not hack in 0 responses so we get an estimate for that week 
sort(unique(report_positive_rat_week$wave_date))
report_positive_rat_week_state$state[report_positive_rat_week_state$wave_date == latest_survey_date]

#report_positive_rat_week_state <- rbind(report_positive_rat_week_state, list('2023-09-14', 'VIC', 0, 0, 0.0000000))
#report_positive_rat_week_state <- rbind(report_positive_rat_week_state, list('2023-09-21', 'NT', 0, 0, 0.0000000))
#report_positive_rat_week_state <- rbind(report_positive_rat_week_state, list('2023-09-21', 'TAS', 0, 0, 0.0000000))
#report_positive_rat_week_state <- rbind(report_positive_rat_week_state, list('2023-09-21', 'WA', 0, 0, 0.0000000))

# Aggregate over aggregate_over weeks
report_positive_rat_week_state_rolling <- function(data) {
  
  data %>% 
    group_by(state) %>%
    mutate(count = rollapplyr(data=count, width=aggregate_over, FUN=sum, align = "right", fill = NA), 
           respondents = rollapplyr(data=respondents, width=aggregate_over, FUN=sum, align = "right", fill = NA),
           proportion = count/respondents) %>%
    filter(!is.na(count))
}

report_positive_rat_week_state_roll <- report_positive_rat_week_state_rolling(report_positive_rat_week_state)

report_positive_rat_weekly_data_state <- function(data) {
  point_df <- data %>%
    group_by(wave_date, state) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100, 
      fill = 0
    ) %>%
    rename(date = wave_date)
  
  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)
  
  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )
  
  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))
  
  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )
  
  point_df
  # save_ggplot(paste0("test_given_", symptoms, "_weekly.png"))
  
}

plot_report_positive_rat_state_roll <- report_positive_rat_weekly_data_state(report_positive_rat_week_state_roll) %>% filter(state %in% c("QLD", "NSW", "VIC", "WA"))

#hack in fix for Victoria
plot_report_positive_rat_state_roll$percentage[plot_report_positive_rat_state_roll$state == "VIC" & plot_report_positive_rat_state_roll$date >= "2023-09-28" & plot_report_positive_rat_state_roll$date <=  "2023-10-05"] <- 0.000000
plot_report_positive_rat_state_roll$upper[plot_report_positive_rat_state_roll$state == "VIC" & plot_report_positive_rat_state_roll$date >= "2023-09-28" & plot_report_positive_rat_state_roll$date <=  "2023-10-05"] <- 50.000000

plot_report_positive_rat_state_roll$percentage[plot_report_positive_rat_state_roll$state == "VIC" & plot_report_positive_rat_state_roll$date >= "2023-11-09" & plot_report_positive_rat_state_roll$date <= latest_survey_date] <- 0.000000
plot_report_positive_rat_state_roll$upper[plot_report_positive_rat_state_roll$state == "VIC" & plot_report_positive_rat_state_roll$date >= "2023-11-09" & plot_report_positive_rat_state_roll$date <= latest_survey_date] <- 50.000000

p <- ggplot(plot_report_positive_rat_state_roll) +
  # add calculated proportions converted to percentages
  # add empirical percentages
  geom_point(
    aes(date, proportion*100),
    size = 2,
    shape = 4
  ) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage),
    size = 6,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper),
    size = 3,
    alpha = 0.2,
    width = 0
  ) +
  
  #geom_vline(xintercept = as.Date("2023-07-01"), linetype="dashed")+
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b%y") +
  scale_alpha(range = c(0, 0.5)) +
  facet_wrap(~state, ncol = 1, scales = "free") +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        # axis.text.x = element_text(size = 7)
  ) +
  
  # and titles
  ggtitle(
    label = paste0("Percentage of respondents who reported positive RAT result to health, \n given positive RAT test")
  ) +
  ylab(paste0("Estimate of percentage reported result, \n given positive RAT"))

p
filepath <- file.path("outputs/figures/", "report_positive_rat_vic_nsw_qld.png")
ggsave(filepath,
       width = 10,
       height = 8,
       bg = 'white')


# For August Hao

# Raw national
report_positive_rat_weekly_data(report_positive_rat_week) %>%
  write_csv(paste0("outputs/report_positive_rat_raw_",latest_survey_date, ".csv"))

# Aggregated national
report_positive_rat_weekly_data(report_positive_rat_week_roll) %>%
  write_csv(paste0("outputs/report_positive_rat_aggregate4weeks_",latest_survey_date, ".csv"))

# Raw by state
report_positive_rat_weekly_data_state(report_positive_rat_week_state) %>%
  write_csv(paste0("outputs/report_positive_rat_state_raw_",latest_survey_date, ".csv"))

# Aggregated by state
report_positive_rat_weekly_data_state(report_positive_rat_week_state_roll) %>%
  write_csv(paste0("outputs/report_positive_rat_state_aggregate4weeks_",latest_survey_date, ".csv"))
