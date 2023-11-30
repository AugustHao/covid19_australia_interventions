# analyse change in macrodistancing behaviour (non-household contact rate) by
# state, using a baseline rate, and survey questions from Freya's survey and the
# BETA barometer

## running Reff model without updted survey data anywhere between ##> and ##< 2 sections


##>1

source("R/lib.R")

source("R/functions.R")

# informative priors for baseline contact parameters
baseline_contact_params <- baseline_contact_parameters(gi_cdf)

# data for plotting
baseline_point <- tibble::tibble(
  date = as.Date("2020-03-01"),
  estimate = baseline_contact_params$mean_contacts[2],
  sd = baseline_contact_params$se_contacts[2],
  type = "Nowcast"
) %>%
  mutate(
    lower = estimate - sd * 1.96,
    upper = estimate + sd * 1.96
  )

# data for the model
data <- macrodistancing_data()

#process the old greta format data into gam style data
contact_data <- data$contacts %>%
  select(date, state, contact_num,wave_date) %>%
  mutate(
    contact_num = ifelse(contact_num > 99, 99, contact_num),
    dow = wday(date) #%>%
    #as.character
  ) #%>% bind_rows(baseline_contact)

public_holidays <- holiday_dates() %>%
  mutate(
    state = abbreviate_states(state)
  ) %>%
  rename(
    holiday = name
  )

school_holidays <- school_holiday_dates() %>%
  mutate(
    state = abbreviate_states(state)
  )



intervention_steps <- interventions(
  end_dates = TRUE#,
  # exclude_after = "2021-10-21"
) %>%
  filter(date <= max(data$location_change_trends$date),
         date >=min(data$contacts$date)) %>%
  # no survey data from during the TAS lockdown in these dates so not possible
  # to fit effect of this lockdown, and therefore excluding this intervention
  filter(!(state == "TAS" & date >= "2021-10-16" & date <= "2021-10-19")) %>%
  mutate(
    intervention_id = paste0(
      "intervention_",
      match(date, unique(date))
    )
  ) %>%
  group_by(intervention_id, state) %>%
  do(
    tibble(
      date = seq(min(data$location_change_trends$date), 
                 max(data$location_change_trends$date), 
                 by = 1),
      intervention_effect = as.numeric(seq(min(data$location_change_trends$date), 
                                           max(data$location_change_trends$date), 
                                           by = 1) >= .$date)
    )
  ) %>%
  group_by(state, date) %>%
  summarise(
    intervention_stage = sum(intervention_effect),
    .groups = "drop"
  ) %>%
  mutate(
    intervention_stage = factor(intervention_stage)
  )



df_fit <- data$location_change_trends %>%
  select(state,date) %>% 
  left_join(
    public_holidays,
    by = c("state", "date")
  ) %>%
  left_join(
    school_holidays,
    by = c("state", "date")
  ) %>%
  left_join(
    intervention_steps,
    by = c("state", "date")
  ) %>% 
  left_join(
    contact_data,
    by = c("state", "date")
  ) %>% 
  mutate(
    holiday = replace_na(holiday, "none"),
    is_a_holiday = holiday != "none",
    is_a_school_holiday = !is.na(school_holiday),
    holiday = factor(holiday),
    date_num = as.numeric(date - min(date)),
    dow = lubridate::wday(date, label = TRUE)
  ) %>%
  select(-school_holiday) %>% 
  filter(!(is.na(contact_num))) %>% 
  arrange(state) %>% 
  nest(
    fit_dat = c(
      date,
      date_num,
      contact_num,
      intervention_stage,
      is_a_holiday,
      holiday,
      is_a_school_holiday,
      dow,
      wave_date
    )
  )


df_pred <- data$location_change_trends %>% 
  select(state,date) %>% 
  left_join(
    public_holidays,
    by = c("state", "date")
  ) %>%
  left_join(
    school_holidays,
    by = c("state", "date")
  ) %>%
  left_join(
    intervention_steps,
    by = c("state", "date")
  ) %>%
  left_join(
    contact_data %>% select(state,date,wave_date),
    by = c("state", "date")
  ) %>% 
  distinct(date,state,.keep_all = TRUE) %>% 
  mutate(
    holiday = replace_na(holiday, "none"),
    is_a_holiday = holiday != "none",
    is_a_school_holiday = !is.na(school_holiday),
    holiday = factor(holiday),
    date_num = as.numeric(date - min(date)),
    dow = lubridate::wday(date, label = TRUE)
  ) %>% 
  arrange(state) %>%
  select(-school_holiday) %>% 
  nest(
    pred_dat = c(
      date,
      date_num,
      intervention_stage,
      is_a_holiday,
      holiday,
      is_a_school_holiday,
      dow,
      wave_date
    )
  )


df_mic <- full_join(
  df_fit,
  df_pred,
  by = "state"
) 



# fit model by state
set.seed(2020-05-30)

pred_trend <- mapply(
  FUN = fit_contact_survey_gam,
  fit_dat = df_mic$fit_dat,
  pred_dat = df_mic$pred_dat,
  date_num_spline_base = 40,
  SIMPLIFY = FALSE
)


pred_plot <- df_mic %>%
  mutate(fit = pred_trend) %>% 
  unnest(fit) %>%
  dplyr::select(-fit_dat, -pred_dat)


saveRDS(pred_plot,
        file = "outputs/macrodistancing_trends.RDS")

# run only up to here for reff update
##<2


# estimates at peak and at latest date
pred_summary <- pred_plot %>%
  group_by(state) %>%
  summarise(peak = which.min(mean),
            peak_estimate = mean[peak],
            peak_low = ci_90_lo[peak],
            peak_high = ci_90_hi[peak],
            peak_date = date[peak],
            latest = which.max(date),
            latest_estimate = mean[latest],
            latest_low = ci_90_lo[latest],
            latest_high = ci_90_hi[latest],
            latest_date = date[latest]) %>%
  select(-peak, -latest)

saveRDS(pred_summary,
        file = "outputs/macrodistancing_trend_summary.RDS")

# fit a null-ish model (independent over
# waves/states) to visualise the data values

pred_trend_null <- mapply(
  FUN = fit_contact_survey_null_gam,
  fit_dat = df_mic$fit_dat,
  pred_dat = df_mic$pred_dat,
  SIMPLIFY = FALSE
)

pred_plot_null <- df_mic %>%
  mutate(fit = pred_trend_null) %>% 
  unnest(fit) %>%
  dplyr::select(-fit_dat, -pred_dat) %>% 
  distinct(wave_date,state,.keep_all = TRUE)


# summarise fitted values for each date/state combination
sry <- pred_plot_null %>%
  rename(
    estimate = mean,
    lower = ci_lo,
    upper = ci_hi
  )

# The width of the horizontal bars for survey data is proportional to the
# duration, but plotted in arbitrary units (which depend on the plot size).
# Rescale it with this tweaking parameter to roughly match the durations

# slim down dataframe to get independent estimates for surveys
survey_points <- data$contacts %>%
  group_by(state, wave_date) %>%
  summarise(
    n = n(),
    wave_duration = first(wave_duration)
  )  %>%
  ungroup() %>%
  right_join(sry) %>%
  mutate(
    width = wave_duration
  ) %>%
  mutate(type = "Nowcast")

# save these fits for plotting later
saveRDS(survey_points, "outputs/macro_data_fit.RDS")
#survey_points <- readRDS("outputs/macro_data_fit.RDS")

#determine cutoff date
left.cutoff.date <- max(data$contacts$date) - years(4)

survey_points <- survey_points %>% filter(wave_date >= left.cutoff.date)

# get holiday dates and subset to where they overlap with surveys
holiday_lines <- survey_points %>%
  mutate(date_start = wave_date - wave_duration / 2,
         date_end = wave_date + wave_duration / 2) %>%
  select(state, date_start, date_end) %>%
  left_join(
    holiday_dates() %>% filter(date >= left.cutoff.date) %>% 
      mutate(state = abbreviate_states(state))
  ) %>%
  filter(date < date_end & date > date_start)

holiday_lines <- holiday_dates() %>% filter(date >= left.cutoff.date) %>% 
  mutate(
    state = abbreviate_states(state)
  ) %>%
  filter(
    date <= max(data$contacts$date) &
      date >= as.Date("2020-03-01")
  )

intervention_steps <- intervention_steps %>% filter(date >= left.cutoff.date)


type <- 1
states <- unique(data$location_change_trends$state)
dates <- unique(data$location_change_trends$date)
n_states <- length(states)

# mock up data object for plotting
plot_data <- list(
  dates = list(
    infection_project = dates,
    latest_mobility = max(dates)
  ),
  states = states,
  n_states = length(states),
  n_dates_project = length(dates)
)


macro_ticks_labels <- split_ticks_and_labels(
  data = survey_points %>% filter(wave_date >= left.cutoff.date) %>% pull(wave_date),
  tick_freq = "1 month",
  label_freq = "5 months",
  label_format = "%b%y",
  label_last = FALSE # for some reason this is having opposite effect, i.e. FALSE is labelling last (as desired)
)





# non-household contacts
p <- plot_trend(use_simulations = FALSE,
                summary_sim = pred_plot,
                data = plot_data,
                multistate = TRUE,
                base_colour = purple,
                max_date = max(data$contacts$date),
                min_date = left.cutoff.date,
                ylim = c(0, 20),
                hline_at = NULL) + 
  ggtitle(label = "Macro-distancing trend",
          subtitle = "Rate of non-household contacts") +
  ylab("Estimated mean number of non-household contacts per day") + 
  # # add baseline estimate
  # geom_hline(yintercept = baseline_point$estimate,
  #   colour = grey(0.5,0.5),
  #   linetype = 5
  # ) +
  
  # # add baseline estimate
  # geom_point(
  #   aes(date, estimate),
  #   data = baseline_point,
  #   size = 0.5,
  #   colour = grey(0.5)
  # ) +
  # geom_errorbar(
  #   aes(
  #     date,
  #     estimate,
  #     ymin = lower,
  #     ymax = upper
  #   ),
  #   data = baseline_point,
  #   width = 0,
  #   colour = grey(0.5)
  # ) + 

  # rug marks for holidays
  geom_rug(
    aes(date),
    data = holiday_lines,
    col = green,
    size = 1,
    length = unit(0.05, "npc"),
    sides = "b",
    inherit.aes = FALSE
  ) +

  # add survey results estimate
  geom_point(
    aes(
      wave_date,
      estimate,
    ),
    data = survey_points,
    size = 2,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(
      wave_date,
      estimate,
      ymin = lower,
      ymax = upper,
    ),
    data = survey_points,
    size = 1,
    alpha = 0.2,
    width = 0
  ) +
  scale_x_date(
    breaks = macro_ticks_labels$ticks,
    labels = macro_ticks_labels$labels
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.ticks.x = element_line(colour = macro_ticks_labels$tick.cols)
  )

p

save_ggplot("macrodistancing_effect.png",
            width = 11.69 / 1.5,
            height = 8.27 / 2.5)


p <- plot_trend(use_simulations = FALSE,
                summary_sim = pred_plot,
                data = plot_data,
                multistate = TRUE,
                base_colour = purple,
                max_date = max(data$contacts$date),
                min_date = left.cutoff.date,
                ylim = c(0, 20),
                hline_at = NULL) + 
  ggtitle(label = "Macro-distancing trend",
          subtitle = "Rate of non-household contacts") +
  ylab("Estimated mean number of non-household contacts per day") + 
  # add baseline estimate
  # geom_hline(yintercept = baseline_point$estimate,
  #            colour = grey(0.5,0.5),
  #            linetype = 5
  # ) +
  # 
  # # add baseline estimate
  # geom_point(
  #   aes(date, estimate),
  #   data = baseline_point,
  #   size = 0.5,
  #   colour = grey(0.5)
  # ) +
  # geom_errorbar(
  #   aes(
  #     date,
  #     estimate,
  #     ymin = lower,
  #     ymax = upper
  #   ),
  #   data = baseline_point,
  #   width = 0,
  #   colour = grey(0.5)
  # ) + 
  
  # rug marks for holidays
  geom_rug(
    aes(date),
    data = holiday_lines,
    col = green,
    size = 1,
    length = unit(0.05, "npc"),
    sides = "b",
    inherit.aes = FALSE
  ) +
  scale_x_date(
    breaks = macro_ticks_labels$ticks,
    labels = macro_ticks_labels$labels
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.ticks.x = element_line(colour = macro_ticks_labels$tick.cols)
  )

p

save_ggplot("macrodistancing_effect_no_data_vis.png",
            width = 11.69 / 1.5,
            height = 8.27 / 2.5)

# non-household contacts
p <- plot_trend(use_simulations = FALSE,
                summary_sim = pred_plot,
                data = plot_data,
                multistate = TRUE,
                base_colour = purple,
                max_date = max(data$contacts$date),
                min_date = max(data$contacts$date) - months(6),
                ylim = c(0, 20),
                hline_at = NULL) + 
  ggtitle(label = "Macro-distancing trend",
          subtitle = "Rate of non-household contacts") +
  ylab("Estimated mean number of non-household contacts per day") + 
  # # # add baseline estimate
  # scale_x_date(limits = c(max(data$contacts$date) - months(6),max(data$contacts$date))) + 
  # geom_point(aes(x = max(data$contacts$date),
  #                y = baseline_point$estimate),
  #            shape = 17,
  #            colour = grey(0.5,0.5),
  #            size = 2) + 
             #            colour = grey(0.5,0.5),
             #            linetype = 5)
  # geom_hline(yintercept = baseline_point$estimate,
  #            colour = grey(0.5,0.5),
  #            linetype = 5
  # ) +
  # 
  # geom_ribbon(aes(ymin = baseline_point$lower,
  #                 ymax = baseline_point$upper),
  #             alpha = 0.1,
  #             colour = grey(0.5,0.2),
  #             fill = grey(0.5,0.1)) +
  # rug marks for holidays
  
geom_rug(
    aes(date),
    data = holiday_lines %>%
      filter(date >= max(data$contacts$date) - months(6)),
    col = green,
    size = 1,
    length = unit(0.05, "npc"),
    sides = "b",
    inherit.aes = FALSE
  ) +
  
  # add survey results estimate
  geom_point(
    aes(
      wave_date,
      estimate,
    ),
    data = survey_points %>%
      filter(wave_date >= max(data$contacts$date) - months(6)),
    size = 2,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(
      wave_date,
      estimate,
      ymin = lower,
      ymax = upper,
    ),
    data = survey_points %>%
      filter(wave_date >= max(data$contacts$date) - months(6)),
    size = 1,
    alpha = 0.2,
    width = 0
  )

p

save_ggplot("macrodistancing_effect_six_month.png")

