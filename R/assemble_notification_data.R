
source("R/functions.R")

#load full linelist format data (NINDSS + NCIMS)
get_latest_linelist()

linelist <- readRDS(paste0("outputs/",get_latest_linelist()))

plot_linelist_by_confirmation_date(linelist = linelist)

#load all summary format data
summary_data <- get_summary_data(states = "VIC")

#visually check for issues
summary_data %>% filter(date>=(max(summary_data$date)-days(30))) %>% 
  ggplot(aes(x = date, y = cases, fill = test_type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~state,scales = "free")

# #remove last day of data in Qld or Vic if it is incomplete 
# summary_data <- summary_data %>% 
#   filter(date < max(summary_data$date) | state == "VIC")

# #visually check for issues again
# summary_data %>% filter(date>=(max(summary_data$date)-months(1))) %>% 
#   ggplot(aes(x = date, y = cases, fill = test_type)) + 
#   geom_col(position = "dodge") + 
#   facet_wrap(~state,scales = "free")

#get qld for the period where NINDSS had RAT duplications
qld_issue_period <- get_qld_summary_data()

#get act for the period where NINDSS had a rat spike issue
act_issue_period <- get_act_summary_data()

#replace linelist components for states with summary data
linelist <- replace_linelist_bits_with_summary(linelist,
                                               summary_data,
                                               states_select = c("VIC"),
                                               start = as_date("2022-01-06"),
                                               end = NULL)

linelist <- replace_linelist_bits_with_summary(linelist,
                                               act_issue_period,
                                               states_select = c("ACT"),
                                               start = as_date("2022-01-06"),
                                               end = as_date("2022-03-28"))

linelist <- replace_linelist_bits_with_summary(linelist,
                                               qld_issue_period,
                                               states_select = c("QLD"),
                                               start = as_date("2022-01-06"),
                                               end = as_date("2023-02-28"))

#check if ACT is properly joined
plot_linelist_by_confirmation_date(linelist = linelist, date_cutoff = "2022-01-01")


#make watermelon style checking plot
plot_linelist_by_confirmation_date(linelist = linelist)
ggsave("outputs/figures/case_count_by_confirmation.png", bg = 'white',height = 5,width = 9)


#impute correct confirmation dates for NSW RAT weekend cases dumped on Monday
nsw_wrong_RATs_period <- seq.Date(as.Date("2023-02-25"),max(linelist$date_confirmation),by = "day")


mondays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 2]
tuesdays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 3]
sundays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 1]
saturdays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 7]

for (week_iter in seq_along(mondays_to_fix)) {
  
  #shift all PCR dates to Tuesday
  linelist <- linelist %>% 
    mutate(date_confirmation = case_when(
      date_confirmation %in% c(saturdays_to_fix[week_iter],
                               sundays_to_fix[week_iter],
                               mondays_to_fix[week_iter]) & 
        test_type == "PCR" & 
        state == "NSW" ~ tuesdays_to_fix[week_iter],
      TRUE ~ date_confirmation
      )
    )
  
  #shift PCR dates back in place via disaggregation
  linelist <- stagger_dates_in_linelist(linelist = linelist,
                                        state_select = "NSW",
                                        test_type = "PCR",
                                        dates_to = c(saturdays_to_fix[week_iter],
                                                     sundays_to_fix[week_iter],
                                                     mondays_to_fix[week_iter]),
                                        date_from = tuesdays_to_fix[week_iter])
  #disaggregate RAT dates
  linelist <- stagger_dates_in_linelist(linelist = linelist,
                                        state_select = "NSW",
                                        test_type = "RAT",
                                        dates_to = c(saturdays_to_fix[week_iter],
                                                     sundays_to_fix[week_iter]),
                                        date_from = mondays_to_fix[week_iter])
  
}


#truncate for jurisdictions with incomplete reporting days (only PCR or RAT)
linelist <- linelist %>% 
  group_by(date_confirmation,state) %>% 
  mutate(type_count = length(unique(test_type))) %>% 
  ungroup() %>% 
  filter(type_count == 2 | 
         date_confirmation <= (max(linelist$date_confirmation) - weeks(1)) |
         state == "NSW") %>%
  #the date filter is necessary to avoid removing pre RAT era cases
  select(!type_count)
#check if any last day appears to have incomplete reporting
plot_linelist_by_confirmation_date(linelist = linelist)

#drop the latest reporting day for some jurisdictions if incomplete 
#typically this is SA due to data uploaded on extraction day
linelist <- linelist %>% 
  filter(date_confirmation < (max(linelist$date_confirmation)) | state != "SA")

plot_linelist_by_confirmation_date(linelist = linelist)
#plot the confirmation plot again after all the fixes
ggsave("outputs/figures/case_count_by_confirmation_post_processing.png", bg = 'white',height = 5,width = 9)
#record the days of lag for each jurisdiction
state_date_lag <- linelist %>% 
  group_by(state) %>% 
  summarise(last_date = max(date_confirmation)) %>% 
  ungroup() %>% 
  mutate(days_lag = max(last_date) - last_date,
         days_lag = as.numeric(days_lag))


state_date_lag
#doublecheck date range
linelist %>% pull(date_confirmation) %>% range()


#use NSW part of the linelist to get delay cdfs for different test modes

#cut off date at the beginning of RAT reporting
delay_to_consider_date_cutoff <- as_date("2022-01-06")

RAT_cdf <- get_notification_delay_cdf(linelist = linelist %>% 
                                        filter(date_confirmation >= delay_to_consider_date_cutoff,
                                               test_type == "RAT"),
                                      use_nsw_delay = TRUE)

PCR_cdf <- get_notification_delay_cdf(linelist = linelist %>% 
                                        filter(date_confirmation >= delay_to_consider_date_cutoff,
                                               test_type == "PCR"),
                                      use_nsw_delay = TRUE)

#impute onsets separately and then put together, not the most efficient approach
#but works better with legacy code

#check if any case has missing test type
table(linelist$test_type)

set.seed(2020-04-29)
linelist_RAT <- linelist %>% 
  filter(test_type == "RAT") %>% 
  impute_linelist(notification_delay_cdf = RAT_cdf)

linelist_PCR <- linelist %>% 
  filter(test_type == "PCR") %>% 
  impute_linelist(notification_delay_cdf = PCR_cdf)

linelist <- rbind(linelist_RAT,linelist_PCR) %>%
  arrange(state, date_confirmation, date_onset)

rm(linelist_RAT,linelist_PCR)
gc()

saveRDS(linelist,"outputs/imputed_linelist.RDS")
#linelist <- readRDS("outputs/imputed_linelist.RDS")

data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = NULL,
                        impute_infection_with_CAR = TRUE,
                        state_specific_right_truncation = TRUE, 
                        cutoff_is_manual = TRUE)
#data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
saveRDS(data, "outputs/pre_loaded_reff_data.RDS")
#data <- readRDS("outputs/pre_loaded_reff_data.RDS")

source("R/watermelon_plot_completion.R")

write_local_cases(data)


#make PCR only version - in dev

# 
# data <- reff_model_data(linelist_raw = linelist %>% filter(test_type == "PCR"),
#                         notification_delay_cdf = NULL,
#                         impute_infection_with_CAR = TRUE,
#                         state_specific_right_truncation = TRUE)
# #data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
# saveRDS(data, "outputs/pre_loaded_reff_data_PCR_only.RDS")
# #data <- readRDS("outputs/pre_loaded_reff_data_old_imputation.RDS")
# 
# source("R/watermelon_plot_completion.R")
# 
#  if (!dir.exists("outputs/PCR_only_local_cases")) {
#    dir.create("outputs/PCR_only_local_cases")
#  }
# 
# write_local_cases(data, dir = "outputs/PCR_only_local_cases",suffix = "PCR_only")
# 
