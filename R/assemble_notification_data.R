
source("R/functions.R")

#load full linelist format data (NINDSS + NCIMS)
get_latest_linelist()

linelist <- readRDS(paste0("outputs/",get_latest_linelist()))

plot_linelist_by_confirmation_date(linelist = linelist)

#load all summary format data
summary_data <- get_summary_data(states = "VIC")

#visually check for issues
summary_data %>% filter(date>=(max(summary_data$date)-days(90))) %>% 
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

#impute correct confirmation dates for NSW RAT weekend cases dumped on Monday and other issues with reporting delays
nsw_wrong_RATs_period <- seq.Date(as.Date("2023-02-25"),max(linelist$date_confirmation),by = "day")

mondays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 2]
tuesdays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 3]
sundays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 1]
saturdays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 7]
wednesdays_to_fix <- nsw_wrong_RATs_period[wday(nsw_wrong_RATs_period) == 4]

#lovely long if else statement to sort out the problematic cases
for (week_iter in seq_along(mondays_to_fix)) {
  
# NT not reporting PCR cases on weekends anymore - dumped on Monday. Stagger out
  if(mondays_to_fix[week_iter] >= "2023-11-13") {
    #shift all PCR cases to Monday
    linelist <- linelist %>% 
      mutate(date_confirmation = case_when(
        date_confirmation %in% c(saturdays_to_fix[week_iter],
                                 sundays_to_fix[week_iter]) & 
          test_type == "PCR" & 
          state == "NT" ~ mondays_to_fix[week_iter],
        TRUE ~ date_confirmation))
    #shift PCR dates back in place via disaggregation
    linelist <- stagger_dates_in_linelist(linelist = linelist,
                                          state_select = "NT",
                                          test_type = "PCR",
                                          dates_to = c(saturdays_to_fix[week_iter],
                                                       sundays_to_fix[week_iter]),
                                          date_from = mondays_to_fix[week_iter],
                                          use_delay_cdf = FALSE)
  }
#Victorian PCR reporting down on 9th and partially down on 10th of July. Cases reported on 12th of July instead
   if (mondays_to_fix[week_iter] == "2023-07-10") {
    #shift all PCR cases to Wednesday
    linelist <- linelist %>% 
      mutate(date_confirmation = case_when(
        date_confirmation %in% c(sundays_to_fix[week_iter],
                                 mondays_to_fix[week_iter]) & 
          test_type == "PCR" & 
          state == "VIC" ~ as_date("2023-07-12"),
        TRUE ~ date_confirmation
      )
      )
    #shift PCR dates back in place via disaggregation    
    linelist <- stagger_dates_in_linelist(linelist = linelist,
                                          state_select = "VIC",
                                          test_type = "PCR",
                                          dates_to = c(sundays_to_fix[week_iter],
                                                       mondays_to_fix[week_iter]),
                                          date_from = wednesdays_to_fix[week_iter], 
                                          use_delay_cdf = FALSE)
    
  }
  if (mondays_to_fix[week_iter] == "2023-07-03") {
    #missing most RAT cases from the prior week - minor counts on Tues-Fri, 
    #nothing on weekend and then rest appear to be dumped on Monday. PCR appears fine
    #shift all RAT dates to Monday
    linelist <- linelist %>% 
      mutate(date_confirmation = case_when(
        date_confirmation %in% c(as_date("2023-06-27"),
                                 as_date("2023-06-28"),
                                 as_date("2023-06-29"),
                                 as_date("2023-06-30")) & 
          test_type == "RAT" & 
          state == "NSW" ~ mondays_to_fix[week_iter],
        TRUE ~ date_confirmation
      )
      )
    #shift PCR dates back in place via disaggregation
    linelist <- stagger_dates_in_linelist(linelist = linelist,
                                          state_select = "NSW",
                                          test_type = "RAT",
                                          dates_to = c(as_date("2023-06-27"),
                                                       as_date("2023-06-28"),
                                                       as_date("2023-06-29"),
                                                       as_date("2023-06-30"),
                                                       as_date("2023-07-01"),
                                                       as_date("2023-07-02")),
                                          date_from = mondays_to_fix[week_iter])
    
  }
  
  else {
    
    #deal with NSW Labour day exception where all RAT tests for Sat-Mon were reported on Tuesday.
    #PCR appears to have been reported on Mon and is therefore fine to use normal function  
    if (mondays_to_fix[week_iter] == "2023-06-12") {
      #missing full RAT from Sat-Sun,so take cases from Tuesday instead
      #disaggregate RAT dates
      linelist <- stagger_dates_in_linelist(linelist = linelist,
                                            state_select = "NSW",
                                            test_type = "RAT",
                                            dates_to =   c(saturdays_to_fix[week_iter],
                                                           sundays_to_fix[week_iter],
                                                           mondays_to_fix[week_iter]),
                                            date_from = tuesdays_to_fix[week_iter])
    }  
    else {
  #deal with easter long weekend exception
  if (mondays_to_fix[week_iter] == "2023-04-10") {
    #missing full RAT and partial PCR data from Good Friday to Easter Monday
    #so include Friday to this shift, and take cases from Tuesday
    #shift all PCR dates to Tuesday
    linelist <- linelist %>% 
      mutate(date_confirmation = case_when(
        date_confirmation %in% c(as_date("2023-04-07"),
                                 saturdays_to_fix[week_iter],
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
                                          dates_to = c(as_date("2023-04-07"),
                                                       saturdays_to_fix[week_iter],
                                                       sundays_to_fix[week_iter],
                                                       mondays_to_fix[week_iter]),
                                          date_from = tuesdays_to_fix[week_iter])
    #disaggregate RAT dates
    linelist <- stagger_dates_in_linelist(linelist = linelist,
                                          state_select = "NSW",
                                          test_type = "RAT",
                                          dates_to = c(as_date("2023-04-07"),
                                                       saturdays_to_fix[week_iter],
                                                       sundays_to_fix[week_iter],
                                                       mondays_to_fix[week_iter]),
                                          date_from = tuesdays_to_fix[week_iter])
  } else { 
    #deal with ANZAC day exception
    if (mondays_to_fix[week_iter] == "2023-04-24") {
      #missing full RAT and partial PCR data on ANZAC Tuesday
      #so shift back from Wednesday
      #shift all weekend PCR to Monday not Tuesday
      linelist <- linelist %>% 
        mutate(date_confirmation = case_when(
          date_confirmation %in% c(saturdays_to_fix[week_iter],
                                   sundays_to_fix[week_iter]) & 
            test_type == "PCR" & 
            state == "NSW" ~ mondays_to_fix[week_iter],
          TRUE ~ date_confirmation
          )
        )
        
      
      #shift PCR dates back in place via disaggregation
      linelist <- stagger_dates_in_linelist(linelist = linelist,
                                            state_select = "NSW",
                                            test_type = "PCR",
                                            dates_to = c(saturdays_to_fix[week_iter],
                                                         sundays_to_fix[week_iter]),
                                            date_from = mondays_to_fix[week_iter])
      #disaggregate RAT dates
      #do this for weekend and then for Tues
      linelist <- stagger_dates_in_linelist(linelist = linelist,
                                            state_select = "NSW",
                                            test_type = "RAT",
                                            dates_to = c(saturdays_to_fix[week_iter],
                                                         sundays_to_fix[week_iter]),
                                            date_from = mondays_to_fix[week_iter])
      
      linelist <- stagger_dates_in_linelist(linelist = linelist,
                                            state_select = "NSW",
                                            test_type = "RAT",
                                            dates_to = c(tuesdays_to_fix[week_iter]),
                                            date_from = as_date("2023-04-26"))
    } else {
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
  }
}}}

#truncate for jurisdictions with incomplete reporting days (only PCR or RAT)
#NOTE NT cases are now so low that there may actually be 0 cases of one type. Visually check the NT before running this. 
#linelist <- linelist %>% 
 # group_by(date_confirmation,state) %>% 
  #mutate(type_count = length(unique(test_type))) %>% 
  #ungroup() %>% 
  #filter(type_count == 2 | 
   #      date_confirmation <= (max(linelist$date_confirmation) - weeks(1)) |
    #     state == "NSW") %>%
  #the date filter is necessary to avoid removing pre RAT era cases
  #select(!type_count)
#check if any last day appears to have incomplete reporting
plot_linelist_by_confirmation_date(linelist = linelist)

#drop the latest reporting day for some jurisdictions if incomplete 
#typically this is SA due to data uploaded on extraction day
linelist <- linelist %>% 
  filter(date_confirmation < (max(linelist$date_confirmation)-1) | state != "WA")

plot_linelist_by_confirmation_date(linelist = linelist)
#plot the confirmation plot again after all the fixes
ggsave("outputs/figures/case_count_by_confirmation_post_processing.png", bg = 'white',height = 5,width = 9)

plot_linelist_by_confirmation_date(linelist = linelist,
                                   date_cutoff = max(linelist$date_confirmation) - days(180))
#plot the confirmation plot for 6months
ggsave("outputs/figures/case_count_by_confirmation_post_processing_6months.png", 
       bg = 'white',
       height = 5,
       width = 9)

linelist <- linelist %>% filter( 
  !(test_type == "RAT" & 
      state == "VIC" &
      date_confirmation > "2023-06-30")) %>%
  filter(!(test_type == "RAT" & 
             state == "QLD" &
             date_confirmation > "2023-08-31")) %>%
  filter(!(test_type == "RAT" & 
             state == "NSW" &
             date_confirmation > "2023-09-30")) %>%
  filter(!(test_type == "RAT" & 
             state == "WA" &
             date_confirmation > "2023-10-08")) %>%
  filter(!(test_type == "RAT" & 
             state == "NT" &
             date_confirmation > "2023-10-21"))
  
#save processed linelist before imputation for new reff model  
saveRDS(
  linelist,
  sprintf(
    "outputs/processed_linelist_%s.RDS",
    linelist$date_linelist[1] %>%
      format.Date(format = "%Y%m%d")
  )
)

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
# delay_to_consider_date_cutoff <- as_date("2022-01-06")
# latest_symptom_survey_cutoff <- as_date("2023-03-21")
# 
# 
# RAT_cdf <- get_notification_delay_cdf(linelist = linelist %>% 
#                                         filter(date_confirmation >= delay_to_consider_date_cutoff,
#                                                date_onset <= latest_symptom_survey_cutoff,
#                                                test_type == "RAT"),
#                                       use_nsw_delay = TRUE)
# 
# PCR_cdf <- get_notification_delay_cdf(linelist = linelist %>% 
#                                         filter(date_confirmation >= delay_to_consider_date_cutoff,
#                                                date_onset <= latest_symptom_survey_cutoff,
#                                                test_type == "PCR"),
#                                       use_nsw_delay = TRUE)
# 
# saveRDS(RAT_cdf,"outputs/presaved_RAT_cdf.RDS")
# saveRDS(PCR_cdf,"outputs/presaved_PCR_cdf.RDS")

RAT_cdf <- readRDS("outputs/presaved_RAT_cdf.RDS")
PCR_cdf <- readRDS("outputs/presaved_PCR_cdf.RDS")

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
                        PCR_only_states = c('VIC', "QLD", "NSW", "WA", "NT"),
                        PCR_only_CAR_reduction_factor = c(0.27, 0.32, 0.5, 0.34, 0.29))
#data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
saveRDS(data, "outputs/pre_loaded_reff_data.RDS")
#data <- readRDS("outputs/pre_loaded_reff_data.RDS")

#remove PCR cases from Victoria for watermelon plot so as not to confuse James
PCR_states <- c("VIC", "QLD", "NSW", "WA", "NT")
linelist <- linelist %>% filter( 
  !(test_type == "RAT" & 
      state %in% PCR_states))

source("R/watermelon_plot_completion.R")

write_local_cases(data)

#run script to check completion probability delay is acceptable
#at the moment need to manually change date of most recent linelist and add in new ones every week

source("R/explore_reporting.R")

