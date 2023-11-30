# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script to view old linelists against current linelist to
# evaluate the performance of the observation model
#
# David J Price
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# function to calculate CI for the inferred case counts based on a NB distn
true_count_quantile <- function(observed_count, probability, quantiles = c(0.05, 0.95)) {
  observed_count + qnbinom(quantiles, observed_count + 1, probability)
}

# which linelists to load/evaluate (could automate this better)

retrospective.linelists <- c(
  "2023-09-14",
  "2023-09-21",
  "2023-09-28", "2023-10-05", 
  "2023-10-12", "2023-10-19",
  "2023-10-26", 
  "2023-11-02",
  "2023-11-09"
)
ndates <- length(retrospective.linelists)

# Specify most recent file
latest.file.date <- "2023-11-23"

#detect.limit <- 0.95
detect.limit <- 0.05

weeks.back <- 9
# this could be done better, but chose this value to show ~a week worth of
# points with the 0.95 cutoff on detection_probability for the plot that is
# 6 weeks back


# Much of this has been hacked together to deal with recent changes in local_cases structure
## mutate here to create columns that don't exist in the older local_cases, so add_row works
## and fills with NAs where not present in subsequent fields
## within the for loop, where the column changes to completion_probability, rename it as detection_probability
## to be consistent with the remainder of the local_cases files (and subsequent code)

all.dat <- read_csv(paste0("outputs/local_cases_input_",retrospective.linelists[1],".csv"),
                    col_types = list(date_onset = col_date(""))) %>%
  mutate(PCR_only = NA,
         PCR_only_CAR = NA,
         "data.date"=ymd(retrospective.linelists[1])) 

for (i in 2:ndates){
  tmp <- read_csv(paste0("outputs/local_cases_input_",retrospective.linelists[i],".csv")) %>%
    mutate("data.date"=ymd(retrospective.linelists[i]))
  
  # For some reason, one was reading as DD/MM/YYYY rather than YYYY-MM-DD so this is my
  # quick hack to fix, could also be done better
  if(all(grepl(pattern = "/", x = tmp$date_onset))){
    tmp <- tmp %>% mutate(date_onset = dmy(date_onset))
  } else{
    tmp <- tmp %>% mutate(date_onset = ymd(date_onset))
  }
  
  #if(any(colnames(tmp) == "completion_probability")){
  # tmp <- tmp %>% rename(detection_probability = completion_probability)
  #}
  
  all.dat <- all.dat %>% add_row(tmp)
  
}

#all.dat <- all.dat %>% 
#   rename(detection_probability = completion_probability)

# load latest line list
latest.ll.all <- read_csv(paste0("outputs/local_cases_input_",latest.file.date,".csv"),
                          col_types = list(date_onset = col_date("")))

earliest.case.date <- max(latest.ll.all$date_onset) - weeks(weeks.back)


# Calculate inferred cases and associated CIs
all.dat <- all.dat %>% mutate(inf.count = count/completion_probability,
                              L = true_count_quantile(count, completion_probability, 0.05),
                              U = true_count_quantile(count, completion_probability, 0.95)) %>%
  filter(
    completion_probability >= detect.limit,
    date_onset >= earliest.case.date)

latest.ll.all <- latest.ll.all %>%
  rename(completion_probability = completion_probability) %>%
  mutate(inf.count = count/completion_probability,
         L = true_count_quantile(count, completion_probability, 0.05),
         U = true_count_quantile(count, completion_probability, 0.95)) %>%
  filter(completion_probability >= detect.limit,
         date_onset >= earliest.case.date)

# create folder for this week (using most recent linelist file)
dir.create(paste0("outputs/reporting_delay/",latest.file.date))

plot.these.dates <- sort(ymd(retrospective.linelists), decreasing = TRUE)[1:6]

for (the.state in c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")){
  
  # Filter files to relevant state
  dat <- all.dat %>%
    filter(state == the.state,
           data.date %in% plot.these.dates)
  
  latest.ll <- latest.ll.all %>%
    filter(state == the.state)
   
  completion_date <- as_date(min(latest.ll$date_onset[latest.ll$completion_probability <1]))  
                             
  dat %>%
    ggplot() +
    aes(x = date_onset) +
    
    geom_col(data = latest.ll, aes(x = date_onset, y = inf.count), fill = "grey80") +
    geom_col(data = latest.ll, aes(x = date_onset, y = count), fill = "grey60") +
    
    geom_vline(xintercept = completion_date) +
    
    geom_point(aes(y = count), pch = 21, fill = "firebrick", size = 2) +
    geom_errorbar(aes(ymin = L, ymax = U), col = "steelblue3", width = 0) +
    geom_point(aes(y = inf.count), pch = 21, fill = "steelblue3", size = 2) +
    ggtitle(the.state) +
    scale_y_continuous("Daily Cases") +
    scale_x_date("Onset Date", date_breaks = "2 weeks", date_labels = "%e/%m") +
    facet_wrap(~as.factor(data.date), ncol = 2) +
    cowplot::theme_cowplot()
  
  ggsave(paste0("outputs/reporting_delay/",latest.file.date,"/",tolower(the.state),"_reporting_delay_",latest.file.date,".pdf"), height = 12, width = 10, units = "in", bg = "white")
  
  
  # The ratio plots, where inferred counts at the time are scaled by the 'inferred'
  # counts in the most recent data (i.e., truth), should be ~1
  

  
  dat %>%
    select(date_onset, data.date, count, inf.count, L, U) %>%
    left_join(latest.ll %>% select(date_onset, count, inf.count, L, U), by = c("date_onset")) %>%
    mutate(count.ratio = count.x/count.y, inf.count.ratio = inf.count.x/inf.count.y) %>%
    ggplot() +
    aes(x = date_onset) +
    
    geom_hline(yintercept = 1, colour = "grey70") +
    geom_hline(yintercept = 0.5, colour = "grey90", lty = 2) +
    
    geom_vline(xintercept = completion_date) +
    
    geom_point(aes(y = count.ratio), pch =21, fill = "firebrick") +
    geom_point(aes(y = inf.count.ratio), pch =21, fill = "steelblue3") +
    
    
    
    ggtitle(the.state) +
    scale_y_continuous("Ratio of data at time to current data") +
    scale_x_date("Onset Date", date_breaks = "1 week", date_labels = "%e/%m") +
    coord_cartesian(ylim = c(0,2)) +
    facet_wrap(~data.date, ncol = 2) +
    cowplot::theme_cowplot()
  
  ggsave(paste0("outputs/reporting_delay/",latest.file.date,"/",tolower(the.state),"_reporting_delay_",latest.file.date,"_ratio.pdf"), height = 12, width = 10, units = "in", bg = "white")
}




