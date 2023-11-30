# Run the full Reff pipeline

source("R/lib.R")

source("R/functions.R")


# Section A) Independent of NNDSS and survey data update:

# 1. Ingest mobility data, fit mobility models and output figures
# (<state>_datastream_model_fit.png x8 and multistate_model_fit.png), and trend
# estimates (google_change_trends.RDS) [~60s]

# mobility data no longer supplied by Google but run the script anyway to keep
# backward compatible date series assembly for distancing models

#source("R/mobility_change.R")
# -- These figs into Mediaflux/to Freya
# -- outputs/mobility_dates.csv into Mediaflux/to Freya

# Section A.1) Dependent on survey data update (the numbered file must be manually
# copied to data/survey_raw):
# this usually comes on Monday so do this first

# 2. parse all contact rate data, automatically remove duplicates, and visually
# check for duplicate ages [~60s]
parse_all_doh_surveys() %>%
  filter(wave > (max(wave) - 4)) %>%
  plot_age_duplication()

ggsave(
  "outputs/figures/age_deduplication_check.png",
  width = 9,
  height = 10
)
# will light up cells with any overly large counts. If this happens check raw data

# 3. Output microdistancing survey data on Mediaflux:
# (data/microdistancing/Barometer wave <wave> compliance.csv and
# data/contacts/barometer/contact numbers wave <wave>.csv) and face_covering,
# run microdistancing model, and output figure (microdistancing_effect.png) and
# trend estimates (microdistancing_trends.RDS) [~20 min]
source("R/microdistancing_change.R")
# -- figure to Mediaflux / Freya


# 4. Run macrodistancing model and output figure ((microdistancing_effect.png) and
# trend estimates (macrodistancing_trends.RDS). Can be run concurrently with
# microdistancing model (i.e. on a separate process/machine) [~10h]
# macro takes half a day to run so best to do on Mondays
#update instruction
source("R/macrodistancing_change.R")
# -- figure to Mediaflux / Freya

# ????. Run test seeking behaviour plots
source("R/survey_analysis_slack.R")
# -- figure to Mediaflux / Freya


# Section B) Dependent on NNDSS data update:
#source("R/check_linelist.R")
#  produces notification date by state plots for the most recent 28 days
# optionally, produce NINDSS only watermelon plot

# 5. Read in and process NINDSS data

# NINDSS linelist 
# read in the full linelist
linelist_full <- load_linelist(use_vic = FALSE,
                               use_nsw = TRUE,
                               date = as_date("2023-07-12"))

#read in the last 6 months only updates
linelist <- load_linelist(use_vic = FALSE, use_nsw = FALSE) 
#cutoff date
#NNDSS has somehow been giving us incomplete days worth of cases at the start of the weekly (six month updates) linelists
#calculate number of cases on each day in both linelists and find the oldest day where both linelists match to use as the cutoff date
#because of NSW being loaded from different data there may not be many matching days. If this is the case, exclude NSW from the summaries
min_ll_date <- linelist %>% pull(date_confirmation) %>% min()

ll_full_summary <- linelist_full %>% filter(linelist_full$date_confirmation >= min_ll_date) %>% count(date_confirmation)
ll_summary <- linelist %>% count(date_confirmation)

ll_matches <- semi_join(ll_summary, ll_full_summary)
cutoff_date <- min(ll_matches$date_confirmation)

#filter six month update by cutoff date
linelist <- linelist %>% filter(linelist$date_confirmation >= cutoff_date)

#join updated linelist with older one
linelist <- linelist %>% 
  filter(state != "NSW") %>% 
  bind_rows(linelist_full %>% 
              filter(date_confirmation < cutoff_date | state == "NSW"))

#fix WA onset dates that are not correct - anything marked No or NA in the symptoms_reported column 
#has an inferred onset date that needs to be removed. Once that is done remove the symptoms_reported column
linelist$date_onset[(linelist$state == "WA" & linelist$symptoms_reported == "No" )] <- NA
linelist$date_onset[(linelist$state == "WA" & is.na(linelist$symptoms_reported))] <- NA
linelist <- linelist[,-12]

# remove dubious SA onset dates that are dated to same as confirmation dates
linelist$date_onset[(linelist$state == "SA" & linelist$date_onset >= as_date("2022-02-27"))] <- NA

#cut off impossible early RATs that could cause date misalignment
#filtered by date TGA approved RATs in Australia
linelist <- linelist %>% 
  filter(
    !(test_type == "RAT" & 
            date_confirmation <= as_date("2021-11-01"))
         )

#check min & max dates
min_date <- min(linelist$date_confirmation)
min_date
(max_date <- max(linelist$date_confirmation))

#remove dubious confirmation dates (shouldn't be any here)
linelist <- linelist %>% filter(date_confirmation >= "2020-01-23")

#fix future dates problem
linelist <- linelist %>% filter(date_confirmation <= max(date_linelist))

# #fix one off qld error
# linelist <- linelist %>% mutate(date_confirmation = case_when(
#   date_confirmation == "2023-05-06" ~ as_date("2023-04-06"),
#   TRUE ~ date_confirmation))

#visual check
plot_linelist_by_confirmation_date(linelist = linelist, date_cutoff = cutoff_date - months(1))

saveRDS(
  linelist,
  sprintf(
    "outputs/linelist_%s.RDS",
    linelist$date_linelist[1] %>%
      format.Date(format = "%Y%m%d")
  )
)


# 6. assemble NINDSS and state supplied summary data 
source("R/assemble_notification_data.R")

# 9. Run R effective model and output figures (R_eff_12_local.png,
# R_eff_1_import.png, R_eff_1_local.png, R_eff_1_local_macro.png,
# R_eff_1_local_micro.png, R_eff_1_local_surv.png, R_eff_2_local.png in both
# outputs/figures/ and outputs/projection/figures/) and trajectory draws for Rob
# Moss ( r_eff_12_local_samples_soft_clamped_50.csv, r_eff_1_local_samples.csv,
# r_eff_12_local_samples.csv, r_eff_12_local_samples_soft_clamped_95.csv in both
# outputs/ and outputs/projection/) [~3-5h]
source("R/R_effective.R")
## - figures, samples as above, plus wt, alpha and delta, and other variants, and
# outputs/output_dates.csv to Mediaflux/Freya


# 2. Vaccination effect - Quantium update usually CoB Tuesday
# do this after reff as we can use out of date quantium data
# [~ 3 hrs]
source("R/immunity_effect.R")
# -- Figs into Mediaflux / to Freya
# -- all dated csv and rds outputs to Mediaflux

# 5. Run surveillance effect models and output figures (surveillance_effect.png
# and notification_delays.png) and model objects (delay_from_onset_cdfs.RDS)
# [~ 2 hrs]
# this is lengthy now and not used in report, so de-prioritise
source("R/rolling_delays.R")
#  -- figs to Mediaflux / to Freya



# Archived older pipeline
# # 5. Sync NNDSS data and write out case data (local_cases.csv) for Monash (Rob
# Hyndman/Mitch) # and U of Melbourne (Rob Moss/Ruarai). RNG seed needs to match
# that in R_effective.R for imputation to # be consistent. This is also done in
# the final script, but I send it to them # whilst waiting [~60s]
# set.seed(2020-04-29) sync_nndss() # this line probably not necessary for HPC /
# widows, but shoudn't break aything data <- reff_model_data(linelist_raw =
# linelist) data$dates$linelist  # check it synced properly # -- is this date
# the correct linelist date? write_local_cases(data) # -- put in Mediaflux and
# notify Mitch/Rob J/Ruarai
#
# # Check no entries classed as "ERROR" (i.e. conflicting PLACE_OF_ACQUISITION
# and CV_SOURCE_INFECTION) # if OK will only list "imported" and "local"
# linelist %>% pull(import_status) %>% table
#
# # write linelist format for UoAdelaide (Tobin/Josh) if necessary # usually
# unnecessary unless edits to raw linelist write_linelist(linelist = linelist)

# 4. TTIQ and Isolation effect
#source("R/isolation_effect.R")
# -- figs and ttiq_effect.csv to Mediaflux / to Freya

# hooray - you're done!
