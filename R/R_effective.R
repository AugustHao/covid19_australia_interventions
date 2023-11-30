# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

source("R/lib.R")

set.seed(2020-04-29)
source("R/functions.R")

# sync up the case data
#sync_nndss()

# overhead arg for TP constant boolean choice
const_TP <- TRUE

# prepare data for Reff modelling
linelist <- readRDS("outputs/imputed_linelist.RDS")


data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = NULL,
                        start_date = as_date("2021-06-01"),
                        immunity_effect_path = "outputs/combined_effect_full.RDS",
                        ascertainment_level_for_immunity = 0.5,
                        impute_infection_with_CAR = FALSE,
                        PCR_only_states = c("VIC", "QLD", "NSW", "WA", "NT"),
                        state_specific_right_truncation = TRUE)
                        
#check data date
data$dates$linelist
data$dates$earliest
# save the key dates for Freya and David to read in, (deprecated) and tabulated
# local cases data for the Robs

write_reff_key_dates(data)
#write_local_cases(data)

# format and write out any new linelists to the past_cases folder for Rob H
#update_past_cases()

# if need to fit TP, skip the following TP calculations and fit reff model with
# TP_obj = NULL

# to use fitted tp model:
# reload saved fitted model for the TP component
fitted_model <- readRDS("outputs/fitted_full_reff_model.RDS")

#calculate baseline contact params from the full TP model for later calculations

HC_0 <- calculate(c(fitted_model$greta_arrays$distancing_effect$HC_0),
                  nsim = 10000,
                  values = fitted_model$draws)
HC_0 <- c(apply(HC_0[[1]],2:3,mean))

HD_0 <- calculate(c(fitted_model$greta_arrays$distancing_effect$HD_0),
                  nsim = 10000,
                  values = fitted_model$draws)
HD_0 <- c(apply(HD_0[[1]],2:3,mean))

OD_0 <- calculate(c(fitted_model$greta_arrays$distancing_effect$OD_0),
                  nsim = 10000,
                  values = fitted_model$draws)
OD_0 <- c(apply(OD_0[[1]],2:3,mean))

OC_0 <- fitted_model$greta_arrays$distancing_effect$OC_0

infectious_days <- infectious_period(gi_cdf)

#get TP params 
fitted_TP_params <- fitted_model$greta_arrays$TP_params

#calculate TP with new data
fitted_TP_calculations <- TP_only_calculations(data = data,
                                               params = fitted_TP_params)

R_eff_imp_1 <- fitted_TP_calculations$R_eff_imp_1
R_eff_loc_1 <- fitted_TP_calculations$R_eff_loc_1

fitted_TP_calculations_draws <- calculate(R_eff_imp_1,
                                          R_eff_loc_1,
                                          values = fitted_model$draws,
                                          nsim = 10000)

#summarise TP samples for reff model
predicted_TP_obj <- list(R_eff_imp_1 = apply(fitted_TP_calculations_draws$R_eff_imp_1,
                                             2:3,
                                             mean),
                         R_eff_loc_1 = apply(fitted_TP_calculations_draws$R_eff_loc_1,
                                             2:3,
                                             mean),
                         log_R0 = fitted_TP_calculations$log_R0,
                         log_Qt = fitted_TP_calculations$log_Qt,
                         distancing_effect = fitted_TP_calculations$distancing_effect,
                         surveillance_reff_local_reduction = fitted_TP_calculations$surveillance_reff_local_reduction)

# hold TP constant at 1 to enable nowcast with reff prior = 1
if (const_TP) {
  predicted_TP_obj$R_eff_loc_1[] <- 1
}

#fit reff-only model
system.time(
  
  refitted_model <- fit_reff_model(data,warmup = 500,
                                   init_n_samples = 1200,
                                   max_tries = 1, 
                                   iterations_per_step = 1000,
                                   TP_obj = predicted_TP_obj)
)

# save the fitted model object
saveRDS(refitted_model, "outputs/fitted_reff_only_model.RDS")
#refitted_model <- readRDS("outputs/fitted_reff_only_model.RDS")

# # visual checks of model fit
# plot_reff_checks(fitted_model)

# output Reff trajectory draws for Rob M
write_reff_sims(refitted_model, 
                dir = "outputs/projection",
                write_reff_1 = FALSE)

vaccine_effect_timeseries <- readRDS(file = "outputs/vaccination_effect.RDS")

#hack in new data and TP calc to fitted model for sample generation
fitted_model$data <- data
fitted_model$greta_arrays <- fitted_TP_calculations

# this part not in use at the mo
# # write sims of C1 without vaccine effect
# write_reff_sims_novax(
#   fitted_model#,
#   #vaccine_timeseries = vaccine_effect_timeseries
# )
# write_reff_sims(fitted_model, 
#                 dir = "outputs/projection",
#                 write_reff_1 = TRUE,write_reff_12 = FALSE)

# generatge sims for plotting
# (saves repeat generation of sims in each reff_plotting call and keeps them consistent)
sims_TP <- reff_plotting_sims(fitted_model,
                              plot_TP_trajectory = TRUE,
                              plot_reff_trajectory = FALSE)

sims_reff <- reff_plotting_sims(refitted_model,
                              plot_TP_trajectory = FALSE,
                              plot_reff_trajectory = TRUE)


sims <- c(sims_reff,sims_TP)
# do plots for main period
reff_plotting(
  refitted_model,
  dir = "outputs",
  sims = sims
)

# most recent six months
reff_plotting(
  refitted_model,
  dir = "outputs",
  subdir = "figures/six_month",
  min_date = NA,
  sims = sims
)

# most recent month
reff_plotting(
  refitted_model,
  dir = "outputs",
  subdir = "figures/one_month",
  min_date = refitted_model$data$dates$latest_mobility - days(40),
  sims = sims
)


# most recent month no nowcast 
#at the moment hack change of setting the scale_x_date(limits = as.Date(c('xxxx-xx-xx', 'xxxx-xx-xx'))) in plot_trend function to fix x-axis scale
reff_plotting(
  refitted_model,
  dir = "outputs",
  subdir = "figures/one_month/no_nowcast",
  min_date = refitted_model$data$dates$latest_mobility - days(40),
  max_date = refitted_model$data$dates$latest_infection,
  sims = sims,
  mobility_extrapolation_rectangle = FALSE
)

# most recent six months no nowcast
#at the moment hack change of manually setting the scale_x_date(limits = as.Date(c('xxxx-xx-xx', 'xxxx-xx-xx'))) in plot_trend function fixes x-axis scale
reff_plotting(  
  refitted_model,
  dir = "outputs",
  subdir = "figures/six_month/no_nowcast",
  min_date = NA,
  max_date = refitted_model$data$dates$latest_infection,
  sims = sims,
  mobility_extrapolation_rectangle = FALSE
)

# # projection plots 
# reff_plotting(
#   refitted_model,  
#   dir = "outputs/projection",
#   max_date = refitted_model$data$dates$latest_project,
#   mobility_extrapolation_rectangle = FALSE,
#   projection_date = refitted_model$data$dates$latest_mobility,
#   sims = sims
# )
# 
# # 6-month projection plots
# reff_plotting(
#   refitted_model,
#   dir = "outputs/projection",
#   subdir = "figures/six_month",
#   min_date = NA,
#   max_date = refitted_model$data$dates$latest_project,
#   mobility_extrapolation_rectangle = FALSE,
#   projection_date = refitted_model$data$dates$latest_mobility,
#   sims = sims
# )


# produce simulations where proportion of variant is constant
#simulate_variant(variant = "wt")
#simulate_variant(variant = "alpha")
#simulate_variant(variant = "delta")
#simulate_variant(variant = "omicron")
# 
# simulate_variant(variant = "alpha", subdir = "alpha/ratio", ratio_samples = TRUE)
# simulate_variant(variant = "delta", subdir = "delta/ratio", ratio_samples = TRUE)
# simulate_variant(variant = "omicron", subdir = "omicron/ratio", ratio_samples = TRUE)



#simulate variant with vax effect

# simulate_variant(
#   variant = "omicron",
#   subdir = "omicron_vax",
#   vax_effect = vaccine_effect_timeseries %>% 
#     filter(variant == "Omicron", 
#            date <= max(fitted_model$data$dates$infection_project)) %>% 
#     select(-variant,-percent_reduction)
# )
# 
# simulate_variant(
#   variant = "omicron",
#   subdir = "omicron_vax",
#   vax_effect = vaccine_effect_timeseries %>% 
#     filter(variant == "Omicron BA2", 
#            date>=as_date("2021-06-01"),
#            date <= max(refitted_model$data$dates$infection_project)) %>% 
#     select(-variant,-percent_reduction)
# )
# 
# 
# simulate_variant(
#   variant = "delta",
#   subdir = "delta_vax",
#   vax_effect = vaccine_effect_timeseries %>% 
#     filter(variant == "Delta", 
#            date>=as_date("2021-06-01"),
#            date <= max(refitted_model$data$dates$infection_project)) %>% 
#     select(-variant,-percent_reduction)
# )
# 
# 
# simulate_variant(
#   variant = "omicron",
#   subdir = "omicron_BA4_vax",
#   vax_effect = vaccine_effect_timeseries %>% 
#     filter(variant == "Omicron BA4/5", 
#            date>=as_date("2021-06-01"),
#            date <= max(refitted_model$data$dates$infection_project)) %>% 
#     select(-variant,-percent_reduction)
# )
# 
# 
# 
# #simulate variant with combined immunity effect
# 
# combined_effect_timeseries_full <- readRDS("outputs/combined_effect_full.RDS")
# 
# # simulate_variant(
# #   variant = "omicron",
# #   subdir = "omicron_combined/",
# #   vax_effect = combined_effect_timeseries_full %>% 
# #     filter(
# #       variant == "Omicron", 
# #       date <= max(fitted_model$data$dates$infection_project),
# #       ascertainment == 0.5
# #     ) %>% 
# #     select(-variant,-percent_reduction, -ascertainment)
# # )
# 
# simulate_variant(
#   variant = "omicron",
#   subdir = "omicron_combined/",
#   vax_effect = combined_effect_timeseries_full %>% 
#     filter(
#       variant == "Omicron BA2", 
#       date>=as_date("2021-06-01"),
#       date <= max(fitted_model$data$dates$infection_project),
#       ascertainment == 0.5
#     ) %>% 
#     select(-variant,-percent_reduction, -ascertainment)
# )
# 
# # simulate_variant(
# #   variant = "omicron",
# #   subdir = "omicron_BA4.5_combined/",
# #   vax_effect = combined_effect_timeseries_full %>% 
# #     filter(
# #       variant == "Omicron BA4/5", 
# #       date>=as_date("2021-06-01"),
# #       date <= max(fitted_model$data$dates$infection_project),
# # 
# #       ascertainment == 0.5
# #     ) %>% 
# #     select(-variant,-percent_reduction, -ascertainment)
# # )
# 
# 
# 
# simulate_variant(
#   variant = "delta",
#   subdir = "delta_combined",
#   vax_effect = combined_effect_timeseries_full %>% 
#     filter(
#       variant == "Delta", 
#       date>=as_date("2021-06-01"),
#       date <= max(refitted_model$data$dates$infection_project),
#       ascertainment == 0.5
#     ) %>% 
#     select(-variant,-percent_reduction, -ascertainment)
# )
# 
# simulate_variant(
#   variant = "omicron",
#   subdir = "omicron_BA4_combined/",
#   vax_effect = combined_effect_timeseries_full %>% 
#     filter(
#       variant == "Omicron BA4/5", 
#       date>=as_date("2021-06-01"),
#       date <= max(refitted_model$data$dates$infection_project),
#       ascertainment == 0.5
#     ) %>% 
#     select(-variant,-percent_reduction, -ascertainment)
# )
# 
# source("R/omicron_delta_combined_compare.R")
# 
# #   plot the new 3-way TP comparison (no vax vs vax vs hybrid)
# no_infection_immunity_c1 <- read_csv(paste0("outputs/projection/r_eff_1_local_samples.csv"),
#                                      col_types =cols(
#                                        .default = col_double(),
#                                        date = col_date(format = ""),
#                                        state = col_character(),
#                                        date_onset = col_date(format = "")
#                                      )) 
# 
# no_vax_or_infection_immunity_c1 <- read_csv(paste0("outputs/projection/r_eff_1_local_without_vaccine_samples.csv"),
#                                             col_types =cols(
#                                               .default = col_double(),
#                                               date = col_date(format = ""),
#                                               state = col_character(),
#                                               date_onset = col_date(format = "")
#                                             )) 
# 
# 
# #ba2 vs ba4
# BA2_TP <- read_csv(paste0("outputs/projection/omicron_combined/r_eff_1_local_samples.csv"),
#                    col_types =cols(
#                      .default = col_double(),
#                      date = col_date(format = ""),
#                      state = col_character(),
#                      date_onset = col_date(format = "")
#                    )) 
# 
# BA4_TP <- read_csv(paste0("outputs/projection/omicron_BA4_combined/r_eff_1_local_samples.csv"),
#                    col_types =cols(
#                      .default = col_double(),
#                      date = col_date(format = ""),
#                      state = col_character(),
#                      date_onset = col_date(format = "")
#                    )) 
# 
# 
# 
# 
# #plot 
# start.date <- ymd("2021-06-01")
# end.date <- Sys.Date()
# vacc.start <- ymd("2021-02-22")
# date.label.format <- "%b %y"
# n.week.labels.panel <- 2
# n.week.ticks <- "1 month"
# 
# # Create date objects for ticks/labels (e.g., show ticks every n.week.ticks, but label every n.week.labels.panel)
# dd <- format(seq.Date(ymd(start.date), end.date, by=n.week.ticks), date.label.format)
# dd.labs <- as.character(dd)
# dd.labs[!(dd.labs %in% dd.labs[(seq(length(dd.labs),1,by=-n.week.labels.panel))])] <- ""
# dd.labs <- gsub(pattern = "^0", replacement = "", x = dd.labs)
# dd.labs <- gsub(pattern = "/0", replacement = "/", x = dd.labs)
# dd <- seq.Date(ymd(start.date), end.date, by=n.week.ticks)
# 
# # Quantiles
# qs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
# 
# r1 <- BA2_TP %>% 
#   reshape2::melt(id.vars = c("date","state","date_onset")) %>%
#   group_by(date,state) %>% 
#   summarise(x = quantile(value, qs), q = qs) %>% 
#   reshape2::dcast(state+date ~ q, value.var = "x") %>%
#   rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95") %>% filter(date <= end.date)
# 
# 
# r2 <- BA4_TP %>% 
#   reshape2::melt(id.vars = c("date","state","date_onset")) %>%
#   group_by(date,state) %>% 
#   summarise(x = quantile(value, qs), q = qs) %>% 
#   reshape2::dcast(state+date ~ q, value.var = "x") %>%
#   rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95")  %>% filter(date <= end.date)
# 
# r2.post <- r2 %>% filter(date >= vacc.start)
# 
# 
# # Plot aesthetics
# outer.alpha <- 0.15
# inner.alpha <- 0.4
# line.alpha <- 0.8
# col1 <- "grey70"
# col2 <- "grey50"
# col3 <- green
# 
# 
# subset.states <- c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")
# 
# ggplot() +
#   ggtitle(
#     label = "Transmission potentials of Omicron BA1/2 and BA4/5 subvariants",
#     subtitle = "With the effects of vaccination and the effect of immunity from infection with Omicron BA1/2 subvariant"
#   ) +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   
#   geom_vline(
#     aes(xintercept = date),
#     data = interventions() %>% filter(state %in% subset.states),
#     colour = "grey80"
#   ) +
#   
#   geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col1, alpha=outer.alpha) +
#   geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col1, alpha=inner.alpha) +
#   geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col1, alpha = line.alpha) +
#   geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col1, alpha = line.alpha) +
#   
#   geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col3, alpha=outer.alpha) +
#   geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col3, alpha=inner.alpha) +
#   geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col3, alpha = line.alpha) +
#   geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col3, alpha = line.alpha) +
#   # 
#   # geom_ribbon(data = r3.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col3, alpha=outer.alpha) +
#   # geom_ribbon(data = r3.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col3, alpha=inner.alpha) +
#   # geom_line(data = r3.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col3, alpha = line.alpha) +
#   # geom_line(data = r3.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col3, alpha = line.alpha) +
#   # geom_vline(
#   #   data = prop_variant_dates() %>% filter(state %in% subset.states),
#   #   aes(xintercept = date),
#   #   colour = "firebrick1",
#   #   linetype = 5
#   # ) +
# 
# geom_vline(xintercept = vacc.start, colour = "steelblue3", linetype = 5) +
#   
#   facet_wrap(~state, ncol = 2, scales = "free") +
#   
#   scale_y_continuous("", position = "right", breaks = seq(0,5,by=1)) +
#   scale_x_date("Date", breaks = dd, labels = dd.labs) +
#   # scale_x_date("Date", date_breaks = "1 month", date_labels = "%e/%m") +
#   
#   coord_cartesian(xlim = c(start.date, end.date),
#                   ylim = c(0, 5)) +
#   
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(legend.position = "right",
#         strip.background = element_blank(),
#         strip.text = element_text(hjust = 0, face = "bold"),
#         axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#         # axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text.x = element_text(size = 9),
#         panel.spacing = unit(1.2, "lines")) #+ 
#   # geom_vline(
#   #   data = prop_variant_dates(),
#   #   aes(xintercept = date),
#   #   colour = "firebrick1",
#   #   linetype = 5
#   # )
# 
# 
# ggsave(paste0("outputs/figures/full_tp_compare_",end.date,"_BA4.png"), height = 10, width = 9, bg = "white")
# 

# ##### for NSW TP no immunity 
# tp_novax <- read_reff_samples("outputs/projection/r_eff_1_local_without_vaccine_samples.csv")
# 
# 
# write_csv(
#   tp_novax %>%
#     filter(state == "NSW"),
#   file = paste0("outputs/nsw_tp_no_immunity_",data$dates$linelist,".csv")
# )

##### behaviour only multiplier

#calculate TP if only behaviour changed (Rt)

#get TP without immunity
TP_no_vax <- reff_1_without_vaccine(fitted_model, 
                                    vaccine_effect = as_tibble(fitted_model$data$vaccine_effect_matrix) %>%
                                             mutate(date = fitted_model$data$dates$infection_project) %>%
                                             pivot_longer(cols = -date, names_to = "state", values_to = "effect")
                                    )

TP_no_vax <- calculate(TP_no_vax, nsim = 10000, values = fitted_model$draws)
TP_no_vax <- apply(TP_no_vax[[1]],2:3,mean)
TP_no_vax <- TP_no_vax[1:length(fitted_model$data$dates$infection),]

#back out surveillance
surveillance_reff_local_reduction <- fitted_model$greta_arrays$surveillance_reff_local_reduction[1:length(fitted_model$data$dates$infection),]

R_t <- TP_no_vax/surveillance_reff_local_reduction

#calculate R0
p_star <- calculate(fitted_model$greta_arrays$distancing_effect$p_star,
                    nsim = 10000,
                    values = fitted_model$draws)
p_star <- apply(p_star[[1]],2:3,FUN="mean")

p_star <- p_star[1:length(fitted_model$data$dates$infection),]

household <- HC_0 * (1 - p_star ^ HD_0) 
non_household <- OC_0 * infectious_days * (1 - p_star ^ OD_0)
R0 <- household + non_household
unique(R0)


#ratio of Rt/R0
R_t_R0_ratio <- R_t/R0

distance_effect_multi <- tibble(value = c(R_t_R0_ratio),
                          state = rep(fitted_model$data$states, each = length(fitted_model$data$dates$infection)),
                          date = rep(fitted_model$data$dates$infection, fitted_model$data$n_states))

#plot
distance_effect_multi %>% ggplot(aes(x = date, y = value, col = state)) + 
  geom_vline(
    aes(xintercept = date),
    data = interventions(),
    colour = alpha("grey75",0.5)
  ) + 
  geom_line()  + facet_wrap(~ state, ncol = 2) + 
  theme_classic() +
  labs(
    x = NULL,
    y = "Multiplier",
    col = "State"
  )  +
  ggtitle(
    label = "Multiplicative reduction in transmission due to behavioral change"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    panel.spacing = unit(1.2, "lines")
  ) +
  scale_colour_manual(
    values = c(
      "darkgray",
      "cornflowerblue",
      "chocolate1",
      "violetred4",
      "red1",
      "darkgreen",
      "darkblue",
      "gold1"
    )
  ) +
  #facet_wrap(~variant, ncol = 1) +
  geom_hline(
    aes(
      yintercept = 0
    ),
    linetype = "dotted"
  ) +    scale_x_date(date_breaks = "2 month", date_labels = "%m") +
  scale_linetype_manual(values = c(1,3)) +
  
  xlab(element_blank())

ggsave("outputs/figures/distancing_effect_multiplier.png",width = 13, height = 6)

write_csv(distance_effect_multi,file = "outputs/distancing_effect_multiplier.csv")

