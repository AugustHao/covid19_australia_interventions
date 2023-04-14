# get list of test type breakdown
new_infections_by_test_type <- linelist %>%
  mutate(cases = 1) %>% 
  group_by(state, date_onset, test_type) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() 




local_cases <- tibble::tibble(
  date_onset = rep(data$dates$onset, data$n_states),
  completion_probability = as.vector(data$completion_prob_mat),
  state = rep(data$states, each = data$n_dates),
  count = as.vector(data$local$cases_infectious),
  acquired_in_state = as.vector(data$local$cases),
  is_included = as.vector(data$valid_mat)
) 

local_cases <- local_cases %>% left_join(new_infections_by_test_type,
                                         by = c("date_onset" = "date_onset", "state" = "state"))


lc_long <- local_cases %>% na.omit() %>% 
  filter(date_onset >(Sys.Date()-months(2))) %>%
  filter(completion_probability > 0.1) %>%
  select(-acquired_in_state) %>%
  mutate(projected_count = count/completion_probability) %>%
  group_by(state, date_onset) %>%
  mutate(proj = projected_count - count) %>%
  select(-projected_count) %>%
  pivot_longer(cols = c("count", "proj"), names_to = "type", values_to = "count")

lc_long%>%
  filter(completion_probability>=0.8,completion_probability<=1) ->check_lc_long

manual_cutoff_line <- lc_long %>%
  filter(type == "count") %>%
  filter(is_included == TRUE) %>%
  group_by(state) %>%
  filter(date_onset == max(date_onset)) %>%
  select(state,date_onset)

prob_line_95 <- lc_long %>%
  filter(type == "count") %>%
  filter(completion_probability >= 0.95) %>%
  group_by(state) %>%
  filter(completion_probability == min(completion_probability)) %>%
  select(state,date_onset)

prob_line_90 <- lc_long %>%
  filter(type == "count") %>%
  filter(completion_probability >= 0.9) %>%
  group_by(state) %>%
  filter(completion_probability == min(completion_probability)) %>%
  select(state,date_onset)

lc_long <- lc_long %>% 
  mutate(type = case_when(type == "count" & test_type == "PCR" ~ "count PCR",
                          type == "count" & test_type == "RAT" ~ "count RAT",
                          type == "count" & test_type == "Total" ~ "count total",
                          TRUE ~ type)) %>% 
  mutate(count = case_when(type %in% c("count PCR","count RAT","count total") ~ cases,
                           TRUE ~ count)) %>% #remove duplicate proj column
  filter(!(type == "proj" & test_type %in% c("RAT","Total"))) #all test type are the same

lc_long %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_onset,
      y = count,
      fill = type
    ),
    stat = "identity"
  ) +
  geom_vline(
    data = prob_line_95,
    aes(
      xintercept = date_onset
    ),
    col = "black"
  ) +
  geom_vline(
    data = manual_cutoff_line,
    aes(
      xintercept = date_onset
    ),
    col = "purple"
  ) +
  facet_wrap(
    facets = vars(state),
    ncol = 2,
    scales = "free_y"
  )

ggsave("outputs/figures/watermelon_completion_with_manual_cutoff.png", bg = 'white',height = 5,width = 9)
