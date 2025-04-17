# == == == == == == == == == == == == == == == 
# ==              Nicole Bonge              ==
# ==         Sunday, April 13, 2025         ==
# ==     Project Code - Tidy Tuesday        ==
# ==    Week 14: Timely and Effective Care  ==
# ==             by US State                ==
# == == == == == == == == == == == == == == ==

# 1.0 Preliminaries ----
rm(list=ls())
getwd()

## 1.1 Libraries ----
library(tidyverse)
library(psych)
library(tidytuesdayR)
library(fec16)
library(naniar)
library(sjmisc)

# 1.2 Functions ----


# 2.0 Import Dataset ----
care_state <- tt_load(2025,week=14)[[1]]
states <- fec16::states

# 3.0 Data Exploration ----
str(care_state)
unique(care_state$measure_name)

# 4.0 Visualization ----
care_state |> 
  filter(measure_id == "IMM_3") |>
  left_join(states, by = "state") |>
  arrange(state_region) |>
  ggplot(aes(x = as.factor(state), y = score, fill = state_region)) +
  geom_col() +
  facet_wrap(~state_region, scales = "free")


# 5.0 Missing ----
gg_miss_case(care_state_wide, facet = state)


# 6.0 Data Reshaping ----
care_state_wide <- care_state |>
  filter(!grepl(pattern = "MIN", x = care_state$measure_id)) |>
  pivot_wider(id_cols = state, names_from = measure_id, values_from = score) |>
  filter(!(state %in% c("AS", "GU", "VI", "MP"))) |>
  select(!"OP_31", "SEV_SEP_6HR")



# 7.0 Descriptives & Visualizations ----
cor <- cor(care_state_wide[,-c(1,4, 11:14)]) |> round(digits = 2)
cor.plot(cor)

# Dropping multicollinear variables
care_state2 <- care_state_wide |>
  select(!(ends_with("HR"))) |>
  select(!("OP_18b"))

describe(care_state2)

ggplot(care_state2, aes(x = HCP_COVID_19)) +
  geom_histogram()

ggplot(care_state2, aes(x = IMM_3)) +
  geom_histogram()

ggplot(care_state2, aes(x = OP_18c)) +
  geom_histogram()

ggplot(care_state2, aes(x = OP_22)) +
  geom_histogram(binwidth = 1)

ggplot(care_state2, aes(x = OP_23)) +
  geom_histogram()

ggplot(care_state2, aes(x = OP_29)) +
  geom_histogram()

ggplot(care_state2, aes(x = SAFE_USE_OF_OPIOIDS)) +
  geom_histogram()

ggplot(care_state2, aes(x = SEP_1)) +
  geom_histogram()

care_state3 <- care_state2 |> 
  mutate(safe_opioids = center(care_state2$SAFE_USE_OF_OPIOIDS)) |>
  select(!("SAFE_USE_OF_OPIOIDS")) |>
  rename(covid_shot_pct = HCP_COVID_19,
         flu_shot_pct = IMM_3,
         avg_time_ed_psych = OP_18c,
         pct_pts_left_ed = OP_22,
         pct_stroke_brain_scan = OP_23,
         pct_fu_colonosc = OP_29,
         pct_care_sepsis = SEP_1
         ) |>
  left_join(states, by = "state") |>
  filter(!state %in% c("DC", "PR")) |>
  select(!("state_name"))

# 8.0 PCA ----
cor <- cor(care_state3[,2:9]) |> round(digits = 2)
cor.plot(cor)

scree(cor)

nfactors(care_state2[,-c(1)])

pca(care_state3[,-c(1,10:11)], nfactors = 1)
pca(care_state3[,-c(1,10:11)], nfactors = 2)
pca(care_state3[,-c(1,10:11)], nfactors = 3)
pca(care_state3[,-c(1,10:11)], nfactors = 4)


# 9.0 Reshape ----
care_state_long <- care_state3 |>
  pivot_longer(cols = covid_shot_pct:safe_opioids, names_to = "measure",
               values_to = "score")
ggplot(care_state_wide3, aes(x = state_region, y = score)) +
  geom_boxplot() +
  facet_wrap(~measure, scale = "free") +
  coord_flip()
ggplot(care_state_long, aes(score, fill = state_region)) +
  geom_histogram() +
  facet_wrap(state_region~measure, scale = "free")


# 10.0 HLM ----
mod.0 <- 