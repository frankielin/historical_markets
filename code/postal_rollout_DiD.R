############################
## Setting up Environment ##
############################
rm(list = ls())

## Loading Packages
library(lfe)
library(data.table)
library(stargazer)
library(did)
library(ggplot2)

## Setting File Location 
data_location <- "../data/cleaned/"

##################
## Loading Data ##
##################
## Load postal panel data
postal_panel_dat <- fread(paste0(data_location,"./data/cleaned/postal_rollout_panel.csv"))
print(colnames(postal_panel_dat))

## Loading the rolled postal data
## This data is the counties where there is already postal service
npostal_panel_dat <- fread(paste0(data_location,"./data/cleaned/non_postal_rollout_panel.csv"))

###################
## Data Cleaning ##
###################
## Creating Variables of Analysis for Rolled Data
postal_panel_dat[, town_state := paste0(Town, State)]
postal_panel_dat[, town_state_id := as.integer(factor(town_state))]
postal_panel_dat[, years_since_shift := Year - Rollout_Year]

## Creating Variables of Analysis for Non-Rolled Data
npostal_panel_dat[, town_state := paste0(Town, State)]
npostal_panel_dat[, town_state_id := as.integer(factor(town_state))]
npostal_panel_dat[, Rollout_Year := 0]

## Stacked Data (For analyzing )
all_panel_dat <- rbind(postal_panel_dat, npostal_panel_dat, fill = TRUE)
all_panel_dat[, town_state_id := as.integer(factor(town_state))]


#######################################
## Callaway-Sant'anna Regression Set ##
#######################################
#### Using the 
## Using the permantly treated as the control
first_roll = 1896
last_roll = 1903
reg_dat <- all_panel_dat[(Year >= (first_roll-2)) & (Year <= (last_roll+5))]

# ## Desperate Fixes
# reg_dat[, Rollout_Year := as.integer(Rollout_Year)]        # numeric
# reg_dat[is.na(Rollout_Year), Rollout_Year := 0L]   
# reg_dat[, has_patient := as.numeric(has_patient)]


# Summary of ATT by cohort and time
table(is.na(reg_dat$has_patient))
table(is.na(reg_dat$Year))
table(is.na(reg_dat$town_state_id))
table(is.na(reg_dat$Rollout_Year))

table(reg_dat$has_patient)
table(reg_dat$Year)
table(reg_dat$town_state_id)
table(reg_dat$Rollout_Year)

gt_out <- att_gt(
  yname = "has_patient", 
  tname = "Year",             
  idname = "town_state_id",   
  gname = "Rollout_Year",
  allow_unbalanced_panel = FALSE,
  control_group = "nevertreated",
  est_method = "reg",
  data = reg_dat
)

summary(gt_out)


# Aggregate dynamic (event-study) effects
event_study <- aggte(gt_out, type = "dynamic")
summary(event_study)

ggdid(event_study) + 
  labs(x = "Event Time (Years relative to treatment)", y = "ATT") +
  theme_minimal()

#############################
## Callaway & Sant'Anna DiD ##
#############################
# reg_min_frame <- -10
# reg_max_frame <- 10
# 
# reg_dat <- postal_panel_dat[
#   years_since_shift >= reg_min_frame & years_since_shift <= reg_max_frame
# ]

reg_min_frame <- -10
reg_max_frame <- 10

reg_dat <- postal_panel_dat[Year >= 1876 & Year <= 1910]


# Run ATT estimator using not-yet-treated as controls
cs_att <- att_gt(
  yname = "has_patient",          # outcome
  tname = "Year",                # time variable
  idname = "town_state_id",               # unit identifier
  gname = "Rollout_Year",        # treatment adoption year
  data = reg_dat,
  control_group = "notyettreated"  # simplest method
)

# Summary of ATT by cohort and time
summary(cs_att)


# Aggregate dynamic (event-study) effects
event_study <- aggte(cs_att, type = "dynamic")
summary(event_study)



#########################
## Regression Analysis ##
#########################
# 1) Create the subset for the event window
reg_min_frame <- -10
reg_max_frame <- 10

reg_dat <- postal_panel_dat[
  years_since_shift >= reg_min_frame & years_since_shift <= reg_max_frame
]

# 2) Manually create dummies for each lead/lag
reg_dat[, change_tm10 := as.integer(years_since_shift == -10)]
reg_dat[, change_tm9  := as.integer(years_since_shift == -9)]
reg_dat[, change_tm8  := as.integer(years_since_shift == -8)]
reg_dat[, change_tm7  := as.integer(years_since_shift == -7)]
reg_dat[, change_tm6  := as.integer(years_since_shift == -6)]
reg_dat[, change_tm5  := as.integer(years_since_shift == -5)]
reg_dat[, change_tm4  := as.integer(years_since_shift == -4)]
reg_dat[, change_tm3  := as.integer(years_since_shift == -3)]
reg_dat[, change_tm2  := as.integer(years_since_shift == -2)]
reg_dat[, change_tm1  := as.integer(years_since_shift == -1)]

reg_dat[, change_tp0  := as.integer(years_since_shift == 0)]
reg_dat[, change_tp1  := as.integer(years_since_shift == 1)]
reg_dat[, change_tp2  := as.integer(years_since_shift == 2)]
reg_dat[, change_tp3  := as.integer(years_since_shift == 3)]
reg_dat[, change_tp4  := as.integer(years_since_shift == 4)]
reg_dat[, change_tp5  := as.integer(years_since_shift == 5)]
reg_dat[, change_tp6  := as.integer(years_since_shift == 6)]
reg_dat[, change_tp7  := as.integer(years_since_shift == 7)]
reg_dat[, change_tp8  := as.integer(years_since_shift == 8)]
reg_dat[, change_tp9  := as.integer(years_since_shift == 9)]
reg_dat[, change_tp10 := as.integer(years_since_shift == 10)]




event_dummies <- c(
  "change_tm10","change_tm9","change_tm8","change_tm7","change_tm6","change_tm5",
  "change_tm4","change_tm3",
  "change_tp0","change_tp1","change_tp2","change_tp3","change_tp4","change_tp5",
  "change_tp6","change_tp7","change_tp8","change_tp9","change_tp10"
)


out <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~1,
  data = mpdta,
  est_method = "reg"
)







# # Build formula
# rhs <- paste(event_dummies, collapse = " + ")
# fmla <- as.formula(paste("has_patient~", rhs, "| town_state + Year"))
# 
# # Run the regression
# diff_in_diff_model <- felm(fmla, data = reg_dat)
# 
# # Summary
# summary(diff_in_diff_model)



# # Example: drop all town_state that have any observation in 1886-1889
# years_to_exclude <- 1886:1890
# 
# # Find town_state to remove
# cities_to_drop <- unique(reg_dat[Year %in% years_to_exclude, town_state])
# 
# # Filter them out
# reg_dat <- reg_dat[!town_state %in% cities_to_drop]

# Check
reg_dat[, .N, by = Year]  # number of observations per year after filtering

## The issue must be that there exists a country where there is only one year for like the first guy or last guy problems 
# Count unique cities per year
unique_cities_per_year <- reg_dat[, .(num_cities = uniqueN(town_state)), by = Year]

# View result
print(unique_cities_per_year)


# Define the full formula with all event dummies (except the baseline, e.g., change_tm1)
fmla <- as.formula(
  "N_patients ~ change_tm10 + change_tm9 + change_tm8 + change_tm7 + change_tm6 + change_tm5 +
   change_tm4 + change_tm3 +
   change_tp0 + change_tp1 + change_tp2 + change_tp3 + change_tp4 + change_tp5 +
   change_tp6 + change_tp7 + change_tp8 + change_tp9 + change_tp10 | town_state + Year")

# Run the regression
model <- felm(fmla, data = reg_dat)

# Summary (felm will automatically drop any collinear dummies and tell you which ones)
summary(model)
############################
## Setting up Environment ##
############################
rm(list = ls())

## Loading Packages
library(lfe)
library(data.table)
library(stargazer)

## Setting File Location 
data_location <- "./Desktop/historical_markets/"

##################
## Loading Data ##
##################
# Load postal panel data
postal_panel_dat <- fread(paste0(data_location,"./data/cleaned/postal_rollout_panel.csv"))
print(colnames(postal_panel_dat))


###################
## Data Cleaning ##
###################
## Creating Variables of Analysis
postal_panel_dat[, town_state := paste0(Town, State)]
postal_panel_dat[, years_since_shift := Year - Rollout_Year]


#########################
## Regression Analysis ##
#########################
# 1) Create the subset for the event window
reg_min_frame <- -10
reg_max_frame <- 10

reg_dat <- postal_panel_dat[
  years_since_shift >= reg_min_frame & years_since_shift <= reg_max_frame
]

# 2) Manually create dummies for each lead/lag
reg_dat[, change_tm10 := as.integer(years_since_shift == -10)]
reg_dat[, change_tm9  := as.integer(years_since_shift == -9)]
reg_dat[, change_tm8  := as.integer(years_since_shift == -8)]
reg_dat[, change_tm7  := as.integer(years_since_shift == -7)]
reg_dat[, change_tm6  := as.integer(years_since_shift == -6)]
reg_dat[, change_tm5  := as.integer(years_since_shift == -5)]
reg_dat[, change_tm4  := as.integer(years_since_shift == -4)]
reg_dat[, change_tm3  := as.integer(years_since_shift == -3)]
reg_dat[, change_tm2  := as.integer(years_since_shift == -2)]
reg_dat[, change_tm1  := as.integer(years_since_shift == -1)]

reg_dat[, change_tp0  := as.integer(years_since_shift == 0)]
reg_dat[, change_tp1  := as.integer(years_since_shift == 1)]
reg_dat[, change_tp2  := as.integer(years_since_shift == 2)]
reg_dat[, change_tp3  := as.integer(years_since_shift == 3)]
reg_dat[, change_tp4  := as.integer(years_since_shift == 4)]
reg_dat[, change_tp5  := as.integer(years_since_shift == 5)]
reg_dat[, change_tp6  := as.integer(years_since_shift == 6)]
reg_dat[, change_tp7  := as.integer(years_since_shift == 7)]
reg_dat[, change_tp8  := as.integer(years_since_shift == 8)]
reg_dat[, change_tp9  := as.integer(years_since_shift == 9)]
reg_dat[, change_tp10 := as.integer(years_since_shift == 10)]




event_dummies <- c(
  "change_tm10","change_tm9","change_tm8","change_tm7","change_tm6","change_tm5",
  "change_tm4","change_tm3",
  "change_tp0","change_tp1","change_tp2","change_tp3","change_tp4","change_tp5",
  "change_tp6","change_tp7","change_tp8","change_tp9","change_tp10"
)



# # Build formula
# rhs <- paste(event_dummies, collapse = " + ")
# fmla <- as.formula(paste("has_patient~", rhs, "| town_state + Year"))
# 
# # Run the regression
# diff_in_diff_model <- felm(fmla, data = reg_dat)
# 
# # Summary
# summary(diff_in_diff_model)



# # Example: drop all town_state that have any observation in 1886-1889
# years_to_exclude <- 1886:1890
# 
# # Find town_state to remove
# cities_to_drop <- unique(reg_dat[Year %in% years_to_exclude, town_state])
# 
# # Filter them out
# reg_dat <- reg_dat[!town_state %in% cities_to_drop]

# Check
reg_dat[, .N, by = Year]  # number of observations per year after filtering

## The issue must be that there exists a country where there is only one year for like the first guy or last guy problems 
# Count unique cities per year
unique_cities_per_year <- reg_dat[, .(num_cities = uniqueN(town_state)), by = Year]

# View result
print(unique_cities_per_year)


# Define the full formula with all event dummies (except the baseline, e.g., change_tm1)
fmla <- as.formula(
  "N_patients ~ change_tm10 + change_tm9 + change_tm8 + change_tm7 + change_tm6 + change_tm5 +
   change_tm4 + change_tm3 +
   change_tp0 + change_tp1 + change_tp2 + change_tp3 + change_tp4 + change_tp5 +
   change_tp6 + change_tp7 + change_tp8 + change_tp9 + change_tp10 | town_state + Year")

# Run the regression
model <- felm(fmla, data = reg_dat)

# Summary (felm will automatically drop any collinear dummies and tell you which ones)
summary(model)


## The issue is that at a certain point, everyone is treated and therefore there is a multi colinearity issue where there is 
## No variaiton to capiture the effect that we are looking for? I think? Let me  






