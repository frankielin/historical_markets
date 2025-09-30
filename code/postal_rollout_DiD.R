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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################
## Loading Data ##
##################
## Load postal panel data
postal_panel_dat <- fread(paste0("../data/clean/postal_rollout_panel.csv"))
print(colnames(postal_panel_dat))

## Loading the rolled postal data
## This data is the counties where there is already postal service
npostal_panel_dat <- fread(paste0("../data/clean/non_postal_rollout_panel.csv"))


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

## Artificially Dropping 50% of towns with zero to mess with the standard errors
## This emulates the decreasing the power
## Step 1: find state-city combos with all_zero patients in 1835–1910
eligible_towns <- npostal_panel_dat[
  Year >= 1835 & Year <= 1896,
  .(all_zero = all(has_patient == 0)),
  by = .(State, Town)
][all_zero == TRUE, .(State, Town)]

## Step 2: randomly select ~75% of those combos
set.seed(777)  # reproducible
drop_towns <- eligible_towns[
  sample(.N, size = floor(0.75 * .N))
]

## Step 3: drop *all rows* from those combos
npostal_panel_dat <- npostal_panel_dat[!drop_towns, on = .(State, Town)]

## Stacked Data (For analyzing )
all_panel_dat <- rbind(postal_panel_dat, npostal_panel_dat, fill = TRUE)
all_panel_dat[, town_state_id := as.integer(factor(town_state))]


#########################
## Callaway-Sant'anna  ##
#########################
#### Raw Callaway Sant'anna
## Using the permantly treated as the control
first_roll = 1896
last_roll = 1903
reg_dat <- all_panel_dat[(Year >= (first_roll-10)) & (Year <= (last_roll+10))]

## Show the distribution of number of cities that this is rolled out to per year
rolled_out_count = reg_dat[Rollout_Date<= Year, .(unique_ids = uniqueN(town_state_id)), by = Rollout_Year]
rolled_out_count
table(reg_dat[,Year])

## Dropping the small sample data
# bad_towns <- unique(reg_dat[Rollout_Year %in% c(1896, 1897, 1898, 1899), town_state_id])
# reg_dat <- reg_dat[!town_state_id %in% bad_towns]

## Running CSDiD
# RHS_desc = "Number of Patents"
# RHS_var  = "N_patients"

RHS_desc = "Probability of Having a Patient"
RHS_var  = "has_patient"

gt_out <- att_gt(
  yname = RHS_var, 
  tname = "Year",             
  idname = "town_state_id",   
  gname = "Rollout_Year",
  allow_unbalanced_panel = TRUE,
  control_group = "nevertreated",
  est_method = "reg",
  data = reg_dat
)

summary(gt_out)


## Dynamic Analysis
event_study <- aggte(gt_out, type = "dynamic")
summary(event_study)

ggdid(event_study) + 
  labs(x = "Event Time (Years Relative to Postal Service Access)", y = "ATT") +
  theme_minimal()

## Group Analysis
## Earlier groups show less power (effect)
## This is expected as these are typically smaller groups
group_att <- aggte(gt_out, type = "group")
summary(group_att)

## Calendar Year Analysis
## Earlier groups show less power (effect)
## This is expected as these are typically smaller groups
calendar_att <- aggte(gt_out, type = "calendar")
summary(calendar_att)

## Overall Effect
## Overall effect calculates across the entire sample
overall_att <- aggte(gt_out, type = "simple") 
summary(overall_att)

##########################
## Creating Event Study ##
##########################
#### Use the estimates from t +/- 9 to to create event study  
## Keep only event times from -9 to 9
keep_idx <- which(event_study$egt >= -9 & event_study$egt <= 9)

## Create a truncated event-study object
event_study_trunc <- event_study
event_study_trunc$egt       <- event_study$egt[keep_idx]
event_study_trunc$att.egt   <- event_study$att.egt[keep_idx]
event_study_trunc$se.egt    <- event_study$se.egt[keep_idx]
event_study_trunc$att.agg   <- event_study$att.agg    
event_study_trunc$se.agg    <- event_study$se.agg

## Event Study
event_study_plot <- 
  ggdid(event_study_trunc) +
  ggtitle(NULL) +
  labs(
    x = "Event Time (Years Relative to Postal Service Access)",
    y = RHS_desc
  ) +
  theme_minimal() +
  theme(legend.position = "none")

event_study_plot

ggsave(paste0("../figures/CSDiD_event_study_",RHS_var,".pdf"), 
       plot = event_study_plot, 
       width = 6, 
       height = 4)


#### Compute cohort-weighted net ATT
## Create variable storing the 2x2 estimates and SEs
gt_dt <- data.table(
  g           = gt_out$group,      # cohort
  time        = gt_out$t,        # relative to treatment
  att         = gt_out$att,        # ATT estimate
  se          = gt_out$se          # standard error
)
gt_dt[, time_to_treat := time - g ] 

## Merging the cohort count per each estimated effect 
gt_dt <- merge( 
  gt_dt,
  rolled_out_count,
  by.x = "g",       
  by.y = "Rollout_Year",  
  all.x = TRUE
)
setnames(gt_dt, "unique_ids", "n_cohort")

## Compute weighted average ATT across all periods and cohorts
manual_post_ATT <- sum(gt_dt[time_to_treat >= 0 & time_to_treat <= 9]$att * gt_dt[time_to_treat >= 0  & time_to_treat <= 9]$n_cohort) /
  sum(gt_dt[time_to_treat >= 0  & time_to_treat <= 9]$n_cohort)

manual_post_ATT 

## Checking if the average effect across the entire time is the same as
## the automatic calculation function
check_post_ATT <- sum(gt_dt[time_to_treat >= 0]$att * gt_dt[time_to_treat >= 0]$n_cohort) /
  sum(gt_dt[time_to_treat >= 0]$n_cohort)

print(check_post_ATT)
overall_att$overall.att
