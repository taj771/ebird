#-------------------------------------------------------------------------------
# Title: model 4
# As start we used n_alt = 5, when replicating make sure to ajust as appropriate
# Date: 9/29/2021

# Description: Make a data set comparable with Apollo

#-------------------------------------------------------------------------------
#Clear memory 
#-------------------------------------------------------------------------------
rm(list = ls())

# Load packages
library(tidyverse)
library(lubridate)
library(furrr)


#-------------------------------------------------------------------------------
# Select these variables

# Number of workers for parallel processing
n_workers = 4
consider_time = 1

# low/high end of stright line travel cost distance km
# The travel distance set as the 5km and 120km, Please change this range as it 
#needed 
travel_cutoff_lo = 5
travel_cutoff_hi = 120

travel_hours_lo = 5/60 # 5/60 is 5 minutes
travel_hours_hi = 2 #

# Choose number of non-chosen alternatives to sample
# originally set it for 5 make changes as it required
n_alts = 5

#-------------------------------------------------------------------------------
# Function to sample non-chosen consideration_sets for each person
# Works at the individual choice level
# choices: dataframe with one row with observer_id, locality_id, year, month
# consideration_sets: dataframe with consideration_sets for each observer_id
# n_alts: number of non-chosen alternatives to include
# 
# returns out: dataframe with full consideration set (chosen and n_alt non-chosen alternatives)
SampleAlternatives = function(choices, consideration_sets, n_alts){
  trip_other = choices %>%
    select(-choice, -locality_id)
  choices = choices %>%
    select(observer_id, locality_id, choice)
  # Return dataframe for each id
  df_alts = consideration_sets %>%
    filter(observer_id == choices$observer_id & 
             locality_id != choices$locality_id) %>% # insure non-chosen
    mutate(choice = 0)
  
  # Only sample if consideration sets less than n_alts
  if(n_alts < nrow(df_alts))
    df_alts = slice_sample(df_alts, n = n_alts) 
  
  out = bind_rows(choices, df_alts) %>%
    left_join(trip_other, by = c("observer_id"))
  
  return(out)
}
#-------------------------------------------------------------------------------

# Import data

# Person-trips
df_pt <- read_csv("./data/processed/ab-ebdusers-person-trips.csv")
# Travel costs
df_travel_costs <- read_csv("./data/processed/ab-ebd-travel-costs.csv")
# User postal codes
df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")
# Euclidean distances
df_euc_dist <- read_csv("./data/processed/pc-hotspot-euclidean-distances.csv")
# All hotspot locations
df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")
# Number of species monthly
df_hot_loc_n_species <- read_csv("./data/processed/n_species_monthly.csv")
# Number of species
df_uniq_spec <- read_csv("./data/processed/n_species_loc.csv")
#number of trips
df_num_trips <- read_csv("./data/processed/num_trips_indi.csv")

#-------------------------------------------------------------------------------
# Travel costs
# Not unique so keep only distinct values
df_costs <- df_travel_costs %>% 
  distinct(locality_id, postal_code, .keep_all = T) %>%
  select(locality_id, postal_code, cost_total, hours, km)
# create postal code-id table
df_pc_id = df_pc_sub %>%
  select(-longitude, -latitude)
# Retrieve relevant person-trips (have postal codes, between 5 - 120 km) 
#(total trips : 39,835)
df_pt_rel <- df_pt %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  # Filter out trips where we don't have postal code info; might want these later though.
  filter(!is.na(postal_code)) %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
  select(observer_id, observation_date, locality_id, locality) # missing Kanaskis travel costs. 


# Create dataframe with chosen trips
df_trip <- df_pt_rel %>%
  mutate(month = as.character(month(observation_date, label = TRUE)),
         year = year(observation_date)) %>%
  group_by(observer_id, month, year) %>%
  mutate(choice = 1,
         choice_occasion = seq(n())) %>%
  ungroup(.) %>%
  mutate(month = factor(month, levels = month.abb),
         year = factor(year)) %>%
  # Make sure we have travel cost for trip
  left_join(df_pc_id, by = "observer_id") %>%
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  filter(!is.na(cost_total)) %>%
  select(observer_id, locality_id, month, year, choice_occasion, choice)   

#------------------------------------------------------------------------------
# Create dataframe for consideration sets
# Vector of hotspots - 1,227 total.
locality_id <- df_hot_loc %>% pull(locality_id)

# Vector of unique users - 404 total.
observer_id <- df_pt_rel %>% select(observer_id) %>% distinct() %>% pull()

df_consideration_sets <- crossing(observer_id, locality_id) %>%
  left_join(df_pc_id, by = "observer_id") %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  # Remove missing cost data
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  filter(!is.na(cost_total))

# Only keep combos we're interested 
if(consider_time == 1){ #  by time
  df_consideration_sets <- df_consideration_sets %>%
    filter(hours <= travel_hours_hi & hours >= travel_hours_lo) %>%
    select(observer_id, locality_id)
  
} else if (consider_time == 0){ # by distance
  df_consideration_sets <- df_consideration_sets %>%
    filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
    select(observer_id, locality_id)
}

#-------------------------------------------------------------------------------
# Sample consideration sets
df_trip_list <- split(df_trip, seq(nrow(df_trip)))

plan(multisession, workers = n_workers)

df_modeling = future_map_dfr(df_trip_list, SampleAlternatives, 
                             consideration_sets = df_consideration_sets, 
                             n_alts = n_alts,
                             .options = furrr_options(seed = TRUE))
closeAllConnections()

# Export
write_csv(df_modeling, "./data/processed/df_modeling.csv")

#-------------------------------------------------------------------------------
# Add back travel costs
df_modeling = df_modeling %>%  
  left_join(df_pc_id, by = "observer_id") %>%
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  select(-postal_code)

# Should all be 1
df_modeling %>%
  group_by(observer_id, year, month, choice_occasion) %>%
  summarise(choice = sum(choice)) %>%
  group_by(choice) %>%
  tally()
#-------------------------------------------------------------------------------
df_modeling = df_modeling %>%
  left_join(df_uniq_spec, by = "locality_id")

#df_modeling = df_modeling %>%
#left_join(df_num_trips, by = "observer_id")

df_modeling = df_modeling%>%
  mutate(month = case_when(month == "Jan"~1,
                           month == "Feb"~1,
                           month == "Mar"~1,
                           month == "Apr"~2,
                           month == "May"~2,
                           month == "Jun"~2,
                           month == "Jul"~3,
                           month == "Aug"~3,
                           month == "Sep"~3,
                           month == "Oct"~4,
                           month == "Nov"~4,
                           month == "Dec"~4))

#-------------------------------------------------------------------------------
#Descriptive statistics of consideration set
#mean trvel time
mean(df_modeling$hours)
#SD of travel time
sd(df_modeling$hours)
#mean distance
mean(df_modeling$km)
#sd distance
sd(df_modeling$km)
#mean cost
mean(df_modeling$cost_total)
sd(df_modeling$cost_total)
#mean number of species
mean(df_hot_loc_n_species$n_species)
sd(df_hot_loc_n_species$n_species)

#-------------------------------------------------------------------------------
# Prepare data for apollo
df_modeling = df_modeling %>%  
  arrange(observer_id, month, year, choice_occasion, locality_id) %>%
  group_by(observer_id, year, month, choice_occasion)  %>% 
  mutate(choice_id = cur_group_id(),
         alt = seq(n()),
         choice = choice * alt) %>%
  ungroup(.)

# Create travel cost in wide format
df_tc = df_modeling %>%
  pivot_wider(choice_id, names_from = "alt", 
              names_prefix = "tc_",
              values_from = "cost_total")
#create num_species in wide format
df_species = df_modeling %>%
  pivot_wider(choice_id, names_from = "alt", 
              names_prefix = "sr_",
              values_from = "n_species")


# Need availability because some people do not have n_alts alternatives in consideration sets
df_avail =  df_modeling %>%
  pivot_wider(choice_id, names_from = "alt", 
              names_prefix = "avail_",
              values_from = "cost_total") %>%
  mutate_at(vars(contains('avail_')), ~(ifelse(is.na(.), 0, 1)))

summary(df_avail)


# return chosen alternative
df_choice = df_modeling %>%
  filter(choice > 0) %>%
  select(observer_id, choice_id, choice, month) 

df_apollo = df_choice %>%
  left_join(df_tc, by = "choice_id") %>%
  left_join(df_species, by = "choice_id")%>%
  left_join(df_avail, by = "choice_id")

#make changes to file name depend on n_alt used above
write_csv(df_apollo, "data/processed/ApolloData_nalt5.csv")



