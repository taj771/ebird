#**we clear the memory/workspace by using rm(list = ls()), before loading the Apollo library**
#-----------------------------------#
#Clear memory 
#-----------------------------------#
rm(list = ls())

#-----------------------------------#
# load packages
#-----------------------------------#
library(apollo)
library(tidyverse)

#-----------------------------------#
#Initialize code
#-----------------------------------#
apollo_initialise( )

#-----------------------------------#
#Set core controls
#-----------------------------------#
apollo_control = list(
  modelName ="output/mnl_1" ,
  modelDescr ="MNL model with ebird data " ,
  indivID = "observer_id",
  workInLogs = T)
#  nCores = 6)  

#-----------------------------------#
# load the .Rdata 
#-----------------------------------#

database = read_csv("./data/processed/ApolloData_nalt5.csv")

#-----------------------------------#
# Divide TC by 10 to for convergence
#-----------------------------------#
database <- database %>%
  mutate_at(vars(starts_with("tc_")), ~./ 10 ) %>%
  arrange(choice_id)

#-----------------------------------#
# Need to set to data.frame for apollo
#-----------------------------------#
database <- as.data.frame(database)

#-----------------------------------#
#Iterative coding for alternatives
#-----------------------------------#
J = length(unique(database$choice))

#-----------------------------------#
#loop-1 : creating ascs for sites (Define model parameters)
#-----------------------------------#
# No ascs for these models yet
#asc = list ()
#for ( j in 1:J ) asc[[ paste0("alt", j ) ]] = paste0("asc_", j)

#-----------------------------------#
#loop-2 : Set all of \ asc = 0 
#-----------------------------------#
apollo_beta = c(b_tc = 0,
                b_sr = 0,
                b_2nd_sr = 0,
                b_3rd_sr = 0,
                b_4th_sr = 0)
                

#-----------------------------------#
#Home is a fixed parameter 
#-----------------------------------#
apollo_fixed = c()

#-----------------------------------#
#apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"mnl_1",overwriteFixed=FALSE)

#-----------------------------------#
#Validation and preparing inputs
#-----------------------------------#
apollo_inputs=apollo_validateInputs()

#-----------------------------------#
#Apollo_probabilities function
#-----------------------------------#

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
#  J = 3 # need this if parallel (nCores > 1 in apollo_controls)
  
  V= list( ) 
  avail = list()
  
  # set up utilities for hotspots
  for ( j in 1:J ) {
    V[[ paste0('choice', j ) ]] = b_tc * get(paste0('tc_', j)) + b_sr * get(paste0("sr_", j)) +
      b_2nd_sr * get(paste0("sr_", j))*(month == 2) +
      b_3rd_sr * get(paste0("sr_", j))*(month == 3) +
      b_4th_sr * get(paste0("sr_", j))*(month == 4) 
    avail[[paste0("choice", j)]]=get(paste0("avail_", j))
  }
  
  
  ###mnl_settings
  mnl_settings = list(
    alternatives = setNames (1:J,  names(V)) ,
    avail = avail ,
    choiceVar = choice ,
    V = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual (recognizing the repeated choice nature of our data)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#-----------------------------------#
#Save the results 
#-----------------------------------#
apollo_modelOutput(model)


#-----------------------------------#
#TWTP & MWTP - species richenss
#-----------------------------------#

#TWTP by number of species
#replace appropriate parameter
#minimum = 1
#25th percentile = 25
#50th percentile = 85
#75th percentile = 122
#maximum = 268

library(pacman)
p_load(tidyverse, car, apollo, haven, sjmisc)

deltaMethod(model, "(b_sr*1 + b_2nd_sr*1 + b_3rd_sr*1 + b_4th_sr*1)/ -b_tc") 

#TWTP by quarter
#replace appropriate quarter
#here we used mean of n_species = 90

deltaMethod(model, "(b_sr*90 + b_4th_sr*90 )/ -b_tc") 

#MWTP
# calculate MWTP
# replace appropriate quarter
deltaMethod(model, "(b_4th_sr)/ -b_tc") 

#apollo_saveOutput(model)
