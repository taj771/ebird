#-------------------------------------------------------------------------------
# Title: model 6
# please run model 4 before run this as this use some output from thre
# Date: 9/29/2021

# Description: Graphs and summary statistics

#-------------------------------------------------------------------------------

# check unique hotspots within relevant trips(878)
df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")
df_num_hot <- df_pt_rel%>%
  group_by(locality_id)%>%
  tally()
df_num_species <- read_csv("./data/processed/n_species_loc.csv")
summary(df_num_species)
df_num_species_set <- df_num_hot%>%
  left_join(df_num_species,by =c("locality_id"))
summary(df_num_species_set)
write_csv(df_num_species_set, "data/processed/num_species_set.csv")

#-----------------------------------------------------------------------------
# graps 
# Number of species - Histogram
df_hot_loc <- read.csv("./data/processed/n_species_loc.csv")
summarise(df_hot_loc)
#number of species monts
df_hot_loc <- read.csv("./data/processed/n_species_monthly.csv")
# locations in choice set
df_modeling <- read.csv("./data/processed/df_modeling.csv")
library(dplyr)
df_hot_loc_set <- df_modeling%>%
  select(locality_id)
df_hot_loc_set = df_hot_loc_set%>%
  distinct(locality_id)
df_num_species <- df_hot_loc_set%>%
  left_join(df_hot_loc, by = c("locality_id"))
library(ggplot2)
library(hrbrthemes)
df_num_species %>%
  ggplot( aes(x=n_species)) +
  geom_histogram( binwidth=3, color="#e9ecef") +
  theme(plot.title = element_text(size=15)) + ggtitle("Number of Species") +
  labs(x="Number of species", y = "count")
#number of species monts - boxplot
df_hot_loc_month <- read.csv("./data/processed/n_species_monthly.csv")
df_num_species_month <- df_hot_loc_set%>%
  left_join(df_hot_loc_month,by = c("locality_id"))
boxplot(df_num_species_month$n_species ~ month, data = df_num_species_month )
#------------------------------------------------------------------------------
# graph of parameter estimations with different n_alt

df_trval_plot = read_csv("./data/processed/travel_cost_plot.csv")
df_species_rich_plot = read_csv("./data/processed/species_rich_plot.csv")
df_2nd_quar_plot = read_csv("./data/processed/1st_quar_sr_pot.csv")
df_3rd_quar_plot = read_csv("./data/processed/3rd_quar_sr_plot.csv")
df_4th_quar_plot = read_csv("./data/processed/4th_quar_sr_plot.csv")


p <- ggplot(data=df_trval_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_species_rich_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_2nd_quar_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_3rd_quar_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_4th_quar_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )
