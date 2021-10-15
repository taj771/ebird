#Clear memory 
#-------------------------------------------------------------------------------
rm(list = ls())
# Import data
# Hot locations
df_hot_loc <- read.csv("./data/processed/num_species_set.csv")
# ab_all data with scientific name
df_ebd_ab_all = read.csv("./data/base/ebd_CA-AB_prv_relJan-2020_subset.csv")

df_ebd_ab_all <- df_ebd_ab_all%>%
  select(scientific_name,locality_id)%>%
  mutate(endang_code = case_when(scientific_name == "Strix varia"~1,
                                 scientific_name == "Dendroica castanea"~2,
                                 scientific_name == "Dendroica virens"~3,
                                 scientific_name == "Certhia americana"~4,
                                 scientific_name == "Athene cunicularia"~5,
                                 scientific_name == "Cardellina canadensis"~6,
                                 scientific_name == "Dendroica tigrina"~7,
                                 scientific_name == "Calcarius ornatus"~8,
                                 scientific_name == "Buteo regalis"~9,
                                 scientific_name == "Centrocercus urophasianus"~10,
                                 scientific_name == "Histrionicus histrionicus"~11,
                                 scientific_name == "Lanius ludovicianus"~12,
                                 scientific_name == "Numenius americanus"~13,
                                 scientific_name == "Charadrius montanus"~14,
                                 scientific_name == "Glaucidium gnoma"~15,
                                 scientific_name == "Falco peregrinus"~16,
                                 scientific_name == "Charadrius melodus"~17,
                                 scientific_name == "Falco mexicanus"~18,
                                 scientific_name == "Oreoscoptes montanus"~19,
                                 scientific_name == "Asio flammeus"~20,
                                 scientific_name == "Anthus spragueii"~21,
                                 scientific_name == "Cygnus buccinator"~22,
                                 scientific_name == "Cathartes aura"~23,
                                 scientific_name == "Aechmophorus occidentalis"~24,
                                 scientific_name == "Melanitta fusca"~25,
                                 scientific_name == "Grus americana"~26,
                                 scientific_name == "Empidonax traillii"~27))

#above include species at risk / sensetive/ endangered so create a dummy only 
# with endangered species
library(dplyr)
#df_ebd_ab_all <- df_ebd_ab_all%>%
  #mutate(end_bin = case_when(scientific_name == "Strix varia"~0,
                                 #scientific_name == "Dendroica castanea"~ 0,
                                 #scientific_name == "Dendroica virens"~ 0,
                                 #scientific_name == "Certhia americana"~ 0,
                                 #scientific_name == "Athene cunicularia"~ 1,
                                 #scientific_name == "Cardellina canadensis"~ 0,
                                 #scientific_name == "Dendroica tigrina"~ 0,
                                 #scientific_name == "Calcarius ornatus"~ 0,
                                 #scientific_name == "Buteo regalis"~ 1,
                                 #scientific_name == "Centrocercus urophasianus"~ 1,
                                 #scientific_name == "Histrionicus histrionicus"~ 0,
                                 #scientific_name == "Lanius ludovicianus"~ 0,
                                 #scientific_name == "Numenius americanus"~ 0,
                                 #scientific_name == "Charadrius montanus"~1,
                                 #scientific_name == "Glaucidium gnoma"~0,
                                 #scientific_name == "Falco peregrinus"~1,
                                 #scientific_name == "Charadrius melodus"~1,
                                 #scientific_name == "Falco mexicanus"~0,
                                 #scientific_name == "Oreoscoptes montanus"~0,
                                 #scientific_name == "Asio flammeus"~0,
                                 #scientific_name == "Anthus spragueii"~0,
                                 #scientific_name == "Cygnus buccinator"~0,
                                 #scientific_name == "Cathartes aura"~0,
                                 #scientific_name == "Aechmophorus occidentalis"~1,
                                 #scientific_name == "Melanitta fusca"~0,
                                 #scientific_name == "Grus americana"~1,
                                 #scientific_name == "Empidonax traillii"~0))


#remove hotspot without endangered species
#df_ebd_ab_all <- df_ebd_ab_all%>%
  #na.omit(df_ebd_ab_all)
df_ebd_ab_all <- df_ebd_ab_all%>%
  replace(is.na(.),0)

#get the unique endangered species per hospot -may need to active if # of enddan
# is under consideration
library(dplyr)
df_ebd_ab_all <- df_ebd_ab_all%>%
  distinct(locality_id,endang_code)

#Binary variable - endangered species
#df_ebd_ab_all <- df_ebd_ab_all%>%
  #distinct(locality_id,end_bin)

#get the total number of unique species per hotspot
library(plyr)
df_ebd_ab_all <- df_ebd_ab_all%>%
  select(locality_id,endang_code)%>%
  count("locality_id")

write.csv(df_ebd_ab_all, "data/processed/endangered_species.csv")



#filter end species details that limited to interested hotspot (878)
#df_endan_spe <- df_hot_loc %>%
  #left_join(df_ebd_ab_all, by = c("locality_id"))%>%
  #select(locality_id,freq)
#filter end species binary details that limited to interested hotspot (878)
#df_endan_spe <- df_hot_loc %>%
  #left_join(df_ebd_ab_all, by = c("locality_id"))%>%
  #select(locality_id,end_bin)
#replace for the hotpots where endangerd species are absent 
df_endan_spe <- df_endan_spe%>%
  replace(is.na(.),0)
#remove multiple rows with same location ID
df_endan_spe <- df_endan_spe%>%
  distinct(locality_id,end_bin == 1, .keep_all = TRUE)

df_endan_spe <- df_endan_spe%>%
  rename(tf = `end_bin == 1`)
df_endan_spe <- df_endan_spe%>%
  mutate(es_binary = case_when(tf == "TRUE"~1,
                               ))
df_endan_spe <- df_endan_spe%>%
  na.omit(df_endan_spe)%>%
  select(locality_id,es_binary)

df_endan_spe_bin <- df_hot_loc %>%
  left_join(df_endan_spe, by = c("locality_id"))%>%
  select(locality_id,es_binary)

df_endan_spe_bin <- df_endan_spe_bin%>%
  replace(is.na(.),0)%>%
  select(locality_id,es_binary)
#write csv with endangers species bianry
write.csv(df_endan_spe_bin, "data/processed/endangered_species_binary.csv")




# adding number of spe square term
df_uniq_spec <- read_csv("./data/processed/n_species_loc.csv")
df_uniq_spec <- df_uniq_spec %>%
  mutate(n_spe2 =n_species*n_species)
write.csv(df_uniq_spec, "data/processed/n_species_loc2.csv")

#creating n_endangered_sp/total_spe

df_endan_spe = read.csv("./data/processed/endangered_species.csv")
df_uniq_spec <- read_csv("./data/processed/n_species_loc.csv")

df_endan_spe_rat <- df_endan_spe %>%
  left_join(df_uniq_spec, by = "locality_id")%>%
  mutate(es_rat = freq/n_species)%>%
  select(locality_id,es_rat)

write.csv(df_endan_spe_rat, "data/processed/endangered_rat.csv")


