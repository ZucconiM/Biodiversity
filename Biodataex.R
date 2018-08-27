library(tidyverse)
library(lubridate)
library(Rmisc) 
library(psych)
library(plyr)
library(car)
library(dplyr)
attach(resum)

######## Base de datos especies ########
species2 <-read_csv("~/Desktop/Diversidad/Diversidad/species.csv")
colnames(species2) <- c("SpeciesID","ParkName", "Category", "Order", "Family", "SName", 
                        "CommonNames", "RecordStatus", "Occurrence", "Nativeness", "Abundance", 
                        "Seasonality", "ConservationStatus", "X14")
resum1 <- species2 %>% group_by(ParkName, SName) %>% filter(Order == "Carnivora" | Order == "Rodentia" | Order == "Soricomorpha" ) %>%  
  filter(!is.na(ConservationStatus)) %>% filter(!is.na(Abundance)) %>% 
  filter(!is.na(Occurrence)) %>% 
  select(Family, ConservationStatus, ParkName, Occurrence)
aa <- species2 %>% count(c("ParkName", "SName", "Occurrence", "Order")) %>% filter(!is.na("Abundance")) %>% 
  filter(!is.na("Occurrence"))
aa1<- aa %>% group_by(ParkName, Order) %>% dplyr::summarise(n = n_distinct(SName))

######## Base de datos parques  ########
parks <-read.csv("~/Desktop/Diversidad/Diversidad/parks.csv")
colnames(parks) <- c("ParkCode","ParkName", "State", "Acres", "Latitude", "Longitude")

######## Base de datos combinadas #######
tot <- full_join(parks,aa1,by="ParkName",copy=FALSE)
head(tot)








