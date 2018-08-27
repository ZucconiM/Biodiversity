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
  filter(!is.na(Occurrence)) %>% select(Family, ConservationStatus, ParkName, Occurrence)

aa <- species2 %>% count(c("ParkName", "SName", "Category", "ConservationStatus")) %>% 
  filter(!is.na("Abundance")) %>% filter(!is.na("Occurrence")) %>%  filter(!is.na("ConservationStatus")) %>% 
  filter(ConservationStatus == "Endangered") %>% filter(Category == "Mammal" | Category == "Bird"| Category == "Reptile")
  # filter(Order == "Carnivora" | Order == "Rodentia" | Order == "Soricomorpha" )

az <- species2 %>% count(c("ParkName", "SName", "Category", "ConservationStatus")) %>% 
  filter(!is.na("Abundance")) %>% filter(!is.na("Occurrence")) %>%  filter(!is.na("ConservationStatus")) %>% filter(Category == "Mammal" | Category == "Bird"| Category == "Reptile")
# filter(Order == "Carnivora" | Order == "Rodentia" | Order == "Soricomorpha" )

aa1<- aa %>% group_by(ParkName, Category, ConservationStatus) %>% dplyr::summarise(n = n_distinct(SName))
aa2<- aa %>% group_by(ParkName,ConservationStatus) %>% dplyr::summarise(n = n_distinct(SName))
aa3<- aa %>% group_by(ParkName, SName, ConservationStatus) %>% dplyr::summarise(n = n_distinct(SName))

######## Base de datos parques  ########
parks <-read.csv("~/Desktop/Diversidad/Diversidad/parks.csv")
colnames(parks) <- c("ParkCode","ParkName", "State", "Acres", "Latitude", "Longitude")

######## Base de datos combinadas FINALES #######
### Matriz aa1
tot <- full_join(parks,aa1,by="ParkName",copy=FALSE)
tot1$ParkName <- as.factor(tot1$ParkName)
tot1 <-tot %>% filter(!is.na(ConservationStatus)) %>% filter(!is.na(Category))
str(tot1)

#### Matríz aa2
tot2 <-full_join(parks,aa2,by="ParkName",copy=FALSE) 

###### Matriz aa3

tot3 <- full_join(parks,aa3,by="ParkName",copy=FALSE) %>% filter(!is.na(ConservationStatus)) 






lin <- lm(n ~ Acres, data= tot1)
summary(lin)




######## Plots ###########

### especies por área en peligro (Sería interesante poner los que no están en peligro, utilizando otrabase de datos, )
ggplot(tot3) + 
  geom_jitter(aes(Acres,n)) + geom_smooth(aes(Acres,n), method=lm, se=FALSE) +
  geom_point()
   theme(legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("")+ xlab("")+
  theme(axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(),
        legend.key=element_blank(), 
        legend.title=element_text(size=18, face= "bold"), 
        legend.text=element_text(size=18,face= "bold"),
        axis.title=element_text(size=15,face= "bold"),
        axis.text=element_text(size=15,face= "bold"))


# Parques con grandes categorías en estado de peligro
ggplot(tot1,aes(x = ParkName, y = n)) + 
  geom_col(aes(fill = Category))+
  scale_fill_manual(values = c(rep(c("orange", "firebrick", "darkgreen"))))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("")+ xlab("")+
  theme(axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(),
        legend.key=element_blank(), 
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        axis.title=element_text(size=13),
        axis.text=element_text(size=13))+
  scale_x_discrete(labels = c("Acadia NP","Arches NP","Badlands NP", "Big Bend NP","Biscayne NP", "Black Canyon NP", "Bryce Canyon NP","Canyonlands NP", 
                              "Capitol Reef NP",  "Carlsbad Caverns NP","Channel Islands NP", "Congaree NP", "Crater Lake NP", "Cuyahoga Valley NP", "Death Valley NP","Dry Tortugas NP ", "Everglades NP",
                              "Glacier Bay NP","Grand Canyon NP", "Grand Teton NP", "Great Basin NP", "Great Smoky Mountains NP", "Guadalupe Mountains NP", 
                              "Haleakala NP", "Hawaii Volcanoes NP", "Hot Springs NP", "Isle Royale NP", "Joshua Tree NP", "Katmai NP", "Kenai Fjords NP", "Lake Clark NP", 
                              "Lassen Volcanic NP", "Mammoth Cave NP", "Mesa Verde NP", "Mount Rainier NP",  "North Cascades NP", "Olympic NP", "Petrified Forest NP", "Pinnacles NP",  
                              "Redwood NP", "Rocky Mountain NP", "Saguaro NP", "Sequoia-Kings Canyon NPs", "Shenandoah NP",
                              "Theodore Roosevelt NP", "Voyageurs NP", "Wind Cave NP", "Wrangell-St Elias NP", "Yellowstone NP", "Yosemite NP", "Zion NP"))



ggplot(tot1, aes(x = Category, y = n)) +
  geom_boxplot()+
  scale_fill_manual(values = c(rep(c("orange", "firebrick", "darkgreen"))))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("")+ xlab("")+
  theme(axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(),
        legend.key=element_blank(), 
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        axis.title=element_text(size=13),
        axis.text=element_text(size=13))
+
  scale_x_discrete(labels = c("Acadia NP","Arches NP","Badlands NP", "Big Bend NP","Biscayne NP", "Black Canyon NP", "Bryce Canyon NP","Canyonlands NP", 
                              "Capitol Reef NP",  "Carlsbad Caverns NP","Channel Islands NP", "Congaree NP", "Crater Lake NP", "Cuyahoga Valley NP", "Death Valley NP","Dry Tortugas NP ", "Everglades NP",
                              "Glacier Bay NP","Grand Canyon NP", "Grand Teton NP", "Great Basin NP", "Great Smoky Mountains NP", "Guadalupe Mountains NP", 
                              "Haleakala NP", "Hawaii Volcanoes NP", "Hot Springs NP", "Isle Royale NP", "Joshua Tree NP", "Katmai NP", "Kenai Fjords NP", "Lake Clark NP", 
                              "Lassen Volcanic NP", "Mammoth Cave NP", "Mesa Verde NP", "Mount Rainier NP",  "North Cascades NP", "Olympic NP", "Petrified Forest NP", "Pinnacles NP",  
                              "Redwood NP", "Rocky Mountain NP", "Saguaro NP", "Sequoia-Kings Canyon NPs", "Shenandoah NP",
                              "Theodore Roosevelt NP", "Voyageurs NP", "Wind Cave NP", "Wrangell-St Elias NP", "Yellowstone NP", "Yosemite NP", "Zion NP"))

attach(species2)

levels(species2$Category)
#seleccionarlos por variables de interés