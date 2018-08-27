library(tidyverse)
library(lubridate)
library(Rmisc) 
library(psych)
library(plyr)
library(car)
library(dplyr)
library(extrafont)
library(extrafontdb)
library(dismo)
library(knitr)
library(wesanderson)
######## Base de datos especies, cambio de nombre de columnas ########
species2 <-read_csv("~/Desktop/Diversidad/Diversidad/species.csv")
colnames(species2) <- c("SpeciesID","ParkName", "Category", "Order", "Family", "SName", 
                        "CommonNames", "RecordStatus", "Occurrence", "Nativeness", "Abundance", 
                        "Seasonality", "ConservationStatus", "X14")

####### Selecci??n de las variables y conteo de frecuencia para Mam??feros, Reptiles y Aves en peligro ############
aa <- species2 %>% count(c("ParkName", "SName", "Category", "ConservationStatus")) %>% 
  filter(!is.na("Abundance")) %>% filter(!is.na("Occurrence")) %>%  filter(!is.na("ConservationStatus")) %>% 
  filter(ConservationStatus == "Endangered") %>% filter(Category == "Mammal" | Category == "Bird"| Category == "Reptile" |Category == "Amphibian" | 
                                                          Category == "Fish"| Category == "Insect" | Category == "Invertebrate")


aaa <- species2 %>% count(c("ParkName", "SName", "Category", "ConservationStatus")) %>% 
  filter(!is.na("Abundance")) %>% filter(!is.na("Occurrence")) %>% filter(!is.na("ConservationStatus"))


kable(summary(tat1))

####### Conteo de Aves, Mam??feros y Reptiles en peligro por cada parque ##############
aa1<- aa %>% group_by(ParkName, Category, ConservationStatus) %>% dplyr::summarise(n = n_distinct(SName))
aaa1 <-aaa %>% group_by(ParkName, Category, ConservationStatus) %>% dplyr::summarise(n = n_distinct(SName)) %>% filter(!is.na("ConservationStatus"))

###### Base de datos con informaci??n extra de los parques ##########
parks <-read.csv("~/Desktop/Diversidad/Diversidad/parks.csv")
colnames(parks) <- c("ParkCode","ParkName", "State", "Acres", "Latitude", "Longitude")

### Uni??n de las bases de datos by ParkName para ??ves, mam??feros y reptiles 
tot <- full_join(parks,aa1,by="ParkName",copy=FALSE)
tot1$ParkName <- as.factor(tot1$ParkName)
tot1 <-tot %>% filter(!is.na(ConservationStatus)) %>% filter(!is.na(Category))

tat <- full_join(parks,aaa1,by="ParkName",copy=FALSE)
tat1 <-tat %>% filter(!is.na(ConservationStatus)) %>% filter(!is.na(Category))



totabla<-tot1 %>% group_by(ParkName, ConservationStatus) %>%  summarise(mean = mean(n), n = n())
kable(totabla)
wer <- tat1 %>% group_by(ParkName) %>% summarise_all(funs(mean, sd)) %>% filter(!is.na(n_sd)) %>% dplyr::select(ParkName, n_mean, n_sd)
View(wer)
colnames(wer) <- c("Park Name", "Mean abundance", "Standard-Deviation abundance")

p4 <- ggplot(tat1,aes(x = ParkName, y = n)) + 
  geom_col(aes(fill = Category))+ 
  scale_fill_manual(values = c(rep(c("lightgreen", "lightgray", "steelblue", "black", "skyblue","#E69F00","darkgreen", "red", "lightgray", "darkgreen", "khaki", "darkbrown", "yellowgreen"))))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("Frequency")+ xlab("Park name")+
  theme(text=element_text(family="Times New Roman"), 
        axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(), 
        legend.title=element_text(size=13, face= "bold",colour="black"), 
        legend.text=element_text(size=11),
        axis.title=element_text(size=13, face= "bold",colour="black"),
        axis.text=element_text(size=9))+
  scale_x_discrete(labels = c("Acadia NP","Arches NP","Badlands NP", "Big Bend NP","Biscayne NP", "Black Canyon NP", "Bryce Canyon NP","Canyonlands NP", 
                              "Capitol Reef NP",  "Carlsbad Caverns NP","Channel Islands NP", "Congaree NP", "Crater Lake NP", "Cuyahoga Valley NP", "Death Valley NP","Dry Tortugas NP ", "Everglades NP",
                              "Glacier Bay NP","Grand Canyon NP", "Grand Teton NP", "Great Basin NP", "Great Smoky Mountains NP", "Guadalupe Mountains NP", 
                              "Haleakala NP", "Hawaii Volcanoes NP", "Hot Springs NP", "Isle Royale NP", "Joshua Tree NP", "Katmai NP", "Kenai Fjords NP", "Lake Clark NP", 
                              "Lassen Volcanic NP", "Mammoth Cave NP", "Mesa Verde NP", "Mount Rainier NP",  "North Cascades NP", "Olympic NP", "Petrified Forest NP", "Pinnacles NP",  
                              "Redwood NP", "Rocky Mountain NP", "Saguaro NP", "Sequoia-Kings Canyon NPs", "Shenandoah NP",
                              "Theodore Roosevelt NP", "Voyageurs NP", "Wind Cave NP", "Wrangell-St Elias NP", "Yellowstone NP", "Yosemite NP", "Zion NP"))

  





# Parques con mam??feros, aves y reptiles categor??as en estado de peligro
p3<-ggplot(tot1,aes(x = ParkName, y = n)) + 
  geom_col(aes(fill = Category))+
  scale_fill_manual(values = c(rep(c("lightgreen", "lightgray", "steelblue", "black", "skyblue","#E69F00","darkgreen"))))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("Frequency")+ xlab("Park name")+
  theme(text=element_text(family="Times New Roman"), 
        axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(), 
        legend.title=element_text(size=13, face= "bold",colour="black"), 
        legend.text=element_text(size=11),
        axis.title=element_text(size=13, face= "bold",colour="black"),
        axis.text=element_text(size=9))+
        scale_x_discrete(labels = c("Acadia NP","Arches NP","Badlands NP", "Big Bend NP","Biscayne NP", "Black Canyon NP", "Bryce Canyon NP","Canyonlands NP", 
                              "Capitol Reef NP",  "Carlsbad Caverns NP","Channel Islands NP", "Congaree NP", "Crater Lake NP", "Cuyahoga Valley NP", "Death Valley NP","Dry Tortugas NP ", "Everglades NP",
                              "Glacier Bay NP","Grand Canyon NP", "Grand Teton NP", "Great Basin NP", "Great Smoky Mountains NP", "Guadalupe Mountains NP", 
                              "Haleakala NP", "Hawaii Volcanoes NP", "Hot Springs NP", "Isle Royale NP", "Joshua Tree NP", "Katmai NP", "Kenai Fjords NP", "Lake Clark NP", 
                              "Lassen Volcanic NP", "Mammoth Cave NP", "Mesa Verde NP", "Mount Rainier NP",  "North Cascades NP", "Olympic NP", "Petrified Forest NP", "Pinnacles NP",  
                              "Redwood NP", "Rocky Mountain NP", "Saguaro NP", "Sequoia-Kings Canyon NPs", "Shenandoah NP",
                              "Theodore Roosevelt NP", "Voyageurs NP", "Wind Cave NP", "Wrangell-St Elias NP", "Yellowstone NP", "Yosemite NP", "Zion NP"))



#### especies por lat vs long 

p1<-ggplot(tot1,aes(x = Longitude, y = Latitude)) + 
geom_point(aes(color = Category), size = 5)+
scale_color_manual(values = c(rep(c("lightgreen", "lightgray", "steelblue", "black", "skyblue","#E69F00","darkgreen"))))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("Latitud")+ xlab("Longitud")+
  theme(text=element_text(family="Times New Roman"), 
        axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=0, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(), 
        legend.title=element_text(size=13, face= "bold",colour="black"), 
        legend.text=element_text(size=11),
        axis.title=element_text(size=13, face= "bold",colour="black"),
        axis.text=element_text(size=11))


#### ??rea

p2<-ggplot(tot1,aes(x = Longitude, y = Latitude))+ 
  geom_point(aes(size = Acres))+
  scale_color_manual(values=wes_palette(n=4, name="GrandBudapest1")) +
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_blank())+
  ylab("Latitud")+ xlab("Longitud")+
  theme(text=element_text(family="Times New Roman"), 
        axis.line=element_line(colour = "black"),
        axis.text.x = element_text(angle=0, hjust=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),
        panel.background=element_blank(), 
        legend.title=element_text(size=13, face= "bold",colour="black"), 
        legend.text=element_text(size=11),
        axis.title=element_text(size=13, face= "bold",colour="black"),
        axis.text=element_text(size=11))

############## Miltiplot##############

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
########################################################

multiplot(p2, p1)

multiplot(p3, p4)

