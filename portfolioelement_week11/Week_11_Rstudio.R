#Setting up workspace
dir.create("data")
dir.create("figures")
dir.create("scripts")

#Downloading data
download.file(
  "https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv",
  "data/SAFI_clean.csv", mode = "wb"
)

#Creating objects
area_hectares<-50
area_hectares
area_hectares*2.7

area_acres<-area_hectares*2.47 #R updates on request

#Functions
sqrt(10)

round(3.14159,digits=2)
round(3.14159,2)
?round

round(digits=2,x=3.14159)

#Vectors
hh_members<-c(3,7,10,6)
wall_type<-c("mudbrick","sunbrick","muddaub")
wall_type

length(hh_members) #how many elements?
class(wall_type) #datatype?
class(hh_members)
str(hh_members)

#Grow your vectors
big_hh_members<-c(1,2,3,hh_members,12,17,6,4)
big_hh_members
big_wall_type<-c(wall_type,wall_type,"tile")
big_wall_type

#Logical vectors
hh_members>5

#Data type COERCIAN
num_char <- c(1, 2, 3, "a") #Vectors in R need to be datatype-consistent
num_logical <- c(1, 2, 3, FALSE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")

class(num_char)
class(num_logical)
class(char_logical)
class(tricky)

num_char
num_logical
char_logical
tricky
sqrt(tricky)

num_logical <- c(1, 2, 3, TRUE)
num_logical
char_logical <- c("a", "b", "c", TRUE)
char_logical
combined_logical <- c(num_logical, char_logical)
combined_logical

class(num_logical)
class(char_logical)

#Subsetting
wall_type[2]
big_wall_type[4:7]
big_wall_type[7:4]
new_wall_type<-big_wall_type[c(2,3,7,1)]

hh_members[3]
hh_members[c(3,1,2, 1,4,3,3,3)]
hh_members[-3]
big_hh_members[-c(2,4,5,7)]

#Conditional subsetting
hh_members>3
hh_members[hh_members>3]
big_hh_members[big_hh_members>6&big_hh_members<15]
big_hh_members[big_hh_members<6|big_hh_members>15]

length(big_hh_members[big_hh_members<6|big_hh_members>15])
sum(hh_members>3)

big_wall_type[big_wall_type=="sunbrick"|big_wall_type=="tile"]
big_wall_type[big_wall_type%in%c("sunbrick","tile")]

#Missing data
rooms<-c(2,1,1,NA,4)
rooms
class(rooms)
max(rooms,na.rm=TRUE)
?max
mean(rooms,na.rm=TRUE)

#Eliminating missing values in subsetting
!is.na(rooms)
rooms[!is.na(rooms)]

#In other functions
na.omit(rooms)

#Install package
install.packages("tidyverse")
library(tidyverse)

interviews<-read_csv("data/SAFI_clean.csv",na="NULL")
getwd()
interviews$memb_assoc

regents<-read.csv2("data/danish_regents.csv")
regents

rooms <- c(1, 5, 2, 1, 3, 1, NA, 3, 1, 3, 2, 1, NA, 1, 8, 3, 1, 4, NA, 1, 3, 1, 2, 1, 7, 1, NA, 4, 3 ,1 ,7 ,8 ,2 ,1 ,NA ,1, 1, 3)
!is.na(rooms)
rooms[!is.na(rooms)]
sum(rooms[!is.na(rooms)]>2)
class(rooms[!is.na(rooms)])
median(rooms[!is.na(rooms)])

regents$duration<-regents$end_reign_year-regents$start_reign_year
mean(regents$duration,na.rm=TRUE)
median(regents$duration,na.rm=TRUE)

interviews
regents
regents<-read_csv("data/danish_regents.csv")
regents
regents<-read_csv2("data/danish_regents.csv")

#Inspect tibbles
interviews
dim(interviews)
nrow(interviews)
ncol(interviews)
head(interviews)
tail(interviews)
names(interviews) #Dette er en vector. Vigtig at der er forskel på data frame og vector
glimpse(interviews)

#Subsetting
interviews[10,]
interviews[1:10,]
interviews[1:10,4]
interviews[10,]
interviews[,4]

#Tibble versus a vector
mean(interviews[,4])
interviews[[4]]
interviews[4]
mean(interviews[[4]])

interviews$no_membrs
mean(interviews$no_membrs)

#Eliminating rows
interviews[131,]
interviews[1:130]
interviews[-131,]
interviews[-c(100:131),]

interviews[["village"]]

#Practise
interviews[100,]

interviews100<-interviews[100,] #Løsning på øvelse

#Learning dplyr
select(interviews,village,no_membrs)
select(interviews,village:respondent_wall_type)
filter(interviews,village=="Chirodzo"|village=="Ruaca",rooms>1)

#Pipes
interviews %>% # %>% symbolet laves med crtl+shift+m
  filter(village=="Chirodzo") %>%
  select(village:respondent_wall_type) %>%
  select(affect_conflicts&liv_count&no_meals)

#Mutate
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  mutate(people_per_room=no_membrs/rooms) %>%
  glimpse()

#Group and summarize
interviews %>% 
  group_by(village) %>% 
  summarise(mean_no_membrs=mean(no_membrs),
            min_no_membrs=min(no_membrs),
            max_no_membrs=max(no_membrs)) %>% 
arrange(mean_no_membrs) 

interviews %>% 
  group_by(village) %>% 
  count()

#ggplot2
interviews_plotting <- interviews %>%
  ## pivot wider by items_owned
  separate_rows(items_owned, sep = ";") %>%
  ## if there were no items listed, changing NA to no_listed_items
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE)) %>%
  ## pivot wider by months_lack_food
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))

interviews_plotting %>% 
  ggplot(aes(x=no_membrs,y=number_items,
             color=village))+
  geom_point()+
  geom_jitter()+
  geom_count()

library(tidyverse)

#Practise danske regenter
regents<-read.csv2("data/danish_regents.csv")
names(regents)
regents %>%
  mutate(duration=end_reign_year-start_reign_year,
         midyear=end_reign_year-duration/2) %>% 
ggplot(aes(x=midyear,y=duration))+
  geom_point()+ 
  geom_smooth()+
  labs(title="Regenters regeringstid")
