##Group K  R Code 

## Hypothesis 1 

#Data Preprocessing Steps: 

library(tidyverse) 

library(dplyr) 

library(ggplot2) 



#Step 1: Importing the dataset 

setwd("C:/Users/Neera/Downloads") 

race.data<- read.csv(file = "CRDT.csv" , header = TRUE , sep = ",") 



view(race.data) 

dim(race.data) 

summary(race.data) 



#Step 2: Handling the missing data 

# check missing values in Cases_Black column  

is.na(race.data$ Cases_Black)  

is.na(race.data$ Cases_White)  

is.na(race.data$ Deaths_Black)  

is.na(race.data$ Deaths_White)  



# total number of missing values in Died column  

sum(is.na(race.data$Cases_Black))  

sum(is.na(race.data$Cases_White))  

sum(is.na(race.data$Deaths_Black))  

sum(is.na(race.data$Deaths_White)) 



##replacing the missing data in the Age column with the mean of that column. 



race.data$Cases_Black = ifelse(is.na(race.data$Cases_Black), 
                               
                               ave(race.data$Cases_Black, FUN = function (x)mean(x, na.rm = TRUE)), 
                               
                               race.data$Cases_Black) 



race.data$Cases_White = ifelse(is.na(race.data$Cases_White), 
                               
                               ave(race.data$Cases_White, FUN = function (x)mean(x, na.rm = TRUE)), 
                               
                               race.data$Cases_White) 



race.data$Deaths_Black = ifelse(is.na(race.data$Deaths_Black), 
                                
                                ave(race.data$Cases_Black, FUN = function (x)mean(x, na.rm = TRUE)), 
                                
                                race.data$Deaths_Black) 



race.data$Deaths_White = ifelse(is.na(race.data$Deaths_White), 
                                
                                ave(race.data$Cases_White, FUN = function (x)mean(x, na.rm = TRUE)), 
                                
                                race.data$Deaths_White) 



View(race.data$Cases_Black)  

View(race.data$Cases_White)  



View(race.data$Deaths_Black)  

View(race.data$Deaths_White)  

##Step 3 : Identifying and removing Outliers 

boxplot(race.data$Cases_Black) 

boxplot(race.data$Cases_White) 

boxplot(race.data$Deaths_Black) 

boxplot(race.data$Deaths_White) 



quantile(race.data$Cases_Black) 

quantile(race.data$Cases_White) 

quantile(race.data$Deaths_Black) 

quantile(race.data$Deaths_White) 



## Step 4: Selecting the required variables 

library(tidyverse) 

r.data<- race.data %>%  
  
  select(State,Cases_Black,Cases_White,Deaths_Black,Deaths_White)  



## Step 5: aggregating the data and  plotting histogram 



hist(race.data$Cases_Black) 

hist(race.data$Cases_White) 



hist(race.data$Deaths_Black) 

hist(race.data$Deaths_White) 





table(race.data$Deaths_Black, race.data$Deaths_White) 





## Hypothesis 2 



#Data Preprocessing Steps: 



#Step 1: Importing the dataset 

setwd(C:\Users\Charlotte\OneDrive - The University of Memphis\Documents\MIS 7190\Florida Covid_19 GroupK) 

Florida_Covid19.data<- read.csv(file="Florida_COVID19_Case_Line_Data_NEW_0",  
                                
                                header=TRUE, sep=",") 



# load the data for Florida_Covid19_Case-Line 

Florida_Covid19.data <- Florida_COVID19_Case_Line_Data_NEW_0 



# colsums to find which column has missing values 

colSums(is.na(Florida_Covid19.data)) 



# calculate mean of Age, na.rm is used to ignore missing values 

mean.age <- mean(Florida_Covid19.data$Age, na.rm = T) 

Florida_Covid19.data$Age[is.na(Florida_Covid19.data$Age)]<-  
  
  mean.age 

mean.age 



# replace missing values for Age 

Florida_Covid19.data$Age[is.na(Florida_Covid19.data$Age)] <- mean.age 

Florida_Covid19.data$Age 



# histogram for Age after NA replaced with mean 

hist(Florida_Covid19.data$Age) 



# aggregate table for age group 

ageGroup.table <- table(Florida_Covid19.data$Age_group) 



#create labels 

label.ageGroup <- c("0-4 years","5-14 years", "15-24 years","25-34 years","35-44 years", "45-54 years",  
                    
                    "55-64 years", "65-74 years", "75-84 years",  
                    
                    "85+ years", "Unknown") 



# bar plot 

barplot(ageGroup.table, names.arg = label.ageGroup,  
        
        main = "Age Groups of Covid19 Cases", col = rainbow(11),  
        
        ylim = c(0,12000), xlab = "Groups by Age") 



# create labels 

label.travelRelated <- c("No", "Yes", "Unknown") 



pie(travelRelated.table, labels = label.travelRelated, col =  
      
      c("blue", "yellow", "red"), main = "Travel Related") 



# check missing values in Travel Related column 

is.na(Florida_Covid19.data$Travel_related) 



# total number of missing values in Travel Related column 

sum(is.na(Florida_Covid19.data$Travel_related)) 



##Hypothesis 3 

#Step 1: Importing the dataset  

#setwd 

setwd("/Users/heribertohernandez/Library/Mobile Documents/com~apple~CloudDocs/Fall Courses 2021/MIS 7190") 

#read csv file 

florida.covid.data <- read.csv(file = "Florida.Covid.Data.ForR.csv", header = T, sep = ",") 

florida.covid.data 

#Step 2: Handling the missing data  

#check for missing values 

is.na(florida.covid.data$Died) 

sum(is.na(florida.covid.data$Died))  

#replace NAs 

k <- c("No") 

florida.covid.data$Died[is.na(florida.covid.data$Died)] <- k 

is.na(florida.covid.data$Died) 

sum(is.na(florida.covid.data$Died)) 

florida.covid.data 

#Step 3: Create Visuals 

data.agg.deaths <- as.data.frame(table(florida.covid.data$Died)) 

data.agg.deaths 

colnames(data.agg.deaths) <-c("Died", "Frequency of Deaths") 

data.agg.deaths 

died.table <- table(florida.covid.data$Died)  

died.table 

label.died <- c("No", "Yes")  

barplot(died.table, names.arg = label.died, 
        
        main = "Deaths due to Covid19 Cases", col = rainbow(2),   
        
        ylim = c(0,70000), xlab = "Died Yes or No")  

# pie chart of deaths  

pie(died.table, labels = label.died, col = c("light green", "red"), main = "Died, Yes or No")  

#Step 4: Create filtered outputs 

#tably function 

age.group.deaths <- tabyl(florida.covid.data, Age_group, Died) 

# filter columns based on names 

filter.rows1 <- florida.covid.data[florida.covid.data$Age_group == "55-64 years" & florida.covid.data$Died == "Yes",] 

filter.rows1 

florida.covid.data %>% select(florida.covid.data$Age_group == "55-64 years", florida.covid.data$Died) 

##Gives me total instances of age group 55-64 + died [yes and no] 

died.table2 <- table(florida.covid.data$Age_group == "55-64 years" & florida.covid.data$Died)  

died.table2 

label.died2 <- c("No", "Yes") 

agg.deaths.group1 <- as.data.frame(table(florida.covid.data$Age_group == "55-64 years")) 

agg.deaths.group1 

filter.value1 <- florida.covid.data[florida.covid.data$Age_group == "55-64 years" & florida.covid.data$Died == c("No","Yes"),] 

filter.value1 

barplot(died.table2, names.arg = label.died2, 
        
        main = "Deaths due to Covid19 Cases", col = rainbow(2),   
        
        xlab = "Died Yes or No")  

pie(died.table2, labels = label.died, col = c("light green", "red"), main = "Died, Yes or No")  

# filter columns based on age group 2 

filter.rows2 <- florida.covid.data[florida.covid.data$Age_group == "65-74 years" & florida.covid.data$Died == "Yes",] 

filter.rows2 

# filter columns based on age group 3 

filter.rows3 <- florida.covid.data[florida.covid.data$Age_group == "85+ years" & florida.covid.data$Died == "Yes",] 

filter.rows3 



##Hypothesis 4 

# importing csv file 

vaccination <- read.csv("covidvaccination.csv", header = TRUE, sep = ",", check.names = FALSE) 



# Viewing the data to maku sure it is in the right format 

view(vaccination) 



# Replacing  space and / in column names with an underscore 

library(tidyverse) 

names(vaccination) %<>%  
  
  tolower() %>% 
  
  str_replace_all(" ", "_") %>% 
  
  str_replace_all("/", "_") 

view(vaccination) 



#removing % symbol from fully_vaccinated column and converting it to numeric column 

pct_to_number<- function(x){ 
  
  x_replace_pct<-sub("%", "", x) 
  
  x_as_numeric<-as.numeric(x_replace_pct) 
  
} 

vaccination[['fully_vaccinated']] = pct_to_number(vaccination[['fully_vaccinated']]) 

view(vaccination) 





# Computing the average vaccination rate for rural and urban areas 

summarise_at(group_by(vaccination,rural_urban_area), 
             
             vars(fully_vaccinated),funs(mean(.,na.rm=TRUE))) 



# Plot the vaccination rate for rural and urban areas 

library(ggplot2) 

p<-ggplot(data=vaccination, aes(x=rural_urban_area, y=fully_vaccinated, fill=rural_urban_area)) + 
  
  geom_bar(stat="identity")+theme_minimal() 

print(p) 



##Sub Hypothesis 4 

setwd("/Users/Keerthana/Downloads/") 

library(ggplot2)     #using ggplot for graphical representation 



vaccinations <- read.csv("vaccinations.csv", check.names = F)   #creating dataframe for dataset vaccination 

#using gsub for replacing all the instances for rural population and total cases 

vaccinations$`Rural population` <- as.numeric(gsub(",", "", vaccinations$`Rural population`))   

vaccinations$`Total cases` <- as.numeric(gsub(",", "", vaccinations$`Total cases`))     



options(scipen=999) 

# plotting the graph  for Rural population and total cases 

ggplot(vaccinations, aes(x=`Rural population`, y=`Total cases`)) + geom_line() 



# plotting the graph  for Rural population and total cases for all states with color representation. 

ggplot(vaccinations, aes(x=`State`)) +  
  
  geom_line(aes(y = `Rural population`, group = 1), color = "darkred") +  
  
  geom_line(aes(y = `Total cases`, group = 2), color="steelblue", linetype="twodash") + 
  
  ylab('Value') + 
  
  coord_flip() 

