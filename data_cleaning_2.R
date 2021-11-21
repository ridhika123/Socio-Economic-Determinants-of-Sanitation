install.packages("tidyverse")
library(tidyverse)
data <- read_csv("~/Desktop/Semester 8/MAT Probability Statistics 2/Project/Prob_Stats_Dataset.csv")
View(data)

# dropping variable with less data
data_drop <- subset(data, select=-c(Dependency_Natural_Capital, Disaster_Preparedness))
# restricting time
data_drop_time <- subset(data_drop, Year > 2004 & Year < 2016)
data_drop_countries <- subset(data_drop_time, Abbreviation != "AND" &
                                Abbreviation != "CPV" &
                                Abbreviation != "TUV" &
                                Abbreviation != "SWZ" &
                                Abbreviation != "SOM" &
                                Abbreviation != "SMR" &
                                Abbreviation != "KNA" &
                                Abbreviation != "PRK" &
                                Abbreviation != "PLW" &
                                Abbreviation != "NRU" &
                                Abbreviation != "MCO" &
                                Abbreviation != "MHL" &
                                Abbreviation != "LIE")
View(data_drop_countries)
# Countries to be removed
# Andorra 
# Cape Verde
# Tuvalu
# Swaziland
# Somalia
# San Marino
# Saint Kitts and Nevis
# Korea, Democratic People's Repub
# Palau
# Nauru
# Monaco
# Marshall Islands
# Liechstenstein

# Imputation of missing data
names(data_drop_countries)
datanew <- data_drop_countries %>% 
  group_by(Country) %>% 
  fill(Food_Import_Dependency, Education,Access_Improved_Sanitation,Rural_Population,ICT_Infrastructure,Dependency_External_Health_Services,
       Water_Dependency_Ratio,Political_Stability,Doing_Business,Age_Dependency_Ratio,Age_Dependency_Ratio,.direction=("downup"))

data_dropped <- drop_na(datanew)

# sum(is.na(datanew$Access_Improved_Sanitation)) # 22 2
# sum(is.na(datanew$Food_Import_Dependency)) # 220 20
# sum(is.na(datanew$Rural_Population)) # 0
# sum(is.na(datanew$ICT_Infrastructure)) # 0
# sum(is.na(datanew$Education)) # 242 22
# sum(is.na(datanew$Dependency_External_Health_Services)) # 165 15
# sum(is.na(datanew$Water_Dependency_Ratio)) # 22 2
# sum(is.na(datanew$Political_Stability)) # 33 3
# sum(is.na(datanew$Doing_Business)) # 66 6 
# sum(is.na(datanew$Age_Dependency_Ratio)) # 11 1
# sum(is.na(datanew$Ecological_Footprint)) # 55 5
# 
# continent_codes <- read_excel("Desktop/Semester 8/MAT Probability Statistics 2/Project/continent_codes.xlsx")
# continent_codes<- subset(continent_codes, select=c(Continent_Code, Three_Letter_Country_Code))
# datanew_continents <- left_join(datanew, continent_codes, by= c("Abbreviation"= "Three_Letter_Country_Code"))
# sum(is.na(datanew_continents$Continent_Code))
# 
# mean_data <- aggregate(datanew_continents[, 4: 14], 
#                        list(datanew_continents$Continent_Code, datanew_continents$Year), 
#                        mean, na.rm=TRUE)
# 
# 
# var_list <- c("Access_Improved_Sanitation", "Food_Import_Dependency","Rural_Population", 
#               "ICT_Infrastructure","Education", "Dependency_External_Health_Services", 
#               "Water_Dependency_Ratio", "Political_Stability", "Doing_Business", "Age_Dependency_Ratio", "Ecological_Footprint")
# 
# # Access_Improved_Sanitation
# country_vector <- unique(subset(datanew_continents, is.na(Access_Improved_Sanitation))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Access_Improved_Sanitation
#     datanew_continents[country_row_vector[i], 4] = to_fill
#     i <- i+1
#     }
#   }
# 
# # Food_Import_Dependency
# country_vector <- unique(subset(datanew, is.na(Food_Import_Dependency))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Food_Import_Dependency
#     datanew_continents[country_row_vector[i], 5] = to_fill
#     i <- i+1
#   }
# }
# 
# # Education 22
# country_vector <- unique(subset(datanew_continents, is.na(Education))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Education
#     datanew_continents[country_row_vector[i], 8] = to_fill
#     i <- i+1
#   }
# }
# 
# # Dependency_External_Health_Services 15
# country_vector <- unique(subset(datanew_continents, is.na(Dependency_External_Health_Services))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Dependency_External_Health_Services
#     datanew_continents[country_row_vector[i], 9] = to_fill
#     i <- i+1
#   }
# }
# 
# # Water_Dependency_Ratio 2
# country_vector <- unique(subset(datanew_continents, is.na(Water_Dependency_Ratio))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Water_Dependency_Ratio
#     datanew_continents[country_row_vector[i], 10] = to_fill
#     i <- i+1
#   }
# }
# 
# # Political_Stability 3
# country_vector <- unique(subset(datanew_continents, is.na(Political_Stability))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Political_Stability
#     datanew_continents[country_row_vector[i], 11] = to_fill
#     i <- i+1
#   }
# }
# 
# # Doing_Business 6
# country_vector <- unique(subset(datanew_continents, is.na(Doing_Business))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Doing_Business
#     datanew_continents[country_row_vector[i], 12] = to_fill
#     i <- i+1
#   }
# }
# 
# # Age_Dependency_Ratio 1
# country_vector <- unique(subset(datanew_continents, is.na(Age_Dependency_Ratio))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Age_Dependency_Ratio
#     datanew_continents[country_row_vector[i], 13] = to_fill
#     i <- i+1
#   }
# }
# 
# #Ecological_Footprint 5
# country_vector <- unique(subset(datanew_continents, is.na(Ecological_Footprint))$Abbreviation)
# for (country_abb in country_vector){
#   i <- 1
#   abbreviation_continent <- subset(datanew_continents, Abbreviation == country_abb)$Continent_Code[1]
#   country_row_vector <- which(datanew_continents$Abbreviation == country_abb)
#   for (year in 2005:2015) {
#     print(i)
#     to_fill <- subset(mean_data, Group.1 == abbreviation_continent & Group.2 == year)$Ecological_Footprint
#     datanew_continents[country_row_vector[i], 14] = to_fill
#     i <- i+1
#   }
# }
# 
# View(datanew_continents)

write.csv(data_dropped, "~/Desktop/Semester 8/MAT Probability Statistics 2/Project/cleaned_data_jonkman_edit.csv")
