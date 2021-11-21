# Variable Index ----

# Access_Improved_Sanitation -------------- id_heal_06
# Food_Import_Dependency ------------------ id_food_03
# Rural_Population ------------------------ id_food_04
# ICT_Infrastructure ---------------------- id_soci_02 
# Education ------------------------------- id_soci_03
# Dependency_External_Health_Services ----- id_heal_03
# Dependency_Natural_Capital -------------- id_ecos_03
# Water_Dependency_Ratio ------------------ id_wate_04
# Political_Stability --------------------- id_gove_01
# Doing_Business -------------------------- id_econ_01
# Disaster_Preparedness ------------------- id_infr_06
# Age_Dependency_Ratio -------------------- id_habi_04
# Ecological_Footprint -------------------- id_ecos_04


# Pulling Data Together ----

data_ids = c('id_food_03', 'id_food_04', 'id_soci_02', 'id_soci_03', 'id_heal_03', 'id_ecos_03',
             'id_wate_04', 'id_gove_01', 'id_econ_01', 'id_infr_06', 'id_habi_04', 'id_ecos_04')

#if adding other variables, add them before the "TRUE ~ 'Not Found'" line
Variable_Namer <- function(old_name) {
  case_when(old_name == 'id_food_03' ~ 'Food_Import_Dependency',
            old_name == 'id_food_04' ~ 'Rural_Population',
            old_name == 'id_soci_02' ~ 'ICT_Infrastructure',
            old_name == 'id_soci_03' ~ 'Education',
            old_name == 'id_heal_03' ~ 'Dependency_External_Health_Services',
            old_name == 'id_ecos_03' ~ 'Dependency_Natural_Capital',
            old_name == 'id_wate_04' ~ 'Water_Dependency_Ratio',
            old_name == 'id_gove_01' ~ 'Political_Stability',
            old_name == 'id_econ_01' ~ 'Doing_Business',
            old_name == 'id_infr_06' ~ 'Disaster_Preparedness',
            old_name == 'id_habi_04' ~ 'Age_Dependency_Ratio',
            old_name == 'id_ecos_04' ~ 'Ecological_Footprint',
            TRUE ~ 'Not_Found')
}

require(tidyverse)

#starting cleaned dataset with response variable of interest, Access to Improved Sanitation Facilities
Cleaned_Dataset <- read.csv(file.path('raw.csv')) %>%
  pivot_longer(names_to = 'Year', values_to = 'Access_Improved_Sanitation', cols = contains('X')) %>%
  rename(Abbreviation = ISO3,
         Country = Name) %>%
  mutate(Year = gsub('X', '', Year),
         Year = as.numeric(Year),
         for_join = paste(Abbreviation, Year, sep = '_'))

for (new_data in data_ids) {
  #make sure to  update file paths. I had the downloaded folder from ND-GAIN in my downloads folder
  #also, if you want to change this to the score .csv, you can change the last part of this
  temp_data <- read.csv(file.path('~', 'Downloads', 'resources', 'indicators', new_data, 'raw.csv')) %>%
    rename(Abbreviation = ISO3,
           Country = Name) %>%
    pivot_longer(names_to = 'Year', values_to = 'value', cols = contains('X')) %>%
    mutate(Year = gsub('X', '', Year),
           Year = as.numeric(Year))
  
  #some specific cleaning needed for some variables
  if (new_data == 'id_wate_04') { #single data point for each country
    for_merge <- temp_data %>%
      filter(!is.na(value)) %>%
      group_by(Country) %>%
      summarise(value = value[1]) %>%
      ungroup()
    
    new_var_name <- Variable_Namer(new_data)
    colnames(for_merge)[grep('value', colnames(for_merge))] <- new_var_name
    
    Cleaned_Dataset <- full_join(Cleaned_Dataset, for_merge, by = 'Country')
    
  } else if (new_data == 'id_ecos_04') { #one or two years with data for each country
    for_merge <- temp_data %>%
      group_by(Abbreviation, Country) %>%
      summarise(value = mean(value, na.rm=T)) %>%
      ungroup() %>%
      select(Country, value)
    
    new_var_name <- Variable_Namer(new_data)
    colnames(for_merge)[grep('value', colnames(for_merge))] <- new_var_name
    
    Cleaned_Dataset <- full_join(Cleaned_Dataset, for_merge, by = 'Country')
    
  } else { #generic merging steps
    #prepping for merge
    for_merge <- temp_data %>%
      mutate(for_join = paste(Abbreviation, Year, sep = '_')) %>%
      select(for_join, value)
    
    #changing variable name
    new_var_name <- Variable_Namer(new_data)
    colnames(for_merge)[grep('value', colnames(for_merge))] <- new_var_name
    
    #merging to Cleaned_Dataset
    Cleaned_Dataset <- full_join(Cleaned_Dataset, for_merge, by = 'for_join')
  }
}


Filtered_Dataset <- Cleaned_Dataset %>%
  select(-for_join)

data.table::fwrite(Filtered_Dataset, file.path('Dataset.csv'))
