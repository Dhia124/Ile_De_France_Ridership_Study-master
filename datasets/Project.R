######### Library ####################
install.packages("readr")
library(readr)

install.packages("sf")
library(sf)

install.packages("dplyr")
library(dplyr)

install.packages("naniar")
library(naniar)

install.packages("readxl")
library(readxl)


library(ggplot2)

install.packages("lubridate")
library(lubridate)

############### Directory
setwd("C:/Users/ASUS/Desktop/Ile_De_France_Ridership_Study-master/datasets/")


#########Analyzing and Visualizing Ridership Patterns in Île-de-France Rail Network############

###### 1 : Data Collection and Cleaning #########

####### Collection

################################################### Data 2023 ############################

validation_data_2023 <- read_excel("validations-reseau-ferre-nombre-validations-par-jour-1er-semestre.xlsx")

View(validation_data_2023)
names(validation_data_2023)


remove_outliers <- function(data, column_name) {
  # Calculate quartiles and IQR
  Q1 <- quantile(data[[column_name]], 0.25)
  Q3 <- quantile(data[[column_name]], 0.75)
  IQR_value <- Q3 - Q1
  
  # Set the lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Remove outliers
  filtered_data <- data[!(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound), ]
  
  return(filtered_data)
}

validation_data_2023 <- remove_outliers(validation_data_2023, "lda")

################# Get unique values of 'lda' column
unique_values <- unique(validation_data_2023$lda)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)



################################################### Data 2022 ##################################

data_2022_S1_NB_FER <- read.delim("data-rf-2022/2022_S1_NB_FER.txt")
nrow(data_2022_S1_NB_FER)

################################### Get unique values of 'lda' column
unique_values <- unique(data_2022_S1_NB_FER$ID_REFA_LDA)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)
###################################

data_2022_S1_NB_FER <- remove_outliers(data_2022_S1_NB_FER, "ID_REFA_LDA")
names(data_2022_S1_NB_FER)



data_2022_S1_PROFIL_FER <- read.delim("data-rf-2022/2022_S1_PROFIL_FER.txt")
data_2022_S1_PROFIL_FER <- remove_outliers(data_2022_S1_PROFIL_FER, "ID_REFA_LDA")



data_2022_S2_Nb_FER <- read.delim("data-rf-2022/2022_S2_NB_FER.txt", sep = ";")
data_2022_S2_Nb_FER <- remove_outliers(data_2022_S2_Nb_FER, "lda")

data_2022_S2_PROFIL_FER <- read.delim("data-rf-2022/2022_S2_PROFIL_FER.txt", sep = ";")
data_2022_S2_PROFIL_FER <- remove_outliers(data_2022_S2_PROFIL_FER, "lda")

###### rename
names(data_2022_S2_Nb_FER)[names(data_2022_S2_Nb_FER) == "lda"] <- "ID_REFA_LDA"
names(data_2022_S2_PROFIL_FER)[names(data_2022_S2_PROFIL_FER) == "lda"] <- "ID_REFA_LDA"


##### Rbind
data_2022_NB_FER <- rbind(data_2022_S1_NB_FER, data_2022_S2_Nb_FER)
data_2022_PROFILL_FER <- rbind(data_2022_S1_PROFIL_FER, data_2022_S2_PROFIL_FER)




########################################### Data 2021 ##################################

data_2021_S1_NB_FER <- read.delim("data-rf-2021/2021_S1_NB_FER.txt")
data_2021_S1_NB_FER <- remove_outliers(data_2021_S1_NB_FER, "ID_REFA_LDA")

data_2021_S1_PROFIL_FER <- read.delim("data-rf-2021/2021_S1_PROFIL_FER.txt")
data_2021_S1_PROFIL_FER <- remove_outliers(data_2021_S1_PROFIL_FER, "ID_REFA_LDA")



data_2021_S2_Nb_FER <- read.delim("data-rf-2021/2021_S2_NB_FER.txt")
data_2021_S2_Nb_FER <- remove_outliers(data_2021_S2_Nb_FER, "ID_REFA_LDA")

data_2021_S2_PROFIL_FER <- read.delim("data-rf-2021/2021_S2_PROFIL_FER.txt")
data_2021_S2_PROFIL_FER <- remove_outliers(data_2021_S2_PROFIL_FER, "ID_REFA_LDA")



##### Rbind
data_2021_NB_FER <- rbind(data_2021_S1_NB_FER, data_2021_S2_Nb_FER)
data_2021_PROFILL_FER <- rbind(data_2021_S1_PROFIL_FER, data_2021_S2_PROFIL_FER)











################# Data 2020 ##################################

data_2020_S1_NB_FER <- read.delim("data-rf-2020/2020_S1_NB_FER.txt")
data_2020_S1_NB_FER <- remove_outliers(data_2020_S1_NB_FER, "ID_REFA_LDA")

data_2020_S1_PROFIL_FER <- read.delim("data-rf-2020/2020_S1_PROFIL_FER.txt")
data_2020_S1_PROFIL_FER <- remove_outliers(data_2020_S1_PROFIL_FER, "ID_REFA_LDA")




data_2020_S2_Nb_FER <- read.delim("data-rf-2020/2020_S2_NB_FER.txt")
data_2020_S2_Nb_FER <- remove_outliers(data_2020_S2_Nb_FER, "ID_REFA_LDA")

data_2020_S2_PROFIL_FER <- read.delim("data-rf-2020/2020_S2_PROFIL_FER.txt")
data_2020_S2_PROFIL_FER <- remove_outliers(data_2020_S2_PROFIL_FER, "ID_REFA_LDA")



##### Rbind
data_2020_NB_FER <- rbind(data_2020_S1_NB_FER, data_2020_S2_Nb_FER)
data_2020_PROFILL_FER <- rbind(data_2020_S1_PROFIL_FER, data_2020_S2_PROFIL_FER)






################# Data 2019 ##################################

data_2019_S1_NB_FER <- read.delim("data-rf-2019/2019_S1_NB_FER.txt")
data_2019_S1_NB_FER <- remove_outliers(data_2019_S1_NB_FER, "ID_REFA_LDA")



data_2019_S1_PROFIL_FER <- read.delim("data-rf-2019/2019_S1_PROFIL_FER.txt")
data_2019_S1_PROFIL_FER <- remove_outliers(data_2019_S1_PROFIL_FER, "ID_REFA_LDA")


data_2019_S2_Nb_FER <- read.delim("data-rf-2019/2019_S2_NB_FER.txt")
data_2019_S2_Nb_FER <- remove_outliers(data_2019_S2_Nb_FER, "ID_REFA_LDA")


data_2019_S2_PROFIL_FER <- read.delim("data-rf-2019/2019_S2_PROFIL_FER.txt")
data_2019_S2_PROFIL_FER <- remove_outliers(data_2019_S2_PROFIL_FER, "ID_REFA_LDA")

##### Rbind
data_2019_NB_FER <- rbind(data_2019_S1_NB_FER, data_2019_S2_Nb_FER)
data_2019_PROFILL_FER <- rbind(data_2019_S1_PROFIL_FER, data_2019_S2_PROFIL_FER)


################# Data 2018 ##################################


data_2018_S1_NB_FER <- read.delim("data-rf-2018/2018_S1_NB_FER.txt")
data_2018_S1_NB_FER <- remove_outliers(data_2018_S1_NB_FER, "ID_REFA_LDA")

data_2018_S1_PROFIL_FER <- read.delim("data-rf-2018/2018_S1_PROFIL_FER.txt")
data_2018_S1_PROFIL_FER <- remove_outliers(data_2018_S1_PROFIL_FER, "ID_REFA_LDA")


data_2018_S2_Nb_FER <- read.delim("data-rf-2018/2018_S2_NB_Fer.txt")
data_2018_S2_Nb_FER <- remove_outliers(data_2018_S2_Nb_FER, "ID_REFA_LDA")

data_2018_S2_PROFIL_FER <- read.delim("data-rf-2018/2018_S2_Profil_Fer.txt")
data_2018_S2_PROFIL_FER <- remove_outliers(data_2018_S2_PROFIL_FER, "ID_REFA_LDA")

##### Rbind
data_2018_NB_FER <- rbind(data_2018_S1_NB_FER, data_2018_S2_Nb_FER)
data_2018_PROFILL_FER <- rbind(data_2018_S1_PROFIL_FER, data_2018_S2_PROFIL_FER)
nrow(data_2018_PROFILL_FER)



################# Data 2017 ##################################

data_2017_S1_NB_FER <- read.delim("data-rf-2017/2017S1_NB_FER.txt")
data_2017_S1_NB_FER <- remove_outliers(data_2017_S1_NB_FER, "ID_REFA_LDA")



data_2017_S1_PROFIL_FER <- read.delim("data-rf-2017/2017S1_PROFIL_FER.txt")

############################ Get unique values of 'ID_REFA_LDA' column
unique_values <- unique(data_2017_S1_PROFIL_FER$ID_REFA_LDA)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)

###### Remove "?" and ""
data_2017_S1_PROFIL_FER <- data_2017_S1_PROFIL_FER %>%
  filter(!(ID_REFA_LDA == "?" | ID_REFA_LDA == ""))

##### Transform character to numeric
data_2017_S1_PROFIL_FER$ID_REFA_LDA <- as.numeric(data_2017_S1_PROFIL_FER$ID_REFA_LDA)

summary(data_2017_S1_PROFIL_FER)


data_2017_S1_PROFIL_FER <- remove_outliers(data_2017_S1_PROFIL_FER, "ID_REFA_LDA")


data_2017_S2_Nb_FER <- read.delim("data-rf-2017/2017_S2_NB_FER.txt")
data_2017_S2_Nb_FER <- remove_outliers(data_2017_S2_Nb_FER, "ID_REFA_LDA")

data_2017_S2_PROFIL_FER <- read.delim("data-rf-2017/2017_S2_PROFIL_FER.txt")
data_2017_S2_PROFIL_FER <- remove_outliers(data_2017_S2_PROFIL_FER, "ID_REFA_LDA")

##### Rbind
data_2017_NB_FER <- rbind(data_2017_S1_NB_FER, data_2017_S2_Nb_FER)
data_2017_PROFILL_FER <- rbind(data_2017_S1_PROFIL_FER, data_2017_S2_PROFIL_FER)
nrow(data_2017_PROFILL_FER)




################ Stops ########################


stops <- read_csv2("zones-d-arrets.csv")
View(stops)

################## Spatial data ###############################################

locations=st_read("REF_ZdA/PL_ZDL_R_05_01_2024.shp",crs=4326)
View(locations)

################### Merge Stops with Spatial #############################

stops_with_locations <- merge(stops, locations, by.x = "ZdAId", by.y = "id_refa")
View(stops_with_locations)



############# Check for missing values in the entire dataset

View(validation_data_2023)

missing_values <- validation_data_2023 %>%
  summarise_all(~sum(is.na(.)))


# Display the result
print(missing_values)


#columns_to_impute <- c("CODE_STIF_RES ", "CODE_STIF_ARRET ")
#validation_data_2023$CODE_STIF_RES[is.na(validation_data_2023$CODE_STIF_RES)] <- mean(validation_data_2023$CODE_STIF_RES, na.rm = TRUE)
#validation_data_2023$CODE_STIF_ARRET[is.na(validation_data_2023$CODE_STIF_ARRET)] <- mean(validation_data_2023$CODE_STIF_ARRET, na.rm = TRUE)

#=========================================> no missing value

####################### Replace ? by mode

clean_dataset <- function(df) {
 
  ############## Calculate the mode of the 'titre' column
  mode_value <- names(sort(table(df$CATEGORIE_TITRE), decreasing = TRUE))[1]
  
  ##################################### Replace "?" with mode value
  
  df$CATEGORIE_TITRE[df$CATEGORIE_TITRE == "?"] <- mode_value
  
  return(df)
}

######### Merge Data
validation_data_2023 <- clean_dataset(validation_data_2023)
names(validation_data_2023)[names(validation_data_2023) == "lda"] <- "ID_REFA_LDA"

View(validation_data_2023)


data_2022_NB_FER <- clean_dataset(data_2022_NB_FER)
data_2021_NB_FER <- clean_dataset(data_2021_NB_FER)
data_2020_NB_FER <- clean_dataset(data_2020_NB_FER)
data_2019_NB_FER <- clean_dataset(data_2019_NB_FER)
data_2018_NB_FER <- clean_dataset(data_2018_NB_FER)
data_2017_NB_FER <- clean_dataset(data_2017_NB_FER)


data_2022_NB_FER$JOUR <- format(as.Date(data_2022_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")
data_2021_NB_FER$JOUR <- format(as.Date(data_2021_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")
data_2020_NB_FER$JOUR <- format(as.Date(data_2020_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")
data_2019_NB_FER$JOUR <- format(as.Date(data_2019_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")
data_2018_NB_FER$JOUR <- format(as.Date(data_2018_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")
data_2017_NB_FER$JOUR <- format(as.Date(data_2017_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")


data_NB_FER <- rbind(validation_data_2023, data_2022_NB_FER,data_2020_NB_FER,data_2019_NB_FER,data_2018_NB_FER,data_2017_NB_FER)



names(validation_data_2023)
names(data_2022_NB_FER)
names(data_2021_NB_FER)
names(data_2020_NB_FER)
names(data_2019_NB_FER)
names(data_2018_NB_FER)
names(data_2017_NB_FER)


summary(validation_data_2023)
summary(data_2022_NB_FER)
summary(data_2021_NB_FER)
summary(data_2020_NB_FER)
summary(data_2019_NB_FER)
summary(data_2018_NB_FER)
summary(data_2017_NB_FER)



View(data_NB_FER)




################## 2 : EDA
names(validation_data_2023)

############################ Get unique values of 'ID_REFA_LDA' column
unique_values <- unique(data_NB_FER$NB_VALD)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)


data_NB_FER$JOUR <- as.Date(data_NB_FER$JOUR)


# Overall Trends
ggplot(data_NB_FER, aes(x = JOUR, y = NB_VALD)) +
  geom_line() +
  ggtitle("Overall Ridership Trends") +
  xlab("Date") +
  ylab("Number of Validations")


#######################################

# Extract month and year
data_NB_FER$Month <- month(data_NB_FER$JOUR, label = TRUE)
data_NB_FER$Year <- year(data_NB_FER$JOUR)

# Plot seasonality and monthly trends
ggplot(data_NB_FER, aes(x = Month, y = NB_VALD, group = Year, color = factor(Year))) +
  geom_line() +
  ggtitle("Seasonality and Monthly Trends") +
  xlab("Month") +
  ylab("Ridership") +
  scale_x_discrete(labels = month.name)






