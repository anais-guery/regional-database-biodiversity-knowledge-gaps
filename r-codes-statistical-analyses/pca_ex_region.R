library(sf)
library(dplyr)
library(FactoMineR)
library(corrr)
library(factoextra)
library(ade4)

#Prepare and clean the datasets
setwd("")
data <- st_read("")
data <- data %>%
  mutate(across(-c(geometry, maille10), ~ 1 - .x)) %>%
  rename_with(~ gsub("new_", "", .x), starts_with("new_"))
data_clean <- st_drop_geometry(data)
data_clean <- data %>%
  select(-bivobs, -gastobs, -araiobs) %>%
  mutate(maille10 = as.character(maille10))

data_clean <- data_clean %>% rename(Amphibians = amphobs)
data_clean <- data_clean %>% rename(Bats = chirobs)
data_clean <- data_clean %>% rename(Birds = oisobs)
data_clean <- data_clean %>% rename(Reptiles = reptobs)
data_clean <- data_clean %>% rename(Rhopalocera = rhopobs)
data_clean <- data_clean %>% rename(Mammals = mammobs)
data_clean <- data_clean %>% rename(Orthoptera = orthobs)
data_clean <- data_clean %>% rename(Odonates = odonign)

#Prepare dataset of administrative entities within the region
setwd("")
code_dept_df <- st_read("")

code_dept_df$maille10_F <- as.character(code_dept_df$maille10_F)
code_dept_df <- code_dept_df %>%
  rename("maille10" = "maille10_F")
code_dept_df <- st_drop_geometry(code_dept_df)
data_joined <- left_join(
  data_clean,
  code_dept_df %>% select(maille10, code_dept_),  
  by = "maille10"
)

data_no_geom <- st_drop_geometry(data_joined)

group_var <- as.factor(data_no_geom$code_dept_)

numerical_data <- data_no_geom %>%
  select(-code_dept_) %>%        
  select(where(is.numeric))     

data_no_geom$departement <- ifelse(data_no_geom$code_dept_ %in% c(64, 40, 33, 24, 47), "Aquitaine",
                                 ifelse(data_no_geom$code_dept_%in% c(16, 17, 79, 86), "Poitou-Charentes", "Limousin"))

group_var <- data_no_geom$departement 

valid_rows <- complete.cases(data_no_geom[, sapply(data_no_geom, is.numeric)]) &
  !is.na(group_var)

numerical_data <- data_no_geom[valid_rows, ] %>%
  select(where(is.numeric))

group_var <- as.factor(group_var[valid_rows])

#Perform PCA
data_normalized <- scale(numerical_data)
pca_result <- PCA(data_normalized, graph = FALSE)
