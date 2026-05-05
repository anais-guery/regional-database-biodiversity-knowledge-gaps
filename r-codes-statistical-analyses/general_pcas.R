library(sf)
library(dplyr)
library(FactoMineR)
library(corrr)
library(factoextra)
library(ggplot2)

#Prepare datasets
setwd("")
data <- st_read("")

setwd("")
code_dept_df <- st_read("")

code_dept_df$maille10_F <- as.character(code_dept_df$maille10_F)
code_dept_df <- code_dept_df %>%
  rename("maille10" = "maille10_F")
code_dept_df <- st_drop_geometry(code_dept_df)
data_joined <- left_join(
  data,
  code_dept_df %>% select(maille10, code_dept_),  
  by = "maille10"
)

#Only keep the environmental variables of interest
data_joined <- st_drop_geometry(data_joined)
data_joined <- subset(data_joined, select = -moyhotsp_1)
data_joined <- subset(data_joined, select = -moypresssy)
data_joined <- subset(data_joined, select = -maille10)
data_joined <- subset(data_joined, select = -moynatural)
data_joined <- subset(data_joined, select = -lacunesesp)
data_joined <- subset(data_joined, select = -ignorancei)
data_joined <- subset(data_joined, select = -partage_es)

#Rename environmental variables
colnames(data_joined)[colnames(data_joined) == 'moyhotspot'] <- 'Hotspots'
colnames(data_joined)[colnames(data_joined) == 'maxnatural'] <- 'Naturalness'
colnames(data_joined)[colnames(data_joined) == 'moypressag'] <- 'Agricultural pressure'
colnames(data_joined)[colnames(data_joined) == 'moypressfr'] <- 'Frequentation pressure'
colnames(data_joined)[colnames(data_joined) == 'moypressur'] <- 'Urbanization pressure'
colnames(data_joined)[colnames(data_joined) == 'moydensite'] <- 'Road density'
colnames(data_joined)[colnames(data_joined) == 'pctzonespr'] <- 'Protected areas'

#Get ex-regions names
data_joined$new_group <- ifelse(data_joined$code_dept_ %in% c(64, 40, 33, 24, 47), "Aquitaine",
                                 ifelse(data_joined$code_dept_%in% c(16, 17, 79, 86), "Poitou-Charentes", "Limousin"))

group_var <- data_joined$new_group 

numerical_data <- data_joined %>%
  select(-code_dept_) %>%
  select(where(is.numeric))

valid_rows <- complete.cases(data_joined[, sapply(data_joined, is.numeric)]) &
  !is.na(group_var)

numerical_data <- data_joined[valid_rows, ] %>%
  select(where(is.numeric))

group_var <- as.factor(group_var[valid_rows])

group_colors <- c("skyblue", "darkgoldenrod3", "mediumaquamarine", "coral1", "plum") # Custom colors for groups
var_color <- "black" # Custom color for variable arrows

#Perform PCA
data_normalized <- scale(numerical_data)
pca_result <- PCA(data_normalized, graph = FALSE)