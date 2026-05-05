library(sf)
library(vegan)
library(dplyr)
library(ggrepel)
library(ggforce)

#Prepare knowledge gaps datasets
setwd("")

amphi <- st_read("")
chiro <- st_read("")
mamm <- st_read("")
oise <- st_read("")
rept <- st_read("")
odon <- st_read("")
orth <- st_read("")
rhop <- st_read("")

amphi <- amphi %>% rename(amphi = lacunesesp)
chiro <- chiro %>% rename(chiro = lacunesesp)
mamm <- mamm %>% rename(mamm = lacunesesp)
oise <- oise %>% rename(oise = lacunesesp)
rept <- rept %>% rename(rept = lacunesesp)
odon <- odon %>% rename(odon = lacunesesp)
orth <- orth %>% rename(orth = lacunesesp)
rhop <- rhop %>% rename(rhop = lacunesesp)

lacunes <- data.frame(Odonates = odon$odon, Orthoptera = orth$orth, Rhopalocera = rhop$rhop)
lacunes <- data.frame(lapply(lacunes, as.numeric))

#Prepare environmental variables datasets
variables_env <- rhop[, c("moyhotspot", "maxnatural", "moypressag" , "moypressfr", "moypressur")]
variables_env <- variables_env %>%
  rename(
    'Hotspots' = moyhotspot,
    'Naturalness' = maxnatural,
    'Agricultural pressure' = moypressag,
    'Frequentation pressure' = moypressfr,
    'Urbanization pressure' = moypressur
  )
variables_env <- data.frame(lapply(variables_env, as.numeric))
variables_env <- st_drop_geometry(variables_env)
variables_env <- variables_env %>% select(-geometry)
variables_env <- na.omit(variables_env)
lacunes <- na.omit(lacunes)

#Calculate redundancy analyses
resultat_rda <- rda(lacunes, variables_env)