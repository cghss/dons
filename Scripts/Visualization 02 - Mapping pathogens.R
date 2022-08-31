
library(tidyverse); library(magrittr)
library(classInt)
library(rgdal)
library(RColorBrewer)
library(rworldmap)
library(ggthemr)
library(LaCroixColoR)

dons <- read_csv("~/Github/dons/Data/DONdatabase.csv")

key <- c("JAP" = "JPN", "XKO" = "XKX")
dons %<>% mutate(ISO = recode(ISO, !!!key))

dons %>%
  count(ISO, DiseaseLevel1) %>%
#  mutate(n = (n>0)) %>%
  mutate(n = sqrt(n)) %>%
  complete(ISO, DiseaseLevel1, fill = list(n = 0)) %>% 
  pivot_wider(names_from = DiseaseLevel1, values_from = n) %>%
  na.omit() -> df

#### In principle not needed
# dons$ISO[!(dons$ISO %in% sPDF$ISO3)] %>% unique()


set.seed(1234)
k <- stats::kmeans(df[,-1], centers = 7, nstart = 10)
df$cluster <- k$cluster

df$cluster <- as.factor(df$cluster)
df %<>% select(ISO, cluster)

countries <- rnaturalearth::ne_countries()
countries %<>% st_as_sf() %>% 
  left_join(df, by = c("iso_a3" = "ISO")) %>% 
  as_Spatial()

# # Attach that all to the map
# sPDF <- joinCountryData2Map(df,
#                             joinCode = "ISO3",
#                             nameJoinColumn = "ISO")

cols <- c(lacroix_palettes$Pamplemousse[1,1:3],
          lacroix_palettes$PeachPear[1,3],
          lacroix_palettes$Pamplemousse[1,4:6])
spplot(countries, 'cluster', col.regions = rev(cols))

################

dons %>%
  count(ISO) -> df

library(viridisLite)

countries <- rnaturalearth::ne_countries()
countries %<>% st_as_sf() %>% 
  left_join(df, by = c("iso_a3" = "ISO")) %>% 
  as_Spatial()

cols <- c(lacroix_palettes$Pamplemousse[1,1:3],
          lacroix_palettes$PeachPear[1,3],
          lacroix_palettes$Pamplemousse[1,4:6])
spplot(countries, 'n', col.regions = viridisLite::mako(16, direction = -1))



