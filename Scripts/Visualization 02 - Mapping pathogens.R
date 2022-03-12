
library(tidyverse); library(magrittr)
library(classInt)
library(rgdal)
library(RColorBrewer)
library(rworldmap)
library(ggthemr)
library(LaCroixColoR)

dons <- read_csv("~/Github/dons/Data/DON-1.2.0002.csv")

key <- c("JAP" = "JPN", "XKO" = "XKX")
dons %<>% mutate(ISO = recode(ISO, !!!key))

dons %>%
  count(ISO, DiseaseLevel1) %>%
#  mutate(n = (n>0)) %>%
  mutate(n = log(n+1)) %>%
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

# Attach that all to the map
sPDF <- joinCountryData2Map(df,
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO")

cols <- c(lacroix_palettes$Pamplemousse[1,1:3],
          lacroix_palettes$PeachPear[1,3],
          lacroix_palettes$Pamplemousse[1,4:6])
spplot(sPDF, 'cluster', col.regions = cols)

################


dons %>%
  count(ISO) -> df

library(viridisLite)

# Attach that all to the map
sPDF <- joinCountryData2Map(df,
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO")

cols <- c(lacroix_palettes$Pamplemousse[1,1:3],
          lacroix_palettes$PeachPear[1,3],
          lacroix_palettes$Pamplemousse[1,4:6])
spplot(sPDF, 'n', col.regions = viridisLite::mako(16, direction = -1))



