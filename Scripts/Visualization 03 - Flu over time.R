
library(tidyverse); library(magrittr); library(lubridate)

dons <- read_csv("~/Github/dons/Data/DONdatabase.csv")

library(extrafont)
loadfonts(device = "win", quiet = TRUE)

lcr <- wesanderson::wes_palette("Rushmore1", n = 5)[c(1,5,3,4,2)]

hxnx <- data.frame(
  DiseaseLevel2 = c('H1N1', 'H1N2', 'H3N2', 'H5N1', 'H5N6', 'H5NX', 
                    'H7N2', 'H7N4', 'H7N7', 'H7N9', 'H7NX', 'H9N2'),
  H = c('H1NX', 'H1NX', 'H3NX', 'H5NX', 'H5NX', 'H5NX', 'H7NX', 'H7NX', 'H7NX', 'H7NX', 'H7NX', 'H9NX')
)


dons %>% filter(DiseaseLevel1 == 'Influenza A') %>%
  mutate(YearEvent = year(mdy(ReportDate))) %>%
  select(DiseaseLevel2,YearEvent) %>%
  mutate(DiseaseLevel2 = str_replace(DiseaseLevel2, ": S-OtrH3N2", "")) %>% 
  na.omit() %>%
  left_join(hxnx) %>% 
ggplot(aes(x = YearEvent, y = DiseaseLevel2, color = H)) + 
  geom_point(size = 8, alpha = 0.2, stroke = 0.175, legend = FALSE) + 
  xlab("") + ylab("") + theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.3, color="grey75"), 
    text = element_text(family = "Bahnschrift", size = 17),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank()) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))  +
  scale_y_discrete(limits=rev) + 
  scale_color_manual(values = lcr)
