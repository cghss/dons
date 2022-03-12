
library(tidyverse); library(magrittr)
dons <- read_csv("~/Github/dons/Data/DON-1.2.0002.csv")

dons %<>% mutate(DiseaseLevel1 = str_replace(DiseaseLevel1, "Crimean-Congo haemorrhagic fever", "CCHF"))
dons %>% count(DiseaseLevel1) %>% top_n(25) %>% pull(DiseaseLevel1) -> top20
dons %<>% filter(DiseaseLevel1 %in% top20)
dons %>% group_by(DiseaseLevel1, YearEvent) %>%
  count() %>% ungroup %>% complete(DiseaseLevel1, YearEvent, fill = list(n = 0)) -> df
  
df %>% group_by(DiseaseLevel1) %>% 
  summarize(total = sum(n)) -> df2

df %<>% left_join(df2) %>% 
  mutate(n = 100*n/total)

df %>% ggplot(aes(x = YearEvent, y = n, fill = DiseaseLevel1)) + 
  geom_area() + 
  xlab("") + ylab("Proportion of reports (%)") + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 14, vjust = 6),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
  facet_wrap(~ DiseaseLevel1, nrow = 5)  -> bottom

####################################

library(tidyverse); library(magrittr)
dons <- read_csv("~/Github/dons/Data/DON-1.2.0002.csv")

dons %>% count(DiseaseLevel1) %>% top_n(4) %>% pull(DiseaseLevel1) -> top20
dons %>% pull(DiseaseLevel1) %>% unique -> all
all <- all[!(all %in% top20)]

for(i in 1:length(all)){
  dons %<>% mutate(DiseaseLevel1 = str_replace(DiseaseLevel1, all[i], 'Other'))}

dons %>% group_by(DiseaseLevel1, YearEvent) %>%
  count() %>% ungroup %>% complete(DiseaseLevel1, YearEvent, fill = list(n = 0)) -> df

df$DiseaseLevel1 %>% unique() -> levs
levs <- c(levs[!levs %in% c("Other")], "Other")
df %<>% mutate(DiseaseLevel1 = factor(DiseaseLevel1, levels = levs))

# colors

library(scales)
rainbow <- hue_pal()(25)

df %>% ggplot(aes(x = YearEvent, y = n, fill = DiseaseLevel1)) + 
  geom_area(size=.5, colour='white') + 
  xlab("") + ylab("Number of reports") + 
  theme_light() + 
  scale_fill_manual(values = c(rainbow[c(4,7,9,16)],'grey70')) + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.92,0.73),
        #legend.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 14, vjust = 6),
        plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) -> top

#####

library(patchwork)

top / bottom + plot_layout(heights = c(1,2.5))
