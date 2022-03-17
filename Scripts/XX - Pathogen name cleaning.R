
# Author: Colin J. Carlson, Yasser Omar
# DONs versions: 1.1.0001 to 1.1.0002
# Content: fix the taxonomy of the outbreak disease names

library(tidyverse)

setwd("~/Github/dons")

raw <- read_csv("./Data/DON-1.1.0001.csv")

raw # Look at the data

raw %>% pull(`Disease/pathogen name`) %>% sort %>% unique # See the names

# This step below does the re-factoring. 

dictionary <-  c("Ebola" = "Ebola haemorrhagic fever",
                 "Yellow Fever" = "Yellow fever",
                 "Zika virus disease" = "Zika virus") # Turn all entries called "Ebola" into "Ebola haemorrhagic fever"

raw %>% mutate(`Disease/pathogen name` = recode(`Disease/pathogen name`, 
                                                !!!dictionary)) -> raw
