
# Author: Colin J. Carlson
# DONs versions: 1.0 to 1.1.0001
# Content: extracts the WHO taxonomy from URLs, and then 

library(tidyverse)

setwd("~/Github/dons")

raw <- read_csv("./Data/DONraw.csv")

# Test of the approach

raw %>% fill(`Year(s) of outbreak`) %>% View()

# OK, these are the columsn that need fixing

raw %>% fill(`Year(s) of outbreak`, `Date recorded`, `Disease/pathogen name`, `Link`) -> fix

View(fix)

# Let's add a taxonomy for the unique DONs consistent with the WHO taxonomy

fix %>% mutate(DONid = gsub('https://www.who.int/csr/don/','DON-',Link)) %>%
  mutate(DONid = gsub("/en/", "", DONid)) %>% 
  mutate(DONid = gsub("_","-", DONid)) -> fix

# Write out

write_csv(fix, 'DON-1.1.0001.csv')
