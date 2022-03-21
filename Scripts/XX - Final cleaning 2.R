
library(tidyverse); library(magrittr); library(lubridate)

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

dons <- read_csv("~/Github/dons/Data/DONdatabase.csv")

dons <- dons %>% arrange(DONid)