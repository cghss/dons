
library(tidyverse); library(magrittr); library(lubridate)

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

dons <- read_csv("~/Github/dons/Data/DON-1.2.0002.csv")

dons %<>% select(-'Case-Fatality ratio')
dons %<>% select(-'RowID')
dons %<>% select(-'Index')
dons %<>% select(-'PrimaryID')
dons %<>% select(-'Etiology')
dons %<>% select(-'Event')
dons %<>% select(-'Duration')

dons %<>% rename(DONid = DonID)
dons %<>% rename(ReportDate = DonDate)


### Clean and eliminate YearEvent

dons %>% mutate(ReportYear = mdy(ReportDate),
                ReportYear = year(ReportYear)) %>% 
  mutate(Match = (ReportYear==YearEvent)) %>%
  filter(Match==FALSE) %>% View()

dons$ReportDate[dons$DONid=='DON-2019-02-27-a'] <- '02/27/2014'
dons$DONid[dons$DONid=='DON-2019-02-27-a'] <- 'DON-2014-02-27-a'
dons$ReportDate[dons$DONid=='DON-2019-02-27-b'] <- '02/27/2014'
dons$DONid[dons$DONid=='DON-2019-02-27-b'] <- 'DON-2014-02-27-b'
dons$ReportDate[dons$DONid=='DON-2015-02-02'] <- '02/02/2016'
dons$DONid[dons$DONid=='DON-2015-02-02'] <- 'DON-2016-02-02'

dons %<>% select(-YearEvent)

##### Date Cleaning Chunk: `OutbreakStart`

dons %>% 
  filter(!is.na(`Outbreak start`)) %>%
  mutate(Test1 = is.na(mdy(`Outbreak start`)),
         Test2 = is.na(my(`Outbreak start`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE) %>% View()

code <- c('no' = NA,
          'Beginning 2018' = '??/??/2018',
          'Beginning March' = '03/??/2018',
          'Beginning of 2018' = "??/??/2018",
          'Centro Sur, Bioko Norte, Litoral' = NA,
          'Early 1999' = '??/??/1999',
          'Early 2017' = '??/??/2017',
          'Early January' = '01/??/1999',
          'Early March' = '03/??/2018',
          'Early October' = '10/??/2010',
          'end 1997' = '??/??/1997',
          'End of September' = '09/??/2013',
          'last months 2000' = '??/??/2000',
          'Last week of October' = '10/??/2006',
          'late 1997' = '??/??/1997',
          'late 2017' = '??/??/2017',
          'mid-1997' = '??/??/1997',
          'mid-January' = '01/??/1999',
          'Start of 2017' = '??/??/2017')

dons %>% mutate(`Outbreak start` = recode(`Outbreak start`, !!!code)) %>% View()
# beginning December 2003	

dons %>% mutate(`Outbreak start` = recode(`Outbreak start`, !!!code)) %>%
  mutate(OutbreakStartYear = NA,
         OutbreakStartMonth = NA,
         OutbreakStartDay = NA) %>%
  
  mutate(DateTest1 = is.na(my(`Outbreak start`))) %>% # Do they have days?
   
  mutate_cond(DateTest1 == TRUE, OutbreakStartYear = year(mdy(`Outbreak start`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, OutbreakStartMonth = month(mdy(`Outbreak start`))) %>% 
  mutate_cond(DateTest1 == TRUE, OutbreakStartDay = day(mdy(`Outbreak start`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, OutbreakStartYear = year(my(`Outbreak start`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, OutbreakStartMonth = month(my(`Outbreak start`))) %>% 
  
  mutate(DateTest2 = !str_detect(`Outbreak start`, '\\/')) %>%  # If they're textual (e.g., "beginning October 2019"), just do month year, i.e., overwrite day
  mutate_cond(DateTest2 == TRUE & !is.na(`Outbreak start`), OutbreakStartDay = NA) %>% #overwrite day
  
  mutate(DateTest3 = str_detect(`Outbreak start`, "\\?\\?/\\?\\?/")) %>%
  mutate_cond(DateTest3 == TRUE & !is.na(`Outbreak start`), OutbreakStartYear = as.numeric(str_replace(`Outbreak start`, "\\?\\?/\\?\\?/", ''))) %>%
  
  # select(`Outbreak start`, OutbreakStartYear, OutbreakStartMonth, OutbreakStartDay) %>% View() # Check how this has worked
  select(-`Outbreak start`) %>%
  select(-DateTest1) %>% 
  select(-DateTest2) %>% 
  select(-DateTest3) -> dons


##### Date Cleaning Chunk: `OutbreakEnd`

dons %>% 
  filter(!is.na(`Outbreak end`)) %>%
  mutate(Test1 = is.na(mdy(`Outbreak end`)),
         Test2 = is.na(my(`Outbreak end`)),
         Test3 = is.na(mdy_hm(`Outbreak end`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE & Test3 == TRUE) %>% View()

dons %>%
  mutate(`Outbreak end` = na_if(dons$`Outbreak end`, 'no')) %>%
  
  mutate(OutbreakEnd = NA) %>% 
  mutate_cond(!is.na(`Outbreak end`), OutbreakEnd = 'yes') %>% 
  
  mutate(OutbreakEndYear = NA,
         OutbreakEndMonth = NA,
         OutbreakEndDay = NA) %>%
  
  mutate(DateTest1 = !is.na(mdy_hm(`Outbreak end`))) %>% # Anything there?
  mutate_cond(DateTest1 == TRUE, OutbreakEndYear = year(mdy_hm(`Outbreak end`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, OutbreakEndMonth = month(mdy_hm(`Outbreak end`))) %>% 
  mutate_cond(DateTest1 == TRUE, OutbreakEndDay = day(mdy_hm(`Outbreak end`))) %>% 
  
  select(-`Outbreak end`) %>%
  select(-DateTest1) -> dons






##### Date Cleaning Chunk: `OutbreakDetection`


dons %>% 
  filter(!is.na(`Outbreak detection`)) %>%
  mutate(Test1 = is.na(mdy(`Outbreak detection`)),
         Test2 = is.na(my(`Outbreak detection`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE) %>% View()



code <- c('Last 2 weeks of May' = '05/??/2005',
          'Early October' = '10/??/2013')

dons %>%
  mutate(`Outbreak detection` = na_if(dons$`Outbreak detection`, 'no')) %>%
  
  mutate(OutbreakDetection = NA) %>% 
  mutate_cond(!is.na(`Outbreak detection`), OutbreakDetection = 'yes') %>% 
  
  mutate(`Outbreak detection` = recode(`Outbreak detection`, !!!code)) %>%
  
  mutate(DateTest1 = is.na(my(`Outbreak detection`))) %>% # Do they have days?
  
  mutate(OutbreakDetectionYear = NA,
         OutbreakDetectionMonth = NA,
         OutbreakDetectionDay = NA) %>%
  
  mutate_cond(DateTest1 == TRUE, OutbreakDetectionYear = year(mdy(`Outbreak detection`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, OutbreakDetectionMonth = month(mdy(`Outbreak detection`))) %>% 
  mutate_cond(DateTest1 == TRUE, OutbreakDetectionDay = day(mdy(`Outbreak detection`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, OutbreakDetectionYear = year(my(`Outbreak detection`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, OutbreakDetectionMonth = month(my(`Outbreak detection`))) %>% 
  
  mutate(DateTest2 = !str_detect(`Outbreak detection`, '/')) %>%  # If they're textual (e.g., "beginning October 2019"), just do month year, i.e., overwrite day
  mutate_cond(DateTest2 == TRUE & !is.na(`Outbreak detection`), OutbreakDetectionDay = NA) %>% #overwrite day

  select(-`Outbreak detection`) %>%
  select(-DateTest1) %>% 
  select(-DateTest2) -> dons





##### Date Cleaning Chunk: `NotificationLocal`


dons %>% 
  filter(!is.na(`Outbreak notification`)) %>%
  mutate(Test1 = is.na(mdy(`Outbreak notification`)),
         Test2 = is.na(my(`Outbreak notification`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE) %>% View()

code <- c('Beginning 1997' = '??/??/1997',
          'beginning 2000' = '??/??/2000',
          'beginning 2004' = '??/??/2004',
          'beginning July' = '07/??/2012',
          'Beginning July' = '07/??/2011',
          'beginning October' = '10/??/1998',
          'Beginnning 2004' = '??/??/2004',
          'First week December' = '??/??/1998',
          'Last week of March' = '03/??/1999',
          'Second week of may' = '05/??/2011')


dons %>%
  mutate(`Outbreak notification` = na_if(dons$`Outbreak notification`, 'no')) %>%
  
  mutate(NotificationLocal = NA) %>% 
  mutate_cond(!is.na(`Outbreak notification`), NotificationLocal = 'yes') %>% 
  
  mutate(`Outbreak notification` = recode(`Outbreak notification`, !!!code)) %>%
  
  mutate(DateTest1 = is.na(my(`Outbreak notification`))) %>% # Do they have days?
  
  mutate(NotificationLocalYear = NA,
         NotificationLocalMonth = NA,
         NotificationLocalDay = NA) %>%
  
  mutate_cond(DateTest1 == TRUE, NotificationLocalYear = year(mdy(`Outbreak notification`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, NotificationLocalMonth = month(mdy(`Outbreak notification`))) %>% 
  mutate_cond(DateTest1 == TRUE, NotificationLocalDay = day(mdy(`Outbreak notification`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, NotificationLocalYear = year(my(`Outbreak notification`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, NotificationLocalMonth = month(my(`Outbreak notification`))) %>% 
  
  mutate(DateTest2 = !str_detect(`Outbreak notification`, '/')) %>%  # If they're textual (e.g., "beginning October 2019"), just do month year, i.e., overwrite day
  mutate_cond(DateTest2 == TRUE & !is.na(`Outbreak notification`), NotificationLocalDay = NA) %>% #overwrite day
  
  mutate(DateTest3 = str_detect(`Outbreak notification`, "\\?\\?/\\?\\?/")) %>%
  mutate_cond(DateTest3 == TRUE & !is.na(`Outbreak notification`), NotificationLocalYear = as.numeric(str_replace(`Outbreak notification`, "\\?\\?/\\?\\?/", ''))) %>%
  
  select(-`Outbreak notification`) %>%
  select(-DateTest1) %>% 
  select(-DateTest2) %>% 
  select(-DateTest3) -> dons

View(dons)



##### Date Cleaning Chunk: `NotificationWHO`


dons %>% 
  filter(!is.na(`Outbreak notification to WHO`)) %>%
  mutate(Test1 = is.na(mdy(`Outbreak notification to WHO`)),
         Test2 = is.na(my(`Outbreak notification to WHO`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE) %>% View()

code <- c('5/5/2016' = '05/05/2016',
          '9/13/2020-10/1/2020' = '09/13/2020',
          'beginning 2002' = '??/??/2002',
          'beginning 2003' = '??/??/2003',
          'Beginning 2012' = '??/??/2012',
          'Early May 1996' = '05/??/1996',
          'Late May 1997' = '05/??/1997',
          'third week May 2001' = '05/??/2001')

dons %>%
  
  mutate(NotificationWHO = NA) %>% 
  mutate_cond(!is.na(`Outbreak notification to WHO`), NotificationWHO = 'yes') %>% 
  mutate_cond(`Outbreak notification to WHO`=='no' & !is.na(`Outbreak notification to WHO`), NotificationWHO = 'no') %>%   # the old........switcheroo
  
  mutate(`Outbreak notification to WHO` = recode(`Outbreak notification to WHO`, !!!code)) %>%
  
  mutate(DateTest1 = is.na(my(`Outbreak notification to WHO`))) %>% # Do they have days?
  
  mutate(NotificationWHOYear = NA,
         NotificationWHOMonth = NA,
         NotificationWHODay = NA) %>%
  
  mutate_cond(DateTest1 == TRUE, NotificationWHOYear = year(mdy(`Outbreak notification to WHO`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, NotificationWHOMonth = month(mdy(`Outbreak notification to WHO`))) %>% 
  mutate_cond(DateTest1 == TRUE, NotificationWHODay = day(mdy(`Outbreak notification to WHO`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, NotificationWHOYear = year(my(`Outbreak notification to WHO`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, NotificationWHOMonth = month(my(`Outbreak notification to WHO`))) %>% 
  
  mutate(DateTest2 = !str_detect(`Outbreak notification to WHO`, '/')) %>%  # If they're textual (e.g., "beginning October 2019"), just do month year, i.e., overwrite day
  mutate_cond(DateTest2 == TRUE & !is.na(`Outbreak notification to WHO`), NotificationWHODay = NA) %>% #overwrite day
  
  mutate(DateTest3 = str_detect(`Outbreak notification to WHO`, "\\?\\?/\\?\\?/")) %>%
  mutate_cond(DateTest3 == TRUE & !is.na(`Outbreak notification to WHO`), NotificationWHOYear = as.numeric(str_replace(`Outbreak notification to WHO`, "\\?\\?/\\?\\?/", ''))) %>%

  select(-`Outbreak notification to WHO`) %>%
  select(-DateTest1) %>% 
  select(-DateTest2) %>% 
  select(-DateTest3) -> dons

View(dons)





##### Date Cleaning Chunk: `OutbreakVerification`

dons %>% 
  filter(!is.na(`Outbreak verification`)) %>%
  mutate(Test1 = is.na(mdy(`Outbreak verification`)),
         Test2 = is.na(my(`Outbreak verification`)),
         Test3 = is.na(mdy_hm(`Outbreak verification`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE & Test3 == TRUE) %>% View()


code <- c('5/??/2016' = '05/??/2016')

dons %>%
  
  mutate(OutbreakVerification = NA) %>% 
  mutate_cond(!is.na(`Outbreak verification`), OutbreakVerification = 'yes') %>% 
  
  mutate(`Outbreak verification` = recode(`Outbreak verification`, !!!code)) %>%
  
  mutate(OutbreakVerificationYear = NA,
         OutbreakVerificationMonth = NA,
         OutbreakVerificationDay = NA) %>%
  
  mutate(DateTest1 = !is.na(mdy_hm(`Outbreak verification`))) %>% # Anything there?
  mutate_cond(DateTest1 == TRUE, OutbreakVerificationYear = year(mdy_hm(`Outbreak verification`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, OutbreakVerificationMonth = month(mdy_hm(`Outbreak verification`))) %>% 
  mutate_cond(DateTest1 == TRUE, OutbreakVerificationDay = day(mdy_hm(`Outbreak verification`))) %>% 
  
  mutate_cond(`Outbreak verification`=='05/??/2016' & !is.na(`Outbreak verification`), OutbreakVerificationYear = 2016) %>% # If they have days, include days!
  mutate_cond(`Outbreak verification`=='05/??/2016' & !is.na(`Outbreak verification`), OutbreakVerificationMonth = 05) %>% 
  
  # mutate(DateTest2 = !is.na(my(`Outbreak verification`))) %>% Don't know why this isn't working but whatever - above chunk does it
  # mutate_cond(DateTest2 == TRUE, OutbreakVerificationYear = year(my(`Outbreak verification`))) %>% # If they have days, include days!
  # mutate_cond(DateTest2 == TRUE, OutbreakVerificationMonth = month(my(`Outbreak verification`))) %>% View()
  
  select(-DateTest1) %>%
  select(-`Outbreak verification`) -> dons
   




##### Date Cleaning Chunk: `LabConfirmation`

dons %>% 
  filter(!is.na(`Laboratory confirmation`)) %>%
  mutate(Test1 = is.na(mdy(`Laboratory confirmation`)),
         Test2 = is.na(my(`Laboratory confirmation`)),
         Test3 = is.na(mdy_hm(`Laboratory confirmation`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE & Test3 == TRUE) %>% View()


code <- c('yes*' = 'yes',
          'no*' = 'no',
          'Beginning May 2008' = '05/??/2008',
          'early August 2005' = '08/??/2005',
          'early August 2005' = '08/??/2005',
          'mid-December 2003' = '12/??/2003',
          'ongoing' = 'no')
          

dons %>% 
  
  mutate(`Laboratory confirmation` = str_replace(`Laboratory confirmation`, ' 0:00', '')) %>% 
  
  mutate(LabConfirmation = NA) %>% 
  mutate_cond(!is.na(`Laboratory confirmation`), LabConfirmation = 'yes') %>% 
  mutate_cond(`Laboratory confirmation`=='no' & !is.na(`Laboratory confirmation`), LabConfirmation = 'no') %>%   # the old........switcheroo
  
  mutate(`Laboratory confirmation` = recode(`Laboratory confirmation`, !!!code)) %>%
  
  mutate(DateTest1 = is.na(my(`Laboratory confirmation`))) %>% # Do they have days?
  
  mutate(LabConfirmationYear = NA,
         LabConfirmationMonth = NA,
         LabConfirmationDay = NA) %>%
  
  mutate_cond(DateTest1 == TRUE, LabConfirmationYear = year(mdy(`Laboratory confirmation`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, LabConfirmationMonth = month(mdy(`Laboratory confirmation`))) %>% 
  mutate_cond(DateTest1 == TRUE, LabConfirmationDay = day(mdy(`Laboratory confirmation`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, LabConfirmationYear = year(my(`Laboratory confirmation`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, LabConfirmationMonth = month(my(`Laboratory confirmation`))) %>% 
  
  mutate(DateTest3 = str_detect(`Laboratory confirmation`, "\\?\\?/\\?\\?/")) %>%
  mutate_cond(DateTest3 == TRUE & !is.na(`Laboratory confirmation`), LabConfirmationYear = as.numeric(str_replace(`Laboratory confirmation`, "\\?\\?/\\?\\?/", ''))) %>% 
  
  select(-`Laboratory confirmation`) %>%
  select(-DateTest1) %>% 
  select(-DateTest3) -> dons
  




##### Date Cleaning Chunk: `PHIntervention`

dons %>% 
  filter(!is.na(`Public health intervention`)) %>%
  mutate(Test1 = is.na(mdy(`Public health intervention`)),
         Test2 = is.na(my(`Public health intervention`)),
         Test3 = is.na(mdy_hm(`Public health intervention`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE & Test3 == TRUE) %>% View()

code <- c('Early October 1999' = '10/??/1999',
          'mid-August 1998' = '08/??/1998')


dons %>% 
  
  mutate(`Public health intervention` = str_replace(`Public health intervention`, ' 0:00', '')) %>% 
  
  mutate(PHIntervention = NA) %>% 
  mutate_cond(!is.na(`Public health intervention`), PHIntervention = 'yes') %>% 
  mutate_cond(`Public health intervention`=='no' & !is.na(`Public health intervention`), PHIntervention = 'no') %>%   # the old........switcheroo
  
  mutate(`Public health intervention` = recode(`Public health intervention`, !!!code)) %>%
  
  mutate(DateTest1 = is.na(my(`Public health intervention`))) %>% # Do they have days?
  
  mutate(PHInterventionYear = NA,
         PHInterventionMonth = NA,
         PHInterventionDay = NA) %>%
  
  mutate_cond(DateTest1 == TRUE, PHInterventionYear = year(mdy(`Public health intervention`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, PHInterventionMonth = month(mdy(`Public health intervention`))) %>% 
  mutate_cond(DateTest1 == TRUE, PHInterventionDay = day(mdy(`Public health intervention`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, PHInterventionYear = year(my(`Public health intervention`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, PHInterventionMonth = month(my(`Public health intervention`))) %>% 
  
  mutate(DateTest3 = str_detect(`Public health intervention`, "\\?\\?/\\?\\?/")) %>%
  mutate_cond(DateTest3 == TRUE & !is.na(`Public health intervention`), PHInterventionYear = as.numeric(str_replace(`Public health intervention`, "\\?\\?/\\?\\?/", ''))) %>% 
  
  select(-`Public health intervention`) %>%
  select(-DateTest1) %>% 
  select(-DateTest3) -> dons



##### Date Cleaning Chunk: `PHCommunication`

dons %>% 
  filter(!is.na(`Public communication`)) %>%
  mutate(Test1 = is.na(mdy(`Public communication`)),
         Test2 = is.na(my(`Public communication`)),
         Test3 = is.na(mdy_hm(`Public communication`))) %>%
  filter(Test1 == TRUE & Test2 == TRUE & Test3 == TRUE) %>% View()



dons %>% 
  
  mutate(`Public communication` = str_replace(`Public communication`, ' 0:00', '')) %>% 
  
  mutate(PHCommunication = NA) %>% 
  mutate_cond(!is.na(`Public communication`), PHCommunication = 'yes') %>% 

  mutate(DateTest1 = is.na(my(`Public communication`))) %>% # Do they have days?
  
  mutate(PHCommunicationYear = NA,
         PHCommunicationMonth = NA,
         PHCommunicationDay = NA) %>%
  
  mutate_cond(DateTest1 == TRUE, PHCommunicationYear = year(mdy(`Public communication`))) %>% # If they have days, include days!
  mutate_cond(DateTest1 == TRUE, PHCommunicationMonth = month(mdy(`Public communication`))) %>% 
  mutate_cond(DateTest1 == TRUE, PHCommunicationDay = day(mdy(`Public communication`))) %>% 
  
  mutate_cond(DateTest1 == FALSE, PHCommunicationYear = year(my(`Public communication`))) %>% # If they don't have days, do not!
  mutate_cond(DateTest1 == FALSE, PHCommunicationMonth = month(my(`Public communication`))) %>% 
  
  select(-`Public communication`) %>%
  select(-DateTest1)  -> dons


########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################

# Final organization, renaming, and text cleaning

dons %>%
  rename(OutbreakEpicenter = 'Outbreak epicenter',
         CasesConfirmed = 'Confirmed cases',
         CasesProbable = 'Probable cases',
         CasesSuspected = 'Suspected cases',
         CasesTotal = 'Total cases',
         Deaths = 'Death count',
         MassGathering = 'Mass gatherings',
         CommunityResistance = 'Community resistance') %>%
  
  select(DONid, Headline, ReportDate, Link,
         DiseaseLevel1, DiseaseLevel2,
         Country, ISO, OutbreakEpicenter,
         CasesTotal, CasesSuspected, CasesProbable, CasesConfirmed, Deaths,
         MassGathering, 
         
         OutbreakStartYear, OutbreakStartMonth, OutbreakStartDay,
         OutbreakDetection, OutbreakDetectionYear, OutbreakDetectionMonth, OutbreakDetectionDay,
         OutbreakVerification, OutbreakVerificationYear, OutbreakVerificationMonth, OutbreakVerificationDay,
         LabConfirmation, LabConfirmationYear, LabConfirmationMonth, LabConfirmationDay,
         NotificationLocal, NotificationLocalYear, NotificationLocalMonth, NotificationLocalDay,
         NotificationWHO, NotificationWHOYear, NotificationWHOMonth, NotificationWHODay,
         
         PHCommunication, PHCommunicationYear, PHCommunicationMonth, PHCommunicationDay,
         PHIntervention, PHInterventionYear, PHInterventionMonth, PHInterventionDay,
         CommunityResistance,
         OutbreakEnd, OutbreakEndYear, OutbreakEndMonth, OutbreakEndDay,
         Notes) -> dons

dons %>% mutate_all(str_replace_all, "<e9>","e") %>%
  mutate_all(str_replace_all, "<e7>","c") %>% 
  mutate_all(str_replace_all, "<e1>","a") %>% 
  mutate_all(str_replace_all, "<ea>","e") %>% 
  mutate_all(str_replace_all, "<f1>","n") %>%  
  mutate_all(str_replace_all, "<f4>","o") %>%  
  mutate_all(str_replace_all, "<ed>","i") %>%  
  mutate_all(str_replace_all, "<e3>","a") %>%  
  mutate_all(str_replace_all, "<d3>","O") %>%  
  mutate_all(str_replace_all, "<f3>","o") %>%  
  mutate_all(str_replace_all, "<e2>","a") %>% 
  #mutate_all(str_replace_all, "<U+009C>","oe") %>%
  mutate_all(str_replace_all, "<ef>","i") %>%
  mutate_all(str_replace_all, "\u0096", "-") %>%
  mutate_all(str_replace_all, "???", "-") %>%
  mutate_all(str_replace_all, "Cap Verde", "Cape Verde") -> dons

dons[dons$DONid=='DON-2016-01-08-b' & dons$Country=='Martinique',]$OutbreakEpicenter <- 'Schoelcher'


dons %>%
  mutate(Link = str_replace(Link, "https://www.who.int/csr/don/", "https://www.who.int/emergencies/disease-outbreak-news/item/")) %>%
  mutate(Link = str_replace(Link, "/en/", "-en/")) %>% 
  mutate(ISO = str_replace(ISO, "JAP", "JPN")) -> dons
  

write_csv(dons, "~/Github/dons/Data/DONdatabase.csv")
