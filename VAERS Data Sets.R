# PURPOSE: To clean VAERS Data that is used for analysis


# Import data needed ------------------------------------------------------

library(tidyverse)

files <- list.files("~/ST-495-Fall-2022/FinalProjectRawData")

setwd("~/ST-495-Fall-2022/FinalProjectRawData")

vaersData <- bind_rows(lapply(files, read_csv))

setwd("~/ST-495-Fall-2022")


# Manipulate data how needed ----------------------------------------------


vaersDataUpdated <- vaersData %>%
  select(-VAERS_ID) %>%
  mutate(RECVDATE = as.Date(RECVDATE, tryFormats = "%m/%d/%Y"),
         RPT_DATE = as.Date(RPT_DATE, tryFormats = "%m/%d/%Y"),
         DATEDIED = as.Date(DATEDIED, tryFormats = "%m/%d/%Y"),
         VAX_DATE = as.Date(VAX_DATE, tryFormats = "%m/%d/%Y"),
         ONSET_DATE = as.Date(ONSET_DATE, tryFormats = "%m/%d/%Y"),
         TODAYS_DATE = as.Date(TODAYS_DATE, tryFormats = "%m/%d/%Y"),
         DIED = ifelse(is.na(DIED), "N", DIED),
         L_THREAT = ifelse(is.na(L_THREAT), "N", L_THREAT),
         ER_ED_VISIT = ifelse(is.na(ER_ED_VISIT), "N", ER_ED_VISIT),
         HOSPITAL = ifelse(is.na(HOSPITAL), "N", HOSPITAL),
         HOSPDAYS = ifelse(is.na(HOSPDAYS), 0, HOSPDAYS),
         X_STAY = ifelse(is.na(X_STAY), "N", X_STAY),
         DISABLE = ifelse(is.na(DISABLE), "N", DISABLE),
         RECOVD = ifelse(is.na(RECOVD), "U", RECOVD),
         V_ADMINBY = ifelse(is.na(V_ADMINBY), "UNK", V_ADMINBY),
         V_FUNDBY = ifelse(is.na(V_FUNDBY), "OTH", V_FUNDBY),
         BIRTH_DEFECT = ifelse(is.na(BIRTH_DEFECT), "N", BIRTH_DEFECT),
         OFC_VISIT = ifelse(is.na(OFC_VISIT), "N", OFC_VISIT)
         )



# Make files of new data --------------------------------------------------

write_csv(vaersDataUpdated, "~/ST-495-Fall-2022/FinalProjectUsedData/2016-21VAERSData.csv")

write_rds(vaersDataUpdated, "~/ST-495-Fall-2022/FinalProjectUsedData/2016-21VAERSData.rds")