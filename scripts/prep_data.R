#' Prep BART PD Use of Force data for analysis

#' TODO:
#' clean up and/or generalize case charges

library(tidyverse)
library(lubridate)

#load data from BART PD site
download.file("https://www.bart.gov/sites/default/files/docs/2017%20UOF%20Data%2020201011%20Redacted_0.xlsx", "source/UOF_2017.xlsx")
download.file("https://www.bart.gov/sites/default/files/docs/2018%20UOF%20Data%2020201011%20Redacted_0.xlsx", "source/UOF_2018.xlsx")
download.file("https://www.bart.gov/sites/default/files/docs/2019%20UOF%20Data%2020201011%20Redacted_0.xlsx", "source/UOF_2019.xlsx")

#define column types
file_cols <- c("text", "text", "text", "text", "text", "date", "date", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text")

#convert to data frames
uof_2017.df <- readxl::read_xlsx("source/UOF_2017.xlsx", col_types = file_cols)
uof_2018.df <- readxl::read_xlsx("source/UOF_2018.xlsx", col_types = file_cols)
uof_2019.df <- readxl::read_xlsx("source/UOF_2019.xlsx", col_types = file_cols)

#combine into one data frame
bart_uof.df <- bind_rows(uof_2017.df, uof_2018.df, uof_2019.df)

#add columns: incident timestamp, citizen age
bart_uof.df <-
  bart_uof.df %>%
  mutate(incident_timestamp = ymd_hms(paste(`Occurred date`, format(`Occurred time`, format = "%H:%M:%S"))),
         citizen_age = round(time_length(interval(`Citizen Date-of-birth`, today()), "year"))
         )

#flatten case charges to one per row, remove duplicates
bart_uof.df <-
  bart_uof.df %>%
  separate_rows(`RMS Case Charges`, sep = ", ") %>%
  distinct()

#cleanup
remove(uof_2017.df, uof_2018.df, uof_2019.df, file_cols)


