#' Prep BART PD Use of Force data for analysis

#' TODO:
#' load and prep penal code crosswalk file

library(tidyverse)
library(lubridate)

#BART DATA
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

#create normalized case charge code
bart_uof.df <-
bart_uof.df %>%
  mutate(charge_norm = str_replace_all(`RMS Case Charges`, c("\\(" = "", "\\)" = "", " " = "")),
         charge_norm = str_to_upper(charge_norm, local = "en"))

#PENAL CODE CROSSWALK
#load CPL crosswalk file
pc_crosswalk.df <- readxl::read_xlsx("source/cpl_pc_crosswalk .xlsx")

#create normalized case charge code and trim columns
pc_crosswalk.df <-
  pc_crosswalk.df %>%
  mutate(charge_norm = base::ifelse(code_type=="ZZ", statutory_code, paste0(code_type, stat_normed))) %>%
  select(type, code_type, statutory_code, stat_normed, charge_norm, cjis, literal_display, offense_descr)

#cleanup
remove(uof_2017.df, uof_2018.df, uof_2019.df, file_cols)


