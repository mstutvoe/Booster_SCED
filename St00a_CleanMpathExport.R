# laden booster export & cleaning
#https://m-path.io/mpathr/guide.html
## more info on how to handle M-path data 

## conclusion on some output vars-----
# start_date not to be used, because can be missing if tb NA
# sent_date not to be used, because duplicates can be the case if just after and before midnight
# scheduled_date: not applicable if isnt sent per protocol (NA)
# scheduled_orig_date: only when also a reminder is sent
# date original: 26-01-01
# adapted: 26-02-20
# adapted: 26-03-17
# adapted: 26-03-31 - last participant ready

## start script -----
rm(list = ls(all = TRUE)) # Clear workspace
library(mpathr)
library(dplyr)
library(skimr)
library(readxl)
library(tidyr)

DataDir<-""
OutDir<-""

# load data -------------------------------------------------------------------------------------------------------
setwd(DataDir)
data <- read_mpath(
  file = "basic_v1.1.csv", # path to the data 
  meta_data = "meta_v1.1.csv" # path to the metadata
) 

meta<- read.csv("2026-03-31_MetaData_participants.csv", sep = ";")

# one participant has two connectionIDs --> give her one (the most common one, and one in meta)
table(data$alias, data$connectionId)
table(data[data$alias=="Guusje","connectionId"])
data$connectionId<-ifelse(data$connectionId==294403,271946,data$connectionId)
table(data[data$alias=="Guusje","connectionId"])

## remove colums not needed ----
data <- data |> 
  select(-c(
    "legacyCode", "initials", "scheduledBeepId", "sentBeepId", "accountCode", 
    "reminderForOriginalSentBeepId",  "questionListLabel"
  )) 
# originalTimeStampSent = first notification; needed for the right timing, because the last one can be just after midnight 
# timeStampSent = the one filled out. in case of no reminder, the orignal. Otherwise the reminder 

## rename -----
data <- data |> 
  rename(
    participant = "connectionId", # renaming connectionId to participant
    questionnaire = "questionListName",
    scheduled_orig = "originalTimeStampSent",
    scheduled = "timeStampScheduled",
    sent = "timeStampSent",
    start = "timeStampStart",
    stop = "timeStampStop"
    
  )

patterns <- c(
  "_yesno$", # removes _yesno, but only if it is at the end of the string (using $) 
  "_multipleChoice", # Remove _multipleChoice suffix
  "_sliderN.*", # Remove _slider suffixes and everything after it
  "_open", # Remove _open
  "context_" # Remove context_ 
)

# Merge them into one string that we can use a regular expression
patterns <- paste0("(", paste(patterns, collapse = "|"), ")")

colnames(data) <- gsub(
  pattern = patterns, 
  replacement = "",
  x = colnames(data)
)

anyDuplicated(names(data)) # 0
anyNA(data$participant)

## match with participant ID -----
meta[!meta$ConnectionID %in% data$participant,]
# B05; N / gestopt / 2025-01-18_Export staat ze nog wel; echter studie niet completed. Dus voor nu niet meenemen. 

# make dates in meta data -----
meta$T0_PreMeet_date<-as.Date(meta$Opstart,  format = "%d-%m-%Y") #T0
meta$T1_StartMeet_date<-as.Date(meta$Startgesprek, format = "%d-%m-%Y") #T1
meta$T2_Inzichtsgesprek_date <- as.Date(meta$Inzichtgesprek, format = "%d-%m-%Y") #T2
meta$T3_Evaluatiegesprek_date<-as.Date(meta$Evaluatiegesprek,  format = "%d-%m-%Y") #T3

df<-merge(meta, data, by.x="ConnectionID", by.y="participant")
table(df$Studienummer)

# create composite for CIS score ------
cis_vars<-grepl("CIS", names(df))
cis_vars<-names(df[,cis_vars])
# calculate CIS4
df <- df %>% mutate(
  CIS4 = CIS_moe + CIS_lichamelijk_uitgeput + CIS_gauw_moe + Fit_computation 
)

## reformat time stamps ----
# reformat to the local timezone 
df <- df |> 
  mutate(across(
    .cols = c("scheduled_orig", "scheduled", "sent", "start", "stop"),
    .fns = \(x) timestamps_to_datetime(x, tz_offset = df$timeZoneOffset)
  ))

# time converted, logical times
head(df[df$questionnaire=="Terugblik",c("scheduled_orig","scheduled", "sent", "start", "stop")])

# make date, lose the exact time (not applicable to "terugblik") 
df$scheduled_orig_date <-as.Date(df$scheduled_orig, format = "%Y-%m-%d %H:%M:%S %Z")
df$scheduled_date <-as.Date(df$scheduled, format = "%Y-%m-%d %H:%M:%S %Z")
df$start_date <- as.Date(df$start, format = "%Y-%m-%d %H:%M:%S %Z") 
df$sent_date <-as.Date(df$sent, format = "%Y-%m-%d %H:%M:%S %Z")

head(df[,c("start_date", "start", "sent_date", "sent")]) # correct

### : save the data (complete df)-----
# for only using in in R
# As an .RData file. When using `load()`, note that the data will be stored in the `data` object in the global environment.
setwd(DataDir)
save(
  df, 
  file = "data.RData"
)

# clean for terugblik ------------------------------------------------------------------------------------------------
library(lubridate)
# select Terugblik
tb<-df[df$questionnaire=="Terugblik",]
tb <- tb[, colSums(!is.na(tb)) > 0]

# duplicates because two days unanswered in a row, and MTs just after 00:00 and before 00:00 (= same day)
anyDuplicated(table(tb$Studienummer, tb$sent_date)) # 3
tb_duplicates <- tb %>%
  group_by(Studienummer, sent_date) %>%
  filter(n() > 1) %>%
  ungroup() 

## create new date variable ------------------------------------
# adapt dates - questionnaire sent 
tb <- tb %>% mutate(
  tb_date = if_else(scheduled_orig_date =="1969-12-31", sent_date, scheduled_orig_date),
  tb_date = if_else(is.na(scheduled_orig_date), scheduled_date, tb_date)
)

tb<-tb[!is.na(tb$Studienummer),]
tb_duplicates <- tb %>%
  group_by(Studienummer, tb_date) %>%
  filter(n() > 1) %>%
  ungroup()

# check wheter there is less 12u inbetween measurements (looks compared to lag(backward), lead(next))
tb <- tb %>%
  arrange(Studienummer, sent) %>%
  group_by(Studienummer) %>%
  mutate(
    diff_hours_lag = as.numeric(difftime(sent, lag(sent), units = "hours")),
    gap_12h_lag = diff_hours_lag >= 12,
    diff_hours_lead = as.numeric(difftime(lead(sent), sent, units = "hours")),
    gap_12h_lead = diff_hours_lead >= 12
    
  ) %>%
  ungroup()

# keep the first row, unless the second row was responded to
tb_clean <- tb %>%
  arrange(Studienummer, sent) %>%
  group_by(Studienummer) %>%
  mutate(
    # identify involvement in a <12h pair
    in_pair = (diff_hours_lag < 12) | (diff_hours_lead < 12),
    
    # start status of neighbours
    start_prev = lag(!is.na(start)),
    start_next = lead(!is.na(start)),
    
    # decide whether to keep this row
    keep = case_when(
      # not part of a <12h pair → always keep
      !in_pair ~ TRUE,
      
      # this is FIRST in pair (gap ahead <12h)
      diff_hours_lead < 12 & !is.na(start) & (is.na(start_next) | !start_next) ~ TRUE,
      diff_hours_lead < 12 & is.na(start) & !is.na(start_next) & start_next ~ FALSE,
      diff_hours_lead < 12 ~ TRUE,  # both NA or both non-NA → keep first
      
      # this is SECOND in pair (gap behind <12h)
      diff_hours_lag < 12 & !is.na(start) & (is.na(start_prev) | !start_prev) ~ TRUE,
      diff_hours_lag < 12 & is.na(start) & !is.na(start_prev) & start_prev ~ FALSE,
      diff_hours_lag < 12 ~ FALSE,  # both NA or both non-NA → drop second
      
      TRUE ~ TRUE
    )
  ) %>%
  filter(keep) %>%
  select(-in_pair, -start_prev, -start_next, -keep) %>%
  ungroup()

# checks done: went well; 16 rows removed. the right ones kept. 
tb_duplicates <- tb_clean %>%
  group_by(Studienummer, tb_date) %>%
  filter(n() > 1) %>%
  ungroup()

# select the right participants -------------------------------------------------------------------
tb<-tb_clean
table(tb$Studienummer, useNA = "always")
table(tb$Completed, useNA = "always")
#tb<-tb[tb$Completed==1 & !is.na(tb$Completed),] # select the participants that completed the study
table(tb$Studienummer, useNA = "always")

# prep for PDT: factor vector to indicate  phases (e.g., "A" and "B") per outcome per participant -------
tb <- tb %>%
  group_by(Studienummer) %>%
  mutate(
    phase_FSE = if_else(sent < T2_Inzichtsgesprek_date, "A", "B"),
    phase_cis4_school = if_else(sent < T2_Inzichtsgesprek_date + weeks(4), "A", "B"),
    phase_satisf_health = if_else(sent < T2_Inzichtsgesprek_date + weeks(6), "A", "B")
  ) %>%
  ungroup()

# indicate  the MTs after the evaluation conversation;
tb <- tb %>%
  group_by(Studienummer) %>%
  mutate(T0_T3 = if_else(sent <= T3_Evaluatiegesprek_date, 1, 0)) %>%
  ungroup()

# select the relevant columns -----
tb <- tb %>%
  select(
    ConnectionID,
    Studienummer,
    alias,
    leeftijd_start,
    Geslacht,
    Completed,
    T0_T3, # to perform selection on the studyperiod
    T0_PreMeet_date,
    T1_StartMeet_date,
    T2_Inzichtsgesprek_date,
    T3_Evaluatiegesprek_date,
    questionnaire,
    fromProtocolName,
    scheduled_orig,
    scheduled,
    sent,
    start,
    stop,
    scheduled_orig_date,
    scheduled_date,
    start_date,
    sent_date,
    tb_date,
    phase_FSE,
    phase_cis4_school,
    phase_satisf_health,
    Grip_op_vermoeidheid,
    Kijk_op_leven,
    Gezondheid,
    school,
    schooldag,
    rooster,
    aanwezig,
    Aanwezig_op_school_computation,
    CIS_moe,
    CIS_lichamelijk_uitgeput,
    CIS_fit,
    CIS_gauw_moe,
    CIS4,
    nieuw_doel_computation,
    day_computation,
    naam_droom_computation,
    samenvatting_doel_computation,
    goal_check_daily_string,
    niet_evaluatie_string
  )

#save tb ------
# for only using in in R
# As an .RData file. When using `load()`, note that the data will be stored in the `data` object in the global environment.
setwd(DataDir)
save(
  tb, 
  file = "data_terugblik.RData"
)

write.table(
  tb,
  file = "2026-03-31_Terugblikcleaned_export_260331.csv",
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  dec = "."  # decimal point
)
