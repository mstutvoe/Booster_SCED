## start script -----
rm(list = ls(all = TRUE)) # Clear workspace

library(dplyr)
library(skimr)
library(readxl)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(stringr)
#install.packages("tidytable")
library(tidytable)

DataDir<-""
OutDir<-""

# load data ---------------------------------------------------------------------------------------------
setwd(DataDir)
load("data_terugblik.RData")

# Adapt B23  - remove period of non intervention 
B23<-tb[tb$Studienummer=="B23",]
#period of not putting effort in the interfven
# had two periods in which he stopped with the intervention. Directly after the Insight Conversation due to holiday.
# then tried a period. and discontinued
# then after a phone call decided to retry
# date inzichts gesprek: "2025-10-06"
# 2025-10-29 # start after holiday
# 	2025-12-25 # drop out again , during holidays
# date call to start againL "2026-02-11"
B23<-B23 %>% mutate(
  active = case_when(
    B23$sent_date <= "2025-10-06" ~1, # pre Insight Conversation
    B23$sent_date > "2025-10-06" &   B23$sent_date < "2025-10-29" ~0, # holiday
    B23$sent_date >= "2025-10-29"  & B23$sent_date < "2025-12-25" ~1,
    B23$sent_date >= "2025-12-25"  & B23$sent_date < "2026-02-15" ~0,
    B23$sent_date >= "2026-02-15"   ~ 1,
    
      )
  
)

# save the tb dataframe ----
setwd(DataDir)
save(
  tb, 
  file = "data_terugblik.RData"
)


# look at compliance  -------------
cnt_TB<- tb %>%
  group_by(Studienummer) %>%
  summarise(
    
    nTB_sent = n(),
    nTB_compl = sum(!is.na(start_date)),
    
    
    s_T0T3 = sum(sent_date < T3_Evaluatiegesprek_date, na.rm = TRUE),
    c_T0T3 = sum(start_date <  T3_Evaluatiegesprek_date, na.rm = TRUE),
    
    
    s_T0T1 = sum(sent_date < T1_StartMeet_date, na.rm = TRUE),
    c_T0T1 = sum(start_date < T1_StartMeet_date, na.rm = TRUE),
    
    s_T0T2 = sum(sent_date < T2_Inzichtsgesprek_date, na.rm = TRUE),
    c_T0T2 = sum(start_date < T2_Inzichtsgesprek_date, na.rm = TRUE),
    
    s_T2T3 = sum(sent_date >= T2_Inzichtsgesprek_date &
                   sent_date < T3_Evaluatiegesprek_date, na.rm = TRUE),
    c_T2T3 = sum(start_date >= T2_Inzichtsgesprek_date &
                   start_date < T3_Evaluatiegesprek_date, na.rm = TRUE),
    
    s_T2T5 = sum(sent_date >= T2_Inzichtsgesprek_date, na.rm = TRUE),
    c_T2T5 = sum(start_date >= T2_Inzichtsgesprek_date, na.rm = TRUE)
  )

# calculate compliance
cnt_TB<- cnt_TB %>% mutate(
  compl_T0T1 = round(c_T0T1/s_T0T1*100,0),
  compl_T0T2 = round(c_T0T2/s_T0T2*100,0),
  compl_T2T3 = round(c_T2T3/s_T2T3*100,0),
  compl_T0T3 = round((c_T0T2+c_T2T3)/(s_T0T2+ s_T2T3)*100,0),
  T0T3_Perc_130 = round(c_T0T3/1.3,0)
)



sel<-cnt_TB %>% select(Studienummer,c_T0T2, compl_T0T2,c_T2T3, compl_T2T3, c_T0T3, compl_T0T3)

# goal info ----------------------------------------------------------------------------
# clean goal data 
goal_clean <- goal %>%
  mutate(samenvatting = samenvatting_doel_computation) %>%
  
  extract(
    samenvatting,
    into = c("goal", "if", "then", "days"),
    regex = "Doel: (.*?)\\nAls: (.*?)\\nDan: (.*?)\\nDagen:(.*)",
    remove = FALSE
  )

goal_clean <- goal_clean %>%
  mutate(
    days = str_trim(days),
    days_vec = str_split(days, " "),
    days_named = map_chr(days_vec, function(x) {
      dagen <- c("ma","di","wo","do","vr","za","zo")
      paste(dagen[which(x == "true")], collapse = "-")
    })
  )

table(goal_clean$days, goal_clean$days_named)
#

final_table <- goal_clean %>%
  filter(!is.na(goal)) %>%
  
  group_by(Studienummer, goal, `if`, then, days_named) %>%
  summarise(
    freq = n(),
    first_date = min(sent_date, na.rm = TRUE),
    .groups = "drop"
  )



final_table<-distinct(final_table)

# save complaince data ----------------
#library(writexcl)
#write_xlsx(x = cnt_TB, path = "2026-03-17_Terugblik_Compliance.xlsx")

# Create workbook and add sheets
wb <- createWorkbook()

addWorksheet(wb, "compl_sum")
writeData(wb, "compl_sum", sel)


addWorksheet(wb, "compliance")
writeData(wb, "compliance", cnt_TB)

addWorksheet(wb, "Goal_info")
writeData(wb, "Goal_info", final_table)

# Save
#saveWorkbook(wb, file = paste0(OutDir, "2026-04-01_Compliance_goal.xlsx"), overwrite = TRUE)

# new insight in data availability (kallman) -------------------------------------------------
tb<-tb[!tb$sent_date> tb$T3_Evaluatiegesprek_date,]
tb<-tb[!is.na(tb$Studienummer),]

# clean school data 
summary(tb$Aanwezig_op_school_computation)
tb$school_aanwezig<-if_else(tb$Aanwezig_op_school_computation > 100, NA,tb$Aanwezig_op_school_computation )
summary(tb$school_aanwezig)


# TB --------
# Visualize participants timeseries
timeseries_date<-ggplot(tb, aes(x = sent_date, y = Grip_op_vermoeidheid, group = Studienummer)) +
  geom_path() +
  facet_wrap(Studienummer~. , scales = "free_x") +
  theme_bw() + theme(panel.grid = element_blank())
# count the row per participant
tb <- tb %>%
  arrange(Studienummer, sent_date) %>%
  group_by(Studienummer) %>%
  mutate(n_terugblik = as.integer(as.Date(sent_date) - min(as.Date(sent_date))) + 1) %>%
  ungroup()
# visuazlie not based on date
timeseries_number<-ggplot(tb, aes(x = n_terugblik, y = Grip_op_vermoeidheid, group = Studienummer)) +
  geom_path() +
  facet_wrap(Studienummer~., scales = "free_x") +
  theme_bw() + theme(panel.grid = element_blank())

# Excluded --------------
timeseries_date_excl<-ggplot(excl, aes(x = sent_date, y = Grip_op_vermoeidheid, group = Studienummer)) +
  geom_path() +
  facet_wrap(Studienummer~. , scales = "free_x") +
  theme_bw() + theme(panel.grid = element_blank())
# count the row per participant
excl <- excl %>%
  arrange(Studienummer, sent_date) %>%
  group_by(Studienummer) %>%
  mutate(n_terugblik = as.integer(as.Date(sent_date) - min(as.Date(sent_date))) + 1) %>%
  ungroup()
# visuazlie not based on date
timeseries_number_excl<-ggplot(excl, aes(x = n_terugblik, y = Grip_op_vermoeidheid, group = Studienummer)) +
  geom_path() +
  facet_wrap(Studienummer~., scales = "free_x") +
  theme_bw() + theme(panel.grid = element_blank())

# save plots (NA distribution) -------------------
setwd(OutDir)
ggsave("2026-04-01_TimlineFSE_Part_date.jpeg", plot = timeseries_date, width = 12, height = 8, units = "in", dpi = 300)
ggsave("2026-04-01_TimlineFSE_Part_number.jpeg", plot = timeseries_number, width = 12, height = 8, units = "in", dpi = 300)
ggsave("2026-04-01_TimlineFSE_Part_excl_date.jpeg", plot = timeseries_date_excl, width = 10, height = 5, units = "in", dpi = 300)
ggsave("2026-04-01_TimlineFSE_Part_excl_number.jpeg", plot = timeseries_number_excl, width = 10, height = 5, units = "in", dpi = 300)


