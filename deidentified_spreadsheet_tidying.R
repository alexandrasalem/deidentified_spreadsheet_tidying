#Load/Install Packages
#install.packages("janitor")
#install.packages('plyr')
#install.packages('readxl')
#install.packages('tidyr')
#install.packages('dplyr')
library(tidyr)
library(janitor)
library(readxl)
library(plyr)
library(dplyr)

conversation_data_raw <- read_excel("/Users/alexandrasalem/Box Sync/MIND-transcripts/MIND_Spreadsheet/ELS Transcript Log OHSU Upload 9.18.17xlsx.xlsx", sheet = 1)

narration_data_raw <- read_excel("/Users/alexandrasalem/Box Sync/MIND-transcripts/MIND_Spreadsheet/ELS Transcript Log OHSU Upload 9.18.17xlsx.xlsx", sheet = 2)

ados_data_raw <- read_excel("/Users/alexandrasalem/Box Sync/MIND-transcripts/MIND_Spreadsheet/ELS Transcript Log OHSU Upload 9.18.17xlsx.xlsx", sheet = 3)


#CLEANING UP:

#this function just cleans up the names and changes the dates from the weird excel number to the right date
janitor_things <- function(inp) {
  inp <- inp %>%
    remove_empty_rows() %>%
    clean_names() %>%
    mutate(t1_sample_date = as.numeric(t1_sample_date)) %>%
    mutate(t2_sample_date = as.numeric(t2_sample_date)) %>%
    mutate(t3_sample_date = as.numeric(t3_sample_date)) %>%
    mutate(t1_sample_date = excel_numeric_to_date(t1_sample_date)) %>%
    mutate(t2_sample_date = excel_numeric_to_date(t2_sample_date)) %>%
    mutate(t3_sample_date = excel_numeric_to_date(t3_sample_date))
}

ados_data <- ados_data_raw[,1:12]
ados_data <- ados_data %>%
  clean_names()
colnames(ados_data)[11] <- "t3_sample_date"
ados_data <- janitor_things(ados_data)

conversation_data <- conversation_data_raw[,1:12]
conversation_data <- janitor_things(conversation_data)

narration_data <- narration_data_raw[,1:12]
narration_data <- janitor_things(narration_data)

#next, joining these. 
#we don't join by ca_group, t1_sample_date, t2_sample_date, or t3_sample_date 
#you'd think ca_group would match perfectly...
#but in the ados data, there's a category "6 to 11" and in the con data they add an extra space so it's "6  to 11" :( --makes a bunch of repetitions
#the time dates across samples (ados, con, nar) actually match almost perfectly, which is better than I expected
#but, there's was a slight mis-match for one value in t1 that would have caused one id repetition. 
#so, i just didn't join by it
data1 <- full_join(ados_data, conversation_data, by=c("lsid", "participant_id", "diag", "site", "age"))
data1 <- data1 %>%
  select(lsid, participant_id, diag, site, age, ca_group.x, t1_sample_date.x, t2_sample_date.x, t3_sample_date.x, ados_t1, ados_t2, ados_t3, conversation_t1, conversation_t2, conversation_t3)
full_data <- full_join(data1, narration_data, by=c("lsid", "participant_id", "diag", "site", "age"))
full_data <- full_data %>%
  select(lsid, participant_id, diag, site, age, ca_group.x, t1_sample_date.x, t2_sample_date.x, t3_sample_date.x, ados_t1, ados_t2, ados_t3, conversation_t1, conversation_t2, conversation_t3, narration_t1, narration_t2, narration_t3)


#and tidying these--gathering all three samples, then making a single date column, then separating the sample type and time point
full_data2 <- gather(full_data, key=sample, value = present, ados_t1, ados_t2, ados_t3, conversation_t1, conversation_t2, conversation_t3, narration_t1, narration_t2, narration_t3)
full_data3 <- full_data2 %>%
  mutate(date = ifelse(sample == "ados_t1" | sample == "conversation_t1" | sample == "narration_t1", as.character(t1_sample_date.x), NA)) %>%
  mutate(date = ifelse(sample == "ados_t2" | sample == "conversation_t2" | sample == "narration_t2", as.character(t2_sample_date.x), date)) %>%
  mutate(date = ifelse(sample == "ados_t3" | sample == "conversation_t3" | sample == "narration_t3", as.character(t3_sample_date.x), date))
full_data3 <- full_data3 %>%
  mutate(ca_group = ca_group.x) %>%
  select(-ca_group.x, -t1_sample_date.x, -t2_sample_date.x, -t3_sample_date.x)
full_data3 <- separate(full_data3, sample, c("sample", "time_point"), sep = "_")
full_data3 <- arrange(full_data3, participant_id, lsid, sample, diag, site, age, ca_group, time_point, date, present)

#now, exporting our results!
write.csv(full_data3, "MIND_spreadsheet.csv")

#this I used to look at the ca_group for the differences I describe above
ad <- ados_data$ca_group
co <- conversation_data$ca_group

