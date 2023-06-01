library(dplyr)
library(tidyverse)
library(tibble)
library(psych)
library(openxlsx)
#Load the data
data <- readxl::read_xlsx('alerts.xlsx')
data <- data %>% replace(.=="NULL", NA) # replace with NA


# Dealing with missing/faulty data:
# In the dataset, there are two columns that resemble ID columns - one unnamed and one named "intID"
n_distinct(data$...1)/length(data$...1)
n_distinct(data$intID)/length(data$intID)
# The first column is more suited to be an ID one, as it has no duplicates
data <- data %>% rename("ID" = "...1")

# Check how many NAs are there
sapply(data, function(x) sum(is.na(x)))
# For CusRiskCategory, assign "Not specified" to NAs
data <- data %>% mutate(CusRiskCategory = replace_na(CusRiskCategory, "Not Specified"))
# For PEP (politically exposed person), we can replace NAs with "N"
data <- data %>% mutate(PEP = replace_na(PEP, "N"))
# For IndustryCode lets replace with code 0 (which would denote 'other')
data <- data %>% mutate(IndustryCode = as.numeric(IndustryCode)) %>%mutate(IndustryCode = replace_na(IndustryCode, 0))
# For CaseState, we can assume NA suppose to represent that the case was not opened (as there is exactly the same amount
# of CaseOpen NA and CaseState)
data <- data %>% mutate(CaseState = replace_na(CaseState, "Not Opened"))
# The only NAs left are those in date columns - information about it is encoded in column CaseState
# We can assign future date to those if we want clean data, but want to keep information about it being or not being null
data_cleaned <- data %>% select(-c("DateCreated","DateClosed","CaseOpen","CaseClosed","CaseReported"))
##########################
### Data visualization###
##########################
#first a basic histogram of alert types
p1 <- ggplot(data_cleaned, aes(x = reorder(AlertType, AlertType, function(x)-length(x)), fill = CusRiskCategory)) +
  geom_bar(position = "stack") +
  labs(x = "Alert type") +
  ggtitle("Alert type count and its severity") +
  theme_minimal()
p1 + coord_flip()

# stacked histogram of current status in regard to risk category
p2 <- ggplot(subset(data_cleaned, CaseState != "Not Opened"), aes(x = CaseState, fill = CusRiskCategory)) +
  geom_bar(position = "stack") +
  labs(x = "Case State (Not Opened excluded)", y = "Count") +
  ggtitle("Case state in relation to Risk Category") +
  theme_minimal()
p2
p3 <- ggplot(subset(data_cleaned, CaseState == "Not Opened"), aes(x="",y=CaseState,fill=CusRiskCategory)) +
      geom_bar(stat="identity", width=1) +
      ggtitle("Case state (Not Opened) in relation to Risk Category") +
      coord_polar("y", start=0)
p3
# Encoding type
data_cleaned <- data_cleaned %>%
  mutate(is_lcfi = if_else(Type == "lcfi", 1, 0)) %>%
  select(-Type)
# Piecharts for is_lcfi
p4<-ggplot(data_cleaned, aes(x="", y=is_lcfi, fill=CusRiskCategory)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
p4
p5 <- ggplot(subset(data_cleaned, is_lcfi == 0), aes(x = "", y = !is_lcfi, fill = CusRiskCategory)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0)
p5
# Encoding PEP
data_cleaned <- data_cleaned %>%
  mutate(is_PEP = if_else(PEP == "Y", 1, 0)) %>%
  select(-PEP)
#Encoding CaseState
data_cleaned <- data_cleaned %>%
  mutate(caseState_Closed = if_else(CaseState == "Closed", 1, 0)) %>% 
  mutate(caseState_RepCon = if_else(CaseState == "Report Confirmed", 1, 0)) %>%
  select(-CaseState)
#Encoding Risk Category
hierarchical_order <- c("Not Specified", "Lower Risk", "Medium Risk", "Higher Risk")
data_encoded <- data_cleaned %>%
  mutate(CusRiskCategory= factor(CusRiskCategory, levels = hierarchical_order, ordered = TRUE)) %>%
  mutate(enc_CusRiskCat = as.integer(CusRiskCategory) - 1) %>%
  select(-CusRiskCategory)


openxlsx::write.xlsx(data_encoded, file='data_encoded.xlsx', sheetName = "data_encoded", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)
