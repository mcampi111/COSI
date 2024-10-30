library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(reshape2)
library(viridis)
library(tidyr)
library(gridExtra)
library(kableExtra)
library(tseries)
library(car)
library(MASS)
library(nnet)
library(lmtest)
library(openxlsx)
library(RColorBrewer)
library(olsrr)
library(readxl)
library(car)
library(coin)
library(ggh4x)
library(ggnewscale)
library(psych)

###############################
# SET DIRECTORY AND READ FILE #
###############################

mydir<- "/Users/mcampi/Desktop/Postdoc_Pasteur/Perrine/code/"

mydir_figs = "/Users/mcampi/Desktop/Postdoc_Pasteur/Perrine/code/figs/"
mydir_excel = "/Users/mcampi/Desktop/Postdoc_Pasteur/Perrine/code/excel_results/"


source(paste(mydir, "utils_COSI.R", sep = "")) 


df_COSI = read.csv(paste( mydir, "Ps_Tests_nodep_uniquecode.csv", sep = ""))

####################
# DATA PREPARATION #
####################

df_COSI2 = df_COSI[,-c(7,8,9,13,14,15,16,17:29,31:41,43,44,45,47:50,52:54)]

colnames(df_COSI2) = c("ID", "Sex","Age", "Shop", "Hearing_Aid",
                       "Department", "Q1", "Q2", "Q3", "PTA_L", "PTA_R", "SNR", "SRT")

#SEX Variable
df_COSI2 = df_COSI2 %>%
  mutate(Sex = ifelse(Sex == "H", "Male", 
                      ifelse(Sex == "F", "Female", Sex)))

df_COSI2$Sex = as.factor(df_COSI2$Sex)

#NAs
df_COSI2[df_COSI2 == ""] <- NA


#COUNT # of COSI Answers
sum(!is.na(df_COSI2$Q1)) + sum(!is.na(df_COSI2$Q2)) + sum(!is.na(df_COSI2$Q3))

#AGE Variable
df_COSI2 <- df_COSI2[df_COSI2$Age <= 99, ]
df_COSI2 <- df_COSI2[df_COSI2$Age >= 18, ]


# Remove rows with missing values in Age column
df_COSI2 <- df_COSI2[complete.cases(df_COSI2$Age), ]

# Define breaks and labels for Age bins
breaks_10 <- c(17.9, seq(30, 100, by = 10))  # Adjusted breaks for bins every 10 years
labels_10 <- c("18-29", paste(seq(30, 90, by = 10), "-", seq(39, 99, by = 10)))  # Labels for bins every 10 years

breaks_2 <- c(17.9, seq(20, 100, by = 2))   # Adjusted breaks for bins every 2 years
labels_2 <- c("18-19", paste(seq(20, 98, by = 2), "-", seq(21, 99, by = 2)))  # Labels for bins every 2 years

# Create AgeBin_10 column (bins every 10 years)
df_COSI2$AgeBin_10 <- cut(df_COSI2$Age, breaks = breaks_10, labels = labels_10)

# Create AgeBin_2 column (bins every 2 years)
df_COSI2$AgeBin_2 <- cut(df_COSI2$Age, breaks = breaks_2, labels = labels_2)


# Convert AgeBin_10 to factor for ordered plotting
df_COSI2$AgeBin_10 <- factor(df_COSI2$AgeBin_10, ordered = TRUE)
# Convert AgeBin_2 to factor for ordered plotting
df_COSI2$AgeBin_2 <- factor(df_COSI2$AgeBin_2, ordered = TRUE)


#Symmetric HL
# BEFORE: Remove rows with missing PTA_L or PTA_R
df_COSI2 <- df_COSI2[complete.cases(df_COSI2[c("PTA_L", "PTA_R")]), ]

df_COSI2$Symmetry <- abs(df_COSI2$PTA_L - df_COSI2$PTA_R) <= 15

table(df_COSI2$Symmetry)



# Apply function to create HL column
#Select PTA_L and drop PTA_R - we could do the opposite or analyse both
df_COSI2$HL <- sapply(df_COSI2$PTA_L, categorize_hearing_loss)

# Convert HL column to factor with ordered levels
df_COSI2$HL <- factor(df_COSI2$HL, 
                      levels = c("Normal", "Slight", "Mild", 
                                 "Moderate", "Moderately severe", "Severe", "Profound"), 
                      ordered = TRUE)

table(df_COSI2$HL)

colnames(df_COSI2)

#drop PTA_R
df_COSI2 = df_COSI2[,-11]

df_COSI2 <- df_COSI2 %>%
  rename(PTA = PTA_L)

colnames(df_COSI2)


#SNR & SRT Variables
#SRT Variable -- NOT RUNNING THIS
#df_COSI2 <- subset(df_COSI2, SRT >= 0 & SRT <= 90)

#SNR Variable --> I RUN THIS FOR NOW
df_COSI2 <- subset(df_COSI2, SNR >= -10 & SNR <= 20)


#COSI VARIABLES
df_COSI2 <- df_COSI2[!is.na(df_COSI2$Q1), ]

df_COSI2$Q1 <- factor(df_COSI2$Q1)
df_COSI2$Q2 <- factor(df_COSI2$Q2)
df_COSI2$Q3 <- factor(df_COSI2$Q3)

#Category In English
levels(df_COSI2$Q1) <- c("Increase social contact", "Others", 
                         "Conversation with 1 or 2 in silence",
                         "Group conversation in noise", 
                         "Group conversation in silence", 
                         "Church or meeting",
                         "Unfamiliar interlocutor on the phone", 
                         "Feeling embarrassed or stupid",
                         "Television/radio at normal volume")

levels(df_COSI2$Q2) <- c("Increase social contact", "Others", 
                         "Conversation with 1 or 2 in silence",
                         "Group conversation in noise", 
                         "Group conversation in silence", 
                         "Church or meeting",
                         "Unfamiliar interlocutor on the phone", 
                         "Feeling embarrassed or stupid",
                         "Television/radio at normal volume")

levels(df_COSI2$Q3) <- c("Increase social contact", "Others", 
                         "Conversation with 1 or 2 in silence",
                         "Group conversation in noise", 
                         "Group conversation in silence", 
                         "Church or meeting",
                         "Unfamiliar interlocutor on the phone", 
                         "Feeling embarrassed or stupid",
                         "Television/radio at normal volume")


# Create a new column Q1_English with the translated levels
df_COSI2$Q1_English <- df_COSI2$Q1
df_COSI2$Q2_English <- df_COSI2$Q2
df_COSI2$Q3_English <- df_COSI2$Q3

# Drop the old Q1 column
df_COSI2$Q1 <- NULL
df_COSI2$Q2 <- NULL
df_COSI2$Q3 <- NULL


# Rename Q1_English to Q1
names(df_COSI2)[names(df_COSI2) == "Q1_English"] <- "Q1"
names(df_COSI2)[names(df_COSI2) == "Q2_English"] <- "Q2"
names(df_COSI2)[names(df_COSI2) == "Q3_English"] <- "Q3"


# Check the levels of the new columns
levels(df_COSI2$Q1)
levels(df_COSI2$Q2)
levels(df_COSI2$Q3)


#HEARING AID#COSI VARIABLES
df_COSI2_HA <- subset(df_COSI2, Hearing_Aid == "True")
df_COSI2 <- subset(df_COSI2, Hearing_Aid == "False")

#WORK WITH SYMMETRIC HEARING LOSS
# Create dataframe with Symmetry TRUE and drop Symmetry column
df_COSI2_sym <- df_COSI2[df_COSI2$Symmetry == TRUE, ]
df_COSI2_sym <- df_COSI2_sym[, !(names(df_COSI2_sym) %in% "Symmetry")]

# Create dataframe with Symmetry FALSE and drop Symmetry column
df_COSI2_asym <- df_COSI2[df_COSI2$Symmetry == FALSE, ]
df_COSI2_asym <- df_COSI2_asym[, !(names(df_COSI2_asym) %in% "Symmetry")]

#COUNT # of COSI Answers
sum(!is.na(df_COSI2_sym$Q1)) + sum(!is.na(df_COSI2_sym$Q2)) + sum(!is.na(df_COSI2_sym$Q3))


###############
# Some counts #
###############

# Summary for Q1
summary_Q1 <- df_COSI2 %>%
  group_by(Q1) %>%
  summarise(count_Q1 = n()) %>%
  arrange(desc(count_Q1))

# Summary for Q2
summary_Q2 <- df_COSI2 %>%
  group_by(Q2) %>%
  summarise(count_Q2 = n()) %>%
  arrange(desc(count_Q2))

# Summary for Q3
summary_Q3 <- df_COSI2 %>%
  group_by(Q3) %>%
  summarise(count_Q3 = n()) %>%
  arrange(desc(count_Q3))

# Combine the summaries into one dataframe
summary_combined <- full_join(full_join(summary_Q1, summary_Q2, 
                                        by = c("Q1" = "Q2")),
                              summary_Q3, by = c("Q1" = "Q3"))

# Display the combined dataframe
summary_combined



summary_Q1_sym <- df_COSI2_sym %>%
  group_by(Q1) %>%
  summarise(count_Q1 = n()) %>%
  arrange(desc(count_Q1))

# Summary for Q2
summary_Q2_sym <- df_COSI2_sym %>%
  group_by(Q2) %>%
  summarise(count_Q2 = n()) %>%
  arrange(desc(count_Q2))

# Summary for Q3
summary_Q3_sym <- df_COSI2_sym %>%
  group_by(Q3) %>%
  summarise(count_Q3 = n()) %>%
  arrange(desc(count_Q3))

# Combine the summaries into one dataframe
summary_combined_sym <- full_join(full_join(summary_Q1_sym, summary_Q2_sym, 
                                        by = c("Q1" = "Q2")),
                              summary_Q3_sym, by = c("Q1" = "Q3"))

# Display the combined dataframe
summary_combined_sym

sum(summary_combined_sym$count_Q1, na.rm = TRUE) + 
  sum(summary_combined_sym$count_Q2, na.rm = TRUE) +
  sum(summary_combined_sym$count_Q3, na.rm = TRUE)


df_long_tbl_paper <- df_COSI2_sym %>%
  pivot_longer(cols = c(Q1, Q2, Q3), names_to = "Question", values_to = "Answer")

# Step 2: Create the contingency table
result_table <- df_long_tbl_paper %>%
  group_by(Answer, HL, Sex) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(HL, Sex), values_from = Count, values_fill = 0)

# Step 2: Create the contingency table
result_table2 <- df_long_tbl_paper %>%
  group_by(Answer, AgeBin_10, HL, Sex) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(HL, Sex), values_from = Count, values_fill = 0)

# Step 3: Reorder columns to group by HL and then Sex (Female first, then Male)
hl_order <- c("Normal", "Slight", "Mild", "Moderate", "Severe", "Profound")
sex_order <- c("Female", "Male")

col_order <- sapply(hl_order, function(hl) paste(hl, sex_order, sep = "_"))
col_order <- as.vector(col_order)  # Flatten the array

# Keep only the columns that exist in the result_table
col_order <- col_order[col_order %in% names(result_table2)]

# Display the result
print(result_table2)

first_two_columns <- names(result_table2)[1:2]

# Combine the first two columns with the desired column order
new_col_order <- c(first_two_columns, col_order)

# Reorder the data frame using the new column order
result_table2 <- result_table2[, new_col_order]

result_table2 %>%
  kbl(format = "latex", booktabs = TRUE)



####################
# Cronbach’s Alpha #
###################

items <- df_COSI2_sym[, c("Q1", "Q2", "Q3")]
items <- na.omit(items) 

# Extract relevant columns
items <- df_COSI2_sym[, c("Q1", "Q2", "Q3")]

# Convert text responses to numeric codes
items$Q1 <- as.numeric(factor(items$Q1))
items$Q2 <- as.numeric(factor(items$Q2))
items$Q3 <- as.numeric(factor(items$Q3))

# Remove rows with missing values
items <- na.omit(items)

sapply(df_COSI2_sym[, c("Q1", "Q2", "Q3")], function(x) sum(is.na(x)))
sapply(items[, c("Q1", "Q2", "Q3")], function(x) sum(is.na(x)))


# Compute Cronbach's alpha with automatic item reversal
reliability <- psych::alpha(items, check.keys = TRUE)

# Print Cronbach's alpha and reversed items
print(paste("Cronbach's Alpha:", reliability$total$raw_alpha))

# Ensure data is numeric and remove NAs
items_numeric <- as.data.frame(lapply(items, function(x) as.numeric(factor(x))))
items_numeric <- na.omit(items_numeric)

# Check item correlations
item_correlations <- cor(items_numeric, use = "pairwise.complete.obs")
print(item_correlations)


#################
# Missing Data #
################

missing_percent <-  df_COSI2_sym %>%
  group_by(HL) %>%
  summarise_all(~ mean(is.na(.)) * 100) %>%
  pivot_longer(cols = -HL, 
               names_to = "Column",
               values_to = "Missing_Percentage")


# Plot the missing percentage per column
ggplot(missing_percent, aes(x = reorder(Column, -Missing_Percentage),
                            y = Missing_Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Percentage of Missing Values per Column",
       x = "Variable",
       y = "Missing Percentage") +
  theme_bw() + facet_wrap(~ HL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#####################
# Statistical Plots #
#####################

ggplot(df_COSI2_sym, aes(x = Age)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(df_COSI2_sym, aes(x = PTA)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of PTA",
       x = "Age",
       y = "Count") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(df_COSI2_sym, aes(x = SRT)) +
  geom_bar(fill = "magenta", color = "black", alpha = 0.8) +
  labs(title = "Distribution of SRT",
       x = "Age",
       y = "Count") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(df_COSI2_sym, aes(x = SNR)) +
  geom_bar(fill = "green3", color = "black", alpha = 0.8) +
  labs(title = "Distribution of SNR",
       x = "Age",
       y = "Count") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(df_COSI2_sym, aes(x = HL)) +
  geom_bar(aes(fill = HL), position = "dodge") +
  #facet_grid(Sex ~ ., scales = "free_x") +
  facet_wrap(~ Sex, scales = "free_x") +
  labs(title = " ",#Bar Plot of Q1, Q2, and Q3 Responses by Sex
       x = "Hearing Loss",
       y = "Count",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) +
  coord_flip()


ggplot(df_COSI2_sym, aes(x = Sex, y = PTA, fill = HL )) +
  geom_boxplot() +
  facet_wrap(~ AgeBin_10, scales = "free_y") +
  labs(title = "",#Boxplot of PTA by Sex, AgeBin_10, and HL
       x = "Sex",
       y = "PTA",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7))


ggplot(df_COSI2_sym, aes(x = Sex, y = SRT, fill = HL )) +
  geom_boxplot() +
  facet_wrap(~ AgeBin_10, scales = "free_y") +
  labs(title = "",#Boxplot of SRT by Sex, AgeBin_10, and HL
       x = "Sex",
       y = "SRT",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))+
  guides(fill = guide_legend(ncol = 7))

ggplot(df_COSI2_sym, aes(x = Sex, y = SNR, fill = HL )) +
  geom_boxplot() +
  facet_wrap(~ AgeBin_10, scales = "free_y") +
  labs(title = "",#Boxplot of SNR by Sex, AgeBin_10, and HL
       x = "Sex",
       y = "SNR",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))+
  guides(fill = guide_legend(ncol = 7)) 

df_long <- df_COSI2_sym %>%
  pivot_longer(cols = starts_with("Q"), 
               names_to = "Question", 
               values_to = "Response")


ggplot(df_long[complete.cases(df_long$Response), ], aes(x = Response)) +
  geom_bar(aes(fill = HL), position = "dodge") +
  facet_grid(Sex ~ Question, scales = "free_x") +
  labs(title = "",#Bar Plot of Q1, Q2, and Q3 Faceted by Sex and HL
       x = "Response",
       y = "Count",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7))



ggplot(df_long[complete.cases(df_long$Response), ], aes(x = Response)) +
  geom_bar(aes(fill = HL), position = "dodge") +
  facet_grid(Sex ~ ., scales = "free_x") +
  labs(title = "",#Bar Plot of Q1, Q2, and Q3 Responses by Sex 
       x = "Response",
       y = "Count",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7))


ggplot(df_long[complete.cases(df_long$Response), ], aes(x = Response)) +
  geom_bar(aes(fill = HL), position = "dodge") +
  facet_grid(Sex ~ ., scales = "free_x") +
  labs(title = " ",#Bar Plot of Q1, Q2, and Q3 Responses by Sex
       x = "Response",
       y = "Count",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) +
  coord_flip()


ggplot(df_long[complete.cases(df_long$Response), ], aes(x = Response)) +
  geom_bar(aes(fill = HL), position = "dodge") +
  facet_wrap(~ Sex) +
  labs(title = "",#Bar Plot of Q1, Q2, and Q3 Responses by Sex 
       x = "Response",
       y = "Count",
       fill = "Hearing Loss") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) +
  coord_flip()



ggplot(df_long[complete.cases(df_long$Response), ], aes(x = HL, y = SNR, fill = HL)) +
  geom_boxplot() +
  facet_grid(Question~ Response) +
  labs(title = "",
       x = "Hearing Loss",
       y = "SNR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) 

ggplot(df_long[complete.cases(df_long$Response), ], aes(x = HL, y = SRT, fill = HL)) +
  geom_boxplot() +
  facet_grid(Question~ Response) +
  labs(title = "",
       x = "Hearing Loss",
       y = "SRT") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) 


ggplot(df_long[complete.cases(df_long$Response), ], aes(x = HL, y = SNR, fill = HL)) +
  geom_boxplot() +
  facet_grid(Sex~ Response) +
  labs(title = "",
       x = "Hearing Loss",
       y = "SNR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) 



ggplot(df_long[complete.cases(df_long$Response), ], aes(x = HL, y = SRT, fill = HL)) +
  geom_boxplot() +
  facet_grid(Sex~ Response) +
  labs(title = "",
       x = "Hearing Loss",
       y = "SRT") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(ncol = 7)) 



##########################
# Descriptive Statistics #
##########################

summary_df <- summary(df_COSI2_sym)

# Convert the summary to a data frame
summary_df <- as.data.frame(summary_df)

write.xlsx(summary_df, 
           file = paste(mydir_excel,
                        "summary_df_COSI2_sym.xlsx",
                        sep=""), 
           rowNames = FALSE)

table(df_COSI2_sym$Q1)
table(df_COSI2_sym$Q2)
table(df_COSI2_sym$Q3)


summary_quantitative <- df_COSI2_sym %>%
  group_by(Sex) %>%
  summarise(
    Age_Count = n(),
    Age_Mean = mean(Age, na.rm = TRUE),
    Age_Std = sd(Age, na.rm = TRUE),
    Age_Min = min(Age, na.rm = TRUE),
    Age_25 = quantile(Age, 0.25, na.rm = TRUE),
    Age_50 = median(Age, na.rm = TRUE),
    Age_75 = quantile(Age, 0.75, na.rm = TRUE),
    Age_Max = max(Age, na.rm = TRUE),
    
    PTA_Count = n(),
    PTA_Mean = mean(PTA, na.rm = TRUE),
    PTA_Std = sd(PTA, na.rm = TRUE),
    PTA_Min = min(PTA, na.rm = TRUE),
    PTA_25 = quantile(PTA, 0.25, na.rm = TRUE),
    PTA_50 = median(PTA, na.rm = TRUE),
    PTA_75 = quantile(PTA, 0.75, na.rm = TRUE),
    PTA_Max = max(PTA, na.rm = TRUE),
    
    SNR_Count = n(),
    SNR_Mean = mean(SNR, na.rm = TRUE),
    SNR_Std = sd(SNR, na.rm = TRUE),
    SNR_Min = min(SNR, na.rm = TRUE),
    SNR_25 = quantile(SNR, 0.25, na.rm = TRUE),
    SNR_50 = median(SNR, na.rm = TRUE),
    SNR_75 = quantile(SNR, 0.75, na.rm = TRUE),
    SNR_Max = max(SNR, na.rm = TRUE),
    
    SRT_Count = n(),
    SRT_Mean = mean(SRT, na.rm = TRUE),
    SRT_Std = sd(SRT, na.rm = TRUE),
    SRT_Min = min(SRT, na.rm = TRUE),
    SRT_25 = quantile(SRT, 0.25, na.rm = TRUE),
    SRT_50 = median(SRT, na.rm = TRUE),
    SRT_75 = quantile(SRT, 0.75, na.rm = TRUE),
    SRT_Max = max(SRT, na.rm = TRUE)
  )



summary_Q1 <- df_COSI2_sym %>%
  group_by(Sex, Q1) %>%
  summarise(Count = n()) %>%
  arrange(Sex, Q1)

# Generate counts for each level of Q2 by Sex
summary_Q2 <- df_COSI2_sym %>%
  group_by(Sex, Q2) %>%
  summarise(Count = n()) %>%
  arrange(Sex, Q2)


# Generate counts for each level of Q3 by Sex
summary_Q3 <- df_COSI2_sym %>%
  group_by(Sex, Q3) %>%
  summarise(Count = n()) %>%
  arrange(Sex, Q3)


summary_HL <- df_COSI2_sym %>%
  group_by(Sex, HL) %>%
  summarise(Count = n()) %>%
  arrange(Sex, HL)


################################
# Statistical Tests & Analysis #
################################

#Differences between 
# Q1, Q2, Q3 vs. HL
# Chi-squared test for independence
chisq.test(table(df_COSI2_sym$Q1, df_COSI2_sym$HL))
chisq.test(table(df_COSI2_sym$Q2, df_COSI2_sym$HL))
chisq.test(table(df_COSI2_sym$Q3, df_COSI2_sym$HL))

# Q1, Q2, Q3 vs. Sex
# Chi-squared test for independence
chisq.test(table(df_COSI2_sym$Q1, df_COSI2_sym$Sex))
chisq.test(table(df_COSI2_sym$Q2, df_COSI2_sym$Sex))
chisq.test(table(df_COSI2_sym$Q3, df_COSI2_sym$Sex))


chisq.test(table(df_COSI2_sym$Q1, df_COSI2_sym$AgeBin_2))
chisq.test(table(df_COSI2_sym$Q2, df_COSI2_sym$AgeBin_2))
chisq.test(table(df_COSI2_sym$Q3, df_COSI2_sym$AgeBin_2))


# HL vs. Sex
# Chi-squared test for independence
chisq.test(table(df_COSI2_sym$HL, df_COSI2_sym$Sex))


# One-way ANOVA
anova_result <- aov(Age ~ Q3, data = df_COSI2_sym) #change the questions
anova_result
summary(anova_result)
model.tables(anova_result, "means")$tables


anova_result <- aov(PTA ~ Q1, data = df_COSI2_sym)
anova_result
summary(anova_result)
model.tables(anova_result, "means")$tables


anova_result <- aov(PTA ~ Q2, data = df_COSI2_sym)
anova_result
summary(anova_result)
model.tables(anova_result, "means")$tables


anova_result <- aov(PTA ~ Q3, data = df_COSI2_sym)
anova_result
summary(anova_result)
model.tables(anova_result, "means")$tables



anova_result <- aov(SNR ~ Q3, data = df_COSI2_sym)#change the question
anova_result
summary(anova_result)
model.tables(anova_result, "means")$tables



anova_result <- aov(SRT ~ Q3, data = df_COSI2_sym)#change the question
anova_result
summary(anova_result)
model.tables(anova_result, "means")$tables


# Kruskal-Wallis test - replacing one-wa ANOVA
kruskal.test(PTA ~ Q3, data = df_COSI2_sym)
kruskal.test(SNR ~ Q3, data = df_COSI2_sym)
kruskal.test(SRT ~ Q3, data = df_COSI2_sym)


###################
#Proportion Tables#
###################

count_table <- df_COSI2_sym %>%
  group_by(Sex, Q1) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate total counts for each Sex group
total_sex <- df_COSI2_sym %>%
  group_by(Sex) %>%
  summarise(total_count = n(), .groups = 'drop')

# Merge the two tables and calculate proportions
proportion_table <- merge(count_table, total_sex, by = "Sex")
proportion_table <- proportion_table %>%
  mutate(proportion = round(count / total_count, 3) )

print(proportion_table)

custom_palette <- colorRampPalette(c("pink", "blue"))(2)

ggplot(proportion_table, aes(x = Q1, y = proportion,
                             fill = Sex)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(title = "",
       x = "Response",
       y = "Proportion") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values = custom_palette)





count_table <- df_COSI2_sym %>%
  group_by(AgeBin_2, Q1) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate total counts for each AgeBin_2 group
total_sex <- df_COSI2_sym %>%
  group_by(AgeBin_2) %>%
  summarise(total_count = n(), .groups = 'drop')

# Merge the two tables and calculate proportions
proportion_table <- merge(count_table, total_sex, by = "AgeBin_2")
proportion_table <- proportion_table %>%
  mutate(proportion = round(count / total_count, 3) )

print(proportion_table)

custom_palette <- colorRampPalette(c("green3", "darkgreen"))(50)

ggplot(proportion_table, aes(x = AgeBin_2, y = proportion,
                             fill = AgeBin_2)) +
  geom_bar(stat = "identity",) +
  facet_wrap(Q1 ~ .)+
  labs(title = "",
       x = "Age",
       y = "Proportion") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values = custom_palette)



count_table <- df_COSI2_sym %>%
  group_by(AgeBin_10, Sex, Q1) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate total counts for each AgeBin_10 group
total_sex <- df_COSI2_sym %>%
  group_by(AgeBin_10,Sex) %>%
  summarise(total_count = n(), .groups = 'drop')

# Merge the two tables and calculate proportions
proportion_table <- merge(count_table, total_sex, by = c("AgeBin_10", "Sex") )
proportion_table <- proportion_table %>%
  mutate(proportion = round(count / total_count, 3) )

print(proportion_table)

custom_palette_male <- colorRampPalette(c("cyan", "blue"))(10)
custom_palette_female <- colorRampPalette(c("pink", "magenta"))(10)

# Combine palettes into a single vector with named elements
combined_palette <- c(custom_palette_male, custom_palette_female)
names(combined_palette) <- c(paste0("Male_", 1:10), paste0("Female_", 1:10))

# Create a combined fill variable
proportion_table <- proportion_table %>%
  mutate(fill_color = paste(Sex, as.numeric(factor(AgeBin_10)), sep = "_"))

# Create the bar plot with the custom color scale and position dodge
ggplot(proportion_table, aes(x = AgeBin_10, y = proportion, fill = fill_color)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(. ~ Q1) +
  labs(title = "",
       x = "Age",
       y = "Proportion") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values = combined_palette)



count_table_Q1 <- df_COSI2_sym %>%
  group_by(AgeBin_10, Sex, Q1) %>%
  summarise(count = n(), .groups = 'drop')

count_table_Q2 <- df_COSI2_sym %>%
  group_by(AgeBin_10, Sex, Q2) %>%
  summarise(count = n(), .groups = 'drop')%>%
  filter(!is.na(Q2))

count_table_Q3 <- df_COSI2_sym %>%
  group_by(AgeBin_10, Sex, Q3) %>%
  summarise(count = n(), .groups = 'drop')%>%
  filter(!is.na(Q3))


total_sex <- df_COSI2_sym %>%
  group_by(AgeBin_10,Sex) %>%
  summarise(total_count = n(), .groups = 'drop')

# Calculate total counts for each AgeBin_10 group
total_sex <- df_COSI2_sym %>%
  group_by(AgeBin_10,Sex) %>%
  summarise(total_count = n(), .groups = 'drop')

# Merge the two tables and calculate proportions
proportion_table_Q1 <- merge(count_table_Q1, 
                             total_sex,
                             by = c("AgeBin_10", "Sex") )
proportion_table_Q1 <- proportion_table_Q1 %>%
  mutate(proportion = round(count / total_count, 3) )

colnames(proportion_table_Q1) = c("AgeBin_10","Sex","Reponse" , "count" , 
                                  "total_count" ,
                                  "proportion" )
proportion_table_Q1$Question = "Q1"


proportion_table_Q2 <- merge(count_table_Q2, 
                             total_sex,
                             by = c("AgeBin_10", "Sex") )
proportion_table_Q2 <- proportion_table_Q2 %>%
  mutate(proportion = round(count / total_count, 3) )

colnames(proportion_table_Q2) = c("AgeBin_10","Sex","Reponse" , "count" , 
                                  "total_count" ,
                                  "proportion" )
proportion_table_Q2$Question = "Q2"


proportion_table_Q3 <- merge(count_table_Q3, 
                             total_sex,
                             by = c("AgeBin_10", "Sex") )
proportion_table_Q3 <- proportion_table_Q3 %>%
  mutate(proportion = round(count / total_count, 3) )


colnames(proportion_table_Q3) = c("AgeBin_10","Sex","Reponse" , "count" , 
                                  "total_count" ,
                                  "proportion" )
proportion_table_Q3$Question = "Q3"


final_all = bind_rows(proportion_table_Q1,
                      proportion_table_Q2,
                      proportion_table_Q3)


#custom_palette_male <- colorRampPalette(c("cyan", "blue"))(10)
#custom_palette_female <- colorRampPalette(c("pink", "magenta"))(10)
color_female = "magenta"
color_male = "blue"

# Combine palettes into a single vector with named elements
combined_palette <- c(Male = color_male, Female = color_female)

# Create a combined fill variable
#proportion_table <- final_all %>%
#  mutate(fill_color = paste(Sex, as.numeric(factor(AgeBin_10)), sep = "_"))

proportion_table <- final_all %>%
  mutate(fill_color = Sex)


# Create the bar plot with the custom color scale and position dodge
ggplot(proportion_table, aes(x = AgeBin_10, y = proportion, fill = fill_color)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(Question ~ Reponse) +
  labs(title = "",
       x = "Age",
       y = "Proportion",
       fill = "Sex") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values = combined_palette)


ggplot(proportion_table, aes(x = Question, y = proportion, fill = fill_color)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid( ~ Reponse) +
  labs(title = "",
       x = "Age",
       y = "Proportion",
       fill = "Sex") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values = combined_palette)

proportion_table_female = subset(proportion_table, Sex == "Female")
proportion_table_male = subset(proportion_table, Sex == "Male")

proportion_table_male %>%
  group_by(AgeBin_10, Question) %>%
  summarise(count_lines = n())

new_row <- data.frame(
  AgeBin_10 = "18-29",
  Sex = "Male",
  Reponse = "Increase social contact",
  count = 0,
  total_count = 182,
  proportion = 0,
  Question = "Q2",
  fill_color = "Male_1"
)

# Insert the new row before the 73rd row
proportion_table_male <- bind_rows(
  proportion_table_male[1:72, ],
  new_row,
  proportion_table_male[73:nrow(proportion_table_male), ]
)

# Check the result
print(proportion_table_male[71:75, ])


new_row2 <- data.frame(
  AgeBin_10 = "18-29",
  Sex = "Male",
  Reponse = "Increase social contact",
  count = 0,
  total_count = 182,
  proportion = 0,
  Question = "Q3",
  fill_color = "Male_1"
)

# Insert the new row before the 145th row
proportion_table_male <- bind_rows(
  proportion_table_male[1:144, ],
  new_row2,
  proportion_table_male[145:nrow(proportion_table_male), ]
)

# Check the result
print(proportion_table_male[143:147, ])



prop_test_sex_results = lapply(1:(dim(proportion_table_male)[1]), function(i)
  prop.test(c(proportion_table_female$count[i],
              proportion_table_male$count[i]),
            c(proportion_table_female$total_count[i],
              proportion_table_male$total_count[i]), alternative = "two.sided") )

p_values_prop = lapply(1:(dim(proportion_table_male)[1]), function(i) 
  round(prop_test_sex_results[[i]]$p.value,3) )
p_values_prop = do.call(rbind, p_values_prop)
AgeBin_10 = subset(proportion_table, Sex== "Female")$AgeBin_10
Responses = subset(proportion_table, Sex== "Female")$Reponse
Questions = subset(proportion_table, Sex== "Female")$Question

df_p_prop = data.frame( pvalues = p_values_prop,
                        AgeBin_10 = AgeBin_10,
                        Responses = Responses,
                        Questions = Questions
)

write.xlsx(df_p_prop, 
           file = paste(mydir_excel, 'prop_test.xlsx', sep = ''))


ggplot(df_p_prop, aes(x = AgeBin_10, y = pvalues)) +
  geom_boxplot(fill = "white") + 
  geom_jitter(aes(color = Responses), width = 0.2, height = 0) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  
  facet_grid(~ Questions) +
  labs(title = "",
       x = "Age",
       y = "P-values") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(color = guide_legend(title = "Responses", ncol = 3))




#########################
#########################
# Statistical Modelling #
#########################
#########################

##################################
#..........MODEL OVERALL.........#
##################################

kappa(model.matrix(~ Q1 - 1, data = df_COSI2_sym))

# Encode categorical variable Q1 into dummy variables
encoded_df <- cbind(df_COSI2_sym[, c("PTA", "SNR", "SRT")],
                    model.matrix(~ Q1 - 1, data = df_COSI2_sym))


colnames(encoded_df)

# Rename columns (optional)
colnames(encoded_df)[-c(1:3)] <- levels(df_COSI2_sym$Q1)


encoded_df_unique = encoded_df %>% distinct()
cor_matrix <- cor(encoded_df_unique, use = "complete.obs")

cor_matrix

###########################################
# 1) Fit multiple linear regression model #
###########################################

# COLLINEARITY DIAGNOSTIC
#https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html

lm_model <- lm(PTA ~ SNR + SRT  + ., data = encoded_df)
round(ols_eigen_cindex(lm_model),3)
ols_plot_diagnostics(lm_model)


#due to presence of multicollinearity between the levels, we do the following:

#########################
# by questions and select Others as the reference category
#######################

df_COSI2_sym$Q1 <- relevel(df_COSI2_sym$Q1, ref = "Others")
df_COSI2_sym$Q2 <- relevel(df_COSI2_sym$Q2, ref = "Others")
df_COSI2_sym$Q3 <- relevel(df_COSI2_sym$Q3, ref = "Others")

lm_model <- lm(PTA ~ SNR + SRT  + Q2,  #change the question
               data = df_COSI2_sym)


ols_plot_resid_fit_spread(lm_model)


# Summary of the model
lm_overall = summary(lm_model)

coeff_lm_overall_Q1 = as.data.frame(round(lm_overall$coefficients, 3))
coeff_lm_overall_Q2 = as.data.frame(round(lm_overall$coefficients, 3))
coeff_lm_overall_Q3 = as.data.frame(round(lm_overall$coefficients, 3))


write.xlsx(coeff_lm_overall_Q3, 
           file = paste(mydir_excel, "lm_overall_Q3.xlsx", sep = ""))#change the question

vif(lm_model)

coefficients(lm_model)

anova(lm_model)

coeff_lm_overall_Q1$Question = "Q1"
coeff_lm_overall_Q2$Question = "Q2"
coeff_lm_overall_Q3$Question = "Q3"

coeff_overall = rbind(coeff_lm_overall_Q1,coeff_lm_overall_Q3,coeff_lm_overall_Q3)


######################################
#........MODEL BY HL CATEGORY........#
######################################

df_COSI2_HL = df_COSI2_sym %>% group_split(HL)

lm_model_HL <- lapply(1:length(df_COSI2_HL), function(i)
  lm(PTA ~ SNR + SRT  + Q3,   #CHANGE THE QUESION HERE
     data = df_COSI2_HL[[i]]) ) 


# Summary of the model
lm_HL = lapply(1:length(df_COSI2_HL), function(i) summary(lm_model_HL[[i]]) )

coeff_lm_HL_Q1 = lapply(1:length(df_COSI2_HL), function(i)
  as.data.frame(round(lm_HL[[i]]$coefficients, 3)))

coeff_lm_HL_Q1 = do.call(rbind, coeff_lm_HL_Q1)

coeff_lm_HL_Q2 = lapply(1:length(df_COSI2_HL), function(i)
  as.data.frame(round(lm_HL[[i]]$coefficients, 3)))

coeff_lm_HL_Q2 = do.call(rbind, coeff_lm_HL_Q2)

coeff_lm_HL_Q3 = lapply(1:length(df_COSI2_HL), function(i)
  as.data.frame(round(lm_HL[[i]]$coefficients, 3)))

coeff_lm_HL_Q3 = do.call(rbind, coeff_lm_HL_Q3)


write.xlsx(coeff_lm_HL_Q3, 
           file = paste(mydir_excel, "lm_HL_Q3.xlsx", sep = ""))#change the question



#################################
#........MODEL BY GENDER........#
#################################

df_COSI2_Sex = df_COSI2_sym %>% group_split(Sex)

lm_model_Sex <- lapply(1:length(df_COSI2_Sex), function(i)
  lm(PTA ~ SNR + SRT  + Q3,   #CHANGE THE QUESION HERE
     data = df_COSI2_Sex[[i]]) ) 


# Summary of the model
lm_Sex = lapply(1:length(df_COSI2_Sex), function(i) summary(lm_model_Sex[[i]]) )

coeff_lm_Sex_Q1 = lapply(1:length(df_COSI2_Sex), function(i)
  as.data.frame(round(lm_Sex[[i]]$coefficients, 3)))

coeff_lm_Sex_Q1 = do.call(rbind, coeff_lm_Sex_Q1)

coeff_lm_Sex_Q2 = lapply(1:length(df_COSI2_Sex), function(i)
  as.data.frame(round(lm_Sex[[i]]$coefficients, 3)))

coeff_lm_Sex_Q2 = do.call(rbind, coeff_lm_Sex_Q2)

coeff_lm_Sex_Q3 = lapply(1:length(df_COSI2_Sex), function(i)
  as.data.frame(round(lm_Sex[[i]]$coefficients, 3)))

coeff_lm_Sex_Q3 = do.call(rbind, coeff_lm_Sex_Q3)


write.xlsx(coeff_lm_Sex_Q3, 
           file = paste(mydir_excel, "lm_Sex_Q3.xlsx", sep = ""))#change the question


##############################
#........MODEL BY AGE........#
##############################

df_COSI2_Age = df_COSI2_sym %>% group_split(AgeBin_10)

lm_model_Age <- lapply(1:length(df_COSI2_Age), function(i)
  lm(PTA ~ SNR + SRT  + Q3,   #CHANGE THE QUESION HERE
     data = df_COSI2_Age[[i]]) ) 


# Summary of the model
lm_Age = lapply(1:length(df_COSI2_Age), function(i) summary(lm_model_Age[[i]]) )

coeff_lm_Age_Q1 = lapply(1:length(df_COSI2_Age), function(i)
  as.data.frame(round(lm_Age[[i]]$coefficients, 3)))

coeff_lm_Age_Q1 = do.call(rbind, coeff_lm_Age_Q1)

coeff_lm_Age_Q2 = lapply(1:length(df_COSI2_Age), function(i)
  as.data.frame(round(lm_Age[[i]]$coefficients, 3)))

coeff_lm_Age_Q2 = do.call(rbind, coeff_lm_Age_Q2)

coeff_lm_Age_Q3 = lapply(1:length(df_COSI2_Age), function(i)
  as.data.frame(round(lm_Age[[i]]$coefficients, 3)))

coeff_lm_Age_Q3 = do.call(rbind, coeff_lm_Age_Q3)


write.xlsx(coeff_lm_Age_Q3, 
           file = paste(mydir_excel, "lm_Age_Q3.xlsx", sep = ""))#change the question


############################################
#........MODEL BY SEX AND AGE GROUP........#
############################################

# Group and split data by both Sex and AgeBin_10
df_COSI2_Sex_Age = df_COSI2_sym %>% group_by(Sex, AgeBin_10) %>% group_split()

# Fit linear models for each subgroup (by Sex and AgeBin_10) using Q1 as the predictor
lm_model_Sex_Age <- lapply(1:length(df_COSI2_Sex_Age), function(i)
  lm(PTA ~ SNR + SRT + Q1,   # Note: Only Q1 is used here
     data = df_COSI2_Sex_Age[[i]]) ) 

# Summary of the model for each subgroup
lm_Sex_Age = lapply(1:length(df_COSI2_Sex_Age), function(i) summary(lm_model_Sex_Age[[i]]) )

# Extract coefficients and bind them into a single data frame
coeff_lm_Sex_Age_Q1 = lapply(1:length(df_COSI2_Sex_Age), function(i)
  as.data.frame(round(lm_Sex_Age[[i]]$coefficients, 3)))

# Combine all coefficient data frames into one
coeff_lm_Sex_Age_Q1 = do.call(rbind, coeff_lm_Sex_Age_Q1)

# Write the coefficients to an Excel file
write.xlsx(coeff_lm_Sex_Age_Q1, 
           file = paste(mydir_excel, "lm_Sex_Age_Q1.xlsx", sep = ""))



coeff_lm_Sex_Age_Q1$Age_Group <- rep(c("18-29", "30-39", "40-49", "50-59", 
                                       "60-69", "70-79", "80-89", "90-99"), 
                                     each = 11)
coeff_lm_Sex_Age_Q1[c(89:176), c(1, 4, "Age_Group")]

# Assuming the first 88 rows are for females and the remaining are for males
coeff_lm_Sex_Age_Q1$Sex <- rep(c("F", "M"), each = 88)


df_p_age_sex = coeff_lm_Sex_Age_Q1 %>%
  dplyr::select(Age_Group, Sex, Pr.t.. = `Pr(>|t|)`)


# Clean up the variable names to remove prefixes like "Q1" and any suffix numbers
coeff_lm_Sex_Age_Q1 <- coeff_lm_Sex_Age_Q1 %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = gsub("^Q1|\\d+$", "", Variable)) %>%
  filter(!grepl("(Intercept)", Variable))

ggplot(coeff_lm_Sex_Age_Q1, aes(x = Age_Group, y = Estimate, color = Sex)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Estimate - 1.96*`Std. Error`, 
                    ymax = Estimate + 1.96*`Std. Error`),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Coefficient Estimates by Age Group and Sex",
       y = "Estimate", x = "Age Group")


ggplot(coeff_lm_Sex_Age_Q1, aes(x = Age_Group, 
                                y = Estimate, 
                                group = interaction(Variable, Sex), 
                                color = Sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Change in Estimates Across Age Groups",
       x = "Age Group", y = "Estimate")


ggplot(coeff_lm_Sex_Age_Q1, aes(x = as.numeric(factor(Age_Group)), 
                                y = Estimate, 
                                group = interaction(Variable, Sex), 
                                color = Sex,
                                fill = Sex)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_x_continuous(breaks = 1:8, 
                     labels = unique(coeff_lm_Sex_Age_Q1$Age_Group)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Smoothed Change in Estimates Across Age Groups",
       x = "Age Group", y = "Estimate")


heatmap_data <- coeff_lm_Sex_Age_Q1 %>%
  dplyr::select(Variable, Age_Group, Sex, Estimate) %>%
  spread(Age_Group, Estimate)

# Create heatmap
ggplot(melt(heatmap_data, id.vars = c("Variable", "Sex")), 
       aes(x = variable, y = Variable, fill = value)) +
  geom_tile() +
  facet_wrap(~ Sex) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-10,10), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Coefficient Estimates",
       x = "Age Group", y = "Variable", fill = "Estimate")




#Wilcox Tests

# Iterate over each dataframe to perform the Wilcoxon test for SRT and SNR
df_COSI2_Age = df_COSI2_sym %>% group_by(AgeBin_10) %>% group_split()

# Iterate over each dataframe to perform the Wilcoxon test for SRT and SNR
results <- lapply(df_COSI2_Age, function(df) {
  age_group <- unique(df$AgeBin_10)
  
  # Subset female and male data, remove rows with NA values in SRT or SNR
  female_data <- df %>% filter(Sex == "Female") %>% filter(!is.na(SRT) & !is.na(SNR))
  male_data <- df %>% filter(Sex == "Male") %>% filter(!is.na(SRT) & !is.na(SNR))
  
  # Debug: Print the number of rows for each group
  cat("Age Group:", age_group, "\n")
  cat("Female Count:", nrow(female_data), "\n")
  cat("Male Count:", nrow(male_data), "\n")
  
  # Ensure there is enough data in both groups
  if (nrow(female_data) > 1 && nrow(male_data) > 1) {
    # Perform Wilcoxon test for SRT
    srt_result <- wilcox.test(female_data$SRT, male_data$SRT, exact = FALSE, alternative = "two.sided")
    
    # Perform Wilcoxon test for SNR
    snr_result <- wilcox.test(female_data$SNR, male_data$SNR, exact = FALSE, alternative = "two.sided")
    
    # Return results
    list(
      age_group = age_group,
      srt_p_value = round(srt_result$p.value,3),
      snr_p_value = round(snr_result$p.value,3)
    )
  } else {
    # Return NA if not enough data
    list(
      age_group = age_group,
      srt_p_value = NA,
      snr_p_value = NA
    )
  }
})

# Convert results to a data frame for easier interpretation
results_df <- do.call(rbind, lapply(results, as.data.frame))
print(results_df)




# Get unique needs in Q1
q1_needs <- unique(df_COSI2_sym$Q1)

# Iterate over each dataframe and each Q1 need to perform the Wilcoxon test
results_q1 <- lapply(df_COSI2_Age, function(df) {
  age_group <- unique(df$AgeBin_10)
  
  # Iterate over each Q1 need
  lapply(q1_needs, function(need) {
    # Subset the data by the specific need
    df_need <- df %>% filter(Q1 == need)
    
    # Subset female and male data, remove rows with NA values in SRT or SNR
    female_data <- df_need %>% filter(Sex == "Female") %>% filter(!is.na(SRT) & !is.na(SNR))
    male_data <- df_need %>% filter(Sex == "Male") %>% filter(!is.na(SRT) & !is.na(SNR))
    
    # Debug: Print the number of rows for each group
    cat("Age Group:", age_group, "Need:", need, "\n")
    cat("Female Count:", nrow(female_data), "\n")
    cat("Male Count:", nrow(male_data), "\n")
    
    # Ensure there is enough data in both groups
    if (nrow(female_data) > 1 && nrow(male_data) > 1) {
      # Perform Wilcoxon test for SRT
      srt_result <- wilcox.test(female_data$SRT, male_data$SRT, exact = FALSE, alternative = "two.sided")
      
      # Perform Wilcoxon test for SNR
      snr_result <- wilcox.test(female_data$SNR, male_data$SNR, exact = FALSE, alternative = "two.sided")
      
      # Return results
      list(
        age_group = age_group,
        need = need,
        srt_p_value = round(srt_result$p.value, 3),
        snr_p_value = round(snr_result$p.value, 3)
      )
    } else {
      # Return NA if not enough data
      list(
        age_group = age_group,
        need = need,
        srt_p_value = NA,
        snr_p_value = NA
      )
    }
  })
})

# Flatten the list and convert to a data frame for easier interpretation
results_q1_df <- do.call(rbind, lapply(unlist(results_q1, recursive = FALSE), as.data.frame))
print(results_q1_df)


############################################
#........MODEL BY PTA AND AGE GROUP........#
############################################

# Group and split data by both PTA and AgeBin_10
df_COSI2_PTA_Age = df_COSI2_sym %>% group_by(HL, AgeBin_10) %>% group_split()

sapply(df_COSI2_PTA_Age, nrow)

# Fit linear models for each subgroup (by Sex and AgeBin_10) using Q1 as the predictor
lm_model_PTA_Age <- lapply(1:length(df_COSI2_PTA_Age), function(i) {
  # Get the subset of data
  data_subset <- df_COSI2_PTA_Age[[i]]
  
  # Check if Q1 has more than one unique value and if there are at least 12 observations
  if (nrow(data_subset) >= 12 && length(unique(data_subset$Q1)) > 1) {
    # Fit the model if conditions are met
    lm(PTA ~ SNR + SRT + Q1, data = data_subset)
  } else {
    # Return NULL if the model cannot be fitted
    NULL
  }
})

# Filter out NULL models before summarizing
valid_indices <- which(!sapply(lm_model_PTA_Age, is.null))
lm_model_PTA_Age <- lm_model_PTA_Age[valid_indices]

# Summary of the model for each subgroup
lm_PTA_Age <- lapply(lm_model_PTA_Age, summary)

# Extract coefficients and add subgroup information (HL and AgeBin_10)
coeff_lm_PTA_Age_Q1 <- lapply(valid_indices, function(i) {
  # Get the summary of the model
  summary_model <- lm_PTA_Age[[which(valid_indices == i)]]
  
  # Extract the coefficients and create a data frame
  coeff_df <- as.data.frame(round(summary_model$coefficients, 3))
  
  # Add HL and AgeBin_10 information to the data frame
  coeff_df$HL <- unique(df_COSI2_PTA_Age[[i]]$HL)
  coeff_df$AgeBin_10 <- unique(df_COSI2_PTA_Age[[i]]$AgeBin_10)
  
  return(coeff_df)
})

# Combine all coefficient data frames into one
coeff_lm_PTA_Age_Q1 <- do.call(rbind, coeff_lm_PTA_Age_Q1)

# Write the coefficients to an Excel file
write.xlsx(coeff_lm_PTA_Age_Q1, 
           file = paste(mydir_excel, "lm_PTA_Age_Q1.xlsx", sep = ""))

# Displaying the combined data frame
print(coeff_lm_PTA_Age_Q1)


df_p_age_pta = coeff_lm_PTA_Age_Q1 %>%
  dplyr::select(AgeBin_10, HL, Pr.t.. = `Pr(>|t|)`)


# Clean up the variable names to remove prefixes like "Q1" and any suffix numbers
coeff_lm_PTA_Age_Q1 <- coeff_lm_PTA_Age_Q1 %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = gsub("^Q1|\\d+$", "", Variable)) %>%
  filter(!grepl("(Intercept)", Variable))



ggplot(coeff_lm_PTA_Age_Q1, aes(x = AgeBin_10, y = Estimate, color = HL)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Estimate - 1.96*`Std. Error`, 
                    ymax = Estimate + 1.96*`Std. Error`),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Coefficient Estimates by Age Group and PTA",
       y = "Estimate", x = "Age Group")


ggplot(coeff_lm_PTA_Age_Q1, aes(x = AgeBin_10, 
                                y = Estimate, 
                                group = interaction(Variable, HL), 
                                color = HL)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Change in Estimates Across Age Groups",
       x = "Age Group", y = "Estimate")


ggplot(coeff_lm_PTA_Age_Q1, aes(x = as.numeric(factor(AgeBin_10)), 
                                y = Estimate, 
                                group = interaction(Variable, HL), 
                                color = HL,
                                fill = HL)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_x_continuous(breaks = 1:8, 
                     labels = unique(coeff_lm_PTA_Age_Q1$AgeBin_10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Smoothed Change in Estimates Across Age Groups",
       x = "Age Group", y = "Estimate")


heatmap_data <- coeff_lm_PTA_Age_Q1 %>%
  dplyr::select(Variable, AgeBin_10, HL, Estimate) %>%
  spread(AgeBin_10, Estimate)

# Create heatmap
ggplot(melt(heatmap_data, id.vars = c("Variable", "HL")), 
       aes(x = variable, y = Variable, fill = value)) +
  geom_tile() +
  facet_wrap(~ HL) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-10,10), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Coefficient Estimates",
       x = "Age Group", y = "Variable", fill = "Estimate")

data_pval <- coeff_lm_PTA_Age_Q1 %>%
  dplyr::select(AgeBin_10, HL, Variable, `Pr(>|t|)` ) %>%
  pivot_wider(names_from = HL, values_from = `Pr(>|t|)` ) %>%
  arrange(AgeBin_10)


# Create LaTeX table for p-values only
kable(data_pval, "latex", booktabs = TRUE, escape = FALSE, col.names = col_names) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  pack_rows("Age Group: 18-29", 1, 9, bold = TRUE, italic = TRUE) %>%
  pack_rows("Age Group: 30-39", 10, 18, bold = TRUE, italic = TRUE) %>%
  # Add more age groups as needed
  add_header_above(c(" " = 2, "Normal to Mild" = 3, "Moderate to Severe" = 3, "Profound" = 1))


###########################
#BOXPLTOS P-VALUES RESULTS#
###########################

mydf_box = read_excel(paste(mydir_excel, 
                            "mydata_boxplots.xlsx", 
                            sep =""), 
                      sheet = 1, col_names = TRUE)

mydf_box$`P-value` = round(as.numeric(mydf_box$`P-value`),3)

desired_order <- c("Intercept", 
                   "SNR", 
                   "SRT", 
                   "Increase social contact", 
                   "Conversation with 1 or 2 in silence", 
                   "Group conversation in noise", 
                   "Group conversation in silence", 
                   "Church or meeting", 
                   "Unfamiliar interlocutor on the phone", 
                   "Feeling embarrassed or stupid", 
                   "Television/radio at normal volume")

# Reorder the levels of the Variable factor
mydf_box$Variable <- factor(mydf_box$Variable, levels = desired_order)


mydf_box_overall = filter(mydf_box, Model == "Overal")

mydf_box_hl = filter(mydf_box, Model == "HL")
mydf_box_hl$Split_Var = factor(mydf_box_hl$Split_Var, levels = levels(df_COSI2_sym$HL))


mydf_box_sex = filter(mydf_box, Model == "Sex")
mydf_box_age = filter(mydf_box, Model == "Age")



ggplot(mydf_box_overall, aes(x = Variable, y = `P-value` )) +
  geom_boxplot(fill = "white") + 
  geom_jitter(aes(color = Question), width = 0.2, height = 0) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "",
       x = "Variables",
       y = "P-values") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(color = guide_legend(title = "Questions", ncol = 3))

mydf_melted_overall <- melt(mydf_box_overall, 
                            id.vars = c("Variable", "Question"),
                            measure.vars = "P-value")


ggplot(mydf_melted_overall, aes(x = Variable, y = Question, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C")  +
  labs(title = "",
       x = "Variables",
       y = "Questions",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

# Define a new column for the fill color based on the p-value conditions
mydf_melted_overall <- mydf_melted_overall %>%
  mutate(fill_color = case_when(
    value > 0.05 ~ "blue",
    value > 0.01 & value <= 0.05 ~ "cyan",
    value <= 0.01 ~ "azure2"
  ))


# Plotting
ggplot(mydf_melted_overall, aes(x = Variable, y = Question, fill = fill_color)) +
  geom_tile() +
  scale_fill_manual(values = c("blue", "cyan", "azure2"), 
                    breaks = c("blue", "cyan", "azure2"), 
                    labels = c("> 0.05", "0.01 - 0.05", "<= 0.01")) +
  labs(title = "",
       x = "Variables",
       y = "Question",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))




ggplot(mydf_box_hl, aes(x = Variable, y = `P-value` )) +
  geom_boxplot(fill = "white") + 
  geom_jitter(aes(color = Question), width = 0.2, height = 0) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  facet_grid(~Split_Var) +
  labs(title = "",
       x = "Variables",
       y = "P-values") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(color = guide_legend(title = "Question", ncol = 3)) #+
#scale_color_viridis_d() 

mydf_melted_hl <- melt(mydf_box_hl, 
                       id.vars = c("Variable", "Question", "Split_Var"),
                       measure.vars = "P-value")

ggplot(mydf_melted_hl, aes(x = Variable, y = Split_Var, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") + 
  facet_wrap(~ Question) +
  labs(title = "",
       x = "Variables",
       y = "Hearing Loss",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

mydf_melted_hl <- mydf_melted_hl %>%
  mutate(fill_color = case_when(
    value > 0.05 ~ "blue",
    value > 0.01 & value <= 0.05 ~ "cyan",
    value <= 0.01 ~ "azure2"
  ))


ggplot(mydf_melted_hl, aes(x = Variable, y = Split_Var, fill = fill_color)) +
  geom_tile() + 
  facet_wrap(~ Question) +
  scale_fill_manual(values = c("blue", "cyan", "azure2"), 
                    breaks = c("blue", "cyan", "azure2"), 
                    labels = c("> 0.05", "0.01 - 0.05", "<= 0.01")) +
  labs(title = "",
       x = "Variables",
       y = "Hearing Loss",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

#ALTERNATIVESSS
# Create a bubble plot
# Define discrete colors for p-values
discrete_colors <- scale_color_manual(
  values = c("<= 0.01" = "blue", "0.01 - 0.05" = "cyan", "> 0.05" = "azure2"),
  breaks = c("<= 0.01", "0.01 - 0.05", "> 0.05"),
  labels = c("<= 0.01", "0.01 - 0.05", "> 0.05")
)

# Create the bubble plot
ggplot(mydf_melted_hl, aes(x = Variable, y = Split_Var, size = value, 
                           color = cut(value, breaks = c(-Inf, 0.01, 0.05, Inf), 
                                       labels = c("<= 0.01", "0.01 - 0.05", "> 0.05")))) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Question, scales = "free_x") +
  scale_size_continuous(range = c(2, 10), breaks = c(0.01, 0.05, 0.1), 
                        labels = c("<= 0.01", "0.01 - 0.05", "> 0.05")) +
  discrete_colors +  # Apply discrete color scale
  labs(title = "Bubble Plot of P-values",
       x = "Variables",
       y = "Hearing Loss",
       color = "P-value",
       size = "P-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        plot.background = element_rect(fill = 'white', color = NA),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))


ggplot(mydf_box_sex, aes(x = Variable, y = `P-value` )) +
  geom_boxplot(fill = "white") + 
  geom_jitter(aes(color = Question), width = 0.2, height = 0) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  facet_grid(~Split_Var) +
  labs(title = "",
       x = "Variables",
       y = "P-values") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(color = guide_legend(title = "Question", ncol = 3)) #+
#scale_color_viridis_d() 

mydf_melted_sex <- melt(mydf_box_sex, 
                        id.vars = c("Variable", "Question", "Split_Var"),
                        measure.vars = "P-value")

ggplot(mydf_melted_sex, aes(x = Variable, y = Split_Var, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") + 
  facet_wrap(~ Question) +
  labs(title = "",
       x = "Variables",
       y = "Sex",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))


mydf_melted_sex <- mydf_melted_sex %>%
  mutate(fill_color = case_when(
    value > 0.05 ~ "blue",
    value > 0.01 & value <= 0.05 ~ "cyan",
    value <= 0.01 ~ "azure2"
  ))


ggplot(mydf_melted_sex, aes(x = Variable, y = Split_Var, fill = fill_color)) +
  geom_tile() + 
  facet_wrap(~ Question) +
  scale_fill_manual(values = c("blue", "cyan", "azure2"), 
                    breaks = c("blue", "cyan", "azure2"), 
                    labels = c("> 0.05", "0.01 - 0.05", "<= 0.01")) +
  labs(title = "",
       x = "Variables",
       y = "Sex",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))





ggplot(mydf_box_age, aes(x = Variable, y = `P-value` )) +
  geom_boxplot(fill = "white") + 
  geom_jitter(aes(color = Question), width = 0.2, height = 0) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  facet_grid(~Split_Var) +
  labs(title = "",
       x = "Variables",
       y = "P-values") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  guides(color = guide_legend(title = "Question", ncol = 3)) #+


mydf_melted_age <- melt(mydf_box_age, 
                        id.vars = c("Variable", "Question", "Split_Var"),
                        measure.vars = "P-value")

ggplot(mydf_melted_age, aes(x = Variable, y = Split_Var, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") + 
  facet_wrap(~ Question) +
  labs(title = "",
       x = "Variables",
       y = "Age Group",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))


mydf_melted_age <- mydf_melted_age %>%
  mutate(fill_color = case_when(
    value > 0.05 ~ "blue",
    value > 0.01 & value <= 0.05 ~ "cyan",
    value <= 0.01 ~ "azure2"
  ))

ggplot(mydf_melted_age, aes(x = Variable, y = Split_Var, fill = fill_color)) +
  geom_tile() + 
  facet_wrap(~ Question) +
  scale_fill_manual(values = c("blue", "cyan", "azure2"), 
                    breaks = c("blue", "cyan", "azure2"), 
                    labels = c("> 0.05", "0.01 - 0.05", "<= 0.01")) +
  labs(title = "",
       x = "Variables",
       y = "Age Group",
       fill = "P-value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))


# Create the bubble plot
ggplot(mydf_melted_age, aes(x = Variable, y = Split_Var, size = value, 
                           color = cut(value, breaks = c(-Inf, 0.01, 0.05, Inf), 
                                       labels = c("<= 0.01", "0.01 - 0.05", "> 0.05")))) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Question, scales = "free_x") +
  scale_size_continuous(range = c(2, 10), breaks = c(0.01, 0.05, 0.1), 
                        labels = c("<= 0.01", "0.01 - 0.05", "> 0.05")) +
  discrete_colors +  # Apply discrete color scale
  labs(title = "Bubble Plot of P-values",
       x = "Variables",
       y = "Hearing Loss",
       color = "P-value",
       size = "P-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        plot.background = element_rect(fill = 'white', color = NA),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))




##########################################################################
#  ANALYSIS FOR CUT OFF OF AGE AND SEX AND GENDER # SEGMENTED REGRESSION #
##########################################################################
library("segmented")
library("mgcv")
library("rpart")


lm_model <- lm(PTA ~ Age + Q1 +SRT + SNR,  #change the question
               data = df_COSI2_sym)


segmented_model <- segmented(lm_model, seg.Z = ~Age)
summary(segmented_model)
breakpoints <- segmented_model$psi[, "Est."]
print(breakpoints)


# Visualize segmented model
plot(df_COSI2_sym$Age, df_COSI2_sym$PTA, pch = 16)
plot(segmented_model, add = TRUE)

# Fit a GAM model
gam_model <- gam(PTA ~ s(Age) + Q1, data = df_COSI2_sym)
summary(gam_model)
plot(gam_model, se = TRUE)



# Segmented Regression with Interactions
# Fit segmented models and extract breakpoints and fitted values
segmented_models <- list()
breakpoints <- list()
toplot_list <- list()

for (q_level in unique(df_COSI2_sym$Q1)) { #CHANGE QUESTION!!!!!!!!!
  subset_data <- df_COSI2_sym %>% filter(Q1 == q_level,  #CHANGE QUESTION!!!!!!!!!
                                         Sex == "Male",
                                         Age < 90,
                                         Age > 20)
  if (nrow(subset_data) > 20) { # Ensure there are enough data points
    linear_model <- lm(PTA ~ Age, data = subset_data)
    segmented_model <- segmented(linear_model, 
                                 seg.Z = ~Age,
                                 npsi = 2,
                                 control = seg.control(display = FALSE))
    segmented_models[[q_level]] <- segmented_model
    breakpoints[[q_level]] <- segmented_model$psi[, "Est."]
    
    # Extract fitted values from the segmented model
    fitted_values_plot <- fitted(segmented_model)
    
    # Extract breakpoints
    breakpoints_plot <- as.numeric(segmented_model$psi[, "Est."])
    
    # Create a dataframe for plotting
    toplot <- data.frame(
      age = subset_data$Age,
      outcome = subset_data$PTA,
      fitted_values = fitted_values_plot,
      Q1 = q_level #CHANGE QUESTION!!!!!!!!!
    )
    
    toplot_list[[q_level]] <- toplot
  }
}

# Combine all toplot dataframes into one
combined_toplot <- do.call(rbind, toplot_list)
combined_breakpoints <- do.call(rbind, lapply(names(breakpoints), function(name) {
  data.frame(Q1 = name, age = breakpoints[[name]]) #CHANGE QUESTION!!!!
}))


ggplot(combined_toplot, aes(x = age, y = outcome)) +
  geom_point(alpha = 0.6) +  # Scatter plot of the data points
  geom_line(aes(y = fitted_values), color = "blue", size = 1) +  # Line for fitted values
  geom_vline(data = combined_breakpoints, aes(xintercept = age), 
             color = "cyan3", 
             linetype = "dashed", size = 1) +  # Vertical lines for breakpoints
  theme_bw() +
  labs(title = "Segmented Regression Model by Q1 Levels", #CHANGE QUESTION
       x = "Age",
       y = "PTA") +
  facet_wrap(~ Q1, scales = "free") + #CHANGE QUESTION
  theme( plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))



#######################################
#ATUOMATIC CODE FOR ALL THE QUESTIONS #
#######################################

# List of questions to process
questions <- c("Q1", "Q2", "Q3")

# Initialize lists to store models, breakpoints, and data for plotting
segmented_models <- list()
breakpoints <- list()
toplot_list <- list()


# Loop through each question catching errors - use this
for (question in questions) {
  # Loop through each level of the current question
  for (q_level in unique(df_COSI2_sym[[question]])) {
    subset_data <- df_COSI2_sym %>% filter(get(question) == q_level, 
                                           Sex == "Female", #Male
                                           Age < 90,
                                           Age > 20)
    if (nrow(subset_data) > 20) { # Ensure there are enough data points
      linear_model <- lm(PTA ~ Age, data = subset_data)
      
      tryCatch({
        segmented_model <- segmented(linear_model, 
                                     seg.Z = ~Age,
                                     npsi = 2,
                                     control = seg.control(display = FALSE))
        segmented_models[[paste(question, q_level, sep = "_")]] <- segmented_model
        breakpoints[[paste(question, q_level, sep = "_")]] <- segmented_model$psi[, "Est."]
        
        # Extract fitted values from the segmented model
        fitted_values_plot <- fitted(segmented_model)
        
        # Create a dataframe for plotting
        toplot <- data.frame(
          age = subset_data$Age,
          outcome = subset_data$PTA,
          fitted_values = fitted_values_plot,
          question = question,
          q_level = q_level
        )
        
        toplot_list[[paste(question, q_level, sep = "_")]] <- toplot
      }, error = function(e) {
        # If an error occurs, try with one breakpoint
        tryCatch({
          segmented_model <- segmented(linear_model, 
                                       seg.Z = ~Age,
                                       npsi = 1,
                                       control = seg.control(display = FALSE))
          segmented_models[[paste(question, q_level, sep = "_")]] <- segmented_model
          breakpoints[[paste(question, q_level, sep = "_")]] <- segmented_model$psi[, "Est."]
          
          # Extract fitted values from the segmented model
          fitted_values_plot <- fitted(segmented_model)
          
          # Create a dataframe for plotting
          toplot <- data.frame(
            age = subset_data$Age,
            outcome = subset_data$PTA,
            fitted_values = fitted_values_plot,
            question = question,
            q_level = q_level
          )
          
          toplot_list[[paste(question, q_level, sep = "_")]] <- toplot
        }, error = function(e) {
          message("Error fitting segmented model with one breakpoint for ", question, " level ", q_level)
        })
      })
    }
  }
}


# Combine all toplot dataframes into one
combined_toplot <- do.call(rbind, toplot_list)
combined_breakpoints <- do.call(rbind, lapply(names(breakpoints), function(name) {
  question_level <- unlist(strsplit(name, "_"))
  data.frame(question = question_level[1], q_level = question_level[2], age = breakpoints[[name]])
}))

combined_toplot = subset(combined_toplot, q_level != "Others")#remove Others from plot
combined_breakpoints = subset(combined_breakpoints, q_level != "Others")#remove Others from plot
combined_breakpoints$age  = round(combined_breakpoints$age, 0)


# Plot and save for each question
for (question in questions) {
  plot <- ggplot(combined_toplot %>% filter(question == !!question), aes(x = age, y = outcome)) +
    geom_point(alpha = 0.6) +  # Scatter plot of the data points
    geom_line(aes(y = fitted_values), color = "blue", size = 1) +  # Line for fitted values
    geom_vline(data = combined_breakpoints %>% filter(question == !!question), 
               aes(xintercept = age), 
               color = "magenta",  #cyan3
               linetype = "dashed", size = 1) +  # Vertical lines for breakpoints
    theme_bw() +
    labs(title = paste("", ""), 
         x = "Age",
         y = "PTA") +
    facet_wrap(~ q_level, scales = "free") +
    theme(plot.background = element_rect(fill = 'white', color = NA),
          panel.background = element_rect(fill = 'white', color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 8, angle = 90),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          strip.text = element_text(color = 'black', size = 13),
          strip.background = element_rect(fill = 'white'))
  
  ggsave(filename = paste0(mydir_figs, "segmented_model_", 
                           question, "_female", #change this to male
                           ".pdf"),
         plot = plot, 
         width = 10, height = 8)
}


combined_breakpoints$age <- round(combined_breakpoints$age, 0)
 
# Save the breakpoints dataframe to a CSV file
write.xlsx(combined_breakpoints, 
          paste(mydir_excel, "breakpoints_female.xlsx", sep=""), #change male and female
          rowNames = FALSE)

break_females = combined_breakpoints
break_males = combined_breakpoints


###################################################################
#  FOCUSED ANALYSIS ON THE MOST IMPORTANT NEEDS BY GENDER AND AGE #
###################################################################

#1 Gender

# Remove rows with NA in SNR or SRT
df_COSI2_sym_clean <- df_COSI2_sym[!is.na(df_COSI2_sym$SNR) & !is.na(df_COSI2_sym$SRT), ]

# List of questions
questions <- c("Q1", "Q2", "Q3")

# Initialize an empty list to store results
all_test_results <- list()

# Loop through each question and perform the Wilcoxon Rank-Sum Test
for (question in questions) {
  # Perform tests for SNR and SRT
  snr_test_results <- perform_wilcoxon_test(df_COSI2_sym_clean, question, "SNR")
  srt_test_results <- perform_wilcoxon_test(df_COSI2_sym_clean, question, "SRT")
  
  # Combine results
  combined_results <- bind_rows(
    snr_test_results %>% mutate(test_variable = "SNR"),
    srt_test_results %>% mutate(test_variable = "SRT")
  )
  
  # Store results in the list
  all_test_results[[question]] <- combined_results
}


Q1 <- all_test_results$Q1 %>%
  rename(response = Q1)

Q2 <- all_test_results$Q2 %>%
  rename(response = Q2)

Q3 <- all_test_results$Q3 %>%
  rename(response = Q3)

# Combine the data frames
combined_test_results<- bind_rows(Q1, Q2, Q3)
combined_test_results <- combined_test_results %>%
  drop_na()
combined_test_results <- combined_test_results %>%
  filter(response != "Others")

# Create the boxplot ) Zilco or U Mann non parametric for not normal
ggplot_box = ggplot(combined_test_results, aes(x = test_variable, 
                                  y = p_value)) +
  geom_boxplot(outlier.shape = NA) + # Outliers will be represented by points
  geom_point(position = position_jitter(width = 0.2), 
             size = 3, aes(color = question)) +
  theme_bw() +
  labs(title = "",
       x = "",
       y = "P-value",
       color = "Question") + 
  facet_wrap(~ response) + #, scales = "free_y"
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
 # scale_y_log10() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        strip.text = element_text(color = 'black', size = 12),
        strip.background = element_rect(fill = 'gray', color = 'black'),
        legend.position = "bottom",
        legend.title = element_text(size = 13)) +
  guides(color = guide_legend(ncol = 3))

ggplot_box

ggsave(filename = paste0(mydir_figs, 
                         "box_wilcox_gender", 
                         ".pdf"),
       plot = ggplot_box, 
       width = 10, height = 8)

combined_test_results$p_value <- round(combined_test_results$p_value, 3)
write.xlsx(combined_test_results, 
           paste(mydir_excel, "wilcoxx_gender_diff.xlsx", sep=""), 
           rowNames = FALSE)


#2 Age

df_COSI2_sym_clean_filtered <- df_COSI2_sym_clean %>%
  filter(!Q1 %in% "Others",
         !Q2 %in% "Others",
         !Q3 %in% "Others")


female_breakpoints <-split(break_females$age, list(break_females$question, break_females$q_level))
male_breakpoints <- split(break_males$age, list(break_males$question, break_males$q_level))


# Combine breakpoints for females and males into one data frame
breakpoints_all <- bind_rows(
  break_females %>%
    mutate(gender = "Female"),
  break_males %>%
    mutate(gender = "Male")
)


df_COSI2_sym_clean_filtered <- df_COSI2_sym_clean_filtered %>%
  rowwise() %>%
  mutate(segment_age_Q1 = get_segment_age(Sex, "Q1", Q1, Age, female_breakpoints, male_breakpoints),
         segment_age_Q2 = get_segment_age(Sex, "Q2", Q2, Age, female_breakpoints, male_breakpoints),
         segment_age_Q3 = get_segment_age(Sex, "Q3", Q3, Age, female_breakpoints, male_breakpoints))



# Apply the function to your data
p_values_Q1 <- collect_p_values_with_sex_kruskal(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
p_values_Q2 <- collect_p_values_with_sex_kruskal(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
p_values_Q3 <- collect_p_values_with_sex_kruskal(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")


p_values_Q1 <- collect_p_values_with_sex_median(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
p_values_Q2 <- collect_p_values_with_sex_median(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
p_values_Q3 <- collect_p_values_with_sex_median(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")


# Combine all p-values into one data frame
p_values_combined_kruskal <- bind_rows(p_values_Q1, p_values_Q2, p_values_Q3)
p_values_combined_median <- bind_rows(p_values_Q1, p_values_Q2, p_values_Q3)

p_values_combined_kruskal$P_Value <- round(p_values_combined_kruskal$P_Value, 3)
write.xlsx(p_values_combined_kruskal, 
           paste(mydir_excel, "kruskal_age_seg_gender_diff.xlsx", sep=""), 
           rowNames = FALSE)

p_values_combined_median$P_Value <- round(p_values_combined_median$P_Value, 3)
write.xlsx(p_values_combined_median, 
           paste(mydir_excel, "median_age_seg_gender_diff.xlsx", sep=""), 
           rowNames = FALSE)


# Step 3: Plot the results
ggplot(p_values_combined_kruskal, aes(x = Value_Col, #p_values_combined_median
                              y = P_Value#-log10(P_Value), 
                              )) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), 
             size = 3, aes(color = Question)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(x = "",
       y = "P-value", #-log10(P_Value)
       title = "") +
  theme_bw() +
  facet_grid(Sex~ Level)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        strip.background = element_rect(fill = 'lightblue', color = 'black'),
        legend.position = "bottom",
        legend.title = element_text(size = 13)) +
  guides(color = guide_legend(ncol = 3))



#INVESTIGATE THE SAMPLE SIWE AND DISTRIBUTION OF SNR and SRT by segments
#This is further investigation -- not needed in the paper
# Calculate sample sizes for Q1
sample_sizes_Q1 <- check_sample_sizes(df_COSI2_sym_clean_filtered, 
                                      "Q1", "segment_age_Q1")

# Calculate sample sizes for Q2
sample_sizes_Q2 <- check_sample_sizes(df_COSI2_sym_clean_filtered, 
                                      "Q2", "segment_age_Q2")

# Calculate sample sizes for Q3
sample_sizes_Q3 <- check_sample_sizes(df_COSI2_sym_clean_filtered, 
                                      "Q3", "segment_age_Q3")


# Plot sample sizes for Q1
gg_ss_overall_Q1 = plot_sample_sizes(sample_sizes_Q1, "Q1", "segment_age_Q1")
# Plot sample sizes for Q2
gg_ss_overall_Q2 =plot_sample_sizes(sample_sizes_Q2, "Q2", "segment_age_Q2")
# Plot sample sizes for Q3
gg_ss_overall_Q3 = plot_sample_sizes(sample_sizes_Q3, "Q3", "segment_age_Q3")

ggsave(filename = paste0(mydir_figs, 
                         "gg_ss_overall_Q1", 
                         ".pdf"),
       plot = gg_ss_overall_Q1, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_ss_overall_Q2", 
                         ".pdf"),
       plot = gg_ss_overall_Q2, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_ss_overall_Q3", 
                         ".pdf"),
       plot = gg_ss_overall_Q3, 
       width = 14, height = 8)



# Get sample sizes including HL
sample_sizes_HL_Q1 <- check_sample_sizes_HL(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
sample_sizes_HL_Q2 <- check_sample_sizes_HL(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
sample_sizes_HL_Q3 <- check_sample_sizes_HL(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")

# Plot sample sizes with HL
gg_ss_HL_Q1 = plot_sample_sizes_HL(sample_sizes_HL_Q1, "Q1", "segment_age_Q1")
gg_ss_HL_Q2 = plot_sample_sizes_HL(sample_sizes_HL_Q2, "Q2", "segment_age_Q2")
gg_ss_HL_Q3 = plot_sample_sizes_HL(sample_sizes_HL_Q3, "Q3", "segment_age_Q3")


ggsave(filename = paste0(mydir_figs, 
                         "gg_ss_HL_Q1", 
                         ".pdf"),
       plot = gg_ss_HL_Q1, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_ss_HL_Q2", 
                         ".pdf"),
       plot = gg_ss_HL_Q2, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_ss_HL_Q3", 
                         ".pdf"),
       plot = gg_ss_HL_Q3, 
       width = 14, height = 8)


#DISTRIBUTION
# Check SNR distributions for Q1
gg_distr_SNR_Q1 = plot_distributions(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
# Check SNR distributions for Q2
gg_distr_SNR_Q2 = plot_distributions(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
# Check SNR distributions for Q3
gg_distr_SNR_Q3 = plot_distributions(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")


ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_SNR_Q1", 
                         ".pdf"),
       plot = gg_distr_SNR_Q1, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_SNR_Q2", 
                         ".pdf"),
       plot = gg_distr_SNR_Q2, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_SNR_Q3", 
                         ".pdf"),
       plot = gg_distr_SNR_Q3, 
       width = 14, height = 8)



# Check SRT distributions for Q1
gg_distr_SRT_Q1 = plot_distributions_SRT(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
# Check SRT distributions for Q2
gg_distr_SRT_Q2 = plot_distributions_SRT(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
# Check SRT distributions for Q3
gg_distr_SRT_Q3 = plot_distributions_SRT(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")



ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_SRT_Q1", 
                         ".pdf"),
       plot = gg_distr_SRT_Q1, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_SRT_Q2", 
                         ".pdf"),
       plot = gg_distr_SRT_Q2, 
       width = 14, height = 8)

ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_SRT_Q3", 
                         ".pdf"),
       plot = gg_distr_SRT_Q3, 
       width = 14, height = 8)



#More plots for advanced studies
# Boxplots
gg_distr_HL_Q1 = plot_distributions_small_multiples(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
gg_distr_HL_Q2 = plot_distributions_small_multiples(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
gg_distr_HL_Q3 = plot_distributions_small_multiples(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")



ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_HL_Q1", 
                         ".pdf"),
       plot = gg_distr_HL_Q1, 
       width = 23, height = 12)

ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_HL_Q2", 
                         ".pdf"),
       plot = gg_distr_HL_Q2, 
       width = 23, height = 12)

ggsave(filename = paste0(mydir_figs, 
                         "gg_distr_HL_Q3", 
                         ".pdf"),
       plot = gg_distr_HL_Q3, 
       width = 23, height = 12)

# Heatmaps
gg_heat_HL_Q1 = plot_distributions_heatmap(df_COSI2_sym_clean_filtered, "Q1", "segment_age_Q1")
gg_heat_HL_Q2 = plot_distributions_heatmap(df_COSI2_sym_clean_filtered, "Q2", "segment_age_Q2")
gg_heat_HL_Q3 = plot_distributions_heatmap(df_COSI2_sym_clean_filtered, "Q3", "segment_age_Q3")

ggsave(filename = paste0(mydir_figs, 
                         "gg_heat_HL_Q1", 
                         ".pdf"),
       plot = gg_heat_HL_Q1, 
       width = 18, height = 12)

ggsave(filename = paste0(mydir_figs, 
                         "gg_heat_HL_Q2", 
                         ".pdf"),
       plot = gg_heat_HL_Q2, 
       width = 18, height = 12)

ggsave(filename = paste0(mydir_figs, 
                         "gg_heat_HL_Q3", 
                         ".pdf"),
       plot = gg_heat_HL_Q3, 
       width = 18, height = 12)

