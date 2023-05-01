require(gdata)
require(corrplot)
require(ggcorrplot)
require(dplyr)
require(likert)
require(plyr)

# raw data
rawData <- read.csv("DMI_DataJam_Final.csv")
# description of each column
descriptor <- read.csv("description.csv")
# Survey data with Q1, Q2, Q3 as column name
survey <- rawData
# Survey data to add actual question in column
survey_with_question <- rawData

# fill in survey column name with survey questions for subsequent use
for (i in 1:ncol(survey_with_question)) {
  name <- colnames(survey_with_question)[i]
  if (startsWith(colnames(survey_with_question)[i], "Q")) {
    colnames(survey_with_question)[i] <-
      descriptor$Description[descriptor$Variable == name]
  }
}

#-------------------------------------------------------------------------------
# 3-9 have 5 level response; q11 suite has 6 levels; use them as an example
# Correlation matrix of items
cormat1 <- survey[, 13:19] %>%
  cor(., use = "pairwise.complete.obs")
cormat2 <- survey[, 22:35] %>%
  cor(., use = "pairwise.complete.obs")

# Correlation matrix plot
corrplot(cormat1, # correlation matrix
         order = "hclust", # hierarchical clustering of correlations
         addrect = 2) # number of rectangles to draw around clusters
ggcorrplot(cormat1,
           # correlation matrix
           type = "lower",
           # print the lower part of the correlation matrix
           hc.order = TRUE,
           # hierarchical clustering of correlations
           lab = TRUE) # add correlation values as labels

corrplot(cormat2, # correlation matrix
         order = "hclust", # hierarchical clustering of correlations
         addrect = 2) # number of rectangles to draw around clusters
ggcorrplot(cormat2,
           # correlation matrix
           type = "lower",
           # print the lower part of the correlation matrix
           hc.order = TRUE,
           # hierarchical clustering of correlations
           lab = TRUE) # add correlation values as labels

#-------------------------------------------------------------------------------
# Q11 analysis
# A custom function to recode numerical responses into ordered factors
Q11_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(
                x == 1,
                "Extremely difficult",
                ifelse(
                  x == 2,
                  "Somewhat difficult",
                  ifelse(
                    x == 3,
                    "Neither difficult or easy",
                    ifelse(x == 4, "Somewhat easy",
                           ifelse(
                             x == 5, "Extremely easy",
                             ifelse(x == 6, "Unsure or N/A", "else")
                           ))
                  )
                )
              ))
  
  y <-
    factor(
      y,
      levels = c(
        "Unsure or N/A",
        "Extremely easy",
        "Somewhat easy",
        "Neither difficult or easy",
        "Somewhat difficult",
        "Extremely difficult"
      )
    )
  
  return(y)
}

q11_data <-
  survey_with_question[, 21:34] %>% mutate_all(Q11_recode) %>% likert()
# Create a stacked bar chart
plot(
  q11_data,
  # Plot the percentages for each response category
  plot.percents = TRUE,
  # Plot the total percentage for negative responses
  plot.percent.low = FALSE,
  # Plot the total percentage for positive responses
  plot.percent.high = FALSE,
  # Whether response categories should be centered
  # This is only helpful when there is a middle response
  # option such as "neutral" or "neither agree nor disagree"
  centered = TRUE
)
#-------------------------------------------------------------------------------
library("reshape2")
library("ggalluvial")
# I noticed Q18 and CountryRecipient can be independent variables
# A function to recode accountable lifecycle into ordered factors
accountable_lifecycle_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(
                x == 6,
                "Data Provisioning",
                ifelse(
                  x == 11,
                  "Data Management",
                  ifelse(
                    x == 12,
                    "Data Subject Matter Expertise",
                    ifelse(x == 13, "Data Analysis",
                           ifelse(x == 14, "Data Consumption", ""))
                  )
                )
              ))
  
  y <-
    factor(
      y,
      levels = c(
        "Data Provisioning",
        "Data Management",
        "Data Subject Matter Expertise",
        "Data Consumption"
      )
    )
  
  return(y)
}
# A function to code CountryRecipient as US and non-US
country_recode <- function(x) {
  y <- ifelse(x == "United States", "US", "Non-US")
  y <- factor(y, levels = c("US", "Non-US"))
}
survey$Q18 <- accountable_lifecycle_recode(survey$Q18)
survey$RecipientCountry <- country_recode(survey$RecipientCountry)

# compare factors against survey responses, use Q4 as an example:
Q4 <- survey %>% select(RecipientCountry, Q18, Q4)
Q4 <- subset(Q4,!is.na(Q18))
Q4 <- melt(Q4,
           id.vars = c("RecipientCountry", "Q18"),
           value.name = "Q4")
# recode Q4
Q4_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(
                x == 1,
                "Strongly disagree",
                ifelse(
                  x == 2,
                  "Somewhat disagree",
                  ifelse(
                    x == 3,
                    "Neither agree nor disagree",
                    ifelse(x == 4, "Somewhat agree",
                           ifelse(x == 5, "Strongly agree", ""))
                  )
                )
              ))
  
  y <-
    factor(
      y,
      levels = c(
        "Strongly disagree",
        "Somewhat disagree",
        "Neither agree nor disagree",
        "Somewhat agree",
        "Strongly agree"
      )
    )
  
  return(y)
}

Q4$Q4 <- Q4_recode(Q4$Q4)
Q4_summary <-
  Q4 %>% group_by(RecipientCountry, Q18, Q4) %>% dplyr::summarise(Freq = n())
# Create a alluvium plot to show the proportion of observations that are shared across different categorical fields.
ggplot(Q4_summary,
       aes(
         y = Freq,
         axis1 = RecipientCountry,
         axis2 = Q18,
         axis3 = Q4
       )) +
  geom_alluvium(aes(fill = RecipientCountry), width = 1 / 12) +
  geom_stratum(width = 1 / 12,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("RecipientCountry", "Q18", "Q4"),
                   expand = c(.1, .1)) +
  scale_fill_brewer(type = "qual", palette = "Set1") 
