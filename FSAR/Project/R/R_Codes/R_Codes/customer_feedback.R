# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("customer_feedback.csv")

#data dictionary
data_dictionary <- read_csv("customer_feedback.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

#observation
feedback %>%
  group_by(rating) %>%
  summarise(N = n())

difference_created_answered <- feedback %>%
  mutate(reponse_time = round(as.numeric(difftime(feedback_answered_timestamp, feedback_created_timestamp, units = "days")), 1 ))

#hisogramm of response time
ggplot(data = difference_created_answered, aes(x = reponse_time)) +
  geom_histogram(binwidth = 5, fill = "dodgerblue4", color = "red")+
  # Set margins for x and y-axis titles and plot title
  theme(axis.title.x = element_text(margin = margin(t = 10, b = 10, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, b = 0, l = 10)),
        plot.title   = element_text(margin = margin(t = 20, b = 15, l = 0)))

