library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")

# First checking the class
class(covid$date)
# Changing to date format
covid$date <- as_date(covid$date)
# Checking the class
class(covid$date)
# Now we can make numeric operations
range(covid$date)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

covid$new_confirmed[covid$new_confirmed < 0] <- 0
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases")

