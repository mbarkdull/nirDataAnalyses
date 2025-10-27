library(tidyverse)

# Read in the data from Yume's Google sheet, filtering any specimens that do not have lat/long and removing some extraneous columns:
rawNir <- read_sheet("https://docs.google.com/spreadsheets/d/1dHIhVuh-Sy2clvqRpMiu5mDXpaXXp3W5HgLbzs2rYNo/edit?usp=sharing") %>%
  filter(Latitude != "NULL",
         Longitude != "NULL") %>%
  select(-c("Order",
            "Superfamily",
            "Family",
            "Subfamily",
            "Subspecies",                  
            "Author (including year)",
            "Zoogeography",
            "Previous owner numbers",
            "Other collection numbers",
            "Data privacy [private/public]",
            "Data entered by"))

# Count how many forward slashes there are in the date column.
rawNir$datePrecision <- str_count(rawNir$`Start Date (Day, Month, Year)`,
                                  pattern = "\\/")
# Zero would mean we have a year only
# One would mean we have a month and a year
# Two would mena we have a day, month, and year

# Based on that info, extract the date:
rawNir <- rawNir %>%
  mutate(year = case_when(datePrecision == 0 ~ `Start Date (Day, Month, Year)`,
                          datePrecision == 1 ~ `Start Date (Day, Month, Year)` %>% str_split_i(pattern = "\\/", i = 2),
                          datePrecision == 2 ~ `Start Date (Day, Month, Year)` %>% str_split_i(pattern = "\\/", i = 3),
                          TRUE ~ "Nonstandard date format"))

# Filter out any rows where the year column does not contain a four-digit number; these are mistakes that should be checked and corrected.
rawNirFormattedYears <- rawNir %>%
  filter(grepl(pattern = "^[0-9]{4}$", x = rawNir$year, perl = TRUE))

# Export that dataframe as a csv:
write_csv(x = rawNirFormattedYears,
          file = "rawNirFormattedYears.csv")