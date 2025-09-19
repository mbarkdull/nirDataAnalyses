library(googlesheets4)
library(tidyverse)

rawNir <- read_sheet("https://docs.google.com/spreadsheets/d/1dHIhVuh-Sy2clvqRpMiu5mDXpaXXp3W5HgLbzs2rYNo/edit?usp=sharing") %>%
  filter(Latitude != "NULL", 
         Longitude != "NULL")

ggplot(data = rawNir, 
       mapping = aes(x = Latitude, 
                     y = `Average IR`)) + 
  geom_point() + 
  geom_smooth() 

