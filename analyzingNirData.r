install.packages("googlesheets4")
library(googlesheets4)
install.packages("tidyverse")
library(tidyverse)

Rawnir <- read_sheet("https://docs.google.com/spreadsheets/d/1dHIhVuh-Sy2clvqRpMiu5mDXpaXXp3W5HgLbzs2rYNo/edit?usp=sharing") %>%
  filter(Latitude != "NULL", 
         Longitude != "NULL")

ggplot(data = Rawnir, 
       mapping = aes(x = Latitude, 
                     y = `Average IR`)) + 
  geom_point() + 
  geom_smooth() 