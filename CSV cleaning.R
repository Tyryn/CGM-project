library(tidyverse)

# Function to make first row column names
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

#--- Cleaning csv script

csv <- read.csv("C:/Users/DELL/Downloads/Tyryn Carnegie 20.09.2020 (1).csv", 
                header=FALSE, comment.char="#")

csv <- csv %>%
  # Keep only the rows with the sensor blood sugar data
  slice(str_which(V2, "Date")[1]:nrow(.)) %>%
  slice(2:nrow(.)) %>%
  slice(str_which(V2, "Date")[1]:nrow(.)) %>%
  # Keep only datetime and sensor blood sugar columns
  select(V2, V3, V32) %>%
  header.true(.)