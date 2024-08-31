library(tidyverse)
library(jsonlite)

ingredients <- read_rds("data/output/ingredients.rds")
ingredients_list <- read_csv("data/output/ingredients-list.csv")

ingredients_regex <- paste(tolower(str_glue("\\b{ingredients_list$ingredient}\\b")), collapse="|")

ingredients_subset <- 
  ingredients |> 
  slice_head(n = 1000) |> 
  mutate(ingredient_name = str_extract(tolower(ingredient), ingredients_regex))

sum(is.na(ingredients_subset$ingredient_name))
