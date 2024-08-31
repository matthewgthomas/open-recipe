library(tidyverse)
library(tidytext)
library(quanteda)

# Function to calculate co-occurrence stats from https://github.com/tm4ss/tm4ss.github.io/blob/master/calculateCoocStatistics.R
# See https://tm4ss.github.io/docs/Tutorial_5_Co-occurrence.html#1_Sentence_detection for uses
source("https://github.com/tm4ss/tm4ss.github.io/raw/master/calculateCoocStatistics.R")

ingredients <- read_rds("data/output/ingredients.rds")
ingredients_1_1000 <- read_csv("data/output/ingredients - processed.csv")
ingredients_1001_plus <- read_csv("data/output/ingredients - processed - 1001-16056.csv")

ingredients_recipes <- 
  ingredients |>
  select(ingredient_id, recipe_id)

ingredients <- 
  bind_rows(
    ingredients_1_1000 |> rename(recipe_id = id, ingredient_text = ingredient),
    ingredients_1001_plus |> left_join(ingredients_recipes)
  )

# How many recipes and ingredients?
max(ingredients$recipe_id)
max(ingredients$ingredient_id)

# How many unique ingredients?
ingredients |> 
  distinct(ingredient_name) |> 
  count()

# Number of ingredients per recipe
ingredients |> 
  count(recipe_id, name = "n_ingredients") |> 
  
  ggplot(aes(x = n_ingredients)) +
  geom_histogram(binwidth = 1)

ingredients |> 
  count(recipe_id, name = "n_ingredients") |> 
  summarise(
    mean = mean(n_ingredients),
    median = median(n_ingredients),
    sd = sd(n_ingredients),
    min = min(n_ingredients),
    max = max(n_ingredients)
  )

# What are the most common ingredients?
ingredients |> 
  count(ingredient_name, sort = TRUE)

ingredients |> 
  count(ingredient_name, sort = TRUE) |> 
  arrange(n)

# Most common prepartion methods?
ingredients |> 
  count(instructions, sort = TRUE)

# Zipf's law for ingredients
ingredients |> 
  count(ingredient_name, sort = TRUE) |> 
  mutate(
    term_frequency = n / sum(n),
    rank = rank(-n)
  ) |> 
  
  ggplot(aes(rank, term_frequency)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# Ingredient co-occurrence
ingredients_text <- 
  ingredients |> 
  group_by(recipe_id) |> 
  summarise(i = paste(ingredient_name, collapse = "; ")) |> 
  ungroup()

ingredients_corpus <- corpus(ingredients_text$i, docnames = ingredients_text$recipe_id)
ingredients_tokens <- tokens(ingredients_corpus, remove_punct = TRUE)

binDTM <- 
  ingredients_tokens |> 
  dfm() |> 
  dfm_weight("boolean")

ingredients_co <- t(binDTM) %*% binDTM

as.matrix(ingredients_co[202:205, 202:205])

coocs <- calculateCoocStatistics("eggs", binDTM, measure="LOGLIK")
# Display the numberOfCoocs main terms
print(coocs[1:100])
