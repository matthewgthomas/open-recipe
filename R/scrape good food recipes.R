library(tidyverse)
library(rvest)

urls <- read_csv("data/input/recipes.csv", col_names = c("url", "dish"))

bbc_urls <- urls |> 
  filter(str_detect(url, "bbcgood")) |> 
  filter(!str_detect(url, "collection"))

# Create empty tibbles to hold the recipes
recipes <- tibble(
  id = integer(),
  name = character(),
  prep_time = character(),
  cook_time = character(),
  serves = character(),
  difficulty = character()
)

ingredients <- tibble()
method <- tibble()
# i <- 1
start_recipe <- i

for (i in start_recipe:nrow(bbc_urls)) {
  current_url <- bbc_urls$url[i]
  
  # recipe <- read_html(current_url)
  recipe <- possibly(read_html, NA_character_)(current_url)
  
  if (is.na(recipe)) {
    print(paste0("Skipping #", i, ": ", current_url))
    next
  }
  
  # Name of recipe
  current_recipe_name <- 
    recipe |> 
    html_element(css = "h1") |> 
    html_text()
  
  # Ingredients
  current_ingredients <- 
    recipe |> 
    html_elements(xpath = "//section[contains(@class, 'recipe__ingredients')]/section/ul/li") |> 
    html_text() |> 
    as_tibble() |> 
    rename(ingredient = value) |> 
    mutate(id = i) |> 
    relocate(id)
  
  # Method
  current_method <- 
    recipe |> 
    html_elements(xpath = "//section[contains(@class, 'recipe__method')]/div/ul/li") |> 
    html_text() |> 
    as_tibble() |> 
    rename(step = value) |> 
    mutate(id = i) |> 
    relocate(id)
  
  # Prep and cook time
  current_timings <- 
    recipe |> 
    html_elements(xpath = "//ul[contains(@class, 'recipe__cook-and-prep')]/li/div/div/ul/li") |> 
    html_text()
  
  # Difficulty and serving size
  current_stats <- 
    recipe |> 
    html_elements(xpath = "//div[contains(@class, 'icon-with-text__children')]") |> 
    html_text()
  
  # Save progress
  recipes <- recipes |> add_row(id = i, name = current_recipe_name, prep_time = current_timings[1], cook_time = current_timings[2], serves = current_stats[3], difficulty = current_stats[2])
  ingredients <- bind_rows(ingredients, current_ingredients)
  method <- bind_rows(method, current_method)
  
  if (i %% 100 == 0) {
    write_rds(recipes, "data/output/recipes.rds")
    write_rds(ingredients, "data/output/ingredients.rds")
    write_rds(method, "data/output/methods.rds")
    
    write_csv(recipes, "data/output/recipes.csv")
    write_csv(ingredients, "data/output/ingredients.csv")
    write_csv(method, "data/output/methods.csv")
  }
  
  print(paste0("Finished recipe ", i, " of ", nrow(bbc_urls)))
  Sys.sleep(0.8)
}

ingredients <- 
  ingredients |> 
  rename(recipe_id = id) |> 
  mutate(ingredient_id = row_number()) |> 
  relocate(ingredient_id) |> 
  mutate(
    ingredient = str_trim(ingredient) |> 
      str_remove("\n")
  )

method <- 
  method |> 
  rename(recipe_id = id) |> 
  mutate(step = str_remove(step, "(STEP [0-9])")) |> 
  
  group_by(recipe_id) |> 
  mutate(step_id = row_number()) |> 
  ungroup() |> 
  relocate(step_id)

# Save everything
write_rds(recipes, "data/output/recipes.rds")
write_rds(ingredients, "data/output/ingredients.rds")
write_rds(method, "data/output/methods.rds")

write_csv(recipes, "data/output/recipes.csv")
write_csv(ingredients, "data/output/ingredients.csv")
write_csv(method, "data/output/methods.csv")