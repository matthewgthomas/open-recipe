library(tidyverse)
library(rvest)

base_url <- "https://www.bbc.co.uk/food/ingredients/a-z/"

ingredient_urls <- paste0(base_url, c(letters, "0-9"))

# Create empty tibble to hold the ingredients
ingredients <- c()

fetch_page <- function(url) {
  possibly(read_html, NA_character_)(url)
}

scrape_ingredient <- function(page) {
  # Names of ingredients on the page
  page |> 
    html_elements(css = "h3") |> 
    html_text()
}

for (i in 1:length(ingredient_urls)) {
  current_url <- ingredient_urls[i]
  
  ingredients_page <- fetch_page(current_url)
  
  if (is.na(ingredients_page)) {
    print(paste0("Skipping ", current_url))
    next
  }
  
  current_ingredients <- scrape_ingredient(ingredients_page)
  
  # Save progress
  ingredients <- c(ingredients, current_ingredients)
  
  # Try to scrape ingredients from subsequent pages
  num_pages <- 
    ingredients_page |> 
    html_elements(xpath = "//div[contains(@class, 'pagination')]/ul/li") |> 
    html_text() |> 
    as.integer() |> 
    max(na.rm = TRUE)
  
  if (!is.infinite(num_pages)) {
    for (j in 2:num_pages) {
      current_url_subpage <- paste0(current_url, "/", j)
      
      ingredients_subpage <- fetch_page(current_url_subpage)
      
      if (is.na(ingredients_subpage)) {
        print(paste0("Skipping ", current_url_subpage))
        next
      }
      
      # Ingredients on this page
      current_ingredients <- 
        ingredients_subpage |> 
        html_elements(css = "h3") |> 
        html_text()
      
      # Save progress
      ingredients <- c(ingredients, current_ingredients)
    }
  }
  
  print(paste0("Finished ", current_url))
  Sys.sleep(0.8)
}

# Save
ingredients |> 
  as_tibble_col(column_name = "ingredient") |> 
  write_csv("data/output/ingredients-list.csv")
