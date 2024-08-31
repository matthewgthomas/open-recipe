library(tidyverse)
library(rollama)
library(jsonlite)

# recipes <- read_rds("data/output/recipes.rds")
ingredients <- read_rds("data/output/ingredients.rds")

# Save a random sample of ingredients
ingredients |> 
  select(ingredient) |> 
  slice_sample(prop = 0.01) |> 
  write_csv("data/output/ingredients-sample.csv")

rollama::list_models()

options(rollama_model = "llama3.1:latest")

# set up the task + content for LLM
prompt <- "Extract data about ingredients from this text from a recipe.
           The text will contain the name of one ingredient and its amount. The text might also include information about how to prepare the ingredient.
           The output must include: the name of the ingredient, the amount to use, and how to prepare it (if included in the text).

        Output must be in JSON Format, follow this example below:
          
        [
          {\"ingredient_name\": \"carrot\", \"amount\": \"100g\", \"instructions\": \"grated\"}
        ]
          
        response start with \"[\" and end with \"]\", after that, no words or symbols.
        the output must always contain 1 JSON object, no more and no less.
          
        Here is the ingredient text:"

processed_ingredients <- tibble(
  ingredient_id = integer(),
  ingredient_text = character(),
  ingredient_name = character(),
  amount = character(), 
  instructions = character()
)

processed_ingredient_blank <- tibble(
  ingredient_name = NA_character_,
  amount = NA_character_, 
  instructions = NA_character_
)

start_id <- i + 1
# end_id <- 16056
end_id <- nrow(ingredients)

for (i in start_id:end_id) {
  content <- ingredients$ingredient[i]
  
  # Merge the task and content for LLM
  q <- paste(prompt, content, sep = "\n")
  
  LLM_output <- query(q, screen = FALSE, model_params = list(temperature = 0.0, seed = 42))
  
  # content_json_string <- str_remove(LLM_output$message$content, "Here is the extracted data in JSON format:\n\n")
  content_json_string <- str_extract(LLM_output$message$content, regex("(\\[.*\\])", dotall = TRUE))
  content_json_string <- str_replace(content_json_string, "\" or \"", " or ")
  
  processed_ingredient <- 
    possibly(
      \(x) fromJSON(x, simplifyDataFrame = TRUE),
      processed_ingredient_blank
    )(content_json_string)
  
  processed_ingredient$ingredient_id <- i
  processed_ingredient$ingredient_text <- content
  processed_ingredients <- bind_rows(processed_ingredients, processed_ingredient)
  
  if (i %% 100 == 0) print(paste0("Finished #", i))
}

write_csv(processed_ingredients, str_glue("data/output/ingredients - processed - 1001-{max(processed_ingredients$ingredient_id)}.csv"))

current_recipe <- ingredients |> 
  filter(ingredient_id == max(processed_ingredients$ingredient_id)) |> 
  pull(recipe_id)

ingredients |> filter(recipe_id == current_recipe)



ingredients_subset <- 
  ingredients |> 
  slice_head(n = 1000) |> 
  mutate(ingredient_id = row_number()) |> 
  relocate(ingredient_id)

processed_ingredients <- 
  processed_ingredients |> 
  mutate(ingredient_id = row_number()) |> 
  relocate(ingredient_id) |> 
  
  mutate(ingredient_id = case_when(
    ingredient_id >= 332 & ingredient_id <= 334 ~ 332,
    ingredient_id >= 335 ~ ingredient_id - 2,
    .default = ingredient_id
  ))

ingredients_subset |> 
  left_join(processed_ingredients) |> 
  write_csv("data/output/ingredients - processed.csv")

processed_ingredients |> 
  count(ingredient_name, sort = TRUE)

processed_ingredients |> 
  count(instructions, sort = TRUE)

# Try extracting ingredients in batches
# batch_size <- 10
# batch_i <- start_id
# 
# while (batch_i < end_id) {
#   content_batch <- ingredients$ingredient[batch_i:(batch_i + batch_size)]
#   
#   # Merge the task and content for LLM
#   q <- paste(prompt, paste(content_batch, collapse = "\n"), sep = "\n")
#   
#   LLM_output <- query(q, screen = FALSE, model_params = list(temperature = 0.0, seed = 42))
#   
#   # content_json_string <- str_remove(LLM_output$message$content, "Here is the extracted data in JSON format:\n\n")
#   content_json_string <- str_extract(LLM_output$message$content, regex("(\\[.*\\])", dotall = TRUE))
#   content_json_string <- str_replace(content_json_string, "\" or \"", " or ")
#   
#   processed_ingredient <- 
#     possibly(
#       \(x) fromJSON(x, simplifyDataFrame = TRUE),
#       processed_ingredient_blank
#     )(content_json_string)
#   
# }
