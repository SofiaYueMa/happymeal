#' Happy meal: Recipe API Search
#'
#' Consumers with health or diet conditions need an easy way to get the recipe of foods they want to eat during the day,
#' and the nutrient information of foods in their daily life. The function allows to search for recipes
#' matching the specified query, and bring health and happiness to your daily meal.
#'
#' The function allows to search for recipes matching the specified query.
#' It asks the user to pick search for recipes, and insert specified queries, including app id, app key, diet, calories, etc.
#' Then the user can get data frame of the recipes matching the conditions.
#'
#' @param q The query text, what item you want the recipes focus on, for example, chicken.
#' @param app_id_recipe Application ID for recipe API.
#' @param app_key_recipe Recipe API key.
#' @param diet Diet label: one of “balanced”, “high-protein”, “high-fiber”, “low-fat”, “low-carb”, “low-sodium”.
#' @param calories Calories that each recipe serves, for example, “calories=100-300” will return
#' all recipes with which have between 100 and 300 kcal per serving.
#' @param excluded Excluding recipes with certain ingredients.  More than one food can be excluded at the same time.
#' Example: excluded=vinegar&excluded=pretzel，will exclude any recipes which contain vinegar or pretzels in their ingredient list.
#' @keywords recipe, meal
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' app_key_recipe = Sys.getenv("recipe_api_key")
#' recipes_search(q = "pork", app_id_recipe = "46707447",
#' app_key_recipe = app_key_recipe, diet = "balanced",
#' calories = "100-1000", excluded = "onion")

recipes_search <- function(q, app_id_recipe, app_key_recipe, diet, calories, excluded) {

  recipes <- "https://api.edamam.com/search"
  r <- GET(recipes, query = list(q = q, app_id = app_id_recipe, app_key = app_key_recipe,
                                 diet = diet, calories = calories, excluded = excluded))
  if (http_error(r)){
    warning("The request produced an error. Please check your app_id or app_key.")
  } else {
    cont <- r$content
    char <- rawToChar(r$content)
    df <- fromJSON(char, simplifyDataFrame = TRUE)
    recipe_results <- df$hits$recipe
    recipe_results <- recipe_results[ , c(2,4,5,9,12,13,16)]
    for(i in 1:length(recipe_results[,1])){
      if(is.list(recipe_results[i,4])){
        recipe_results[i,4]=paste(unlist(recipe_results[i,4]),collapse=";")
      }
    }
    for(i in 1:length(recipe_results[,1])){
      if(is.list(recipe_results[i,5])){
        recipe_results[i,5]=paste(unlist(recipe_results[i,5]),collapse=";")
      }
    }
    print(recipe_results)
  }
}
