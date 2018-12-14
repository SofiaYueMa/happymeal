#' Happy meal: Food Database API Search
#'
#' Consumers with health or diet conditions need an easy way to get the recipe of foods they want to eat during the day,
#' and the nutrient information of foods in their daily life. The function allows to search for food database matching the specified query,
#' and bring health and happiness to your daily meal.
#'
#' The function provides you with tools to find nutrition and diet data for generic foods, packaged foods,
#' and restaurant meals matching the specified queries, including app id, app key, ingredients, etc.
#' Then the user can get data frame of food database matching the conditions.
#'
#' @param app_id_food Application ID for food database API.
#' @param app_key_food Food database API key.
#' @param ingr The ingredient that the user wants for the food database. URL-encode should be included this string.
#' @keywords food, meal
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' app_key_food = Sys.getenv("food_api_key")
#' food_database_search(app_id = "bd9e5519", app_key_food = app_key_food, ingr = "large%20apple")

food_database_search <- function(app_id_food, app_key_food, ingr) {

  food_database <- "https://api.edamam.com/api/food-database/parser"
  r2 <- GET(food_database, query = list(app_id = app_id_food, app_key = app_key_food, ingr = ingr))
  if (http_error(r2)){
    warning("The request produced an error. Please check your app_id or app_key.")
  } else {
    cont2 <- r2$content
    char2 <- rawToChar(r2$content)
    df2 <- fromJSON(char2, simplifyDataFrame = TRUE)
    food_database_results <- df2$parsed
    food_database_results <- as.data.frame(unlist(food_database_results))
    food_database_results <- cbind(rownames(food_database_results), data.frame(food_database_results, row.names=NULL))
    names(food_database_results) <- c("label name", "value")
    print(food_database_results)
  }
}
