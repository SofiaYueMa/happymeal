#' Happy meal: Nutrients API Search
#'
#' Consumers with health or diet conditions need an easy way to get the recipe of foods they want to eat during the day,
#' and the nutrient information of foods in their daily life.
#' The function allows to search for nutrient information of specified foods within several seconds,
#' and bring health and happiness to your daily meal.
#'
#' The function allows to search for nutrient information of specified foods. It asks the user to insert specified queries,
#' including app id, app key, ingredients, etc.
#' Then the user can get data frame of the nutrient information matching the conditions.
#'
#' @param app_id_nutrient Application ID for nutrient API.
#' @param app_key_nutrient Nutrient API key.
#' @param ingr The ingredient that the user wants for the nutrient information. URL-encode should be included this string.
#' @keywords nutrient, meal
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' app_key_nutrient = Sys.getenv("nutrient_api_key")
#' nutrient_search(app_id_nutrient = "a81f90d1",
#' app_key_nutrient = app_key_nutrient, ingr = "large%20apple")


nutrient_search <- function(app_id_nutrient, app_key_nutrient, ingr) {

  nutrients <- "https://api.edamam.com/api/nutrition-data"
  r1 <- GET(nutrients, query = list(app_id = app_id_nutrient, app_key = app_key_nutrient, ingr = ingr))
  if (http_error(r1)){
    warning("The request produced an error. Please check your app_id or app_key.")
  } else {
    cont1 <- r1$content
    char1 <- rawToChar(r1$content)
    df1 <- fromJSON(char1, simplifyDataFrame = TRUE)
    df1 <- as.data.frame(unlist(df1))
    df1 <- cbind(rownames(df1), data.frame(df1, row.names=NULL))
    names(df1) <- c("label name", "value")
    print(df1)
  }
}


