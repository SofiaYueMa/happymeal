#' Happy meal: UPC or Barcode Search
#'
#' Consumers with health or diet conditions need an easy way to get the recipe of foods they want to eat during the day,
#' and the nutrient information of foods in their daily life. The function allows for search of an item based UPC/Barcode number,
#' and let you know more about what you eat.
#'
#' This is a service allowing you to submit a UPC or barcode and find a match for it in the food database.
#' Then the user can get data frame of the detailed information matching the conditions.
#'
#' @param app_id_food Application ID for food database API.
#' @param app_key_food Food database API key.
#' @param upc The UPC or barcode number for the food. It is usually a 12-units number.
#' @keywords upc, barcode, meal
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' app_key_food = Sys.getenv("food_api_key")
#' upc_barcode_search(upc = "041250102410", app_id_food = "bd9e5519", app_key_food = app_key_food)


upc_barcode_search <- function(upc, app_id_food, app_key_food) {
  upc_barcode <- "https://api.edamam.com/api/food-database/parser?"
  r3 <- GET(upc_barcode, query = list(upc = upc, app_id = app_id_food, app_key = app_key_food))
  if (http_error(r3)){
    warning("The request produced an error. Please check your app_id or app_key.")
  } else {
    cont3 <- r3$content
    char3 <- rawToChar(r3$content)
    df3 <- fromJSON(char3, simplifyDataFrame = TRUE)
    df3 <- as.data.frame(unlist(df3))
    df3 <- cbind(rownames(df3), data.frame(df3, row.names=NULL))
    names(df3) <- c("label name", "value")
    print(df3)
  }
}

