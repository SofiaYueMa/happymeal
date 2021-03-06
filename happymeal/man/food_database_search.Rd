% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/food_database_search.R
\name{food_database_search}
\alias{food_database_search}
\title{Happy meal: Food Database API Search}
\usage{
food_database_search(app_id_food, app_key_food, ingr)
}
\arguments{
\item{app_id_food}{Application ID for food database API.}

\item{app_key_food}{Food database API key.}

\item{ingr}{The ingredient that the user wants for the food database. URL-encode should be included this string.}
}
\description{
Consumers with health or diet conditions need an easy way to get the recipe of foods they want to eat during the day,
and the nutrient information of foods in their daily life. The function allows to search for food database matching the specified query,
and bring health and happiness to your daily meal.
}
\details{
The function provides you with tools to find nutrition and diet data for generic foods, packaged foods,
and restaurant meals matching the specified queries, including app id, app key, ingredients, etc.
Then the user can get data frame of food database matching the conditions.
}
\examples{
app_key_food = Sys.getenv("food_api_key")
food_database_search(app_id = "bd9e5519", app_key_food = app_key_food, ingr = "large\%20apple")
}
\keyword{food,}
\keyword{meal}
