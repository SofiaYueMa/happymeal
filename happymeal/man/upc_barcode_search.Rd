% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upc_barcode_search.R
\name{upc_barcode_search}
\alias{upc_barcode_search}
\title{Happy meal: UPC or Barcode Search}
\usage{
upc_barcode_search(upc, app_id_food, app_key_food)
}
\arguments{
\item{upc}{The UPC or barcode number for the food. It is usually a 12-units number.}

\item{app_id_food}{Application ID for food database API.}

\item{app_key_food}{Food database API key.}
}
\description{
Consumers with health or diet conditions need an easy way to get the recipe of foods they want to eat during the day,
and the nutrient information of foods in their daily life. The function allows for search of an item based UPC/Barcode number,
and let you know more about what you eat.
}
\details{
This is a service allowing you to submit a UPC or barcode and find a match for it in the food database.
Then the user can get data frame of the detailed information matching the conditions.
}
\examples{
app_key_food = Sys.getenv("food_api_key")
upc_barcode_search(upc = "041250102410", app_id_food = "bd9e5519", app_key_food = app_key_food)
}
\keyword{barcode,}
\keyword{meal}
\keyword{upc,}
