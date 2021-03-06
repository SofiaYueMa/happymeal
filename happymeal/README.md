
happymeal
=========

<img src="https://images.agoramedia.com/everydayhealth/gcms/cs-Simple-Tips-for-Your-Diabetes-Diet-1440x810.jpg" width="100%" />

Leading a fast pace life, do you have enough time to think about your meals everyday?

The `happymeal` package makes your meals healthy and easy:

-   You can search for recipes matching the specified query within seconds.

-   It can provide you complete nutrient information of specified foods, and make you an expert of dietitian.

-   It can give data frame of food database matching the conditions.

-   You can know more about what you eat with the NPC or barcode and a simple click.

The links of the package on Github site is <https://sofiayuema.github.io/happymeal/>

Installation
------------

You can install the released version of happymeal from [Github](https://github.com/) with:

``` r
devtools::install_github("SofiaYueMa/happymeal/happymeal")
```

The `happymeal` package contains four functions: `recipes_search()`, `nutrient_search()`, `food_database_search()`, and `upc_barcode_search()`.

`recipes_search` Function
-------------------------

The `recipes_search` function allows to search for recipes matching the specified query.

Here is an example. Imagine that you want a balanced diet with ingredients of pork, and with calories witin 100 kcal to 1000 kcal.You do not like onion, so you want exclude onion in your dinner. Then with your application id and API key, you can get all the recipes that meets your requirement with a simple click.

    library(happymeal)
    app_key_recipe = Sys.getenv("recipe_api_key")
    recipes_search(q = "pork", app_id_recipe = "46707447", app_key_recipe = app_key_recipe, diet = "balanced", calories = "100-1000", excluded = "onion")

<img src="https://i.postimg.cc/pVQ2hQ8H/recipes-research.png" width="100%" />

If you insert your application id or API key with mistake, the users is remined of the mistake.

    The request produced an error. Please check your app_id or app_key.

`nutrient_search` Function
--------------------------

The `nutrient_search` function allows to search for nutrient information of specified foods within several seconds.

Here is an example. Imagine that you want to know nutrient information of large apple. Then with your application id and API key, you can get the detailed information with a simple click. Note that you need to use URL-encode in the arguments of ingredients.

    app_key_nutrient = Sys.getenv("nutrient_api_key")
    nutrient_search(app_id_nutrient = "a81f90d1", app_key_nutrient = app_key_nutrient, ingr = "large%20apple")

<img src="https://i.postimg.cc/1Xy1cT6Z/nutrient-serach.png" width="100%" />

If you insert your application id or API key with mistake, the users is remined of the mistake.

    The request produced an error. Please check your app_id or app_key.

`food_database_search` Function
-------------------------------

The `food_database_search` function allows to search for food database matching the specified queries.

Here is an example. Imagine that you want to know detailed information of large apple. Then with your application id and API key, you can get the detailed information with a simple click. Note that you need to use URL-encode in the arguments of ingredients.

    app_key_food = Sys.getenv("food_api_key")
    food_database_search(app_id = "bd9e5519", app_key = app_key_food, ingr = "large%20apple")

<img src="https://i.postimg.cc/Wp7vbNLp/food-database-search.png" width="100%" />

If you insert your application id or API key with mistake, the users is remined of the mistake.

    The request produced an error. Please check your app_id or app_key.

`upc_barcode_search` Function
-----------------------------

The `upc_barcode_search` function allows to search of an item based UPC/Barcode number.

Here is an example. Imagine that you want to know detailed information of the item you have. Then with your application id and API key, you can get the detailed information with a simple click. Note that the upc should be 12-unit number, you can find it on your food pacakge.

    app_key_food = Sys.getenv("food_api_key")
    upc_barcode_search(upc = "041250102410", app_id_food = "bd9e5519", app_key_food = app_key_food)

<img src="https://i.postimg.cc/1tcLtHd7/npc-barcode-search.png" width="100%" />

If you insert your application id or API key with mistake, the users is remined of the mistake.

    The request produced an error. Please check your app_id or app_key.
