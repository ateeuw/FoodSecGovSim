# dictionary grouping target implementers of governance measures into the categories: 
# governmental entities, food producing entities, unclear, knowledge broker entities, trade entities, non-food manufacturing/processing entities, food consuming entities, citizen entities, 
# food retail entities, knowledge producing entities, banks/credit suppliers, & non-food retail entities

timpl_class2 <- list()

timpl_class2[["no agents"]] <- c("none")

timpl_class2[["generic agents"]] <- c("country", "no one specific", "unskilled labourer", "employer", "female labourer", "skilled labourers",
                                     "resident", "unemployed", "homeless", "citizen", "volunteers", "women", "local community", "population", "residential entity", "villager", "taxpayer", "village", "volunteer",
                                     "urban households", "individual", "poor urban households", "poor households", "poor urban households", "low-income household", "high-income household", "rural non-farmer", "urban non-farmer", 
                                     "middle-income households", "rural people", "household", "urban household", "labourer", "investor", "foreign sector", "rural household")

timpl_class2[["food producers"]] <- c("herder", "food producing firm", "livestock raiser", "crop producers", "chicken flock", "livestock raising household", "sheep", "unskilled farm labourers", "farmland owner", "farm labourer", "part-time farm households", "poor farming households", "farms in central-upper income quantile", "farms in upper income quantile", "agricultural labourer", "poor farming households", "farms in central-lower income quantile", "farms in central-upper income quantile", "farms in upper income quantile", "farms in lower-income quantile", "rural farmer", "urban farmer", "farmer in other countries", "farmer", "farming household", "farm", "agricultural entity", "producer", "agro-pastoralist", "pastoralist", "farmer population", "producer household", "multinational food-products company")

timpl_class2[["food consumers"]] <- c("children consumers", "consumer population", "customer pool", "global consumer", "international consumer", "consumer in other countries", "tourist", "rural consumer", "consumer", "resident/consumer", "consumer household")

timpl_class2[["food retailers"]] <- c("food vendor", "hotel", "retailer", "stores", "bakeries", "restaurants", "food vendor/store", "store", "restaurant", "shops")

timpl_class2[["food storers and processors"]] <- c("processor", "food industry", "food processor", #processors
                                                   "wholesaler", "storage facility", "food warehouse operators", "food storage operators", "storing agent") #storage

timpl_class2[["food distributors transporters"]] <- c("transporter", "transporters", "public transport vehicle", "transportation vehicle", "food distribution operators", "distribution centre")

timpl_class2[["political entities"]] <- c("government", "political union", "trade partnership")

timpl_class2[["other non-food agents"]] <- c("electricity users", "energy users",
                                             "plantation forest owners", "land manager", "ecological entity",
                                             "unskilled nonfarm labourers", "construction workers", 
                                             "input supplier", "water manager",
                                             "credit suppliers", "credit supplier", 
                                             "weather forecaster", "researchers", "university researchers", "independant researchers", "agricultural researchers", "meteorological agency",
                                             "unclear", "agricultural extension services", "educators", "veterenary",
                                             "pesticide industry", "industry", "agricultural machinery manufacturing industry", "fertiliser industry", "water supplier", "industrial entity")

timpl_class2[["food traders"]] <- c("trader", "farmer cooperative", "herder cooperation")

# timpl_class2[["governmental entities"]] <- c("government", "political union")

#timpl_class2[["food producing entities"]] <- c("food producing firm", "livestock raiser", "crop producers", "chicken flock", "livestock raising household", "sheep", "unskilled farm labourers", "farmland owner", "farm labourer", "part-time farm households", "poor farming households", "farms in central-upper income quantile", "farms in upper income quantile", "agricultural labourer", "poor farming households", "farms in central-lower income quantile", "farms in central-upper income quantile", "farms in upper income quantile", "farms in lower-income quantile", "rural farmer", "urban farmer", "farmer in other countries", "farmer", "farming household", "farm", "agricultural entity", "producer", "agro-pastoralist", "pastoralist", "farmer population", "producer household", "multinational food-products company")

#timpl_class2[["generic, overarching entities"]] <- c("urban households", "individual", "poor urban households", "poor households", "poor urban households", "low-income household", "high-income household", "rural non-farmer", "urban non-farmer", "middle-income households", "rural people", "household", "urban household", "labourer", "investor", "foreign sector", "rural household")

# timpl_class2[["unclear"]] <- c("unclear")

# timpl_class2[["knowledge broker entities"]] <- c("agricultural extension services", "educators", "veterenary")

# timpl_class2[["trade entities"]] <- c("trade partnership", "farmer cooperative", "trader")

# timpl_class2[["non-food manufacturing/processing entities"]] <- c("pesticide industry", "industry", "agricultural machinery manufacturing industry", "fertiliser industry", "water supplier", "industrial entity")

# timpl_class2[["food distribution/storage entities"]] <- c("wholesaler", "storage facility", "food warehouse operators", "food storage operators", "food distribution operators", "storing agent", "distribution centre")

# timpl_class2[["food consuming entities"]] <- c("children consumers", "consumer population", "customer pool", "global consumer", "international consumer", "consumer in other countries", "tourist", "rural consumer", "consumer", "resident/consumer", "consumer household")

# timpl_class2[["citizen entities"]] <- c("resident", "unemployed", "homeless", "citizen", "volunteers", "women", "local community", "population", "residential entity", "villager", "taxpayer", "village", "volunteer")

# timpl_class2[["food retail entities"]] <- c("food vendor", "hotel", "retailer", "stores", "bakeries", "restaurants", "food vendor/store", "store", "restaurant", "shops")

# timpl_class2[["knowledge producing entities"]] <- c("researchers", "university researchers", "independant researchers", "agricultural researchers", "meteorological agency")

# timpl_class2[["banks/credit suppliers"]] <- c("credit suppliers", "credit supplier")

# timpl_class2[["non-food retail entities"]] <- c("input supplier", "water manager")

# timpl_class2[["generic or non-food labourers"]] <- c("unskilled nonfarm labourers", "construction workers", "unskilled labourer", "employer", "female labourer", "skilled labourers")

# timpl_class2[["non-agricultural land users"]] <- c("plantation forest owners", "land manager", "ecological entity")

# timpl_class2[["transport entities"]] <- c("transporter", "transporters", "public transport vehicle", "transportation vehicle")

#timpl_class2[["no agents"]] <- c("none")

# timpl_class2[["food processing entities"]] <- c("processor", "food industry", "food processor")

# timpl_class2[["non-food consumers"]] <- c("electricity users", "energy users")

# timpl_class2[["no one specific"]] <- c("no one specific")