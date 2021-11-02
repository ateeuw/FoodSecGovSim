# dictionary grouping target implementers of governance measures into the categories: 
# governmental entities, food producing entities, unclear, knowledge broker entities, trade entities, non-food manufacturing/processing entities, food consuming entities, citizen entities, 
# food retail entities, knowledge producing entities, banks/credit suppliers, & non-food retail entities

timpl_class <- list()

timpl_class[["governmental entities"]] <- c("government", "political union")

timpl_class[["food producing entities"]] <- c("herder", "food producing firm", "livestock raiser", "crop producers", "chicken flock", "livestock raising household", "sheep", "unskilled farm labourers", "farmland owner", "farm labourer", "part-time farm households", "poor farming households", "farms in central-upper income quantile", "farms in upper income quantile", "agricultural labourer", "poor farming households", "farms in central-lower income quantile", "farms in central-upper income quantile", "farms in upper income quantile", "farms in lower-income quantile", "rural farmer", "urban farmer", "farmer in other countries", "farmer", "farming household", "farm", "agricultural entity", "producer", "agro-pastoralist", "pastoralist", "farmer population", "producer household", "multinational food-products company")

timpl_class[["generic, overarching entities"]] <- c("country", "urban households", "individual", "poor urban households", "poor households", "poor urban households", "low-income household", "high-income household", "rural non-farmer", "urban non-farmer", "middle-income households", "rural people", "household", "urban household", "labourer", "investor", "foreign sector", "rural household")

timpl_class[["unclear"]] <- c("unclear")

timpl_class[["knowledge broker entities"]] <- c("agricultural extension services", "educators", "veterenary")

timpl_class[["trade entities"]] <- c("trade partnership", "farmer cooperative", "trader", "herder cooperation")

timpl_class[["non-food manufacturing/processing entities"]] <- c("pesticide industry", "industry", "agricultural machinery manufacturing industry", "fertiliser industry", "water supplier", "industrial entity")

timpl_class[["food distribution/storage entities"]] <- c("wholesaler", "storage facility", "food warehouse operators", "food storage operators", "food distribution operators", "storing agent", "distribution centre")

timpl_class[["food consuming entities"]] <- c("children consumers", "consumer population", "customer pool", "global consumer", "international consumer", "consumer in other countries", "tourist", "rural consumer", "consumer", "resident/consumer", "consumer household")

timpl_class[["citizen entities"]] <- c("resident", "unemployed", "homeless", "citizen", "volunteers", "women", "local community", "population", "residential entity", "villager", "taxpayer", "village", "volunteer")

timpl_class[["food retail entities"]] <- c("food vendor", "hotel", "retailer", "stores", "bakeries", "restaurants", "food vendor/store", "store", "restaurant", "shops")

timpl_class[["knowledge producing entities"]] <- c("weather forecaster", "researchers", "university researchers", "independant researchers", "agricultural researchers", "meteorological agency")

timpl_class[["banks/credit suppliers"]] <- c("credit suppliers", "credit supplier")

timpl_class[["non-food retail entities"]] <- c("input supplier", "water manager")

timpl_class[["generic or non-food labourers"]] <- c("unskilled nonfarm labourers", "construction workers", "unskilled labourer", "employer", "female labourer", "skilled labourers")

timpl_class[["non-agricultural land users"]] <- c("plantation forest owners", "land manager", "ecological entity")

timpl_class[["transport entities"]] <- c("transporter", "transporters", "public transport vehicle", "transportation vehicle")

timpl_class[["none"]] <- c("none")

timpl_class[["food processing entities"]] <- c("processor", "food industry", "food processor")

timpl_class[["non-food consumers"]] <- c("electricity users", "energy users")

timpl_class[["no one specific"]] <- c("no one specific")