# dictionary grouping food system commodities into the categories, using FAOs commodity list: http://www.fao.org/waicent/faoinfo/economic/faodef/annexe.htm 
# cereals & cereal products, fruits and derived products, vegetables and derived products, sugar crops and sweeteners and derived products, fibres of vegetal and animal origin, composite foods, 
# seeds for planting, non-food tree crops and derived products, products from live animals, oil-bearing crops and derived products, products from slaughtered animals, livestock, 
# stimulant crops and derived products, beverages, roots and tubers and derived products, pulses and derived products, fodder crops and derived products, nuts and derived products, 
# beverage crops and spices, vegetal and animal oils and fats, hides and skins, other commodities. * indicate that commodity group is not part of the FAO commodity list.



comm_class2 <- list()
comm_class2[["cereals & cereal products"]] <- c("staple grain", "two-season rice", "one-season rice", "rice", "maize", "wheat", "sorghum", "millet", "grain", "coarse grains", "barley", "bread", "maize flour", "oats", "other cereals", "pasta", 
                                               "refined grains", "teff", "whole grains", "cereals", "maize hybrid/improved", "maize local", "processed rice", "hybrid rice", "foodgrain",
                                               "spring maize", "summer maize", "winter wheat", "flour", "genetically modified maize")

comm_class2[["composite crops*"]] <- c("staple crop", "perennial crop", "annual crop", "crops","trees", "other crops", "cash crops", "horticultural crops", "industrial crops", "non-food crops", 
                                      "other starchy staples", "export crops", "food crops", "agroforestry", "upland crop", "arable crops", "orchard")

comm_class2[["compisite foods*"]] <- c("food products", "other processed food", "food", "other food", "food at home", "food away from home", "processed food", "staple food",
                                      "healthy food", "unhealthy food", "food of plant origin", "own-produced food", "livestock products", "other animal products", "oil")

comm_class2[["livestock and animal sourced foods**"]] <- c("bovine meat", "animal products", "pig", "cattle", "goat", "poultry", "sheep", "livestock", "broiler chicken", "buffalo", "camel", "chicken", "cow", "other livestock", "oxen",
                                                          "crossbred cattle", "laying hen", "male calf", "beef cattle", "dairy cattle", "duck", "goose", "slaughter pig", "sow",
                                                          "lamb", "turkey", "ewes", "feeder cattle", "suckler-cattle", "indigenous chicken", #livestock
                                                          
                                                          "milk", "dairy", "egg", "dairy products", "milk fat", "non-fat milk solid", "raw milk", "yoghurt", #products from live animals
                                                          
                                                          "poulty meat", "goat meat", "meat", "fish", "beef", "other meat", "cattle meat", "shrimp", "pork", "sheep meat") #products from slauthered animals

comm_class2[["vegetables and derived products"]] <- c("fresh vegetable", "perishable vegetable", "vegetable", "onion", "tomato", "broccoli", "cabbage", "garlic")

comm_class2[["oil crops and vegetable oils and fats**"]] <- c("soybean oil", "sunflower", "groundnut", "soybean", "coconut", "jatropha", "linseed", "oil palm", "oil seeds", "oilseed", "palm oil", "rape seed", "sesame", 
                                                           "peanut", "biodiesel", "biomass fuel", "sunflower seed", "cooking oil", #oil-bearing crops and derived products
                                                           
                                                           "edible oil", "vegetable oil", "vegetable fat" #vegetable oils and fats
)

comm_class2[["fruits and derived products"]] <- c("fresh fruit", "cherry", "perishable fruit", "durian", "fruit", "plantain", "avocado", "banana", "mango")


comm_class2[["stimulant crops, sugar crops, derived products, and additives**"]] <- c("raw sugar", "sugar beet", "sugar", "sugarcane", "candy", #sugar crops and sweeteners, and derived products
                                                                                   
                                                                                   "spice", "tobacco", "coffee", "cocoa", "chat", "cocoa beans", "tea", #stimulant crops and derived products
                                                                                   
                                                                                   "beverages", "alcoholic beverages", "carbonated soft drinks", #beverages
                                                                                   
                                                                                   "hop", "malt barley", #beverage crops and spices
                                                                                   
                                                                                   "salt")

comm_class2[["pulses, nuts, and derived products**"]] <- c("cowpea", "pulses", "beans", "chickpea", "dry beans", "faba bean", "haricot bean", "legumes", "lentil", "vetch", #pulses and derived products
                                                          
                                                          "cashews", "nuts") #nuts and derived products

comm_class2[["fodder crops and derived products"]] <- c("fodder", "animal feed", "feedgrain", "fodder crop", "fonio", "grass", "napier grass", "feed", "alfalfa")

comm_class2[["non-food crops and derived products**"]] <- c("cotton", "plant-based fibers", "raw cotton", #fibres of vegetal origin
                                                         "wood", "eucalyptus", "wood products", #non-food tree crops, and derived products*
                                                         "leather" #hides and skins
  
)

#comm_class2[["sugar crops and sweeteners, and derived products"]] <- c("sugar", "sugarcane", "candy")

# comm_class2[["fibres of vegetal and animal origin"]] <- c("cotton", "plant-based fibers", "raw cotton")

comm_class2[["seeds for planting*"]] <- c("hybrid/improved maize seed", "seeds", "local maize seed", "seedgrain")

# comm_class2[["non-food tree crops, and derived products*"]] <- c("wood", "eucalyptus", "wood products")

# comm_class2[["products from live animals"]] <- c("milk", "dairy", "egg", "dairy products", "milk fat", "non-fat milk solid", "raw milk", "yoghurt")

# comm_class2[["oil-bearing crops and derived products"]] <- c("soybean oil", "sunflower", "groundnut", "soybean", "coconut", "jatropha", "linseed", "oil palm", "oil seeds", "oilseed", "palm oil", "rape seed", "sesame", 
#                                                             "peanut", "biodiesel", "biomass fuel", "sunflower seed", "cooking oil")
# comm_class2[["products from slaughtered animals"]] <- c("poulty meat", "goat meat", "meat", "fish", "beef", "other meat", "cattle meat", "shrimp", "pork", "sheep meat")

# comm_class2[["livestock"]] <- c("cattle", "goat", "poultry", "sheep", "livestock", "broiler chicken", "buffalo", "camel", "chicken", "cow", "other livestock", "oxen",
#                                "crossbred cattle", "laying hen", "male calf", "beef cattle", "dairy cattle", "duck", "goose", "slaughter pig", "sow",
#                                "turkey", "ewes", "feeder cattle", "suckler-cattle", "indigenous chicken")

# comm_class2[["stimulant crops and derived products"]] <- c("spice", "tobacco", "coffee", "cocoa", "chat", "cocoa beans", "tea")

# comm_class2[["beverages"]] <- c("beverages", "alcoholic beverages", "carbonated soft drinks")

comm_class2[["roots and tubers, and derived products"]] <- c("cassava", "potato", "roots", "sweet potato", "tubers", "yam", "cocoyam", "enset")

# comm_class2[["pulses and derived products"]] <- c("cowpea", "pulses", "beans", "chickpea", "dry beans", "faba bean", "haricot bean", "legumes", "lentil", "vetch")


# comm_class2[["nuts and derived products"]] <- c("cashews", "nuts")

# comm_class2[["beverage crops and spices"]] <- c("hop")

# comm_class2[["vegetable and animal oils and fats"]] <- c("edible oil", "vegetable oil", "vegetable fat")

# comm_class2[["hides and skins"]] <- c("leather")

# comm_class22[["other commodities*"]] <- c("enset") #, "salt")