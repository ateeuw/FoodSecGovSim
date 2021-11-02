# dictionary grouping food system commodities into the categories, using FAOs commodity list: http://www.fao.org/waicent/faoinfo/economic/faodef/annexe.htm 
# cereals & cereal products, fruits and derived products, vegetables and derived products, sugar crops and sweeteners and derived products, fibres of vegetal and animal origin, composite foods, 
# seeds for planting, non-food tree crops and derived products, products from live animals, oil-bearing crops and derived products, products from slaughtered animals, livestock, 
# stimulant crops and derived products, beverages, roots and tubers and derived products, pulses and derived products, fodder crops and derived products, nuts and derived products, 
# beverage crops and spices, vegetal and animal oils and fats, hides and skins, other commodities. * indicate that commodity group is not part of the FAO commodity list.



comm_class <- list()
comm_class[["cereals & cereal products"]] <- c("staple grain", "two-season rice", "one-season rice", "rice", "maize", "wheat", "sorghum", "millet", "grain", "coarse grains", "barley", "bread", "maize flour", "oats", "other cereals", "pasta", 
                                                 "refined grains", "teff", "whole grains", "cereals", "maize hybrid/improved", "maize local", "processed rice", "hybrid rice", "foodgrain",
                                               "spring maize", "summer maize", "winter wheat", "flour", "genetically modified maize")
comm_class[["fruits and derived products"]] <- c("fresh fruit", "cherry", "perishable fruit", "durian", "fruit", "plantain", "avocado", "banana", "mango")
comm_class[["vegetables and derived products"]] <- c("fresh vegetable", "perishable vegetable", "vegetable", "onion", "tomato", "broccoli", "cabbage", "garlic")
comm_class[["sugar crops and sweeteners, and derived products"]] <- c("raw sugar", "sugar beet", "sugar", "sugarcane", "candy")
comm_class[["fibres of vegetal and animal origin"]] <- c("cotton", "plant-based fibers", "raw cotton")
comm_class[["compisite foods*"]] <- c("food products", "animal products", "other processed food", "food", "other food", "food at home", "food away from home", "processed food", "staple food",
                                          "healthy food", "unhealthy food", "food of plant origin", "own-produced food", "livestock products", "other animal products", "oil")
comm_class[["composite crops*"]] <- c("staple crop", "perennial crop", "annual crop", "crops","trees", "other crops", "cash crops", "horticultural crops", "industrial crops", "non-food crops", 
                                        "other starchy staples", "export crops", "food crops", "agroforestry", "upland crop", "arable crops", "orchard")
comm_class[["seeds for planting*"]] <- c("hybrid/improved maize seed", "seeds", "local maize seed", "seedgrain")
comm_class[["non-food tree crops, and derived products*"]] <- c("wood", "eucalyptus", "wood products")
comm_class[["products from live animals"]] <- c("milk", "dairy", "egg", "dairy products", "milk fat", "non-fat milk solid", "raw milk", "yoghurt")
comm_class[["oil-bearing crops and derived products"]] <- c("soybean oil", "sunflower", "groundnut", "soybean", "coconut", "jatropha", "linseed", "oil palm", "oil seeds", "oilseed", "palm oil", "rape seed", "sesame", 
                                                              "peanut", "biodiesel", "biomass fuel", "sunflower seed", "cooking oil")
comm_class[["products from slaughtered animals"]] <- c("bovine meat", "poulty meat", "goat meat", "meat", "fish", "beef", "other meat", "cattle meat", "shrimp", "pork", "sheep meat")
comm_class[["livestock"]] <- c("pig", "lamb", "cattle", "goat", "poultry", "sheep", "livestock", "broiler chicken", "buffalo", "camel", "chicken", "cow", "other livestock", "oxen",
                                 "crossbred cattle", "laying hen", "male calf", "beef cattle", "dairy cattle", "duck", "goose", "slaughter pig", "sow",
                               "turkey", "ewes", "feeder cattle", "suckler-cattle", "indigenous chicken")
comm_class[["stimulant crops and derived products"]] <- c("spice", "tobacco", "coffee", "cocoa", "chat", "cocoa beans", "tea")
comm_class[["beverages"]] <- c("beverages", "alcoholic beverages", "carbonated soft drinks")
comm_class[["roots and tubers, and derived products"]] <- c("cassava", "potato", "roots", "sweet potato", "tubers", "yam", "cocoyam")
comm_class[["pulses and derived products"]] <- c("cowpea", "pulses", "beans", "chickpea", "dry beans", "faba bean", "haricot bean", "legumes", "lentil", "vetch")
comm_class[["fodder crops and derived products"]] <- c("fodder", "animal feed", "feedgrain", "fodder crop", "fonio", "grass", "napier grass", "feed", "alfalfa")
comm_class[["nuts and derived products"]] <- c("cashews", "nuts")
comm_class[["beverage crops and spices"]] <- c("hop", "malt barley")
comm_class[["vegetable and animal oils and fats"]] <- c("edible oil", "vegetable oil", "vegetable fat")
comm_class[["hides and skins"]] <- c("leather")
comm_class[["other commodities*"]] <- c("enset", "salt")