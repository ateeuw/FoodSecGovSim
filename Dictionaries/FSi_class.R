# dictionary grouping food security indicators into the categories: 
# availability, accessibility, utilisation, stability, & unclear


FSi_class <- list()

FSi_class[["availability"]] <- c("energy contribution of X", "vitamin C contribution of X", "vitamin A contribution of X", "folate contribution of X", "calcium contribution of X", "beta-carotene contribution of X", "zinc contribution of X", "vitamin B6 contribution of X", "thiamin contribution of X", "riboflavin contribution of X", "niacin contribution of X", "iron contribution of X", "fiber contribution of X", "protein contribution of X", "fat contribution of X", "carbohydrate contribution of X", "pesticide use for X", "number of (active/passive) farms", "area used for X", "supply of water for irrigation", "area irrigated", "production of X", "productivity of X", 
                                   "yield of X", "yield gap of X", "availability of X", "area left fallow/unused", "supply of X", "consumption of agricultural water", "fertiliser use for X",
                                 "production of fertiliser", "production of X for self-consumption", "maximum irrigation water deficit")

FSi_class[["access - general"]] <- c("land use inequality", "access to X")

FSi_class[["access - economic"]] <- c("income from X production", "profit from X retail", "variability in import price of X", "profitability of land", "profitability of water use", "profitability of labour", "farm profit", "marginal cost of producing X", "marginal cost of shipping X",
                             "farm gate price of X", "price of X", "import price of X", "price of irrigation water", "added value of X", 
                             "contribution of X to GDP", "income", "discretionary income", "income from X", "income from agriculture", "income inequality", "poverty incidence/rate",
                             "poverty severity", "poverty depth/gap", "capital assets", "demand for X", "demand for farm land", "production costs",
                             "dietary income differential", "economic benefit/increase in output value of additional production of X", "inflation", "poverty rate/incidence/headcount ratio", 
                             "sales of X", "selling time of X", "unemployed", "demand for agrochemicals", "demand for capital", "demand for farm labour", "demand for agricultural water", 
                             "price of agrochemicals", "price of labour", "price of land", "price of fertilisers", "profitability of livestock", "return to investments (in agriculture)", 
                             "equivalent variation", "agricultural production value of X", "max price of X", "minimum equivalent variation", "profit from X production",
                             "production costs for X", "compensation variation", "economic surplus", "transportation cost of X")

FSi_class[["access - physical"]] <- c("X shopping frequency", "import of X", "export of X", "people walking to store as a last resort", "use of irrigation water")

FSi_class[["utilisation"]] <- c("calorie intake", "consumption of X", "Healthy Eating Index", "share of X in diet", "quality adjusted life years", "dietary income inequality", "food losses of X", "purchase of X",
                                "quality of life", "child malnourishment")

FSi_class[["stability"]] <- c("volatility of income", "resilience to water scarcity", "water security", "shortage of X", "stocks/surplus of X", "incidence of higher average income with higher variance (%)",
                                "incidence of higher average income with lower variance (%)", "incidence of identical average income and variance (%)", "variability in price of X",
                                "incidence of ïdentical average income with lower variance (%)", "incidence of lower average income with higher variance", "variability in equivalent variation",
                                "incidence of lower average income with lower variance [%]", "variance in income", "self-sufficiency of X", "food security risk", "variability in income",
                                "incidence of identical average income with lower variance (%)", "incidence of lower income with higher variance", "trade balance of X", "volatility of X prices",
                              "volatility of equivalent variation", "probability of food shortages/deficits/stockout falls", "volatility of X consumption", 
                              "contribution of X to GDP during drought", "price of X during drought", "income during drought", 
                              "access to X during drought", "availability of X during drought", "reliability of irrigation water supply",
                              "vulnerability of irrigation water supply", "import reliance for X")

FSi_class[["unclear"]] <- "unclear"
