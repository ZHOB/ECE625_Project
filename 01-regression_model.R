setwd("D:/Academic/Fall21/ECE625/final project/ECE625_Project/")
houses = read.csv("./data/clean_data_28-11",  header=TRUE)[-1]  # read csv file 

attach(houses)

no_factor_col = c('net_area',
                  'lot_size_M2',
                  'site_coverage_int',
                  'build_year_mbc_Y')

cor(houses[,no_factor_col])

set.seed(10) 
## 90% training and 10% test
train=sample(1:nrow(houses), nrow(houses)/1.11) # second way to create test and train set, replacement false.. once you use the fold you cannot use it again
test=(-train)
dim(houses[test,])


houses_lm.fit02=lm(assessed_value~ net_area
                  +(lot_size_M2)
                  +(site_coverage_int)
                  +(build_year_mbc_Y)
                  +basement_finished
                  +has_garage
                  +has_fireplace
                  +fully_taxable
                  +fully_complete
                  +walkout_basement
                  +air_conditioning
                  +building_count
                  +neighbourhood
                  +geometry
                  +market_building_class_split
                  +has_basement,
                  data=houses[train,])
summary(houses_lm.fit02) #Multiple R-squared:  0.8132,	Adjusted R-squared:  0.812 
assessed_value_predicted = predict(houses_lm.fit1, houses[test,])
residuals()
