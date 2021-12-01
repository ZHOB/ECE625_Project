library(MASS)

houses = read.csv("/Users/zhob/Desktop/ECE625/project/ECE625_Project/data/02-merge_data_30-11.csv",  header=TRUE)[-1]  # read csv file 

attach(houses)

set.seed(10) 
## 90% training and 10% test
train=sample(1:nrow(houses), nrow(houses)/1.11) # second way to create test and train set, replacement false.. once you use the fold you cannot use it again
test=(-train)
dim(houses[test,])

lda.fit=lda(assessed_value~landuse_description+market_building_class+property_type + effective_build_year+net_area+basement_finished+has_garage +  has_fireplace + assessed_value+street_name+postal_code+neighbourhood+fully_taxable+fully_complete+lot_size+building_count+build_year_mbc+walkout_basement+air_conditioning+valuation_group+site_coverage+tot_gross_area_description, houses[train,])
predict = predict(lda.fit, houses[test,])

#table(predict$class, houses[test,])
#mean(predict$class == houses[test,])
