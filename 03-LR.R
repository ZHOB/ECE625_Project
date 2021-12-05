library(MASS)
library(ggplot2)


house = read.csv("/Users/zhob/Desktop/ECE625/project/ECE625_Project/data/clean_data_28-11.csv",  header=TRUE)

dim(house)
str(house)
summary(house)

attach(house)
UniqueValue = function (x) {length(unique(x)) }
apply(house, 2, UniqueValue)

NaValue = function (x) {sum(is.na(x)) }
apply(house, 2, NaValue)

BlankValue = function (x) {sum(x=="") }
apply(house, 2, BlankValue)

MissPercentage = function (x) {100 * sum (is.na(x)) / length (x) }
apply(house, 2, MissPercentage)

set.seed(1)
row.number = sample(1:nrow(house), 0.9*nrow(house))
train = house[row.number,]
test = house[-row.number,]
dim(train)
dim(test)

attach(train)
model1 = glm(factor(assessed_value)~has_garage
                   +basement_finished 
                   #+walkout_basement 
                   #+air_conditioning 
                   +net_area 
                   #+site_coverage_int 
                   +tot_gross_area_description_M2 
                   +has_basement 
                   #+building_count_type 
                   #+valuation_group_type 
                   #+neighbourhood 
                   #+market_building_class_split 
                   #+landuse_description 
                   #+geometry 
                   #+effective_build_year 
                   #+market_building_class 
                   #+build_year_mbc_Y 
                   +has_fireplace 
                   +fully_taxable
				   ,data=train, family=binomial)
summary(model1)

###Predict for training data and find training accuracy
pred.prob = predict(model1, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
predict_result=predict(model1,data=test)
#table(pred.prob, assessed_value)
rmse=(mean((predict_result- test$assessed_value)^2))^0.5
rmse

