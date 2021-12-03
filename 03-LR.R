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
model1 = glm(factor(assessed_value)~#has_garage
                   #basement_finished #0.002339464
                   #+walkout_basement #0.002863827
                   #+air_conditioning #0.002823491
                   net_area #0.001855437
                   #+site_coverage_int #0.002682317
                   +tot_gross_area_description_M2 #0.001875605
                   #+has_basement #0.002601646
                   #+building_count_type #0.003025169
                   #+valuation_group_type #0.002178122
                   #+neighbourhood #0.002238625
                   #+market_building_class_split #0.001714263
                   #+landuse_description #0.002803324
                   #+geometry #0.003005002
                   #+effective_build_year #0.001915941
                   #+market_building_class #0.001714263
                   #+build_year_mbc_Y #0.001915941
                   +has_fireplace #0.001452081
                   +fully_taxable #0.003005002
				   ,data=train, family=binomial)
summary(model1)

###Predict for training data and find training accuracy
pred.prob = predict(model1, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, assessed_value)


