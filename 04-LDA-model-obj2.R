library(MASS)

house = read.csv("/Users/zhob/Desktop/ECE625/project/ECE625_Project/data/data_for_classification_model.csv",  header=TRUE)[-1]  # read csv file 

attach(house)
names(house)
dim(house)


nb=as.factor(neighbourhood)
nb=as.numeric(nb)

land_des=as.factor(landuse_description)
land_des=as.numeric(land_des)

mk_building=as.factor(market_building_class)
mk_building=as.numeric(mk_building)

mk_building_sp=as.factor(market_building_class_split)
mk_building_sp=as.numeric(mk_building_sp)

#scale
nb=scale(nb)
land_des=scale(land_des)
mk_building=scale(mk_building)
mk_building_sp=scale(mk_building_sp)

house$market_building_class=mk_building
house$landuse_description=land_des
house$neighbourhood=nb
house$market_building_class_split=mk_building_sp

house$has_garage[has_garage=='Yes']=1
house$has_garage[has_garage=='NO']=0
house$has_garage=as.numeric(house$has_garage)

house$basement_finished[house$basement_finished=='Yes']=1
house$basement_finished[house$basement_finished=='NO']=0
house$basement_finished=as.numeric(house$basement_finished)

house$has_fireplace[house$has_fireplace=='Yes']=1
house$has_fireplace[house$has_fireplace=='NO']=0
house$has_fireplace=as.numeric(house$has_fireplace)

house$fully_taxable[house$fully_taxable=='Yes']=1
house$fully_taxable[house$fully_taxable=='NO']=0
house$fully_taxable=as.numeric(house$fully_taxable)

house$walkout_basement[house$walkout_basement=='Yes']=1
house$walkout_basement[house$walkout_basement=='NO']=0
house$walkout_basement=as.numeric(house$walkout_basement)

house$air_conditioning[house$air_conditioning=='Yes']=1
house$air_conditioning[house$air_conditioning=='NO']=0
house$air_conditioning=as.numeric(house$air_conditioning)

house$has_basement[house$has_basement=='Yes']=1
house$has_basement[house$has_basement=='No']=0
house$has_basement=as.numeric(house$has_basement)


set.seed(10) 
## 90% training and 10% test

dt=sample(nrow(house),nrow(house)*0.9)
train=as.data.frame(house[dt,])
test=as.data.frame(house[-dt,])

lda.fit=lda(increase~#landuse_description
					#+market_building_class
					property_type #***useful feature
					#+effective_build_year #not useful
					+net_area # *
					#+ basement_finished
					#+ has_garage
					#+ has_fireplace
					+assessed_value
					#+ street_name #really slow and does not help
					#+ postal_code #really slow and does not help
					#+neighbourhood
					#+ fully_taxable
					#+ fully_complete
					#+ building_count
					#+ walkout_basement
					+ air_conditioning # *
					#+ valuation_group
					+ geometry # *
					#+ Assessment.Year ##Error: variable 16 appears to be constant within groups
					#+ Legal.Description #memery exhausted
					#+ Zoning # * but slow
					+ Assessed.Value
					#+Assessment.Class.1
					+Assessment.Class...1 # *
					#+ lot_size_M2
					#+ build_year_mbc_Y #not useful
					#+ site_coverage_int
					+ tot_gross_area_description_M2 # *
					+ market_building_class_split # *
					+ X3string_postal_code #***useful
					#+ has_basement 
                   ,data=house,subset=dt)
                   
#lda.fit

predict = predict(lda.fit,test)

names(predict)

aa = table(predict$class,test$increase)

aa

#accurcy
mean(predict$class == test$increase)     #0.7201848

lda.fit.without.av=lda(increase~#landuse_description
					#+market_building_class
					property_type #***useful feature
					#+effective_build_year #not useful
					+net_area # *
					#+ basement_finished
					#+ has_garage
					#+ has_fireplace
					#+assessed_value
					#+ street_name #really slow and does not help
					#+ postal_code #really slow and does not help
					#+neighbourhood
					#+ fully_taxable
					#+ fully_complete
					#+ building_count
					#+ walkout_basement
					+ air_conditioning # *
					#+ valuation_group
					+ geometry # *
					#+ Assessment.Year ##Error: variable 16 appears to be constant within groups
					#+ Legal.Description #memery exhausted
					#+ Zoning # * but slow
					#+ Assessed.Value
					#+Assessment.Class.1
					+Assessment.Class...1 # *
					#+ lot_size_M2
					#+ build_year_mbc_Y #not useful
					#+ site_coverage_int
					+ tot_gross_area_description_M2 # *
					+ market_building_class_split # *
					+ X3string_postal_code #***useful
					#+ has_basement 
                   ,data=house,subset=dt)
                   
#lda.fit

predict = predict(lda.fit.without.av,test)

names(predict)

aa = table(predict$class,test$increase)

aa

#accurcy
mean(predict$class == test$increase)     #0.7201848
