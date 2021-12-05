library(MASS)

house = read.csv("/Users/zhob/Desktop/ECE625/project/ECE625_Project/data/clean_data_28-11.csv",  header=TRUE)[-1]  # read csv file 

attach(house)
names(house)
dim(house)

#numeric
nb=as.factor(neighbourhood)
nb=as.numeric(nb)

land_des=as.factor(landuse_description)
land_des=as.numeric(land_des)

mk_building=as.factor(market_building_class)
mk_building=as.numeric(mk_building)

mk_building_sp=as.factor(market_building_class_split)
mk_building_sp=as.numeric(mk_building_sp)

#year is not so useful.
#house$build_year_mbc_Y=( house$build_year_mbc_Y-min( house$build_year_mbc_Y))/(max( house$build_year_mbc_Y)-min( house$build_year_mbc_Y))

#scale
nb=scale(nb)
land_des=scale(land_des)
mk_building=scale(mk_building)
mk_building_sp=scale(mk_building_sp)

#normalization -not as good as scale
# nb=(nb-min(nb))/(max(nb)-min(nb))
# land_des=(land_des-min(land_des))/(max(land_des)-min(land_des))
# mk_building=(mk_building-min(mk_building))/(max(mk_building)-min(mk_building))
# mk_building_sp=(mk_building_sp-min(mk_building_sp))/(max(mk_building_sp)-min(mk_building_sp))

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

valuation_group_type=rep(0,49584)
valuation_group_type[valuation_group=='RESIDENTIAL SOUTH']=1
valuation_group_type[valuation_group=='RESIDENTIAL NORTH']=2
valuation_group_type[valuation_group=='RESIDENTIAL RIVVAL']=3
valuation_group_type[valuation_group=='RESIDENTIAL LAND']=4
valuation_group_type[valuation_group=='RESIDENTIAL WC']=5

 building_count_type=rep(1,49584)
 building_count_type[building_count==1]=0
 building_count_type[building_count>1]=1
 #building_count_type=as.factor(building_count_type)
 
 house=data.frame(house,valuation_group_type)
 house=data.frame(house,building_count_type)


assessed_value_mean <- ifelse( assessed_value > mean(assessed_value), yes = 1, no = 0)
assessed_value_median <- ifelse( assessed_value > median(assessed_value), yes = 1, no = 0)

house = data.frame(house, assessed_value_mean)
house = data.frame(house, assessed_value_median)


set.seed(10) 
## 90% training and 10% test

dt=sample(nrow(house),nrow(house)*0.9)
train=as.data.frame(house[dt,])
test=as.data.frame(house[-dt,])

house$valuation_group_type=scale(house$valuation_group_type)

lda.fit=lda(assessed_value_median~has_garage #***
                   #+basement_finished 
                   #+walkout_basement 
                   #+air_conditioning
                   +net_area #0.001855437 #***
                   +site_coverage_int #***
                   +tot_gross_area_description_M2 #***
                   #+has_basement 
                   #+building_count_type 
                   #+valuation_group_type 
                   #+neighbourhood
                   #+market_building_class_split 
                   #+landuse_description 
                   #+geometry 
                   +effective_build_year #***
                   #+market_building_class 
                   +build_year_mbc_Y  #***
                   #+has_fireplace 
                   #+fully_taxable 
                   ,data=house,subset=dt)
lda.fit

predict = predict(lda.fit,test)

names(predict)

aa = table(predict$class,test$assessed_value_median)

aa

#accurcy
mean(predict$class == test$assessed_value_median) #0.8550111

#error rate
(aa[2,1]+aa[1,2])/sum(sum(aa) ) #0.1449889

lda.fit02=lda(assessed_value_mean~#has_garage 
                   #+basement_finished 
                   +walkout_basement #***
                   +air_conditioning #***
                   +net_area #0.001855437 #***
                   +site_coverage_int #***
                   #+tot_gross_area_description_M2 
                   +has_basement #***
                   #+building_count_type 
                   #+valuation_group_type 
                   #+neighbourhood
                  # +market_building_class_split 
                  # +landuse_description 
                  # +geometry 
                  # +effective_build_year 
                   +market_building_class #***
                  # +build_year_mbc_Y  
                   +has_fireplace #***
                  # +fully_taxable 
                   ,data=house,subset=dt)
lda.fit

predict = predict(lda.fit02,test)

names(predict)

aa = table(predict$class,test$assessed_value_mean)

aa

#accurcy
mean(predict$class == test$assessed_value_mean) #0.8812261

#error rate
(aa[2,1]+aa[1,2])/sum(sum(aa) ) #0.1187739


