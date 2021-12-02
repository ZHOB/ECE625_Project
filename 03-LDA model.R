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


set.seed(10) 
## 90% training and 10% test
##train=sample(1:nrow(houses), nrow(houses)/1.11) # second way to create test and train set, replacement false.. once you use the fold you cannot use it again
##test=(-train)


smp_size_raw <- floor(0.9 * nrow(house))
train_ind_raw <- sample(nrow(house), size = smp_size_raw)
train <- house[train_ind_raw, ]
test <- house[-train_ind_raw, ]


house$valuation_group_type=scale(house$valuation_group_type)

house_lda.fit02=lda(assessed_value~ #house$has_garage
                   #+house$basement_finished
                   #+house$walkout_basement
                   house$air_conditioning
                   +house$net_area
                   +house$site_coverage_int
                   +house$tot_gross_area_description_M2
                   +house$has_basement
                   +house$building_count_type
                   +house$valuation_group_type
                   +house$neighbourhood
                   #+house$market_building_class_split
                   #+house$landuse_description
                   #+house$geometry
                   #+house$effective_build_year
                   #+house$market_building_class
                   #+house$build_year_mbc_Y
                   #+house$has_fireplace
                   #+house$fully_taxable
                   ,data=train)
summary(house_lm.fit02)

predict = predict(lda.fit, houses[test,])

#table(predict$class, houses[test,])
#mean(predict$class == houses[test,])

