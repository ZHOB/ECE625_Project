setwd("/Users/nanaseki/Desktop/ECE625/Projects/ECE625_Project")
house=read.csv('./data/clean_data_28-11.csv',header=T)[-1]
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

# house$geometry=as.factor(house$geometry)
# house$geometry=as.numeric(house$geometry)
# house$geometry=(house$geometry-min(house$geometry))/(max(house$geometry)-min(house$geometry))

#make those binary variable as numeric
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

# #make the valuation_group as numeric
 valuation_group_type=rep(0,49584)
 valuation_group_type[valuation_group=='RESIDENTIAL SOUTH']=1
 valuation_group_type[valuation_group=='RESIDENTIAL NORTH']=2
 valuation_group_type[valuation_group=='RESIDENTIAL RIVVAL']=3
 valuation_group_type[valuation_group=='RESIDENTIAL LAND']=4
 valuation_group_type[valuation_group=='RESIDENTIAL WC']=5
 #valuation_group_type=as.factor(valuation_group_type)
 #valuation_group_type=scale(valuation_group_type)
 
#make it normalized
#valuation_group_type=(valuation_group_type-min(valuation_group_type))/(max(valuation_group_type)-min(valuation_group_type))
#valuation_group_type

  
 #make building count as numeric
 building_count_type=rep(1,49584)
 building_count_type[building_count==1]=0
 building_count_type[building_count>1]=1
 #building_count_type=as.factor(building_count_type)
 
 house=data.frame(house,valuation_group_type)
 house=data.frame(house,building_count_type)
 
 
 
 # #scale the other numeric classes
 # house$net_area=scale(house$net_area)
 # house$lot_size_M2=scale(house$lot_size_M2)
 # house$site_coverage_int=scale(house$site_coverage_int)
 # house$tot_gross_area_description_M2=scale(house$tot_gross_area_description_M2)
 #normalization
 # house$net_area=(house$net_area-min(house$net_area))/(max(house$net_area)-min(house$net_area))
 # house$lot_size_M2=(house$lot_size_M2-min(house$lot_size_M2))/(max(house$lot_size_M2)-min(house$lot_size_M2))
 # house$site_coverage_int=( house$site_coverage_int-min( house$site_coverage_int))/(max( house$site_coverage_int)-min( house$site_coverage_int))
 # house$tot_gross_area_description_M2=(house$tot_gross_area_description_M2-min(house$tot_gross_area_description_M2))/(max(house$tot_gross_area_description_M2)-min(house$tot_gross_area_description_M2))
 
 
 
 #divide the training set
 dt=sample(nrow(house),nrow(house)*0.9)
 train=house[dt,]
 test=house[-dt,]
 
 house$valuation_group_type=scale(house$valuation_group_type)
 #house$valuation_group_type=(valuation_group_type-min(valuation_group_type))/(max(valuation_group_type)-min(valuation_group_type))
 
 #house$landuse_description=landuse_description
 #linear_regression Some factors are not considered in.

 house_lm.fit02=lm(assessed_value~#house$has_garage
                   #+house$basement_finished
                   #+house$walkout_basement
                   #+house$air_conditioning
                   +house$net_area
                   #+house$site_coverage_int
                   #+house$tot_gross_area_description_M2
                   # +house$has_basement
                   # +house$building_count_type
                   # +house$valuation_group_type
                   # +house$neighbourhood
                   # +house$market_building_class_split
                   #+house$landuse_description
                   #+house$geometry
                   #+house$effective_build_year
                   #+house$market_building_class no
                   #+house$build_year_mbc_Y
                   #+house$has_fireplace
                   # +house$fully_taxable
                   ,data=house,subset=dt)
 summary(house_lm.fit02)
 predict_result=predict(house_lm.fit02,data=test)
 rmse=(mean((predict_result- house$assessed_value)^2))^0.5
 rmse
 #330141.1 only with this parameter get the lowest rmse.
 
 #rmse 335956.2 the other factors are not in a linear relation with the assessed value
 #337959.4 no scale on original numeric type.+normalize va-group
 #341003.4 after #all normalization
 #rmse= 341528.2 not using mk-split #in scale
 #rmse= 339835.5 after using mk_split #in scale
 #rmse= 339827.4 after not using #+house$has_fireplace#+house$fully_taxable
 #to see the relations between numeric onses.
 pairs(house[,c(5,19,21,22,9)])
 
 #using spline
 library(splines)
 assessed_value=house$assessed_value
 net_area=house$net_area
 plot(net_area,assessed_value,col="darkgrey")
 house_ns.fit=lm(assessed_value~ns(house$net_area,df=4)
                 +ns(house$tot_gross_area_description_M2,df=4)
                   ,data=house,subset = dt)
 pred=predict( house_ns.fit,newdata=list(test),se=T)
 lines(house$net_area, pred$fit,col="red",lwd=2)
 
 plot(tot_gross_area_description_M2,assessed_value,col="darkgrey")
 lines(house$tot_gross_area_description_M2, pred2$fit,col="red",lwd=2)
 #rmse
 rmse=(mean((pred$fit- house$assessed_value)^2))^0.5
 rmse
 #using natural spline and house$net_area, the rmse reduced to 146870.2
 #using ns in house$tot_gross_area_description_M2 and net_area, the rmse reduce to 140607.9

 
 
 
 
