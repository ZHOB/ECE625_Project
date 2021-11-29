setwd("/Users/nanaseki/Desktop/ECE625/Projects/ECE625_Project")
house=read.csv('./data/clean_data_28-11.csv',header=T)[-1]
attach(house)
names(house)
dim(house)
#these are Y/N factors
#has_garage_f=as.factor(has_garage)
#bs_f=as.factor(basement_finished)
#has_fireplace_f=as.factor(has_fireplace)
#f_tax=as.factor(fully_taxable)
#walk_b_f=as.factor(walkout_basement)
#air_cond=as.factor(air_conditioning)
#has_base=as.factor(has_basement)

nb=as.factor(neighbourhood)#too much
land_des=as.factor(landuse_description)#not so useful at the moment
mk_building=as.factor(market_building_class)#too much type
property=as.factor(property_type)#all the same


#logistic_regression
#currently not so useful.
set.seed(10)
library(stats)
glm.fit=glm(assessed_value~has_garage_f+bs_f+has_fireplace_f+f_tax+walk_b_f+air_cond+net_area+site_coverage_int+tot_gross_area_description_M2+has_base,data=house)
glm.fit=glm(assessed_value~as.factor(has_garage)
            +as.factor(basement_finished)
            +as.factor(has_fireplace)
            +as.factor(fully_taxable)
            +as.factor(walkout_basement)
            +as.factor(air_conditioning)
            +net_area
            +site_coverage_int
            +tot_gross_area_description_M2
            +as.factor(has_basement)
            ,data=train)

#10 chosen factor
#not so useful
# house_after_factor=c(as.factor(has_garage),
#                      as.factor(basement_finished),
#                      as.factor(has_fireplace),
#                      as.factor(fully_taxable),
#                      as.factor(walkout_basement),
#                      as.factor(air_conditioning),
#                      net_area,
#                      site_coverage_int,
#                      tot_gross_area_description_M2,
#                      as.factor(has_basement)
#                      )

#make those binary variable as factor
house$has_garage=as.factor(has_garage)
house$basement_finished= as.factor(basement_finished)
house$has_fireplace= as.factor(has_fireplace)
house$fully_taxable= as.factor(fully_taxable)
house$walkout_basement=as.factor(walkout_basement)
house$air_conditioning=as.factor(air_conditioning)
house$has_basement=as.factor(has_basement)

#make the valuation_group as factor.
 valuation_group_type=rep(0,49584)
 valuation_group_type[valuation_group=='RESIDENTIAL SOUTH']=1
 valuation_group_type=rep(0,49584)
 valuation_group_type[valuation_group=='RESIDENTIAL SOUTH']=1
 valuation_group_type[valuation_group=='RESIDENTIAL NORTH']=2
 valuation_group_type[valuation_group=='RESIDENTIAL RIVVAL']=3
 valuation_group_type[valuation_group=='RESIDENTIAL LAND']=4
 valuation_group_type[valuation_group=='RESIDENTIAL WC']=5
 valuation_group_type=as.factor(valuation_group_type)
 valuation_group_type
 house=data.frame(house,valuation_group_type)
  
 #make building count as factor
 building_count_type=rep(1,49584)
 building_count_type[building_count==1]=1
 building_count_type[building_count>1]=2
 building_count_type=as.factor(building_count_type)
 house=data.frame(house,building_count_type)
 
 #divide the training set
 dt=sample(nrow(house),nrow(house)*0.9)
 train=house[dt,]
 test=house[-dt,]
 #linear_regression Some factors are not considered in.
 #Multiple R-squared:  0.8086,	Adjusted R-squared:   0.8072 
 house_lm.fit02=lm(assessed_value~house$has_garage
                   +house$basement_finished
                   +house$has_fireplace
                   #+house$fully_taxable
                   +house$walkout_basement
                   +house$air_conditioning
                   +house$net_area
                   +house$site_coverage_int
                   +house$tot_gross_area_description_M2
                   +house$has_basement
                   +house$building_count_type
                   +house$valuation_group_type
                   +as.factor(house$neighbourhood)#
                   +as.factor(house$market_building_class_split)#?
                   +house$build_year_mbc_Y
                   +house$landuse_description
                  #+house$effective_build_year
                   ,data=house,subset=dt)
 summary(house_lm.fit02) 
 predict_result=predict(house_lm.fit02,data=test)
 #model rss 8.508109e+14
 sum(residuals(house_lm.fit02)^2)
 
 
 #lm only with numeric: not so good.Multiple R-squared:  0.6365,	Adjusted R-squared:  0.6364 
 house.lm.fit.num=lm(assessed_value~net_area 
                   +lot_size_M2
                   +site_coverage_int
                   +tot_gross_area_description_M2
                   ,data=train)
 plot(house.lm.fit.num)
 summary(house.lm.fit.num)  
 
 #with out years and complex columns
 house[1,]
 pairs(house[,-c(1,2,3,4,11,12,18,19,21,24)])
 house_edit=house[,-c(1,2,3,4,11,12,18,19,21,24)]
 
#2 year-types plot
 plot(build_year_mbc_Y,assessed_value)
 plot(effective_build_year,assessed_value)
 
 #try some subset selection
 library(leaps)
 regfit.fwd=regsubsets(assessed_value~house$has_garage
                       +house$basement_finished
                       +house$has_fireplace
                       #+house$fully_taxable
                       +house$walkout_basement
                       +house$air_conditioning
                       +house$net_area
                       +house$site_coverage_int
                       +house$tot_gross_area_description_M2
                       +house$has_basement
                       +house$building_count_type
                       +house$valuation_group_type
                       +as.factor(house$neighbourhood)#
                       +as.factor(house$market_building_class_split)#?
                       +house$build_year_mbc_Y
                       +house$landuse_description,#
                       data=house,
                       nvmax=10,
                       method="forward")
 coef(regfit.fwd,10)#the category has too many times, not so effective
 
 
 