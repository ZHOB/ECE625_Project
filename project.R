dirc_zihao = "/Users/zhob/Desktop/ECE625/project/ECE625_Project/data/data.csv"

dirc_xiaohan = "/Users/nanaseki/Desktop/ECE625/Projects/ECE625_Project/data/clean_data_28-11.csv"

dirc_luisa = ""

## add your own local directory and change the follow to your own dirctory to use :)
data <- read.csv(dirc_zihao)

## 90% of the sample size

set.seed(2)
dt = sort(sample(nrow(data), nrow(data)*.9))
train<-data[dt,]
test<-data[-dt,]

