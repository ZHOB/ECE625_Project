dirc = "/Users/zhob/Desktop/ECE625/project/ECE625_Project/data/data.csv"
data <- read.csv(dirc)
## 90% of the sample size

set.seed(2)
dt = sort(sample(nrow(data), nrow(data)*.9))
train<-data[dt,]
test<-data[-dt,]

