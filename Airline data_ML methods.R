setwd("C:/Users/user/Downloads/110_1課程/五234_商業分析：SAS  R應用/HW3")
airline <- read.csv("airline_survey.csv",sep=",")
airline$satisfaction <- as.factor(ifelse(airline$satisfaction == "satisfied", 1, 0))
table(airline$satisfaction) 

index <- sample(1:nrow(airline),1000)
airline.sm<- airline[index,]
airline.sm <- na.omit(airline.sm)
airline.sm <- airline.sm %>%
  mutate(
    Gender = ifelse(Gender=='Male',1,0),
    Customer.Type = ifelse(Customer.Type=='Loyal Customer',1,0),
    Type.of.Travel = ifelse(Type.of.Travel=='Business travel',1,0),
    Class = ifelse(Class=='Business',0,ifelse(Class=='Eco',1,2)),
    Flight.Distance = (Flight.Distance - min(Flight.Distance)) / (max(Flight.Distance) - min(Flight.Distance)),
    Departure.Delay.in.Minutes= (Departure.Delay.in.Minutes - min(Departure.Delay.in.Minutes)) / (max(Departure.Delay.in.Minutes) - min(Departure.Delay.in.Minutes)),
    Arrival.Delay.in.Minutes= (Arrival.Delay.in.Minutes - min(Arrival.Delay.in.Minutes)) / (max(Arrival.Delay.in.Minutes) - min(Arrival.Delay.in.Minutes))
    )
table(airline.sm$satisfaction)

#1.
###(a)
library(dplyr)
#決策樹
library(rpart)
tree <- rpart(satisfaction ~ .-id -X ,data=airline.sm, method="class")
library(rpart.plot) 
rpart.plot(tree)
rpart.rules(tree,cover=T)
tree_prune <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(tree_prune)
#Random Forest
library(randomForest)
rf <- randomForest(satisfaction ~ .-id -X, data = airline.sm, importance=TRUE, ntree=100)
importance(rf)
varImpPlot(rf, n.var = min(20, nrow(rf$importance)),
           main = 'Top 20 - variable importance')

#2.
library(factoextra)
data <- airline.sm[,c(5:9,11,14:16,18)]
E.dist <- dist(data, method="euclidean") # 歐式距離
tree1 <- hclust(E.dist, method="ward.D2")
fviz_nbclust(data, FUN = hcut, method = "wss") #運用Gap statistic找適合的分群數目
plot(tree1, xlab="Euclidean",h=-1)
rect.hclust(tree1,k=3,border="red")
cluster <- cutree(tree1, k=3)
table(cluster) 
data = cbind(data,cluster)
ggplot(data, aes(x=as.factor(cluster), y=Flight.Distance)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cluster), y=Age)) + 
  geom_boxplot()
ggplot( data =data) +
  geom_bar( aes( x = Type.of.Travel)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Class)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Inflight.wifi.service)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Ease.of.Online.booking)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Online.boarding)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Seat.comfort)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Inflight.entertainment)) + 
  facet_wrap( ~ cluster)
ggplot( data =data) +
  geom_bar( aes( x = Leg.room.service)) + 
  facet_wrap( ~ cluster)

library(cluster)
ac_ward <- agnes(E.dist, method = "ward")
ac_ward$ac #接近1表示分類方式穩固
