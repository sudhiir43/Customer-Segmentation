# set working directory and read data into r

rm(list=ls())
setwd("E:\\Project\\data")
mydata<-read.csv("DataRetail_1.csv")

##############################################


head(mydata)
length(unique(mydata$ProductID))

#to check top 30 popular products
rev(sort(table(mydata$ProductID)))[1:30]

sum(is.na(mydata))
mydata$Transaction.Time<-gsub("-","/",mydata$Transaction.Time)
mydata$Transaction.Time<-strptime(mydata$Transaction.Time, "%d/%m/%Y %H:%M")
summary(mydata)
time1<-strptime("2016-12-31 19:36:00", "%Y-%m-%d %H:%M:%S") # most recent day
# calculate the days since perticular transaction happened
mydata$days_since<-as.numeric(difftime(time1=time1,time2 =mydata$Transaction.Time),units="days")
str(mydata)
#extract usefull variables#####
sapply(mydata, class) # check the class of each column
mydata$Transaction.Time<-NULL # transaction time throwing an error while working with sqldf because of the class
library(sqldf)
data1<-sqldf("SELECT CustomerID,MIN(days_since) AS 'recency',COUNT(*) AS 'frequency',
             AVG(Cost) AS 'avgcost',MAX(days_since)-MIN(days_since) AS 'tenure' FROM mydata GROUP BY 1 ")
summary(data1)
newdata<-data1
hist(data1$recency)
hist(data1$frequency)
hist(data1$avgcost)
hist(log(data1$avgcost),breaks=100)

#preparing the data for clustering#####

data1$avgcost<-log(data1$avgcost) # log transformation  

#change the row names to customer id numbers and remove customerID column for easy clustering
row.names(data1)<-data1$CustomerID
data1$CustomerID<-NULL
boxplot(data1)
data1<-scale(data1) #scale the data
boxplot(data1)

#####################################
######### H Cluctering ##############
#####################################


d<-dist(data1)   # calculating the distance matrix

c<-hclust(d,method = "ward.D2") 

plot(c) #plotting dendrogram

members<-cutree(c,k=6)
rect.hclust(c, k=6, border="red")
#plot()
table(members)
#members
aggregate(newdata[,2:5],by=list(members),mean)


#####################################
######### K Means ################
#####################################

withinss<-c()
for(i in seq(1,15)){ 
  set.seed(100)
  kmeans1<-kmeans(data1,i,iter.max = 100)
  withinss<-c(withinss,sum(kmeans1$withinss))
}
plot(withinss,main="withinss",cex=1.5,col='blue')
kmeans2<-kmeans(data1,9,iter.max = 100) # pick the optimal K
members2<-kmeans2$cluster

aggregate(newdata[,2:5],by=list(members2),mean)
finaldata<-cbind(newdata,members2)
library(ggplot2)
ggplot(finaldata,aes(frequency,avgcost))+geom_jitter(color=members2)

ggplot(finaldata,aes(frequency,recency))+geom_jitter(color=members2)

ggplot(finaldata,aes(frequency,recency))+geom_jitter(color=members2)

library(fpc)
plotcluster(newdata,members2)


############   managerial segmentation   ##################

# easy to interpret and easy to update 

data2<-sqldf("SELECT CustomerID,MIN(days_since) AS 'recency',COUNT(*) AS 'frequency',MAX(days_since) AS first_purchase,
             AVG(Cost) AS 'avgcost',MAX(days_since)-MIN(days_since) AS 'tenure' FROM mydata GROUP BY 1 ")

data2$segment<-NA
data2$segment[which(data2$recency>183*3)]="inactive"
data2$segment[which(data2$recency<=183*3&data2$recency>183*2)]="cold"
data2$segment[which(data2$recency<=183*2&data2$recency>183*1)]="warm"
data2$segment[which(data2$recency<=183*1)]="active"

boxplot(data2$avgcost)
median(data2$avgcost)

data2$segment[which(data2$segment=="warm"&data2$first_purchase<=183*2)]="new warm"
data2$segment[which(data2$segment=="warm"&data2$avgcost>180)]="warm high value"
data2$segment[which(data2$segment=="warm"&data2$avgcost<=180)]="warm low value"

data2$segment[which(data2$segment=="active"&data2$first_purchase<=183*1)]="new active"
data2$segment[which(data2$segment=="active"&data2$avgcost>180)]="active high value"
data2$segment[which(data2$segment=="active"&data2$avgcost<=180)]="active low value"


table(data2$segment)
########## customer lifetime estimation  #################
data3<-sqldf("SELECT CustomerID,MIN(days_since) AS 'recency',COUNT(*) AS 'frequency',MAX(days_since) AS first_purchase,
             AVG(Cost) AS 'avgcost',MAX(days_since)-MIN(days_since) AS 'tenure' FROM mydata GROUP BY 1 ")
data3$segment<-NA
data3$segment[which(data3$recency>183*3)]="inactive"
data3$segment[which(data3$recency<=183*3&data2$recency>183*2)]="cold"
data3$segment[which(data3$recency<=183*2&data2$recency>183*1)]="warm"
data3$segment[which(data3$recency<=183*1)]="active"

boxplot(data3$avgcost)
median(data3$avgcost)

data3$segment[which(data3$segment=="warm"&data3$first_purchase<=183*2)]="new warm"
data3$segment[which(data3$segment=="warm"&data3$avgcost>180)]="warm high value"
data3$segment[which(data3$segment=="warm"&data3$avgcost<=180)]="warm low value"

data3$segment[which(data3$segment=="active"&data3$first_purchase<=183*1)]="new active"
data3$segment[which(data3$segment=="active"&data3$avgcost>180)]="active high value"
data3$segment[which(data3$segment=="active"&data3$avgcost<=180)]="active low value"
data3$segment<-factor(x=data3$segment,levels = c("inactive","cold","warm high value",
                                                 "warm low value","new warm","active high value",
                                                 "active low value","new active"))

###################################################################################

#calculating the customer segments one six months erliar

data4<-sqldf("SELECT CustomerID,MIN(days_since) AS 'recency',COUNT(*) AS 'frequency',MAX(days_since) AS first_purchase,
             AVG(Cost) AS 'avgcost',MAX(days_since)-MIN(days_since) AS 'tenure' FROM mydata GROUP BY 1 ")
data4$recency<-data4$recency - 183
data4$segment<-NA
data4$segment[which(data4$recency>183*3)]="inactive"
data4$segment[which(data4$recency<=183*3 & data4$recency>183*2)]="cold"
data4$segment[which(data4$recency<=183*2 & data4$recency>183*1)]="warm"
data4$segment[which(data4$recency<=183*1&data4$recency>0)]="active"

boxplot(data4$avgcost)
median(data4$avgcost)

data4$segment[which(data4$segment=="warm" & data4$first_purchase<=183*2)]="new warm"
data4$segment[which(data4$segment=="warm" & data4$avgcost>180)]="warm high value"
data4$segment[which(data4$segment=="warm" & data4$avgcost<=180)]="warm low value"

data4$segment[which(data4$segment=="active" & data4$first_purchase<=183*1)]="new active"
data4$segment[which(data4$segment=="active" & data4$avgcost>180)]="active high value"
data4$segment[which(data4$segment=="active" & data4$avgcost<=180)]="active low value"

data4$segment<-factor(x=data4$segment,levels = c("inactive","cold","warm high value",
                                                 "warm low value","new warm","active high value",
                                                 "active low value","new active"))
table(data4$segment,data3$segment)



######### Assoiation Rules  ##########

library(arules)
trans = read.transactions("DataRetail_1.csv", format = "single", sep = ",", cols = c("TransactionID", "ProductID"))
transdata=as(trans, "data.frame") # just to check
rules = apriori(trans,parameter = list(sup = 0.0005, conf = 0.5,target="rules"))

summary(rules)
p= as(rules, "data.frame")

q = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)

#################################################################

### SCORING MODEL #####
rm(list=ls())
setwd("E:\\Project\\data")
data<-read.csv("DataRetail_1.csv")
data$Transaction.Time<-gsub("-","/",data$Transaction.Time)
data$Transaction.Time<-strptime(data$Transaction.Time, "%d/%m/%Y %H:%M")

data$Year_Of_Purchase<-as.numeric(format(data$Transaction.Time,"%Y"))
time1<-strptime("2016-12-31 19:36:00", "%Y-%m-%d %H:%M:%S") # most recent day

# calculate the days since perticular transaction happened
data$Days_Since<-as.numeric(difftime(time1=time1,time2 =data$Transaction.Time),units="days")
data$Transaction.Time<-NULL

#extracting useful features

library(sqldf)

customers_2015<-sqldf("SELECT CustomerID,RetailStore,MIN(Days_Since)-365 AS 'First_Purchase',MAX(Days_Since)-365 AS 'Recency',
                      AVG(Cost) AS Avg_Amount,COUNT(*) AS Frequency , MAX(Cost) AS 'Max_Amount',
                      MIN(Cost) AS 'Min_Amount' FROM data WHERE Days_Since >365 GROUP BY 1 ")

#compute revenue generated by the customers in 2016
revenue_2016<-sqldf("SELECT CustomerID,SUM(Cost) AS 'revenue_2016'  FROM data WHERE Year_Of_Purchase=2016 GROUP BY 1")

#merge customers_2015 and revenue 2016

in_sample<- merge(customers_2015,revenue_2016,all.x=T)
in_sample$revenue_2016[is.na(in_sample$revenue_2016)]=0
in_sample$Active<-as.numeric(in_sample$revenue_2016>0)
table(in_sample$Active)
head(in_sample)

###############################################################
library(nnet)
# a multinom neural network to calculate the probabilities of each class
prob.model=multinom(Active~First_Purchase+Recency+Avg_Amount+Frequency+Max_Amount+Min_Amount,in_sample)

coef(prob.model)
table(in_sample$Active)
#just to check
table1<-table(in_sample$Active,round(predict(prob.model,customers_2015,type='probs')+.3))
sum(diag(table1))/sum(table1)
table1[4]
#std(prob.model)

#customers who are active in 2016
z=which(in_sample$Active==1)

amount.model<-lm(revenue_2016~First_Purchase+Recency+Avg_Amount+Frequency+Max_Amount+Min_Amount,in_sample[z,])
summary(amount.model)

plot(in_sample[z,]$revenue_2016,amount.model$fitted.values)
#apply log transformation

amount.model2<-lm(formula= log(revenue_2016)~log(Avg_Amount)+log(Frequency)+log(Recency)+log(Max_Amount)+RetailStore,in_sample[z,])
summary(amount.model2)

plot(log(in_sample[z,]$revenue_2016) ,amount.model2$fitted.values,main="Fitted vs Original")
plot(amount.model2$fitted.values,amount.model2$residuals)

customers_2016<-sqldf("SELECT CustomerID,MIN(Days_Since) AS 'First_Purchase',RetailStore,MAX(Days_Since) AS 'Recency',
                      AVG(Cost) as 'Avg_Amount',COUNT(*) AS Frequency, MAX(Cost) AS 'Max_Amount',
                      MIN(Cost) AS 'Min_Amount' FROM data GROUP BY 1 ")


customers_2016$Prob_Predicted<-predict(prob.model,customers_2016,type='probs')
customers_2016$Revenue_Predicted<-exp(predict(amount.model2,customers_2016))
customers_2016$Score<-customers_2016$Prob_Predicted*customers_2016$Revenue_Predicted
summary(customers_2016)

summary(customers_2016$Score) # check the revenue statistics
# customers with score above 100
p<-which(customers_2016$Score>100)
print(length(p))
plot(customers_2016$Score)


# I tried to build recommander engine but not able to build a perfect one

###### Recommander systems   ########


#list1<-colSums(cfdata[,-1])
#rev(sort(list1))

#rm(list=ls())
#library("recommenderlab")
#library(reshape2)
#mydata2<-read.csv("DataRetail_1.csv")
#cfdata<-dcast(mydata2,TransactionID~ProductID)
#affinity.matrix<- as(cfdata,"realRatingMatrix")
#length(cfdata$TransactionID)
#Rec.model<-Recommender(affinity.matrix[1:15000], method = "IBCF",param=list(normalize = "Z-score",method="Cosine"))
#summary(Rec.model)


#recommended.items.5ID31817 <- predict(Rec.model, affinity.matrix["5ID31817",], n=1)
# to display them
#as(recommended.items.5ID31817, "list")
# to obtain the top 3
#recommended.items.5ID31817.top3 <- bestN(recommended.items.5ID31817, n = 3)
# to display them
#as(recommended.items.5ID31817.top3, "list")

