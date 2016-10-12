library(ggplot2)
setwd("D:/input/Rossmann")


train <- read.table("train.csv",header=T,sep=",")
test <- read.table("test.csv",header=T,sep=",")
store <- read.table("store.csv",header=T,sep=",")

new.train <- train
new.train$Promo <- as.factor(new.train$Promo)
new.train$Open <- as.factor(new.train$Open)
new.train$DayOfWeek <- as.factor(new.train$DayOfWeek)
new.train$SchoolHoliday <- as.factor(new.train$SchoolHoliday)


new.train <- merge(new.train,store)
test <- merge(test,store)

new.train$Month <- as.factor(format(as.Date(new.train$Date),"%m"))
new.train$Day <- as.factor(format(as.Date(new.train$Date),"%d"))
new.train$Year <- as.factor(format(as.Date(new.train$Date),"%Y"))


sales.by.store.df <- aggregate(new.train$Sales,by = list(new.train$Store),mean)
names(sales.by.store.df) <- c("Store","Average.Sales")
ggplot2(data=sales.by.store.df,aes(x=Store,y=Average.Sales)) + geom_point(color="blue") + 
  ggtitle("Average sales by store id")

sales.by.day.df <- aggregate(new.train$Sales,by = list(new.train$DayOfWeek),mean)
names(sales.by.day.df) <- c("DayOfWeek","Average.Sales")
ggplot(data=sales.by.day.df,aes(x=DayOfWeek,y=Average.Sales,fill=DayOfWeek)) +
  geom_bar(stat="identity") + ggtitle("Average sales by day of the week")

ggplot(data=new.train,aes(x=Customers,y=Sales)) + geom_point(color="purple") + ggtitle("Sales Vs No. of Customers")

sales.by.promo.df <- aggregate(new.train$Sales,by = list(new.train$Promo),function(x){mean(as.numeric(x))})
names(sales.by.promo.df) <- c("Promo","Average.Sales")
ggplot(data=sales.by.promo.df,aes(x=Promo,y=Average.Sales,fill=(as.integer(sales.by.promo.df$Promo)+1))) + 
  geom_bar(stat="identity") + ggtitle("Average Sales by promo")

sales.by.stateH.df <- aggregate(new.train$Sales,by = list(new.train$StateHoliday),function(x){mean(as.numeric(x))})
sales.by.stateH.df <- sales.by.stateH.df[-1,]
names(sales.by.stateH.df) <- c("StateHoliday","Average.Sales")
ggplot(data=sales.by.stateH.df,aes(x=StateHoliday,y=Average.Sales,fill=c("blue","red","green"))) + 
  geom_bar(stat="identity") + ggtitle("Average sales by state holiday")

sales.by.schoolH.df <- aggregate(new.train$Sales,by = list(new.train$SchoolHoliday),function(x){mean(as.numeric(x))})
names(sales.by.schoolH.df) <- c("SchoolHoliday","Average.Sales")
ggplot(data=sales.by.schoolH.df,aes(x=SchoolHoliday,y=Average.Sales,fill=SchoolHoliday)) + geom_bar(stat="identity") +
  ggtitle("Average sales by school holiday")

sales.by.storeType.df <- aggregate(new.train$Sales,by = list(new.train$StoreType),function(x){mean(as.numeric(x))})
names(sales.by.storeType.df) <- c("Store.Type","Average.Sales")
ggplot(data=sales.by.storeType.df,aes(x=Store.Type,y=Average.Sales,fill=Store.Type)) + geom_bar(stat="identity") +
  ggtitle("Average Sales by store type")

sales.by.assortment.df <- aggregate(new.train$Sales,by = list(new.train$Assortment),function(x){mean(as.numeric(x))})
names(sales.by.assortment.df) <- c("Assortment","Average.Sales")
ggplot(data=sales.by.assortment.df,aes(x=Assortment,y=Average.Sales,fill=Assortment)) + geom_bar(stat="identity") +
  ggtitle("Average Sales by assortment type")

sales.by.distance.df <- aggregate(new.train$Sales,by = list(new.train$CompetitionDistance),mean)
names(sales.by.distance.df) <- c("CompDistance","Average.Sales")
ggplot(data=sales.by.distance.df,aes(x=CompDistance,y=Average.Sales)) + geom_point(color="blue") + 
  ggtitle("Average sales by Competition Distance")

sales.by.month.df <- aggregate(new.train$Sales,by = list(new.train$Month),mean)
names(sales.by.month.df) <- c("Month","Average.Sales")
ggplot(data=sales.by.month.df,aes(x=Month,y=Average.Sales,fill=Month)) + geom_bar(stat="identity") + 
  ggtitle("Average Sales by Month")

sales.by.date.df <- aggregate(new.train$Sales,by = list(new.train$Day),mean)
names(sales.by.date.df) <- c("Date","Average.Sales")
ggplot(data=sales.by.date.df,aes(x=Date,y=Average.Sales,fill=Date)) + geom_bar(stat="identity") + 
  ggtitle("Average sales by Date")

sales.by.year.df <- aggregate(new.train$Sales,by = list(new.train$Year),mean)
names(sales.by.year.df) <- c("Year","Average.Sales")
ggplot(data=sales.by.year.df,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") + 
  ggtitle("Average sales by Year")

sales.by.monthDay.df <- aggregate(new.train$Sales,by=list(new.train$Month,new.train$Day),mean)
names(sales.by.monthDay.df) <- c("Month","Date","Average.Sales")
ggplot(data=sales.by.monthDay.df,aes(Month,Average.Sales,fill=Date)) + geom_bar(stat="identity") + facet_wrap(~Date) +
  ggtitle("Sales per date by month")

sales.by.storeP.df <- aggregate(new.train$Sales,by = list(new.train$Store,new.train$Promo),mean)
names(sales.by.storeP.df) <- c("Store","isPromo","Average.Sales")
ggplot(data=sales.by.storeP.df,aes(Store,Average.Sales,color=isPromo)) + geom_point() +
  ggtitle("Average Sales of each store by promo")


sales.by.dayP.df <- aggregate(new.train$Sales,by = list(new.train$DayOfWeek,new.train$Promo),mean)
names(sales.by.dayP.df) <- c("DayOfWeek","isPromo","Average.Sales")
ggplot(data=sales.by.dayP.df,aes(x=DayOfWeek,y=Average.Sales,fill=isPromo)) +
  geom_bar(stat="identity",position = "dodge") + ggtitle("Average sales by Promo within day of the week")

sales.by.storeTypeA.df <- aggregate(new.train$Sales,by = list(new.train$StoreType,new.train$Assortment),
                                    function(x){mean(as.numeric(x))})
names(sales.by.storeTypeA.df) <- c("Store.Type","Assortment","Average.Sales")
ggplot(data=sales.by.storeTypeA.df,aes(x=Store.Type,y=Average.Sales,fill=Assortment)) + 
  geom_bar(stat="identity",position = "dodge") + ggtitle("Average Sales by Assortment within store type")