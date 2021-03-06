library(dplyr)
library(ggplot2)
library(e1071)
library(MASS)
library(tidyr)
library(caret)
library(forecast)

vgSales.df <- read.csv("vgsales.csv")
#View(vgSales.df)
#plot(vgSales.df)
summary(vgSales.df)             # Find summary stats for each column
#vgSales.df[vgSales.df == "N/A"] <- NA
#vgSales.df <- na.omit(vgSales.df)
#attach(vgSales.df)
vgSales.df <- vgSales.df[!(vgSales.df$Year %in% c("N/A", "2017", "2020")),]

vgSales.df$Name <- as.factor(as.character(vgSales.df$Name))
vgSales.df$Platform <- as.factor(as.character(vgSales.df$Platform))
vgSales.df$Year <- as.numeric(as.character(vgSales.df$Year))
vgSales.df$Genre <- as.factor(as.character(vgSales.df$Genre))
vgSales.df$Publisher <- as.factor(as.character(vgSales.df$Publisher))
max(vgSales.df$Year, na.rm=TRUE)

#Histogram of frequency of game releases by year
hist(vgSales.df$Year, col = "red", xlab = "Year", ylab = "Frequency", main = "Frequency of game releases by year")

#Bar chart of Global sales by year
aggregateRevenue <- aggregate(Global_Sales~Year, vgSales.df, sum)
plot(aggregateRevenue, type = 'h', xlab="Year", ylab = "Global Sales", col = "green", lwd = 8, main = "Global Sales per year")

#Bar chart of Global sales of games by genre
revenueByGenre <- aggregate(Global_Sales~Genre, vgSales.df, sum)
arrangeByGenre <- arrange(revenueByGenre, desc(Global_Sales))
arrangeByGenre$Genre <- factor(arrangeByGenre$Genre, levels = arrangeByGenre$Genre)
ggplot(arrangeByGenre, aes(Genre, Global_Sales)) + geom_bar(fill="red",stat = "identity") + ggtitle("Global Sales by Genre")

#Bar chart of Global sales of games by platform
revenueByPlatform <- aggregate(Global_Sales~Platform, vgSales.df, sum)
arrangeByPlatform <- arrange(revenueByPlatform, desc(Global_Sales))
arrangeByPlatform$Platform <- factor(arrangeByPlatform$Platform, levels = arrangeByPlatform$Platform)
ggplot(arrangeByPlatform, aes(Platform, Global_Sales)) + geom_bar(fill="blue",stat = "identity") + ggtitle("Global Sales by Platform")

#Bar chart of Global sales of games by top 10 Publishers 
revenueByPublisher <- aggregate(Global_Sales~Publisher, vgSales.df, sum)
arrangeRevenueByPublisherByGlobalSales <- arrange(revenueByPublisher,desc(Global_Sales))
top10=arrangeRevenueByPublisherByGlobalSales[1:10,]
ggplot(top10,aes(Publisher,Global_Sales, fill=Publisher))+ geom_bar(stat = "identity") + ggtitle("Top 10 Publishers by Global Revenue") + theme(legend.position = "top")

#Top publisher by revenue each year
arrangeByYearAndPublisher <- vgSales.df %>% group_by(Year, Publisher) %>% summarize(ToalGlobalSales = sum(Global_Sales)) %>% arrange(desc(ToalGlobalSales)) %>% top_n(1)
ggplot(arrangeByYearAndPublisher, aes(x=as.factor(Year), ToalGlobalSales, fill=Publisher)) + geom_bar(stat = "identity") + ggtitle("Top publisher by revenue each year") + theme(legend.position = "top")

#Top genre by revenue each year
arrangeByYearAndGenre <- vgSales.df %>% group_by(Year, Genre) %>% summarize(ToalGlobalSales = sum(Global_Sales)) %>% arrange(desc(ToalGlobalSales)) %>% top_n(1)
ggplot(arrangeByYearAndGenre, aes(x=as.factor(Year), ToalGlobalSales, fill=Genre)) + geom_bar(stat = "identity") + ggtitle("Top Genre by revenue each year") + theme(legend.position = "top")

#Top platform by revenue each year
arrangeByYearAndPlatform <- vgSales.df %>% group_by(Year, Platform) %>% summarize(ToalGlobalSales = sum(Global_Sales)) %>% arrange(desc(ToalGlobalSales)) %>% top_n(1)
ggplot(arrangeByYearAndPlatform, aes(x=as.factor(Year), ToalGlobalSales, fill=Platform)) + geom_bar(stat = "identity") + ggtitle("Top Platform by revenue each year") + theme(legend.position = "top")

#Sales by region 
vgSalesRegion <- vgSales.df %>% gather(Region, Revenue, 7:10) 
vgSalesRegion$Region <- factor(vgSalesRegion$Region)

by_regions <- vgSalesRegion %>%
  group_by(Region) %>%
  summarize(ToalGlobalSales = (sum(Revenue)/sum(Global_Sales))*100.0) %>%
  arrange(desc(ToalGlobalSales))

ggplot(by_regions, aes(x=as.factor(Region), ToalGlobalSales, fill = Region)) + 
  geom_bar(stat = "identity") +
  ggtitle("Sales by region") +
  theme(legend.position = "top")

#Correlation of sales among regions
regionSales <- vgSales.df[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")]
cor(regionSales)

#Linear regression of Genre, Platform and Publisher against Global_Sales
#Partition data
selected.var <- c(3, 4, 5, 11)
size <- dim(vgSales.df)[1]
size
set.seed(1)
train.index <- sample(size, size*0.6)
train.df <- vgSales.df[train.index, selected.var]
valid.df <- vgSales.df[-train.index, selected.var]

vgSales.lm <- lm(Global_Sales ~ ., data=train.df)
summary(vgSales.lm)

#Model Prediction and accuracy
#Training set
vgSales.lm.pred <- predict(vgSales.lm, train.df)
accuracy(vgSales.lm.pred, train.df$Global_Sales)
#Validation set (Error here)
vgSales.lm.pred <- predict(vgSales.lm, valid.df)
accuracy(vgSales.lm.pred, valid.df$Global_Sales)

