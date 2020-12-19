library(dplyr)
library(ggplot2)
library(e1071)
library(MASS)
library(tidyr)

vgSales.df <- read.csv("vgsales.csv")
vgSalesRegion <- vgSales.df %>% gather(Region, Revenue, 7:10) 
vgSalesRegion$Region <- factor(vgSalesRegion$Region)

by_regions <- vgSalesRegion %>% 
  group_by(Region) %>%
  summarize(ToalGlobalSales = sum(Global_Sales), Percentage = ToalGlobalSales/sum(vgSalesRegion$Revenue) * 100) %>%
  arrange(desc(ToalGlobalSales))

ggplot(by_regions, aes(x=as.factor(Region), ToalGlobalSales, fill = Region)) + 
  geom_bar(stat = "identity") +
  ggtitle("Sales by region") +
  theme(legend.position = "top")
