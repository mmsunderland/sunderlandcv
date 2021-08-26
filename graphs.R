library(tidyverse)

scopusdata<-read.csv("scopusdata.csv")

yearsum <- scopusdata %>%
  count(Year, name="articles")

sumgrants <- grants %>%
  pull(Amount)%>%
  sum(na.rm=TRUE)

citesum<- scopusdata %>%
  group_by(Year)%>%
  tally(Cited.by, name="cites")

yearmerg <-left_join(yearsum, citesum, by="Year")

plot1<-ggplot(yearsum, aes(x=Year, y=n))  + 
  geom_bar(stat="identity",colour="#535353", fill="#84D5F0") +
  xlab("Year") + ylab("Number of Articles")  + 
  ggtitle("Citation Data for Matthew Sunderland")+
  scale_x_continuous(breaks = seq(2007, 2021, 1),
                     limits=c(2006, 2022)) +
  geom_vline(xintercept = 2016, size = 1, colour = "#535353",
             linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, span=0.4) 
plot1

plot2<-ggplot(yearmerg, aes(x=Year, y=cites))  + 
  geom_bar(stat="identity",colour="#535353", fill="#84D5F0") +
  xlab("Year") + ylab("Number of Citations")  + 
  ggtitle("Citation Data for Matthew Sunderland")+
  scale_x_continuous(breaks = seq(2007, 2021, 1),
                     limits=c(2006, 2022)) +
  geom_vline(xintercept = 2016, size = 1, colour = "#535353",
             linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, span=0.4) 
plot2


#Plot by year
plot1 + facet_grid(Year ~ .)
