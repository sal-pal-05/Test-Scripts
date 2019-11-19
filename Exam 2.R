##Exam 2

library(nutshell)
library(stringr)
##load test2.Rdata
load(file="test2_data_rev.Rdata")
x=t2$date_chr

date=x
mm=str_sub(string=date, start=1, end=2)
mm
dd=str_sub(string=date, start=4, end=5)
dd

yy=str_sub(string=date, start=7, end=10)
yy

fulldate=str_c(yy,mm,dd, sep="/")
fulldate=as.character(fulldate)
fulldate




##section 2

library(data.table)
library(tidyverse)
library(readxl)
load(file="aurelia_15minCell_statareas.Rdata")
read_excel("Aurelia_SEAMAP_2012-2018_30minCell.xlsx")
##(data.table)
library(data.table)
d1= fread(input="aurelia_15minCell_statareas.txt", sep=",", header= T, stringsAsFactors=F)

d2=read.csv(file="aurelia_15minCell_statareas.txt", header=T, sep=",", stringsAsFactors=F)

d3=read.table (file="aurelia_15minCell_statareas.txt", sep=",", header=T, stringsAsFactors = F)

d4=read_csv(file="aurelia_15minCell_statareas.txt") 

x=d4[d4$year=="2012",]
x

subset1=subset(x=d4, year=="2012")
subset1
save(subset1, file="newaurelia_data.Rdata")


##Section 3
load(file="test2_deep.Rdata")
load(file="test2_mid.Rdata")
load(file="test2_shallow.Rdata")
shallow.df=as.data.frame(shallow)
deep.df=as.data.frame(deep)
mid.df=as.data.frame(mid)
mid.df
##put all three df's back together
bound=rbind(shallow.df,deep.df,mid.df)
bound


load(file="t2-1.Rdata")
load(file="t2-2.Rdata")

t2_1.df=as.data.frame(t2)
t2_2.df=as.data.frame(t2.1)
##join
join1=full_join(t2_1.df, t2_2.df, by = "parcel.id")
join1



##merge
merge1=merge(t2_1.df,t2_1.df,by="parcel.id")
merge1




##Section 4

bat= data("batting.2008")

bat=batting.2008
HR=tapply(X=bat$HR, INDEX= list(bat$teamID), FUN = sum)
HR

stats.team=aggregate(x=bat [,c("AB", "H","BB","2B","HR")], by=list(bat$teamID),FUN=sum)
stats.team

sum.team = bat %>%  group_by(teamID) %>% summarize (HRsum = sum(HR), HRmean=mean(HR),HRsd=sd(HR))
sum.team


##bonus
library(reshape2)








