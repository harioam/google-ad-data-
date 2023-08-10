
---jrt assignment
setwd("C:/hariom datas from aban r")
getwd()
salesdata=read.csv("Dummy Sales Ledger for Insight.csv")
View(insightdata)
view(salesdata)
library(dplyr)
summary(salesdata)
head(salesdata)
colnames(salesdata)
table(sum(is.na(salesdata)))
apply(salesdata,2,function (salesdata)sum(is.na(salesdata)))
glimpse(salesdata)
summary(salesdata$GROSS_AMT)
table(salesdata$CGST)
salesdata$CGST_AMT[is.na(salesdata$CGST_AMT)]=round(0.06*salesdata$GROSS_AMT,2)
salesdata$IS_FREE[is.na(salesdata$IS_FREE)]= "NotFree"
sum(is.na(salesdata$IS_FREE))
salesdata$`SALE RETURN QTY`[is.na(salesdata$`SALE RETURN QTY`)]=FALSE
table(salesdata$SGST)
salesdata$SGST_AMT[is.na(salesdata$SGST_AMT)]=0.06*salesdata$TAXABLE_AMT
sum(is.na(salesdata$SGST_AMT))
p=ggplot(salesdata,aes(x=Category_Name,y=MRP))
p+geom_point()
p+geom_histogram()
mean(salesdata$MRP,salesdata$Category_Name="Non_Veg")
total_revenue=sum(salesdata$TOTAL_VALUE)
total_qty_sales=sum(salesdata$`QTY purchased in Pieces`)
average_price=mean(salesdata$MRP)
salesdata$discount=salesdata$MRP-salesdata$RATE
average_discount=mean(salesdata$discount)
boxplot(salesdata$discount)
salesdata[,c("MRP","TAXABLE_AMT","TOTAL_VALUE","total_tax")]
salesdata$total_tax=salesdata$TOTAL_VALUE-salesdata$TAXABLE_AMT
describe(salesdata)
hist(salesdata$discount) 
tab1=table(salesdata$ZONE,salesdata$total_tax)
boxplot(tab1)
logical(10)
x1=list(x=c(1,2,3))
x2=list(x=c(2,3,4))
x1+x2
all(c(1==TRUE,0==FALSE,logical(10)))
x=c(1,2,10,5,5,6,11,-18,9)
a=sum((x<0)*2+(X>=0&x<=10)*3+(x>10)*4)
a
l=list(c(1,2,3))
nchar(l)
x=123456
y=floor((log10(x))+1
y
y        
x="Rajesh Chauhan"
strsplit(x,split=NULL)
y=median(x=c(1,2,3,4,5))
y
for(salesdata$Category_Name in Veg){
  print(mean(salesdata$MRP))
}
for(salesdata$SALE.RETURN.QTY in NA){
  print(mean(salesdata$SALE.RETURN.QTY))
}
summary(salesdata)
 nrow(salesdata[(salesdata$cgst>6)])
 p=(salesdata[(salesdata$cgst>6),]) 
library(dplyr)
 
 lp 
View(p)
nrow(salesdata[salesdata$SALE.RETURN.QTY==3,])
q=(salesdata[salesdata$SALE.RETURN.QTY==3],)
View(q)
quantile(salesdata$MRP)
quantile(salesdata$discount)
library(ggplot2)
m=ggplot(salesdata,aes(salesdata$discount,salesdata$TOTAL_VALUE)+ geom_line(color="red")
m       
quantile(salesdata$MRP)       
range(salesdata$SGST_AMT)
range(salesdata$GROSS_AMT)
ggplot(salesdata,aes(salesdata$GROSS_AMT,salesdata$RATE)+ geom_line()
names(salesdata)   
hist(salesdata$RATE)
hist(salesdata$SALE.RETURN.QTY)
hist(salesdata$discount)
salesdata$SALE.RETURN.QTY
table(salesdata$SALE.RETURN.QTY)
hist(salesdata$SALE.RETURN.QTY,salesdata$QTY.purchased.in.Pieces)
hist(salesdata$QTY.purchased.in.Pieces)
max(salesdata$MRP)
salesdata["rate","max"]
hist(salesdata$QTY.purchased.in.Pieces,breaks = 200)
help(hist)
hist(salesdata$RATE,breaks = 15,col = "grey",border = "red")
boxplot(salesdata$QTY.purchased.in.Pieces,col="red",border = "green")
boxplot(salesdata$RATE)
boxplot(salesdata$TOTAL_VALUE,salesdata$discount)
boxplot(salesdata$total_tax,salesdata$discount,salesdata$TOTAL_VALUE,salesdata$RATE)
table(salesdata)
tab1=table(salesdata)
tab1
tab2=table(salesdata$SALE.RETURN.QTY)
tab2
tab3=table(salesdata$total_tax)
tab3
table(salesdata$discount,salesdata$SALE.RETURN.QTY)
-----
  AMAZON ARAMBAGH FOOD MART BIG BASKET Big Bazar EASY DAY
Non_Veg     15                513         78       250      127
Veg          7                293         25        50       78

FUTURE RETAIL- BIG BAZAAR   GT MORE RETAIL NATURE’S BASKET
Non_Veg                        28 2860         357              14
Veg                             6 1029         309               4

SPENCER’S
Non_Veg       748
Veg           403
######---
tab4=prop.table(salesdata$ZONE,salesdata$Category_Name,salesdata$IS_FREE)
tab4
prop.table(tab4)
Non_Veg  Veg
East Zone     1502  934
North Zone    1312  648
South Zone     513  134
West Zone      975  273

, ,  = Free


Non_Veg  Veg
East Zone      109   64
North Zone     206   74
South Zone     160   47
West Zone      213   30
######--
prop.table(tab4,1)
prop.table(tab4,2)
margin.table(tab4)
margin.table(tab4,1)
margin.table(tab4,2)
addmargins(tab4,2)
tabx=xtabs(~zone,data = salesdata)
