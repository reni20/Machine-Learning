library (arules)
library (arulesViz)
library (grid)
#Splitting the data :

dt <- split(mydata$product_name, mydata$order_id)
# Converting data to a class of transactions
dt2 = as(dt,"transactions")
itemFrequencyPlot(dt2,topN=20,type="absolute")
# What are customers likely to buy before they purchase "Banana" 
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), appearance = list(default="lhs",rhs="Banana"),control = list(verbose=F)
#Plot graph-based visualisation:

subrules2 <- head(sort(rules, by="lift"), 10)

plot(subrules2, method="graph",control=list(type="items",main=""))
