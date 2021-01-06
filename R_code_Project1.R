library(readxl)


setwd("C:/Users/abheer/Desktop/Data science/Marketing & Retail Analytics/Datasets")

mktbasket <- read.csv("ccn.csv", header = TRUE)

mktbasket

# Making the invoice.no column into factor
mktbasket$Bill.Number <- as.factor(mktbasket$Bill.Number)
print(mktbasket)


# Arranging the rows such that each row has unique invoice number 
# along with all the products in the invoce 
mktbasket.Agg <- split(mktbasket$Item.Desc, mktbasket$Bill.Number)
head(mktbasket.Agg)

# Removing duplicate items in any invoice number 
mktbasket.Agg2 = list()
for (i in 1:length(mktbasket.Agg)) {
  mktbasket.Agg2[[i]] = unique(mktbasket.Agg[[i]])
}

mktbasket.Agg2

library(arules)

# Converting the data into transactions datastructure 
# This datastructure is availble in arules package 

Txns <- as(mktbasket.Agg2, "transactions")
summary(Txns)
inspect(Txns[10])

# Getting the freq of certian items 

freq <- itemFrequency(Txns)
head(freq)
freq <- freq[order(-freq)]
freq["CAPPUCCINO"]

barplot(freq[1:20])

itemFrequencyPlot(Txns, support = .001)
itemFrequencyPlot(Txns, type = c("relative","absolute") 
                  weighted = FALSE ,topN = 10)
?itemFrequencyPlot

arules = apriori(Txns, parameter = list(
  supp = 0.0005, conf = 0.2)
    )
inspect(sort(arules, by="lift"))

mba <- inspect(sort(arules, by="lift"))
rules.df = as(mba, "data.frame")


# PLoting the graph using aruleviz package 
library(arulesViz)
library(RColorBrewer)
plot(arules, control = list(col=brewer.pal(11, "Spectral")))

subrules <- head(sort(arules, by = "support"), 20)
inspect(subrules)
plot(subrules, method = "graph")

rules1.df = as(arules, "data.frame")

# Prob a(LHS support)
rules1.df$LHS_support = rules1.df$support/rules1.df$confidence

# Prob b(RHS support)
rules1.df$RHS_support = rules1.df$confidence/rules1.df$lift


print(rules1.df)
write.table(rules1.df, file = "mba_output1.csv", sep = ",",
            append = FALSE, row.names = FALSE)












