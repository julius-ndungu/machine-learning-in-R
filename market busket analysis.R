# Market basket analysis is the study of items that are 
# purchased (or otherwise grouped) together in a single 
# transaction or multiple, sequential transactions. 
# Market Basket Analysis is a modelling technique based 
# upon the theory that if you buy a certain group of items, 
# you are more (or less) likely to buy another group of items.
# 

# importing the dataset
library(arules)
library(arulesViz)

df <- read.csv("basket_analysis.csv", colClasses = "factor")
df <- df[,-1]
head(df)
str(df)
# Apriori algorithm
#default 2757 rules
rule_default <- apriori(df)
#reducing the number of rules
rules <- apriori(df,
             parameter = list(minlen = 2,
                              maxlen = 5,
                              supp = .1,
                              conf = .5),
             appearance = list(none = c("Apple=False","Bread=False","Butter=False",
                                        "Cheese=False","Corn=False","Dill=False","Eggs=False","Ice.cream=False",
                                        "Kidney.Beans=False", "Milk=False","Nutmeg=False",     
                                        "Onion=False","Sugar=False","Unicorn=False","Yogurt=False","chocolate=False" )))
# The rules

# Support is the fraction of transactions in the 
# dataset that contain the item or item set
#  Confidence is the proportion of times the 
# customer has taken the item Y given she has 
# also taken X
# Lift is ratio of Confidence of the Rule divided by 
# support of Product Y alone


inspect(rules)
# checking for redundancy
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
rules_pruned <-rules[!redundant]
inspect(rules_pruned)
# te 1st 10rules ordered by lift

inspect(sort(rules, by = "lift")[1:10])

# visualizing the rules
plot(rules_pruned)
plot(rules_pruned, method = "graph")
inspectDT(rules_pruned)
## shiny exploration
ruleExplorer(rules_pruned)
