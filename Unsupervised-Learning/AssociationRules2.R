#install.packages("arules")
#install.packages("RColorBrewer")
library(arules)
library(arulesViz)
library(RColorBrewer)

trans1<-read.transactions("C:/Users/duzzi/Documents/University_Projects/Unsupervised-Learning/data/survey_cleaned.csv", format="basket", sep=",", skip=0, header=TRUE) # reading the file as transactions
trans1
inspect(trans1)
size(trans1)
length(trans1)

rules <- apriori(trans1,supp=0.05)
rules.byconf<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)



# What is the profile of the people having treatment for mental health issues?
rules.treatment<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="treatment Yes"), control=list(verbose=F))
rules.treatment.byconf<-sort(rules.treatment, by="confidence", decreasing=TRUE)
inspect(head(rules.treatment.byconf))

plot(rules.treatment.byconf, method= "graph")

# What is the profile of the people having no treatment for mental health issues?
rules.notreatment<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="treatment No"), control=list(verbose=F))
rules.notreatment.byconf<-sort(rules.notreatment, by="confidence", decreasing=TRUE)
inspect(head(rules.notreatment.byconf))

plot(rules.notreatment.byconf)

# What is associated with often having work interference because of mental health conditions??
rules.interference<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="work_interfere Often"), control=list(verbose=F))
rules.interference.byconf<-sort(rules.interference, by="confidence", decreasing=TRUE)
inspect(head(rules.interference.byconf))

plot(rules.interference.byconf)
plot(rules.interference.byconf, method= "graph")

# What is associated with never having work interference because of mental health conditions??
rules.interference<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="work_interfere Never"), control=list(verbose=F))
rules.interference.byconf<-sort(rules.interference, by="confidence", decreasing=TRUE)
inspect(head(rules.interference.byconf))

plot(rules.interference.byconf)
plot(rules.interference.byconf, method= "graph")

# What leads to work interference?
rules.history<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="family_history Yes"), control=list(verbose=F))
rules.history.byconf<-sort(rules.history, by="confidence", decreasing=TRUE)
inspect(head(rules.history.byconf))

# What leads to work interference?
rules.anonymity<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="anonymity Yes"), control=list(verbose=F))
rules.anonymity.byconf<-sort(rules.anonymity, by="confidence", decreasing=TRUE)
inspect(head(rules.anonymity.byconf))

# What leads to work leave?
rules.leave<-apriori(data=trans1, parameter=list(supp=0.05, conf=0.08), appearance=list(default="lhs", rhs="leave Very easy"), control=list(verbose=F))
rules.leave.byconf<-sort(rules.leave, by="confidence", decreasing=TRUE)
inspect(head(rules.leave.byconf))


# using itemFrequencyPlot() function
arules::itemFrequencyPlot(trans1, topN = 20,
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")


