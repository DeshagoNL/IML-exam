#load the data and name it df
df <- read.csv("613234EconomicsJournalSubscriptionData.csv")

#check for missing values
apply(df, 2, function(x) any(is.na(x)))

#check for multicollinearity
ggpairs(df, columns = c("subs", "price", "charpp", "citations", "foundingyear"))

#create standardized values
dfs <- df %>%
  mutate(priceST = scale(price),
         charppST = scale(charpp), 
         citationsST = scale(citations), 
         foundingyearST = scale(foundingyear))

#make graphs with loess curve to check relationship
ggplot(dfs, aes(x=price, y=subs))+geom_point()+geom_smooth(method=loess)
ggplot(dfs, aes(x=charpp, y=subs))+geom_point()+geom_smooth(method=loess)
ggplot(dfs, aes(x=citations, y=subs))+geom_point()+geom_smooth(method=loess)
ggplot(dfs, aes(x=foundingyear, y=subs))+geom_point()+geom_smooth(method=loess)

#check fit without transformations
pricenotrans <- lm(subs ~ priceST, data = dfs)
summary(pricenotrans)

charppnotrans <- lm(subs ~ charppST, data = dfs)
summary(charppnotrans)

citationsnotrans <- lm(subs ~ citations, data=dfs)
summary(citationsnotrans)

foundingyearnotrans <- lm(subs ~ foundingyearST, data=dfs)
summary(foundingyearnotrans)

#check fit with transformations
pricelog <- lm(subs ~ log(price), data = dfs)
summary(pricelog)
pricepoly <- lm(subs ~ poly(priceST, 3), data = dfs)
summary(pricepoly)

charpppoly <- lm(subs ~ poly(charppST, 5), data = dfs)
summary(charpppoly)

citationslog <- lm(subs ~ log(citations), data = dfs)
summary(citationslog)

citationspoly <- lm(subs ~ poly(citationsST, 3), data = dfs)
summary(citationspoly)

foundingyearpoly <- lm(subs ~ poly(foundingyearST, 2), data=dfs)
summary(foundingyearpoly)
#make the model without correcting for non-linear relationships
model1 <- lm(subs ~ priceST + charppST + citationsST + foundingyearST, data = dfs)

#make the model with corrections for non-linear relationships
model2 <- lm(subs ~ poly(priceST, 3) + charppST + poly(citationsST, 3) + poly(foundingyearST, 2), data = dfs)

#print the model to a table
wordreg(list(model1, model2),
        file="Regression Table question 3.doc")