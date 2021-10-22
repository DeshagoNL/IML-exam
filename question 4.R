#load the data and name it df
df <- read.csv("613234EcommerceCustomers.csv")

#check for missing values
apply(df, 2, function(x) any(is.na(x)))

#check for multicollinearity
ggpairs(df, columns = c("AvgSessionLength", "TimeonApp", "TimeonWebsite", "LengthofMembership"))

#make the model and check the R-squared to see if there is a good linear relationship
model1 <- lm(YearlyAmountSpent ~ AvgSessionLength + TimeonApp + TimeonWebsite + LengthofMembership, data = df)
summary(model1)

#TimeonWebsite has no statistical significance. Therefore, we test if the r-squared changes if we remove it from the model 
model2 <- lm(YearlyAmountSpent ~ AvgSessionLength + TimeonApp + LengthofMembership, data = df)
summary(model2)

#it doesn't changed the r-squared, so we continue with the simplified model and print that to a table
wordreg(list(model2),
        file="Regression Table question 4.doc")