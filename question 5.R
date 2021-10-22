#load the data and name it df
df <- read.csv("613234Philadelphia_Crime_Rate.csv")

#check for missing values
apply(df, 2, function(x) any(is.na(x)))

#remove the row with missing data
df1 <- na.omit(df)

#check for multicollinearity
ggpairs(df1, columns = c("HousePrice", "MilesPhila", "PopChg"))

## we check for outliers using cook's distance
#run the regression model with all the cases in it first
model1 <- lm(CrimeRate ~ HousePrice + MilesPhila + PopChg, data=df1)
summary(model1)

#define CooksD as cook’s distance for each case
CooksD <- cooks.distance(model1)

#define n as the number of rows in a dataset*
n <- nrow(df1)

#print all row numbers that have a cook’s D larger than 4/n
names(CooksD)[(CooksD > (4/n))]

#Store the outliers in an object (you need to make the values numeric first)
Outliers <- as.numeric(names(CooksD)[(CooksD > (4/n))])

#Remove the outliers from the data
df2 <- df1[-c(Outliers),]

#run this model to see what the difference is in fit
model2 <- lm(CrimeRate ~ HousePrice + MilesPhila + PopChg, data=df2)
summary(model2)

#to show the relative effect of house prices, all the variables are scaled in a seperate model
model3 <- lm(CrimeRate ~ scale(HousePrice) + scale(MilesPhila) + scale(PopChg), data=df2)
summary(model3)

#the r-squared more than doubles, so the fit is a lot better without outliers. 
#Therefore, we will use the model without outliers. We will print all models however, to better show the difference.
wordreg(list(model1, model2, model3),
        file="Regression Table question 5.doc")
