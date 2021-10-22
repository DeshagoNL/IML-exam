#load the data and name it df
df <- read.csv("613234Student_Alcohol_Consumption.csv")

#create dummies for the categorial variables
df <- mutate(df, address_Dummy =recode(address, "U"=0, "R"=1))
df <- mutate(df, romantic_Dummy =recode(romantic, "no"=0, "yes"=1))
df <- mutate(df, activities_Dummy =recode(activities, "no"=0, "yes"=1))

#check for missing values
apply(df, 2, function(x) any(is.na(x)))

#check for multicollinearity
ggpairs(df, columns = c("health", "address_Dummy", "age", "romantic_Dummy", "activities_Dummy"))

#make the model where all the students are included
model1 <- lm(Alcohol ~ health + address_Dummy + scale(age) + romantic_Dummy + activities_Dummy, data = df)

#make the model where only males are included
model2 <- lm(Alcohol ~ health + address_Dummy + scale(age) + romantic_Dummy + activities_Dummy, data = subset(df, sex == "M"))

#make the model where only females are included
model3 <- lm(Alcohol ~ health + address_Dummy + scale(age) + romantic_Dummy + activities_Dummy, data = subset(df, sex == "F"))

#print the model to a table
wordreg(list(model1, model2, model3),
        file="Regression Table question 1.doc")

#calculate average alcohol consumption 
mean(subset(df$Alcohol, df$sex == "M"))
mean(subset(df$Alcohol, df$sex == "F"))