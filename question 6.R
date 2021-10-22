#load the data and name it df
df <- read.csv("613234graduate_earnings.csv")

#check for missing values
apply(df, 2, function(x) any(is.na(x)))

#for the first dataset we remove the missing values
df1 <- na.omit(df)

#create second dataset to manipulate so our original dataset remains the same
df2 <- df

#create dummies for the catagorical variable
df2 <- dummy_cols(df2,
                 select_columns = c("Public"))

#replace NA's with 0's
df2 <- mutate_at(df2, c("Public_0", "Public_1"), ~replace(., is.na(.), 0))

#create missing data dummies for continuous variables
df2$SAT_NA <- ifelse(is.na(df2$SAT),1,0)
df2$need_fraction_NA <- ifelse(is.na(df2$need_fraction),1,0)

#fill missing values of the continuous variables with mean
df2$SAT[is.na(df2$SAT)] <- mean(df2$SAT, na.rm=TRUE)
df2$need_fraction[is.na(df2$need_fraction)] <- mean(df2$need_fraction, na.rm=TRUE)

#check for multicollinearity
ggpairs(df1, columns = c("Public", "SAT", "Price", "need_fraction"))

#there might be multicollinearity between price and public. Run the model with and without public to see if there might be problems
model1 <- lm(Earn ~ Public + SAT + Price + need_fraction, data=df1)
summary(model1)

model2 <- lm(Earn ~ SAT + Price + need_fraction, data=df1)
summary(model2)

#check if the missing values are non-random
model1 <- lm(Public_NA ~ SAT + Price + need_fraction + Earn, data=df2)
summary(model1)

model2 <- lm(SAT_NA ~ Public_0 + Price + need_fraction + Earn, data=df2)
summary(model2)

model3 <- lm(need_fraction_NA ~ Public_0 + Price + SAT + Earn, data=df2)
summary(model3)

# make the models with missing values removed and missing values replaced
model1 <- lm(Earn ~ Public + SAT + Price + need_fraction, data=df1)

model2 <- lm(Earn ~ Public_1 + SAT + Price + need_fraction, data=df2)

#output these models to word so they can be compared
wordreg(list(model1, model2),
        file="Regression Table question 6.doc")
