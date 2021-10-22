#load the data and name it df
df <- read.csv("613234amazon_books.csv")

#check for missing values
apply(df, 2, function(x) any(is.na(x)))

#remove the row with missing data
df <- na.omit(df)

#create a dummy variable for HardorPaper
df1 <- mutate(df, HardOrPaperD =recode(HardOrPaper, "P"=0, "H"=1))

#check for multicollinearity
ggpairs(df1, columns = c("HardOrPaperD", "Height", "NumPages", "Weight", "Thick"))

#create standardized values
dfs <- df1 %>%
  mutate(AmazonPriceST = scale(AmazonPrice),
          HeightST = scale(Height), 
          NumPagesST = scale(NumPages), 
          WeightST = scale(Weight),
          ThickST = scale(Thick))

#make the model
model1 <- lm(AmazonPriceST ~ HardOrPaperD + HeightST + NumPagesST + WeightST, data = dfs)

#print the model to a table
wordreg(list(model1),
        file="Regression Table question 2.doc")


