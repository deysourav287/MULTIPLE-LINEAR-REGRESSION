sales <- read.csv('"C:/Users/sourav/Desktop/sales/revenue_line_list_data.csv')
 
#Gives certain statistical information about the data.
summary(sales) 

# Displays the dimensions of the dataset
dim(sales) 

# Plot the variables to see their trends
plot(sales) 
library(corrplot) # Library to finds the correlation between the variables
num.cols<-sapply(sales, is.numeric)
num.cols
cor.data<-cor(sales[,num.cols])
cor.data
corrplot(cor.data, method='color')

#Split the data into training and testing
set.seed(2)
library(caTools) #caTools has the split function 

# Assigning it to a variable split, sample.split is one of the functions we are using. With the ration value of 0.7, it states that we will have 70% of the sales data for training and 30% for testing the model
split <- sample.split(sales, SplitRatio = 0.7) 
split
train <- subset(sales, split = 'TRUE') #Creating a training set 
test <- subset(sales, split = 'FALSE') #Creating a testing set by assigning FALSE
head(train)
head(test)
View(train)
View(test)

#Creates the model. Here, lm stands for the linear regression model. Revenue is the target variable we want to track.
Model <- lm(Revenue ~., data = train) 
summary(Model) 
# Prediction
#The test data was kept for this purpose
pred <- predict(Model, test) #This displays the predicted values
pred  

# Find the residuals
res<-residuals(Model) 

# Convert the residual into a dataframe
res<-as.data.frame(res) 

# Prints the residuals
res 

# compare the predicted vs actual values
results<-cbind(pred,test$Revenue)
results
colnames(results)<-c('predicted','real')
results<-as.data.frame(results)
head(results)

#now, compare the predicted vs actual values
plot(test$Revenue, type = 'l', lty = 1.8, col = "red")

#Now let’s plot our test revenue with the following command
lines(pred, type = "l", col = "blue") #The output looks like below

#plot the prediction fully with the following command
#The output looks like below, this graph shows the expected Revenue
plot(pred, type = "l", lty = 1.8, col = "blue")

# Calculating the accuracy
rmse <- sqrt(mean(pred-sales$Revenue)^2) # Root Mean Square Error is the standard deviation of the residuals
rmse