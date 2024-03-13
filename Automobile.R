# Read the data
Data = read.csv("/Users/xiaoyuzhu/Documents/MGSC661/Automobile_data.csv")
# 26 columns & 205 rows
ncol(Data)
nrow(Data)
# Get type
str(Data)
# value count
# numeric variable 
unique(Data$symboling) # 6 rateings from -2 to 3
unique(Data$normalized.losses) #contain '?' 
unique(Data$wheel.base)
unique(Data$length)
unique(Data$height)
unique(Data$width)
unique(Data$curb.weight)
unique(Data$engine.size)
unique(Data$bore) # contain '?'
unique(Data$stroke) # constain '?'
unique(Data$compression.ratio)
unique(Data$horsepower) # contain ? 
unique(Data$peak.rpm) # constain '?'
unique(Data$city.mpg)
unique(Data$highway.mpg)
unique(Data$price) # constain '?'

# categorical variable
unique(Data$make) # 22 makers
unique(Data$fuel.type) # gas & diesel
unique(Data$aspiration) # std and turbo
unique(Data$num.of.doors) # 2, 4, '?' may convert to num 
unique(Data$body.style) # "convertible" "hatchback"   "sedan"       "wagon"       "hardtop" 
unique(Data$drive.wheels) # "rwd" "fwd" "4wd"
unique(Data$engine.location) # "front" "rear"
unique(Data$engine.type) # "dohc"  "ohcv"  "ohc"   "l"     "rotor" "ohcf"  "dohcv"
unique(Data$num.of.cylinders) # "four","six","five","three","twelve","two","eight" may convert to num
unique(Data$fuel.system) # "mpfi" "2bbl" "mfi"  "1bbl" "spfi" "4bbl" "idi"  "spdi"

# convert data type 
Data$normalized.losses <- as.integer(Data$normalized.losses)
Data$bore <- as.double(Data$bore)
Data$stroke <- as.double(Data$stroke)
Data$peak.rpm <- as.integer(Data$peak.rpm)
Data$price <- as.integer(Data$price)
Data$horsepower <- as.integer(Data$horsepower)

# convert words into numbers 
Data[Data$num.of.doors == "two",]$num.of.doors = 2
Data[Data$num.of.doors == "four",]$num.of.doors = 4
Data$num.of.doors <- as.integer(Data$num.of.doors)

Data[Data$num.of.cylinders == "two",]$num.of.cylinders = 2
Data[Data$num.of.cylinders == "three",]$num.of.cylinders = 3
Data[Data$num.of.cylinders == "four",]$num.of.cylinders = 4
Data[Data$num.of.cylinders == "five",]$num.of.cylinders = 5
Data[Data$num.of.cylinders == "six",]$num.of.cylinders = 6
Data[Data$num.of.cylinders == "eight",]$num.of.cylinders = 8
Data[Data$num.of.cylinders == "twelve",]$num.of.cylinders = 12
Data$num.of.cylinders <- as.integer(Data$num.of.cylinders)

# Dealing with '?'
Data[Data=="?"] <- NA
Data[,"normalized.losses"][is.na(Data[,"normalized.losses"])] <- mean(Data[,"normalized.losses"], na.rm = TRUE)
Data[,"bore"][is.na(Data[,"bore"])] <- round(mean(Data[,"bore"], na.rm = TRUE),2)
Data[,"stroke"][is.na(Data[,"stroke"])] <- round(mean(Data[,"stroke"], na.rm = TRUE),2)
Data[,"peak.rpm"][is.na(Data[,"peak.rpm"])] <- mean(Data[,"peak.rpm"], na.rm = TRUE)
Data[,"price"][is.na(Data[,"price"])] <- mean(Data[,"price"], na.rm = TRUE)
Data[,"horsepower"][is.na(Data[,"horsepower"])] <- mean(Data[,"horsepower"], na.rm = TRUE)
Data[,"num.of.doors"][is.na(Data[,"num.of.doors"])] <- 4
Data[Data$num.of.doors == "?",]$num.of.doors = 4

str(Data)
# Visualize Categorical data with price
library(ggplot2)
require(methods)
library(ggpubr)
plot1 = ggplot(Data, aes(price, y=make)) + geom_boxplot() + ggtitle("Boxplot between price and maker")
plot2 = ggplot(Data, aes(price, y=fuel.type)) + geom_boxplot() + ggtitle("Boxplot between price and fuel.type")
plot3 = ggplot(Data, aes(price, y=aspiration)) + geom_boxplot() + ggtitle("Boxplot between price and aspiration")
plot4 = ggplot(Data, aes(price, y=body.style)) + geom_boxplot() + ggtitle("Boxplot between price and body.style")
plot5 = ggplot(Data, aes(price, y=drive.wheels)) + geom_boxplot() + ggtitle("Boxplot between price and drive.wheels")
plot6 = ggplot(Data, aes(price, y=engine.location)) + geom_boxplot() + ggtitle("Boxplot between price and engine.location")
plot7 = ggplot(Data, aes(price, y=engine.type)) + geom_boxplot() + ggtitle("Boxplot between price and engine.type")
plot8 = ggplot(Data, aes(price, y=fuel.system)) + geom_boxplot() + ggtitle("Boxplot between price and fuel.system")
plot9 = ggplot(Data, aes(price, y=num.of.doors)) + geom_boxplot() + ggtitle("Boxplot between price and num.of.doors")
ggarrange(plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,ncol = 2,nrow = 5, align = "v")

# Count plot: pie chart & bar chart 
library(lessR)
plot1 =pie(table(Data$make),hole = 0, values = "%", main="Makers for automobile" )  
plot2 =pie(table(Data$body.style),hole = 0, values = "%", main="Makers for body style" )  
plot3 =pie(table(Data$engine.type),hole = 0, values = "%", main="Makers for engine type" )  
plot4 =pie(table(Data$fuel.system),hole = 0, values = "%", main="Makers for fuel system" )  
plot5 =ggplot(Data, aes(x = fuel.type)) +geom_bar() + ggtitle("Count plot of fuel type")
plot6 =ggplot(Data, aes(x = aspiration)) +geom_bar() + ggtitle("Count plot of asperiation")
plot7 =ggplot(Data, aes(x = engine.location)) +geom_bar() + ggtitle("Count plot of engine location")
plot8 =ggplot(Data, aes(x = drive.wheels)) +geom_bar() + ggtitle("Count plot of drive wheels")
plot9 =ggplot(Data, aes(x = num.of.doors)) +geom_bar() + ggtitle("Count plot of num of doors")
#plot10 =ggplot(Data, aes(x = num.of.cylinders)) +geom_bar() + ggtitle("Count plot of num of cylinders")
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,ncol = 2,nrow = 5, align = "v")

# visualize numeric values 
# some of variables may surrfer extreme value
plot1 = ggplot(Data, aes(x=symboling,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('symboling')
plot2 = ggplot(Data, aes(x=normalized.losses,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('normalized.losses')
plot3 = ggplot(Data, aes(x=wheel.base,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('wheel.base')
plot4 = ggplot(Data, aes(x=length,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('length')
plot5 = ggplot(Data, aes(x=height,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('height')
plot6 = ggplot(Data, aes(x=width,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('width price')
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol = 2,nrow = 3, align = "v")

plot7 = ggplot(Data, aes(x=curb.weight,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('curb.weight')
plot8 = ggplot(Data, aes(x=engine.size,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('engine.size')
plot9 = ggplot(Data, aes(x=bore,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('bore')
plot10 = ggplot(Data, aes(x=stroke,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('stroke')
ggarrange(plot7,plot8,plot9,plot10,ncol = 2,nrow = 2, align = "v")
plot11 = ggplot(Data, aes(x=compression.ratio,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('horsepower')
plot12 = ggplot(Data, aes(x=horsepower,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('peak.rpm')
plot13 = ggplot(Data, aes(x=peak.rpm,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('engine.size')
plot14 = ggplot(Data, aes(x=city.mpg,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('city.mpg')
plot15 = ggplot(Data, aes(x=highway.mpg,y=price)) + geom_point(position=position_jitter(w=0.1,h=0)) + geom_smooth() +xlab('highway.mpg')
ggarrange(plot11,plot12,plot13,plot14,ncol = 2,nrow = 2, align = "v")

# get all the numeric variables 
Data_vars=Data[,c(1,2,10,11,12,13,14,16,17,19:26)]
library(GGally)
ggpairs(Data_vars) # too mess up 
#some potential multiconlinearity
res <- cor(Data_vars)
round(res, 2)

# check skewness
library(moments)
skewness(Data$symboling) # 6 rateings from -2 to 3
skewness(Data$normalized.losses) #contain '?' 
skewness(Data$wheel.base)
skewness(Data$length)
skewness(Data$height)
skewness(Data$width)
skewness(Data$curb.weight)
skewness(Data$engine.size)
skewness(Data$bore) # contain '?'
skewness(Data$stroke) # constain '?'
skewness(Data$compression.ratio)
skewness(Data$horsepower) # contain ? 
skewness(Data$peak.rpm) # constain '?'
skewness(Data$city.mpg)
skewness(Data$highway.mpg)
skewness(Data$num.of.cylinders)
skewness(Data$price) # constain '?'

# check linearity
library(car)
reg = lm(price~symboling,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~normalized.losses,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~wheel.base,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~length,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~width,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~height,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~curb.weight,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~engine.size,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~bore,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~stroke,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~compression.ratio,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~horsepower,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~peak.rpm,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~city.mpg,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~highway.mpg,data=Data)
summary(reg)
outlierTest(reg)

reg = lm(price~num.of.cylinders,data=Data)
summary(reg)
outlierTest(reg)

# too much variables? try PCA! 
pca=prcomp(Data_vars, scale=TRUE)
pca 
library(ggfortify)
autoplot(pca, data = Data_vars, loadings = TRUE,  loadings.label.repel = TRUE )

# dummyfied all categorical variables 
Data$fuel.type =as.factor(Data$fuel.type)
Data$aspiration =as.factor(Data$aspiration)
Data$body.style =as.factor(Data$body.style)
Data$drive.wheels =as.factor(Data$drive.wheels)
Data$engine.location =as.factor(Data$engine.location)
Data$engine.type =as.factor(Data$engine.type)
Data$fuel.system =as.factor(Data$fuel.system)
Data$num.of.doors =as.factor(Data$num.of.doors)
Data$make =as.factor(Data$make)
attach(Data)

# outlier test # three observation failed the test 
reg = lm(price ~ .,data=Data)
library("car")
outlierTest(reg)

# remove these outliers 
Data = Data[-c(17, 10, 75), ] 

# heat matrix 
Data_vars = as.matrix(Data_vars)
heatmap(Data_vars, scale="column")

# train test split 
require(caTools)
require(methods)
sample=sample.split(Data$price, SplitRatio=0.75)  ##assign random value of TRUE/FALSE to each obs
train_set=subset(Data, sample==TRUE) ## send obs with TRUE to train set
test_set=subset(Data, sample==FALSE) ## send obs with FALSE to test set


# Random Forest Tunning 
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(Class~., data=Data, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)


library(ranger)     
#library(caret)      
#library(h2o)
library(randomForest)

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(0, 25, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.5, .6, .70, .80),
  num.trees = c(500,1000,1500),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = price ~ ., 
    data            = train_set, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# lowest OOB RMSE combination 
#mtry node_size sampe_size num.trees 
#8         3        0.8      500


forest=randomForest(content_rating~duration+title_year+facenumber_in_poster, cp=0.01, na.action=na.omit)
summary(classifiedforest)

RF = randomForest(x = train_set[,1:25],y = train_set$price, nodesize = 3,sample_size = 0.8,mtry = 8, ntree = 500)
y_pred = predict(RF, newdata = test_set[,1:25])
install.packages("Metrics")
library(Metrics)
results_1 = rmse(y_pred, test_set$price)
results_1
summary(RF)
varImpPlot(RF)


library(dplyr)
library(gbm) 
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  bag.fraction = c(.65, .8, 1),
  n.trees = c(500,1000,2000),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)


random_index <- sample(1:nrow(train_set), nrow(train_set))
random_train_set<- train_set[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = price ~ .,
    distribution = "gaussian",
    data = random_train_set,
    n.trees = hyper_grid$n.trees[i],
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

# lowest RMSE combination 
#shrinkage interaction bag.fraction num.trees 
# 0.3              5         1.00     500

GBM = gbm(price ~ .,data=train_set,distribution= "gaussian", shrinkage = 0.3, interaction.depth = 5,bag.fraction = 1, n.trees = 500)
y_pred = predict(GBM, newdata = test_set[,1:25])
library(Metrics)
results_2 = rmse(y_pred, test_set$price)
results_2
summary.gbm(GBM)
a = summary(GBM)
a <- a[order(-a$rel.inf),]
barplot(height = a$rel.inf,las = 2, names = a$var,cex.names=0.6,horiz = TRUE,xlim= c(0,50))

# bagging model 
library(rpart)
library(ipred)
library(caret)
set.seed(123)
bag <- bagging(formula = price ~ .,data = train_set,nbagg = 200,coob = TRUE,control = rpart.control(minsplit = 2, cp = 0))
y_pred = predict(bag, newdata = test_set[,1:25])
library(Metrics)
results_3 = rmse(y_pred, test_set$price)
results_3