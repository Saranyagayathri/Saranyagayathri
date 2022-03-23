df <- read.csv(choose.files())
dim(df)
library(stringr)
library(dplyr)
library(reshape2)
head(df)
str(df)
Date_time <- str_split_fixed(df$InvoiceDate, " ", 2)
invoice_date <- Date_time[,1]
invoice_time <- Date_time[,2]
data <- cbind(df, invoice_date, invoice_time)
data$invoice_date <- as.Date(data$invoice_date,"%d/%m/%Y")
data <- data[, c(-5,-10)]
head(data)
range(data$invoice_date)
colSums(is.na(data))
data_na <- na.omit(data)
range(data_na$invoice_date)
today <- as.Date('2011-12-11', format='%Y-%m-%d')
data_na$Monetary <- data_na$Quantity * data_na$UnitPrice
head(data_na)
# spliting the data into date and time
data_RFM <- data_na %>%
  group_by(CustomerID) %>%  
  mutate(frequency=n(),
       recency=as.numeric(today-invoice_date),
       Monetary=sum(Monetary)) %>%  
  filter(invoice_date==max(invoice_date)) %>%
  filter(CustomerID==max(CustomerID))
data_RFM_seg <- data_RFM[,c(6,9,10,11)] 
data_RFM_seg
data_RFM_seg <- unique(data_RFM_seg)
data_RFM_seg_1 <- data_RFM_seg[ , -1]
data_RFM_seg_scale <- scale(data_RFM_seg_1)


# calculating the z score for every obs
# obs-mean/sd
# optimal number of clusters
df1 <- data_RFM_seg_scale
df1
wss <- (nrow(df1)-1)*sum(apply(mydata,2,var))

for (i in 1:15) wss[i] <- sum(kmeans(df1,centers=i)$withinss)
plot(1:15, wss, type='b', xlab="number of cluster",
     ylab="within groups sum of squares")
kmeans <- kmeans(df1, 6)
kmeans
cluster_num <- kmeans$cluster
data_RFM_seg <- as.data.frame(data_RFM_seg)
data_RFM_seg_final <- cbind(data_RFM_seg, cluster_num)
# profiling of cluster and cluster 6 average value
# by doing with mean
tapply(data_RFM_seg_final$recency,data_RFM_seg_final$cluster_num, mean)
tapply(data_RFM_seg_final$frequency,data_RFM_seg_final$cluster_num, mean)
tapply(data_RFM_seg_final$Monetary,data_RFM_seg_final$cluster_num, mean)
# the best costumer is 2 . concluding by seeing the result in frequency and recency
# find with min value

tapply(data_RFM_seg_final$recency,data_RFM_seg_final$cluster_num, min)
tapply(data_RFM_seg_final$frequency,data_RFM_seg_final$cluster_num, min)
tapply(data_RFM_seg_final$Monetary,data_RFM_seg_final$cluster_num, min)
#
tapply(data_RFM_seg_final$recency,data_RFM_seg_final$cluster_num, max)
tapply(data_RFM_seg_final$frequency,data_RFM_seg_final$cluster_num, max)
tapply(data_RFM_seg_final$Monetary,data_RFM_seg_final$cluster_num, max)


iris
mydata <- iris
library(caTools)
set.seed(133)
split <- sample.split(mydata$Species, SplitRatio = 0.75)
split
table(split)
training <- subset(mydata, split==T)
test <- subset(mydata, split==F)
nrow(training)
nrow(test)
library(e1071)
names(training)
nrow(test)
bayes_model <- naiveBayes(Species~., data = training)
summary(bayes_model)
y_pred <- predict(bayes_model, newdata = test)
y_pred
library(caret)
cm <- table(test$Species, y_pred)
confusionMatrix(cm)
# anova test
anova <- aov(Sepal.Length~., data=mydata)
summary(anova)
