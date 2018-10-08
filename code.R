df<-read.csv("F:\\kaggle\\4.Telstra Service Fault\\train.csv")
event<-read.csv("F:\\kaggle\\4.Telstra Service Fault\\event_type.csv")
log<-read.csv("F:\\kaggle\\4.Telstra Service Fault\\log_feature.csv")
resource<-read.csv("F:\\kaggle\\4.Telstra Service Fault\\resource_type.csv")
severity<-read.csv("F:\\kaggle\\4.Telstra Service Fault\\severity_type.csv")
test<-read.csv("F:\\kaggle\\4.Telstra Service Fault\\test.csv")
colnames(df)
colnames(event)
colnames(log)
colnames(resource)
colnames(severity)
colnames(test)
View(test)
## One hot encoding, to have equal no. of rows in each of the datasets, 
##so as to combine finally with the train.,

## Binning the volume columns in the log-features data
log$volbin<-vector(mode = "integer",length = 58671)

##360 tak 12 grp of 30, 500 tak 1 grp aur baaki saare 1 grp
i<-1
j<-0   ## no of bins
for(i in 1:nrow(log))
{
  for(j in 0:11)  
  {
    if(log$volume[i]>(j*30) & log$volume[i]<=((j+1)*30))
    {
      log$volbin[i]<-j+1
    }
  }
}  ## This takes care of the 1st 12 bins till 360 volume


log$volbin<-ifelse((log$volume>361 & log$volume<=502),13,log$volbin)
log$volbin<-ifelse((log$volume>502),14,log$volbin)
View(log)
log$volbin<-factor(log$volbin)
levels(log$volbin)<-c("0-30","31-60","61-90","91-120","121-150","151-180","181-210","211-240","241=270","271-300","301-330","331-360","360-500","500+")
levels(log$volbin)

library(dummies)
library(dplyr)

## log-feature
log_df<-dummy.data.frame(log)
View(log_df1)

log_df1<-log_df %>% group_by(id) %>% summarise_all(funs(sum)) ## To combine the values by id numbers
                                        ##and reduce the rows to unique values of id numbers.
colnames(log_df)


## event type
event_df<-dummy.data.frame(event)
event_df1<-event_df %>% group_by(id) %>% summarise_all(funs(sum)) 
View(event_df1)
colnames(event_df)

## resource_type
resource_df<-dummy.data.frame(resource)
resource_df1<-resource_df %>% group_by(id) %>% summarise_all(funs(sum)) 
View(resource_df1)
colnames(resource_df)

## severity_type
severity_df<-dummy.data.frame(severity)
colnames(severity_df)
severity_df1<-severity_df %>% group_by(id) %>% summarise_all(funs(sum))  ## Unique values.
View(severity_df1)


#### Now combining all the 4 datasets having 18552 rows each, severity,log,resource and event into one main dataset.
data_combine<-cbind(event_df1,log_df1,resource_df1,severity_df1)


### Now subsetting this data to match with the id's in train df, having 7381 rows.
df<- df[order(df$id),]   ## Sorting in ascending order
test<-test[order(test$id),]
rownames(test)<-1:nrow(test)
View(test)
View(df)
data[100,]

## Getting the indexes from the main data for the train and test.
i<-1
ind_train<-vector(mode = "numeric")
ind_test<-vector(mode = "numeric")

for(i in 1:nrow(data_combine))
{
  ind_train<-ifelse(df$id==data_combine$id[i],data_combine$id[i],ind_train)
  ind_test<-ifelse(test$id==data_combine$id[i],data_combine$id[i],ind_test)
}

table(ind_train)

## Dividing the data into the respective train and test portions.
data_train<-data[ind_train,]
data_test<-data[ind_test,]
View(data_train)


### Now combining the data to the train and test to get the actual train and test
train_final<-cbind.data.frame(df,data_train)
test_final<-cbind.data.frame(test,data_test)
View(train_final)

## Dropping the repeated id columns during the cbind & location column from train and test.
train_final<-train_final[,-c(2,4,58,445,460,471)]
test_final<-test_final[,-c(2,3,57,444,459,470)]
colnames(train_final)
colnames(test_final)

library(nnet)
model<-multinom(fault_severity~.,data = train_final, family="multinomial",MaxNWts =10000000)
summary(model)

results<-predict(model,test_final,type = "prob")


x<-cbind(test_final$id,results)
# x<-as.data.frame(x)
# x$results<-factor(x$results)
# y<-dummy.data.frame(x)
colnames(x)<-c("id","predict_0","predict_1","predict_2")
View(x)
write.csv(x,file="submission.csv", row.names = F)






