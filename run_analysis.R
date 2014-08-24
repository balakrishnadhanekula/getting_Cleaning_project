features     = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/features.txt',header=FALSE);
activityType = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/activity_labels.txt',header=FALSE);
subjectTrain = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/train/subject_train.txt',header=FALSE);
xTrain       = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/train/X_train.txt',header=FALSE);
yTrain       = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/train/y_train.txt',header=FALSE);

colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

training_Data = cbind(yTrain,subjectTrain,xTrain);

subjectTest = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/test/subject_test.txt',header=FALSE);
xTest       = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/test/X_test.txt',header=FALSE);
yTest       = read.table('/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/test/y_test.txt',header=FALSE);

colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

test_Data = cbind(yTest,subjectTest,xTest);

final_Data = rbind(training_Data,test_Data);

colNames  = colnames(final_Data);

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

final_Data = final_Data[logicalVector==TRUE];

final_Data = merge(final_Data,activityType,by='activityId',all.x=TRUE);

colNames  = colnames(final_Data);

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(final_Data) = colNames;

finalDataNoActivityType  = final_Data[,names(final_Data) != 'activityType'];

tidy_Data    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

tidy_Data    = merge(tidy_Data,activityType,by='activityId',all.x=TRUE);

write.table(tidy_Data, '/home/bala/Downloads/getting_cleaning_data/UCI HAR Dataset/tidy_Data.txt',row.names=TRUE,sep='\t');
