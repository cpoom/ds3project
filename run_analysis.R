# The "run_analysis()" function analyzes data from Samsung Galaxy phone testing.
# The source of the data is UCI:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# This data should be downloaded and unzipped to a network or local directory.
# The current working directory must be set to this location before this script
# is called.
#

# This function loads and manipulates serveral files using the run_sub_analysis()
#  function (see below) that is called on both the test data and the training data.
# The resulting dataframes are then combined into 1 larger dataframe.
#
# The dataframe is then grouped by study participant (subject) and activity
#   (walking, sitting, etc.) and the average of each of the other variables are
#   is taken for each subject/activity pair. This dataframe is the return value
#   and is a tidy dataset with meaningful variable names.


run_analysis<-function() {
  
    library(dplyr)
  
    # load and manipulate both the test and training datasets
    testdf<-run_sub_analysis("test")
    traindf<-run_sub_analysis("train")
    
    #combine the 2 sets of data into one big one
    mergeddf<-rbind(testdf,traindf)

    # Group the observation data by subject and activity
    subjActs<-group_by(mergeddf,subjectid,activity)
    
    # Create a tidy dataframe based on the grouping above.
    # Non-grouping variables will be averaged over each group.
    avgSubjActs<-summarize(subjActs,
          averagetbodyacceleratormeanxaxis=mean(tBodyAccmeanX),
          averagetbodyacceleratormeanyaxis=mean(tBodyAccmeanY),
          averagetbodyacceleratormeanzaxis=mean(tBodyAccmeanZ),
          averagetbodyacceleratorstdxaxis=mean(tBodyAccstdX),
          averagetbodyacceleratorstdyaxis=mean(tBodyAccstdY),
          averagetbodyacceleratorstdzaxis=mean(tBodyAccstdZ),
          averagetgravityacceleratormeanxaxis=mean(tGravityAccmeanX),
          averagetgravityacceleratormeanyaxis=mean(tGravityAccmeanY), 
          averagetgravityacceleratormeanzaxis=mean(tGravityAccmeanZ),
          averagetgravityacceleratorstdxaxis=mean(tGravityAccstdX),
          averagetgravityacceleratorstdyaxis=mean(tGravityAccstdY),
          averagetgravityacceleratorstdzaxis=mean(tGravityAccstdZ),
          averagetbodyacceleratorjerkmeanxaxis=mean(tBodyAccJerkmeanX),
          averagetbodyacceleratorjerkmeanyaxis=mean(tBodyAccJerkmeanY),
          averagetbodyacceleratorjerkmeanzaxis=mean(tBodyAccJerkmeanZ),
          averagetbodyacceleratorjerkstdxaxis=mean(tBodyAccJerkstdX),
          averagetbodyacceleratorjerkstdyaxis=mean(tBodyAccJerkstdY),
          averagetbodyacceleratorjerkstdzaxis=mean(tBodyAccJerkstdZ),
          averagetbodygyromeanxaxis=mean(tBodyGyromeanX),
          averagetbodygyromeanyaxis=mean(tBodyGyromeanY),
          averagetbodygyromeanzaxis=mean(tBodyGyromeanZ),
          averagetbodygyrostdxaxis=mean(tBodyGyrostdX),
          averagetbodygyrostdyaxis=mean(tBodyGyrostdY),
          averagetbodygyrostdzaxis=mean(tBodyGyrostdZ),
          averagetbodygyrojerkmeanxaxis=mean(tBodyGyroJerkmeanX),
          averagetbodygyrojerkmeanyaxis=mean(tBodyGyroJerkmeanY),
          averagetbodygyrojerkmeanzaxis=mean(tBodyGyroJerkmeanZ),
          averagetbodygyrojerkstdxaxis=mean(tBodyGyroJerkstdX),
          averagetbodygyrojerkstdyaxis=mean(tBodyGyroJerkstdY),
          averagetbodygyrojerkstdzaxis=mean(tBodyGyroJerkstdZ),
          averagetbodyacceleratormagmean=mean(tBodyAccMagmean),
          averagetbodyacceleratormagstd=mean(tBodyAccMagstd),
          averagetgravityacceleratormagmean=mean(tGravityAccMagmean),
          averagetgravityacceleratormagstd=mean(tGravityAccMagstd),
          averagetbodyacceleratorjerkmagmean=mean(tBodyAccJerkMagmean),
          averagetbodyacceleratorjerkmagstd=mean(tBodyAccJerkMagstd),
          averagetbodygyromagmean=mean(tBodyGyroMagmean),
          averagetbodygyromagstd=mean(tBodyGyroMagstd),
          averagetbodygyrojerkmagmean=mean(tBodyGyroJerkMagmean),
          averagetbodygyrojerkmagstd=mean(tBodyGyroJerkMagstd),
          averagefbodyacceleratormeanxaxis=mean(fBodyAccmeanX),
          averagefbodyacceleratormeanyaxis=mean(fBodyAccmeanY),
          averagefbodyacceleratormeanzaxis=mean(fBodyAccmeanZ),
          averagefbodyacceleratorstdxaxis=mean(fBodyAccstdX),
          averagefbodyacceleratorstdyaxis=mean(fBodyAccstdY),
          averagefbodyacceleratorstdzaxis=mean(fBodyAccstdZ),
          averagefbodyacceleratormeanfreqxaxis=mean(fBodyAccmeanFreqX),
          averagefbodyacceleratormeanfreqyaxis=mean(fBodyAccmeanFreqY),
          averagefbodyacceleratormeanfreqzaxis=mean(fBodyAccmeanFreqZ),
          averagefbodyacceleratorjerkmeanxaxis=mean(fBodyAccJerkmeanX),
          averagefbodyacceleratorjerkmeanyaxis=mean(fBodyAccJerkmeanY),
          averagefbodyacceleratorjerkmeanzaxis=mean(fBodyAccJerkmeanZ),
          averagefbodyacceleratorjerkstdxaxis=mean(fBodyAccJerkstdX),
          averagefbodyacceleratorjerkstdyaxis=mean(fBodyAccJerkstdY),
          averagefbodyacceleratorjerkstdzaxis=mean(fBodyAccJerkstdZ),
          averagefbodyacceleratorjerkmeanfreqxaxis=mean(fBodyAccJerkmeanFreqX),
          averagefbodyacceleratorjerkmeanfreqyaxis=mean(fBodyAccJerkmeanFreqY),
          averagefbodyacceleratorjerkmeanfreqzaxis=mean(fBodyAccJerkmeanFreqZ),
          averagefbodygyromeanxaxis=mean(fBodyGyromeanX),
          averagefbodygyromeanyaxis=mean(fBodyGyromeanY),
          averagefbodygyromeanzaxis=mean(fBodyGyromeanZ),
          averagefbodygyrostdxaxis=mean(fBodyGyrostdX),
          averagefbodygyrostdyaxis=mean(fBodyGyrostdY),
          averagefbodygyrostdzaxis=mean(fBodyGyrostdZ),
          averagefbodygyromeanfreqxaxis=mean(fBodyGyromeanFreqX),
          averagefbodygyromeanfreqyaxis=mean(fBodyGyromeanFreqY),
          averagefbodygyromeanfreqzaxis=mean(fBodyGyromeanFreqZ),
          averagefbodyacceleratormagmean=mean(fBodyAccMagmean),
          averagefbodyacceleratormagstd=mean(fBodyAccMagstd),
          averagefbodyacceleratormagmeanfreq=mean(fBodyAccMagmeanFreq),
          averagefbodybodyacceleratorjerkmagmean=mean(fBodyBodyAccJerkMagmean),
          averagefbodybodyacceleratorjerkmagstd=mean(fBodyBodyAccJerkMagstd),
          averagefbodybodyacceleratorjerkmagmeanefreq=mean(fBodyBodyAccJerkMagmeanFreq),
          averagefbodybodygyromagmean=mean(fBodyBodyGyroMagmean),
          averagefbodybodygyromagstd=mean(fBodyBodyGyroMagstd),
          averagefbodybodygyromagmeanfreq=mean(fBodyBodyGyroMagmeanFreq),
          averagefbodybodygyrojerkmagmean=mean(fBodyBodyGyroJerkMagmean),
          averagefbodybodygyrojerkmagstd=mean(fBodyBodyGyroJerkMagstd),
          averagefbodybodygyrojerkmagmeanfreq=mean(fBodyBodyGyroJerkMagmeanFreq))    
}

# This function loads and manipulates data in one of the two sub-directories.
# It returns a dataframe.

run_sub_analysis<-function(dirnm) {

  # load file with most of the observation data
  fn<-sprintf("%s\\X_%s.txt",dirnm,dirnm)
  X_test<-read.csv(fn,header=FALSE,stringsAsFactors=FALSE)

  # data is read in a single char variable per row
  # below we split it on white space (which results in a list of lists).
  splitV1<-function(x){ strsplit(x,"([\t| |\r|\n])+")}
  x1<-splitV1(X_test[1]$V1)
  
  # We cast the list-of-lists as a dataframe
  x1df<-as.data.frame(x1)
  
  # The resulting dataframe has rows and columns mixed up
  # so we call the "t" function which flips the dataframe on its side
  tx1df<-t(x1df)
  
  # we shave off the first column, which is all blank strings
  tx1df<-tx1df[,2:562]
  
  # now we convert all values from char to numeric
  # and then make it a dataframe again
  txn<-structure(sapply(tx1df, as.numeric), dim=dim(tx1df))
  xtdf<-as.data.frame(txn)

  # load the subject ID for each row of the observation dataframe   
  fn<-sprintf("%s\\subject_%s.txt",dirnm,dirnm)
  subjectTest<-read.csv(fn,header=FALSE,stringsAsFactors=FALSE)
 
  # load the activity for each row of the observation dataframe 
  fn<-sprintf("%s\\y_%s.txt",dirnm,dirnm)
  y_test<-read.csv(fn,header=FALSE,stringsAsFactors=FALSE)
  
  # load the activity labels file, which is in the parent directory
  fn<-"activity_labels.txt"
  actlbls<-read.csv(fn,header=FALSE,stringsAsFactors=FALSE,sep=" ")

  # create a vector of activities, translating the activity integer
  # to the corresponding text description
  lblvec<-xlateActivity(y_test,actlbls) 
 
  # add the subject and activity columns to the other observation data
  subjectTest<-cbind(subjectTest,lblvec)
  sxdf<-cbind(subjectTest,xtdf)

  # load the feature file, which is in the parent directory
  #  this file contains the variable names for the dataframe
  varNames<-read.csv("features.txt",header=FALSE,stringsAsFactors=FALSE,sep=" ")
  
  # take strange characters our of variable names
  # and then apply them to data frame
  vn<-c("subjectid","activity",varNames$V2)
  vn<-sapply(vn,function(y) {gsub("-", "", y)})
  vn<-sapply(vn,function(y) {gsub("\\(", "", y)})
  vn<-sapply(vn,function(y) {gsub("\\)", "", y)}) 
  vn<-sapply(vn,function(y) {gsub(",", "_", y)})    
  colnames(sxdf)<-vn    
  
  # keep only the 2 columns we added plus any columns that include
  # the text "mean" or "std"
  keepcolbools<-grepl("(subjectid|activity|mean|std)",vn)
  subsxdf<-sxdf[,keepcolbools]
  
  subsxdf
}

# translate the activity numbers to the corresponding activity descriptions

xlateActivity<-function(y_test,actlbls){

    lblvec<-rep(" ",length(y_test$V1))
    for (i in 1:length(lblvec)) {
       index<-  y_test[i,1]   
       lblvec[i]<-actlbls[index,2]
    }
    
    lblvec
}