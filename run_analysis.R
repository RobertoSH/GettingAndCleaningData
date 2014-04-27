rm(list=ls())

Read.File<-function(Path,File){
  #This function makes easier load all files in R
  #it only needs the path and file you want to load.
  
  #Here you create a name for the new variable.
  Name<-substr(File,start=1,stop=nchar(File)-4)
  #Here you assign the table to the new variable.
  assign(x=Name,
         value=read.table(file=paste(Path,File,sep=""),stringsAsFactors=F),
         envir=globalenv())
}


#This is the path where your "UCI HAR Dataset" folder is
Data<-"C:/Users/rsuarez/Desktop/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/"


#Here you add all paths y files you need to load

Path1<<-paste(Data,"test/",sep="")
File1<-list.files(Path1,pattern="[.txt]$")

Path2<<-paste(Data,"test/Inertial Signals/",sep="")
File2<-list.files(Path2,pattern="[.txt]$")

Path3<<-paste(Data,"train/",sep="")
File3<-list.files(Path3,pattern="[.txt]$")

Path4<<-paste(Data,"train/Inertial Signals/",sep="")
File4<-list.files(Path4,pattern="[.txt]$")

Path5<<-paste(Data,"",sep="")
File5<-list.files(Path5,pattern="[.txt]$")
File5<-File5[!File5%in%c("README.txt","features_info.txt","test","Tidy.txt")]


#aux it's only to do not let R print table at console
aux<-capture.output({
  res <- lapply(File1,function(x) Read.File(Path1,x));
 })

aux<-capture.output({
  res <- lapply(File2,function(x) Read.File(Path2,x));
})

aux<-capture.output({
  res <- lapply(File3,function(x) Read.File(Path3,x));
})

aux<-capture.output({
  res <- lapply(File4,function(x) Read.File(Path4,x));
})

aux<-capture.output({
  res <- lapply(File5,function(x) Read.File(Path5,x));
})

#Test part

#Only Mean and Std variables
variables_test<-c(grep("[Ss]td",features[,2], value=TRUE),
             grep("[Mm]ean",features[,2], value=TRUE))
variables_test<-variables_test[!variables_test%in%grep("[Mm]eanF",
                                             features[,2],
                                             value=TRUE)]
variables_test<-variables_test[!variables_test%in%grep("angle",
                                                       features[,2],
                                                       value=TRUE)]

tidy_1<-cbind(subject_test,y_test,X_test[,features[,2]%in%variables_test])
names(tidy_1)<-c("Subject","Activity",
                 features[features[,2]%in%variables_test,2])

tidy_1$Activity<-merge(tidy_1,activity_labels,by.x="Activity",by.y="V1")$V2


#Test part

#Only Mean and Std variables
#Only Mean and Std variables
variables_train<-c(grep("[Ss]td",features[,2], value=TRUE),
                   grep("[Mm]ean",features[,2], value=TRUE))
variables_train<-variables_train[!variables_train%in%grep("[Mm]eanF",
                                                          features[,2],
                                                          value=TRUE)]
variables_train<-variables_train[!variables_train%in%grep("angle",
                                                          features[,2],
                                                          value=TRUE)]

tidy_2<-cbind(subject_train,y_train,X_train[,features[,2]%in%variables_train])
names(tidy_2)<-c("Subject","Activity",
                 features[features[,2]%in%variables_train,2])

tidy_2$Activity<-merge(tidy_2,activity_labels,by.x="Activity",by.y="V1")$V2


Tidy<-rbind(tidy_1,tidy_2)

#Write txt
write.table(Tidy,paste(Data,"Tidy.txt",sep=""))

#indepent Tidy
Tidy_Average<-as.data.frame.table(tapply(Tidy[,3],
                                         list(Subject=Tidy$Subject,Activity=Tidy$Activity),
                                         mean))
#Mean for each variable
for(i in 4:68){
Tidy_Average[,i]<-as.data.frame.table(tapply(Tidy[,i],
                     list(Subject=Tidy$Subject,Activity=Tidy$Activity),
                     mean))$Freq
}

#Same names for both Tidy data sets
names(Tidy_Average)<-names(Tidy)
#Write txt
write.table(Tidy_Average,paste(Data,"Tidy_Average.txt",sep=""))
