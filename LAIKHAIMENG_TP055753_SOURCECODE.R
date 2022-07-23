#Property of LAI KHAI MENG 

#packages
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("plyr")
library(plyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)

#Read sample data
sample_data=read_delim(file="Insert file path of Student downloaded",
                       delim =";",
                       quote =" ")
View(sample_data)

#Replace all double quote by empty string
tempcol=c()
sample_data_edit=data.frame(index=1:nrow(sample_data))
for(col in 1:length(sample_data)){
  tempcol=c()
  for(row in sample_data[,col]){
    temp=gsub("\"","",row)
    tempcol=c(tempcol,temp)
  }
  sample_data_edit=cbind(sample_data_edit,tempcol)
}

#Rename all headers
colname=c("index",names(sample_data))
names(sample_data_edit)=colname
sample_data_edit=subset(sample_data_edit,select = -c(index))
names(sample_data_edit)=c("School","Gender","Peer_group","Demographic_area",
                          "Num_family","Par_mari_status","Mum_hig_edu","Fath_high_edu",
                          "Mot_occ","Fat_occ","Reason","Custody","Duration","Revison_time",
                          "Num_Fail","School_support","Family_backing","Finance",
                          "Afte_sch_activities","Preschool","Further_edu","Internet",
                          "Relationship","Family_Relationship","Interlude","Time_with_Fri",
                          "Day_alc","Week_alc","Fitness","Non_attendant","Grade_1","Grade_2",
                          "Grade_3")
#Final data
View(sample_data_edit)


#Formating the data
summary(sample_data_edit)

sample_data_edit$Grade_1=as.numeric(sample_data_edit$Grade_1)
sample_data_edit$Grade_2=as.numeric(sample_data_edit$Grade_2)
sample_data_edit$Grade_3=as.numeric(sample_data_edit$Grade_3)

sample_data_edit$School_support=as.factor(sample_data_edit$School_support)
sample_data_edit$School_support=as.factor(sample_data_edit$Family_backing)
sample_data_edit$Finance=as.factor(sample_data_edit$Finance)
sample_data_edit$Par_mari_status=as.factor(sample_data_edit$Par_mari_status)
sample_data_edit$Day_alc=as.factor(sample_data_edit$Day_alc)
sample_data_edit$Week_alc=as.factor(sample_data_edit$Week_alc)
sample_data_edit$Mum_hig_edu=as.factor(sample_data_edit$Mum_hig_edu)
sample_data_edit$Fath_high_edu=as.factor(sample_data_edit$Fath_high_edu)

summary(sample_data_edit)

#Final data
View(sample_data_edit)


#Question 1
#What extra help factor that affect the grades of the student.
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis1
#The relationship between school support and the maths grade of the student

Anal_1=ggplot(data=sample_data_edit,aes(x=Grade_1,y=Grade_2))+
  geom_point(aes(colour=School_support,shape=School_support,size=School_support))+
  annotate("rect", fill='purple',xmin = 15, xmax = 20, ymin = 15, ymax = 20,alpha = .2)+
  geom_vline(aes(xintercept=15),
             color="blue", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=15),
             color="blue", linetype="dashed", size=1)

Anal_1.1=ggplot(data=sample_data_edit,aes(x=School_support,y=Grade_3))+
  geom_boxplot(aes(fill=School_support))

Anal_1.1.2=ggarrange(Anal_1,Anal_1.1, 
                     labels = c("School support for Grade 1&2", "School Support for Grade 3"),
                     ncol = 2, nrow = 1)

#School support dont affect thier grade.

annotate_figure(Anal_1.1.2, top = text_grob("The relationship between school support and the maths grade of the student
                                   ",color = "black", face = "bold", size = 14))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 2
#The relationship between school support and family backing against the maths grade of the student

Anal_1.2=ggplot(data=sample_data_edit,aes(x=Grade_2,y=Grade_3))+
  geom_point(aes(colour=School_support,shape=School_support,size=School_support))+
  geom_smooth(method = "lm", se = FALSE,aes(linetype=Family_backing))+
  ggtitle("School support/family backing Grade 2/3")+
  geom_vline(aes(xintercept=15),
             color="black", linetype="solid", size=1)+
  geom_hline(aes(yintercept=15),
             color="black", linetype="solid", size=1)

Anal_1.2.1=ggplot(data = sample_data_edit,aes(x=School_support,y=Grade_1,fill=School_support))+
  geom_histogram(stat="summary",fun=mean,aes(colour=School_support))+
  ggtitle("School_support Grade1")

Anal_1.2.2=ggplot(data = sample_data_edit,aes(x=Family_backing,y=Grade_1,fill=Family_backing))+
  geom_histogram(stat="summary",fun=mean)+
  ggtitle("Family_support Grade1")

Anal_1.2.3=ggarrange(Anal_1.2,Anal_1.2.1,Anal_1.2.2,
                   ncol = 2, nrow = 2)

#Students without school support and family support can still get high
marks.
annotate_figure(Anal_1.2.3, top = text_grob("The relationship between school support and family backing against the maths grade"
                                          ,color = "black", face = "bold", size = 14))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 2 Analysis
#Does  family factors affect the grades of the student
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 2 Analysis 1
#The relationship between Par_mari_status and the maths grade of the student

Anal_2.1=ggplot(data=sample_data_edit,aes(x=Grade_1,y=Grade_2))+
  geom_smooth(method = "lm", se = FALSE,aes(colour=Par_mari_status,linetype=Par_mari_status))+
  ggtitle("Parents marriage status and the maths Grade 1/2")+
  annotate("rect", fill='purple',xmin = 15, xmax = 20, ymin = 15, ymax = 20,alpha = .2)


Anal_2.1.1=ggplot(data=sample_data_edit,aes(x=Par_mari_status,y=Grade_3))+
  geom_histogram(stat="summary",fun=mean,aes(colour=Par_mari_status,fill=Par_mari_status))+
  labs(y="Mean of Grade_3")+
  ggtitle("Parents marriage status and the maths Grade 3")


Anal_2.1.2=ggarrange(Anal_2.1,Anal_2.1.1, 
                     ncol = 2, nrow = 1)
#It can be said that student whos parent are apart does better overall
annotate_figure(Anal_2.1.2, top = text_grob("The relationship between Par_mari_status and the maths grade of the student",
                                            color = "black", face = "bold", size = 14))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 2 Analysis 2
#The relationship between mum's education level with grades
Anal_2.2=ggplot(sample_data_edit, aes(x=Grade_1,fill=Mum_hig_edu)) + 
  geom_density(alpha=0.4)+
  ggtitle("Mum eduaction level vs Grade 1")+
  geom_vline(aes(xintercept=16),
             color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=6),
             color="black", linetype="dashed", size=1)

Anal_2.2.1=ggplot(sample_data_edit, aes(x=Grade_2,fill=Mum_hig_edu)) + 
  geom_density(alpha=0.4)+
  ggtitle("Mum eduaction level vs Grade 2")+
  geom_vline(aes(xintercept=16),
             color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=6),
             color="black", linetype="dashed", size=1)

ggarrange(Anal_2.2,Anal_2.2.1, 
                     ncol = 2, nrow = 1)

Anal_2.2.3=subset(sample_data_edit,select = c(Mum_hig_edu,Grade_1,Grade_2))

alpha=Anal_2.2.3
#Mum_education level does not affect their grade
alpha %>%
  group_by(Mum_hig_edu) %>%
  summarise_at(vars(Grade_1,Grade_2), list(name = mean))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 2 Analysis 3
#The relationship between dad's education level with grades
Anal_2.3=ggplot(sample_data_edit, aes(x=Grade_1,fill=Fath_high_edu)) + 
  geom_density(alpha=0.4)+
  ggtitle("FATHER eduaction level vs Grade 1")+
  geom_vline(aes(xintercept=16),
             color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=6),
             color="black", linetype="dashed", size=1)

Anal_2.3.1=ggplot(sample_data_edit, aes(x=Grade_2,fill=Fath_high_edu)) + 
  geom_density(alpha=0.4)+
  ggtitle("Father eduaction level vs Grade 2")+
  geom_vline(aes(xintercept=16),
             color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=6),
             color="black", linetype="dashed", size=1)

#FATHER_education level does not affect their grade
ggarrange(Anal_2.3,Anal_2.3.1, 
          ncol = 2, nrow = 1)

Anal_2.3.3=subset(sample_data_edit,select = c(Fath_high_edu,Grade_1,Grade_2))

alpha=Anal_2.3.3

alpha %>%
  group_by(Fath_high_edu) %>%
  summarise_at(vars(Grade_1,Grade_2), list(name = mean))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 2 Analysis 4
#The relationship between custody and Grades of student

Anal_2.4=ggplot(data=sample_data_edit,aes(x=Custody,y=Grade_1,fill=Custody))+
  geom_boxplot(aes(colour=Custody,alpha=0.4))+
  ggtitle("Custody of students level vs Grade 1")

Anal_2.4.1=ggplot(data=sample_data_edit,aes(x=Custody,y=Grade_2,fill=Custody))+
  geom_boxplot(aes(colour=Custody,alpha=0.4))+
  ggtitle("Custody of students level vs Grade 2")

Anal_2.4.2=ggplot(data=sample_data_edit,aes(x=Custody,y=Grade_3,fill=Custody))+
  geom_boxplot(aes(colour=Custody,alpha=0.4))+
  ggtitle("Custody of students level vs Grade 3")

Anal_2.4.3=ggarrange(Anal_2.4,Anal_2.4.1,Anal_2.4.2,
                     ncol = 3, nrow = 1)

#Custody
#their mother perform best while students under neither of their parents performed the worst.
annotate_figure(Anal_2.4.3, top = text_grob("The relationship between custody and Grades of student"
                                            ,color = "black", face = "bold", size = 14))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 3
#Which gender has a better grade ?
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 1
#Finding out the relationship  between Gender and Grades

Anal_3.1=subset(sample_data_edit,select =c(Gender,Grade_1,Grade_2,Grade_3))

alpha=Anal_3.1

#male studet has a better grade
alpha %>%
  group_by(Gender) %>%
  summarise_at(vars(Grade_1), list(mean = mean))
paste("Female vs Male Student Grade 1 mean")

alpha %>%
  group_by(Gender) %>%
  summarise_at(vars(Grade_2), list(mean = mean))
paste("Female vs Male Student Grade 2 mean")

alpha %>%
  group_by(Gender) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("Female vs Male Student Grade 3 mean")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 2
#The relationship between Gender and custody with grades?

#Yes,male student under mother perform the best
ggplot(sample_data_edit,aes(x=Grade_1,y=Grade_2))+
  geom_point(aes(colour=Gender,size=Gender))+
  geom_smooth(method = "lm", se = FALSE,aes(linetype=Custody))+
  geom_hline(yintercept = 15,colour='black')+
  geom_vline(xintercept= 15,colour='black')+
  geom_hline(yintercept = 5,colour='red')+
  geom_vline(xintercept= 5,colour='red')+
  annotate("rect", fill='red',xmin = 15, xmax = 20, ymin = 15, ymax = 20,alpha = .2)+
  annotate("rect", fill='purple',xmin = 0, xmax = 5, ymin = 0, ymax = 5,alpha = .2)+
  ggtitle("The relationship between Gender and custody with grades")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 3
#The relationship between Gender,custody and family relationship.


sample_data_edit$Family_Relationship=as.numeric(sample_data_edit$Family_Relationship)

Anal_3.3=subset(sample_data_edit,Family_Relationship>3&Gender=="F",select = c(Gender,Custody,Family_Relationship,Grade_3))
Anal_3.3.1=subset(sample_data_edit,Family_Relationship>3&Gender=="M",select = c(Gender,Custody,Family_Relationship,Grade_3))

alpha=Anal_3.3
beta=Anal_3.3.1

#male gender benefits more when family relationship is high

alpha %>%
  group_by(Custody) %>%
  summarise_at(vars(Grade_3), list(name = mean))
paste("Female Student/High Family Relationship")

beta %>%
  group_by(Custody) %>%
  summarise_at(vars(Grade_3), list(name = mean))
paste("Male Student/High Family Relationship")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 4
#Find the relationship between gender and relationship with grades

sample_data_edit$Relationship=as.factor(sample_data_edit$Relationship)


Anal_3.4=ggplot(data = sample_data_edit,aes(x=Relationship,y=Grade_2,fill=Relationship))+
  geom_histogram(stat="summary",fun=mean,aes(colour=Relationship))+
  ggtitle("The relationship between gender and relationship with Grade 2")+
  facet_wrap(~Gender)

Anal_3.4.1=ggplot(data = sample_data_edit,aes(x=Relationship,y=Grade_3,fill=Relationship))+
  geom_histogram(stat="summary",fun=mean,aes(colour=Relationship))+
  ggtitle("The relationship between gender and relationship with Grade 3")+
  facet_wrap(~Gender)

Anal_3.4.2=ggarrange(Anal_3.4,Anal_3.4.1,
                     ncol = 2, nrow = 1)
#relationship do affect.Affects female more than male
annotate_figure(Anal_3.4.2, top = text_grob("The relationship between gender and relationship with Grades"
                                            ,color = "black", face = "bold", size = 14))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 5
#Find the relationship between gender,relationship and interlude

#the male students who has no relationship needs more free time to score distinction then female students.
#Male students majority need 5 hours while female need only 3 hrs
ggplot(sample_data_edit,aes(x=Grade_1,y=Grade_2))+
  geom_point(aes(colour=Interlude,size=Interlude))+
  geom_smooth(method = "lm", se = FALSE,aes(linetype=Relationship))+
  ggtitle("Find the relationship between gender,relationship and interlude with Grade 1 and 2")+
  geom_hline(yintercept = 15,colour='black')+
  geom_vline(xintercept= 15,colour='black')+
  annotate("rect", fill='blue',xmin = 15, xmax = 20, ymin = 15, ymax = 20,alpha = .2)+
  facet_wrap(~Gender)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 6
#The relationship between gender,interlude and revision time


Anal_3.6=subset(sample_data_edit,Revison_time>3&Interlude<3,select = c(Gender,Revison_time,Interlude,Grade_3))
Anal_3.6.1=subset(sample_data_edit,Revison_time<3&Interlude>3,select = c(Gender,Revison_time,Interlude,Grade_3))

alpha=Anal_3.6
beta=Anal_3.6.1

#students need a higher revision time can outperfrom better 
alpha %>%
  group_by(Gender) %>%
  summarise_at(vars(Grade_3), list(name = mean))
paste("High Revison time &Interlude")

beta %>%
  group_by(Gender) %>%
  summarise_at(vars(Grade_3), list(name = mean))
paste("Low Revison time &Interlude")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 7
#Find the relationship does fitness affect different gender in getting better grades

sample_data_edit$Gender=as.factor(sample_data_edit$Gender)

Anal_3.7=ggplot(sample_data_edit,aes(x=Fitness,y=Grade_2,))+
  geom_boxplot(aes(fill=Fitness))+
  ggtitle("Find the relationship between Fitness with Grade 2")+
  facet_wrap(~Gender)

Anal_3.7.1=ggplot(sample_data_edit,aes(x=Fitness,y=Grade_3,))+
  geom_boxplot(aes(fill=Fitness))+
  ggtitle("Find the relationship between Fitness with Grade 3")+
  facet_wrap(~Gender)

Anal_3.7.2=ggarrange(Anal_3.7,Anal_3.7.1,
                     ncol = 1, nrow = 2)

#fitness does not affect.
annotate_figure(Anal_3.7.2, top = text_grob("The relationship does fitness affect different gender in getting grades"
                                            ,color = "black", face = "bold", size = 14))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 8
#Find the relationship does Fitness and Day_alc affects the grade.

sample_data_edit$Day_alc=as.numeric(sample_data_edit$Day_alc)

Anal_3.8=subset(sample_data_edit,Fitness>3 & (Day_alc<3) & Grade_3>16,select = c(Gender,Fitness,Day_alc,Grade_3))
Anal_3.8.1=subset(sample_data_edit,Fitness<3 & (Day_alc>3) & Grade_3>6,select = c(Gender,Fitness,Day_alc,Grade_3))

#high_day_alc does affect student and affects male more
aggregate(Anal_3.8$Grade_3, by=list(Anal_3.8$Gender), FUN=mean)
paste("High Fitness & Low Day_alc")

aggregate(Anal_3.8.1$Grade_3, by=list(Anal_3.8.1$Gender), FUN=mean)
paste("Low Fitness & High day_alc")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 9
#Find the relationship between Gender,Fitness and Week_alc

sample_data_edit$Week_alc=as.numeric(sample_data_edit$Week_alc)


Anal_4.9=subset(sample_data_edit,Fitness>3 & (Week_alc<3) & Grade_3>16,select = c(Gender,Week_alc,Fitness,Grade_3))
Anal_4.9.1=subset(sample_data_edit,Fitness<3 & (Week_alc>3) & Grade_3>6,select = c(Gender,Week_alc,Fitness,Grade_3))

#high_weeeknd_alc does affect student and affects female more
aggregate(Anal_4.9$Grade_3, by=list(Anal_4.9$Gender), FUN=mean)
paste("High Fitness & Low Week_alc")

aggregate(Anal_4.9.1$Grade_3, by=list(Anal_4.9.1$Gender), FUN=mean)
paste("Low Fitness & High Week_alc")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 10
#The relationship between Gender and Further_edu

#does not matter when comes to gender

Anal_3.10=ggplot(sample_data_edit,aes(x=Grade_1))+
  geom_bar(stat='count',position=position_dodge(),aes(fill=Further_edu))+
  geom_vline(xintercept= 15,colour='black',linetype="dashed")+
  geom_vline(xintercept= 5,colour='purple',linetype="dashed")+
  ggtitle("Find the relationship between Gender and Further_edu with Grade 1")+
  facet_wrap(~Gender)

Anal_3.10.1=ggplot(sample_data_edit,aes(x=Grade_2))+
  geom_bar(stat='bin',position=position_dodge(),aes(fill=Further_edu))+
  geom_vline(xintercept= 15,colour='black',linetype="dashed")+
  geom_vline(xintercept= 5,colour='purple',linetype="dashed")+
  ggtitle("Find the relationship between Gender and Further_edu with Grade 2")+
  facet_wrap(~Gender)



Anal_3.10.2=ggarrange(Anal_3.10+coord_flip(),Anal_3.10.1+coord_flip(),
                      ncol = 1, nrow = 2)

annotate_figure(Anal_3.10.2, top = text_grob("The relationship between gender and further studies"
                                             ,color = "purple", face = "bold", size = 14))



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 11
#The relationship between Gender and reason



sample_data_edit$Reason=as.factor(sample_data_edit$Reason)

Anal_3.11=ggplot(sample_data_edit,aes(x=Reason,y=Grade_2,))+
  geom_boxplot(aes(fill=Reason))+
  ggtitle("Find the relationship between Reason and Gender with Grade 2")+
  facet_wrap(~Gender)


Anal_3.11.1=ggplot(sample_data_edit,aes(x=Reason,y=Grade_3,))+
  geom_boxplot(aes(fill=Reason))+
  ggtitle("Find the relationship between Reason and Gender with Grade 3")+
  facet_wrap(~Gender)

Anal_3.11.2=ggarrange(Anal_3.11,Anal_3.11.1,
                      ncol = 1, nrow = 2)

#Male stuent who choose reputaion perfrom the best while for female it
#is others.For both gender choosing course would lead you to scoring
#lower ing Grade_2.

annotate_figure(Anal_3.11.2, top = text_grob("#The relationship between Gender and reason"
                                             ,color = "purple", face = "bold", size = 14))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 4
#Which Peer_group perform the best in the Grades?
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 1

#Student age 20 perfrom the best overapp and 21 and 22 are the worst
ggplot(data = sample_data_edit,aes(x=Peer_group,y=Grade_1,fill=Peer_group))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Peer_group  vs G1")

ggplot(data = sample_data_edit,aes(x=Peer_group,y=Grade_2,fill=Peer_group))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Peer_group vs G2")

ggplot(data = sample_data_edit,aes(x=Peer_group,y=Grade_3,fill=Peer_group))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Peer_group vs G3")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 2
#The relationship between Peer_group and Time_with Fri

#Time with friends is does not affect student age group 20  from being the best however it affects student age
#15-19.

Anal_4.1=subset(sample_data_edit,Time_with_Fri>3,select =c(Peer_group,Time_with_Fri,Grade_1,Grade_2,Grade_3))
Anal_4.1.1=subset(sample_data_edit,Time_with_Fri<3,select =c(Peer_group,Time_with_Fri,Grade_1,Grade_2,Grade_3))

Anal_4.1.2=ggplot(Anal_4.1,aes(x=Peer_group,y=Grade_1))+
  geom_point(aes(colour=Time_with_Fri))+
  geom_hline(yintercept = 15,colour='black',linetype='dashed')+
  ggtitle("Peer_group/HIGH Time_with_Fri vs Grade_1")

Anal_4.1.3=ggplot(Anal_4.1.1,aes(x=Peer_group,y=Grade_1))+
  geom_point(aes(colour=Time_with_Fri))+
  geom_hline(yintercept = 15,colour='black',linetype='dashed')+
  ggtitle("Peer_group/LOW Time_with_Fri vs Grade_1")

Anal_4.1.4=ggarrange(Anal_4.1.2,Anal_4.1.3,
                     ncol = 1, nrow = 2)

annotate_figure(Anal_4.1.4, top = text_grob("The relationship between Peer_group and Time_with Fri"
                                            ,color = "blue", face = "bold", size = 14))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 3
#The relationship between Peer_group and Internet

#Internet does help

Anal_4.3=subset(sample_data_edit,Peer_group==20,select =c(Peer_group,Internet,Grade_1,Grade_2,Grade_3))
Anal_4.3.1=subset(sample_data_edit,Peer_group==21,select =c(Peer_group,Internet,Grade_1,Grade_2,Grade_3))

Alpha=Anal_4.3
Beta=Anal_4.3.1

Alpha %>%
  group_by(Internet) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("STUDENT PEER_GROUP=20")

Beta %>%
  group_by(Internet) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("STUDENT PEER_GROUP=21")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 4
#The relationship between Peer_group,Internet and Finance

#Finance dont help student age 20 but help student age 15-19

Anal_4.4=subset(sample_data_edit,Internet=="yes",select =c(Peer_group,Internet,Finance,Grade_1,Grade_2,Grade_3))

ggplot(data = sample_data_edit,aes(x=Peer_group,y=Grade_3,fill=Finance))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Finance/Peer_group&Internet vs G3")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 5
#The relationship between Peer_group,Pre_school

#Pre_school does  help student age in achieving higher score 


ggplot(data=sample_data_edit,aes(x=Peer_group,y=Grade_1),fill=Preschool)+
  geom_point(aes(colour=Preschool,size=Preschool))+
  geom_hline(yintercept = 15,colour='black',linetype='dashed')+
  ggtitle("Peer_group&Pre_school vs G1")

ggplot(data=sample_data_edit,aes(x=Peer_group,y=Grade_2),fill=Preschool)+
  geom_point(aes(colour=Preschool,size=Preschool))+
  geom_hline(yintercept = 15,colour='black',linetype='dashed')+
  ggtitle("Peer_group&Pre_schoolvs G2")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 6
#The relationship between Peer_group,Pre_school and Further_edu
#Further education does matter,reason why student age 20 do so well

Anal_4.6=subset(sample_data_edit,Preschool=='yes',select =c(Peer_group,Preschool,Further_edu,Grade_1,Grade_2,Grade_3))
Anal_4.6.1=mutate(Anal_4.6,Total_mean=(Grade_1+Grade_2+Grade_3/3))

Alpha=Anal_4.6.1
Alpha %>%
  group_by(Peer_group,Further_edu) %>%
  summarise_at(vars(Total_mean), list(mean = mean))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 5:Which school students  perform better in the exams ?
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 1
Anal_5.1=subset(sample_data_edit,select =c(School,Grade_1,Grade_2,Grade_3))

#MS STUDENT BETTER OVERALL

alpha=Anal_5.1

alpha %>%
  group_by(School) %>%
  summarise_at(vars(Grade_1), list(mean = mean))
paste("GP Vs MS Student Grade 1 mean")

alpha %>%
  group_by(School) %>%
  summarise_at(vars(Grade_2), list(mean = mean))
paste("GP Vs MS Student Grade 2 mean")

alpha %>%
  group_by(School) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("GP Vs MS Student Grade 3 mean")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 2
#Find the relationship between duration and school with grades

#The further the lower the grades
#but this does not affect ms student

Anal_5.2=ggplot(sample_data_edit,aes(x=Duration,fill=School))+
  geom_bar(stat='count',position = position_dodge())+
  ggtitle("Frequency of different school students against Duration")

Anal_5.2.1=ggplot(data = sample_data_edit,aes(x=Duration,y=Grade_1,fill=School))+
  geom_histogram(stat="summary",position = position_dodge(),fun=mean)+
  ggtitle("Duration vs Grade_1")


Anal_5.2.2=ggplot(data = sample_data_edit,aes(x=Duration,y=Grade_2,fill=School))+
  geom_histogram(stat="summary",position = position_dodge(),fun=mean)+
  ggtitle("Duration vs Grade_2")


Anal_5.2.3=ggplot(data = sample_data_edit,aes(x=Duration,y=Grade_3,fill=School))+
  geom_histogram(stat="summary",position = position_dodge(),fun=mean)+
  ggtitle("Duration vs Grade_3")

Anal_5.2.4=ggarrange(Anal_5.2,Anal_5.2.1,Anal_5.2.2,Anal_5.2.3,
                     ncol = 2, nrow = 2)

annotate_figure(Anal_5.2.4, top = text_grob("Find the relationship between duration and school with grades"
                                            ,color = "black", face = "bold", size = 14))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 3
#Find the relationship between school,duration and interlude

#Students of interlude of 2 and stay less than 3 hrs from school achieve a higher grade

Anal_5.3=subset(sample_data_edit,Duration<3&School=="GP",select =c(School,Duration,Interlude,Grade_3))
Anal_5.3.1=subset(sample_data_edit,Duration<3&School=="MS",select =c(School,Duration,Interlude,Grade_3))

alpha=Anal_5.3
beta=Anal_5.3.1

alpha %>%
  group_by(Interlude) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("GP Vs MS Student Grade 3 mean")

beta %>%
  group_by(Interlude) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("GP Vs MS Student Grade 3 mean")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 4
#The relationship between school and School_support with Grade_1

#When given school support it can be seen thatt MS STUDENT benefits from it more
ggplot(data=sample_data_edit,aes(x=School_support,y=Grade_1,fill=School))+
  geom_boxplot()+
  ggtitle("School_support vs Grade_1")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 5
#The relationship between School,School_support and Family_backing compare with grades



Anal_5.5.1=subset(sample_data_edit,School_support=='yes'&Family_backing=='yes',select =c(School,School_support,Family_backing,Grade_3))
Anal_5.5.2=subset(sample_data_edit,School_support=='no'&Family_backing=='no',select =c(School,School_support,Family_backing,Grade_3))

alpha=Anal_5.5.1
beta=Anal_5.5.2

#When it comes to school_support and family backing ms students benefits more.
alpha %>%
  group_by(School) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("YES School_suport/Family_backing vs Grade_3")

beta %>%
  group_by(School) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("NO School_suport/Family_backing vs Grade_3")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 6
#The relationship  between School and Afte_sch_activities with their Grades


sample_data_edit$Afte_sch_activities

Anal_5.6.1=filter(sample_data_edit,Afte_sch_activities=='yes')
Anal_5.6.2=filter(sample_data_edit,Afte_sch_activities=='no')

alpha=Anal_5.6.1
beta=Anal_5.6.2

#YES AFTER SCHOOL ACTIVITES IS GP
#NO AFTER SCHOOL ACTIVITES IS MS

alpha %>%
  group_by(School) %>%
  summarise_at(vars(Grade_2), list(mean = mean))
paste("YES Afte_sch_activities")

beta %>%
  group_by(School) %>%
  summarise_at(vars(Grade_2), list(mean = mean))
paste("NO Afte_sch_activities")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 7
#The relationship between School,Afte_school_activites and Time_with_Fri with Grades

Anal_5.7.1=subset(sample_data_edit,Afte_sch_activities=='yes'&Time_with_Fri>3,select =c(School,Afte_sch_activities,Time_with_Fri,Grade_3))
Anal_5.7.2=subset(sample_data_edit,Afte_sch_activities=='yes'&Time_with_Fri<3,select =c(School,Afte_sch_activities,Time_with_Fri,Grade_3)) 

alpha=Anal_5.7.1
beta=Anal_5.7.2

#DOES not matter time_with friends cause as long as afte_sch_activites is yes
#gp will be higher

alpha %>%
  group_by(School) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("YES Afte_sch_activities,High Time_with_Fri")

beta %>%
  group_by(School) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
paste("YES Afte_sch_activities,Low Time_with_Fri")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 8
#The relationship between School,Time_with_Fri and Revison_time

#High_Revision_time helps Ms students in getting better result

sample_data_edit$Revison_time=as.numeric(sample_data_edit$Revison_time)
sample_data_edit$Interlude=as.numeric(sample_data_edit$Interlude)

Anal_5.8.1=subset(sample_data_edit,Time_with_Fri<3&Revison_time<3,select =
                    c(School,Afte_sch_activities,Time_with_Fri,Revison_time,Grade_2,Grade_3))

Anal_5.8.2=subset(sample_data_edit,Time_with_Fri<3&Revison_time>3,select =
                    c(School,Afte_sch_activities,Time_with_Fri,Revison_time,Grade_2,Grade_3))


Anal_5.8.3=ggplot(data = Anal_5.8.1,aes(x=School,y=Grade_3,fill=School))+
  geom_histogram(stat="summary",position = position_dodge(),fun=mean)+
  ggtitle("Low Time_with Fri/Low Revison_time")

Anal_5.8.4=ggplot(data = Anal_5.8.2,aes(x=School,y=Grade_3,fill=School))+
  geom_histogram(stat="summary",position = position_dodge(),fun=mean)+
  ggtitle("Low Time_with Fri/High Revison_time")


Anal_5.8.5=ggarrange(Anal_5.8.3,Anal_5.8.4,
                     ncol = 1, nrow = 2)

annotate_figure(Anal_5.8.5, top = text_grob("The relationship between School,Time_with_Fri and Revison_time"
                                            ,color = "black", face = "bold", size = 14))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 9
#The relationship between  School and Non_attendant

#I believe non-attedance does affect student badly
#-more GP students miss their classes compare to MS.


ggplot(sample_data_edit,aes(x=Non_attendant))+
  geom_point(stat='count',position = "jitter",aes(size=School,colour=School))+
  ggtitle("Non_attedant vs School")+
  geom_vline(xintercept=75,colour='black',linetype='dashed')+
  geom_vline(xintercept=25,colour='purple',linetype='dashed')

Anal_5.9.1=subset(sample_data_edit,School=='GP'&(Non_attendant==0|Non_attendant==75),select=c(School,Non_attendant,Grade_1,Grade_2,Grade_3))
Anal_5.9.2=subset(sample_data_edit,School=='MS'&(Non_attendant==0|Non_attendant==25),select=c(School,Non_attendant,Grade_1,Grade_2,Grade_3))


View(Anal_5.9.2)
Anal_5.9.3=mutate(Anal_5.9.1,Total_mean=(Grade_1+Grade_2+Grade_3/3))
Anal_5.9.4=mutate(Anal_5.9.2,Total_mean=(Grade_1+Grade_2+Grade_3/3))

alpha=Anal_5.9.3
beta=Anal_5.9.4

alpha %>%
  group_by(Non_attendant) %>%
  summarise_at(vars(Total_mean), list(mean = mean))%>%
  print(n=40)

beta %>%
  group_by(Non_attendant) %>%
  summarise_at(vars(Total_mean), list(mean = mean))%>%
  print(n=40)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 10
#The relationship between  school and Internet 

#Internet does matter.So when we look depper it can be seen that it affect ms student more

ggplot(data = sample_data_edit,aes(x=Internet,y=Grade_2,fill=School))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("The relationship between  school and Internet  with Grade 2")

ggplot(data = sample_data_edit,aes(x=Internet,y=Grade_3,fill=School))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("The relationship between  school and Internet  Grade 3")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 11
#The relationship between  school,internet and finance

#Having both help student overall.But it can be seen that we both attribuite ms student can
#score higher than gp student.

ggplot(data = sample_data_edit,aes(x=Internet,y=Grade_2,fill=Finance))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  facet_wrap(~School)+
  ggtitle("The relationship between  school,Internet and Finance  with Grade 2")

#Question 6:Students from which Demographic_area  perform the best in their exam ?
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 1

#Urban student performs the best.
ggplot(data = sample_data_edit,aes(x=Demographic_area,y=Grade_1,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Demographic_area vs G1")

ggplot(data = sample_data_edit,aes(x=Demographic_area,y=Grade_2,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Demographic_area vs G2")

ggplot(data = sample_data_edit,aes(x=Demographic_area,y=Grade_3,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("Demographic_area vs G3")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 2
#The relationship  between  demographic area and Peer_Group

sample_data_edit$Demographic_area=as.factor(sample_data_edit$Demographic_area)

#21 years old and live-in urban area has a higher chance of getting high grades for their exams.
Anal_6.2=ggplot(sample_data_edit,aes(x=Peer_group,y=Grade_2))+
  geom_point(position = "jitter",aes(colour=Demographic_area))+
  geom_hline(yintercept = 15,colour='black',linetype='dashed')+
  ggtitle("Demographic_area/Peer_group vs Grade_2")

Anal_6.2.1=ggplot(sample_data_edit,aes(x=Peer_group,y=Grade_3))+
  geom_point(position = "jitter",aes(colour=Demographic_area))+
  geom_hline(yintercept = 15,colour='black',linetype='dashed')+
  ggtitle("Demographic_area/Peer_group vs Grade_3")

Anal_6.2.2=ggarrange(Anal_6.2,Anal_6.2.1,
                     ncol = 1, nrow = 2)

annotate_figure(Anal_6.2.2, top = text_grob("The relationship  between  demographic area and Peer_Group"
                                            ,color = "black", face = "bold", size = 14))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 3
#The relationship  between  demographic area,Peer_Group and Gender

#Peer group 20 is the best.Female students livin in urban does better than male
Anal_6.3=subset(sample_data_edit,Peer_group==20,select =c(Demographic_area,Peer_group,Gender,Grade_3))

alpha=Anal_6.3

alpha %>%
  group_by(Demographic_area,Gender) %>%
  summarise_at(vars(Grade_3), list(mean = mean))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 4
#The relationship between  demographic area and  Num_family

#Num_family family attributes does affect the overall grade of students.
#It can be seen it affecting rural students more than urban students

Anal_6.4=ggplot(data = sample_data_edit,aes(x=Num_family,y=Grade_2,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("The relationship between  demographic area and  Num_family  with Grade 2")

Anal_6.4.1=ggplot(data = sample_data_edit,aes(x=Num_family,y=Grade_3,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("The relationship between  demographic area and  Num_family  with Grade 3")

Anal_6.4.2=ggarrange(Anal_6.4,Anal_6.4.1,
                     ncol = 1, nrow = 2)

annotate_figure(Anal_6.4.2, top = text_grob("The relationship  between  demographic area and Num_family"
                                            ,color = "black", face = "bold", size = 14))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 5
#The relationship between demographic area,Num_family and Family relationship

#Family relationship does not affect average of the students
Anal_6.5=subset(sample_data_edit,Family_Relationship>3&Num_family=="LE3",select =c(Demographic_area,Num_family,Family_Relationship,Grade_1))
Anal_6.5.1=subset(sample_data_edit,Family_Relationship<3&Num_family=="LE3",select =c(Demographic_area,Num_family,Family_Relationship,Grade_1))


alpha=Anal_6.5
beta=Anal_6.5.1

alpha %>%
  group_by(Demographic_area) %>%
  summarise_at(vars(Grade_1), list(mean = mean))

beta%>%
  group_by(Demographic_area) %>%
  summarise_at(vars(Grade_1), list(mean = mean))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 6
#The relationship between  Demographic_area and Num_fail

#It can be said that number of fails does help urban student get
#a higher overall.It can be seen as urban students majority have not
#failed before.

Anal_6.6=ggplot(data=sample_data_edit,aes(x=Num_Fail,fill=Demographic_area))+
  geom_bar(stat="count", width=0.7,position = position_dodge())+
  ggtitle("The frequency of Num_fail for Demographic-area")


Anal_6.6.1=ggplot(data = sample_data_edit,aes(x=Num_Fail,y=Grade_3,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  ggtitle("The relationship between  Demographic_area and Num_fail  with Grade 2")

Anal_6.6.2=ggarrange(Anal_6.6,Anal_6.6.1,
                     ncol = 1, nrow = 2)

annotate_figure(Anal_6.6.2, top = text_grob("The relationship between  Demographic_area and Num_fail"
                                            ,color = "black", face = "bold", size = 14))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 7
#The relationship between demographic area and Mot_occ

#Urban students high=health lowest would be woring from home
#Rural students high =teacher losesst would be health

ggplot(data = sample_data_edit,aes(x=Mot_occ,y=Grade_1,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  labs(y= "Mean of Grade_1", x = "Mot_occ")+
  ggtitle("The relationship between demographic area and Mot_occ for Grade_1")


ggplot(data = sample_data_edit,aes(x=Mot_occ,y=Grade_2,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  labs(y= "Mean of Grade_2", x = "Mot_occ")+
  ggtitle("The relationship between demographic area and Mot_occ for Grade_2")  
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Analysis 8
#The relationship between demographic area Mot_occ and Fat_occ

#mother is health for father,services or also health 

Anal_6.8=subset(sample_data_edit,Mot_occ=='health',select =c(Demographic_area,Mot_occ,Fat_occ,Grade_1,Grade_2,Grade_3))

ggplot(data = Anal_6.8,aes(x=Fat_occ,y=Grade_1,fill=Demographic_area))+
  geom_histogram(stat="summary",fun=mean,position = position_dodge())+
  labs(y= "Mean of Grade_1", x = "Fat_occ")+
  theme_dark()+
  ggtitle("The relationship between demographic area and Mot_occ for Grade_1")

