#data frame
?data.frame

studentID<- c(1:5)
age<- c(20,25,23,19,31)
name<- c("AZMINUR","ASHANUR","LIAKAT","MONOARA","SANIA")

studentData<-data.frame(studentID,age,name)

print(studentData)

#add a column in same frame
blood <- c("O+","A+","O+","A+","A+")
studentData <-cbind(studentData,blood)
print(studentData)

#add new row in this frame
studentData <- rbind(studentData,c(6,70,"JAMSER","O+"))
print(studentData)

#access
studentData["name"]
studentData[c("name","age")]



# list data
g<- "My List"
h<- c(25,26,18,29)
j<- matrix(1:10, nrow=5)
k<- c("one","two",'three')
mylist <- list(title=g,age=h,j,k)
print(mylist)
#access from list
mylist[[2]]



#user input
#by using readline
var1 = readline(prompt ="Enter any value: ")
var2 = readline(prompt = "Enter any number: ")

#readline take all input as string so need to convert the value
var2 = as.integer(var2)
print(var1)
print(var2)


#user input by using scan
scaninput<- scan()
#press enter 2 times for end this process
print(scaninput)

#input string by scan
inputString <- scan(what ="")
print(inputString)

#use of edit function
mydata<- data.frame(age= numeric(0),gender= character(0),weight = numeric(0))
mydata<-edit(mydata)
print(mydata)
#save frame data as .csv file
write.csv(mydata, file = "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/10-11-2024/mydata.csv", row.names = FALSE)


#import .csv file
importData <- read.csv("C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/10-11-2024/mobile_usage_behavioral_analysis.csv")
print(importData)

#selecting 5th to 10th row of the importData
importData[5:10,]
#selecting only 1 and 10 row
importData[c(1,10),]
#selecting 1 and 10 column
importData[,c(1,10)]



#selecting subset data
subset(importData,Location=="Chicago")

subset(importData,Age>=50)

