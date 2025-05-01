switch(2,"red","green","blue")


i <- 1

while (i <= 5) {
  print(i)
  
  i <- i + 1
}

i<-0
while(i<6){
  i<- i+1
  if(i==3){
    break
  }
  print(i)
 
}

i<-0
while(i<6){
  
  if(i==3){
    break
  }
  print(i)
  i<- i+1
}


i<-0
while(i<6){
  i<- i+1
  if(i==3){
    next
  }
  print(i)
}

for(x in 1:10){
  print(x)
}

for(x in 1:2){
  for(y in 1:2)(
    print(x+y)
  )
}

addNumber <- function(a,b){
  sum<- a+b
  return(sum)
}
print(addNumber(4,5))

#vector type
a <- c(1,2,3,4,5,6,7,8,9,10)
b <- c("red","green","blue")
c <- c(TRUE,FALSE,FALSE,TRUE,TRUE)

min(a)#minimum
max(a)#maximum
mean(a)#mean value
sqrt(a)#square root
abs(a)#return + value
length(a)
nchar(b)#print length

a+1

#Sort a Vector
X<-c(23,9,67,9,12,4)
sort(X)#Accending

sort(X, decreasing = TRUE)#Desending

#Access Vector
#here vector index start from 1
X[1]
X[1:3]
X[c(1,3,5)]
X[c(1:3,9)]
X[c(1:10)]


#Matrix
?matrix
mdat <- matrix(c(1:20), nrow = 5, ncol = 4)
mdat

mdat1 <- matrix(c(1:20), nrow = 5, ncol = 4, byrow = TRUE)
mdat1

cell<- c(1:4)
rname<- c("R1","R2")
cname<- c("C1","C2")

mdat3 <- matrix(cell, nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(rname,cname))
mdat3

mdat3[2,]#row access
mdat3[,2]#column access

mdat3[1,2]#access at(1,2)1 row 2 column

mdat3[1,c(1,2)]#access row 1 column 1,2

mdat3[,c(1,2)]#access column 1,2

#array
?array

array1 <- array(1:18,c(2,3,4))
array1
#data frame
?data.frame

studentID<- c(1:5)
age<- c(20,25,23,19,31)
name<- c("AZMINUR","ASHANUR","LIAKAT","MONOARA","SANIA")

studentData<-data.frame(studentID,age,name)
View(studentData)
