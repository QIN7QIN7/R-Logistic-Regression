options(max.print=100)#Set the maximum number of displayed lines for the print function
options(scipen = 200)#Cancel Scientific notation Display
library(ggplot2)
data("iris")
#Check the partitioning effect of x1 and x2
ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+
  geom_point(shape=I(1),size=I(3))+
  xlab("Petal.Length")+
  ylab("Petal.Width")+
  theme(axis.title.x = element_text(vjust = 1, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15))
# collinearity diagnosis
temp<-cor(iris[c(3,4)])
kappa(temp,exact = TRUE)#We need k<100
#Write cost function
Cost<-function(X,y,theta){
  return(-(1/length(y))*(y%*%(log(1/(exp(-X%*%theta)+1)))+(1-y)%*%(log(1-(1/(exp(-X%*%theta)+1))))))
}
#Build variables to store theta values for two lines
theta_record<-matrix(1,3,2)
rownames(theta_record)<-c("theta0","theta1","theta2")
colnames(theta_record)<-c("line1","line2")






# Divide the training set and testing set
train_sub = sample(nrow(iris),7.5/10*nrow(iris))
test_data =iris[-train_sub,]
iris = iris[train_sub,]

##Separate the first category (setosa)
#Construct x1, x2, and theta matrices
x1<-as.matrix(iris$Petal.Length)
x2<-as.matrix(iris$Petal.Width)
X<-cbind(1,x1,x2)
colnames(X)<-c("num1","col_1","col_4")
theta<-matrix(1,3,1)
rownames(theta)<-c("theta0","theta1","theta2")
#Process y with setosa as 1 and others as 0 as the standard
y<-t(as.matrix(iris$Species))
i <- 1
while (i <= length(y)) {
  print(i)
  if(y[i]=="setosa"){
    y[i]<-1
  }else{
    y[i]<-0
  }
  i <- i + 1
}
y<-as.numeric(y)

Cost(X,y,theta)

iterations<-100000
alpha<-0.001
J_history<-matrix(0,iterations,1)
epsilon <- 1e-9

for(i in 1:iterations){
  theta<-theta-alpha*t(X) %*% (1/(exp(-X%*%theta)+1)-y)
  J_history[i] <- Cost(X,y,theta)
  if (i>2){
    if(abs(J_history[i]-J_history[i-1])<=epsilon){
      print(i)
      print(J_history[i])
      break
    }
  }
}

Cost(X,y,theta)
theta_record[,1]<-theta

df <- data.frame(x=1:length(J_history), y=J_history)

ggplot(df, aes(x,y)) + labs(x="Number of iterations", y="Cost") +
  geom_line(aes(y=y, x=x)) + 
  scale_color_discrete(name="Learning Rate")+scale_x_log10()+ylim(0,NA)

ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+
  geom_point(shape=I(1),size=I(3))+
  xlab("1")+
  ylab("2")+
  theme(axis.title.x = element_text(vjust = 1, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15))+
  geom_abline(slope = -(theta[2]/theta[3]), intercept = -(theta[1]/theta[3]))








##Separate the secend category (versicolor)
#Delete setosa data, process y with versicolor set to 1 and others set to 0 as the standard
iris_no_setosa<-iris[iris$Species!= "setosa", ]
x1<-as.matrix(iris_no_setosa$Petal.Length)
x2<-as.matrix(iris_no_setosa$Petal.Width)
X<-cbind(1,x1,x2)
colnames(X)<-c("num1","col_1","col_4")
theta<-matrix(1,3,1)
rownames(theta)<-c("theta0","theta1","theta2")
y<-t(as.matrix(iris_no_setosa$Species))
i <- 1
while (i <= length(y)) {
  print(i)
  if(y[i]=="versicolor"){
    y[i]<-1
  }else{
    y[i]<-0
  }
  i <- i + 1
}
y<-as.numeric(y)

Cost(X,y,theta)

iterations<-100000
alpha<-0.001
J_history<-matrix(0,iterations,1)
epsilon <- 1e-9

for(i in 1:iterations){
  theta<-theta-alpha*t(X) %*% (1/(exp(-X%*%theta)+1)-y)
  J_history[i] <- Cost(X,y,theta)
  if (i>2){
    if(abs(J_history[i]-J_history[i-1])<=epsilon){
      print(i)
      print(J_history[i])
      break
    }
  }
}

Cost(X,y,theta)
theta_record[,2]<-theta

df <- data.frame(x=1:length(J_history), y=J_history)

ggplot(df, aes(x,y)) + labs(x="Number of iterations", y="Cost") +
  geom_line(aes(y=y, x=x)) + 
  scale_color_discrete(name="Learning Rate")+scale_x_log10()+ylim(0,NA)

ggplot(iris_no_setosa,aes(Petal.Length,Petal.Width,color=Species))+
  geom_point(shape=I(1),size=I(3))+
  xlab("1")+
  ylab("2")+
  theme(axis.title.x = element_text(vjust = 1, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15))+
  geom_abline(slope = -(theta[2]/theta[3]), intercept = -(theta[1]/theta[3]))






#Put all the data and all the lines together to display the results
theta_record
ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+
  geom_point(shape=I(1),size=I(3))+
  xlab("Petal.Length")+
  ylab("Petal.Width")+
  theme(axis.title.x = element_text(vjust = 1, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15))+
  geom_abline(slope = -(theta_record[2,1]/theta_record[3,1]), intercept = -(theta_record[1,1]/theta_record[3,1]))+
  geom_abline(slope = -(theta_record[2,2]/theta_record[3,2]), intercept = -(theta_record[1,2]/theta_record[3,2]))






#Check the effectiveness of the model
logistic_regression <- function(feature) {
  if(c(1,feature)%*%theta_record[,1]>0){
    return("setosa")
  }else if(c(1,feature)%*%theta_record[,2]>0) {
    return("versicolor")
  }else{
    return("virginica")
  }
}


#input：c(Sepal.Length,Petal.Width)
test=test_data[,c(3,4)]
test_real=as.character(test_data[,5])
succeed=0
fail=0

for(i in c(1:nrow(test_data))){
  predict<-logistic_regression(unlist(test[i,]))
  if(test_real[i]==predict){
    succeed<-succeed+1
  }else{
    fail<-fail+1
  }
}

#View accuracy rate
print("accuracy rate is:")
print(succeed/(succeed+fail))

