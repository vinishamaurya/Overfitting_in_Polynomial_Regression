#=============================================================================================
# OVERFITTING DUE TO INCREASE IN NUMBER OF FEATURES IN LINEAR REGRESSION
#=============================================================================================
file=read.csv('C://Users//Vinay//Downloads//cars.csv')
nrow(file)
names(file)


set.seed(0)
rand = sample(1:nrow(file),350)
train = file[rand, ]
test = file[-rand, ]

#ploting regression line for different sample size from train

l1=list()
lis<-c(20,30,40,50,70,100,200,350)
lis
for (i in lis) {
  set.seed(0)
  rand = sample(1:nrow(train),i)
  train1 = file[rand, ]
  m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
           + I(Weight^6) + I(Weight^7), train1)
  print(m7)
  file_name = paste("C:/Users/Vinay/Downloads/rcodes/image/Sample_Size_", i, ".pdf", sep="")
  pdf(file_name)
  plot(train1$Weight,train1$MPG,col='blue', pch=19, cex=0.5,xlab = 'Weight',ylab='Miles Per Gallon')
  lines(sort(train1$Weight), fitted(m7)[order(train1$Weight)], col='brown', type='l',pch=20) 
  title(paste(main = 'Polynomial regression (Order-7) of Sample Size:',i,sep=''))
  jpeg(file = paste("C:/Users/Vinay/Downloads/rcodes/image/original/Sample_Size_",i,".jpeg", sep = ""))
  plot(train1$Weight,train1$MPG, pch=19, cex=0.5,col='blue',xlab = 'Weight',ylab = "Miles Per Gallon")
  lines(sort(train1$Weight), fitted(m7)[order(train1$Weight)], col='red', type='l',pch=20) 
  title(paste(main = 'Polynomial regression (Order-7) of Sample Size: ',i,sep=''))
  legend("topright", legend=c("Spread of Data", "Regression line"),
         col=c("blue", "red"), lty=c(NA,1),pch=c('*',NA), cex=0.8)
  
  dev.off()
  pred = predict(m7, newdata=test)
  b=sum((pred-test$MPG)^2)
  
  l1<-(c(l1,b))
  print(b)
  
}

print(l1)
#============================================================================
#  Ploting Test Error vs Sample Size Graph for order - 7
#=============================================================================

jpeg(file = "C:/Users/Vinay/Downloads/rcodes/image/Sample_Size_testerror.jpeg")
plot(lis,l1, pch=19, cex=0.5,type='l',xlab = 'Sample Size',ylab = 'Test Error')
title(main = 'Test Error vs Sample Size (Order-7)')
dev.off()


#============================================================================
# Poly regression line for 4 random samples taken from sample size 20 and 100
#============================================================================
lis2=c(1,2,3,4,5,6,7,8,9,10)
lis2

rand = sample(1:nrow(train),20)
train1 = file[rand, ]

l2=list()
l3=list()

x=train1['MPG'] 
x_test=test['MPG']
class(x)
class(x_test)
dim(x)
x_test
x['ll']<-(train1['Weight'])
colnames(x_test)

for (j in c(20,100)) {
  s=c(1,4,10,12)
  for (k in s) {
    set.seed(k)
    rand = sample(1:nrow(train),100)
    train1 = file[rand, ]
    x=train1['MPG']  
    x_test=test['MPG']
    l2=list()
    l3=list()
    for (i in lis2) {
      #set.seed(i+1)
      
      x=cbind(x,x[col]<-(train1['Weight']^i))
      colnames(x)<-(c('MPG',paste("Weight",1:i, sep = "")))
      #dim(x)
      m2 <- lm(MPG ~., data=x)
      print(m2)
      a=(sum(m2$residuals^2)^(1/2))
      print(a)
      x_test=cbind(x_test,x_test[col]<-(test['Weight']^i))
      colnames(x_test)<-(c('MPG',paste("Weight",1:i, sep = "")))
      pred = predict(m2, newdata=x_test)
      b2=(sum((pred-test$MPG)^2)^(1/2))
      print(b)
      l2<-c(l2,b2)
      l3<-c(l3,a)
      
    }
    if (k==1) {
      plot(lis2, l2, pch=19, cex=0.5,type='l',col='brown',xlab = 'Complexity',ylab = 'Test RMSE')
     
    }
    if (k==4) {
      lines(lis2, l2, col='red', type='l') 
    }
    if (k==10) {
      lines(lis2, l2, col='green', type='l') 
    }
    if (k==12) {
      lines(lis2, l2, col='blue', type='l') 
    }
    title(paste(main = 'Test RMSE vs Complexity Plot of Sample Size:100 (different random sample with replacement) ',sep=''))
    legend("topleft", legend=c("Test RMSE of Sample-1 ", "Test RMSE of Sample-2","Test RMSE of Sample-3","Test RMSE of Sample-4"),
           col=c("brown", "red","green","blue"), lty=1:2, cex=0.8)
    
    
  }
  
  
}


#===========================================================
#        Ploy Regression Plot for Sample size: 20,100
#===========================================================

lis3=c(20,100)
for (j in lis3){
  lis2=c(1,2,3,4,5,6,7,8,9)
  lis2
  
  rand = sample(1:nrow(train),j)
  train1 = file[rand, ]
  
  l2=list()
  l3=list()
  
  x=train1['MPG']  
  x_test=test['MPG']
  
  for (i in lis2) {
    #set.seed(i+1)
    
    x=cbind(x,x[col]<-(train1['Weight']^i))
    colnames(x)<-(c('MPG',paste("Weight",1:i, sep = "")))
    #dim(x)
    m2 <- lm(MPG ~., data=x)
    print(m2)
    a=(sum(m2$residuals^2)^(1/2))
    print(a)
    x_test=cbind(x_test,x_test[col]<-(test['Weight']^i))
    colnames(x_test)<-(c('MPG',paste("Weight",1:i, sep = "")))
    pred = predict(m2, newdata=x_test)
    b2=(sum((pred-test$MPG)^2)^(1/2))
    print(b)
    #plot.new()
    if (i==1) {
      plot(sort(x$Weight1), fitted(m2)[order(x$Weight1)], pch=19, cex=0.5,type='l',xlab = 'Weight',ylab = 'Miles Per Gallon')
      
    }
    if (i==2) { 
      lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='red', type='l')
    }
    if (i==7) {
      lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='green', type='l')
      
    }
    if (i==8) {
      lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='blue', type='l') 
      
    }
    if (i==9) {
      lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='pink', type='l') 
      
    }
    title(paste(main = 'Polynomial regression lines of Sample Size: ',j,sep = ''))
    legend("topright", legend=c("Regression line of degree 1", "Regression line of degree 2","Regression line of degree 7","Regression line of degree 8","Regression line of degree 9"),
           col=c("black", "red","green","blue","pink"), lty=1:2, cex=0.8)
    
    l2<-c(l2,b2)
    l3<-c(l3,a)
    
  }
  
  #jpeg(file = paste("C:/Users/Vinay/Downloads/rcodes/image/rmse_plot_for_sample",j,".jpeg",sep=''))
  #plot(lis2, l2, pch=19, cex=0.5,type='l',col='blue',xlab = 'Complexity',ylab = 'RMSE')
  #lines(lis2, l3, col='red', type='l') 
  #title(paste(main = 'RMSE vs Complexity Plot for Sample Size: ',j,sep=''))
  #legend("topleft", legend=c("RMSE of Test", "RMSE of Train"),
  #      col=c("blue", "red","green","blue","pink"), lty=1:2, cex=0.8)
  
  #dev.off()
}

#==================================================================================
#         Test-Train RMSE vs Complexity plot Sample Size: 20 and 100
#==================================================================================
lis3=c(20,100)
for (j in lis3){
  lis2=c(1,2,3,4,5,6,7,8,9)
  lis2
  
  rand = sample(1:nrow(train),20)
  train1 = file[rand, ]
  
  l2=list()
  l3=list()
  
  x=train1['MPG']  
  x_test=test['MPG']
  
  
  for (i in lis2) {
    #set.seed(i+1)
    
    x=cbind(x,x[col]<-(train1['Weight']^i))
    colnames(x)<-(c('MPG',paste("Weight",1:i, sep = "")))
    #dim(x)
    m2 <- lm(MPG ~., data=x)
    print(m2)
    a=(sum(m2$residuals^2)^(1/2))
    print(a)
    x_test=cbind(x_test,x_test[col]<-(test['Weight']^i))
    colnames(x_test)<-(c('MPG',paste("Weight",1:i, sep = "")))
    pred = predict(m2, newdata=x_test)
    b2=(sum((pred-test$MPG)^2)^(1/2))
    print(b)
    
    l2<-c(l2,b2)
    l3<-c(l3,a)
    
  }
  
  jpeg(file = paste("C:/Users/Vinay/Downloads/rcodes/image/rmse_plot_for_sample",j,".jpeg",sep=''))
  plot(lis2, l2, pch=19, cex=0.5,type='l',col='blue',xlab = 'Complexity',ylab = 'RMSE')
  lines(lis2, l3, col='red', type='l') 
  title(paste(main = 'RMSE vs Complexity Plot for Sample Size: ',j,sep=''))
  legend("topleft", legend=c("RMSE of Test", "RMSE of Train"),
         col=c("blue", "red"), lty=1:2, cex=0.8)
  
  dev.off()
}

#==========================================================================
#    Poly regression line of diffrent samples of size 20 and 100
#==========================================================================
lis2=c(1,2,3,4,5,6,7,8,9,10)


for (j in c(20,100)) {
  s=c(1,4,10,12)
  for (k in s) {
    set.seed(k)
    rand = sample(1:nrow(train),100)
    train1 = file[rand, ]
    x=train1['MPG']  
    x_test=test['MPG']
    l2=list()
    l3=list()
    for (i in lis2) {
      #set.seed(i+1)
      
      x=cbind(x,x[col]<-(train1['Weight']^i))
      colnames(x)<-(c('MPG',paste("Weight",1:i, sep = "")))
      #dim(x)
      m2 <- lm(MPG ~., data=x)
      print(m2)
      a=(sum(m2$residuals^2)^(1/2))
      print(a)
      x_test=cbind(x_test,x_test[col]<-(test['Weight']^i))
      colnames(x_test)<-(c('MPG',paste("Weight",1:i, sep = "")))
      pred = predict(m2, newdata=x_test)
      b2=(sum((pred-test$MPG)^2)^(1/2))
      print(b)
      
      
      if (i==1) {
        plot(sort(x$Weight1), fitted(m2)[order(x$Weight1)], pch=19,col='brown', cex=0.5,type='l',xlab = 'Weight',ylab = 'Miles Per Gallon')  
      }
      if (i==2) {
        lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='red', type='l') 
      }
      if (i==7) {
        lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='green', type='l') 
      }
      if (i==8) {
        lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='blue', type='l') 
      }
      if (i==9) {
        lines(sort(x$Weight1), fitted(m2)[order(x$Weight1)], col='orange', type='l') 
      }
      title(paste(main = 'Polynomial regression lines of Sample Size: ',j,sep=''),xlab = 'Weight',ylab = 'Miles Per Gallon')
      legend("topright", legend=c("Reg. line of order 1", "Reg. line of order 2","Reg. line of order 7","Reg. line of order 8","Reg. line of order 9"),
             col=c("brown", "red","green","blue","orange"), lty=1:2, cex=0.8)
      l2<-c(l2,b2)
      l3<-c(l3,a)
      
    }
    
    
  }
  
  
}

