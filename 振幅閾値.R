amplitude_mean <- c()
for(k in 1:8){ 
  
  string <- paste("D:/analyze/0605/",k,"/CSV",sep = "")  #日付変更
  setwd(string)
  getwd()
  
  files <- list.files(path = string,full.names = T) 
  
  exclusionUntil <- 100   
  
  ###############################背景分の減算処理#################################
  
  bg <- paste("background.csv",sep = "")
  eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
  b <- background[,3]
  #############################葉の各点##############################
  amplitude <- c()
  count <- 0
  over <- 0
  
  for(i in 0:512){
    for(j in 0:512){
      if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
        count <- count + 1
        
        df <- paste("Result",i,"_", j, ".csv", sep = "")
        eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
        
        x <- data[,1]
        y <- data[,3] - b
        p <- c()
        t <- c()
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        
        for(k in exclusionUntil:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
          d1 <- sp$y[k+1]-sp$y[k]
          d2 <- sp$y[k+2]-sp$y[k+1]
          if((d1>=0) && (d1*d2<0)){
            p <- append(p,sp$y[k+1])
          }
          else if((d1<=0) && (d1*d2<0)){
            t <- append(t,sp$y[k+1])
          }
        }
        #print(p)
        #print(t)
        if((mean(p)-mean(t))>40){
          over <- over + 1
        }
        amplitude <- append(amplitude,mean(p) - mean(t))
        #print(amplitude)
      }
    }
  }
  amplitude_mean <- append(amplitude_mean,mean(amplitude))
  print(amplitude_mean)
  print(paste(over,"/",count))
}
index <- rep(0,8)
plot(index,over/count,xlab = "",ylab = "amplitude",xlim = c(0,4),ylim = c(0,1))
par(new=T)
#write.csv(amplitude_mean,"../../amplitude.CSV")

#######################################################################################################

amplitude_mean <- c()
for(k in 0:18){ 
  
  string <- paste("D:/analyze/0722/",k,"/CSV",sep = "")  #日付変更
  setwd(string)
  getwd()
  
  files <- list.files(path = string,full.names = T) 
  
  exclusionUntil <- 100   
  
  ###############################背景分の減算処理#################################
  
  bg <- paste("background.csv",sep = "")
  eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
  b <- background[,3]
  #############################葉の各点##############################
  amplitude <- c()
  count <- 0
  over <- 0
  
  for(i in 0:512){
    for(j in 0:512){
      if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
        count <- count + 1
        
        df <- paste("Result",i,"_", j, ".csv", sep = "")
        eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
        
        x <- data[,1]
        y <- data[,3] - b
        p <- c()
        t <- c()
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        
        for(k in exclusionUntil:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
          d1 <- sp$y[k+1]-sp$y[k]
          d2 <- sp$y[k+2]-sp$y[k+1]
          if((d1>=0) && (d1*d2<0)){
            p <- append(p,sp$y[k+1])
          }
          else if((d1<=0) && (d1*d2<0)){
            t <- append(t,sp$y[k+1])
          }
        }
        #print(p)
        #print(t)
        if((mean(p)-mean(t))>40){
          over <- over + 1
        }
        amplitude <- append(amplitude,mean(p) - mean(t))
        #print(amplitude)
      }
    }
  }
  amplitude_mean <- append(amplitude_mean,mean(amplitude))
  print(amplitude_mean)
  print(paste(over,"/",count))
}
index <- rep(1,19)
plot(index,over/count,xlab = "",ylab = "amplitude",xlim = c(0,4),ylim = c(0,1))
par(new=T)
#write.csv(amplitude_mean,"../../amplitude.CSV")

#######################################################################################################

amplitude_mean <- c()
for(k in 0:14){ 
  
  string <- paste("D:/analyze/0708/",k,"/CSV",sep = "")  #日付変更
  setwd(string)
  getwd()
  
  files <- list.files(path = string,full.names = T) 
  
  exclusionUntil <- 100   
  
  ###############################背景分の減算処理#################################
  
  bg <- paste("background.csv",sep = "")
  eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
  b <- background[,3]
  #############################葉の各点##############################
  amplitude <- c()
  count <- 0
  over <- 0
  
  for(i in 0:512){
    for(j in 0:512){
      if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
        count <- count + 1
        
        df <- paste("Result",i,"_", j, ".csv", sep = "")
        eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
        
        x <- data[,1]
        y <- data[,3] - b
        p <- c()
        t <- c()
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        
        for(k in exclusionUntil:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
          d1 <- sp$y[k+1]-sp$y[k]
          d2 <- sp$y[k+2]-sp$y[k+1]
          if((d1>=0) && (d1*d2<0)){
            p <- append(p,sp$y[k+1])
          }
          else if((d1<=0) && (d1*d2<0)){
            t <- append(t,sp$y[k+1])
          }
        }
        #print(p)
        #print(t)
        if((mean(p)-mean(t))>40){
          over <- over + 1
        }
        amplitude <- append(amplitude,mean(p) - mean(t))
        #print(amplitude)
      }
    }
  }
  amplitude_mean <- append(amplitude_mean,mean(amplitude))
  print(amplitude_mean)
  print(paste(over,"/",count))
}
index <- rep(2,15)
plot(index,over/count,xlab = "",ylab = "amplitude",xlim = c(0,4),ylim = c(0,1))
par(new=T)
#write.csv(amplitude_mean,"../../amplitude.CSV")

#######################################################################################################

amplitude_mean <- c()
for(k in 0:8){ 
  
  string <- paste("D:/analyze/0624/",k,"/CSV",sep = "")  #日付変更
  setwd(string)
  getwd()
  
  files <- list.files(path = string,full.names = T) 
  
  exclusionUntil <- 100   
  
  ###############################背景分の減算処理#################################
  
  bg <- paste("background.csv",sep = "")
  eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
  b <- background[,3]
  #############################葉の各点##############################
  amplitude <- c()
  count <- 0
  over <- 0
  
  for(i in 0:512){
    for(j in 0:512){
      if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
        count <- count + 1
        
        df <- paste("Result",i,"_", j, ".csv", sep = "")
        eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
        
        x <- data[,1]
        y <- data[,3] - b
        p <- c()
        t <- c()
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        
        for(k in exclusionUntil:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
          d1 <- sp$y[k+1]-sp$y[k]
          d2 <- sp$y[k+2]-sp$y[k+1]
          if((d1>=0) && (d1*d2<0)){
            p <- append(p,sp$y[k+1])
          }
          else if((d1<=0) && (d1*d2<0)){
            t <- append(t,sp$y[k+1])
          }
        }
        #print(p)
        #print(t)
        if((mean(p)-mean(t))>40){
          over <- over + 1
        }
        amplitude <- append(amplitude,mean(p) - mean(t))
        #print(amplitude)
      }
    }
  }
  amplitude_mean <- append(amplitude_mean,mean(amplitude))
  print(amplitude_mean)
  print(paste(over,"/",count))
}
index <- rep(3,9)
plot(index,over/count,xlab = "",ylab = "amplitude",xlim = c(0,4),ylim = c(0,1))
par(new=T)
#write.csv(amplitude_mean,"../../amplitude.CSV")

#######################################################################################################

amplitude_mean <- c()
for(k in 0:8){ 
  
  string <- paste("D:/analyze/0617/",k,"/CSV",sep = "")  #日付変更
  setwd(string)
  getwd()
  
  files <- list.files(path = string,full.names = T) 
  
  exclusionUntil <- 100   
  
  ###############################背景分の減算処理#################################
  
  bg <- paste("background.csv",sep = "")
  eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
  b <- background[,3]
  #############################葉の各点##############################
  amplitude <- c()
  count <- 0
  over <- 0
  
  for(i in 0:512){
    for(j in 0:512){
      if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
        count <- count + 1
        
        df <- paste("Result",i,"_", j, ".csv", sep = "")
        eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
        
        x <- data[,1]
        y <- data[,3] - b
        p <- c()
        t <- c()
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        
        for(k in exclusionUntil:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
          d1 <- sp$y[k+1]-sp$y[k]
          d2 <- sp$y[k+2]-sp$y[k+1]
          if((d1>=0) && (d1*d2<0)){
            p <- append(p,sp$y[k+1])
          }
          else if((d1<=0) && (d1*d2<0)){
            t <- append(t,sp$y[k+1])
          }
        }
        #print(p)
        #print(t)
        if((mean(p)-mean(t))>40){
          over <- over + 1
        }
        amplitude <- append(amplitude,mean(p) - mean(t))
        #print(amplitude)
      }
    }
  }
  amplitude_mean <- append(amplitude_mean,mean(amplitude))
  print(amplitude_mean)
  print(paste(over,"/",count))
}
index <- rep(0,9)
plot(index,over/count,xlab = "",ylab = "amplitude",xlim = c(0,4),ylim = c(0,1))
par(new=T)
#write.csv(amplitude_mean,"../../amplitude.CSV")

#######################################################################################################