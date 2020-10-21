amplitude_mean <- c()
for(k in 1:9){ 
  
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

for(i in 0:512){
  for(j in 0:512){
    if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
      df <- paste("Result",i,"_", j, ".csv", sep = "")
      eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
      
      x <- data[,1]
      y <- data[,3] - b
      p <- c()
      t <- c()
      
      sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
      #d <- diff(sp$y)
      #d <- append(d,0,after = 0)
      #ave <- mean(d)
      #sd <- sd(d)
      #d <- (d - ave)/sd
      
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
      # par(new=T)
      # plot(sp$x,d,type="l",xlab="time",ylab="bioluminescence",xaxt="n",xlim=c(0,1008),ylim=c(-1.5,1.5),col=rgb(0,0.8,0,alpha = 0.05))
      # axis(side=1,xaxp=c(0,1008,7),tck=1.0,lty="dotted",xlim=c(0:1008))
       amplitude <- append(amplitude,mean(p) - mean(t))
       #print(amplitude)
    }
  }
}
amplitude_mean <- append(amplitude_mean,mean(amplitude))
print(amplitude_mean)
}
write.csv(amplitude_mean,"../../amplitude.CSV")