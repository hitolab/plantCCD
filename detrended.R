library(stats)

for(k in 1:8){                                         #枚数変更
  
  string <- paste("D:/analyze/0605/",k,"/CSV",sep = "")  #日付変更
  setwd(string)
  getwd()

files <- list.files(path = string,full.names = T) 

exclusionUntil <- 100                                     #削除部分変更
png("../Rplot.png",width = 600, height = 400)

###############################背景分の減算処理#################################

bg <- paste("background.csv",sep = "")
eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
b <- background[,3]
#############################葉の各点##############################

for(i in 0:512){
  for(j in 0:512){
    if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
      df <- paste("Result",i,"_", j, ".csv", sep = "")
      eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
      if(data[1,3] != 0){
        x <- data[,1]
        y <- data[,3] - b
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        d <- diff(sp$y)
        d <- append(d,0,after = 0)
        ave <- mean(d)
        sd <- sd(d)
        d <- (d - ave)/sd
        #print(sd(d))
        sp$x <- replace(sp$x,c(0:exclusionUntil),NA)
        d <- replace(d,c(0:exclusionUntil),NA)
        
        
        
        #####################グラフ描画############################      
        par(new=T)
        plot(sp$x,d,type="l",xlab="time",ylab="bioluminescence",xaxt="n",xlim=c(0,1008),ylim=c(-1.5,1.5),col=rgb(0,0.8,0,alpha = 0.05))
        axis(side=1,xaxp=c(0,1008,7),tck=1.0,lty="dotted",xlim=c(0:1008))
      }
    }
  }
}
################################葉全体のグラフ########################################


df <- paste("Result.csv", sep = "")
eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
x <- data[,1]
y <- data[,3] - b

sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
d <- diff(sp$y)
d <- append(d,0,after = 0)
ave <- mean(d)
sd <- sd(d)
d <- (d - ave)/sd

sp$x <- replace(sp$x,c(0:exclusionUntil),NA)
d <- replace(d,c(0:exclusionUntil),NA)


par(new=T)
plot(sp$x,d,type="l",xlab="time",ylab="",xaxt="n",xlim=c(0,1008),ylim=c(-1.5,1.5),col="red",lwd=3,axes = FALSE)
axis(side=1,xaxp=c(0,1008,7),tck=1.0,lty="dotted",xlim=c(0:1008))
mtext(side=4,text = "bioluminescence(whole)",col="red")
axis(4)

#####################グラフの出力################
dev.off()
}