#for(k in 0:19){                                         #枚数変更

string <- paste("D:/analyze/0624/3/CSV",sep = "")  #日付変更
setwd(string)
getwd()

files <- list.files(path = string,full.names = T) 

exclusionUntil <- 50                                             #削除部分変更
ybottom <- 0             #縦軸グラフ描画範囲
ytop <-4000

#png("../Rplot_.png",width = 600, height = 400)

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
      p <- c()
      
      sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
      for(l in 1:(length(sp$y)-3)){     #微分した値の符号が変化したとき、そのxの値をpに保存
        d1 <- sp$y[l+1]-sp$y[l]
        d2 <- sp$y[l+2]-sp$y[l+1]
        if((d1>=0) && (d1*d2<0)){
          p <- append(p,l+1)
        }
      }
      print(p)
      
      sp$x <- replace(sp$x,c(0:exclusionUntil),NA)
      sp$y <- replace(sp$y,c(0:exclusionUntil),NA)
      

      
#####################グラフ描画############################      
      par(new=T)
      plot(sp$x,sp$y,type="l",xlab="time",ylab="bioluminescence",xaxt="n",xlim=c(0,1008),ylim=c(ybottom,ytop),col=rgb(0,0.8,0,alpha = 0.05))
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
p <- c() 

sp <- smooth.spline(x,y,spar=0.7)　#関数の平滑化

sp$x <- replace(sp$x,c(0:exclusionUntil),NA)
sp$y <- replace(sp$y,c(0:exclusionUntil),NA)


for(l in 1:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
  d1 <- sp$y[l+1]-sp$y[l]
  d2 <- sp$y[l+2]-sp$y[l+1]
  if(((d1>=0) && (d1*d2<0)) == TRUE){
    p <- append(p,l+1)
  }
}
print(p)

theta <- c()   #位相を定義

for(i in 1:length(p)) {
  for(j in (p[i]+1):p[i+1]){
   theta[j] <- 2*pi*(j-p[i]+1)/(p[i+1]-p[i]+1)
  }
  
}


par(new=T)
plot(sp$x,sp$y,type="l",xlab="time",ylab="",xaxt="n",xlim=c(0,1008),ylim=c(ybottom,ytop),col="red",lwd=3,axes = FALSE)
axis(side=1,xaxp=c(0,1008,7),tck=1.0,lty="dotted",xlim=c(0:1008))
#mtext(side=4,text = "bioluminescence(whole)",col="red")
axis(4)

#####################グラフの出力################
#dev.off()
