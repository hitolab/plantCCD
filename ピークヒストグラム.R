for(k in 0:18){                                         #枚数変更

string <- paste("D:/analyze/0617/",k,"/CSV",sep = "")  #日付変更
setwd(string)
getwd()

files <- list.files(path = string,full.names = T) 

exclusionUntil <- 100   
png("../Rplot_p.png",width = 600, height = 200)

###############################背景分の減算処理#################################

bg <- paste("background.csv",sep = "")
eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
b <- background[,3]
#############################葉の各点##############################
peaks <- c()
count <- 0

for(i in 0:512){
  for(j in 0:512){
    if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
      count <- count + 1
      df <- paste("Result",i,"_", j, ".csv", sep = "")
      eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
      if(data[1,3] != 0){
        x <- data[,1]
        y <- data[,3] - b
        p <- c()
        
        sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
        for(l in exclusionUntil:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
          d1 <- sp$y[l+1]-sp$y[l]
          d2 <- sp$y[l+2]-sp$y[l+1]
          if((d1>=0) && (d1*d2<0)){
            peaks <- append(peaks,l+1)
          }
        }
        print(peaks)
        hist(peaks,xlim = c(0,1008),ylim = c(0,30),xaxt="n",breaks = 1008,xlab = "",ylab = "",yaxp=c(0,30,2))
        axis(side=1,xaxp=c(0,1008,7),tck=1.0,lty="dotted",xlim=c(0:1008))
      }
    }
  }
}
dev.off()
}