ratio_mean <- c()
for(k in 3:18){                                         #枚数変更

string <- paste("D:/analyze/0605/",k,"/CSV",sep = "")  #日付変更
setwd(string)
getwd()


files <- list.files(path = string,full.names = T) 

exclusionUntil <- 100   
png("../ratio.png",width = 600, height = 400)
###############################背景分の減算処理#################################

bg <- paste("background.csv",sep = "")
eval(parse(text=paste("background"," <-read.csv('", bg, "',header=TRUE)",sep = "")))
b <- background[,3]
#############################葉の各点##############################
sum_cos <- rep(0,length = 1008)
sum_sin <- rep(0,length = 1008)
ratio <- c()
count <- 0
last_peak <- 1008

for(i in 0:512){
  for(j in 0:512){
    if(file.exists(paste("Result",i,"_",j,".csv",sep = ""))){ 
      count <- count + 1
      df <- paste("Result",i,"_", j, ".csv", sep = "")
      eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
      x <- data[,1]
      y <- data[,3] - b
      p <- c()
      ###################ピーク算出#################        
      sp <- smooth.spline(x,y,spar=0.6)　#関数の平滑化
      for(l in 1:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
        d1 <- sp$y[l+1]-sp$y[l]
        d2 <- sp$y[l+2]-sp$y[l+1]
        if((d1>=0) && (d1*d2<0)){
          p <- append(p,l+1)
        }
      }
      print(p)
      if(length(p)>2){
        if(last_peak > p[length(p)]){
          last_peak <- p[length(p)]
        }
        ##################位相算出#####################         
        theta_p <- c()   #位相を定義
        
        
        for(i in 1:(length(p)-1)) {
          for(j in p[i]:p[i+1]){
            theta_p[j] <- 2*pi*(j-p[i])/(p[i+1]-p[i])
          }
        }
        for (k in 0:p[1]) {
          theta_p[p[1]-k] <- (2*pi - theta_p[p[1]+k])
        }
        theta_p <- theta_p[!is.na(theta_p)]
        #par(new=T)
        #plot(theta_p,xlim=c(0,1008),ylim=c(0,7),col=rgb(0,0.8,0,alpha = 0.05))
        #print(theta_p)
        ###################同期率算出######################        
        for(k in 1:1008){
          sum_cos[k] <- sum_cos[k] + cos(theta_p[k]) 
          sum_sin[k] <- sum_sin[k] + sin(theta_p[k])
        }
      } 
      #par(new=T)
      #plot(cos(theta_p),xlim=c(0,1008),col=rgb(0,0.8,0,alpha = 0.05))
    }
  }
}
#print(sum_cos)
#print(sum_sin)
for (n in 0:last_peak) {
  ratio[n] <- (sqrt((sum_cos[n])^2 + (sum_sin[n])^2))/count
}
ratio <- replace(ratio,c(0:exclusionUntil),NA)
print(ratio)
plot(ratio,type = "l",xaxt="n",xlim=c(0,1008),ylim = c(0,1),xlab="time",ylab="synchronize ratio",)
axis(side=1,xaxp=c(0,1008,7),tck=1.0,lty="dotted",xlim=c(0:1008))
print(mean(ratio[!is.na(ratio)]))
dev.off()
ratio_mean <- append(ratio_mean,mean(ratio[!is.na(ratio)]))
}
print(ratio_mean)
write.csv(ratio_mean,"../../ratio.CSV")
################################葉全体のグラフ########################################
# 
# 
# df <- paste("Result.csv", sep = "")
# eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))
# x <- data[,1]
# y <- data[,3] - b
# p <- c() 
# 
# sp <- smooth.spline(x,y,spar=0.7)　#関数の平滑化
# 
# for(l in 1:(length(sp$y)-2)){     #微分した値の符号が変化したとき、そのxの値をpに保存
#   d1 <- sp$y[l+1]-sp$y[l]
#   d2 <- sp$y[l+2]-sp$y[l+1]
#   if((d1>=0) && (d1*d2<0)){
#     p <- append(p,l+1)
#   }
# }
# print(p)
# 
# theta_p <- c()   #位相を定義
# for(i in 1:(length(p)-1)) {
#   for(j in (p[i]+1):p[(i+1)]){
#     theta_p[j] <- 2*pi*(j-p[i]+1)/(p[i+1]-p[i]+1)
#   }
# }
# 
# for (k in 0:p[1]) {
#   theta_p[k] <- (2*pi - theta_p[2*p[1]-k])
# }
# 
# sp$x <- replace(sp$x,c(0:exclusionUntil),NA)
# sp$y <- replace(sp$y,c(0:exclusionUntil),NA)
# 
# #par(new=T)
# #plot(theta_t,xlim=c(0,1008),col = "red")