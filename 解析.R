# 平滑化を行うデータを作成

  

setwd("C:/Users/alskd/Desktop/test")
getwd()
files <- list.files(path = "C:/Users/alskd/Desktop/test",full.names = T)

#phi <- c()

for(k in 0:15){
  for(l in 0:15){
  df <- paste("Result",k,"_", l, ".csv", sep = "")#文字列の形成
  eval(parse(text=paste("data"," <-read.csv('", df, "',header=TRUE)",sep = "")))#文字列の実行
 x <- data[,1]
 y <- data[,2]
# p <- c() #ピーク時の時間
 
 
 #関数による平滑化
 sp <- smooth.spline(x,y,spar=0.6)
 
 if((max(sp$y)-min(sp$y))>30){
   for(i in 1:(length(y)-3)){     #微分した値の符号が変化するとき、そのxの値を保存
#     d1 <- sp$y[i+1]-sp$y[i]
#     d2 <- sp$y[i+2]-sp$y[i+1]
#     if((d1>=0) && (d1*d2<0)){
#       p <- append(p,i+1)
#       }
     }
#   print(p)
   
#   phase <- rep(0,length=p[1])#位相の角度（ピーク時0,範囲は0~1）
#   for(i in 1:(length(p)-1)){ #ピーク間内での処理を（ピーク数ー１）回
#     for(j in (p[i]+1):p[i+1]){
#       phase <- append(phase,(j-p[i]-1)/(p[i+1]-p[i]-1))
#       phi <- rbind(phi,phase*2*pi)　#位相の角度
#       }
#   }
   }else{
     replace(sp$y,(sp$y>0),0)
     phase <- rep(0,length(x)) #背景部分の値を全部０に
     }
 

  }  
}
#for(i in 1:ncol(phi)){
#  coordinate_x <- append(coordinate_x,(cos(phi[,c(i)])))
#  coordinate_y <- append(coordinate_y,(sin(phi[,c(i)])))
#  value <- abs(sum(sqrt((coordinate_x*coordinate_x)+(coordinate_y*coordinate_y)))/length(coordinate_x))
#}


#書き出し
 #write.csv(phase,"C:/Users/alskd/Desktop/phase.csv")
# 図の作成
# plot(value)
 #plot(sp$x,sp$y,type="l")
 

