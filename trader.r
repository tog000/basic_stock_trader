# Parameters
moving_avg1=5
moving_avg2=20

# Cambiar esto al directorio que contiene el archivo
setwd("~/documents/workspace/bitcoin-trader/r")
stock=read.table("mexico.txt")
price <- stock[,2];
xaxis=seq(length(price))
plot(xaxis, price, type="l",xlab="Days",ylab="Price")
grid()

avg1=array()
avg1[0:moving_avg1] = NaN;
# Compute moving averages
for(i in moving_avg1:length(price)){
  indices=seq(from=i-moving_avg1,to=i)
  avg1[i]=mean(price[indices]);
}
lines(avg1,col="red")

avg2=array()
avg2[0:moving_avg2] = NaN;
# Compute moving averages
for(i in moving_avg2:length(price)){
  indices=seq(from=i-moving_avg2,to=i)
  avg2[i]=mean(price[indices]);
}
lines(avg2,col="blue")
title("Stock Value")
legend("bottomright",legend=c("AVG1","AVG2"), horiz=TRUE,fill=c("red","blue"))

for(i in xaxis){
  if(!is.na(avg1[i]) && !is.na(avg2[i]) ){
    
  }
}



