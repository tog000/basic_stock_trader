# Parameters
moving_avg1=10
moving_avg2=20
capital=50000

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

stock=0;
trend=0;
current_capital=capital
buy=array()
sell=array()
for(i in xaxis){
  if(!is.na(avg1[i]) && !is.na(avg2[i]) ){
    # Logica de compra o venta
    if(avg1[i]>avg2[i] && current_capital>0){
      print(paste("BUYING  @",price[i],"FOR",current_capital))
      stock=current_capital/price[i]
      current_capital=0;
      buy<-c(buy,i)
    }else if(avg2[i]>avg1[i] && stock>0){
      print(paste("SELLING @",price[i],stock))
      current_capital=price[i]*stock;
      stock=0;
      sell<-c(sell,i)
    }
    
    #TODO Do we ever want to short sell?
    
  }
}


points(buy,price[buy],pch=19,col='green');
points(sell,price[sell],pch=19,col='red');

#legend("bottomright",legend=c("AVG1","AVG2","BUY","SELL"), horiz=TRUE,fill=c("red","blue","green","red"))
legend("bottomright",legend=c("AVG1","AVG2"), horiz=TRUE,fill=c("red","blue"))


final_capital=current_capital+stock*price[length(price)-1];
print(paste("PROFIT =",((final_capital/capital)-1)*100,"%"))


