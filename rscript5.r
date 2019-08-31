# Rscript for Assignment 2 of ECO 423

n<-11;m1<-500.9;m2<-1.4562;s1<-0.1*m1;s2<-0.035*m2;p<-0.3
# m1 is the mean for kerosene
# m2 is the mean for exchange
# s1 std dev for kerosene
# s2 std dev for exchange
# p id the correlation coefficient

# rnorm is the function for producing random numbers
x<-rnorm(n,m1,s1)

n1<-12

kerosene_price<-rnorm(n1,0,0)

flight_no<-rnorm(n1,0,0)
flight_no[1]<-522

exchange<-rnorm(n1,0,0)

exchange_rate_change<-rnorm(n1,0,0)
exchange_rate_change[1]<-0.00

flight_rate<-rnorm(n,0,0)

earning<-rnorm(n1+2,0,0)
earning[n1+2]<-0

earning_final<-rnorm(n1,0,0)
earning_final[n1]<-0

y<- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

#print(cor(x,y))
#print(sd(x))
#print(sd(y))

for (i in 1:n){
	kerosene_price[i]<-x[i]
}
kerosene_price[12]<-kerosene_price[1]

for (i in 1:n){
	exchange[i]<-y[i]
}
exchange[12]<-exchange[1]

for (i in 2:n1){
	exchange_rate_change[i]<-((exchange[i]-exchange[i-1])/exchange[i-1])*100
}

for (i in 1:n1){
	flight_rate[i]<-exchange_rate_change[i]*(-0.8)
}

for (i in 2:n1){
	flight_no[i]<-flight_no[i-1]*(100+flight_rate[i])/100
}

for (i in 1:n1){
	earning[i]<-flight_no[i]*40000-7000*flight_no[i]-exchange[i]*kerosene_price[i]*13*flight_no[i]-11000000
}

for (i in 1:n1){
	earning[n1+2]<-earning[n1+2]+earning[i]
}

for (i in 1:n1){
	earning_final[i]<-earning[i]
}

#for (i in 1:n1){
#	print(earning[i])
#}

#for (i in 1:n1){
#	print(exchange[i])
#}

#print(earning[n1+2])

#print(cor(earning_final,kerosene_price))

#for (i in 1:n1){
#	output<-c(earning[i],kerosene_price[i])
#	print(output)
#}

output<-c(earning[1],kerosene_price[1],exchange[1])
#output<-c(earning[1],exchange[1])

print(output)