# Rscript for Assignment 2 of ECO 423

n<-10;m1<-500.9;m2<-1.4562;s1<-0.1*m1;s2<-0.035*m2;p<-0.3
x<-rnorm(n,m1,s1)

n1<-12

kerosene_price<-rnorm(n1,0,0)
kerosene_price[1]<-500.9
kerosene_price[12]<-500.9

flight_no<-rnorm(n1,0,0)
flight_no[1]<-522

exchange<-rnorm(n1,0,0)
exchange[1]<-1.4562
exchange[12]<-1.4562

exchange_rate<-rnorm(n1,0,0)
exchange_rate[1]<-0.00

flight_rate<-rnorm(n,0,0)

earning<-rnorm(n1,0,0)
earning[14]<-0

y<- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

#print(cor(x,y))
#print(sd(x))
#print(sd(y))

for (i in 1:10){
	kerosene_price[i+1]<-x[i]
}

for (i in 1:10){
	exchange[i+1]<-y[i]
}

for (i in 2:12){
	exchange_rate[i]<-((exchange[i]-exchange[i-1])/exchange[i-1])*100
}

for (i in 1:12){
	flight_rate[i]<-exchange_rate[i]*(-0.8)
}

for (i in 2:12){
	flight_no[i]<-flight_no[i-1]*(100+flight_rate[i])/100
}

for (i in 1:12){
	earning[i]<-flight_no[i]*40000-7000*flight_no[i]-exchange[i]*kerosene_price[i]*13*flight_no[i]-11000000
}

for (i in 1:12){
	earning[14]<-earning[14]+earning[i]
}

print(earning[14])