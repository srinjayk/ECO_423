#!/usr/bin/Rscript
#args=commandArgs(trailingOnly=TRUE)
#library("ggplot2")
#t=read.csv(args[1])
#print(t)
#p<-ggplot(t)+geom_line(aes(x=user_number,y=avg_latency_time,color=url_number))+labs(x='user number',y='Average latency time',title='latency vs per user for differrent pages')+facet_grid(url_number~.)
#ggsave(p,file = paste0(args[3], args[2], ".png"),device = "png")
n<-13;m1<-0.667;m2<-0.124;s1<-10.00;s2<-3.50;p<-0.3
x<-rnorm(n,m1,s1)

n1<-14

kerosene_price<-rnorm(n1,0,0)
kerosene_price[1]<-500.9
kerosene_price[14]<-500.9

flight_no<-rnorm(n1,0,0)
flight_no[1]<-522

exchange<-rnorm(n1,0,0)
exchange[1]<-1.4343
exchange[14]<-1.4343

flight_rate<-rnorm(n,0,0)

earning<-rnorm(n1,0,0)
earning[14]<-0

y<- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

#print(cor(x,y))
#print(sd(x))
#print(sd(y))
for (i in 1:13){
	flight_rate[i]<-y[i]*(-0.8)
}

#for (i in 1:10){
#	print(flight_rate[i]<-y[i]*(-0.8))
#}

for (i in 1:12){
	kerosene_price[i+1]<-kerosene_price[i]*(100+x[i])/100
}

for (i in 1:12){
	flight_no[i+1]<-flight_no[i]*(100+flight_rate[i])/100
}

for (i in 1:12){
	exchange[i+1]<-exchange[i]*(100+y[i])/100
}

for (i in 1:12){
	earning[i]<-flight_no[i]*40000-7000*flight_no[i]-exchange[i]*kerosene_price[i]*13*flight_no[i]-12000000
}

for (i in 1:12){
	earning[14]<-earning[14]+earning[i]
}

print(exchange[2])