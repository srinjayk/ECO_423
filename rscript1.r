#!/usr/bin/Rscript
#args=commandArgs(trailingOnly=TRUE)
#library("ggplot2")
#t=read.csv(args[1])
#print(t)
#p<-ggplot(t)+geom_line(aes(x=user_number,y=avg_latency_time,color=url_number))+labs(x='user number',y='Average latency time',title='latency vs per user for differrent pages')+facet_grid(url_number~.)
#ggsave(p,file = paste0(args[3], args[2], ".png"),device = "png")
n<-11;m1<-0.667;m2<-0.113;s1<-10.00;s2<-3.50;p<-0.3
x<-rnorm(n,m1,s1)

y<- s2*p*(x-m1)/s1 + m2 + s2*rnorm(n,0,sqrt(1-p^2))

print(y)
