lambda=0.2

#thousand simulation, 40 exponentials

#given that lambda is 0.2, the expected mu=5 and sd=5

library(ggplot2)

##for means

exp<-c()
for (i in 1:1000){
  exp<-rbind(exp,rexp(40,0.2))
}

mns<-c()
for (i in 1:1000) {
  mns[i]<-mean(exp[i,])
}
means<-cumsum(mns[1:1000])/(1:1000) ## cumulative means. how the mean changes as the sample size increases

g_m<-ggplot(data.frame(x=1:1000, y=means), aes(x=x, y=y))+geom_line(size=1.5)
g_m<-g_m+ylim(min(means)-0.1,max(means)+0.1)+geom_hline(yintercept=5, size=1, color="red")
g_m<-g_m+labs(title="Change in Sample Mean as Sample Size Increase", x="Sample Size", y="Mean")

means[1000]

##for var

#sample var=sigma^2/n 




p1<-ggplot(data.frame(x=1:50, y=mns[1:50]), aes(x=x, y=y))+geom_point()+geom_line(size=0.5)
p1<-p1+ylim(min(mns[1:50]-qnorm(0.975)*sqrt(vrs[1:50]/40))-0.1,max(mns[1:50]+qnorm(0.975)*sqrt(vrs[1:50]/40))+0.1)+geom_hline(yintercept=5, size=1, color="red")
p1<-p1+labs(title="Change in Sample Mean as Sample Size Increase", x="Sample Size", y="Mean")
p1<-p1+geom_pointrange(aes(ymin=mns[1:50]-qnorm(0.975)*sqrt(vrs[1:50]/40), ymax=mns[1:50]+qnorm(0.975)*sqrt(vrs[1:50]/40)), color=ifelse(mns[1:50]-qnorm(0.975)*sqrt(vrs[1:50]/40)>5 | mns[1:50]+qnorm(0.975)*sqrt(vrs[1:50]/40)<5, "red","black"), alpha=0.2)
p1