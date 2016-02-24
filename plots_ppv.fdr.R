
remove(list=ls())
par(mfrow=c(1,1))
setwd(getwd())

alpha<-0.05
power<-seq(0,1,length.out=100)
ppv<-power/(alpha+power)
fdr<-alpha/(alpha+power)

data<-cbind(power,ppv,fdr)





# 2d
pdf("ppv&fdr.pdf")
plot(power,ppv,main="",xlab=expression(statistical~power~(1-beta)),ylab="",ylim=c(0,1),xlim=c(0,1),type="l",lwd=3,col="black",axes=F);axis(1);axis(2)
text(.8,.85,"PPV")
lines(power,fdr,lwd=3,col="grey70")
points(.95,0.05,pch=8,col="grey40")
text(.8,.15,"FDR",col="grey70")
dev.off()




# 3d

# Below is a function that will carryout a Bayesian conditional probability calculation: 
# the probability of a 'false-positive / probability of any rejection of the null'.  
# In essence, this tells us the perecentage of null rejections that are false-positives - the false discovery rate
# because we have the false-positives in the numerator, divided by the total number of ways one can get a positive result, hence, the proportion of positives that are false!

alpha<-0.05
power<-seq(.2,.8,length.out=50)
prior<-seq(0,.5,length.out=50)
calc<-function(alpha,prior,power)
{
  fdr<-(alpha*(1-prior)/(alpha*(1-prior)+(power*prior)))  # this is the Bayesian equation to calculate the false discover rate
  return(fdr)
}
fdrValues<-(sapply(rep(prior[1:length(prior)],1),calc,alpha=alpha,power=power))
# rows toggle the "power" value
rownames(fdrValues)<-paste0("Power=",round(power,2))            # name the rows
# columns toggle the "prior" value
colnames(fdrValues)<-paste0("Prior=",round(prior,2))            # name the columns
fdrValues

# expand the calculations to a grid for 3d plotting
fdrGrid<-expand.grid(fdrValues)
powerPriorGrid<-expand.grid(power,prior)
grid<-data.frame(fdrGrid,powerPriorGrid)
names(grid)<-c("fdr","power","prior")

# 3d plot
library(lattice)
pdf("fdr3d.pdf")
par.set<-list(axis.line=list(col="transparent"),clip=list(panel="off"))
wireframe(fdr~power*prior,data=grid,scales=list(z.ticks=.01,arrows=F),ylim=c(max(prior),min(prior)),xlim=c(min(power),max(power)),xlab=expression(statistical~power~(1-beta)),ylab="R/(R+1)",zlab="FDR",par.settings=par.set,drape=T,col.regions=rainbow(150,start=.2,end=0,alpha=.6),screen=list(z=30,x=-70))
dev.off()

