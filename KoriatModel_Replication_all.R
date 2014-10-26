## Figure 1 (Koriat, 2012)
## Sampling N=5,7,9 for the binominal distribution. 
## PCmaj is calculated as a probability that a target 
## choice represent a majority for a given sample. 

suppressMessages(library(ggplot2))
Pmaj<-c()
PCmaj<-c()
PCmajsum<-c()
PCmajtemp<-c()
SCindex<-c()
Sample<-c()
Pmaj1<-seq(.5,1,.1)
Pmaj2<-seq(.5,1,.05)
N<-c(5,7,9)
for (n in N){
    ## PCmaj = C(N/k)*p^k*(1-p)^(N-k) = (N!/k!(N-k)!)*Pmaj^k*Pmaj(N-k) , where
    ## N - sample size, k - number of majority outcomes. 
    ## For example, for n=5 the probability is the sum of PCmaj(3), PCmaj(4), and PCmaj(5).
    ## PC(3:5) for N=5 and Pmaj=.5 ==> 
    ##              C(5/3*0.5^3*0.5^2) + C(5/4*0.5^4*0.5^1) + C(5/5*0.5^5*0.5^0)  
    for (p in Pmaj1) {
        PCmajsum<-sum(dbinom(ceiling(n/2):n, n, p))
        PCmaj<-c(PCmaj,PCmajsum)
    }
    ## SC Index calculated as an inversion of standard deviation of a proportion of the favoring answers
    ## 
    
    for (p in Pmaj2) {
        PCmajtemp<-dbinom(0:n,n,p)
        Prop<-c(0:n)/n
        SCindex<-c(SCindex,sum(PCmajtemp*(1-sqrt(Prop*(1-Prop)))))
    }
}
Pmaj<-c(rep(Pmaj1,3))
Sample<-c(rep(5,6),rep(7,6),rep(9,6))
fig1<-data.frame(Pmaj,PCmaj,Sample)
fig1$Sample<-as.factor(fig1$Sample)
## Plot PCmaj as a function (sum of probabilities) of Pmaj for n = 5,7,9
ggplot(fig1, aes(x=Pmaj, y=PCmaj, group=Sample,shape=Sample, title="PCmaj as a function of Pmaj for sample size of 5,7, and 9")) + 
    geom_line() +
    geom_point() +
    scale_shape(solid=F, name="Sample Size")

## Figures 2a (Koriat, 2012)
## The consistency of the favoring the answer (1-sqrt(E(p)E(q)) as a function of Pmaj
Pmaj<-c(rep(Pmaj2,3))
Sample<-c(rep(5,11),rep(7,11),rep(9,11))
fig2<-data.frame(Pmaj, SCindex, Sample)
fig2$Sample<-as.factor(fig2$Sample)
## Plot SCindex as a function of Pmaj for n = 5,7,9
ggplot(fig2, aes(x=Pmaj, y=SCindex, group=Sample,shape=Sample, title="Self confidence as a function of Pmaj for sample size of 5,7, and 9")) + 
    geom_line() +
    geom_point() +
    scale_shape(solid=F, name="Sample Size")

## Figures 2b (Koriat, 2012)
## The consistency of the favoring the answer (1-sqrt(E(p)E(q)) as a function of PCmaj

fig1$SCindex<-subset(fig2, fig2$Pmaj %in% Pmaj1)$SCindex
ggplot(fig1, aes(x=PCmaj, y=SCindex, group=Sample,shape=Sample, title="Self confidence as a function of PCmaj for sample size of 5,7, and 9")) + 
    geom_line() +
    geom_point() +
    scale_shape(solid=F, name="Sample Size")

## Figure 3a (Koriat, 2012)
## The consistency of the favoring the answer as a function of PCmaj for the majority of answers for N=5,7,9

SCindex<-c()
PCtemp<-c()
PC_maj<-c()
Pmaj2<-seq(.5,.95,.05)
SampleSize<-c()
PC_maj<-c()
SCtype<-c()
N<-c(5,7,9)

for (n in N){
    
    for (p in Pmaj2){
        PC_maj<-c(PC_maj, sum(dbinom(4:n,n,p)))
        PCtemp<-dbinom(0:n,n,p)
        Prop<-(0:n)/n
        pr<-length(Prop)/2
        Propmin<-Prop[Prop<.5]
        Propmaj<-Prop[Prop>.5]  
        PCmin<-PCtemp[0:pr]
        PCmaj<-PCtemp[(pr+1):(2*pr)]  
        Indexmin<-1-sqrt(Propmin*(1-Propmin))
        Indexmaj<-1-sqrt(Propmaj*(1-Propmaj))    
        SCmin<-weighted.mean(Indexmin,PCmin)
        SCmaj<-weighted.mean(Indexmaj,PCmaj)
        SCtype<-c(SCtype, "Minority","Majority")
        SCindex<-c(SCindex,SCmin,SCmaj)
        SampleSize<-c(SampleSize,n)
    }
}
P_maj<-rep(rep(Pmaj2,each=2), times=3)
PC_maj<-rep(PC_maj, each=2)
SSize<-rep(SampleSize, each=2)
allSC<-data.frame(P_maj,SCindex,PC_maj, SCtype, SSize)

## Plot Self consistency as a function of Pmaj for N=7
SCsample7<-subset(allSC,SSize==7)
ggplot(SCsample7, aes(x=P_maj, y=SCindex, group=SCtype,shape=SCtype, title="Consistency of majority and minority answers as a function of Pmaj for N = 7")) + 
    geom_line() +
    geom_point(aes(shape=SCtype, fill=SCtype)) +
    scale_shape_manual(values=c(21,21))+
    scale_fill_manual(values=c("black","white")) +
    xlab("Pmaj") +
    ylab("Self Consistency") +
    theme(legend.title=element_blank())
## Plot Self consistency as a function of PCmaj for N=7
ggplot(SCsample7, aes(x=PC_maj, y=SCindex, group=SCtype,shape=SCtype, title="Consistency of majority and minority answers as a function of Pmaj for N = 7")) + 
    geom_line() +
    geom_point(aes(shape=SCtype, fill=SCtype)) +
    scale_shape_manual(values=c(21,21))+
    scale_fill_manual(values=c("black","white")) +
    xlab("PCmaj") +
    ylab("Self Consistency") +
    theme(legend.title=element_blank())
