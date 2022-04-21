##########Preparations###########
rm(list = ls(all=TRUE))
setwd("D://R//Rdata")
fdata <- read.table("原始数据2.txt",sep='\t',header=T)
View(fdata)
WSRL <- apply(fdata[,c(6:31)],1,FUN = mean)
ESRL <- apply(fdata[,c(8,9,14,17,18,23,26,27)],1,FUN =mean)
BSRL <- apply(fdata[,c(10,11,12,19,20,21,28,30)],1,FUN=mean)
PSRL <- apply(fdata[,c(6,7,13,15,16,22,24,25,29,31)],1,FUN=mean)
SA <- apply(fdata[,c(32:38)],1,FUN=mean)
AB <- apply(fdata[,c(39:45)],1,FUN =mean)
CA <- apply(fdata[,c(46:53)],1,FUN=mean)
SLWA <- apply(fdata[,c(32:53)],1,FUN=mean)
sdata<- cbind(fdata,WSRL,ESRL,BSRL,PSRL,SA,AB,CA,SLWA)
write.table(sdata,"fzxdata3.txt",quote=FALSE,sep='\t')
##########Reliabilty######
odata <- read.table("fzxdata3.txt",sep='\t',header=T)
WSRL <- odata[,c(6:31)]
ESRL <- odata[,c(8,9,14,17,18,23,26,27)]
BSRL <- odata[,c(10,11,12,19,20,21,28,30)]
PSRL <- odata[,c(6,7,13,15,16,22,24,25,29,31)]
SLWA <- odata[,c(32:53)]
SA <- odata[,c(32:38)]
AB <- odata[,c(39:45)]
CA <- odata[,c(46:53)]

sumvarWSRL <- sum(apply(WSRL,2,var))
varsumWSRL <- var(apply(WSRL,1,sum))
aphaWSRL <- (26/25)*(1-sumvarWSRL/varsumWSRL)

sumvarESRL <- sum(apply(ESRL,2,var))
varsumESRL <- var(apply(ESRL,1,sum))
alphaESRL <- (8/7)*(1-sumvarESRL/varsumESRL)

sumvarBSRL <- sum(apply(BSRL,2,var))
varsumBSRL <- var(apply(BSRL,1,sum))
alphaBSRL <- (8/7)*(1-sumvarBSRL/varsumBSRL)

sumvarPSRL <- sum(apply(BSRL,2,var))
varsumPSRL <- var(apply(BSRL,1,sum))
alphaPSRL <- (10/9)*(1-sumvarPSRL/varsumPSRL)

sumvarSLWA <- sum(apply(SLWA,2,var))
varsumSLWA <- var(apply(SLWA,1,sum))
alphraSLWA <- (22/21)*(1-sumvarSLWA/varsumSLWA)

sumvarSA <- sum(apply(SA,2,var))
varsumSA <- var(apply(SA,1,sum))
alphraSA <- (7/6)*(1-sumvarSA/varsumSA)

sumvarAB <- sum(apply(AB,2,var))
varsumAB <- var(apply(AB,1,sum))
alphraAB <- (7/6)*(1-sumvarAB/varsumAB)

sumvarCA <- sum(apply(CA,2,var))
varsumCA <- var(apply(CA,1,sum))
alphraCA <- (8/7)*(1-sumvarCA/varsumCA)
##########Descriptive#########
library("pastecs")
odata1 <- read.table("fzxdata.txt",sep='\t',header=T)
odata2 <- odata1[,c(3,5,54:61)]
aldescdata <- stat.desc(odata2)
descdata <- aldescdata[c(4,5,7,9,13),]
sexfreq <- prop.table(table(odata1[,4]))*100
##########t-test#############
odata <- read.table("fzxdata.txt",sep='\t',header=T)
attach(odata)
t.test(writingscore ~ Q1)
t.test(WSRL ~ Q1)
t.test(SLWA ~ Q1)
detach(odata)
##########Correlation#######
odata <- read.table("fzxdata.txt",sep='\t',header=T)
corredata <- odata[,c(3,5,54:61)]
cor(corredata)
attach(corredata)
cor.test(writingscore,WSRL)
cor.test(writingscore,ESRL)
cor.test(writingscore,PSRL)
cor.test(writingscore,BSRL)
cor.test(writingscore,SLWA)
cor.test(writingscore,SA)
cor.test(writingscore,AB)
detach(corredata)
##########Regression######
library(MASS)
f1 <- lm(odata$writingscore~odata$ESRL+odata$PSRL+odata$BSRL+odata$SA+odata$AB+odata$CA)
summary(f1)
f2 <- lm(odata$writingscore~odata$ESRL+odata$PSRL+odata$BSRL,data=odata)
summary(stepAIC(f2,direction="backward"))
f3 <- lm(odata$writingscore~odata$ESRL+odata$PSRL+odata$BSRL+odata$SA+odata$AB+odata$CA,data=odata)
summary(stepAIC(f3,direction="backward"))