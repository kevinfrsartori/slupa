####
# first analysis of SLUPA survey
# 28-03-2024
####

# next time, ask PhD defence date
# In what year of postdoc are you
# Ask salary AFTER taxe, for being comparable to stipend

survey<-read.table("Survey_modified_28032024.csv",sep = ",",h=T,na.strings = "")

# Average questions
apply(X = survey[,8:69],MARGIN = 2,FUN = mean,na.rm=T)
apply(X = survey[,8:69],MARGIN = 2,FUN = table)



hist(survey$salary[which(survey$gender == "Woman")],breaks = 20,col=rgb(0,1,0,.5),
     xlim=c(20000,50000),main = "Green, Women, Purple, men; Others not shown (too few)",
     xlab="Monthly salary (sek)")
hist(survey$salary[which(survey$gender == "Man")],breaks = 20,col=rgb(.2,0,1,.5),add=T)

boxplot(survey$salary~survey$position, ylab="Monthly salary (sek)", xlab="Position type", las=1)

summary(lm(salary ~ BD + gender + position + started + arrived, data = survey))

pca<-FactoMineR::PCA(survey[,3:69],quali.sup = c(1,2),quanti.sup = c(3,4,5))

library(MASS)

gender<-survey[which(survey$gender %in% c("Man","Woman")),c(3,8:69)]
lda_g<-lda(gender ~., gender)
lda_g$mean

plot(lda_g, col = as.integer(gender$gender))


pos<-na.omit(survey[,c(4,8:69)])
levels(as.factor(pos$position))

lda_p<-lda(position ~., pos)

lda_p

plot(lda_p,col=as.integer(as.factor(pos$position)))


library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)