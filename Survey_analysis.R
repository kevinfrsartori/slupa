####
# first analysis of SLUPA survey
# 28-03-2024
####

survey<-read.table("Survey_modified_28032024.csv",sep = ",",h=T,na.strings = "")


# next time, ask PhD defence date
# In what year of postdoc are you
# Ask salary AFTER taxe, for being comparable to stipend

# Salary predictors

hist(survey$salary[which(survey$gender == "Woman")],breaks = 20,col=rgb(0,1,0,.5),
     xlim=c(20000,50000),main = "Green, Women, Purple, men; Others not shown (too few)",
     xlab="Monthly salary (sek)")
hist(survey$salary[which(survey$gender == "Man")],breaks = 20,col=rgb(.2,0,1,.5),add=T)
boxplot(survey$salary~survey$position, ylab="Monthly salary (sek)", xlab="Position type", las=1)
summary(lm(salary ~ BD + gender + position + started + arrived, data = survey))

# Average responses
## II - General health questionnaire
apply(X = survey[,8:35],MARGIN = 2,FUN = mean,na.rm=T)
## III - Work-related quality of life
apply(X = survey[,36:59],MARGIN = 2,FUN = table)
barplot(apply(X = survey[,36:69],MARGIN = 2,FUN = table)$III24,main = "I am satisfied with the overall quality of my working life")
## IV - Self-efficacy scale
hist(apply(X = survey[,60:69],MARGIN = 1,FUN = sum, na.rm=T), main = "Self-efficacy scale",
      xlab="Responses sum",ylim=c(-8,40),las=1)
segments(x0 = 30,y0 = -2,x1 = 30,y1 = -8, lwd = 3)
segments(x0 = 25,y0 = -4,x1 = 25,y1 = -6, lwd = 2)
segments(x0 = 35,y0 = -4,x1 = 35,y1 = -6, lwd = 2)
segments(x0 = 25,y0 = -5,x1 = 35,y1 = -5, lwd = 1,lty = 2)
text(35,-5,"Worldwide mean and sd", pos= 4)

# Discriminant analysis

# A simple PCA reveals the trends
pca<-FactoMineR::PCA(survey[,3:69],quali.sup = c(1,2),quanti.sup = c(3,4,5))

# Linear discriminant analysis
library(MASS)
# predict gender
gender<-survey[which(survey$gender %in% c("Man","Woman")),c(3,8:69)]
lda_g<-lda(gender ~., gender)
plot(lda_g, col = as.integer(gender$gender))

for (i in 1:dim(gender)[1]) {
  genderloo<-gender[-i,]
  lda_g<-lda(gender ~., gender)
  res_t<-data.frame(observed=gender[i,"gender"], predicted=as.character(predict(lda_g,gender[i,])$class))
  if (i == 1) { res<-res_t }else{ res<-rbind(res,res_t) }
}

corrects<-length(which(res$observed==res$predicted))
missings<-length(which(is.na(res$predicted)))
incorrects<-dim(gender)[1]-(corrects+missings)

corrects/(corrects+incorrects)


# predict contract
pos<-na.omit(survey[,c(4,8:69)])
levels(as.factor(pos$position))
lda_p<-lda(position ~., pos)
plot(lda_p,col=as.integer(as.factor(pos$position)))

for (i in 1:dim(pos)[1]) {
  genderloo<-pos[-i,]
  lda_p<-lda(position ~., pos)
  res_t<-data.frame(observed=pos[i,"position"], predicted=as.character(predict(lda_p,pos[i,])$class))
  if (i == 1) { res<-res_t }else{ res<-rbind(res,res_t) }
}

corrects<-length(which(res$observed==res$predicted))
missings<-length(which(is.na(res$predicted)))
incorrects<-dim(gender)[1]-(corrects+missings)
corrects/(corrects+incorrects)




library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)