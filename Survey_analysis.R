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

lda_g<-lda(gender ~., gender)
ld1_g<-predict(lda_g,gender)$x
ld1_g_cor<-cor(x = gender[,-1],y = ld1_g,use = "pairwise.complete.obs",method = "pearson")
rownames(ld1_g_cor)[order(ld1_g_cor)]

# By increasing correlation with ld1:

#III12 I am involved in decisions that affect me in my own area of work
lda_g$means[,"III12"]
#Man    Woman 
#3.975000 3.509434 

#III19 I often feel excessive levels of stress at work
lda_g$means[,"III19"]
#Man   Woman 
#2.57500 3.09434 

#C6 felt capable of making decisions about things? 2 Same as usual; 3 Less so than usual
lda_g$means[,"C6"]
#Man    Woman 
#2.025000 2.339623 

#B2 had difficulty in staying asleep once you are off? 1 Not at all; 2 No more than usual; 3 Rather more than usual;
lda_g$means[,"B2"]
#Man    Woman 
#1.725000 2.188679 

#A7 been having hot or cold spells? 1 Not at all; 2 No more than usual; 3 Rather more than usual;
lda_g$means[,"A7"]
#Man    Woman 
#1.375000 1.811321

#A5 been getting any pains in your head? 1 Not at all; 2 No more than usual; 3 Rather more than usual;
lda_g$means[,"A5"]
#Man    Woman 
#1.600000 2.037736 


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

lda_p<-lda(position ~., pos)
ld1_p<-predict(lda_p,pos)$x
ld1_p_cor<-cor(x = pos[,-1],y = ld1_p,use = "pairwise.complete.obs",method = "pearson")

# ld1
ld1_p_cor[,1][order(ld1_p_cor[,1])]
rownames(ld1_p_cor)[order(ld1_p_cor[,1])]

#III21 Recently, I have been feeling reasonably happy all things considered
round(sort(lda_p$means[,"III21"]),2)
#stipend forskare  postdoc 
#2.94     3.06     3.44

#III8 When I have done a good job it is acknowledged by my line manager
round(sort(lda_p$means[,"III8"]),2)
#postdoc forskare  stipend 
#3.70     3.83     4.31 


#ld2
ld1_p_cor[,2][order(ld1_p_cor[,2])]
rownames(ld1_p_cor)[order(ld1_p_cor[,2])]

#IV5 Thanks to my resourcefulness, I know how to handle unforeseen situations.
round(sort(lda_p$means[,"IV5"]),2)
#forskare  postdoc  stipend 
#2.83     3.29     3.31 

#A6 been getting a feeling of tightness or pressure in your head?
round(sort(lda_p$means[,"A6"]),2)
#forskare  postdoc  stipend 
#1.67     2.14     2.25 

#D4
round(sort(lda_p$means[,"D4"]),2)
#forskare  postdoc  stipend 
#1.11     1.30     1.75 


library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)