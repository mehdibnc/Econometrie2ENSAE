setwd("C://Users//benoit//Desktop//ENS//ENSAE//2A//S2//Econo 2//projet")

#Requirements
library(haven)
library(utils)
library(ggplot2)
library(stargazer)
library(Hmisc)
library(dplyr)
library(plyr)
library(reshape2)
library(readr)
library(lfe)

#Data loading
MAGANewsData <- read.csv("C:/Users/benoit/Desktop/ENS/ENSAE/2A/S2/Econo 2/projet/MAGANewsData.csv")

#Vérification de l'unicité du tuple ('state','town','county') dans la base 
# ie : chaque ville apparait bien une seule fois
nrow(unique(MAGANewsData[,c('state','town','county')])) == nrow(MAGANewsData) #youpi

#On drop s'il y a des NA dans une de ses 3 colonnes
MAGANewsDataSNA <- subset(MAGANewsData, (!is.na(MAGANewsData$maganews1998)) & (!is.na(MAGANewsData$maganews2000) & (!is.na(MAGANewsData$maganews2003))))
nrow(MAGANewsDataSNA)/nrow(MAGANewsData)

###########################
#Statistiques descriptives#
###########################

#Question 1
unique(MAGANewsData$maganews1998)
unique(MAGANewsData$maganews2000)
unique(MAGANewsData$maganews2003)

#Si on ignore les NA: (A DISCUTER AVEC MEHDI)
maga_98<-sum(MAGANewsData$maganews1998,na.rm = TRUE)
maga_00<-sum(MAGANewsData$maganews2000,na.rm = TRUE)
maga_03<-sum(MAGANewsData$maganews2003,na.rm = TRUE)

#proportion de villes ayant accés à MAGA news en 1998, 2000 et 2003 : 
maga_98/nrow(MAGANewsData) #6.3%
maga_00/nrow(MAGANewsData) #19.5%
maga_03/nrow(MAGANewsData) #52.3%


na98<-sum(is.na(MAGANewsData$maganews1998))
na03<-sum(is.na(MAGANewsData$maganews2003))
sup_98 <- (maga_98+na98)/nrow(MAGANewsData) #34.22%
sup_03 <- (maga_03+na03)/nrow(MAGANewsData) #58.9%

#Si on ne considère pas les lignes avec des NA pour chaque année considérée
maga_98_NA<-sum(na.omit(MAGANewsDataSNA$maganews1998))
maga_00_NA<-sum(na.omit(MAGANewsDataSNA$maganews2000))
maga_03_NA<-sum(na.omit(MAGANewsDataSNA$maganews2003))
maga_98_NA/length(na.omit(MAGANewsDataSNA$maganews1998)) 
maga_00_NA/length(na.omit(MAGANewsDataSNA$maganews2000))
maga_03_NA/length(na.omit(MAGANewsDataSNA$maganews2003)) 

#Question 2 
control <- MAGANewsData[MAGANewsData$maganews2000 == 0,]
treated <- MAGANewsData[MAGANewsData$maganews2000 == 1,]




#Via la formule
diffindiff <- (mean(treated$reppresfv2p2000,na.rm = TRUE)-mean(treated$reppresfv2p1996, na.rm = TRUE)) - (mean(control$reppresfv2p2000, na.rm = TRUE)-mean(control$reppresfv2p1996, na.rm = TRUE))
diffindiff

copy <- MAGANewsData[,c("maganews1998","maganews2000","reppresfv2p2000","reppresfv2p1996")]

#dataset pour la regression du diffindiff qui permettra d'obtenir estimateur et variance
pane <- rbind(copy[,c("reppresfv2p1996","maganews1998")],setNames(copy[,c("reppresfv2p2000","maganews2000")],c("reppresfv2p1996","maganews1998")))
period <- rep(0,nrow(pane))
period[(nrow(copy)+1):nrow(pane)]<-1
pane["time"] <- period
pane["treated"] <- c(as.numeric(copy$maganews2000 == 1),as.numeric(copy$maganews2000 == 1))
pane["interaction"] <- pane$time * pane$treated
colnames(pane) <- c('propor_vote_rep','maganews','year_2000','treated','interaction')
reg <- lm(propor_vote_rep ~year_2000 + treated + interaction, data=pane)
summary(reg)
stargazer(reg)

#Question 3 
#L'estimateur n'est pas crédible car common trend assumption non satisfaite probablement

#Rep. 2-party vote share in Presidential elections in 1988
mean(control$reppresfv2p1988,na.rm = TRUE) 
mean(treated$reppresfv2p1988,na.rm = TRUE) 
mean(control$reppresfv2p1992,na.rm = TRUE) 
mean(treated$reppresfv2p1992,na.rm = TRUE) 
mean(control$reppresfv2p1996,na.rm = TRUE) 
mean(treated$reppresfv2p1996,na.rm = TRUE)

year<-c("1988", "1992", "1996")
vote_rep_control<-c(mean(control$reppresfv2p1988,na.rm = TRUE),mean(control$reppresfv2p1992,na.rm = TRUE),mean(control$reppresfv2p1996,na.rm = TRUE) )
vote_rep_treated<-c(mean(treated$reppresfv2p1988,na.rm = TRUE) ,mean(treated$reppresfv2p1992,na.rm = TRUE),mean(treated$reppresfv2p1996,na.rm = TRUE))
df<-data.frame(year,vote_rep_control, vote_rep_treated, stringsAsFactors=FALSE)

plot(df$vote_rep_control, type="l", col="blue", xlab="année", ylab="Part du vote républicain", main="",  xaxt="n")+
  axis(1, at=seq(1, 3, by=1), labels = c("1988","1992","1996"))
  lines(df$vote_rep_treated, col='red')+
  legend("topright", lty=c(1,1), col=c("blue","red"), legend = c("Control", "treated"))



#Question 4

mean_sd <- function(base,variable){
  return(c(mean(base[[variable]],na.rm = TRUE),sd(base[[variable]],na.rm = TRUE)))
}
  
mean_sd(control,"college2000")
mean_sd(treated,"college2000")

mean_sd(control,"male2000")
mean_sd(treated,"male2000")

mean_sd(control,"black2000")
mean_sd(treated,"black2000")

mean_sd(control,"hisp2000")
mean_sd(treated,"hisp2000")

mean_sd(control,"married2000")
mean_sd(treated,"married2000")

mean_sd(control,"urban2000")
mean_sd(treated,"urban2000")

mean_sd(control,"unempl2000")
mean_sd(treated,"unempl2000")
  
mean_sd(control,"pop2000")
mean_sd(treated,"pop2000")
  
mean_sd(control,"reppresfv2p2000")
mean_sd(treated,"reppresfv2p2000")
  
  
###################################
#Selection et Score de Propension#
###################################


#Question 1
#Ecriture

#Question 2
model_1 <- glm(maganews2000 ~ totpreslvpop1996 + reppresfv2p1996,family=binomial(link='logit'),data=MAGANewsData)
summary(model_1)
stargazer(model_1)
model_2 <- glm(maganews2000 ~ totpreslvpop1996 + reppresfv2p1996 + nocable2000 + nocable1998 + noch2000 + noch1998,family=binomial(link='logit'),data=MAGANewsDataSNA)
summary(model_2)
stargazer(model_2)
model_3 <- glm(maganews2000 ~ totpreslvpop1996 + reppresfv2p1996 + nocable2000 + nocable1998 + noch2000 + noch1998 + hs2000+college2000+male2000+married2000+hisp2000+black2000+pop2000+unempl2000+income2000,family=binomial(link='logit'),data=MAGANewsDataSNA)
summary(model_3)
stargazer(model_3)
#Question 3


#Question 4
#calcul

#Question 5
#GMM 
moments <- function(beta, data) {
  #mettre un dataframe avec le traitement D en premier et les variables à prendre ensuite
  data<-as.data.frame(data)
  d <- as.numeric(data[,1])
  x <- data.matrix(data[, 2:ncol(data)])
  m <- t(exp(x%*%beta)*(1-d)-d)%*%as.matrix(x)
  return(cbind(m))
}


data_1 <- MAGANewsDataSNA[,c("maganews2000","totpreslvpop1996", "reppresfv2p1996")]
moments(beta = as.matrix(c(1:2)),data = data_1)

gmm_1<- gmm(moments,data_1)


summary(gmm_1)

###################################
#Estimation d'impact par reg lin  #
###################################

reg_1 <- lm(reppresfv2p00m96 ~ maganews2000 + totpreslvpop1996 + reppresfv2p1996 + nocable2000 + nocable1998 + noch2000 + noch1998 + hs2000+college2000+male2000+married2000+hisp2000+black2000+pop2000+unempl2000+income2000,data=MAGANewsDataSNA)
summary(reg_1)
reg_2 <- felm(reppresfv2p00m96 ~ maganews2000 + totpreslvpop1996 + reppresfv2p1996 + nocable2000 + nocable1998 + noch2000 + noch1998 + hs2000+college2000+male2000+married2000+hisp2000+black2000+pop2000+unempl2000+income2000 | county ,data=MAGANewsDataSNA)
summary(reg_2)

###################################
#Effet placebo                    #
###################################

placebo_1 <- felm(reppresfv2p96m92 ~ maganews2000 + totpreslvpop1996 + reppresfv2p1996 + nocable2000 + nocable1998 + noch2000 + noch1998 + hs2000+college2000+male2000+married2000+hisp2000+black2000+pop2000+unempl2000+income2000 | county ,data=MAGANewsDataSNA)
summary(placebo_1)
placebo_2 <- felm(reppresfv2p92m88 ~ maganews2000 + totpreslvpop1996 + reppresfv2p1996 + nocable2000 + nocable1998 + noch2000 + noch1998 + hs2000+college2000+male2000+married2000+hisp2000+black2000+pop2000+unempl2000+income2000 | county ,data=MAGANewsDataSNA)
summary(placebo_2)










