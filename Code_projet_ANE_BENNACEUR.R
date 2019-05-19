setwd("C://Users//benoit//Desktop//ENS//ENSAE//2A//S2//Econo 2//projet")

#########
#Package#
#########

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
library(gmm)
library(pscl)


#################
#Chargement Base#
#################

MAGANewsData <- read.csv("C:/Users/benoit/Desktop/ENS/ENSAE/2A/S2/Econo 2/projet/MAGANewsData.csv")

#Vérification de l'unicité du tuple ('state','town','county') dans la base 
# ie : chaque ville apparait bien une seule fois
nrow(unique(MAGANewsData[,c('state','town','county')])) == nrow(MAGANewsData) #youpi

#On drop s'il y a des NA dans une de ses 3 colonnes
MAGANewsDataSNA <- subset(MAGANewsData, (!is.na(MAGANewsData$maganews1998)) & (!is.na(MAGANewsData$maganews2000) & (!is.na(MAGANewsData$maganews2003))))
nrow(MAGANewsDataSNA)/nrow(MAGANewsData)

###########################
#PARTIE 1                 #
###########################

#Question 1

unique(MAGANewsData$maganews1998)
unique(MAGANewsData$maganews2000)
unique(MAGANewsData$maganews2003)

maga_98<-sum(MAGANewsData$maganews1998,na.rm = TRUE)
maga_00<-sum(MAGANewsData$maganews2000,na.rm = TRUE)
maga_03<-sum(MAGANewsData$maganews2003,na.rm = TRUE)

maga_98/nrow(MAGANewsData) #6.3%
maga_00/nrow(MAGANewsData) #19.5%
maga_03/nrow(MAGANewsData) #52.3%


na98<-sum(is.na(MAGANewsData$maganews1998))
na03<-sum(is.na(MAGANewsData$maganews2003))
sup_98 <- (maga_98+na98)/nrow(MAGANewsData) #34.22%
sup_03 <- (maga_03+na03)/nrow(MAGANewsData) #58.9%

maga_98_NA<-sum(na.omit(MAGANewsDataSNA$maganews1998))
maga_00_NA<-sum(na.omit(MAGANewsDataSNA$maganews2000))
maga_03_NA<-sum(na.omit(MAGANewsDataSNA$maganews2003))
maga_98_NA/length(na.omit(MAGANewsDataSNA$maganews1998)) 
maga_00_NA/length(na.omit(MAGANewsDataSNA$maganews2000))
maga_03_NA/length(na.omit(MAGANewsDataSNA$maganews2003)) 

#Question 2 

control <- MAGANewsData[MAGANewsData$maganews2000 == 0,]
treated <- MAGANewsData[MAGANewsData$maganews2000 == 1,]

diffindiff <- (mean(treated$reppresfv2p2000,na.rm = TRUE)-mean(treated$reppresfv2p1996, na.rm = TRUE)) - (mean(control$reppresfv2p2000, na.rm = TRUE)-mean(control$reppresfv2p1996, na.rm = TRUE))
diffindiff

copy <- MAGANewsData[,c("maganews1998","maganews2000","reppresfv2p2000","reppresfv2p1996")]

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


sum(is.na(MAGANewsData$reppresfv2p1988))/9256
sum(is.na(MAGANewsData$reppresfv2p1992))/9256
sum(is.na(MAGANewsData$reppresfv2p1996))/9256


#Question 4

mean_sd <- function(base,variable){
  return(c(mean(base[[variable]],na.rm = TRUE),sd(base[[variable]],na.rm = TRUE)))
}

mean_sd(control,"college2000")
mean_sd(treated,"college2000")
mean_sd(MAGANewsData,"college2000")

mean_sd(control,"male2000")
mean_sd(treated,"male2000")
mean_sd(MAGANewsData,"male2000")

mean_sd(control,"black2000")
mean_sd(treated,"black2000")
mean_sd(MAGANewsData,"black2000")

mean_sd(control,"hisp2000")
mean_sd(treated,"hisp2000")
mean_sd(MAGANewsData,"hisp2000")

mean_sd(control,"married2000")
mean_sd(treated,"married2000")
mean_sd(MAGANewsData,"married2000")

mean_sd(control,"urban2000")
mean_sd(treated,"urban2000")
mean_sd(MAGANewsData,"urban2000")

mean_sd(control,"unempl2000")
mean_sd(treated,"unempl2000")
mean_sd(MAGANewsData,"unempl2000")

mean_sd(control,"pop2000")
mean_sd(treated,"pop2000")
mean_sd(MAGANewsData,"pop2000")

mean_sd(control,"reppresfv2p2000")
mean_sd(treated,"reppresfv2p2000")
mean_sd(MAGANewsData,"reppresfv2p2000")



###########################
#PARTIE 2                 #
###########################


#Question 2
model_1 <- glm(maganews2000 ~ totpreslvpop1996 + reppresfv2p1996,family=binomial(link='logit'),data=MAGANewsData)
summary(model_1)
pR2(model_1)
stargazer(model_1)
model_2 <- glm(maganews2000 ~ totpreslvpop1996 + reppresfv2p1996 +nocable1998+noch1998+sub1998 ,family=binomial(link='logit'),data=MAGANewsDataSNA)
summary(model_2)
pR2(model_2)
stargazer(model_2)
model_3 <- glm(maganews2000 ~ totpreslvpop1996 + reppresfv2p1996 +noch1998+ nocable1998 +college00m90+hs00m90 +unempl00m90+income00m90+sub1998+poptot1998+pop18p2000+black00m90+hisp00m90,family=binomial(link='logit'),data=MAGANewsDataSNA)
summary(model_3)
pR2(model_3)
stargazer(model_3)

#Question 5



g <- function(beta, data) {
  #mettre un dataframe avec le traitement D en premier et les variables à prendre ensuite
  data<-as.data.frame(data)
  d <- as.numeric(data[,1])
  x <- data.matrix(data[, 2:ncol(data)])
  m <- as.vector((exp(x%*%beta)*(as.vector(1-d))))*as.matrix(x)
  return(cbind(m))
}

data_3 <- MAGANewsDataSNA[,c("maganews2000","totpreslvpop1996", "reppresfv2p1996","noch1998", "nocable1998", "college00m90","hs00m90","unempl00m90","income00m90","sub1998","poptot1998","pop18p2000","black00m90","hisp00m90")]
init3 <- (lm(maganews2000 ~ 0+totpreslvpop1996 + reppresfv2p1996 +noch1998+ nocable1998 +college00m90+hs00m90 +unempl00m90+income00m90+sub1998+poptot1998+pop18p2000+black00m90+hisp00m90,data = MAGANewsDataSNA))$coefficients
gmm3 <- gmm(g,na.omit(data_3),t0=init3,type="iterative",itermax = 5000,tol = 1e-15)
summary(gmm3)
stargazer(gmm3)

data_2 <- MAGANewsDataSNA[,c("maganews2000","totpreslvpop1996", "reppresfv2p1996","noch1998", "nocable1998","sub1998")]
init2 <- (lm(maganews2000 ~ 0+totpreslvpop1996 + reppresfv2p1996 +noch1998+ nocable1998 +sub1998,data = MAGANewsDataSNA))$coefficients
gmm2 <- gmm(g,na.omit(data_2),t0=init2,type="iterative",itermax = 5000,tol = 1e-15)
summary(gmm2)
stargazer(gmm2)


data_1 <- MAGANewsDataSNA[,c("maganews2000","totpreslvpop1996", "reppresfv2p1996")]
init1 <- (lm(maganews2000 ~ 0+totpreslvpop1996 + reppresfv2p1996,data = MAGANewsDataSNA))$coefficients
gmm1 <- gmm(g,na.omit(data_1),t0=init1,type="iterative",itermax = 5000,tol = 1e-15)
summary(gmm1)
stargazer(gmm1)

nrow(na.omit(data_3))
nrow(na.omit(data_2))
nrow(na.omit(data_1))

theta_chap <- function(coefs,data){
  #maganews2000 en deuxieme colonne
  #en premiere colonne il faut la variable reppresfv2p00m96
  #puis a partir de la variable 3 c'est les X
  data<-as.data.frame(data)
  y <- as.numeric(data[,1])
  d <- as.numeric(data[,2])
  x <- data.matrix(data[,3:ncol(data)])
  e <- exp(x%*%coefs)
  return((1/sum(d,na.rm = T))*sum(y*(d-(1-d)*e),na.rm = T))
}

data_chap_model3 <- MAGANewsDataSNA[,c("reppresfv2p00m96" ,"maganews2000","totpreslvpop1996", "reppresfv2p1996","noch1998", "nocable1998", "college00m90","hs00m90","unempl00m90","income00m90","sub1998","poptot1998","pop18p2000","black00m90","hisp00m90")]
theta_chap(as.matrix(gmm3$coefficients),na.omit(data_chap_model3)) #estimateur theta chapeau pour le modele 3

data_chap_model2 <- MAGANewsDataSNA[,c("reppresfv2p00m96", "maganews2000","totpreslvpop1996", "reppresfv2p1996","noch1998", "nocable1998","sub1998")]
theta_chap(as.matrix(gmm2$coefficients),na.omit(data_chap_model2)) #estimateur theta chapeau pour le modele 2

data_chap_model1 <- MAGANewsDataSNA[,c("reppresfv2p00m96", "maganews2000","totpreslvpop1996", "reppresfv2p1996")]
theta_chap(as.matrix(gmm1$coefficients),na.omit(data_chap_model1)) #estimateur theta chapeau pour le modele 1

var_theta_chap <- function(coefs,data){
  #coefs les coefficients du gmm
  #data exactement les memes formats que les data_chap_modeli
  #comme pour theta_chap, il faut lancer avec na.omit(data_chap...)
  #sinon des NA se mettent dans les matrices et ça explose 
  data<-as.data.frame(data)
  y <- as.numeric(data[,1])
  d <- as.numeric(data[,2])
  x <- data.matrix(data[,3:ncol(data)])
  II <- c()
  FF <- c()
  U<-c()
  for (i in 1:dim(x)[1]){
    yi <- y[i]
    di <- d[i]
    xi <- x[i,]
    ei <- exp(xi %*% coefs)
    I <- sum(t(xi)%*%xi * (1-di)*ei) 
    II <- c(II,I)
    f<-(di-(1-di)*ei)
    u <- yi*(1-di)*as.vector(xi)*ei
    U <- c(U,u)
  }
  EU <- sum(U)/length(U)
  EI <- sum(II)/length(II)
  EI <- 1/EI
  res=c()
  for (i in 1:dim(x)[1]){
    yi <- y[i]
    di <- d[i]
    xi <- x[i,]
    ei <- exp(xi %*% coefs)
    f1i <- (di-(1-di)*ei)
    aux <- f1i*(yi-xi*EU*EI)
    aux2 <- aux*aux
    res <- c(res,aux2)
  }
  fin <- sum(res)/length(res)
  fin <- fin/(mean(d)^2)
  return(fin)
}

sd_theta_chap3 <- sqrt(var_theta_chap(as.matrix(gmm3$coefficients),na.omit(data_chap_model3)))
sd_theta_chap2 <- sqrt(var_theta_chap(as.matrix(gmm2$coefficients),na.omit(data_chap_model2)))
sd_theta_chap1 <- sqrt(var_theta_chap(as.matrix(gmm1$coefficients),na.omit(data_chap_model1)))


###################################
#Partie 3                         #
###################################

#Question 1

reg_1 <- lm(reppresfv2p00m96 ~  maganews2000+ totpresvotes1996 + reppresfv2p1996 + reppresfv2p96m92 + nocable1998 +college00m90+hs00m90 +unempl00m90+income00m90+sub1998+poptot1998+pop18p2000+black00m90+hisp00m90,data=MAGANewsDataSNA,weights=MAGANewsDataSNA$totpresvotes1996)
summary(reg_1)
stargazer((reg_1))

reg_2 <- felm(reppresfv2p00m96 ~ maganews2000 + totpresvotes1996 + reppresfv2p1996 + reppresfv2p96m92 + nocable1998 +college00m90+hs00m90 +unempl00m90+income00m90+sub1998+poptot1998+pop18p2000+black00m90+hisp00m90 |county, data=MAGANewsDataSNA,weights=MAGANewsDataSNA$totpresvotes1996)
summary(reg_2)
stargazer((reg_2))



###################################
#Partie 4                         #
###################################

data_placebo_1 <- MAGANewsDataSNA[,c("county","reppresfv2p96m92","reppresfv2p1996","reppresfv2p1988","totpresvotes1996", "maganews2000","nocable1998","college00m90","hs00m90","unempl00m90","income00m90","sub1998","poptot1998","pop18p2000","black00m90","hisp00m90")] 
data_placebo_1 <- na.omit(data_placebo_1)

we<-c(data_placebo_1$totpresvotes1996)
placebo_1 <- felm(reppresfv2p96m92 ~ maganews2000 + totpresvotes1996 + reppresfv2p1996+ reppresfv2p1988 + nocable1998 +college00m90+hs00m90 +unempl00m90+income00m90+sub1998+poptot1998+pop18p2000+black00m90+hisp00m90 | county ,data=data_placebo_1,weights=we)
summary(placebo_1)
stargazer(placebo_1)

data_placebo_2 <- MAGANewsDataSNA[,c("county","reppresfv2p1996","reppresfv2p1988","totpresvotes1996","reppresfv2p92m88", "maganews2000", "nocable1998","college00m90","hs00m90","unempl00m90","income00m90","sub1998","poptot1998","pop18p2000","black00m90","hisp00m90")] 
data_placebo_2 <- na.omit(data_placebo_2)

we<-c(data_placebo_2$totpresvotes1996)
placebo_2 <- felm(reppresfv2p92m88 ~ maganews2000 + totpresvotes1996 + reppresfv2p1996 + reppresfv2p1988 + nocable1998 +college00m90+hs00m90 +unempl00m90+income00m90+sub1998+poptot1998+pop18p2000+black00m90+hisp00m90 | county ,data=data_placebo_2,weights=we)
summary(placebo_2)
stargazer(placebo_2)
