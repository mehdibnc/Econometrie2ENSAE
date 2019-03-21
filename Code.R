setwd("D:/Documents/ensae cours/S2/econometrie 2/projet")


#Requirements
library(haven)


#Data loading
MAGANewsData <- read_dta("D:/Documents/ensae cours/S2/econometrie 2/projet/MAGANewsData.dta")
#Vérification de l'unicité du tuple ('state','town','county') dans la base 
# ie : chaque ville apparait bien une seule fois. 
nrow(unique(MAGANewsData[,c('state','town','county')])) == nrow(MAGANewsData) #youpi


#Statistiques descriptives

#Question 1
unique(MAGANewsData$maganews1998)
unique(MAGANewsData$maganews2000)
unique(MAGANewsData$maganews2003)

#Si on ignore les NA: 
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


#Question 2 
control <- MAGANewsData[MAGANewsData$maganews2000 == 0,]
treated <- MAGANewsData[MAGANewsData$maganews2000 == 1,]

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

reg <- lm(reppresfv2p1996 ~time + treated + interaction, data=pane)
summary(reg)




#Question 3 
#L'estimateur n'est pas crédible car... ? common trend assumption non satisfaite probablement
#



#Question 4
#Regardons quelques statistiques descriptives pour les groupes de traitement et de controle 
#en période 1 (en 98-96). Pour montrer que les groupes ne sont pas forcément comparable.








