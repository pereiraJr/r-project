library(dummies)
library(dplyr)

smellsAbril <- read.csv('data/bad_smells_abril.csv', header = T)
smellsMaio <- read.csv('data/bad_smells_maio.csv', header = T)
qtdColsAbril <- ncol(smellsAbril)

smellsAbrilCleaned <-smellsAbril %>% group_by(package_name) %>% summarise(blob=sum(BLOB), 
                                                     lm=sum(LM), 
                                                     sak=sum(SAK), 
                                                     cc=sum(CC), 
                                                     igs=sum(IGS), 
                                                     mim=sum(MIM), 
                                                     nlmr=sum(NLMR), 
                                                     lic=sum(LIC))

#codeSmellsAbrilCleaned <- smellsAbrilCleaned
smellsAbrilCleaned$total <- rowSums(smellsAbrilCleaned[,-1])
smellsAbrilCleaned <- smellsAbrilCleaned[order(-smellsAbrilCleaned$total),]

maioSum <-sum(smellsMaioCleaned[,-1])
abrilSum <- sum(smellsAbrilCleaned[,-1])

sum(colMeans(smellsAbril[,-1:-2]))/ncol(smellsMaioCleaned[,-1])

mediaCodeSmellsAbril <- mean(data.matrix(codeSmellsAbrilCleaned[,-1]))

#Removendo a Coluna de Total
mediaCodeSmellsAbril <- mean(data.matrix(smellsAbrilCleaned[,-1:ncol(smellsAbrilCleaned)]))

paste('Code Smells de Abril:  ',  abrilSum)
paste('O pacote com mais bugs em Abril foi o :', head(smellsAbrilCleaned$package_name,1))
paste('MEDIA CODE SMELL ABRIL', mediaCodeSmellsAbril)