setwd("/home/administrateur/Documents/depotgit/Simu_AOS_git")
library(GREMLIN)
#library(latex2exp)
library(ggplot2)
library(mclust)



namesFG <- c('plants','pollinators','birds','ants')
nFG <- 4;
v_K <- c(7,2,2,1)
v_NQ <- c(141,173,46,30)
E = matrix(1,nFG - 1,2)
E[,2] = c(2,3,4)
nnet =  3
typeInter = c('inc','inc','inc')
v_distrib = rep('bernoulli',3)


load (file = 'res_simu_AoAS/res_simu_AoAS_DATTILO/param2/paramSim2.Rdata')
ltheta_true <- ltheta
lpi_true <- lpi


dirSaveSimuData <- 'res_simu_AoAS/res_simu_AoAS_DATTILO/param2/data'
dirSaveSimuRes <- 'res_simu_AoAS/res_simu_AoAS_DATTILO/param2/res_MBM'

nSimu = 100



truev_K <- v_K_estimJoin <- resComparJoin  <- matrix(0, nSimu, nFG)
resComparPlFl <- v_K_estimPlFl <- matrix(0, nSimu, 2)
resComparPlAn <- v_K_estimPlAn <- matrix(0, nSimu, 2)
resComparPlSe <- v_K_estimPlSe <- matrix(0, nSimu, 2)

resComparPlFlAn <- v_K_estimPlFlAn <- matrix(0, nSimu, 3)
resComparPlFlSe <- v_K_estimPlFlSe <- matrix(0, nSimu, 3)
resComparPlAnSe <- v_K_estimPlAnSe <- matrix(0, nSimu, 3)

for (i in 1:nSimu)
{
  print(i)
  #load data
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  list_Net <- datasim$list_Net
  trueZ <- datasim$classif
  truev_K[i,] <- vapply(1:4,function(q){length(unique(trueZ[[q]]))},1)
  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
  # MBM reload saved estimation
  load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparJoin[i,] <-  comparClassif(trueZ,estimZ)
  v_K_estimJoin[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K


  ## Plant Flovis
  iNet = 1
  iFG = c(1,2)
  res_estim <- multipartiteBM(list_Net[iNet], namesFG = namesFG[iFG], v_distrib = v_distrib[iNet] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1)[iFG], initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparPlFl[i,] <- comparClassif(trueZ[iFG], estimZ)
  v_K_estimPlFl[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K


  ## Plant Ant
  iNet = 2
  iFG = c(1,3)
  res_estim <- multipartiteBM(list_Net[iNet], namesFG = namesFG[iFG], v_distrib = v_distrib[iNet] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1)[iFG], initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparPlAn[i,] <- comparClassif(trueZ[iFG], estimZ)
  v_K_estimPlAn[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K


  ## Plant Seed
  iNet = 3
  iFG = c(1,4)
  res_estim <- multipartiteBM(list_Net[iNet], namesFG = namesFG[iFG], v_distrib = v_distrib[iNet] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1)[iFG], initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparPlSe[i,] <- comparClassif(trueZ[iFG], estimZ)
  v_K_estimPlSe[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K



  ## Plant Flovis Ant
  iNet = 1:2
  iFG = 1:3
  res_estim <- multipartiteBM(list_Net[iNet], namesFG = namesFG[iFG], v_distrib = v_distrib[iNet] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1)[iFG], initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparPlFlAn[i,] <- comparClassif(trueZ[iFG], estimZ)
  v_K_estimPlFlAn[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K


  ## Plant Flovis Seed
  iNet = c(1,3)
  iFG = c(1,2,4)
  res_estim <- multipartiteBM(list_Net[iNet], namesFG = namesFG[iFG], v_distrib = v_distrib[iNet] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1)[iFG], initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparPlFlSe[i,] <- comparClassif(trueZ[iFG], estimZ)
  v_K_estimPlFlSe[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K


  ## Plant Ant Seed
  iNet = c(2,3)
  iFG = c(1,3,4)
  res_estim <- multipartiteBM(list_Net[iNet], namesFG = namesFG[iFG], v_distrib = v_distrib[iNet] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1)[iFG], initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparPlAnSe[i,] <- comparClassif(trueZ[iFG], estimZ)
  v_K_estimPlAnSe[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K



}

save(v_K_estimJoin,resComparJoin,
     resComparPlFl,v_K_estimPlFl,
     resComparPlAn,v_K_estimPlAn,
     resComparPlSe,v_K_estimPlSe,
     resComparPlFlAn,v_K_estimPlFlAn,
     resComparPlFlSe,v_K_estimPlFlSe,
     resComparPlAnSe,v_K_estimPlAnSe,
     file="res_simu_AoAS/comparisonJoinSepDattilo2.Rdata")




##### plotting
library(reshape2)
library(ggplot2)
library(plyr)

dfARIPlant = data.frame(PlFl = resComparPlFl[,1],PlFlAn=resComparPlFlAn[,1],PlFlAnSe = resComparJoin[,1])
dfARIPlant = melt(dfARIPlant,value.name = "ARI",variable.name = "Model")
#dfARIPlant$Model =revalue(dfARIPlant$Model, c("PlFl"="PlFl", "PlFlAn"="PlFlAn","Join"="PlFlSe"))
#dfARIPlant$Model = factor(dfARIPlant$Model, c("PlFl","PlFlAn","PlFlSe"))
ggplot(dfARIPlant,aes(x=Model,y=AUC)) +geom_boxplot()


dfARIFlovis = data.frame(PlFl=resComparPlFl[,2],PlFlAn=resComparPlFlAn[,2],PlFlAnSe = resComparJoin[,2])
dfARIFlovis = melt(dfARIFlovis,value.name = "ARI",variable.name = "Model")
ggplot(dfARIFlovis,aes(x=Model,y=AUC)) +geom_boxplot()


