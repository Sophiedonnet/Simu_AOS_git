
setwd("/home/administrateur/Documents/depotgit/Simu_AOS_git")
library(GREMLIN)
#library(latex2exp)
library(ggplot2)
library(mclust)



nFG <- 2;
namesFG <- c('famers','plants')
v_K <- c(3,2)
v_NQ <- c(30,37)
E = matrix(1,2,2)
E[1,] = c(1,1)
E[2,] = c(1,2)
nnet <- 2
typeInter = c('diradj','inc')
v_distrib = rep('bernoulli',2)


dirSaveSimuData <- 'res_simu_AoAS/res_simu_AoAS_MIRES/param1/data'
dirSaveSimuRes <- 'res_simu_AoAS/res_simu_AoAS_MIRES/param1/res_MBM'

nSimu = 100

truev_K <- v_K_estimJoin <- resComparJoin  <- matrix(0, nSimu, nFG)
resComparRel <- v_K_estimRel <- matrix(0, nSimu, 1)
resComparInv <- v_K_estimInv <- matrix(0, nSimu, nFG)


for (i in 1:nSimu)
{
  print(i)
  #load data
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  listNet <- datasim$list_Net
  trueZ <- datasim$classif
  truev_K[i, ] <-
    vapply(1:nFG, function(q) {
      length(unique(trueZ[[q]]))
    }, 1)

  # MBM reload saved estimation
  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuRes, nameres, sep = '/'))
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  resComparJoin[i, ] <-  comparClassif(trueZ, estimZ)
  v_K_estimJoin[i, ] <- res_estim$fittedModel[[1]]$paramEstim$v_K

  ## separated BM
  ### farmer
  res_estimRel <- multipartiteBM(listNet[1], namesFG = namesFG[1], v_distrib = v_distrib[1] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = 1, initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estimRel$fittedModel[[1]]$paramEstim$Z
  resComparRel[i,] <- comparClassif(trueZ[1], estimZ)
  v_K_estimRel[i,] <- res_estimRel$fittedModel[[1]]$paramEstim$v_K


  ### Inventory
  res_estimInv <- multipartiteBM(listNet[2], namesFG = namesFG[1:2], v_distrib = v_distrib[2] , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1), initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  estimZ <- res_estimInv$fittedModel[[1]]$paramEstim$Z
  resComparInv[i,] <- comparClassif(trueZ, estimZ)
  v_K_estimInv[i,] <- res_estimInv$fittedModel[[1]]$paramEstim$v_K
}

#save(v_K_estimJoin,resComparJoin,resComparRel,v_K_estimRel,resComparInv,v_K_estimInv,file="res_simu_AoAS/comparisonJoinSepMIRES.Rdata")



load("res_simu_AoAS/comparisonJoinSepMIRES.Rdata")

##### plotting
library(reshape2)
library(ggplot2)
library(gridExtra)

dfARIfarm = data.frame(Rel = resComparRel[,1],Inv=resComparInv[,1],RelInv = resComparJoin[,1])
dfARIfarm = melt(dfARIfarm,value.name = "ARI",variable.name = "Data")
p1=ggplot(dfARIfarm,aes(x=Data,y=ARI)) +geom_boxplot() +theme_bw() + ylim(c(0,1)) + ggtitle("Farmers clustering")


dfARIplant = data.frame(Inv=resComparInv[,2],RelInv = resComparJoin[,2])
dfARIplant = melt(dfARIplant,value.name = "ARI",variable.name = "Data")
p2 =ggplot(dfARIplant,aes(x=Data,y=ARI)) +geom_boxplot() +theme_bw() + ylim(c(0,1)) + ggtitle("Species clustering")

pdf("~/Dropbox/Multiplex/Ecologie/article/StatisticalModeling/review1/Paper_and_Supplementary/MiresClustering.pdf")
grid.arrange(p1,p2,nrow=1)
dev.off()



unique.vK.estim <- unique.matrix(v_K_estimJoin)
tab <- rep(0, nrow(unique.vK.estim))
for (i in 1:nrow(unique.vK.estim)) {
  test <-
    matrix(
      unique.vK.estim[i, ],
      ncol = nFG,
      nrow = nrow(v_K_estimJoin),
      byrow = T
    )
  tab[i] <- sum(rowSums(test == v_K_estimJoin) == nFG)
}
library(xtable)
xtable(cbind(unique.vK.estim, t(t(tab))),digits=0)


unique.vK.estim <- unique.matrix(v_K_estimInv)
tab <- rep(0, nrow(unique.vK.estim))
for (i in 1:nrow(unique.vK.estim)) {
  test <-
    matrix(
      unique.vK.estim[i, ],
      ncol = nFG,
      nrow = nrow(v_K_estimInv),
      byrow = T
    )
  tab[i] <- sum(rowSums(test == v_K_estimInv) == nFG)
}
library(xtable)
xtable(cbind(unique.vK.estim, t(t(tab))),digits=0)


unique.vK.estim <- unique.matrix(v_K_estimRel)
tab <- rep(0, nrow(unique.vK.estim))
for (i in 1:nrow(unique.vK.estim)) {
  test <-
    matrix(
      unique.vK.estim[i, ],
      ncol = 1,
      nrow = nrow(v_K_estimRel),
      byrow = T
    )
  tab[i] <- sum(rowSums(test == v_K_estimRel) ==1)
}
library(xtable)
xtable(cbind(unique.vK.estim, t(t(round(tab)))),digits=0)




