
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


dirSaveSimuRes <- 'res_simu_AoAS/res_simu_AoAS_MIRES/param1/res_MBM'



for (i in 1:nSimu)
{
  print(i)
  #load data
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  listNet <- datasim$list_Net
  # estim
  res_estim <- multipartiteBM(listNet, namesFG = namesFG, v_distrib = v_distrib , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1), initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
  save(res_estim, file = paste(dirSaveSimuRes,nameres,sep = '/' ))
}
