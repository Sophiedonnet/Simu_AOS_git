#rm(list = ls())
if (Sys.info()['nodename']=='PORTDONNET'){
  direc <- "D:/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/Ecologie/Code/MultipartiteCodes"
}else{
  direc <- "/home/donnet/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/Ecologie/Code/MultipartiteCodes"
}

setwd(direc)
library(GREMLIN)
library(latex2exp)
library(ggplot2)
library(mclust)

#####################################################################

nFG <- 2;
namesFG <- c('famers','plants')
v_K <- c(3,2)
v_NQ <- c(30,37)
E = matrix(1,2,2)
E[1,] = c(1,1)
E[2,] = c(1,2)
nnet <- 2
typeInter = c('diradj','inc')
# #
#
# #---------------------- parameters
   list_pi = list(c(0.31,0.42,0.27),c(0.65,0.35))
   list_theta   <- lapply(1:nnet,function(i){matrix(0,v_K[E[i,1]],v_K[E[i,2]])})
   list_theta[[1]][1,] = c(0.025,0.123,0.053)
   list_theta[[1]][2,]  = c(0.159, 0.300,0.070)
   list_theta[[1]][3,] =  c(0.374, 0.585,  0.357)
   list_theta[[2]][1,] = c(0.186 ,0.653)
   list_theta[[2]][2,] = c(0.559,0.905 )
   list_theta[[2]][3,] = c( 0.390,0.696)
#
# save(list_theta,list_pi,file = '/home/donnet/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/Ecologie/Code/generalized_multi_BM/res_simu_AoAS/res_simu_AoAS_MIRES/param1/paramSim1.Rdata')
load(file = 'res_simu_AoAS/res_simu_AoAS_MIRES/param1/paramSim1.Rdata')
ltheta_true <- list_theta
lpi_true <- list_pi

# #------------------------SIMULATION -----------------------------
nSimu = 100 ;
# v_distrib = rep('bernoulli',2)
dirSaveSimuData <- 'res_simu_AoAS/res_simu_AoAS_MIRES/param1/data'
# # #
# # for (i in 1:nSimu) {
#    datasim <- rMBM(v_NQ ,E , typeInter = typeInter, v_distrib = v_distrib, list_pi  = list_pi, list_theta = list_theta, seed = NULL, namesFG = namesFG,keepClassif = TRUE)
#    namedata  <- paste('datasim',i,'.Rdata',sep = "")
#    save(datasim,file = paste(dirSaveSimuData,namedata,sep = '/' ))
# # }


#---------------- ESTIMATION ------------------------------------
dirSaveSimuRes <- 'res_simu_AoAS/res_simu_AoAS_MIRES/param1/res_MBM'
# for (i in 1:nSimu)
# {
#   print(i)
#   #load data
#   namedata  <- paste('datasim',i,'.Rdata',sep = "")
#   load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
#   listNet <- datasim$list_Net
#   # estim
#   res_estim <- multipartiteBM(listNet, namesFG = namesFG, v_distrib = v_distrib , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1), initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
#   nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
#   save(res_estim, file = paste(dirSaveSimuRes,nameres,sep = '/' ))
# }
#

 
 ##########################################################
 #------------------- EXPLOITATION -------------------
 ###########################################################
 
 #----------------------------------------
 # #----------- VK estimés ---------------
 #----------------------------------------
 
 truev_K <- v_K_estim <- resCompar <- matrix(0,nSimu,nFG)
 for (i in 1:nSimu)
 {
   print(i)
   #load data
   namedata  <- paste('datasim',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
   trueZ <- datasim$classif
   truev_K[i,] <- vapply(1:nFG,function(q){length(unique(trueZ[[q]]))},1)
   nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
   estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
   resCompar[i,] <-  comparClassif(trueZ,estimZ)
   v_K_estim[i,] <- res_estim$fittedModel[[1]]$paramEstim$v_K
 }
 
 #-------------------------------------------
 ##---------------- TABLE des VK récupérés
 #-------------------------------------------
 unique.vK.estim <- unique.matrix(v_K_estim)
 tab <- rep(0,nrow(unique.vK.estim))
 for  (i in 1:nrow(unique.vK.estim)) {
   test <- matrix(unique.vK.estim[i,],ncol = nFG,nrow = nrow(v_K_estim),byrow = T)
   tab[i] <- sum(rowSums(test == v_K_estim) == nFG)
 }
 library(xtable)
 xtable(cbind(unique.vK.estim,t(t(tab))))
 
 
 #----------------------------------------- 
 #  q = 1  :  bad  estimation what happens
 #-----------------------------------------
 w1 <- which(abs(v_K_estim[,1] - truev_K[,1]) != 0 )
 res_1 = array(0,c(2,3,length(w1)))
 for (j in 1:length(w1)) {
   i <- w1[j]
   namedata  <- paste('datasim',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
   nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
   trueZ <- datasim$classif
   estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
   res_1[,,j] <- table(estimZ[[1]],trueZ[[1]])
 }
 
 #--------------------------------------
 ## table of simulation classif par FG 1
 #-------------------------------------- 
 table_1 <- matrix(0,nSimu,1)
 for (i in 1:nSimu) {
   namedata  <- paste('datasim',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
   table_1[i,] = table(datasim$classif[[1]])
 }
 
 
 
 ARI_FG <- as.data.frame(c(resCompar[,1],resCompar[,2]))
 ARI_FG$FG <- as.factor(c(rep(1,nSimu),rep(2,nSimu)))
 colnames(ARI_FG) = c('ARI','FG')
 ARI_FG$EstimatedK1 <- as.factor(rep(vapply(1:nSimu,function(i){v_K_estim[i,1]},1),2))
 
 g <- ggplot(ARI_FG, aes(group = FG, x = FG,y = ARI)) + geom_boxplot(outlier.shape = NA) + geom_jitter(aes(group = EstimatedK1,colour = EstimatedK1,shape = EstimatedK1),size=2,position = position_jitter(0.2),)
 g <- g + labs(shape = expression(hat(K)[1]), colour = expression(hat(K)[1]))
g
ggsave("ARIsimuMIRES.png",width = 20, height = 20, units = "cm")


 
 #-------------------------------------------------------------------------------
 ########################### RELABELLING for parameter estimation quality
 #-------------------------------------------------------------------------------
 w <- which(vapply(1:100,function(i){sum(v_K_estim[i,] == truev_K[i,]) - nFG},1)==0)
 
 all_param_estim <- array(0,c(length(w),4, 5))
 for (j in 1:length(w)){
   
   i <- w[j]
   namedata  <- paste('datasim',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
   nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
   trueZ <- datasim$classif
   estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
   
   
   relab <- lapply(1:nFG,function(q){unlist(mapClass(trueZ[[q]],estimZ[[q]])$aTOb)})
   param_estim_relab <- list(list_pi = NULL,list_theta  = NULL)
   
   ltheta_estim <-  lapply(1:nnet,function(u){
     fgRow <- E[u,1]
     fgCol <- E[u,2]
     res <- res_estim$fittedModel[[1]]$paramEstim$list_theta[[u]][relab[[fgRow]],relab[[fgCol]]]
     return(res)
   })
   lpi_estim <-  lapply(1:nFG,function(u){
     res <- res_estim$fittedModel[[1]]$paramEstim$list_pi[[u]][relab[[u]]]
     return(res)
   })
   param_estim_relab = do.call(cbind, ltheta_estim)
   vec_pi_estim <-  unlist(lpi_estim)
   #param_estim_relab <- cbind(vec_pi_estim[1:truev_K[1,1]],param_estim_relab)
   param_estim_relab <- rbind(c(vec_pi_estim),param_estim_relab)
   all_param_estim[j,,] <- param_estim_relab
 }
 
 mat_true_param <- do.call(cbind, ltheta_true)
 vec_pi <-  unlist(lpi_true)
 #mat_true_param <- cbind(vec_pi[1:truev_K[1,1]],mat_true_param)
 mat_true_param <- rbind(c(vec_pi),mat_true_param)
 
 
 MSE <- vapply(1:length(w),function(i){(all_param_estim[i,,]-mat_true_param)^2},mat_true_param)
 MSE <- apply(MSE,c(2,3),mean)
 round(MSE,4)
 
 #dimnames(all_param_estim) = list(SimuIndex = 1:73,ClusterRow = c('pi',vapply(1:7,function(k){paste('1',k,sep=',')},'1')), ClusterCol=c('pi','2,1','2,2','3,1','3,2','4,1'))
 dimnames(all_param_estim) = list(SimuIndex = 1:length(w),ClusterRow = c('pi',vapply(1:3,function(k){paste('FG1.B',k,sep='')},'1')), ClusterCol=c('FG1.B1','FG1.B2','FG1.B3','FG2.B1','FG2.B2'))
 
 
 
 library(dplyr)
 library(tidyr)
 
 table_param_estim <- as.data.frame.table(all_param_estim)
 colnames(table_param_estim) <- c('SimuIndex','Krow','Kcol','Estim')
 
 table_param_estim <- table_param_estim%>%arrange(SimuIndex)
 #table_param_estim <-  table_param_estim %>%separate( 'Kcol', c('FGcol','Blockcol'), sep = ",", remove =TRUE,
 #        convert = FALSE, extra = "warn", fill = "left")
 #table_param_estim <- table_param_estim %>%separate( 'Krow', c('FGrow','Blockrow'), sep = ",", remove =TRUE,
 #                               convert = FALSE, extra = "warn", fill = "left")
 #table_param_estim <- table_param_estim %>% mutate(FGrow = replace_na(FGrow, 0))
 #table_param_estim <-  table_param_estim  %>% mutate(FGcol = replace_na(FGcol, 0))
 
 
 #dimnames(mat_true_param) = list( ClusterRow = c('pi',vapply(1:7,function(k){paste('1',k,sep=',')},'1')), ClusterCol=c('pi','2,1','2,2','3,1','3,2','4,1'))
 dimnames(mat_true_param) = list(ClusterRow = c('pi',vapply(1:3,function(k){paste('FG1.B',k,sep='')},'1')), ClusterCol=c('FG1.B1','FG1.B2','FG1.B3','FG2.B1','FG2.B2'))
 
 
 table_param_true <- as.data.frame.table(mat_true_param)
 table_param_true$x <- rep(0.2,nrow(table_param_true))
 colnames(table_param_true) <- c('Krow','Kcol','True','x')
 #table_param_true <-  table_param_true %>%separate( 'Kcol', c('FGcol','Blockcol'), sep = ",", remove =TRUE,
 #                                                    convert = FALSE, extra = "warn", fill = "left")
 #table_param_true <- table_param_true %>%separate( 'Krow', c('FGrow','Blockrow'), sep = ",", remove =TRUE,
 #                                                    convert = FALSE, extra = "warn", fill = "left")
 #table_param_true <- table_param_true %>% mutate(FGrow = replace_na(FGrow, 0))
 #table_param_true <-  table_param_true  %>% mutate(FGcol = replace_na(FGcol, 0))
 
 
bp  <- ggplot(table_param_estim, aes(x=rep(0,length(table_param_estim$Estim)),y = Estim)) + geom_boxplot()
bp <- bp + theme(axis.title.x = element_blank(),
   axis.text.x = element_blank(),
   axis.ticks.x = element_blank(),
   axis.title.y = element_blank(),
   panel.grid.major = element_blank(), 
   panel.grid.minor = element_blank())
bp <- bp + geom_point(data = table_param_true,aes(y = True,x = x),shape=17,  color="blue", size=2)
bp <- bp + facet_grid(Krow ~ Kcol,shrink = TRUE,scales="free_y",switch = "y")
bp <- bp +  scale_y_continuous(breaks = c(0.2,0.5,1),position='right')
 
 bp
 ggsave("boxplotsimuMIRES.png",width = 20, height = 20, units = "cm")
 
 
 
 
 