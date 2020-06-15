#rm(list = ls())
if (Sys.info()['nodename']=='PORTDONNET'){
  direc <- "D:/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/Ecologie/Code/MultipartiteCodes"
}else{
  direc <- "~/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/MULTIPARTITE/CODES/SIMU_PAPIER_AOS/Simu_AOS_git/"
}



setwd(direc)
library(GREMLIN)
library(latex2exp)
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


#---------------------- parameters : param1

# ### param1
# list_pi = list(c(0.4675,0.1606, 0.1351,0.0784,0.1061, 0.0142,0.0381),c(0.06,0.94),c(0.1,0.9),c(1))
#
# # ## param 2
# # list_pi[[1]] = c(0.46,0.16, 0.15,0.184,0.1061, 0.1,0.1)
# # list_pi[[1]] = list_pi[[1]]/sum(list_pi[[1]])
#
#
# list_theta   <- lapply(1:nnet,function(i){matrix(0,v_K[E[i,1]],v_K[E[i,2]])})
# list_theta[[1]][1,] = c(0.0957,0.0075)
# list_theta[[1]][2,1]  = 0.01
# list_theta[[1]][3,2] = 0.0003
# list_theta[[1]][4,] = c(0.1652, 0.0343)
# list_theta[[1]][5,] = c(0.2018,0.138)
#
# list_theta[[2]][1,2] = 0.0006
# list_theta[[2]][2,] = c(0.5431, 0.0589)
# list_theta[[2]][4,] = c(0.6620,0.1542)
# list_theta[[2]][7,] = c(0.8492,0.3565)
#
# list_theta[[3]][1,] = c(0.0013)
# list_theta[[3]][3,] = 0.0753
# list_theta[[3]][5,] = 0.0163
# list_theta[[3]][6,] = 0.5108
#
# save(list_theta,list_pi,file = '/home/donnet/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/Ecologie/Code/generalized_multi_BM/res_simu_AoAS/res_simu_AoAS_DATTILO/param1/paramSim1.Rdata')


# #---------------------- parameters : param2
# lpi = list(c(0.4675,0.1606, 0.1351,0.0784,0.1061, 0.0142,0.0381),c(0.06,0.94),c(0.1,0.9),c(1))
# lpi[[2]] = c(0.1,0.9)
# lpi[[1]] = c(0.46,0.16, 0.15,0.184,0.1061, 0.1,0.1)
# lpi[[1]] = lpi[[1]]/sum(lpi[[1]])
# ltheta   <- lapply(1:nnet,function(i){matrix(0,v_K[E[i,1]],v_K[E[i,2]])})
# ltheta[[1]][1,] = c(0.0957,0.0075)
# ltheta[[1]][2,1]  = 0.01
# ltheta[[1]][3,2] = 0.0003
# ltheta[[1]][4,] = c(0.1652, 0.0343)
# ltheta[[1]][5,] = c(0.2018,0.138)
#
# ltheta[[2]][1,2] = 0.0006
# ltheta[[2]][2,] = c(0.5431, 0.0589)
# ltheta[[2]][4,] = c(0.6620,0.1542)
# ltheta[[2]][7,] = c(0.8492,0.3565)
#
# ltheta[[3]][1,] = c(0.0013)
# ltheta[[3]][3,] = 0.0753
# ltheta[[3]][5,] = 0.0163
# ltheta[[3]][6,] = 0.5108
#
# list_pi <- lpi
# list_theta <- ltheta
# save(ltheta,lpi,file = '/home/donnet/WORK_ALL/RECHERCHE/TRAVAUX_RECHERCHE/Avner-Pierre/Ecologie/Code/generalized_multi_BM/res_simu_AoAS/res_simu_AoAS_DATTILO/param2/paramSim2.Rdata')
load (file = 'res_simu_AoAS/res_simu_AoAS_DATTILO/param2/paramSim2.Rdata')
ltheta_true <- ltheta
lpi_true <- lpi
##########  ------------------SIMULATION -----------------------------
nSimu = 100 ;
# #
dirSaveSimuData <- 'res_simu_AoAS/res_simu_AoAS_DATTILO/param2/data'
# #
# for (i in 1:nSimu){
#     datasim <- rMBM(v_NQ = v_NQ ,E  = E , typeInter =  typeInter, v_distrib, list_pi = list_pi, list_theta = list_theta, seed = NULL, namesFG = namesFG,keepClassif = TRUE)
#     namedata  <- paste('datasim',i,'.Rdata',sep = "")
#     save(datasim,file = paste(dirSaveSimuData,namedata,sep = '/' ))
# }


#
# # #---------------- ESTIMATION ------------------------------------
dirSaveSimuRes <- 'res_simu_AoAS/res_simu_AoAS_DATTILO/param2/res_MBM'
# for (i in 1:nSimu)
#  {
#  print(i)
#  #load data
#  namedata  <- paste('datasim',i,'.Rdata',sep = "")
#  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
#  list_Net <- datasim$list_Net
#  # estim
#  res_estim <- multipartiteBM(list_Net, namesFG = namesFG, v_distrib = v_distrib , v_Kmin = 1 , v_Kmax = 10 , v_Kinit = c(1,1,1,1), initBM = TRUE, save = FALSE , verbose = FALSE,nbCores = 10)
#  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
#  save(res_estim, file = paste(dirSaveSimuRes,nameres,sep = '/' ))
# }


##########################################################
#------------------- EXPLOITATION -------------------
###########################################################

#----------------------------------------
# #----------- VK estimés ---------------
#----------------------------------------

truev_K <- v_K_estim <- resCompar <- matrix(0,nSimu,4)
for (i in 1:nSimu)
{
   print(i)
   #load data
   namedata  <- paste('datasim',i,'.Rdata',sep = "")
   load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
   trueZ <- datasim$classif
   truev_K[i,] <- vapply(1:4,function(q){length(unique(trueZ[[q]]))},1)
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
  test <- matrix(unique.vK.estim[i,],ncol = 4,nrow = nrow(v_K_estim),byrow = T)
  tab[i] <- sum(rowSums(test == v_K_estim) == 4)
}
library(xtable)
xtable(cbind(unique.vK.estim,t(t(tab))))

#----------------------------------------- 
#  q = 3  :  bad  estimation what happens
#-----------------------------------------
w3 <- which(abs(v_K_estim[,3] - truev_K[,3]) !=  0 )
res_3 = matrix(0,length(w3),2)
for (j in 1:length(w3)) {
  i <- w3[j]
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
  trueZ <- datasim$classif
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  res_3[j,] <- table(estimZ[[3]],trueZ[[3]])
}
boxplot(resCompar[w3,3])

#----------------------------------------- 
#  q = 1  :  bad  estimation what happens
#-----------------------------------------
w1 <- which(abs(v_K_estim[,1] - truev_K[,1]) != 0 )
res_1 = array(0,c(6,7,length(w1)))
for (j in 1:length(w1)) {
  i <- w[j]
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
  trueZ <- datasim$classif
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  res_1[,,j] <- table(estimZ[[1]],trueZ[[1]])
}
#----------------------------------------- 
#  q = 1  :  bad  estimation what happens
#-----------------------------------------
w2 <- which(abs(v_K_estim[,2] - truev_K[,2]) !=  0 )
res_2 = list(0,length(w2))
for (j in 1:length(w2)){
  i <- w2[j]
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  nameres  <- paste('resMBM_',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuRes,nameres,sep = '/' ))
  trueZ <- datasim$classif
  estimZ <- res_estim$fittedModel[[1]]$paramEstim$Z
  res_2[[j]] <- table(estimZ[[2]],trueZ[[2]])
}

#--------------------------------------
## table of simulation classif par FG 2
#-------------------------------------- 
table_2 <- matrix(0,nSimu,2)
for (i in 1:nSimu) {
  namedata  <- paste('datasim',i,'.Rdata',sep = "")
  load(file = paste(dirSaveSimuData,namedata,sep = '/' ))
  table_2[i,] = table(datasim$classif[[2]])
}

ARI_FG1 <- data.frame(resCompar[,1])
ARI_FG1$K <- as.factor(v_K_estim[,1])
names(ARI_FG1) = c('ARI','Estimated_K_1')



g <- ggplot(ARI_FG1, aes(x=Estimated_K_1, y=ARI)) + geom_boxplot() + geom_jitter(shape = 16, size=3,position=position_jitter(0.2))
g <- g + scale_x_discrete(expression(hat(K)[1]))
g <- g + ylab(expression(paste('ARI for Z' ^'1'),sep=' ')) +  theme(text = element_text(size = 20)) 
g

save_plot  = "/home/donnet/Dropbox/Multiplex/Ecologie/article/StatisticalModeling"
ggsave(paste(save_plot,"ARI_simu_DATTILO.png",sep='/'),width = 20, height = 20, units = "cm")


#-------------------------------------------------------------------------------
########################### RELABELLING for parameter estimation quality
#-------------------------------------------------------------------------------
w <- which(vapply(1:100,function(i){sum(v_K_estim[i,] == truev_K[i,]) - nFG},1)==0)

all_param_estim <- array(0,c(length(w),truev_K[1,1]+1, sum(truev_K[1,-1])+1))
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
  param_estim_relab <- cbind(vec_pi_estim[1:truev_K[1,1]],param_estim_relab)
  param_estim_relab <- rbind(c(NA,vec_pi_estim[-(1:truev_K[1,1])]),param_estim_relab)
  all_param_estim[j,,] <- param_estim_relab
}

#----------------------------------- 

mat_true_param <- do.call(cbind, ltheta_true)
vec_pi <-  unlist(lpi)
mat_true_param <- cbind(vec_pi[1:truev_K[1,1]],mat_true_param)
mat_true_param <- rbind(c(NA,vec_pi[-(1:truev_K[1,1])]),mat_true_param)



#--------------- TABLE BIais and RMSE -------------------------------- 
MSE <- vapply(1:length(w),function(i){(all_param_estim[i,,]- mat_true_param)^2},mat_true_param)
MSE <- apply(MSE,c(1,2),mean)
RMSE <- sqrt(MSE)
dim(RMSE)
round(RMSE,4)

Biais <- vapply(1:length(w),function(i){(all_param_estim[i,,]- mat_true_param)},mat_true_param)
Biais <- apply(Biais,c(1,2),mean)

Biais_alpha <- Biais[-1,-1]
RMSE_alpha <- RMSE[-1 ,-1]
round(Biais,4)

TABLE_alpha <- matrix(0,7,5)
for (l in 1:7){
  for (k in 1:5){
  TABLE_alpha[l,k] = paste(round(Biais_alpha[l,k],4),round(RMSE_alpha[l,k],4),sep = '/' )
  }
}

xtable(TABLE_alpha,digits = 4)

Biais_pi <- round(matrix(c(Biais[-1,1], Biais[1,-1],NA,NA),2,7,byrow=TRUE),4)
RMSE_pi <-  round(matrix(c(RMSE[-1,1], RMSE[1,-1],NA,NA),2,7,byrow=TRUE),4)

TABLE_pi <- matrix(0,2,7)
for (l in 1:2){
  for (k in 1:7){
    TABLE_pi[l,k] = paste(round(Biais_pi[l,k],4),round(RMSE_pi[l,k],4),sep = '/' )
  }
}
xtable(t(TABLE_pi),digits = 4)
#dimnames(all_param_estim) = list(SimuIndex = 1:73,ClusterRow = c('pi',vapply(1:7,function(k){paste('1',k,sep=',')},'1')), ClusterCol=c('pi','2,1','2,2','3,1','3,2','4,1'))
dimnames(all_param_estim) = list(SimuIndex = 1:73,ClusterRow = c('pi',vapply(1:7,function(k){paste('FG1.B',k,sep='')},'1')), ClusterCol=c('pi','FG2.B1','FG2.B2','FG3.B1','FG3.B2','FG4.B1'))



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
dimnames(mat_true_param) = list(ClusterRow = c('pi',vapply(1:7,function(k){paste('FG1.B',k,sep='')},'1')), ClusterCol=c('pi','FG2.B1','FG2.B2','FG3.B1','FG3.B2','FG4.B1'))

table_param_true <- as.data.frame.table(mat_true_param)
table_param_true$x <- rep(0.2,nrow(table_param_true))
colnames(table_param_true) <- c('Krow','Kcol','True','x')
#table_param_true <-  table_param_true %>%separate( 'Kcol', c('FGcol','Blockcol'), sep = ",", remove =TRUE,
 #                                                    convert = FALSE, extra = "warn", fill = "left")
#table_param_true <- table_param_true %>%separate( 'Krow', c('FGrow','Blockrow'), sep = ",", remove =TRUE,
#                                                    convert = FALSE, extra = "warn", fill = "left")
#table_param_true <- table_param_true %>% mutate(FGrow = replace_na(FGrow, 0))
#table_param_true <-  table_param_true  %>% mutate(FGcol = replace_na(FGcol, 0))


bp  <- ggplot(table_param_estim, aes(x = rep(0,length(table_param_estim$Estim)),y = Estim)) + geom_boxplot()
bp <- bp + theme(axis.title.x = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title.y = element_blank(),
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank())
bp <- bp + geom_point(data = table_param_true,aes(y =True,x=x),shape=17,  color="blue", size=2)
#bp <- bp + facet_grid(FGrow + Blockrow ~ FGcol + Blockcol,shrink = TRUE,scales="free_y",switch = "y")
bp <- bp + facet_grid(Krow ~ Kcol,shrink = TRUE,scales="free_y",switch = "y")
bp <- bp +  scale_y_continuous(breaks = c(0.2,0.5,1),position='right')
bp <- bp + theme(text = element_text(size = 20))
bp
ggsave("boxplot_simu_Dattilo.png",width = 20, height = 20, units = "cm")


