rm(list=ls())
library(BGLR) #load BLR
library(caret)
library(plyr)
library(tidyr)
library(dplyr)
library(reshape2)
#####Loading data set#############
datasets <- c("Disease_AL", "EYT_1_AL", "EYT_2_AL", "EYT_3_AL", "Groundnut_AL", 
              "Indica_AL", "Japonica_AL", "Maize_AL", "Wheat_1_AL", 
              "Wheat_2_AL", "Wheat_3_AL", "Wheat_4_AL", "Wheat_5_AL", 
              "Wheat_6_AL")
for(dataset in datasets){
  load(sprintf("data/%s.RData", dataset), verbose = TRUE)
  ls()
  Pheno=Pheno
  Geno=Geno
  head(Pheno)
  Traits_to_evaluate=colnames(Pheno)[2:((ncol(Pheno)+1)/2)]
  Traits_to_evaluate
  ToP_Selection=20
  # Variables definitions ---------------------------------------------------------
  #######Model to be implemented
  model_name <- "BGBLUP"
  Name_data_set=dataset
  Version=1
  
  ########Folds and iterations to use 
  cv_folds_number <- 5
  tuning_folds_number <-10
  iterations_number <- 10000
  burn_in <- 2500
  lambda <- 0.5
  # Data preparation -------------------------------------------------------------
  Pheno <- Pheno %>% arrange(Line)
  
  # Select the unique lines both in pheno and geno and sort them
  final_geno_lines <- intersect(Pheno$Line, rownames(Geno)) %>% sort()
  Geno <- Geno[final_geno_lines, final_geno_lines]
  dim(Geno)
  
  ###########Directory to save the results#####
  results_dir <- file.path(
     "results_Bino_Data_Augmentation",
     model_name,
     Name_data_set,
     Version)
  
  ######Metrics to avaluate prediction performance
  Metrics=function(Y,Yhat){
     u <- union(Yhat, Y)
     xtab1<- table(factor(Yhat, u), factor(Y, u))
     Conf_Matrix1=confusionMatrix(xtab1, positive="1")
     pccc=Conf_Matrix1$overall[1]
     kappa=Conf_Matrix1$overall[2]
     sensitivity=Conf_Matrix1$byClass[1]
     Specificity=Conf_Matrix1$byClass[2]
     F1=Conf_Matrix1$byClass[7]
     
     return(c(pccc, kappa,sensitivity,Specificity,F1))
  }
  ######Cost function to optimize the Threshold###
  Cost_function_T=function(data,par){
     Y=data$Y
     Phat=data$Phat
     TT=par[1]
     Yhat=ifelse(Phat>TT,1,0)
     Pred_Performance=Metrics(Y,Yhat)
     Diff=abs(c(Pred_Performance[4]-Pred_Performance[3]))
     Diff=as.numeric(Diff)
     return(Diff)
  }
  Pheno$Env=dataset
  head(Pheno)
  Envs_to_Evaluate=unique(Pheno$Env)
  All_envs_Val_Hyperparams=data.frame()
  ALL_envs_Accuracy_A_Tst=data.frame()
  ALL_envs_Accuracy_B_Tst=data.frame()
  ALL_envs_Accuracy_C_Tst=data.frame()
  ALL_envs_Accuracy_D_Tst=data.frame()
  ALL_envs_Accuracy_Tst=data.frame()
  Predictions_all_envs=data.frame()
  Summary_final=data.frame()
  Predictions_final=data.frame()

  #for (env in Envs_to_Evaluate) {
     env=Envs_to_Evaluate[1]
     SKM::echo("*** Env: %s ***", env)
     Pheno_E=Pheno[Pheno$Env==env,]
     Pheno_E=droplevels(Pheno_E)
     All_traits_Val_Hyperparams=data.frame()
     ALL_traits_Accuracy_A_Tst=data.frame()
     ALL_traits_Accuracy_B_Tst=data.frame()
     ALL_traits_Accuracy_C_Tst=data.frame()
     ALL_traits_Accuracy_D_Tst=data.frame()
     ALL_traits_Accuracy_Tst=data.frame()
     Predictions_all_traits=data.frame()
  for (trait in Traits_to_evaluate) {
   #trait=Traits_to_evaluate[1]
       SKM::echo("*** Trait: %s ***", trait)
     
     y <- Pheno_E %>% pull(trait)
     Predictions <- data.frame()
     
     Value_Top=quantile(y,probs=1-ToP_Selection/100)
     y_Bin=ifelse(y>c(Value_Top),1,0)
     table(y_Bin)
     MEAN=mean(y)
     SD=sd(y)
     Bin_name=paste("Bin",trait, sep="_")
  
     y_Bin_Factor=as.factor(y_Bin)
     folds <- SKM::cv_kfold_strata(y_Bin_Factor, k = cv_folds_number)
     
     
     Pheno_E=cbind(Pheno_E,Y_Bin=y_Bin)
     head(Pheno_E)
     colnames(Pheno_E)=c(colnames(Pheno_E)[-ncol(Pheno_E)],Bin_name[1])
     head(Pheno_E)
     # Cross validation -----------------------------------------------------------
     AllHyperparams=data.frame()
     Accuracy_A_Tst=data.frame()
     Accuracy_B_Tst=data.frame()
     Accuracy_C_Tst=data.frame()
     Accuracy_D_Tst=data.frame()
     Predictions=data.frame()
     Accuracy_A_Val=data.frame()
     Predictions=data.frame()
     Accuracy_A_Val=data.frame()
    for (i in seq_along(folds)) {
  #   i=1
        SKM::echo("\t*** Fold %s / %s ***", i, length(folds))
        fold <- folds[[i]]
    
        validation_indices <-fold$training[y_Bin_Factor[fold$training]=="1"]
        training_indices <- setdiff(fold$training, validation_indices)
        testing_indices <- fold$testing
        Lines_Fold_i=unique(Pheno_E$Line[fold$testing])
        length(Lines_Fold_i)
        
        Line <- model.matrix(~ 0 + Line, data =Pheno_E)
    
        lines <- sort(Pheno_E$Line)
        Geno2 <- Geno[lines,lines]
        dim(Geno2)
      
        yy<-y_Bin_Factor
        #Se seleccionan las testing agregadas al Pheno_Aug (son las primeras puesto que se agregan al principio)
       
        GenoLine <- Line %*% Geno2 %*% t(Line)
     
        y_na <-yy
        y_na[testing_indices]=NA
        ETA <- list(list(K = GenoLine, model = "RKHS"))
  
    
           model <- BGLR::BGLR(
              y = y_na,
              ETA = ETA,
              response_type = "ordinal",
              nIter = iterations_number,
              burnIn = burn_in,
              verbose = FALSE
           )
           
           validation_predictions <- model$probs[testing_indices,2]
           validation_observed <- yy[testing_indices]
           validation_Bin_observed <- yy[testing_indices]
     
          
           Pred_Bin_validation=ifelse( validation_predictions>0.5,1,0)
           Pred_Performance_A_Val=Metrics(validation_Bin_observed,Pred_Bin_validation)
           
           Accuracy_A_Val=rbind(Accuracy_A_Val, data.frame(Model="Ordinal",Trait=trait, Accuracy=Pred_Performance_A_Val[1], Kappa=Pred_Performance_A_Val[2],Sensitivity=Pred_Performance_A_Val[3],Specificity=Pred_Performance_A_Val[4],F1=Pred_Performance_A_Val[5]))
           Accuracy_A_Val
           
        Predictions <- rbind(
           Predictions,
           data.frame(
              Env =Pheno_E$Env[fold$testing],
              Trait=trait,
              Line = Pheno_E$Line[fold$testing],
              Fold = i,
              Observed =validation_observed,
              Predicted =Pred_Bin_validation,
              Prob_pred_tst=validation_predictions,
    
              Opt_Threshold_Clasif=0.5
      
           )
        )
        Summary_final=rbind(Summary_final, Accuracy_A_Val)
        Predictions_final=rbind(Predictions_final, Predictions)
     }
  }
  results_dir <- file.path(results_dir)
  SKM::mkdir(results_dir)
  write.csv(Summary_final,file=file.path(results_dir,"Accuracy_A_Conventional_V3.csv"))
  write.csv(Predictions_final,file=file.path(results_dir,"ALL_Predictions_Conventional_V3.csv"))
}
  
  
  
  
  
  
