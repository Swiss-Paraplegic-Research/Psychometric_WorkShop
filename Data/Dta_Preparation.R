rm(list = ls())


path_sample = "C:/Users/C. Fellinghauer/OneDrive/Data Fittery/WB/Uzbekistan/NASP/Workshop/Data/"
  
  library(missForest)
  library(readxl)
  library(dplyr)
  library(foreign)
  library(missRanger)
  library(splitstackshape)
  library(data.table)
  library(stringr)
  
  WHODAS = c(paste("D1", 1:6, sep = "."),  #Understanding and communicating
             paste("D2", 1:5, sep = "."),  #Getting around
             paste("D3", 1:4, sep = "."),  #Self-care
             paste("D4", 1:5, sep = "."),  #Getting along with people
             paste("D5", 1:4, sep = "."),  #Life activities - without the work items 50% items (now it is out!)
             paste("D6", 1:8, sep = "."))  #Participation in society
  
  
  path = ".../WB/Uzbekistan/NASP/"
  
  path_syntax = paste0(path, "Syntax/Rasch/", sep = "")
  path_data = paste0(path, "Data/")
  
  #################*call the data------------------
  
  
  
  ##taking the imputed data
   data.WHODAS = read.csv(paste0(path_data, "Data_Uzbekistan_imputed.csv"), sep = ",")  
   dim(data.WHODAS)
   colnames(data.WHODAS)
   
   inv_inc = 1/prop.table(table(rowSums(data.WHODAS[, WHODAS])))
   data.WHODAS[, "Score"] = rowSums(data.WHODAS[, WHODAS])
   
   iip = function(x){
     y = inv_inc[names(inv_inc)%in%x]
     return(y)
   }
   
   data.WHODAS[, "IIP"] = NA
   library(purrr)
   for(i in 1:nrow(data.WHODAS)){
   data.WHODAS[i, "IIP"] = iip(data.WHODAS[i, "Score"])
   }
  
   
   range_0_20 =which(data.WHODAS[, "Score"]<=20 & data.WHODAS[, "Score"])
   Rows_0_20 = sample(range_0_20, 30*3, replace = FALSE, prob = data.WHODAS$IIP[range_0_20])
   
   range_80_plus =which(data.WHODAS[, "Score"]>=80 & data.WHODAS[, "Score"])
   Rows_80_plus = sample(range_80_plus, 110*3, replace = FALSE, prob = data.WHODAS$IIP[range_80_plus])
   
   range_20_80 = which(data.WHODAS[, "Score"]>=20 & data.WHODAS[, "Score"]<=80)
   Rows_20_80 = sample(range_20_80, 200*3, replace = FALSE, prob = data.WHODAS$IIP[range_20_80])
  
   range_40_60 = which(data.WHODAS[, "Score"]>=40 & data.WHODAS[, "Score"]<=60)
   Rows_40_60 = sample(range_40_60, 100*3, replace = FALSE, prob = data.WHODAS$IIP[range_40_60])
   
   range_60_80 = which(data.WHODAS[, "Score"]>=60 & data.WHODAS[, "Score"]<=80)
   Rows_60_80 = sample(range_60_80, 50*3, replace = FALSE, prob = data.WHODAS$IIP[range_60_80])
   
   Sample_WHODAS = data.WHODAS[c(Rows_0_20, Rows_80_plus, Rows_20_80, Rows_40_60, Rows_60_80), ]
   hist(Sample_WHODAS[, "Score"], main = "sort of normal")
   
   write.csv(Sample_WHODAS, paste0(path_sample, "Dta_Workshop.csv"))
   
   