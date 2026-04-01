#next command deletes everything from working memory and start new
#to avoid errors because of previous active objects 
rm(list = ls())

path = "C:/Users/fellinghauer_c/OneDrive - Schweizer Paraplegiker-Gruppe/Dokumente/Documents/Workshop/Psychometric_WorkShop/"

#*********************************
#RM----------
#The most common Rasch model is the model for dichotomous items.

#load eRm
#install.packages("eRm")
library(eRm)


# simulate 10 dichotomous items for 500 persons with sim.rasch
# set the seed to 1234
?sim.rasch 

RM_Data <- eRm::sim.rasch(500, 10, seed = 1234)

#run a Rasch model on Data_Dicho
RM_analysis <- RM(RM_Data)
print(summary(RM_analysis))


#plot a Person-Item Map and sort the responses by increasing difficulty
#also sometimes called Wright map

plotPImap(RM_analysis)

############################################################################3
#Polytomous Data----------------
#load the package psych
#install.packages("psych")
library("psych")

#here a function to simulate polytomous data

set.seed(9876)  # reproducibility
Data_Poly <- sim.poly.npl(nvar = 10, n = 500, low = -4, high = 4, a = sample(seq(0.4, 1.4, 0.1), 10, replace = FALSE), c = 0, 
                          z = 1, d = NULL, mu = 0, sd = 1.96, cat = 3)$items 

#*************************************** 
#RSM--------------

#run a Rating Scale Model with Data_Poly

RSM_Poly = RSM(Data_Poly)
summary(RSM_Poly)

#*********************************
#Partial Credit Model

PCM_Poly = PCM(Data_Poly)
summary(PCM_Poly)


#************************************
#Difference Rating Scale Model and Partial Credit Model
#Loading a Function ThresholdMap from Github

source(paste0(path, "Exercises/ThresholdMap.R"))


##Run the ThresholdMap() function for the thresholds of RSM_Poly and PCM_Poly 
#(check function thresholds()). 

#use 
par(mar = c(2,2,2,1)) #to vary the size of margins
#check ?par to see how the function works

ThresholdMap(thresholds(RSM_Poly))
ThresholdMap(thresholds(PCM_Poly))

graphics.off() #to reset the plotting window


##RSM vs. PCM-------------
anova(RSM_Poly, PCM_Poly)


#****************************************
#The data used in this file is a sample of data with WHODAS ratings, that were
#selected to illustrate Rasch methods. It is not representative. 


#loading the data
# dta is an imputed dataset with randomly selected ratings on WHODAS items

dta = read.csv(paste0(path, "Data/Dta_Workshop.csv"))


#*******************************************************************
#WHODAS Data------------

##Link Manual---------
#https://iris.who.int/server/api/core/bitstreams/13c8676c-bdd5-41e9-beae-30343a96d4f0/content

#WHODAS items without the Work items D5.5-D5.8 and the additional UZ
Items = c(paste("D1", 1:6, sep = "."),  #Understanding and communicating
          paste("D2", 1:5, sep = "."),  #Getting around
          paste("D3", 1:4, sep = "."),  #Self-care
          paste("D4", 1:5, sep = "."),  #Getting along with people
          paste("D5", 1:4, sep = "."),  #Life activities - without the work items 50% items (now it is out!)
          paste("D6", 1:8, sep = "."))  #Participation in society



#load following R-packages
#install.packages("mirt", "iarm")
library(mirt)
library(iarm)


# check response coding (frequencies including missing values) for each WHODAS item
apply(dta[, Items], 2, table, useNA = "always")

# before starting check if we have the entire score range
# if not create dummies
# minimum raw total score is 0
# maximum raw total score is 32 x 4 = 128

range(dta[, "Score"])

colnames(dta)

dta = as.data.frame(rbind(dta, 
      c("", "", "Dummy", rep(0, 32), rep(NA, 8), 0, NA)
      ))

dta[, Items] = apply(dta[, Items],2, as.numeric)

range(rowSums(dta[, Items]))
#*******************************************************************
#Item Fit--------------------

####with Package eRm----------

PCM_eRm = PCM(dta[, Items], sum0 = TRUE)

###Calculating item fit with package eRm requires the function itemfit(), 
##which uses the output of the person.parameter() function.

PP_eRm = person.parameter(PCM_eRm)

?itemfit

IFit_eRm = eRm::itemfit(PP_eRm)

#We started with the PCM analysis using the package eRm
#the PCM from package eRm would be the best approach but it has its limitations.
#It is slower, which makes a difference with large sample sizes.
#It does not allow certain manipulations, such as using parameter from earlier analyses
#on new data - so called item anchoring.

#PCM can alsow be calculated with package mirt.
#mirt is more flexible and faster - but instead of the raw scores for estimation it
#uses mean and sd of raw score distribution. 
#meaning analyses when chosing one or the other package will lead to different findings.



###Exercise 1------------
####with Package mirt---------

#run a PCM and get item fit with mirt - you can use whatever available.

PCM_mirt = mirt(dta[, Items], 1, itemtype = "Rasch")
IFit_mirt = mirt::itemfit(PCM_mirt, "infit")


cbind(infit_eRm = IFit_eRm$i.infitMSQ,
infit_mirt = IFit_mirt$infit)


#*******************************************************************
#Thresholds-------------
####with Package eRm----------------------------

Thr_eRm = thresholds(PCM_eRm)
Thr_eRm

#Threshold Map (see L.65, L.79)
par(mar = c(2,2,2,1))
ThresholdMap(Thr_eRm)


#Person Item Map
plotPImap(PCM_eRm)


#ICC curves
# plotICC(PCM_eRm, item.subset = "all")
# #wait...
# #reset graphical display with
# graphics.off()

###Exercise 2------------
####with Package mirt----------------

Thr_mirt = coef(PCM_mirt, simplify = TRUE, 
                IRTpars=TRUE)$items


#ThresholdMap 
par(mar = c(2,2,2,1))
ThresholdMap(Thr_mirt, RPackage = "mirt")

  
  
#Person-Item Map is not directly available in mirt
#Suggestions - 
#a) Recycling the person item map syntax to run with thresholds and person parameter from mirt
#write function without the brackets to see the code
plotPImap
#b) Use the wrightMap function from the R package WrightMap

library(WrightMap)


PP_mirt = fscores(PCM_mirt, method = "WLE", 
                  full.scores = TRUE, 
                  full.scores.SE = TRUE)

wrightMap(PP_mirt[,1], Thr_mirt[,-1])


#ICC curves 

# for(i in 1:length(Items)){
#   print(empirical_plot(dta[, Items], i, 
#           smooth = TRUE, 
#           main = Items[i]))
# }




#*******************************************************************
#Targeting----------------------

#For the targeting we can look at the mean thresholds and mean person parameter

####with Package eRm-----------------


#Item Difficulties
Mean_IP_eRm = mean(Thr_eRm$threshtable$`1`[,1]) #mean location = mean of thresholds
SD_IP_eRm = sd(as.vector(Thr_eRm$threshtable$`1`[,-1]), na.rm = TRUE) #sd location != sd of thresholds - make sd of thresholds
Target_IP_eRm = cbind(Mean_IP_eRm, SD_IP_eRm)

#Person Abilities 

Theta_eRm = PP_eRm$theta.table

Mean_PP_eRm = mean(Theta_eRm[,1], na.rm = TRUE)
SD_PP_eRm = sd(Theta_eRm[,1], na.rm = TRUE)
Target_PP_eRm = cbind(Mean_PP_eRm, SD_PP_eRm)


Targeting_eRm = cbind(rbind(Target_IP_eRm, Target_PP_eRm))
rownames(Targeting_eRm) = c("Difficulty", "Ability")
colnames(Targeting_eRm) = c("Mean", "SD")

round(Targeting_eRm, 3)

#Also check the PImap

plotPImap(PCM_eRm)


#Person Separation Index
?SepRel
PSI_eRm = SepRel(PP_eRm)
PSI_eRm


###Exercise 3------------
####with Package mirt-----------

#Item Difficulties
Mean_IP_mirt =  mean(Thr_mirt[,-1], na.rm = TRUE)
SD_IP_mirt = sd(as.vector(Thr_mirt[,-1]), na.rm = TRUE)
Target_IP_mirt = cbind(Mean_IP_mirt, SD_IP_mirt)


#Compute person parameter estimates

#Person Abilities  
Mean_PP_mirt = mean(PP_mirt[,1], na.rm = TRUE)
SD_PP_mirt = sd(PP_mirt[,1], na.rm = TRUE)
Target_PP_mirt = cbind(Mean_PP_mirt, SD_PP_mirt)


Targeting_mirt = cbind(rbind(Target_IP_mirt, Target_PP_mirt))
rownames(Targeting_mirt) = c("Difficulty", "Ability")
colnames(Targeting_mirt) = c("Mean", "SD")

round(Targeting_mirt,3)

#PSI 
#!delete
PSI_mirt =  empirical_rxx(PP_mirt)
PSI_mirt

#*******************************************************************
#Local Item Dependencies-----------------
####with Package eRm---------------

#get the residual matrix from ppar (person parameter estimation)
Res_eRm = residuals(PP_eRm)
LID_eRm = cor(Res_eRm, method = "spearman")

#search the LID table
cor_eRm = LID_eRm  #to avoid overwriting the original matrix

# setting values of the lower triangle and the diagonal to missing with lower.tri
cor_eRm[lower.tri(cor_eRm, diag = TRUE)] = NA

# gives the pairwise correlations
which(cor_eRm > 0.2, arr.ind = TRUE)

#load corrplot
library(corrplot)

corrplot(LID_eRm) #displays graphically the correlation matrix

#LID Plot - in form of graphical model
source(paste0(path, "Exercises/LID_Graph.r") )
par(mar=c(0,0,0,0))
LID_Graph(LID_eRm, cut = 0.2, 
          print.out = TRUE, cex = 0.7, 
          vertex.size = 20, 
          vertex.label.dist = 0, 
          vertex.color = "violet")

#type colors() for more colors
colors()

#using mean corr + 0.2 as cut-off
Mean_LID_eRm = mean(cor_eRm, na.rm= TRUE) # calculates the average residual   
Mean_LID_eRm + 0.2

LID_Graph(LID_eRm, cut = "Q3star", print.out = TRUE)


###Exercise 4----------
####with package mirt -----------

#Residual Correlations
LID_mirt = mirt::residuals(PCM_mirt, type = "Q3")

#check the table
#delete!----------------
cor_mirt = LID_mirt  #to avoid overwriting the original matrix

# setting values of the lower triangle and the diagonal to missing with lower.tri
cor_mirt[lower.tri(cor_mirt, diag = TRUE)] = NA

# gives the pairwise correlations
which(cor_mirt > 0.2, arr.ind = TRUE)


#get corrplot
corrplot(LID_mirt) #displays graphically the correlation matrix

#LID Plot - in form of graphical model
LID_Graph(LID_mirt, cut = 0.2, print.out = TRUE, 
          cex = 1.4, 
          vertex.size = 17, vertex.label.dist = 0, 
          vertex.color = "sandybrown")


#calculate cut-off Q3star
Mean_LID_mirt = mean(cor_mirt, na.rm= TRUE) # calculates the average residual   


#get LID graph with cut off at Q3star
LID_Graph(LID_mirt, cut = "Q3star", 
          print.out = TRUE, vertex.color =  "forestgreen")
Mean_LID_mirt + 0.2

#*******************************************************************
#Dimensionality-----------------
####with Package eRm---------------

PCA_eRm = eigen(LID_eRm)
PCA_eRm

Eigen_eRm = PCA_eRm$values
Eigen_eRm
Perc_Eigen_eRm = eigen(LID_eRm)$value/sum(eigen(LID_eRm)$value)*100 
Cum_Perc_Eigen_eRm = cumsum(Perc_Eigen_eRm)
Eigenvalue_Tble_eRm = cbind(Eigen_eRm, Perc_Eigen_eRm, Cum_Perc_Eigen_eRm)
round(Eigenvalue_Tble_eRm,2) 

#First eigenvalue should ideally be below 2
#screeplot
par(mar = c(2,2,2,2))
barplot(Eigen_eRm, main="Screeplot : WHODAS 2.0", las = 2)


#Plot item loadings

plot(PCA_eRm$vectors[, 1], PCA_eRm$vectors[, 2], 
     xlab = "1st component",
     ylab = "2nd component", 
     main = "WHDOAS PCA-Loading", col = "white", 
     xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.5) )

text(PCA_eRm$vectors[, 1], 
     PCA_eRm$vectors[, 2], 
     Items, cex = 1)

segments(0, -0.6, 0, 0.6, col = "red", lty = "dotted")
segments(-0.6, 0, 0.6, 0, col = "orange", lty = "dotted")


#3D Visualization - z-axis with values from PC3 - Loadings on 3rd component
#install.packages("rgl")
library(rgl)

plot3d(PCA_eRm$vectors[, 1], 
       PCA_eRm$vectors[, 2], 
       PCA_eRm$vectors[, 3], 
       xlab = "PC1",
       ylab = "PC2", 
       zlab = "PC3", 
       col = "white")


text3d(PCA_eRm$vectors[, 1], 
       PCA_eRm$vectors[, 2], 
       PCA_eRm$vectors[, 3],
       texts = Items)


###Exercise 5------------------
####with Package mirt---------------


PCA_mirt = eigen(LID_mirt)
Eigen_mirt = PCA_mirt$values
Perc_Eigen_mirt = eigen(LID_mirt)$value/sum(eigen(LID_mirt)$value)*100 
Cum_Perc_Eigen_mirt = cumsum(Perc_Eigen_mirt)
Eigenvalue_Tble_mirt = cbind(Eigen_mirt, Perc_Eigen_mirt, Cum_Perc_Eigen_mirt)
Eigenvalue_Tble_mirt 

#First eigenvalue should ideally be below 2
par(mar = c(2,2,2,2))
barplot(Eigen_mirt, main="Screeplot : WHODAS 2.0", las = 2)


#Plot item loadings

plot(PCA_mirt$vectors[, 1], PCA_mirt$vectors[, 2], xlab = "1st component",
     ylab = "2nd component", main = "WHDOAS PCA-Loading", col = "white", 
     xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.5) )

text(PCA_mirt$vectors[, 1], PCA_mirt$vectors[, 2], Items, cex = 1)

segments(0, -0.6, 0, 0.6, col = "red", lty = "dotted")
segments(-0.6, 0, 0.6, 0, col = "orange", lty = "dotted")


#3D Visualization - z-axis with values from PC3 - Loadings on 3rd component
#install.packages("rgl")
library(rgl)

plot3d(PCA_mirt$vectors[, 1], PCA_mirt$vectors[, 2], PCA_mirt$vectors[, 3], xlab = "PC1",
       ylab = "PC2", zlab = "PC3", col = "white")


text3d(PCA_mirt$vectors[, 1], PCA_mirt$vectors[, 2], PCA_mirt$vectors[, 3],
       texts = Items)

  
  
##Multidimensional Rasch -----------------------
#Dimensionality: Additional Approach I
#not available in eRm
##we test if a two-dimensional model fits better than a unidimensional model


#find the items of PC1 which load negatively
which(PCA_mirt$vectors[,1] < 0) # negative sign on PC1

#find the items of PC1 which load positively
which(PCA_mirt$vectors[,1] > 0) # positive sign on PC1


##Fixing slopes
##the multidimensional model does not exist for Rasch but for 2PL model. Fixing the
#the slope to all equal 1 mimics a Rasch parameterization.
##how to set which items are in which dimension and how to set the slopes fixed at one
##assumptions that the first (F1) and second (F2) dimensions are not independent COV = .


#spec1 is a workaround to have a Rasch model, 
#it will be close but not be totally identical to PCM_mirt 



#model specification for 1-dimensional Rasch model
spec1 <-  "F1 =  1-32
          START = (1-32, a1, 1.0)
          FIXED = (1-32, a1) "

#model specification for 2-dimensional Rasch model
spec2 <- "
              F1 =  1, 2,  3,  4,  5,  6, 16, 17, 18, 19, 27
              F2 = 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32
              START = (1, 2, 3, 4,  5,  6, 16, 17, 18, 19, 27, a1, 1.0)
              START = (7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32, a2, 1.0)
              FIXED = (1, 2,  3,  4,  5,  6, 16, 17, 18, 19, 27, a1)
              FIXED = (7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32, a2)
              FREE = (GROUP, COV11)
              FREE = (GROUP, COV22)
              COV = F1*F2 "



## run the model with the specifications above.
PCM_mirt_1dim =  mirt(dta[, Items], model = spec1, 
                      itemtype = "gpcm", verbose = FALSE)

PCM_mirt_2dim = mirt(dta[, Items], 
                     model = spec2, 
                     itemtype = "gpcm", 
                     verbose = FALSE)


## compare the two models 1dim against 2dim with anova
anova(PCM_mirt_1dim, PCM_mirt_2dim)

#***************************************
#Dimensionality: Additional Approach II
##Proportion sign. T-Tests -----------------------
#not feasible in eRm - needs parameter anchoring

PC1_mirt_neg = which(PCA_mirt$vectors[,1] < 0) # negative sign on PC1
PC1_mirt_pos = which(PCA_mirt$vectors[,1] > 0) # positive sign on PC1


## Extract the coefficients from Rasch model
#we need the common item parameter estimates for anchoring
coef_mirt_1dim = coef(PCM_mirt, simplify = TRUE)
coef_mirt_1dim


## Anchoring the items loading negatively

#First run a Rasch analysis of the negatively loading items, as usual, but with option pars = "values"
mod_mirt_neg = mirt(dta[, Items[PC1_mirt_neg] ], 1, 
                    itemtype = "Rasch", 
                    pars = "values") 

#get the coefficient matrix and replace the values with the common item paramere estimates (line 443)
#to fix the 4 thresholds.

mod_mirt_neg[which(mod_mirt_neg[ ,"name"]=="d1"), 
             "value"] = coef_mirt_1dim$items[PC1_mirt_neg,"d1"]
mod_mirt_neg[which(mod_mirt_neg[ ,"name"]=="d2"), "value"] = coef_mirt_1dim$items[PC1_mirt_neg,"d2"]
mod_mirt_neg[which(mod_mirt_neg[ ,"name"]=="d3"), "value"] = coef_mirt_1dim$items[PC1_mirt_neg,"d3"]
mod_mirt_neg[which(mod_mirt_neg[ ,"name"]=="d4"), "value"] = coef_mirt_1dim$items[PC1_mirt_neg,"d4"]

#finally set this, otherwise the anchored start values will not be kept fixed.
mod_mirt_neg$est = FALSE



## Do the same with the items loading positively

mod_mirt_pos = mirt(dta[, Items[PC1_mirt_pos] ], 1, itemtype= "Rasch", pars = "values") 

mod_mirt_pos[which(mod_mirt_pos[ ,"name"]=="d1"), "value"] = coef_mirt_1dim$items[PC1_mirt_pos,"d1"]
mod_mirt_pos[which(mod_mirt_pos[ ,"name"]=="d2"), "value"] = coef_mirt_1dim$items[PC1_mirt_pos,"d2"]
mod_mirt_pos[which(mod_mirt_pos[ ,"name"]=="d3"), "value"] = coef_mirt_1dim$items[PC1_mirt_pos,"d3"]
mod_mirt_pos[which(mod_mirt_pos[ ,"name"]=="d4"), "value"] = coef_mirt_1dim$items[PC1_mirt_pos,"d4"]

mod_mirt_pos$est = FALSE

## Run the anchored analysis for each dimension
#by setting pars = to the parameter estimates fixed previously

mod_PCM_neg_anchored = mirt(dta[, Items[PC1_mirt_neg]  ], 
                            1, itemtype= "Rasch", 
                            pars = mod_mirt_neg) #anchored analysis
mod_PCM_pos_anchored = mirt(dta[, Items[PC1_mirt_pos]  ],
                            1, itemtype= "Rasch", 
                            pars = mod_mirt_pos) #anchored analysis


## Extract the Theta estimates including the SE using fscores()
Theta_mirt_neg = fscores(mod_PCM_neg_anchored , full.scores.SE = TRUE)
Theta_mirt_pos = fscores(mod_PCM_pos_anchored, full.scores.SE = TRUE)

N = nrow(Theta_mirt_neg)

## Making a scatterplot: thetas dim 1 versus thetas dim 2

par(mar = c(4,4,2,1))
plot(Theta_mirt_neg[,1], Theta_mirt_pos[,1], 
     xlab = "Dim 1: PC1 negative loading",
     ylab = "Dim 2: PC1 positive loading",
     main = "Ability Estimates per Dimension",
     pch = 20, 
     col = "darkgrey")

#Trace a diagonal 
segments(x0 = -4, y0 = -4, x1 = 4, y1 = 4, col = "red")
##Individual pairwise t-tests (formula in slides)

Difference_theta = Theta_mirt_neg[,"F1"] - Theta_mirt_pos[,"F1"]  
SSE = sqrt(Theta_mirt_pos[,"SE_F1"]^2 + Theta_mirt_neg[,"SE_F1"]^2)
T_test_abs =  abs(Difference_theta/SSE)

## Proportion above 2.5
#for unidimensionality it should be small < 5%
sum(T_test_abs > 2)/length(T_test_abs) * 100


#*******************************************************************
#Differential Item Functioning--------------


##Anderson LR-test----------------
#test for DIF at score level
dta0 = dta[-nrow(dta), ]
dta0_PCM = PCM(dta0[, Items])

LR_eRm_Gender = LRtest(dta0_PCM, 
                  splitcr = dta0$Gender, 
                  se = TRUE)
LR_eRm_Gender

LR_eRm_AgeGrp = LRtest(dta0_PCM, 
                  splitcr = dta0$Age_grp, 
                  se = TRUE)
LR_eRm_Age_Grp


##ANOVA---------------

#Two-way anova approach - based on syntax from unpublished R-package R2R.
#uses the R-package eRm for estimation
#to see the function just type anova_DIF after having the function sourced (line 528)
#critical: more sensitive for model misfit, assumption of normality of residuals is not guarantied.
#often still used because implemented in original Rasch softwares (RUMM2030, winsteps)

#load PP
library(PP)

source(paste0(path, "/Exercises/anova_DIF.r"))

DIF_aov_eRm_Gender = anova_DIF(dta[, Items],
                           dta$Gender, 
                           model = "PCM",
                           nci = 9,  #number of score class interval
                           p.adj="BH") #correction for multiple testing
DIF_aov_eRm_Gender


DIF_aov_eRm_AgeGrp = anova_DIF(dta[, Items],
                           dta$Age_grp, 
                           model = "PCM",
                           nci = 9,  #number of score class interval
                           p.adj="BH") #correction for multiple testing
DIF_aov_eRm_AgeGrp



##partgam_DIF() and lordif() are independent of eRm or mirt

##Partial Gamma analysis----------
#Gamma is a rank-based measure of association. It compares, for each item and a grouping variable,
#the number of concordant and discordant pairs.
#detects only uniform DIF, fewer distributional assumptions
#sign in front of gamma gives the direction of the difference 
#size of gamma gives importance of discordance, ~|gamma| > 0.21 DIF (Bjorner et al. (1998))


DIF_partGam_Gender = partgam_DIF(
  dat.items = dta[, Items],
  dat.exo = dta$Gender,
  p.adj = c("BH")
)



DIF_partGam_AgeGrp = partgam_DIF(
  dat.items = dta[, Items],
  dat.exo = dta$Age_grp,
  p.adj = c("BH")
)



#visualize with iarm function ICC plot()
#uses functions from psychotools for parameter estimation

ICCplot(dta[, Items], itemnumber = 1:4, 
        method = "cut", cinumber = 9, 
        difvar = as.factor(dta$Gender), 
        diflabels = c("M", "F"), dif = "yes")

ICCplot(dta[, Items], itemnumber = 11, 
        method = "cut", cinumber = 9, 
        difvar = as.factor(dta$Age_grp), 
        diflabels = c("(14.5,39.5]", "(39.5,59.5]", 
                      "(59.5,69.5]",  "(69.5,100]"),
        dif = "yes")



##Ordinal Regression----------
#lordif flags uniform and non-uniform DIF
#more power with larger sample sizes - provides effect sizes not only p-values
#12 stands for uniform
#23 non uniform
#13 total dif
#for reporting look at:
#item flagging relies mainly on significance of chi12, chi13, and chi23.
#Effect size pseudo.McFadden > 0.02 large DIF


#load lordif
library(lordif)

lordif_Gender = lordif(dta[, Items], dta$Gender, model = "GPCM")
names(lordif_Gender)

cbind(Items, lordif_Gender$flag, lordif_Gender$stats)


lordif_Agegrp = lordif(dta[, Items], dta$Age_grp, model = "GPCM")
lordif_Agegrp

cbind(Items, lordif_Agegrp$flag, lordif_Agegrp$stats)


##Multi-group approach---------------
###with package mirt---------------


#This function performs likelihood-ratio DIF testing by comparing:
#a baseline model (item parameters constrained equal across groups)
#a model where specific parameters are freed
#This approach is based on multi-group estimation.
#multi-group analyses accounts for group effects in item parameter estimation 

DIF_MG_mirt_Gender = multipleGroup(
  data = dta[, Items],
  model = 1,
  group = as.factor(dta$Gender),
  itemtype = "Rasch")


#Common parameters tested:
#"d" → difficulty (uniform DIF)

DIF_Gender_mirt = DIF(
  DIF_MG_mirt_Gender,
  which.par =  paste0("d",1:4),
 )

print(DIF_Gender_mirt)



#same for AgeGrp
#will not run if all response options are not found across subgroups...
#suggestion to recode age variable

DIF_MG_mirt_AgeGrp = multipleGroup(
  data = dta[, Items],
  model = 1,
  group = as.factor(dta$Age_grp),
  itemtype = "Rasch")

#the 'how' to recode it could be guided by following Rasch Tree 


##RaschTree-------------------------
#R-Package Vignette
#https://cran.r-project.org/web/packages/psychotree/vignettes/raschtree.pdf

#this approach bases on recursive partitioning
#This approaches looks at item DIF but provides decision to split the sample based on 
#DIF-variable causing the most instability.
#it allows to test DIF for continuous DIF-variables
library(psychotree)

dta_tree = dta

#make sure that the srg.items are numeric and the pf.factors are coded as factors
dta_tree[, Items] = apply(dta_tree[, Items], 
                          2, as.numeric)

#data.frame data.srg.tree.final first only contains the DIF-variables 
dta_tree_final = data.frame(  #
  Age = as.ordered(dta_tree$Age), #
  Gender = as.factor(dta_tree$Gender))

#adding the item matrix $srg  to data.srg.tree.final data.frame
dta_tree_final$whodas = array(as.matrix(dta_tree[, Items]),  #
                          dim = c(nrow(dta_tree), length(Items)), #
                          dimnames = list(NULL, Items))

## computes the Rasch tree
rasch_tree = pctree(whodas ~ Gender + Age, 
                    data = dta_tree_final,
                    minsplit = 50)

# draws the Rasch-Tree plot
plot(rasch_tree, "profile")

#Transformation Table-----------
#once all problems have been addressed.

PP_eRm

output = print(PP_eRm)
TT_PP_0 = as.data.frame(output$`1`)


TT_PP_0[, "Score_0_100"] = NA 
library(scales)

TT_PP_0[, "Score_0_100"] = scales::rescale(TT_PP_0[, "Estimate"], to = c(0, 100) )
TT_PP_0

###Exercise 6-----------
####with package mirt------------

TT0 = round(cbind(rowSums(dta[, Items]), PP_mirt),4)

TT_mirt = as.data.frame(unique(TT0[, c(1,2)]))
TT_mirt[, "Score_0_100"] = NA
TT_mirt[, "Score_0_100"] = scales::rescale(TT_mirt[, "F1"],
                                           to = c(0, 100)) 

TT_mirt[order(TT_mirt[,1]),]



#**********************************************
library(cowsay)
library(crayon)
library(bannerCommenter)


txt = banner("We are now through the R-code part.",
       "But I still have a few topics to present with regard to Rasch.",
       "Stay focussed 😉, we are almost done!")


 say(cyan$bold("Questions?"), by = "whale")
 txt
