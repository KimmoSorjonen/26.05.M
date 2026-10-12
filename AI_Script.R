
############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

#################################
## Loading packages

library(lavaan)

#################################

n <- 1323 ## sample size

rm <- matrix(c( ## correlations reported by Wang & Niu (2026)
  
  1.000, 0.845, 0.323, 0.725,
  0.845, 1.000, 0.329, 0.780,
  0.323, 0.329, 1.000, 0.407,
  0.725, 0.780, 0.407, 1.000), nrow=4)

colnames(rm) <- rownames(rm) <- c("PA","SE","MU","LI") ## names of variables

#################################
## Alternative model

altmod <- "

## Loadings

CSE =~ PA+1*SE+MU+LI

## (Error) variances

PA ~~ PA
SE ~~ SE
MU ~~ MU
LI ~~ LI

CSE ~~ CSE

"

fit.alt <- lavaan(altmod, sample.cov=rm, ## fitting model to data 
            sample.nobs=n, meanstructure=F)

summary(fit.alt, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results

mod2 <- paste(altmod,"\nPA~~SE") ## addinf correlated residuals

fit2 <- lavaan(mod2, sample.cov=rm, ## fitting model to data 
        sample.nobs=n, meanstructure=F)

summary(fit2, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################


