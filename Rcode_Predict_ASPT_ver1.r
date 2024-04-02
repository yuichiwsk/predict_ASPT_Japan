# R code for model development of predicting the ASPT index

# Required packages 
library(openxlsx); library(dplyr); library(psych); library(MuMIn)
library(car); library(missForest); library(ggplot2)

#############################################################
##### Model development
#############################################################
## Data 
d10 <- read.xlsx("Data_Predict_ASPT_index_ver2.xlsx", sheet = "Data_modeldev1")

## Select necessary data for model development
d11 <- d10 %>%
	select(ASPT, logArea, Elevation, Urban, Paddy, Dry, 
	Urban3, Paddy3, Dry3, pHmin, logBOD, logSS)

## Scattor plots and correlation coefficients among variables 
pairs.panels(d11, scale = F, ellipses = F, smooth = F, cex=1, pch=21)

## Model selection
options(na.action = "na.fail")
full.model <- lm(ASPT ~ ., data = d11)
mod.sel <- dredge(full.model, rank="AICc") # model selection

## Best model
best.m <- get.models(mod.sel, subset = 1)[[1]]
summary(best.m) # check the best model
# Check VIF (Variance Inflation Factor) just for reference
car::vif(best.m) # All 3
pred_ASPT <- predict(best.m)

## R2
cor(d10$ASPT, pred_ASPT)^2 # 0.6930758
## RMSE
sqrt(mean(best.m$residuals^2)) # 0.4685093

## Comparing Model-predicted vs observed ASPT values
par(mfrow=c(1, 1), mar=c(3, 3, 0.5, 0.5), mgp=c(1.8, 0.5, 0), cex = 1.8)
plot(d10$ASPT ~ pred_ASPT, pch = 21, cex = 1.1, bg = grey(0.1, 0.5), 
col = 0, xlim = c(4, 9), ylim=c(4, 9), xlab = "Model-predicted", ylab = "Observed")
abline(a=0, b=1, lwd=2, col = grey(0.1, 0.5))
abline(a=-1, b=1, lty= 2, lwd = 2)
abline(a=1, b=1, lty= 2, lwd = 2)





#############################################################
##### Model validation
#############################################################
# ## Data 
d20 <- read.xlsx("Data_Predict_ASPT_index_ver1.xlsx", sheet = "Data_validation1")

## Select necessary data for obtain predicted ASPT values
d21 <- d20 %>%
	select(logArea, Elevation, Urban, Paddy, Dry, 
	Urban3, Paddy3, Dry3, pHmin, logBOD, logSS)

## Predict the ASPT index
pred_ASPT2 <- predict(best.m, newdata= d21)
# pred_ASPT2 <- predict(best.m, newdata= d21, interval = "prediction", level = 0.95) # with prediction intervals


## Comparing Model-predicted vs observed ASPT values 
d22 <- data.frame(d20, pred_ASPT2)
g1 <- ggplot(d22, aes(x = pred_ASPT2, y = obs_ASPT, 
					colour = Area_BM, shape = Source_BM)) +
	scale_y_continuous(limits=c(3,9)) + scale_x_continuous(limits=c(3,9)) + 
	theme_bw(base_size = 35) +
	geom_point(size = 7, alpha = 0.8) +
	geom_abline(intercept = 0, slope = 1, color=grey(0.5, 0.9), linewidth = 1) + 
	geom_abline(intercept = 1, slope = 1, color=grey(0.5, 0.9), linetype="dashed", linewidth  = 1) + 
	geom_abline(intercept = -1, slope = 1, color=grey(0.5, 0.9), linetype="dashed", linewidth = 1) +
	xlab("") + ylab("") +  theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g1
# Save the figure as png file
ggsave(file = "g1.png", plot = g1, dpi = 300, width = 13, height = 10)

# R2  
cor(d22$pred_ASPT2, d22$obs_ASPT)^2 # 0.5471546
## RMSE
sqrt(mean((d22$pred_ASPT2 - d22$obs_ASPT)^2)) # 0.7489353



#############################################################
##### Predicting the ASPT index for 2925 water quality monitoring sites
#############################################################
## Data 
d30 <- read.xlsx("Data_Predict_ASPT_index_ver1.xlsx", sheet = "Data_allWQMsites1")

## Select necessary data
d31 <- d30 %>%
	select(logArea, Elevation, Urban, Paddy, Dry, 
	Urban3, Paddy3, Dry3, pHmin, logBOD, logSS)

## Some NA values need to be imputed 
## using the random forest algorithm with the "missForest" package.
# Settings
set.seed(111)
# doParalle::registerDoParallel(cores = 6)

## Imputation (not run because taking some time)
# missForest1  <- missForest(d31, ntree = 5000, parallelize = "no", verbose = TRUE) # verbose = TRUE,
# Note the calculation with ntree = 5000 will takes some time (> 1h)
# d31_imputed <- missForest1$ximp # imputed data
# head(d31_imputed)


## Read imputed data
d40 <- read.xlsx("Data_Predict_ASPT_index_ver1.xlsx", sheet = "Data_allWQMsites_imputed1")
d41 <- d40 %>%
	select(logArea, Elevation, Urban, Paddy, Dry, 
	Urban3, Paddy3, Dry3, pHmin, logBOD, logSS)
d41$Elevation <- log10(d41$Elevation) # log10-transform

## Predict the ASPT index
pred_ASPT3 <- predict(best.m, newdata= d41)
# pred_ASPT3 <- predict(best.m, newdata= d41, interval = "prediction", level = 0.95) # with prediction intervals


## Calculate propotions
# Very Good -- ASPT >= 7.5 
sum(pred_ASPT3>=7.5)/length(pred_ASPT3) # 29%
# Good -- 6 =< ASPT < 7.5
(sum(pred_ASPT3>=6)-sum(pred_ASPT3>=7.5))/length(pred_ASPT3) # 50%
# Fair -- 5 =< ASPT < 6
(sum(pred_ASPT3>=5)-sum(pred_ASPT3>=6))/length(pred_ASPT3) # 14%
# Not Good -- ASPT < 5
sum(pred_ASPT3<5)/length(pred_ASPT3) # 8%

ASPT_evaluation <- ifelse(pred_ASPT3 >= 7.5, 4,
                          ifelse(pred_ASPT3 >= 6, 3,
                                 ifelse(pred_ASPT3 >= 5, 2, 1)))



