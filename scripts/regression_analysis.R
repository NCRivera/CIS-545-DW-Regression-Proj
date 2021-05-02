# 0.0 LIBRARIES ----

# install.packages("dotwhisker")
# install.packages('TMB', type = 'source') # Required by dotwhisker package if error ocurred.
# install.packages("remotes")
# remotes::install_github("r-link/corrmorant")

library(tidyverse)
library(tidymodels)
library(dotwhisker)
library(corrmorant)
library(olsrr)

# 1.0 DATA IMPORTATION ----
RedWineQuality   <- read.csv(file = "./data/winequality-red.csv", sep = ";") %>% tibble() 

RedWineQuality
RedWineQuality %>% glimpse()


## 1.1 CLEANING COLUMN NAMES ----
library(janitor)
RedWineQuality <- RedWineQuality %>% clean_names(case = "upper_camel")


## WHITE WINE QUALITY DATA ----
WhiteWineQuality <- read.csv(file = "./data/winequality-white.csv", sep = ";") %>% tibble() 
WhiteWineQuality <- WhiteWineQuality %>% clean_names(case = "upper_camel")


# 2.0 CORRELATION PLOT ----
corrmorant(RedWineQuality, style = "blue_red")


# 3.0 MODEL BUILDING ----
LinearModel_Initial <- lm(formula = "Quality ~ .", data = RedWineQuality)
summary(LinearModel_Initial)


# 3.1 STEPWISE REGRESSION, P VALUE ----
StepModelForward  <- ols_step_forward_p(LinearModel_Initial)
StepModelBackward <- ols_step_backward_p(LinearModel_Initial)
StepModelBoth     <- ols_step_both_p(LinearModel_Initial)


## 3.1.1 REGRESSION DIAGNOSTICS ----
plot(StepModelForward)
plot(StepModelBackward)
plot(StepModelBoth)


# 3.2 Best Subset Selection ----
BestSubsetModel <- ols_step_best_subset(model = LinearModel_Initial, metric = "rsquare")
plot(BestSubsetModel)
