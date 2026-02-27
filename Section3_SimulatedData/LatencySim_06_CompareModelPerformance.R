# Peak Latency Simulation Script: 6. Compare Model Performance 

# This script calculates the error and bias of the attentional orienting speed 
# estimates (single-group and between-group differences) and statistical power
# to detect the attentional orienting speed effect for each peak latency analysis
# approach. 

# ***See Section3_SimulatedData README.md available on the LME_MixedEffectsERPPeakLatency
# GitHub for pipeline details: https://github.com/basclab/LME_MixedEffectsERPPeakLatency/tree/main/Section3_SimulatedData

# Requirements: 
  # - Needs R Version 3.6.1 and packages listed below
  # - importFilename: File containing the estimated marginal means for each 
  #   attentional orienting speed group and other model predictors across all models 
  #   and simulated datasets (samples) for one combination of missingness pattern, 
  #   peak amplitude magnitude, and resting state noise profile (e.g., Missingness 
  #   Pattern #1/10 microvolts peak/preschool noise). See ModelOutput_DataDictionary.xlsx 
  #   file for further information.
  # - See instructions in data environment for specifying other variables

# Script Functions:
  # 1. Load model output file
  # 2. Characterize model problems
  # 3. Calculate error and bias of High orienting speed single-group estimates
  # 4. Calculate error and bias of Low orienting speed single-group estimates
  # 5. Calculate error and bias of High-Low orienting speed group difference
  # 6. Calculate power to detect attentional orienting speed effect

# Outputs: 
  # - Tables summarizing model problems, error, bias, and statistical power for 
  #   each peak latency analysis approach

# Copyright 2026 Serena K. Mon, Megan J. Heise, Lindsay C. Bowman
# Brain and Social Cognition Lab, University of California Davis, Davis, CA, USA.

# Permission is hereby granted, free of charge, to any person obtaining a 
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense, 
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(data.table) # V.1.13.2; fread function
library(dplyr) # V.1.0.2; %>% operator
library(stringr) # V.1.4.9; str_remove function

#-----------------------------------------------------------------------
# DATA ENVIRONMENT 

# Specify variables needed to import corresponding file
missType <- 'Miss1' # Formatted as 'Miss#'
peakType <- '10MicroV' # Formated as '#MicroV'
noiseProfile <- 'PreschoolNoise' # Formatted as '[Sample]Noise'
sampleN <- 1000
subjectN <- 48
importFolder <- 'C:/Users/basclab/Desktop/Section3_SimulatedData/03_ModelOutput/'
importFilename <- paste0(importFolder, 'ModelOutput_', missType, '_', peakType, '_', noiseProfile, '_sampleN', sampleN, '_subN', subN, '.csv')

# Specify population values of attentional orienting groups (single-group and 
# High-Low group difference)
highOSpeed <- 110
lowOSpeed <- 120
diffOSpeed <- c(highOSpeed-lowOSpeed) 

#-------------------------------------------------------------------------------
# 1. LOAD MODEL OUTPUT FILE

# Load specified model output file for specified missingness pattern, peak amplitude
# magnitude, and resting state noise profile
modelOutput <- fread(filename)

# Format modelType column 
modelOutput$modelType <- factor(modelOutput$modelType, levels = c('LME', 'ANOVA'))

# Format low trial-count percentage labels
modelOutput$caseDeletionPct <- factor(modelOutput$caseDeletionPct, levels = c('Pop','000','006','011','032','050','060','070','080'),
                                      labels = c('Population','0% Low Trial-Count','6% Low Trial-Count','11% Low Trial-Count','32% Low Trial-Count','50% Low Trial-Count','60% Low Trial-Count','70% Low Trial-Count','80% Low Trial-Count')) 

#-------------------------------------------------------------------------------
# 2. CHARACTERIZE MODEL PROBLEMS 

# Check for occurrence of LME models with specific problems and then update corresponding variables:
# - notConvergeN = -99 & singFitN = 1: Special singular fit error (Lapack routine dgesv: system is exactly singular: U[1,1] = 0)
LME_spSingFitN_ind <- which(modelOutput$notConvergeN == -99 & modelOutput$singFitN == 1)
modelOutput$notConvergeN[LME_spSingFitN_ind] = 0 # Update notConvergeN to 0 for documentation
# - notConvergeN = -99 & singFitN = -99: Other type of error
#   This did not occur for any simulated sample so no updates were needed 
LME_otherError_ind <- which(modelOutput$modelType == "LME" & modelOutput$notConvergeN == -99 & modelOutput$singFitN == -99)

# Check for occurrence of ANOVA models with error due to insufficient subjects 
# per cell (e.g., no younger subjects in order to examine the age effect) after 
# casewise deletion
ANOVA_otherError_ind <- which(modelOutput$modelType == "ANOVA" & modelOutput$notConvergeN == -99 & modelOutput$singFitN == -99)
# We update notConvergeN error code from -99 to 1 so that we can count the occurrence of this error
modelOutput$notConvergeN[modelOutput$modelType == "ANOVA"] = ifelse(modelOutput$notConvergeN[modelOutput$modelType == "ANOVA"] == -99, 1, 0)
# We update singFitN error code from -99 to 0 for documentation
modelOutput$singFitN[modelOutput$modelType == "ANOVA"] = ifelse(modelOutput$singFitN[modelOutput$modelType == "ANOVA"] == -99, 0, 0)

# Calculate the percent of excluded models by dividing the number of models with
# problems by the total number of simulated samples
modelProblemTable  <- modelOutput %>% 
  group_by(modelType, caseDeletionPct) %>%
  summarise(notConvergeN_sum = sum(notConvergeN, na.rm = T),
            notConvergeN_pct = round(100*notConvergeN_sum/sampleN, 1),
            singFitN_sum = sum(singFitN, na.rm = T),
            singFitN_pct = round(100*singFitN_sum/sampleN, 1),
            allProb_pct = notConvergeN_pct + singFitN_pct) 

#-------------------------------------------------------------------------------
# 3. CALCULATE ERROR AND BIAS OF HIGH ORIENTING SPEED SINGLE-GROUP ESTIMATES

# Calculate average and standard deviation of marginal means
highOSpeed_marginalMeans <- modelOutput %>% 
  group_by(modelType, caseDeletionPct) %>%
  summarise(highOSpeedMean = mean(highOSpeedEMM, na.rm = TRUE), 
            highOSpeedStdDev = sd(highOSpeedEMM, na.rm = TRUE))

# Calculate root mean squared error (RMSE) 
highOSpeed_MSESummary <- modelOutput %>%
  mutate(highOSpeed_SE = (highOSpeedEMM-highOSpeed)^2) %>%
  group_by(modelType, caseDeletionPct) %>%
  summarise(RMSE = sqrt(mean(highOSpeed_SE, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(RMSERounded = round(RMSE, 2))

# Calculate average relative bias = average percentage difference of the model's 
# marginal mean from the population value across simulated datasets
highOSpeed_relBias <- modelOutput %>%
  mutate(highOSpeed_bias = highOSpeedEMM-highOSpeed,
         highOSpeed_relBias = 100*(highOSpeed_bias/highOSpeed))
highOSpeed_relBiasSummary <- highOSpeed_relBias %>%
  group_by(modelMatch, caseDeletionPct) %>%
  summarise(avgRelBias = mean(highOSpeed_relBias, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avgRelBiasRounded = round(avgRelBias, 2))

#-------------------------------------------------------------------------------
# 4. CALCULATE ERROR AND BIAS OF LOW ORIENTING SPEED SINGLE-GROUP ESTIMATES

# Calculate average and standard deviation of marginal means
lowOSpeed_marginalMeans <- modelOutput %>% 
  group_by(modelType, caseDeletionPct) %>%
  summarise(lowOSpeedMean = mean(lowOSpeedEMM, na.rm = TRUE), 
            lowOSpeedStdDev = sd(lowOSpeedEMM, na.rm = TRUE))

# Calculate RMSE
lowOSpeed_MSESummary <- modelOutput %>%
  mutate(lowOSpeed_SE = (lowOSpeedEMM-lowOSpeed)^2) %>%
  group_by(modelType, caseDeletionPct) %>%
  summarise(RMSE = sqrt(mean(lowOSpeed_SE, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(RMSERounded = round(RMSE, 2))

# Calculate average relative bias 
lowOSpeed_relBias <- modelOutput %>%
  mutate(lowOSpeed_bias = lowOSpeedEMM-lowOSpeed,
         lowOSpeed_relBias = 100*(lowOSpeed_bias/lowOSpeed))
lowOSpeed_relBiasSummary <- lowOSpeed_relBias %>%
  group_by(modelMatch, caseDeletionPct) %>%
  summarise(avgRelBias = mean(lowOSpeed_relBias, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avgRelBiasRounded = round(avgRelBias, 2))

#-------------------------------------------------------------------------------
# 5. CALCULATE ERROR AND BIAS OF HIGH-LOW ORIENTING SPEED GROUP DIFFERENCE

# Calculate average and standard deviation of marginal means
diffOSpeed_marginalMeans <- modelOutput %>% 
  group_by(modelType, caseDeletionPct) %>%
  summarise(diffOSpeedMean = mean(diffOSpeedEMM, na.rm = TRUE), 
            diffOSpeedStdDev = sd(diffOSpeedEMM, na.rm = TRUE))

# Calculate RMSE
diffOSpeed_MSESummary <- modelOutput %>%
  mutate(diffOSpeed_SE = (diffOSpeedEMM-diffOSpeed)^2) %>%
  group_by(modelType, caseDeletionPct) %>%
  summarise(RMSE = sqrt(mean(diffOSpeed_SE, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(RMSERounded = round(RMSE, 2))

# Calculate average relative bias 
diffOSpeed_relBias <- modelOutput %>%
  mutate(diffOSpeed_bias = diffOSpeedEMM-diffOSpeed,
         diffOSpeed_relBias = 100*(diffOSpeed_bias/diffOSpeed))
diffOSpeed_relBiasSummary <- diffOSpeed_relBias %>%
  group_by(modelMatch, caseDeletionPct) %>%
  summarise(avgRelBias = mean(diffOSpeed_relBias, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avgRelBiasRounded = round(avgRelBias, 2))

#-------------------------------------------------------------------------------
# 6. CALCULATE POWER TO DETECT ATTENTIONAL ORIENTING SPEED EFFECT

# Power = number of datasets in which the model detected a significant attentional
# orienting speed effect divided by the total number of simulated datasets
sigEffectTable <- modelOutput %>% 
  group_by(modelType, caseDeletionPct) %>%
  summarise(sigOSpeed = round(sum(oSpeedPValue <= 0.05, na.rm = T)/sampleN, 2),
            sigAge = round(sum(agePValue <= 0.05, na.rm = T)/sampleN, 2))