# Peak Latency Analysis of Real Preschooler ERP Data 

# This script was used to analyze P1 peak latency data presented in Section 4 of
# Mon, Heise, and Bowman (submitted). The input files contain single-trial and 
# mean-averaged (i.e., condition-level) P1 peak latencies. 

# Our research question was: do children with greater matrix reasoning scores
# (as measured with the Kaufman Brief Intelligence Test, Second Edition) have 
# shorter P1 peak latencies (as an index of faster global stimulus processing)?

# We compared two peak latency analysis approaches:
  # - LME analysis of single-trial peak latency
  # - Two conventional ANCOVA approaches using two common trial thresholds: 
  #   10 trials per condition, 15 trials per condition. 
# See manuscript for more information about each analysis approach. 

# Requirements:
  # - Needs R Version 3.6.1 and packages listed below
  # - Input files containing single-trial and mean-averaged P1 peak latencies 
  #   saved in the same working directory (see Section 1 below)

# Script Functions:
  # 1. Load data files
  # 2. Format single-trial data for analysis
  # 3. Calculate ICC for LME analysis
  # 4. Fit LME model
  # 5. Fit exploratory LME model with interaction
  # 6. Format mean-averaged data for analysis
  # 7. Fit 10-Trial ANCOVA model
  # 8. Fit 15-Trial ANCOVA model
  # 9. Compare matrix reasoning estimates across models

# Outputs: 
# - Estimated matrix reasoning predictor across models

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

# Load required packages
library(dplyr) # V.1.0.2; dataframe manipulation
library(car) # V.3.01-0; contr.sum function
library(lme4) # V.1.1-25; LME models
library(lmerTest) # V.3.1-3; p-values for LME
library(sjstats) # V.0.18.1; ICC function
library(tidyverse) # V.1.3.0; group_by function
library(emmeans) # V.1.5.3; emtrends function
library(afex) # V.0.28-1; ANOVA analysis
library(rstatix) # V.0.6.0; Shapiro-Wilk function for ANOVA
library(lattice) # V.0.20-38; qqmath function to check normal residuals
library(performance) # V.0.6.1; check_convergence function

# Set working directory to folder containing input files
setwd('C:/Users/basclab/Desktop/Section4_RealData')

#-------------------------------------------------------------------------------
# 1. LOAD DATA FILES 

# Data files are in long format:
  # - For the single-trial data (dfTrial), each row corresponds to one participant, 
  #   one electrode, one emotion condition, one actor, and one trial presentation.
  # - For the mean-averaged data (dfMeanAvg_10TrialMin, dfMeanAvg_15TrialMin),   
  #   each row corresponds to one participant, one electrode cluster (left or right),
  #   and one emotion condition (data has been averaged across actors and trial 
  #   presentations).
dfTrial <- read.csv('p1_pLat_trialLevel_example.csv')
dfMeanAvg_10TrialMin <- read.csv('p1_pLat_condLevel_10TrialMin_example.csv')
dfMeanAvg_15TrialMin <- read.csv('p1_pLat_condLevel_15TrialMin_example.csv')

#-------------------------------------------------------------------------------
# 2. FORMAT SINGLE-TRIAL DATA FOR ANALYSIS

# Format SUBJECTID and ACTOR columns as factors
dfTrial$SUBJECTID <- as.factor(dfTrial$SUBJECTID)
dfTrial$ACTOR <- as.factor(dfTrial$ACTOR)

# Specify effects coding for the emotion condition column
dfTrial$emotion <- as.factor(dfTrial$emotion)
contrasts(dfTrial$emotion) = contr.Sum(levels(dfTrial$emotion))

# Specify indicator coding for the electrode column with O1 as the reference level
dfTrial$ELECTRODE <- factor(dfTrial$channel, levels = c("O1","O2","PO3","PO4","PO7","PO8"))

#-------------------------------------------------------------------------------
# 3. CALCULATE ICC FOR LME ANALYSIS

# Calculate intraclass correlation (ICC) of subject ID, actor, and electrode using
# intercept-only model
LME_ICC <- lmer(peakLatencyP1 ~ 1 + (1|SUBJECTID) + (1|ACTOR) +(1|ELECTRODE),
                data=dfTrial, REML=TRUE)
summary(LME_ICC)

# Store random effect variances and SD for ICC calculations
LME_ICCRandEffect <- data.frame(summary(LME_ICC)$varcor)

# ICC associated with subject: 0.041
round(LME_ICCRandEffect[1,4]/
        (LME_ICCRandEffect[1,4] + LME_ICCRandEffect[2,4] + LME_ICCRandEffect[3,4] + LME_ICCRandEffect[4,4]),3)

# ICC associated with actor: 0.003
round(LME_ICCRandEffect[2,4]/
        (LME_ICCRandEffect[1,4] + LME_ICCRandEffect[2,4] + LME_ICCRandEffect[3,4] + LME_ICCRandEffect[4,4]),3)

# ICC associated with electrode: 0.004
round(LME_ICCRandEffect[3,4]/
        (LME_ICCRandEffect[1,4] + LME_ICCRandEffect[2,4] + LME_ICCRandEffect[3,4] + LME_ICCRandEffect[4,4]),3)

#-------------------------------------------------------------------------------
# 4. FIT LME MODEL

# We determine the final model using an iterative model selection process:
# - We specify the maximal model based on recommendations from Brauer and 
#   Curtin (2017); this model had singular fit
LME_Maximal <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber + 
                      (1 + emotion + presentNumber|SUBJECTID) + 
                      (1 + emotion + presentNumber|ACTOR) +
                      (1 + emotion + presentNumber|ELECTRODE),
                    data=dfTrial, REML=TRUE)
summary(LME_Maximal) 

# - We remove the by-actor random slopes of emotion and trial presentation
#   number because actor had the lowest ICC; this model had singular fit
LME_SubElecSlopesOnly <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber +
                                (1 + emotion + presentNumber|SUBJECTID) + 
                                (1|ACTOR) + 
                                (1 + emotion + presentNumber|ELECTRODE),
                              data=dfTrial, REML=TRUE)
summary(LME_SubElecSlopesOnly)

# - We remove the by-electrode random slopes of emotion and trial presentation
#   number because electrode had the second lowest ICC; this model failed to 
#   converge
LME_SubSlopesOnly <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber +
                            (1 + emotion + presentNumber|SUBJECTID) + 
                            (1|ACTOR) + 
                            (1|ELECTRODE),
                          data=dfTrial, REML=TRUE)
summary(LME_SubSlopesOnly)

# - We fit the model with uncorrelated by-subject random slopes and random intercept;
#   this model failed to converge
LME_UncorrSubSlopes <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber +
                              (1|SUBJECTID) + (0 + emotion + presentNumber|SUBJECTID) +
                              (1|ACTOR) + 
                              (1|ELECTRODE),
                            data=dfTrial, REML=TRUE)
summary(LME_UncorrSubSlopes)

# - We remove the by-subject random slope of trial presentation number; this 
#   model failed to converge
LME_SimplSubSlope <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber +
                            (1 + emotion|SUBJECTID) + 
                            (1|ACTOR) + 
                            (1|ELECTRODE),
                          data=dfTrial, REML=TRUE)
summary(LME_SimplSubSlope)

# - Final model: We remove the by-subject random slope of emotion; this model
#   converged
LME_Final <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber +
                    (1|SUBJECTID) + 
                    (1|ACTOR) +
                    (1|ELECTRODE),
                  data=dfTrial, REML=TRUE)
summary(LME_Final)

# Check LME assumptions using check_model function from the performance package 
check_model(LME_Final) 

#-------------------------------------------------------------------------------
# 5. FIT EXPLORATORY LME MODEL WITH INTERACTION

# We examine an additional model that is identical to the final model with the 
# addition of a fixed interaction of emotion condition and trial presentation number
LME_FinalInteract <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber +
                            emotion:presentNumber + 
                            (1|SUBJECTID) +
                            (1|ACTOR) +
                            (1|ELECTRODE),
                          data=dfTrial, REML=TRUE)
summary(LME_FinalInteract) 

# Perform model comparison between final model and interaction model; models
# were fit without REML for this comparison
LME_FinalComp <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber + 
                        (1|SUBJECTID) + (1|ACTOR) + (1|ELECTRODE),
                      data=dfTrial, REML=FALSE)
LME_FinalInteractComp <- lmer(peakLatencyP1 ~ kbitMatricesCenter + emotion + presentNumber + 
                                emotion:presentNumber + 
                                (1|SUBJECTID) + (1|ACTOR) + (1|ELECTRODE),
                              data=dfTrial, REML=FALSE)
anova(LME_FinalComp, LME_FinalInteractComp)

#-------------------------------------------------------------------------------
# 6. FORMAT MEAN-AVERAGED DATA FOR ANALYSIS

# For 10-trial dataset, format SUBJECTID column as factor
dfMeanAvg_10TrialMin$SUBJECTID <- as.factor(dfMeanAvg_10TrialMin$SUBJECTID)
# Specify effects coding for the emotion condition column
dfMeanAvg_10TrialMin$emotion <- as.factor(dfMeanAvg_10TrialMin$emotion)
contrasts(dfMeanAvg_10TrialMin$emotion)=contr.Sum(levels(dfMeanAvg_10TrialMin$emotion))

# Repeat steps for formatting 15-trial dataset
dfMeanAvg_15TrialMin$SUBJECTID <- as.factor(dfMeanAvg_15TrialMin$SUBJECTID)
dfMeanAvg_15TrialMin$emotion <- as.factor(dfMeanAvg_15TrialMin$emotion)
contrasts(dfMeanAvg_15TrialMin$emotion)=contr.Sum(levels(dfMeanAvg_15TrialMin$emotion))

#-------------------------------------------------------------------------------
# 7. FIT 10-TRIAL ANCOVA MODEL 

ancova_10TrialMin <- aov_ez(id = "SUBJECTID", dv = "peakLatencyP1", data = dfMeanAvg_10TrialMin,
                            within = c("emotion","hemisphere"),
                            covariate = c("kbitMatricesCenter"), 
                            observed = c("hemisphere", "kbitMatricesCenter"),
                            factorize = FALSE)
summary(ancova_10TrialMin) 

# Check ANCOVA assumptions:
# - Linearity
ggscatter(dfMeanAvg_10TrialMin, x = "kbitMatricesCenter", y = "peakLatencyP1",
          facet.by  = c("emotion", "hemisphere"),
          short.panel.labs = FALSE) + stat_smooth(method = "loess", span = 0.9)

# - Homogeneity of regression slopes
dfMeanAvg_10TrialMin %>%
  unite(col = "group", emotion, hemisphere) %>%
  anova_test(peakLatencyP1 ~ group*kbitMatricesCenter)

# - Normal distribution of residuals
# Fit regression model, the covariate goes first
reg_10TrialMin <- lm(peakLatencyP1 ~ kbitMatricesCenter + emotion*hemisphere, data = dfMeanAvg_10TrialMin)
# Inspect the model diagnostic metrics
reg_10TrialMin_metrics <- augment(reg_10TrialMin)
shapiro_test(reg_10TrialMin_metrics$.resid) 

# - Homogeneity of variance
dfMeanAvg_10TrialMin %>% levene_test(peakLatencyP1 ~ emotion*hemisphere)

#-------------------------------------------------------------------------------
# 8. FIT 15-TRIAL ANCOVA MODEL 

ancova_15TrialMin <- aov_ez(id = "SUBJECTID", dv = "peakLatencyP1", data = dfMeanAvg_15TrialMin,
                            within = c("emotion","hemisphere"),
                            covariate = c("kbitMatricesCenter"), 
                            observed = c("hemisphere", "kbitMatricesCenter"),
                            factorize = FALSE)
summary(ancova_15TrialMin) 

# Check ANCOVA assumptions:
# - Linearity
ggscatter(dfMeanAvg_15TrialMin, x = "kbitMatricesCenter", y = "peakLatencyP1",
          facet.by  = c("emotion", "hemisphere"),
          short.panel.labs = FALSE) + stat_smooth(method = "loess", span = 0.9)

# - Homogeneity of regression slopes
dfMeanAvg_15TrialMin %>%
  unite(col = "group", emotion, hemisphere) %>%
  anova_test(peakLatencyP1 ~ group*kbitMatricesCenter)

# - Normal distribution of residuals
# Fit regression model, the covariate goes first
reg_15TrialMin <- lm(peakLatencyP1 ~ kbitMatricesCenter + emotion*hemisphere, data = dfMeanAvg_15TrialMin)
# Inspect the model diagnostic metrics
reg_15TrialMin_metrics <- augment(reg_15TrialMin)
shapiro_test(reg_15TrialMin_metrics$.resid) 

# - Homogeneity of variance
dfMeanAvg_15TrialMin %>% levene_test(peakLatencyP1 ~ emotion*hemisphere)

#-------------------------------------------------------------------------------
# 9. COMPARE MATRIX REASONING ESTIMATES ACROSS MODELS

# Use emtrends for all models so that confidence intervals are comparable 
# - We pick kbitMatricesCenter of 13 because this is the median
# - We pick presentNumber of 5.5 to be comparable to the simulations
mLME_Final <- emtrends(LME_Final, ~kbitMatricesCenter, var = "kbitMatricesCenter", 
                       at = list(kbitMatricesCenter = 13, presentNumber = 5.5), 
                       mode = "satterthwaite", lmerTest.limit = 240000, adjust='sidak',
                       infer = c(TRUE, TRUE))
mANCOVA_10TrialMin <- emtrends(ancova_10TrialMin, ~kbitMatricesCenter, var = "kbitMatricesCenter", 
                               at = list(kbitMatricesCenter = 13),
                               adjust='sidak', infer = c(TRUE, TRUE))
mANCOVA_15TrialMin <- emtrends(ancova_15TrialMin, ~kbitMatricesCenter, var = "kbitMatricesCenter", 
                               at = list(kbitMatrices = 13),
                               adjust='sidak', infer = c(TRUE, TRUE))