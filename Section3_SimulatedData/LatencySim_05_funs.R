# Peak Latency Simulation Helper Functions
# - extractANOVAOutput
# - extractLMEOutput

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

#-------------------------------------------------------------------------------
# extractANOVAOutput: Fit an ANOVA model to the input mean-averaged peak latency 
# dataframe.
# - Format: 
  #   modelOutput <- extractANOVAOutput(dfInput_cond, caseDeletionPct, sampleID) 
# - Inputs:
  # - dfInput_cond: Long dataframe with each row corresponding to a mean-averaged
  #   peak latency value.
  # - caseDeletionPct: Percentage of low trial-count subjects (e.g., 006).
  # - sampleID: Simulated dataset (sample) ID (e.g., 0001).
# - Output: 
  # - modelOutput: Dataframe containing the estimated marginal means for each  
  #   attentional orienting speed group and other model predictors.
extractANOVAOutput = function(dfInput_cond, caseDeletionPct, sampleID) {
  tryCatch({
    # Fit ANOVA model
    fit.ANOVA <- aov_ez(id = "SUBJECTID", dv = "peakLatencyP1", data = dfInput_cond,  
                        between = c("oSpeed", "age"), observed = c("oSpeed", "age"))
    
    # Calculate marginal means for each attentional orienting speed group 
    mANOVA_oSpeed <- emmeans(fit.ANOVA, ~ oSpeed)
    # Extract attentional orienting speed marginal means
    mANOVA_oSpeedGroup <- data.frame(mANOVA_oSpeed)[,1:3]
    # Calculate High-Low group difference
    mANOVA_oSpeedGroupDiff <- data.frame(summary(pairs(mANOVA_oSpeed)))
    
    # Calculate marginal means for each age group
    mANOVA_age <- emmeans(fit.ANOVA, ~ age)
    # Extract age marginal means
    mANOVA_ageGroup <- data.frame(mANOVA_age)[,1:3]
    # Calculate Old-Young group difference
    mANOVA_ageGroupDiff <- data.frame(summary(pairs(mANOVA_age)))
    
    # Create dataframe with model predictors and other information 
    return(data.frame(lowOSpeedEMM = mANOVA_oSpeedGroup[2,2], lowOSpeedSE = mANOVA_oSpeedGroup[2,3],
                      highOSpeedEMM = mANOVA_oSpeedGroup[1,2], highOSpeedSE = mANOVA_oSpeedGroup[1,3],
                      diffOSpeedEMM = mANOVA_oSpeedGroupDiff[1,2], diffOSpeedSE = mANOVA_oSpeedGroupDiff[1,3],
                      oSpeedEffect = NA, 
                      oSpeedPValue = summary(fit.ANOVA)[[1,6]], 
                      youngEMM = mANOVA_ageGroup[2,2], youngSE = mANOVA_ageGroup[2,3], 
                      oldEMM = mANOVA_ageGroup[1,2], oldSE = mANOVA_ageGroup[1,3],
                      diffAgeEMM = mANOVA_ageGroupDiff[1,2], diffAgeSE = mANOVA_ageGroupDiff[1,3],
                      ageEffect = NA, 
                      agePValue = summary(fit.ANOVA)[[2,6]],
                      presentNumEffect = NA, # Not relevant for ANOVA
                      presentNumPValue = NA, # Not relevant for ANOVA
                      subjectIntercept = NA, # Not relevant for ANOVA
                      actorIntercept = NA, # Not relevant for ANOVA
                      trialN = NA, # Not relevant for ANOVA
                      notConvergeN = 0, singFitN = 0, modelType = "ANOVA",
                      caseDeletionPct = caseDeletionPct, sample = sampleID))
  }, error=function(e) {
    return(data.frame(lowOSpeedEMM = NA, lowOSpeedSE = NA,
                        highOSpeedEMM = NA, highOSpeedSE = NA,
                        diffOSpeedEMM = NA, diffOSpeedSE = NA,
                        oSpeedEffect = NA, 
                        oSpeedPValue = NA, 
                        youngEMM = NA, youngSE = NA, 
                        oldEMM = NA, oldSE = NA,
                        diffAgeEMM = NA, diffAgeSE = NA,
                        ageEffect = NA, 
                        agePValue = NA,
                        presentNumEffect = NA, # Not relevant for ANOVA
                        presentNumPValue = NA, # Not relevant for ANOVA
                        subjectIntercept = NA, # Not relevant for ANOVA
                        actorIntercept = NA, # Not relevant for ANOVA
                        trialN = NA, # Not relevant for ANOVA
                        notConvergeN = -99, singFitN = -99, modelType = "ANOVA",
                        caseDeletionPct = caseDeletionPct, sample = sampleID)) # -99 for both notConvergeN and singFitN is used to designate other errors
  }) 
}

#-------------------------------------------------------------------------------
# extractLMEOutput: Fit an LME model to the input single-trial peak latency dataframe 
# with the specified formula.
# - Format: 
  #   modelOutput <- extractLMEOutput(formulaInput, dfInput_trial, caseDeletionPct, sampleID) 
# - Inputs:
  # - formulaInput: LME model formula (e.g., peakLatencyP1 ~ oSpeed + age + presentNumber + (1|SUBJECTID) + (1|ACTOR)).
  # - dfInput_trial: Long dataframe with each row corresponding to a single-trial
  #   peak latency value.
  # - caseDeletionPct: Percentage of low trial-count subjects (e.g., 006).
  # - sampleID: Simulated dataset (sample) ID (e.g., 0001).
# - Output: 
  # - modelOutput: Dataframe containing the estimated marginal means for each  
  #   attentional orienting speed group and other model predictors.
extractLMEOutput = function(formulaInput, dfInput_trial, caseDeletionPct, sampleID) {
  tryCatch({
    # Fit LME model
    fit.LME <- lmer(formulaInput, data=dfInput_trial, REML = TRUE)
    
    # If the model does not have problems (i.e., non-convergence or singular fit)
    if (check_convergence(fit.LME) && !check_singularity(fit.LME)) {
      # Calculate marginal means for each attentional orienting speed group
      mLME_oSpeed <- emmeans(fit.LME, pairwise ~ oSpeed, mode = "satterthwaite",
                             lmerTest.limit = 240000, at = list(presentNumber = presentNumberRef))
      # Extract attentional orienting speed marginal means
      mLME_oSpeedGroup <- data.frame(summary(mLME_oSpeed)$emmeans)[,1:3]
      # Calculate High-Low group difference
      mLME_oSpeedGroupDiff <- data.frame(summary(mLME_oSpeed)$contrasts) 
      
      # Calculate marginal means for each age group
      mLME_age <- emmeans(fit.LME, pairwise ~ age, mode = "satterthwaite",
                          lmerTest.limit = 240000, at = list(presentNumber = presentNumberRef))
      # Extract age marginal means
      mLME_ageGroup <- data.frame(summary(mLME_age)$emmeans)[,1:3]
      # Calculate Old-Young group difference
      mLME_ageGroupDiff <- data.frame(summary(mLME_age)$contrasts)
      
      # Create dataframe with model predictors and other information 
      return(data.frame(lowOSpeedEMM = mLME_oSpeedGroup[2,2], lowOSpeedSE = mLME_oSpeedGroup[2,3],
                        highOSpeedEMM = mLME_oSpeedGroup[1,2], highOSpeedSE = mLME_oSpeedGroup[1,3],
                        diffOSpeedEMM = mLME_oSpeedGroupDiff[1,2], diffOSpeedSE = mLME_oSpeedGroupDiff[1,3],
                        oSpeedEffect = unname(fixef(fit.LME)[2]),
                        oSpeedPValue = summary(fit.LME)$coefficients[2,5],
                        youngEMM = mLME_ageGroup[2,2], youngSE = mLME_ageGroup[2,3],
                        oldEMM = mLME_ageGroup[1,2], oldSE = mLME_ageGroup[1,3],
                        diffAgeEMM = mLME_ageGroupDiff[1,2], diffAgeSE = mLME_ageGroupDiff[1,3],
                        ageEffect = unname(fixef(fit.LME)[3]),
                        agePValue = summary(fit.LME)$coefficients[3,5],
                        presentNumEffect = unname(fixef(fit.LME)[4]),
                        presentNumPValue = summary(fit.LME)$coefficients[4,5],
                        subjectIntercept = data.frame(VarCorr(fit.LME))[1,5], 
                        actorIntercept = data.frame(VarCorr(fit.LME))[2,5], 
                        trialN = nobs(fit.LME), 
                        notConvergeN = 0, singFitN = 0, modelType = "LME",
                        caseDeletionPct = caseDeletionPct, sample = sampleID))
    }
    # Check for model problems and return model output accordingly
    else if (!check_convergence(fit.LME)) {
      return(data.frame(lowOSpeedEMM = NA, lowOSpeedSE = NA,
                        highOSpeedEMM = NA, highOSpeedSE = NA,
                        diffOSpeedEMM = NA, diffOSpeedSE = NA,
                        oSpeedEffect = NA,
                        oSpeedPValue = NA,
                        youngEMM = NA, youngSE = NA,
                        oldEMM = NA, oldSE = NA,
                        diffAgeEMM = NA, diffAgeSE = NA,
                        ageEffect = NA,
                        agePValue = NA,
                        presentNumEffect = NA,
                        presentNumPValue = NA,
                        subjectIntercept = data.frame(VarCorr(fit.LME))[1,5],
                        actorIntercept = data.frame(VarCorr(fit.LME))[2,5], 
                        trialN = nobs(fit.LME),
                        notConvergeN = 1, singFitN = 0, modelType = "LME",
                        caseDeletionPct = caseDeletionPct, sample = sampleID))
    }
    else if (check_singularity(fit.LME)) {
      return(data.frame(lowOSpeedEMM = NA, lowOSpeedSE = NA,
                        highOSpeedEMM = NA, highOSpeedSE = NA,
                        diffOSpeedEMM = NA, diffOSpeedSE = NA,
                        oSpeedEffect = NA,
                        oSpeedPValue = NA,
                        youngEMM = NA, youngSE = NA,
                        oldEMM = NA, oldSE = NA,
                        diffAgeEMM = NA, diffAgeSE = NA,
                        ageEffect = NA,
                        agePValue = NA,
                        presentNumEffect = NA,
                        presentNumPValue = NA,
                        subjectIntercept = data.frame(VarCorr(fit.LME))[1,5],
                        actorIntercept = data.frame(VarCorr(fit.LME))[2,5], 
                        trialN = nobs(fit.LME),
                        notConvergeN = 0, singFitN = 1, modelType = "LME",
                        caseDeletionPct = caseDeletionPct, sample = sampleID))
    }
  }, 
  error=function(e) {
    if (e$message == "Lapack routine dgesv: system is exactly singular: U[1,1] = 0") {
      return(data.frame(lowOSpeedEMM = NA, lowOSpeedSE = NA,
                        highOSpeedEMM = NA, highOSpeedSE = NA,
                        diffOSpeedEMM = NA, diffOSpeedSE = NA,
                        oSpeedEffect = NA,
                        oSpeedPValue = NA,
                        youngEMM = NA, youngSE = NA,
                        oldEMM = NA, oldSE = NA,
                        diffAgeEMM = NA, diffAgeSE = NA,
                        ageEffect = NA,
                        agePValue = NA,
                        presentNumEffect = NA,
                        presentNumPValue = NA,
                        subjectIntercept = data.frame(VarCorr(fit.LME))[1,5],
                        actorIntercept = data.frame(VarCorr(fit.LME))[2,5], 
                        trialN = nobs(fit.LME),
                        notConvergeN = -99, singFitN = 1, modelType = "LME",
                        caseDeletionPct = caseDeletionPct, sample = sampleID)) # -99 for notConvergeN only is used to designate this specific singular fit error
    }
    else{
      return(data.frame(lowOSpeedEMM = NA, lowOSpeedSE = NA,
                        highOSpeedEMM = NA, highOSpeedSE = NA,
                        diffOSpeedEMM = NA, diffOSpeedSE = NA,
                        oSpeedEffect = NA,
                        oSpeedPValue = NA,
                        youngEMM = NA, youngSE = NA,
                        oldEMM = NA, oldSE = NA,
                        diffAgeEMM = NA, diffAgeSE = NA,
                        ageEffect = NA,
                        agePValue = NA,
                        presentNumEffect = NA,
                        presentNumPValue = NA,
                        subjectIntercept = data.frame(VarCorr(fit.LME))[1,5],
                        actorIntercept = data.frame(VarCorr(fit.LME))[2,5], 
                        trialN = nobs(fit.LME),
                        notConvergeN = -99, singFitN = -99, modelType = "LME", 
                        caseDeletionPct = caseDeletionPct, sample = sampleID)) # -99 for both notConvergeN and singFitN is used to designate other errors
    }
  }) 
}