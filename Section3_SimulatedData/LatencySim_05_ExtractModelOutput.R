# Peak Latency Simulation Script: 5. Extract Model Output

# This script imports the simulated peak latency file for each sample and percentage 
# of low trial-count subjects. Data are analyzed using two approaches: 
  # - LME analysis of trial-level (i.e., single-trial) peak latency 
  # - Conventional ANOVA analysis using condition-level (i.e., mean-averaged)
  #   peak latency using a common trial threshold (10 trials per condition).
  #   Subjects who do not meet the trial threshold for a specified percentage of
  #   low trial-count subjects were casewise deleted in the LatencySim_03_SimulateERPData.m script.

# This script is run separately for each combination of missingness pattern, peak
# amplitude magnitude, and resting state noise profile.

# ***See Section3_SimulatedData README.md available on the LME_MixedEffectsERPPeakLatency
# GitHub for pipeline details: https://github.com/basclab/LME_MixedEffectsERPPeakLatency/tree/main/Section3_SimulatedData

# Requirements: 
  # - Needs R Version 3.6.1 and packages listed below
  # - importFolder: Folder containing the peak latency files created by 
  #   LatencySim_04_OrganizeDataFiles.R
  # - See instructions in data environment and step 1 for specifying other variables
  # - LatencySim_05_fun.R is saved in the same working directory

# Script Functions:
  # 1. Specify simulation parameters
  # 2. Load each simulated data file
  # 3. Fit LME model
  # 4. Fit ANOVA model
  # 5. Save model output across all simulated datasets

# Outputs: 
  # - One .csv file containing the estimated marginal means for each attentional
  #   orienting speed group and other model predictors across all models, simulated
  #   datasets, and low trial-count percentages. There is one file for each combination
  #   of missingness pattern, peak amplitude magnitude, and resting state noise 
  #   profile (e.g., Missingness Pattern #1/10 microvolts peak/preschool noise). 
  #   See ModelOutput_DataDictionary.xlsx file for further information.

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
library(data.table) # V.1.13.2; fread function
library(stringr) # V.1.4.0; str_sub function
library(car) # V.3.0-10; contr.sum function
library(lme4) # V.1.1-25; LME models
library(lmerTest) # V.3.1-3; p-values for LME
library(afex) # V.0.28-1; ANOVA models
library(emmeans) # V.1.5.3; estimated marginal means calculation
library(performance) # V.0.6.1; check_convergence function
library(foreach) # V.1.5.2; foreach function
library(doParallel) # V.1.0.17; %dopar% operator

#-------------------------------------------------------------------------------
# DATA ENVIRONMENT

# Set up parallel processing to reduce processing time
numCore <- 7
clust<-makeCluster(numCore) 
registerDoParallel(clust)
showConnections()

# Add functions used for fitting LME and ANOVA models 
source("LatencySim_05_funs.R") 

# Specify noise characteristics to import 
missType <- 'Miss1' # Formatted as 'Miss#'
peakType <- '10MicroV' # Formated as '#MicroV'
noiseProfile <- 'PreschoolNoise' # Formatted as '[Sample]Noise'

# Specify folder location of data files for analysis
importFolder <- paste0('C:/Users/basclab/Desktop/Section3_SimulatedData/02_P1PeakLatencyOutput_Final/', missType, '_', peakType, '_', noiseProfile)

# Extract percentages of low trial-count subjects using subfolders in importFolder
caseDeletionPctArray <- list.dirs(importFolder, full.names = FALSE, recursive = FALSE) 

# Specify folder location to save output file
saveFolder <- 'C:/Users/basclab/Desktop/Section3_SimulatedData/03_ModelOutput'

#-------------------------------------------------------------------------------
# 1. SPECIFY SIMULATION PARAMETERS

# The following parameters were set in the LatencySim_02_GenerateSubjectDataLog.R
# script 
sampleN <- 1000 # Number of simulated samples
subjectN <- 48 # Number of simulated subjects per sample
presentNumberRef <- 5.5 # Average presentation number simulated in data (used for emmeans calculations)

# Specify LME model formula
formulaLME = peakLatencyP1 ~ oSpeed + age + presentNumber + (1|SUBJECTID) + (1|ACTOR)

# Used to track processing duration
start_time = Sys.time()

#-------------------------------------------------------------------------------
# Loop through each percentage of low trial-count subjects using foreach loop for 
# parallel processing

modelOutput <- foreach (i=1:length(caseDeletionPctArray), .packages=c('tidyr', 'data.table', 'performance', 'emmeans', 'stringr', 'car', 'lme4', 'lmerTest', 'afex')) %dopar% { # Specify R packages used within foreach loop
  setDTthreads(1) # Used for fread function 
  
  # Preallocate empty list to store model output. We specify the length as the 
  # number of analysis models (2: LME and ANOVA) fit to each sample 
  modelOutput_oneCaseDeletionPct = vector("list", 2*sampleN)

  # Extract the current percentage of low trial-count subjects  
  caseDeletionPct <- caseDeletionPctArray[i]

  # Make directory of all .csv files in the current caseDeletionPct folder 
  fileDir <- list.files(path = paste0(importFolder, '/', caseDeletionPct), pattern = ".csv", full.names = TRUE, recursive = FALSE)
 
  for (j in 1:length(fileDir)) { # Loop through each simulated sample
    #-----------------------------------------------------------------------------
    # 2. LOAD EACH SIMULATED DATA FILE 
    
    # Import peak latency file containing single-trial and mean-averaged peak latencies
    # for the specified sample and percentage of low trial-count subjects
    dfImport <- fread(fileDir[j]) 
    
    # Extract sample ID
    sampleID <- str_sub(fileDir[j],-28,-25)
    
    # Specify SUBJECTID and ACTOR columns as factors for subsequent analysis 
    dfImport$SUBJECTID <- as.factor(dfImport$SUBJECTID)
    dfImport$ACTOR <- as.factor(dfImport$ACTOR)
    # Specify oSpeed and age columns as factors with effects coding 
    dfImport$oSpeed <- as.factor(dfImport$oSpeed)
    contrasts(dfImport$oSpeed) <- contr.Sum(levels(dfImport$oSpeed))
    dfImport$age <- as.factor(dfImport$age)
    contrasts(dfImport$age) <- contr.Sum(levels(dfImport$age)) 
    
    # Subset trial-level and condition-level peak latency data
    dfImport_trial <- dfImport[dfImport$peakLatencyType == "Trial",]
    dfImport_cond <- dfImport[dfImport$peakLatencyType == "Cond",]
  
    #---------------------------------------------------------------------------
    # 3. FIT LME MODEL
    
    modelOutput_oneCaseDeletionPct[[(2*(j-1))+1]] <- extractLMEOutput(formulaLME, dfImport_trial, caseDeletionPct, sampleID) 
    
    #---------------------------------------------------------------------------
    # 4. FIT ANOVA MODEL
    
    # Note that low trial-count subjects have already been casewise deleted
    # and excluded in LatencySim_03_SimulateERPData.m
    modelOutput_oneCaseDeletionPct[[(2*(j-1))+2]] <- extractANOVAOutput(dfImport_cond, caseDeletionPct, sampleID) 
  }
  modelOutput_oneCaseDeletionPct # This line ensures that the foreach loop will return the output variable 
}
modelOutputFinal <- do.call(rbind, do.call(Map, c(f = rbind, modelOutput))) # Bind model output across all low trial-count percentages into one dataframe

# Calculate processing duration
end_time = Sys.time()
end_time - start_time

#-------------------------------------------------------------------------------
# 5. SAVE MODEL OUTPUT ACROSS ALL SIMULATED DATASETS 
modelOutputFilename = paste0(saveFolder, '/ModelOutput_', missType, '_', peakType, '_', noiseProfile, '_sampleN', sampleN, '_subN', subjectN, '.csv')
fwrite(modelOutputFinal, file = modelOutputFilename, row.names = FALSE)

# Close parallel cluster
stopCluster(clust)
showConnections()