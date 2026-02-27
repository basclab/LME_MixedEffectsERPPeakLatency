# Peak Latency Simulation Script: 2. Generate Subject Data Log

# This script creates subject data logs for the specified number of simulated 
# datasets (samples), number of subjects, missingness pattern, and percentages of 
# low trial-count subjects. This script is run separately for each combination of
# missingness pattern and percentage of low trial-count subjects.  

# For each simulated sample, an equal number of subjects are assigned to the two
# attentional orienting speed groups (High/Low) and the two age groups (Younger/Older).  
# For each percentage of low trial-count subjects, subjects are randomly assigned to 
# have less than 10 trials per condition and missing trials are specified. Subject 
# data logs containing missing trial information for each sample and subject are 
# saved for extracting mean-averaged peak latency (LatencySim_03_SimulateERPData.m) 
# and inducing missing trials (LatencySim_04_OrganizeDataFiles.R). The same subject 
# data logs are used across all simulated peak amplitude magnitudes and resting
# state noise profiles.

# ***See Section3_SimulatedData README.md available on the LME_MixedEffectsERPPeakLatency
# GitHub for pipeline details: https://github.com/basclab/LME_MixedEffectsERPPeakLatency/tree/main/Section3_SimulatedData

# Requirements: 
  # - Needs R Version 3.6.1 and packages listed below
  # - binDescriptorFileKeyName: File specifying trial-level bin information (number, 
  #   label, and 5-digit event marker). This file is created by the LatencySim_01_CreateBinDescriptorFile.m script. 
  # - saveFolder: Folder for saving subject data logs 
  # - Variables used to specify data simulation parameters in step 1: sampleN, subjectN, 
  #   actorN, presentN, emotionLabel, oSpeedAgeLabel, missType, caseDeletionpctArray

# Script Functions:
  # 1. Specify simulation parameters
  # 2. Import bin descriptor file key
  # 3. Define function for specifying missing trials
  # 4. Loop through each simulated sample and specify missing trials for each subject
  # 5. Save subject data log files

# Outputs: 
  # - subjectDataLog_forScript3: File containing each simulated sample and subject's 
  #   assigned attentional orienting speed/age group and condition-level bins after 
  #   inducing missing trials for the specified missingness pattern and percentages 
  #   of low trial-count subjects.
  # - subjectDataLog_forScript4: File containing each simulated sample and subject's 
  #   assigned attentional orienting speed/age group and induced missing trials 
  #   for the specified missingness pattern and percentages of low trial-count subjects.

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

library(data.table) # V.1.13.2; fwrite function
library(stringi) # V.1.4.6; stri_join function
library(stringr) # V.1.4.0; str_pad function
library(openxlsx) # V.4.2.3; read.xlsx function
library(foreach) # V.1.5.2; foreach function
library(doParallel) # V.1.0.17; %dopar% operator
library(doRNG) # V.1.8.3; reproducible parallel results
RNGkind("L'Ecuyer-CMRG")

#-------------------------------------------------------------------------------
# DATA ENVIRONMENT

# Set up parallel processing to reduce processing time
numCore <- 3
clust <- makeCluster(numCore) 
registerDoParallel(clust)
showConnections()

# Specify filepath to bin descriptor file key used to keep track of unique trial-level bins
binDescriptorFileKeyName <- 'C:/Users/basclab/Desktop/Section3_SimulatedData/LMESimulation_BinDescriptorFileKey_ForPeakLatency.xlsx'

# Specify folder location to save subject data logs
saveFolder <- 'C:/Users/basclab/Desktop/Section3_SimulatedData/LMESimulation_P1PeakLatency/01_SubjectDataLog/'

#-------------------------------------------------------------------------------
# 1. SPECIFY SIMULATION PARAMETERS

# Number of simulated samples
sampleN <- 1000 

# Number of simulated subjects per sample. In this simulation, this variable is 
# divisible by 4 so that there are an equal number of subjects in each attentional 
# orienting speed/age group
subjectN <- 48 

# Number of unique actors per sample
actorN <- 5 

# Number of trial presentations of each emotion condition/actor combination
presentN <- 10 

# Total number of trials per emotion condition
emotionTrialN <- actorN*presentN 

# Name of each emotion condition (only one condition was simulated)
emotionLabel <- c('6') 

# Number of emotion conditions
emotionN <- length(emotionLabel) 

# Name of each attentional orienting speed/age group
oSpeedAgeLabel <- c("lowOSpeed_olderAgeGroup", "higOSpeed_olderAgeGroup", "lowOSpeed_youngerAgeGroup", "higOSpeed_youngerAgeGroup")

# Specify missingness pattern
missType = 'Miss1' # Formatted as 'Miss#'

# Specify probability weight distribution for trial presentation numbers 6-10. 
# - The weight for numbers 1-5 is equal to 1 - presentNumberWeight6to10.
# - For example, if presentNumberWeight6to10 = 0.7, then 70% of missing trials 
#   are from trial presentation numbers 6-10 and 30% of trials are from trial
#   presentation numbers 1-5.
# - If both weight variables are equal to 0.5, an equal number of missing trials 
#   are drawn from each trial presentation number (MCAR).
presentNumberWeight6to10 <- 0.7

# Specify probability weight distribution for Younger age group. 
# - The weight for older age is equal to 1 - ageWeightyounger.
# - For example, if ageWeightYounger = 0.7, then 70% of subjects selected for 
#   more missing trials and subsequent casewise deletion are from the Younger 
#   age group. 
ageWeightYounger <- 0.7

# Specify percentage of low trial-count subjects (e.g., 6 corresponds to 6% of
# subjects induced to have less than 10 trials/condition remaining). To replicate 
# results from manuscript, we run this script twice (once for each caseDeletionPctArray
# variable listed below)
caseDeletionPctArray <- c(0, 6, 11, 32)
#caseDeletionPctArray <- c(50, 60, 70, 80)

#-------------------------------------------------------------------------------
# 2. IMPORT BIN DESCRIPTOR FILE KEY 

binDescriptorFileKey <- read.xlsx(binDescriptorFileKeyName) 

# Extract stimuli-related information for each trial-level bin
binDescriptorFileKey$emotion <- as.factor(substr(binDescriptorFileKey$binLabel,1,1)) # Extract emotion condition ID (e.g., 6)
binDescriptorFileKey$presentNumber <- as.numeric(substr(binDescriptorFileKey$binLabel,4,5)) # Extract trial presentation number (e.g, 01)

# Create probability weight column for trial presentation number based on value 
# specified above
binDescriptorFileKey$presentNumberWeight <- ifelse(binDescriptorFileKey$presentNumber > 5,
                                         (presentNumberWeight6to10/(emotionTrialN/2)), 
                                         ((1-presentNumberWeight6to10)/(emotionTrialN/2)))

#-------------------------------------------------------------------------------
# 3. DEFINE FUNCTION FOR SPECIFYING MISSING TRIALS

# induceMissingTrials_forPeakLatency: Function to randomly specify missing trials
# for each subject in a simulated sample, and assign subjects to low trial-counts 
# (i.e., less than 10 trials/condition). Note this function assumes that there is
# 1 simulated emotion condition; see comments for needed modifications for 2+ emotion 
# conditions. 
# - Format: 
  #   subjectDataLog_oneSample <- induceMissingTrials_forPeakLatency(subjectDataLog_oneSample, caseDeletionPct)
# - Inputs:
  # - subjectDataLog_oneSample: Dataframe containing each subject's assigned 
  #   attentional orienting speed/age group.
  # - caseDeletionPct: Percentage of subjects with low trial-count (i.e., less than
  #   10 trials/condition).
# - Output: 
  # - subjectDataLog_oneSample: Copy of the input subjectDataLog_oneSample with
  #   additional columns specifying the condition-level bin equation and array of 
  #   induced missing trials for the specified percentage of low trial-count subjects.
induceMissingTrials_forPeakLatency <- function(subjectDataLog_oneSample, caseDeletionPct) {
  # Calculate the number of low trial-count subjects by multiplying caseDeletionPct
  # by the total number of subjects. If this value is not an integer, the output
  # is rounded up. 
  caseDeletionN <- ceiling((caseDeletionPct/100)*subjectN)
  
  # For labeling purposes, format percentage with 3 digits
  caseDeletionPct_pad <- str_pad(caseDeletionPct, 3, pad="0")
  
  # Create column for missing trials for this caseDeletionPct
  missTrialColName = paste0('missTrials_', caseDeletionPct_pad)
  
  # Randomly sample the subject IDs that will have low trial-counts based on the 
  # specified age weights and the caseDeletionN variable
  subjectCaseDeletion <- sample(subjectDataLog_oneSample$SUBJECTID, caseDeletionN, 
                                replace = FALSE, prob=subjectDataLog_oneSample$ageWeight)
  
  # Calculate the maximum number of trials that can be removed from each condition
  # before the subject is considered to have a low trial-count and would be
  # casewise deleted (e.g., if there are 50 trials, a maximum of 40 trials can
  # be removed for an included (i.e., not casewise deleted) subject)
  trialMissingThreshold <- emotionTrialN - 10

  # Loop through each subject who will not be casewise deleted, randomly select 
  # a subset of trials from each condition to remove, and create a condition-level  
  # bin equation of the remaining trials (after inducing missing trials)
  for (subject in subjectDataLog_oneSample$SUBJECTID) {
    subjectMissTrialLabelArray = c() # Used for recording trials to be removed in subsequent R script
    
    # Identify row to save bin in SubjectDataLog and index dataframe
    subjectDataLog_row <- subjectDataLog_oneSample$SUBJECTID == subject 
    
    if (subject %in% subjectCaseDeletion) { 
      # For subjects with a low trial-count, at least one emotion condition will 
      # have less than 10 trials remaining
      trialMissing <- c(sample(x=(trialMissingThreshold+1):emotionTrialN, size = 1),
                        sample(x=0:emotionTrialN, size = emotionN-1, replace = TRUE))     
      
    } else { 
      # For subjects who do NOT have a low trial-count, all emotion conditions
      # will have at least 10 trials
      trialMissing <- sample(x=0:trialMissingThreshold, size = emotionN, 
                             replace = TRUE)
    }
    
    # Shuffle order of emotion conditions and then loop through each condition
    # (This line is added so that one condition does not consistently have more
    # missing trials due to how the trialMissing variable is defined for subjects
    # with low trial-count.)
    emotionLabelRand <- sample(emotionLabel)
    
    for (j in 1:length(emotionLabelRand)) {
        
        emotionTrialMissing <- trialMissing[j] # Extract the number of missing trials for this condition
        
        # Find this emotion bin's trial bin rows
        emotionIndex <- which(binDescriptorFileKey$emotion == emotionLabelRand[j])
        
        if (emotionTrialMissing != 0) { # If this emotion condition was not selected to have 0 missing trials
          # Extract the presentation number probability weights for this subject and condition
          emotionProbWeight <- binDescriptorFileKey$presentNumberWeight[emotionIndex]
          
          # Randomly select the missing trials based on the specified probability 
          # weights and the number of missing trials
          emotionIndexMissing <- sample(emotionIndex, emotionTrialMissing, 
                                        replace = FALSE, prob=emotionProbWeight)
          
          # Save missing bin information for this subject
          missTrialIndex = emotionIndex[emotionIndex %in% emotionIndexMissing] # Format bins in the order listed in binDescriptorFileKey
          missTrialLabel = paste(binDescriptorFileKey$binLabel[missTrialIndex], collapse = ';')
          
          # We need this below line if there are 2+ emotion conditions (for miss trials to list all emotions) 
          subjectMissTrialLabelArray <- paste0(subjectMissTrialLabelArray, ';', missTrialLabel)
        }
        else if (emotionTrialMissing == 0) { # Otherwise create empty array if this subject has 0 missing trials
          emotionIndexMissing <- c()
        }
        
        if (!(subject %in% subjectCaseDeletion)) {
          # We will only save remaining bin information if the subject has at 
          # least 10 trials remaining (i.e., not casewise deleted)
          remainTrialIndex = emotionIndex[!(emotionIndex %in% emotionIndexMissing)]
          
          # Create variables for condition-level bin equation based on trial-level
          # bins after inducing missing trials
          remainBinNum_format <- paste0('B', binDescriptorFileKey$binNumber[remainTrialIndex]) # Append 'B' for trial-level bins (e.g., c('B2', 'B3'))
          remainBinNum_count <- length(remainBinNum_format) # Count number of trial-level bins (e.g., 2)
          binEquation_numer <- stri_join(remainBinNum_format, collapse = '+') # Format trial-level bins with + (e.g., 'B2+B3')
          binEquation_final <- paste0('(',binEquation_numer,')','/', remainBinNum_count) # Create condition-level bin equation as the average of the trial-level bins (e.g., '(B2+B3)/2')
          
          # Identify bin number and label (Note: Not flexible for multiple emotion conditions!)
          emotionBinNum = paste0('B', subjectDataLog_oneSample[subjectDataLog_row,]$currentBinNumber)
          emotionBinLabel = paste0(emotionLabelRand[j], '____', caseDeletionPct_pad)
          
          # Format condition-level bin equation (e.g., 'B52 = (B2+B2)/2 label 6____000')
          subjectDataLog_oneSample[subjectDataLog_row,][emotionBinNum] <- paste0(emotionBinNum, ' = ', binEquation_final, ' label ', emotionBinLabel)
          
          # Update this subject's currentBinNumber for subsequent condition-level bins 
          subjectDataLog_oneSample[subjectDataLog_row,]$currentBinNumber <- subjectDataLog_oneSample[subjectDataLog_row,]$currentBinNumber + 1
        }
    }
    # Store induced trial-level bins (e.g., 60102;60103); due to formatting, we 
    # index at 2 to remove the leading semicolon
    subjectDataLog_oneSample[subjectDataLog_row,][missTrialColName] <- substring(subjectMissTrialLabelArray,2)
  }
  return(subjectDataLog_oneSample)
}

#-------------------------------------------------------------------------------
# 4. LOOP THROUGH EACH SIMULATED SAMPLE AND SPECIFY MISSING TRIALS FOR EACH SUBJECT

# Create column names for saving condition-level bin equations for subject data log for script 3 
firstEquationBin <- nrow(binDescriptorFileKey) + 1 # For example, if there are 50 trial-level bins, the next available bin is 51 (for the population condition-level bin)
lastEquationBin <- firstEquationBin + length(caseDeletionPctArray) # There will be additional condition-level bins corresponding to each specified percentage of low trial-count subjects
binNumber_colNames <- paste0('B', firstEquationBin:lastEquationBin) # Format column names (e.g., B51, B52, etc.)

# Format column names for subject data log for script 4 (e.g., missTrials_000, missTrials_006, etc.)
missTrials_colNames <- paste0("missTrials_", str_pad(caseDeletionPctArray, 3, pad = '0'))

# Used to track processing duration
start_time = Sys.time()

set.seed(20230626) # Specify seed for reproducible results
subjectDataLog <- foreach (i=1:sampleN, .packages=c('stringi', 'stringr')) %dorng% { # Specify R packages used within foreach loop 
  # Initialize temporary variables for storing this sample's subject data
  # log information (this variable will be reset for each data sample)
  subjectDataLog_oneSample <- data.frame(SUBJECTID = 1:subjectN)
  
  # Add columns for condition-level bins and missing trials
  subjectDataLog_oneSample[c(binNumber_colNames, missTrials_colNames)] <- NA
  
  # Add counter for the current bin number
  subjectDataLog_oneSample$currentBinNumber <- firstEquationBin
  
  # Randomly assign age groups
  subjectNSubgroup = ceiling(subjectN/length(oSpeedAgeLabel)) # Number of subjects assigned to each age group (this number is rounded up if subjectN is not evenly divisible by the number of age groups)
  oSpeedAgeGroupArray_original = rep(oSpeedAgeLabel, each=subjectNSubgroup); # Create age intercept array (NOTE: This array is not randomized yet and may have a length greater than subjectN)
  subjectDataLog_oneSample$oSpeed_age <- sample(oSpeedAgeGroupArray_original, subjectN, replace = FALSE) # Extract randomized age intercept array of length subjectN 
    
  # Extract age column based on oSpeed_age column
  subjectDataLog_oneSample$age <- substr(subjectDataLog_oneSample$oSpeed_age, 11, 25)
  
  # Create probability weight column for age group using based on values specified
  # in step 1
  subjectDataLog_oneSample$ageWeight <- ifelse(subjectDataLog_oneSample$age == 'youngerAgeGroup', 
                                 ageWeightYounger/(subjectN/2), (1-ageWeightYounger)/(subjectN/2))
  
  # Add population condition-level bin equation (no missing trials); this equation
  # will be the same for all subjects
  allBinNum_format <- paste0('B', binDescriptorFileKey$binNumber) # Append 'B' for all trial-level bins (e.g., c('B1', 'B2', 'B3', ...))
  allBinNum_count <- length(allBinNum_format) # Count number of all trial-level bins (e.g., 50)
  binEquation_popNumer <- stri_join(allBinNum_format, collapse = '+') # Format trial-level bins with + (e.g., 'B1+B2+B3+...')
  binEquation_popFinal <- paste0("(",binEquation_popNumer,")","/", allBinNum_count) # Create condition-level bin equation as the average of all trial-level bins (e.g., '(B1+B2+B3+...)/50')
  subjectDataLog_oneSample$B51 <- paste0('B51 = ', binEquation_popFinal, ' label 6____Pop') # Format condition-level bin equation (e.g., '(B1+B2+B3+...)/50 label 6____Pop')
  
  # Update current bin number to +1 to reflect added population condition-level bin
  subjectDataLog_oneSample$currentBinNumber <- subjectDataLog_oneSample$currentBinNumber + 1
  
  #-----------------------------------------------------------------------------
  # Loop through each specified percentage of low trial-count subjects
  for (j in 1:length(caseDeletionPctArray)) { 
    
    # Induce corresponding number of missing trials and output updated subject data log
    subjectDataLog_oneSample <- induceMissingTrials_forPeakLatency(subjectDataLog_oneSample, caseDeletionPctArray[j])
  }
  
  subjectDataLog_oneSample$sample <- i # Record current sample ID
  subjectDataLog_oneSample # This line ensures that the foreach loop will return the output variable 
}
subjectDataLogFinal <- do.call(rbind, subjectDataLog) # Bind subject data log output across all samples into one dataframe

# Calculate processing duration
end_time = Sys.time()
end_time - start_time

#-------------------------------------------------------------------------------
# 5. SAVE SUBJECT DATA LOG FILES

# For the subject data log for script 3 (contains condition-level bins)
subjectDataLog_forScript3 <- subjectDataLogFinal[,c("sample", "SUBJECTID", "oSpeed_age", binNumber_colNames)] # Extract columns for export
subjectDataLogFilename_forScript3 = paste0(saveFolder, missType, '_SubjectDataLog_sampleN',sampleN,'_subN',subjectN,'_forScript3.csv') # Specify filename
fwrite(subjectDataLog_forScript3, file = subjectDataLogFilename_forScript3, row.names = FALSE)

# For the subject data log for script 4 (contains missing trial information)
subjectDataLog_forScript4<- subjectDataLogFinal[,c("sample", "SUBJECTID", "oSpeed_age", missTrials_colNames)]
subjectDataLogFilename_forScript4 = paste0(saveFolder, missType, '_SubjectDataLog_sampleN',sampleN,'_subN',subjectN,'_forScript4.csv')
fwrite(subjectDataLog_forScript4, file = subjectDataLogFilename_forScript4, row.names = FALSE)

# Close parallel cluster
stopCluster(clust)
showConnections()