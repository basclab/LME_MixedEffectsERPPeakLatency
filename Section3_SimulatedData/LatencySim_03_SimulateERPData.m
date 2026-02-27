% Peak Latency Simulation Script: 3. Simulate ERP Data

% This script simulates trial-level (i.e., single-trial) ERP data for one 
% emotion condition for subsequent analysis of peak latency. Simulated subjects
% are assigned to a High/Low attentional orienting speed group and a Younger/Older 
% age group based on the subject data log created with the LatencySim_02_GenerateSubjectDataLog.R 
% script. The current script is run for each combination of missingness pattern,
% peak amplitude magnitude, and resting state noise profile. Two helper 
% functions are used: simulateOneSample_forPeakLatency, simulateOneSubject_forPeakLatency

% The number of simulated datasets (samples), subjects per sample,  
% random seed, lead field, electrode montage, electrodes of interest, dipole 
% location and orientation, peak amplitude magnitude, and resting state noise
% files are specified in this current script. Other simulation parameters 
% (e.g., number of simulated trials) are specified in the helper functions. 
% Missing data are specified via condition-level bins from the imported subject 
% data log. Subjects randomly assigned to have fewer than 10 trials per condition 
% do not have condition-level bins extracted for the specified low
% trial-count percentage.

% ***See Section3_SimulatedData README.md available on the LME_MixedEffectsERPPeakLatency
% GitHub for pipeline details: https://github.com/basclab/LME_MixedEffectsERPPeakLatency/tree/main/Section3_SimulatedData

% Requirements:    
    % - Needs MATLAB R2019a, EEGLAB v 2019_0, ERPLAB v 8.01, SEREEGA v 1.1.0
        % - For more information on EEGLAB, see: Delorme, A. & Makeig, S. (2004).
        %   EEGLAB: An open source toolbox for analysis of single-trial EEG dynamics
        %   including independent component analysis. https://sccn.ucsd.edu/eeglab/index.php
        % - For more information on ERPLAB, see: Lopez-Calderon, J., & Luck, S. J.
        %   (2014). ERPLAB: An open-source toolbox for the analysis of event-related
        %   potentials. https://erpinfo.org/erplab/    
        % - For more information on SEREEGA, see: Krol, L. R., Pawlitzki, J., Lotte, F.,
        %   Gramann, K., & Zander, T. O. (2018). SEREEGA: Simulating event-related EEG
        %   activity. https://github.com/lrkrol/SEREEGA
    % - Pediatric Head Atlas release v 1.1, Atlas 2 (4-8 years old) files
        % - Atlas files should be requested and downloaded from: https://www.pedeheadmod.net/pediatric-head-atlases/
        % - The folder containing the atlas files should then be added
        %   to the MATLAB path (via "Home" > "Set Path" > "Add with Subfolders").
    % - Filepath to the following files and folders:
        % - importSubjectDataLogFilepath: File containing each simulated 
        %   sample and subject's assigned attentional orienting speed/age group 
        %   and condition-level bins after inducing missing trials for the 
        %   specified missingness pattern and percentages of low trial-count 
        %   subjects. This file is created by the LatencySim_02_GenerateSubjectDataLog.R script.
        % - saveFolder: Folder for saving simulated data output files.
        %   This parent folder has the following subfolders: 01_P1PeakLatencyOutput_PreMerge
        %   and (optional) 01_ERPFiles (see Outputs section below). 
        % - noiseProfileFolder: Folder containing the trial-level real noise
        %   waveforms saved as .erp files for the corresponding sample 
        %   (i.e., infant or preschooler). To download these files, go to
        %   https://osf.io/b53wj/ > "Files" > "InfantNoise" or "PreschoolNoise" >
        %   "ForP1Simulations".
    % - Variables used to specify data simulation parameters: sampleN,
    %   subjectN, peakAmp, noiseProfile, and other variables listed below.
    % - Requires parallel computing toolbox (if not available, set M = 1)
    % - (Specified in simulateOneSubject_forPeakLatency function) Filepath to 
    %   the following file used during processing: 
        % - binDescriptorFilename: File specifying trial-level bin information
        %   (number, label, and 5-digit event marker). This file is created 
        %   by the LatencySim_01_CreateBinDescriptorFile.m script. 
          
% Script Functions:
    % 1. Define simulation parameters
    % 2. Simulate data with helper functions
    
% Outputs:
    % - NOTE: All output files are generated and saved in the
    %   simulateOneSample_forPeakLatency function.   
    % - Peak latency .txt files with one peak latency value per bin/
    %   electrode/subject. There is one file for each simulated sample.
    %   Each file contains the following columns:
        % - worklat: The peak latency for this simulated subject's
        %   specified bin and electrode. This value is extracted from a positive 
        %   peak from O2 (corresponding to E83 of the EGI HydroCel GSN 
        %   128-electrode montage) over a 85-145 ms time window.
        % - value: The peak amplitude for this simulated subject's specified
        %   bin and electrode. This value is extracted from a positive 
        %   peak from O2 (corresponding to E83 of the EGI HydroCel GSN 
        %   128-electrode montage) over a 85-145 ms time window.
        % - chindex: The electrode number (e.g., electrode #1 corresponds to 
        %   E83 in this simulation).
        % - chlabel: The label for this electrode (e.g., E83).
        % - bini: A number generated by ERPLAB when concatenating ERP
        %   data files across each sample's subjects. NOTE: This number
        %   is NOT used for subsequent processing. 
        % - binlabel: A label composed of [SUBJECTID]_:_[trial-specific bin label].
        %   The trial-specific bin label corresponds to the labels from the
        %   bin descriptor file (created in LatencySim_01_CreateBinDescriptorFile.m).
        %   The binlabel column is used to identify subject ID and stimuli-related  
        %   information in LatencySim_04_OrganizeDataFiles.R. 
        % - ERPset: This column is intentionally empty because it is NOT needed 
        %   for identifying subject ID (see binlabel column above). 
    % - (Optional) .erp files containing the trial-level waveforms for all
    %   subjects in a sample. There is one file for each simulated sample.
    %   These files are useful for visualizing waveforms or troubleshooting. 

% Copyright 2026 Serena K. Mon, Megan J. Heise, Lindsay C. Bowman
% Brain and Social Cognition Lab, University of California Davis, Davis, CA, USA.

% Permission is hereby granted, free of charge, to any person obtaining a 
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense, 
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included 
% in all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

%% DATA ENVIRONMENT
tic % Used to keep track of simulation duration

% Specify filepath to subject data log for script 3 with each subject's assigned 
% attentional orienting speed/age group and condition-level bins after 
% inducing missing trials for the specified missingness pattern and percentages 
% of low trial-count subjects. 
importSubjectDataLogFilepath = 'C:\Users\basclab\Desktop\Section3_SimulatedData\01_SubjectDataLog\Miss1_SubjectDataLog_sampleN1000_subN48_forScript3.csv';
opts = detectImportOptions(importSubjectDataLogFilepath); % Extract MATLAB's default import options
opts = setvartype(opts,{'B51', 'B52', 'B53', 'B54', 'B55'},'char'); % Specify condition-level bin equations as character
subjectDataLog = readtable(importSubjectDataLogFilepath, opts);

% Specify folder location for saving output files. This parent folder
% contains the 01_P1PeakLatencyOutput_PreMerge subfolder (see Outputs section above).
saveFolder = 'C:\Users\basclab\Desktop\Section3_SimulatedData';

%% 1. DEFINE SIMULATION PARAMETERS

% Extract sampleN and subjectN based on imported subject data log filename
importSubjectDataLogFilepath_split = split(importSubjectDataLogFilepath, ["_", "sampleN", "subN"]);
sampleN = str2num(importSubjectDataLogFilepath_split{6}); % Number of simulated samples
subjectN = str2num(importSubjectDataLogFilepath_split{8}); % Number of simulated subjects per sample

% Noise characteristics for simulated data
peakAmp = -32; % Peak amplitude magnitude at dipole when measured at electrode O2 (-32 corresponds to a positive peak of 10 µV; -63 corresponds to a peak of 20 µV)
noiseProfile = 'PreschoolNoise'; % InfantNoise or PreschoolNoise

% Set seed for reproducible results
randSeed = 20230627;

%% 2. SIMULATE DATA WITH HELPER FUNCTIONS

[ALLEEG EEG CURRENTSET ALLCOM] = eeglab; % Initialize EEGLAB

% Define the following simulation parameters with the SEREEGA lf_generate_frompha
% function:
% - Lead field: Pediatric Head Atlas release v 1.1, Atlas 2 (4-8 years old)
% - Electrode montage: EGI HydroCel GSN 128-electrode montage
% - Electrodes of interest: O2 (corresponding to E83 of the HydroCel GSN
%   montage). An additional electrode (O1, corresponding to E70) was also
%   simulated because the SEREEGA toolbox required simulating 2+ electrodes.
leadField   = lf_generate_frompha('4to8','128','labels',{'E70','E83'});

% Identify the nearest source location to the right temporal lobe
% source reported in Wong et al. (2008) with MNI coordinates of
% x = 22, y = -76, z = 5
sourceLocs  = lf_get_source_nearest(leadField, [22 -76 5]);

% Specify the dipole orientation reported in Wong et al. (2008)
% for the right cuneus source
leadField.orientation(sourceLocs,:) = [-0.08 -0.91 -0.40];

% Load resting state noise files for the specified sample
noiseProfileFolder = ['C:\Users\basclab\Desktop\Section3_SimulatedData\00_', noiseProfile, 'Repository\'];
noiseProfileFilenameArrayStuct = dir(fullfile(noiseProfileFolder, '*.erp'));
noiseProfileFilenameArray = {noiseProfileFilenameArrayStuct.name};
[tempERP noiseProfileERPArray] = pop_loaderp( 'filename', noiseProfileFilenameArray, 'filepath', noiseProfileFolder );

M = 7; % Define number of workers for parfor loop. Set M = 1 if you do not have parallel computing toolbox set up

% Loop through each of the specified data samples (defined with the
% sampleStart and sampleN variables)
parfor (sample = 1:sampleN, M)
    % Set substream of each iteration for reproducible results
    stream = RandStream('CombRecursive', 'Seed', randSeed);
    RandStream.setGlobalStream(stream);
    stream.Substream = sample;
    
    % Extract subjectDataLog rows for this sample to specify each subject's
    % assigned attentional orienting speed/age group and condition-level
    % bins after inducing missing trials
    subjectDataLog_oneSample = subjectDataLog(subjectDataLog.sample == sample,:);
    
    % Call helper function with specified input arguments (see comments in
    % simulateOneSample_forPeakLatency function for more information)
    simulateOneSample_forPeakLatency(sample, subjectN, saveFolder, leadField, sourceLocs, peakAmp, noiseProfileERPArray, subjectDataLog_oneSample);
end

toc % Used to keep track of simulation duration