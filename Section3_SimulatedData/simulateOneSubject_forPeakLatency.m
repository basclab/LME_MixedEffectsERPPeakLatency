% Peak Latency Simulation Helper Function: Simulate One Subject's ERP Data File

% Purpose: This function simulates an ERP data file for one subject. The file
% contains trial-level (i.e., single-trial) ERP waveforms that have been generated
% for 50 trials consisting of 1 emotion condition (A), 5 different 'actors', and 10
% presentations of each emotion condition/actor combination.  

% Notes: 
    % - The peak latency of trial-level waveforms are modulated by actor, 
    %   subject, attentional orienting speed, and age. 
    % - The peak amplitude magnitude was selected to produce the desired 
    %   positive peak amplitude measured at electrode O2. We do not
    %   simulate any peak amplitude modulations.
    % - Real noise from the specified noise profile are added in a subsequent
    %   step in the simulateOneSample_forPeakLatency function.

% ***See Section3_SimulatedData README.md available on the LME_MixedEffectsERPPeakLatency
% GitHub for pipeline details: https://github.com/basclab/LME_MixedEffectsERPPeakLatency/tree/main/Section3_SimulatedData

% Format:
    % ERP = simulateOneSubject_forPeakLatency(samplePeakLatency_oneSubject, sampleActorPeakLatencyArray, leadField, sourceLocs, peakAmp)

% Inputs:
    % - samplePeakLatency_oneSubject: Peak latency value for this 
    %   subject's assigned attentional orienting speed/age group. 
    % - sampleActorPeakLatencyArray: Array of actor intercepts randomly generated in
    %   the simulateOneSample_forPeakLatency function. The first value corresponds 
    %   to the intercept for actor 01, the second value corresponds to actor 02, etc.
    % - leadField: Data structure created in LatencySim_03_SimulateERPData.m 
    %   for specifying the lead field, electrode montage, electrodes of interest,
    %   and dipole orientation.
    % - sourceLocs: Index used to identify the dipole location from the
    %   leadField structure. This variable was created in
    %   LatencySim_03_SimulateERPData.m.
    % - peakAmp: Peak amplitude magnitude at dipole (values have been
    %   selected to produce the desired positive peak amplitude measured at
    %   electrode O2).
    
% Other Requirements: 
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
    % - Filepath to the following file used during processing:
        % - binDescriptorFilename: File specifying each bin's number, label, 
        %   and 5-digit event markers. This file is created by the
        %   LatencySim_01_CreateBinDescriptorFile.m script.
        
% Function Steps:
    % 1. Define simulation parameters
    % 2. Define function for generating ERP signal class
    % 3. Generate trial-level waveforms with parameters and function from steps 1-2
    % 4. Update event marker preceding codes with trial presentation number
    % 5. Extract bin-based epochs
    % 6. Calculate trial-level ERPs
    % 7. Delete extra simulated electrodes

% Outputs:
    % - ERP: ERP file containing the subject's trial-level waveforms.

% Usage Example:
    % >> samplePeakLatency_oneSubject = 115; 
    % >> sampleActorPeakLatencyArray = [-7.3117    4.1694  -11.2942    9.3109   11.5938];
    % >> leadField   = lf_generate_frompha('4to8','128','labels',{'E70','E83'});
    % >> sourceLocs  = lf_get_source_nearest(leadField, [22 -76 5]);
    % >> leadField.orientation(sourceLocs,:) = [-0.08 -0.91 -0.40];
    % >> peakAmp = -32;
    % >> ERP = simulateOneSubject_forPeakLatency(samplePeakLatency_oneSubject, sampleActorPeakLatencyArray, leadField, sourceLocs, peakAmp)

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

function ERP = simulateOneSubject_forPeakLatency(samplePeakLatency_oneSubject, sampleActorPeakLatencyArray, leadField, sourceLocs, peakAmp)
%% 1. DEFINE SIMULATION PARAMETERS 

    % Specify filepath of bin descriptor file
    binDescriptorFilename = 'C:\Users\basclab\Desktop\Section3_SimulatedData\LMESimulation_BinDescriptorFile_ForPeakLatency.txt';
    
    % Specify 3-digit preceding code used for creating event markers. Each 
    % preceding code corresponds to one emotion condition/actor
    % combination. The first digit (e.g., 6) corresponds to the emotion 
    % condition (e.g., A) and the last two digits correspond to the actor ID. 
    % For more information, see the LMESimulation_EventMarkerMappingKey_ForPeakLatency.xlsx spreadsheet
    emotionPrecCodes = ["601", "602", "603", "604", "605"];
    
    % Define parameters for simulated epoch window 
    preStimPeriod = 200; % Pre-stimulus/baseline period (ms)
    postStimPeriod = 300; % Post-stimulus period (ms)
    samplingRate = 1000; % Sampling rate (Hz)

    % Define parameters for presentation of each emotion condition/actor
    presentN = 10; % Number of presentations of each emotion condition/actor combination
    presentNumberArray = repmat(pad(string(1:presentN),2,'left','0')',length(emotionPrecCodes),1); % Create a string array with repeating values of 1, 2, ..., presentN for each emotion condition/actor combination

    % Create epoch structure based on above parameters
    epochs = struct('n', presentN, 'srate', samplingRate, 'length', preStimPeriod+postStimPeriod,'prestim',preStimPeriod);

    % Define parameters for the peak amplitude and latency of the one emotion condition 
    peakAmpDv = 0; % Peak amplitude deviation (this value is 0 because we introduce waveform noise via real noise profiles in a subsequent step)
    peakAmpSlope = 0; % Peak amplitude slope (this value is 0 because we do not want to simulate a change in amplitude with each successive presentation)
    peakLatencyDv = 0; % Peak latency deviation (this value is 0 because we introduce waveform noise via real noise profiles in a subsequent step)
    peakWindow = 60; % Window length for simulated peak (i.e., a peak spanning 85-145 ms has a window length of 60 ms)

    % Define parameters for subject-specific intercept added to peak
    % latency
    subjectPeakLatencyMean = 0; % Peak latency population mean for subject intercept distribution
    subjectPeakLatencySD = 10; % Peak latency population standard deviation for subject intercept distribution 
      
    % Specify number of unique actors based on input argument
    actorN = length(sampleActorPeakLatencyArray);   

    % Specify electrode to keep
    keepERPChan = 2; 

    % Specify unneeded channels as a string for ERPLAB input.
    % In this example, we simulate 2 channels and the channel we want (O2)
    % is channel 2 so we delete the first channel. This step is important
    % because the real noise file contains data for one channel (O2)
    deleteERPChans = 'delerpchan( 1)';
    
%% 2. DEFINE FUNCTION FOR GENERATING ERP SIGNAL CLASS

    % Anonymous function to generate an ERP "signal class" with the specified
    % peak amplitude, latency, and slope. The signal class is then used by the
    % generate_scalpdata function to simulate trial-level waveforms.  
    % - Inputs:
        % - overallPeakAmp: Peak amplitude specified in
        %   LatencySim_03_SimulateERPData.m.
        % - overallPeakLatency: Peak latency summed over the peak latency values
        %   for the actor intercept, subject intercept, and attentional orienting 
        %   speed/age intercept.  
	    % - peakAmpSlope: Decay rate for the peak amplitude.
        % - Other variables (e.g., emotionPeakWindow) are defined above and held
        %   constant for all simulated subjects. 
    % - Output: 
        % - ERP signal class with the specified parameters.    
    erp = @(overallPeakAmp, overallPeakLatency, peakAmpSlope) ...
        utl_check_class(struct( ...
        'type', 'erp', ...
        'peakLatency', overallPeakLatency, ...
        'peakWidth', peakWindow, ...
        'peakAmplitude', overallPeakAmp, ...
        'peakAmplitudeSlope', peakAmpSlope, ...
        'peakAmplitudeDv', peakAmpDv, ...
        'peakLatencyDv', peakLatencyDv, ...
        'peakLatencyShift', 0));

%% 3. GENERATE TRIAL-LEVEL WAVEFORMS WITH PARAMETERS AND FUNCTION FROM STEPS 1-2

    % Randomly select this subject's peak latency intercept from a normal 
    % distribution with above parameters
    subjectPeakLatencyIncrement = normrnd(subjectPeakLatencyMean,subjectPeakLatencySD); 
    
    % Generate peak amplitude array using the same value for all emotion condition/actor
    % combinations
    overallPeakAmpArray = repelem(peakAmp,actorN);

    % Generate peak latency array with one value for each unique emotion
    % condition/actor combination. Given that only one emotion condition was 
    % simulated, each value consists of the sum of the actor intercept, 
    % subject intercept, and attentional orienting speed/age intercept. 
    % (NOTE: While the actor intercept varies, the subject and attentional 
    % orienting speed/age intercepts do not.) 
        % - For example, the first value of overallPeakLatencyArray is the sum
        %   of the peak latencies for actor 01 + subject intercept + 
        %   attentional orienting speed/age intercept. The second value is 
        %   the sum of actor 02 + subject intercept + attentional orienting 
        %   speed/age intercept, and so on. 
    overallPeakLatencyArray = repelem(preStimPeriod,actorN) + repelem(samplePeakLatency_oneSubject,actorN) + repmat(subjectPeakLatencyIncrement,1,actorN) + repmat(sampleActorPeakLatencyArray, 1, length(samplePeakLatency_oneSubject));
   
    % Simulate the first emotion condition/actor's 10 trial-level waveforms 
    % (corresponding to 10 presentations of this emotion condition/actor). 
    v = 1; % Counter variable indexing the overallPeakLatencyArray variable
    % Define a component consisting of a neural source, dipole orientation 
    % and ERP signal
    componentTemp1 = struct('source', sourceLocs, ... 
        'signal', {{erp(overallPeakAmpArray(v),overallPeakLatencyArray(v),peakAmpSlope)}});
    % Validate the component structure
    componentTemp1 = utl_check_component(componentTemp1, leadField);
    % Simulate the trial-level waveforms with the specified component,
    % leadfield, and epoch structure
    dataTemp1 = generate_scalpdata(componentTemp1, leadField, epochs, 'showprogress', 0);
    % Format the simulated data as an EEGLAB .set file and add the
    % corresponding event marker preceding code (e.g., 601)
    EEGTemp1 = utl_create_eeglabdataset(dataTemp1, epochs, leadField, ...
        'marker', convertStringsToChars(emotionPrecCodes(v)));

    % Repeat this process for the next emotion condition/actor until trial-level
    % waveforms for all emotion condition/actor combinations have been simulated
    v = 2;
    componentTemp2 = struct('source', sourceLocs, ...
        'signal', {{erp(overallPeakAmpArray(v),overallPeakLatencyArray(v),peakAmpSlope)}});
    componentTemp2 = utl_check_component(componentTemp2, leadField);
    dataTemp2 = generate_scalpdata(componentTemp2, leadField, epochs, 'showprogress', 0);
    EEGTemp2 = utl_create_eeglabdataset(dataTemp2, epochs, leadField, ...
        'marker', convertStringsToChars(emotionPrecCodes(v)));
    EEG1 = pop_mergeset(EEGTemp1,EEGTemp2); % Merge the two EEG .set files together (note that pop_mergeset only accepts two input arguments at a time)

    v = 3;
    componentTemp3 = struct('source', sourceLocs, ...
        'signal', {{erp(overallPeakAmpArray(v),overallPeakLatencyArray(v),peakAmpSlope)}});
    componentTemp3 = utl_check_component(componentTemp3, leadField);
    dataTemp3 = generate_scalpdata(componentTemp3, leadField, epochs, 'showprogress', 0);
    EEGTemp3 = utl_create_eeglabdataset(dataTemp3, epochs, leadField, ...
        'marker', convertStringsToChars(emotionPrecCodes(v)));
    EEG2 = pop_mergeset(EEG1,EEGTemp3);

    v = 4;
    componentTemp4 = struct('source', sourceLocs, ...
        'signal', {{erp(overallPeakAmpArray(v),overallPeakLatencyArray(v),peakAmpSlope)}});
    componentTemp4 = utl_check_component(componentTemp4, leadField);
    dataTemp4 = generate_scalpdata(componentTemp4, leadField, epochs, 'showprogress', 0);
    EEGTemp4 = utl_create_eeglabdataset(dataTemp4, epochs, leadField, ...
        'marker', convertStringsToChars(emotionPrecCodes(v)));
    EEG3 = pop_mergeset(EEG2,EEGTemp4);

    v = 5;
    componentTemp5 = struct('source', sourceLocs, ...
        'signal', {{erp(overallPeakAmpArray(v),overallPeakLatencyArray(v),peakAmpSlope)}});
    componentTemp5 = utl_check_component(componentTemp5, leadField);
    dataTemp5 = generate_scalpdata(componentTemp5, leadField, epochs, 'showprogress', 0);
    EEGTemp5 = utl_create_eeglabdataset(dataTemp5, epochs, leadField, ...
        'marker', convertStringsToChars(emotionPrecCodes(v)));
    EEG = pop_mergeset(EEG3,EEGTemp5);

    EEG = epoch2continuous(EEG); % Concatenate the epoched dataset into a continuous dataset

%% 4. UPDATE EVENT MARKER PRECEDING CODES WITH TRIAL PRESENTATION NUMBER

    allEventTypes = {EEG.event.type}'; % Extract the subject's event markers (consisting of 3-digit preceding codes) 
    nonBoundaryEventIdx = ~strcmp(allEventTypes, "boundary"); % Locate non-boundary event markers (i.e., event markers from the emotionPrecCodes array)
    % Update the event marker name with the presentation number (e.g., convert
    % the first instance of 601 to 60101; convert the second instance of 601 to
    % 60102, etc.)
    allEventTypes(nonBoundaryEventIdx) = strcat(allEventTypes(nonBoundaryEventIdx), cellstr(presentNumberArray));
    [EEG.event.type] = deal(allEventTypes{:}); % Update the subject's event marker array with the final 5-digit event markers
    EEG = eeg_checkset(EEG, 'eventconsistency'); % Check EEG event array for inconsistencies (e.g., event markers out of order) 

%% 5. EXTRACT BIN-BASED EPOCHS

    % Create EventList
    EEG  = pop_creabasiceventlist(EEG, 'AlphanumericCleaning', 'on', 'BoundaryNumeric', {-99}, 'BoundaryString', {'boundary'});

    % Assign events to bins
    EEG  = pop_binlister(EEG, 'BDF', binDescriptorFilename, 'IndexEL',  1, 'SendEL2', 'EEG', 'UpdateEEG', 'off', 'Voutput', 'EEG','Report','off');

    % Extract bin-based epochs and baseline correct
    EEG = pop_epochbin(EEG, [-preStimPeriod  postStimPeriod],  'pre');

%% 6. CALCULATE TRIAL-LEVEL ERPS

    ERP = pop_averager(EEG , 'Criterion', 'good', 'DQ_flag', 1, 'ExcludeBoundary', 'off', 'SEM', 'on');

%% 7. DELETE EXTRA SIMULATED ELECTRODES

    % We delete O1 (extra simulated electrode) from the data file so that 
    % the data file has only O2. This makes it easier to append the O2 data
    % when we add the simulated ERPset with the real noise file. 
    ERP = pop_erpchanoperator( ERP, { deleteERPChans} , 'ErrorMsg', 'popup', 'KeepLocations',  0, 'Warning', 'on' );
    
    % Save binerror for final bin channel only
    ERP.binerror = ERP.binerror(keepERPChan,:,:); 
end