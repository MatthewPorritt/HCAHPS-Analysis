library(rstudioapi)
library(reticulate)
library(dplyr)

#Set working directory to folder this script is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Create list of measure IDs used for CMS overall rating
#https://data.cms.gov/provider-data/topics/hospitals/overall-hospital-quality-star-rating
#https://qualitynet.cms.gov/files/627bb14cb1ccb90016b538a5?filename=OverallStarRating_Jul22_QUS.pdf
#https://qualitynet.cms.gov/files/603966dda413b400224ddf50?filename=Star_Rtngs_CompMthdlgy_v4.1.pdf
#Calculate avg z score for all metrics per group
#Overall stat group ratings for score:
#Mortality 22%
#Safety of Care 22%
#Readmission 22%
#Patient Experience 22%
#Timely and Effective Care 12%
#Re-weight when missing a full measure group!
#End scores are calculated using k-means clustering within the peer groups of facilities who have the same number of stat groups containing at least 3 metrics

#7 mortality measures
mortality_measures <- c(
  'MORT_30_AMI', #30-day death rate for heart attack patients
  'MORT_30_CABG', #Death rate for coronary artery bypass graft surgery patients
  'MORT_30_COPD', #Death rate for chronic obstructive pulmonary disease (COPD) patients
  'MORT_30_HF', #30-day death rate for heart failure patients
  'MORT_30_PN', # 30-day death rate for pneumonia patients
  'MORT_30_STK', # Death rate for stroke patients
  'PSI_4', #Death rate among surgical inpatients with serious treatable complications
  'PSI_4_SURG_COMP',
  
  'MORT-30-AMI',
  'MORT-30-CABG',
  'MORT-30-COPD',
  'MORT-30-HF',
  'MORT-30-PN',
  'MORT-30-STK',
  'PSI-4',
  'PSI-4-SURG-COMP'
)

#8 safety measures
safety_measures <- c(
  'HAI_1_SIR', #Central-line associated bloodstream infection (CLABSI)
  'HAI_1a_SIR', #
  'HAI_2_SIR', #Catheter-associated urinary tract infection (CAUTI)
  'HAI_2a_SIR', #
  'HAI_3_SIR', #Surgical site infection from colon surgery (SSI: Colon)
  'HAI_4_SIR', #Surgical site infection from abdominal hysterectomy (SSI-abdominal hysterectomy)
  'HAI_5_SIR', #Methicillin-resistant Staphylococcus aureus (or MRSA) blood infections (Antibioticresistant blood infections)
  'HAI_6_SIR', # Clostridioides difficile (or C.diff.) infections (Intestinal infections)
  'COMP_HIP_KNEE', # Rate of complications for hip and knee replacement patients
  'PSI_90', #Patient Safety and Adverse Events Composite
  
  'HAI-1-SIR',
  'HAI-1a-SIR',
  'HAI-2-SIR',
  'HAI-2a-SIR',
  'HAI-3-SIR',
  'HAI-4-SIR',
  'HAI-5-SIR',
  'HAI-6-SIR',
  'COMP-HIP-KNEE',
  'PSI-90',
)

#11 readmission measures
readmission_measures <- c(
  'EDAC_30_AMI', #Acute myocardial infarction excess days in acute care (EDAC)
  'READM_30_CABG', # Rate of unplanned readmission after coronary artery bypass graft (CABG) surgery
  'READM_30_COPD', # Rate of unplanned readmission for chronic obstructive pulmonary disease patient
  'EDAC_30_HF', #Heart failure excess days in acute care (EDAC)
  'READM_30_HIP_KNEE', #30-day rate of readmission for hip and knee replacement patients
  'EDAC_30_PN', #Pneumonia excess days in acute care (EDAC)
  'READM_30_HOSP_WIDE', #Rate of readmission after discharge from hospital
  'OP_32', #Facility 7-day risk standardized hospital visit rate after outpatient colonoscopy
  'OP_35_ADM', #Admissions visits for patients receiving outpatient chemotherapy
  'OP_35_ED', #Emergency department (ED) visits for patients receiving outpatient chemotherapy
  'OP_36', # Hospital visits after hospital outpatient surgery
  
  'EDAC-30-AMI',
  'READM-30-CABG',
  'READM-30-COPD',
  'EDAC-30-HF',
  'READM-30-HIP-KNEE',
  'EDAC-30-PN',
  'READM-30-HOSP-WIDE',
  'OP-32',
  'OP-35-ADM',
  'OP-35-ED',
  'OP-36',
)

#8 patient experience
patient_experience_measures <- c(
  'H_COMP_1_A_P', #Communication with nurses ALWAYS ++
  'H_COMP_1_U_P', #USUALLY 
  'H_COMP_1_SN_P', #SOMETIMES or NEVER --
  'H_COMP_2_A_P', #Communication with doctors
  'H_COMP_2_U_P',
  'H_COMP_2_SN_P',
  'H_COMP_3_A_P', #Responsiveness of hospital staff
  'H_COMP_3_U_P',
  'H_COMP_3_SN_P',
  'H_COMP_5_A_P', #Communication about medicines
  'H_COMP_5_U_P',
  'H_COMP_5_SN_P',
  'H_COMP_6_Y_P', #YES ++ Discharge information
  'H_COMP_6_N_P',#NO --
  'H_COMP_7_SA', #Care transition
  'H_COMP_7_A',
  'H_COMP_7_D_SD',
  'H_CLEAN_HSP_A_P', #H-HSP-RATING Hospital rating (Q21) + H-RECMND: Willingness to recommend hospital (Q22) / 2
  'H_CLEAN_HSP_U_P',
  'H_CLEAN_HSP_SN_P',
  'H_QUIET_HSP_A_P', 
  'H_QUIET_HSP_U_P',
  'H_QUIET_HSP_SN_P',
  'H_HSP_RATING_9_10', # rated 9-10 H-CLEAN-HSP Cleanliness of hospital environment (Q8) + H-QUIET-HSP Quietness of hospital environment (Q9) / 2
  'H_HSP_RATING_7_8', # rated 7-8
  'H_HSP_RATING_0_6', # rated 0-6
  'H_RECMND_DY', #definitely yes
  'H_RECMND_PY', #probably yes
  'H_RECMND_DN', #definitely no
  
  'H-COMP-1-A-P',
  'H-COMP-1-U-P',
  'H-COMP-1-SN-P',
  'H-COMP-2-A-P',
  'H-COMP-2-U-P',
  'H-COMP-2-SN-P',
  'H-COMP-3-A-P',
  'H-COMP-3-U-P',
  'H-COMP-3-SN-P',
  'H-COMP-5-A-P',
  'H-COMP-5-U-P',
  'H-COMP-5-SN-P',
  'H-COMP-6-Y-P',
  'H-COMP-6-N-P',
  'H-COMP-7-SA',
  'H-COMP-7-A',
  'H-COMP-7-D-SD',
  'H-CLEAN-HSP-A-P',
  'H-CLEAN-HSP-U-P',
  'H-CLEAN-HSP-SN-P',
  'H-QUIET-HSP-A-P',
  'H-QUIET-HSP-U-P',
  'H-QUIET-HSP-SN-P',
  'H-HSP-RATING-9-10',
  'H-HSP-RATING-7-8',
  'H-HSP-RATING-0-6',
  'H-RECMND-DY',
  'H-RECMND-PY',
  'H-RECMND-DN'
)

#12 timely and effective care
timely_and_effective_care_measures <- c(
  'IMM_3', #Percent of healthcare workers vaccinated against Influenza
  'OP_22', #Percentage of patients who left the emergency department before being seen
  'OP_23', #Percentage of patients who came to the emergency department with stroke symptoms who received brain scan results within 45 minutes of arrival
  'OP_29', #Appropriate follow-up interval for normal colonoscopy in average risk patients
  'OP_33', #External beam radiotherapy for bone metastases
  'PC_01', #: Percent of newborns whose deliveries were scheduled too early (1-3 weeks early), when a scheduled delivery was not medically necessary
  'SEP_1', # Percentage of patients who received appropriate care for severe sepsis and septic shock
  'OP_3b', #Average number of minutes before outpatients with chest pain or possible heart attack who needed specialized care were transferred to another hospital
  #'', #??? not needed / can't find except on website - Avg number of minutes before outpatients with possible heart attack who needed special care were sent to another hospital
  'OP_18b', # Average time patients spent in the emergency department before being sent home
  'OP_8', #Outpatients with low back pain who had an MRI without trying recommended treatments first, such as physical therapy
  'OP_10', #Outpatient CT scans of the abdomen that were “combination” (double) scans
  'OP_13', #Medicare patients who got cardiac imaging stress tests to screen for surgical risk before low-risk outpatient surgery
  
  #'EDV', #???
  #'OP_2', #???
  #'OP_31', #???
  #'PCH_01', #???
  
  'IMM-3',
  'OP-22',
  'OP-23',
  'OP-29',
  'OP-33',
  'PC-01',
  'SEP-1',
  'OP-3b',
  'OP-18b',
  'OP-8',
  'OP-10',
  'OP-13'
)

all_measures <- c(mortality_measures,
                  safety_measures,
                  readmission_measures,
                  patient_experience_measures,
                  timely_and_effective_care_measures)

#Set up dataframes
df_header <- c(
  'Facility_Name',
  'Facility_ID',
  'Measure_Date',
  'Measure_ID',
  'Measure_Group',
  'Score',
  'Period_Start',
  'Period_End',
  'Source_Sheet'
)

facility_name_variables <- c('Facility Name', 'Facility.Name', 'Facility_Name', 'FacilityName',
                             'Hospital Name', 'Hospital_Name', 'Hospital.Name', 'HospitalName',
                             'Provider Name', 'Provider_Name', 'Provider.Name', 'ProviderName',
                             'VHA Facility', 'VHA_Facility', 'VHA.Facility', 'VHAFacility')

facility_id_variables <- c('Facility ID', 'Facility.ID', 'Facility_ID', 'FacilityID',
                           'Hospital ID', 'Hospital_ID', 'Hospital.ID', 'HospitalID',
                           'Provider ID', 'Provider_ID', 'Provider.ID', 'ProviderID',
                           'CCN#')

measure_id_variables <- c('Measure ID', 'Measure.ID', 'Measure_ID', 'MeasureID',
                          'HCAHPS Measure ID', 'HCAHPS_Measure_ID', 'HCAHPS.Measure.ID', 'HCAHPSMeasureID',
                          'TRISS Measure ID', 'TRISS_Measure_ID', 'TRISS.Measure.ID', 'TRISSMeasureID')
  
score_variables <- c('Score',
                     'Rate',
                     'Answer Percent', 'Answer.Percent', 'Answer_Percent', 'AnswerPercent',
                     'HCAHPS Answer Percent', 'HCAHPS.Answer.Percent', 'HCAHPS_Answer_Percent', 'HCAHPSAnswerPercent',
                     'Observed Rate per 1,000', 'Observed.Rate.per.1,000', 'Observed_Rate_per_1,000',
                     'Observed Rate per 1.000', 'Observed.Rate.per.1.000', 'Observed_Rate_per_1.000',
                     'ObservedRateper1,000', 'ObservedRatePer1,000',
                     'ObservedRateper1.000', 'ObservedRatePer1.000',
                     'National Rate', 'National.Rate', 'National_Rate', 'NationalRate')

period_start_variables <- c('Measure Start Date', 'Measure_Start_Date', 'Measure.Start.Date', 'MeasureStartDate',
                            'Start Date', 'Start_Date', 'Start.Date', 'StartDate')

period_end_variables <- c('Measure End Date', 'Measure_End_Date', 'Measure.End.Date', 'MeasureEndDate',
                          'End Date', 'End_Date', 'End.Date', 'EndDate')

facility_data_raw <- data.frame(matrix(NaN,ncol=9,nrow=0))
colnames(facility_data_raw) <- df_header
faxton_data_raw <- data.frame(matrix(NaN,ncol=9,nrow=0))
colnames(faxton_data_raw) <- df_header
national_data_raw <- data.frame(matrix(NaN,ncol=9,nrow=0))
colnames(national_data_raw) <- df_header


work_table <- data.frame(matrix(NaN,ncol=9,nrow=0))
folder_table <- data.frame(matrix(NaN,ncol=9,nrow=0))


#Define a function to get measure data from any of the folders, trimmed or untrimmed, all facilities, or national
collect_measure_files <- function(folder) {
  files <- list.files(path=paste(getwd(),folder,sep='/'))
  files <- files[files != 'junk']
  folder_table <- data.frame(matrix(NaN,ncol=9,nrow=0))
  colnames(folder_table) <- df_header
  
  
  #Iterate through every file
  fileNum <- 0
  for (file in files){
    fileNum <- fileNum + 1
    print(paste('Reading ',file,'...','[',fileNum,' of ',length(files),']',sep=''))
    
    
    #Check if the file header indicates a useful file
    file_colnames <- names(read.table(paste(folder, '\\', file, sep=''),header=TRUE,nrows=1,sep=',')[- 1, ])
    # print(file_colnames)
    junk_file = FALSE
    if(!any(file_colnames %in% score_variables)) {
      print('No score column')
      junk_file = TRUE
    }
    if(!any(file_colnames %in% measure_id_variables)) {
      print('No measure ID column')
      junk_file = TRUE
    }
    if(junk_file) {
      junk_folder <- paste(folder, '\\', 'junk', sep='')
      if (!isTRUE(file.info(junk_folder)$isdir)) { dir.create(junk_folder, recursive=TRUE) }
      file.rename(from = paste(folder, '\\', file, sep=''),
                  to = paste(folder, '\\', 'junk', '\\', file, sep=''))
      next
    }
    
    
    #Grab all relevant variable data
    work_table <- read.csv(paste(folder, '\\', file, sep=''),header=TRUE)
    if(!('Facility_Name' %in% names(work_table)))
    { names(work_table)[names(work_table) %in% facility_name_variables] <- 'Facility_Name' }
    if(!('Facility_ID' %in% names(work_table)))
    { names(work_table)[names(work_table) %in% facility_id_variables] <- 'Facility_ID' }
    if(!('Measure_ID' %in% names(work_table)))
    { names(work_table)[names(work_table) %in% measure_id_variables] <- 'Measure_ID' }
    if(!('Score' %in% names(work_table)))
    { names(work_table)[names(work_table) %in% score_variables] <- 'Score' }
    if(!('Period_Start' %in% names(work_table)))
    { names(work_table)[names(work_table) %in% period_start_variables] <- 'Period_Start' }
    if(!('Period_End' %in% names(work_table)))
    { names(work_table)[names(work_table) %in% period_end_variables] <- 'Period_End' }

    
    #Fill in table with other relevant data such as date, source, and empty columns
    if(folder=='national_measure_files' | folder=='national_measure_files_trimmed') {
      work_table$Facility_Name <- rep('National', nrow(work_table))
      work_table$Facility_ID <- rep('National', nrow(work_table))
    }
    if(!('Facility_Name' %in% colnames(work_table)))
    { work_table$Facility_Name <- rep(NA, nrow(work_table)) }
    if(!('Facility_ID' %in% colnames(work_table)))
    { work_table$Facility_ID <- rep(NA, nrow(work_table)) }
    work_table$Measure_Date <- rep(substr(file,0,7),nrow(work_table))
    #Sort measure IDs into their respective measure groups
    work_table$Measure_Group <- rep(NA, nrow(work_table))
    #Remove rows where MeasureID contains substring 'OP_2' but aren't an actual measure
    #Removed filters for HAI misidentification
    work_table <- work_table[!grepl('OP_20',work_table$Measure_ID) & !grepl('OP_21',work_table$Measure_ID) &
                              !grepl('OP-20',work_table$Measure_ID) & !grepl('OP-21',work_table$Measure_ID)
                              # & !grepl('CIUPPER',work_table$Measure_ID) & !grepl('CILOWER',work_table$Measure_ID) &
                              # !grepl('CI_UPPER',work_table$Measure_ID) & !grepl('CI_LOWER',work_table$Measure_ID) &
                              # !grepl('DOPC_DAYS',work_table$Measure_ID) & !grepl('ELIGCASES',work_table$Measure_ID) &
                              # !grepl('NUMERATOR',work_table$Measure_ID)
                              ,]
    for (row in 1:nrow(work_table)) {
      id <- work_table$Measure_ID[row]
      # print(paste('Sorting ID:',id))
      if(!(id %in% all_measures)) {
        end <- gregexpr('_', id)[[1]][2]
        if(is.na(end)) { end <- gregexpr('-', id)[[1]][2] }
        id <- substr(id,1,end-1)
        if(!(id %in% all_measures)) { id <- paste(substr(id,1,nchar(id)-1), "_SIR", sep="") }
      }
      if(id %in% mortality_measures) { work_table[row,'Measure_Group'] <- 'Mortality' }
      else if (id %in% safety_measures) { work_table[row,'Measure_Group'] <- 'Safety' }
      else if (id %in% readmission_measures) { work_table[row,'Measure_Group'] <- 'Readmission' }
      else if (id %in% patient_experience_measures) { work_table[row,'Measure_Group'] <- 'Patient Experience' }
      else if (id %in% timely_and_effective_care_measures) { work_table[row,'Measure_Group'] <- 'Timely and Effective Care' }
      else {
        print(paste('WARNING: Did not find measure_group. ID:', id))
        pause
        work_table[row,'Measure_Group'] <- NA
      }
    }
    work_table[(work_table$Score == 'Not Applicable' |
                  work_table$Score == 'Not applicable' |
                  work_table$Score == 'Not Available' |
                  work_table$Score == 'Not available' |
                  work_table$Score == 'N/A'),'Score'] <- NA
    if(!('Period_Start' %in% colnames(work_table))) { work_table$Period_Start <- rep(NA, nrow(work_table)) }
    if(!('Period_End' %in% colnames(work_table))) { work_table$Period_End <- rep(NA, nrow(work_table)) }
    work_table$Source_Sheet <- rep(file, nrow(work_table))
    
    
    #Convert what variables we can into factors or dates for ease of use
    work_table$Facility_Name <- as.factor(work_table$Facility_Name)
    work_table$Facility_ID <- as.factor(work_table$Facility_ID)
    work_table$Measure_Date <- gsub('_', '/01/', work_table$Measure_Date)
    work_table$Measure_Date <- as.Date(work_table$Measure_Date, format = "%m/%d/%Y")
    work_table$Measure_ID <- gsub('-', '_', work_table$Measure_ID)
    work_table$Measure_ID <- as.factor(work_table$Measure_ID)
    work_table$Measure_Group <- as.factor(work_table$Measure_Group)
    work_table$Period_Start <- as.Date(work_table$Period_Start, format = "%m/%d/%Y")
    work_table$Period_End <- as.Date(work_table$Period_End, format = "%m/%d/%Y")
    work_table$Source_Sheet <- as.factor(work_table$Source_Sheet)
    
    #Append work_table to folder_table
    work_table <- work_table[, c('Facility_Name',
                                 'Facility_ID',
                                 'Measure_Date',
                                 'Measure_ID',
                                 'Measure_Group',
                                 'Score',
                                 'Period_Start',
                                 'Period_End',
                                 'Source_Sheet')]
    folder_table <- rbind(folder_table, work_table)
  }
  
  return(folder_table)
}


#Create a function to find and remove rows with the same dates, IDs, and scores from faxton and national data
find_duplicate_score <- function(df) {
  duplicate_date_ids <- duplicated(df[,c('Measure_Date','Measure_ID')]) |
                        duplicated(df[,c('Measure_Date','Measure_ID')], fromLast = TRUE)
  if(!any(duplicate_date_ids)) {
    print('No duplicate dates and ids found')
    return(df[FALSE,])
  }
  date_id_scores <- df[duplicate_date_ids,c('Measure_Date','Measure_ID','Score')]
  duplicate_scores_check <- duplicated(date_id_scores) |
                            duplicated(date_id_scores, fromLast = TRUE)
  if(any(!duplicate_scores_check)) {
    print('WARNING: The same date and measure ids with differing scores found')
    return(df[duplicate_date_ids,])
  }
  print('WARNING: duplicate date and id rows found, but scores were identical')
  return(df[duplicate_date_ids,])
}


#Use above functions to get and check data
national_data_raw <- collect_measure_files('national_measure_files_trimmed')
national_data_raw <- national_data_raw[with(national_data_raw, order(Measure_Date, Measure_ID)),]
national_data_dups <- find_duplicate_score(national_data_raw)
#National data has repeated date/id rows, but all with identical scores
national_data <- national_data_raw[!duplicated(national_data_raw[,c('Measure_Date','Measure_ID')]),]
rm(national_data_dups, national_data_raw)


facility_data_raw <- collect_measure_files('facility_measure_files_trimmed')
facility_data_raw <- facility_data_raw[with(facility_data_raw, order(Measure_Date, Measure_ID)),]

rm(folder_table, work_table)

#Check that there are no rows with NA in both Facility_Name and Facility_ID
na_facilities <- facility_data_raw[(is.na(facility_data_raw$Facility_Name) & is.na(facility_data_raw$Facility_ID)),]
if(nrow(na_facilities) > 0) {
  print("WARNING: Rows with no facility identification")
}

#Check that Facility_Name and Facility_ID match up
name_id_pairs <- unique(facility_data[(!is.na(facility_data$Facility_Name) & !is.na(facility_data$Facility_Name)),
                                      c('Facility_Name','Facility_ID')])
mismatched_rows <- data.frame()
if(length(name_id_pairs) != length(unique(facility_data$Facility_Name)) |
   length(name_id_pairs) != length(unique(facility_data$Facility_ID))) {
  print('WARNING: facility Name/ID mismatches found')
  mismatches <- duplicated(name_id_pairs$Facility_Name) |
                duplicated(name_id_pairs$Facility_Name, fromLast = TRUE) |
                duplicated(name_id_pairs$Facility_ID) |
                duplicated(name_id_pairs$Facility_ID, fromLast = TRUE)
  mismatches <- name_id_pairs[mismatches,]
  mismatched_rows <- facility_data[interaction(facility_data[,c('Facility_Name','Facility_ID')]) %in% interaction(mismatches),]
  names <- unique(mismatches$Facility_Name)
  ids <- unique(mismatches$Facility_ID)
  for(name in 1:length(names)) {
    if (length(mismatches[(mismatches$Facility_Name == name & !is.na(mismatches$Facility_ID)),'Facility_ID']) > 1) {
      print(paste("WARNING: Multiple non-NA ids for name:",name))
    }
  }
  for(id in 1:length(ids)) {
    if (length(mismatches[(mismatches$Facility_ID == id & !is.na(mismatches$Facility_Name)),'Facility_Name']) > 1) {
      print(paste("WARNING: Multiple non-NA name for id:",id))
    }
  }
}

#Align Facility_Name and Facility_ID


#Check that facility_data doesn't have repeat facility, date, measure columns
facility_date_id_dups <- duplicated(df[,c('Facility_ID','Measure_Date','Measure_ID')]) |
                          duplicated(df[,c('Facility_ID','Measure_Date','Measure_ID')], fromLast = TRUE)
