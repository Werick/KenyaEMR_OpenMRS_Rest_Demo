# This script is to be used to transfer/migrate data from OpenMRS to KenyEMR
# This is an initial attempt for some few variables
# Written on 10/09/2020
# Written by Wafula Erick
# This script will use openmrs platform REST webservices module to transfer data from
# a Vanila OpenMRS server to KenyaEMR

#library
source('script/openmrsKenyaemrHelperFunctions.R')
source('script/otherHelperFunctions.R')
# set up

# Run this query on kenyaemr to resolve concept uuid issues
# update concept set uuid= concat(concept_id,repeat('A',36-length(concept_id)))
# where uuid not like '%AAA%'

# Get required files
# 1. Login credentials
# 2. Concept Mapping
# 3. List of patients mapped in openmrs and KenyaEMR using person uuid

df_userdetails <- read.csv("./data/credentials.csv")
concept_mapping_df <- read.csv(file = "./data/openmrs_kenyaemr_green_card_concep_mapping.csv",
                               stringsAsFactors = FALSE)

# concept uuid loolup table. Am forced to use this after KenyaEMR failed to honor the contract
# of concept uuid naming which is usually the concept_id + a couple for letter "A" at the end
df_concept_uuid_lookup <- read.csv(file = "./data/concept_uuid_lookup.csv",
                                   stringsAsFactors = FALSE)

# Get patient details for both OpenMRS and KenyaEMR
df_ke_patient_list <- read.csv("./data/kenyaemr_patient_list_kitare.csv" ,stringsAsFactors = FALSE)
df_openmrs_patient_list <- read.csv("./data/openmrs_patient_list_kitare.csv", stringsAsFactors = FALSE)

df_ke_patient_list$clinic_id <- df_ke_patient_list$ke_clinic_id
df_openmrs_patient_list$clinic_id <- df_openmrs_patient_list$openmrs_clinic_id


df_patient_list <- df_ke_patient_list %>%
  left_join(df_openmrs_patient_list, by='clinic_id')

# get patient Missing clinic id
df_patient_missing_id <- df_patient_list %>% 
  filter(is.na(openmrs_patient_uuid))

# Drop patients not matched in Kenyaemr
df_patient_list <- df_patient_list %>% 
  filter(!(is.na(openmrs_patient_uuid)))

  
  
# Get use name password
# It is good to user a super user who has the same username and password on both systems
username <- df_userdetails$username
password <- df_userdetails$password
# glimpse(df_userdetails)

#Set the base Urls for the two servers of interest
openmrs_base_url <- "http://192.168.1.200:8080/openmrs/ws/rest/v1/"
kenyaemr_base_url <- "http://192.168.1.201:8080/openmrs/ws/rest/v1/"

#Encode username and password using base64 as required by OpenMRS
userpass <-base64_enc(paste(username,":",password, sep = ""))


# Test Server connection
get_authenticated_openmrs <- checkServerConnection(openmrs_base_url,userpass)
get_authenticated_kenyaemr <- checkServerConnection(kenyaemr_base_url,userpass)

if (get_authenticated_openmrs$authenticated & get_authenticated_kenyaemr$authenticated) {
  print("Good to go now")
} else {
  print("Not Authenticated Try again")
}
  

# Get all HIV consultation/followup encounters for each patient from bothe OpenMRS and KenyaEMR
# Using the patient uuid
# the assumption is that we have list of all the patients mapped on both systems
# the workflow assumes that patients have been created on both systems
# We loop thro all the patients but for now we will start with one test patient
# as proof of concept

# Change this to  for a loop once you are done
#

#function to process a given encounter
processEncounter <- function(df) {
  # Get the encounter from openmrs and carryout the necessary mapping of the concepts
  enc_uuid <- df['uuid']
  #print(paste("THIS UUID",enc_uuid))
  encounter <- getEncounterbyUuid(openmrs_base_url,enc_uuid,userpass)
  encounter$obs$openmrs_question <- apply(encounter$obs,1,setobs_question)
  encounter$obs$openmrs_answer <- apply(encounter$obs,1,setobs_answer)
  
  # Arrange the obs
  encounter$obs <- encounter$obs %>%
    arrange(openmrs_question)
  
  # Set the uuid for the question based on the concep mapping sheet
  encounter$obs$kenyaemrquestionuuid <- apply(encounter$obs,1,setKenyaEmrQuestionUuid)
  encounter$obs$kenyaemranswer <- apply(encounter$obs,1,setKenyaEmrAnswerValue)
  
  # List of questions that have not been mapped. We need to keep this list
  # so that we can review and mapp where possible
  not_mapped_questions <- encounter$obs %>%
    filter(is.na(kenyaemranswer) | is.na(kenyaemrquestionuuid) | kenyaemranswer == "NULL")
  
  # Select only those that have been mapped
  encounter$obs <- encounter$obs %>%
    filter(!(is.na(kenyaemranswer) | is.na(kenyaemrquestionuuid) | kenyaemranswer == "NULL"))
  
  returnList <- list("NotMappedObs" = not_mapped_questions,
                     "encounter" = encounter)
  
  return(returnList)
  
}


#get patient from openmrs
df_global_all_encounter <- data.frame()
df_global_all_question_not_mapped <- data.frame()
gcounter <- 0

df_patient_list$has_encounter <-0
df_patient_list$n_encounter <-0

#nrow(df_patient_list)
for (row in 1:nrow(df_patient_list)) {
  patientUuid <- df_patient_list[row, 'openmrs_patient_uuid']
  
  # Get all the encounters for the selected patient from Openmrs
  openmrs_encounter<-getAllPatient_encountersList(openmrs_base_url, patientUuid, userpass)
  
  
  print(paste("Current on patient Number: ", row))
  # Set the encounter type and date
  openmrs_encounter$enc_date <- apply(openmrs_encounter, 1, setencounter_date)
  openmrs_encounter$enc_type <- apply(openmrs_encounter, 1, setencounter_type)
  
  # Get all the encounters for the selected patient from KenyaEmr
  patientUuid <- df_patient_list[row, 'ke_patient_uuid']
  
  kenyaemr_encounter<-getAllPatient_encountersList(kenyaemr_base_url, patientUuid, userpass)
  
  # Set the encounter type and date
  kenyaemr_encounter$enc_date <- apply(kenyaemr_encounter, 1, setencounter_date)
  kenyaemr_encounter$enc_type <- apply(kenyaemr_encounter, 1, setencounter_type)
  
  #convert to date format
  kenyaemr_encounter$enc_date_0 <- as.Date(as.character(kenyaemr_encounter$enc_date), 
                                                format="%m/%d/%Y")
  openmrs_encounter$enc_date_0 <- as.Date(as.character(openmrs_encounter$enc_date), 
                                               format="%d/%m/%Y")
  
  # filter encounters
  kenyaemr_encounter <- kenyaemr_encounter %>%
    filter(trimws(enc_type) =='HIV Consultation')
  
  # We don't need initial bcoz the patient is already created
  openmrs_encounter <- openmrs_encounter %>%
    filter(!(trimws(enc_type) =='PSC INITIAL'))
  
  
  # get encounters in OpenMRS that are not in KenyaEMR using encounter date
  df_enc_list_to_migrate <- openmrs_encounter %>%
    filter(!(enc_date_0 %in% kenyaemr_encounter$enc_date_0))
  
  if (nrow(df_enc_list_to_migrate)>0) {
    df_patient_list[row,'has_encounter'] <- 1
    df_patient_list[row,'n_encounter'] <- nrow(df_enc_list_to_migrate)
    
    # Add columns to help in Analyis
    df_enc_list_to_migrate$successful <- 0
    df_enc_list_to_migrate$ke_uuid <- NA
    
    
    # Keep a list of all encounters being migrated
    if (nrow(df_global_all_encounter)==0) {
      df_global_all_encounter <- df_enc_list_to_migrate
    } else {
      df_global_all_encounter <- df_global_all_encounter %>%
        rbind(df_enc_list_to_migrate)
    }
    
    # this will be used in building the payload especially the encounterProviders section
    
    encounter_ke <- getEncounterbyUuid(kenyaemr_base_url,kenyaemr_encounter[1,'uuid'],userpass)
    
    for (r in 1: nrow(df_enc_list_to_migrate)) {
      #print(paste("GETTING DAMN HERE",nrow(df_enc_list_to_migrate)))
      gcounter <- gcounter + 1
      
      retList <- processEncounter(df_enc_list_to_migrate[r,])
      
      # Keep track of all questions not mapped
      if (nrow(df_global_all_question_not_mapped)==0) {
        df_global_all_question_not_mapped <- retList$NotMappedObs
      } else {
        df_global_all_question_not_mapped <- df_global_all_question_not_mapped %>%
          rbind(retList$NotMappedObs)
      }
      
      post_payload <- generate_encounter_payload(retList$encounter,encounter_ke)
      
      feedback <- post_encounter(kenyaemr_base_url, post_payload, userpass)
      if (feedback$status_code == "201") {
        print("TEST 1 SUCCESS")
        text_content <- content(feedback,"text")
        
        #Convert to json
        json_object <- fromJSON(text_content, flatten = TRUE)
        df_global_all_encounter[gcounter,'successful'] <-1
        df_global_all_encounter[gcounter,'ke_uuid'] <-json_object$uuid
      }
      
    }
    
  }
  
}


