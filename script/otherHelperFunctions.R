# Other general fucntions
setencounter_date <- function(df) {
  enc_date <- str_sub(df['display'],start = -10, end = -1)
  return(as.character(enc_date))
  
}

setencounter_type <- function(df) {
  enc_type <- str_sub(df['display'],start = 1,end = nchar(df['display'])-11)
  return(enc_type)
}

setobs_question <- function(df) {
  #print(df)
  #str_split returns a list of items- use unlist to get the individual items
  obs_question <- str_split(df['display'], ":")
  #print(unlist(obs_question))
  return(unlist(obs_question)[1])
  
}

setobs_answer <- function(df) {
  obs_answer <- strsplit(df['display'],":")
  return(as.character(trimws(unlist(obs_answer)[2])))
  
}

# function to build an encounter object with appropriate properties from the json object
processObs <- function(jsonObject) {
  jsonObject$obs$openmrs_question <- apply(jsonObject$obs,1,setobs_question)
  jsonObject$obs$openmrs_answer <- apply(jsonObject$obs,1,setobs_answer)
  
  return(jsonObject)
}


setKenyaEmrQuestionUuid <- function(df) {
  
  kequestionuuid <- NA
  emr_mapping_df <- concept_mapping_df %>%
    filter(trimws(question_openmrs)  == trimws(df['openmrs_question']))
  if (nrow(emr_mapping_df)>0) {
    #kequestionuuid <- paste(as.character(emr_mapping_df[nrow(emr_mapping_df),'kenyaemr_uuid']), strrep("A",36-nchar(as.character(emr_mapping_df[nrow(emr_mapping_df),'kenyaemr_uuid']))), sep = "")
    answ <- df_concept_uuid_lookup %>%
      filter(as.character(concept_id)== as.character(emr_mapping_df[nrow(emr_mapping_df),'kenyaemr_uuid']))
    
    if (nrow(answ)>0) {
      kequestionuuid <- answ[nrow(answ), 'uuid']
    } 
    
  }
  return(kequestionuuid)
}

setKenyaEmrAnswerValue <- function(df) {
  
  answerValue <- NA
  emr_mapping_df <- concept_mapping_df %>%
    filter(toupper(trimws(question_openmrs))  == toupper(trimws(df['openmrs_question'])))
  if (nrow(emr_mapping_df)>0) {
    
    # print(emr_mapping_df)
    
    if (emr_mapping_df[nrow(emr_mapping_df),'question_type'] == "Numeric") {
      #print(emr_mapping_df)
      answerValue <- as.numeric(df['openmrs_answer'])
      #print(answerValue)
      
    } else if (emr_mapping_df[nrow(emr_mapping_df),'question_type'] == "Date") {
      
      answerValue <- as.character(df['openmrs_answer'])
      
    }  else if (emr_mapping_df[nrow(emr_mapping_df),'question_type'] == "Boolean") {
      
      if (toupper(trimws(df['openmrs_answer'])) == "YES") {
        answerValue <- 1
      } else {
        answerValue <- 0
      }
      
      # print(paste(answerValue,df['openmrs_answer']))
      
      
    } else if (emr_mapping_df[nrow(emr_mapping_df),'question_type'] == "coded") {
      
      # we create key value pair in order to get the selected response
      kenyaemr_mapped_list <- emr_mapping_df[nrow(emr_mapping_df),'ke_answer_uuid_list']
      openmrs_mapped_list <- emr_mapping_df[nrow(emr_mapping_df),'openmrs_answer_key_list']
      
      kenyaemr_mapped_list_split <- unlist(str_split(kenyaemr_mapped_list, ",")) 
      openmrs_mapped_list_split <- unlist(str_split(openmrs_mapped_list, ","))
      
      # the assumption is that the two lists are of the same length
      
      #initialize the List
      answer_key_value_list <- list ()
      
      for(i in 1:length(openmrs_mapped_list_split)) {
        
        key <- toupper(trimws(openmrs_mapped_list_split[i])) 
        value <- trimws(kenyaemr_mapped_list_split[i])
        answer_key_value_list[[key]] <- value
        
      }
      
      answeruuid <- as.character(answer_key_value_list[toupper(df['openmrs_answer'])])
      #answerValue <- paste(as.character(answeruuid), strrep("A",36-nchar(as.character(answeruuid))), sep = "")
      answ <- df_concept_uuid_lookup %>%
        filter(as.character(concept_id)==as.character(answeruuid))
      if (nrow(answ)>0) {
        answerValue <- answ[nrow(answ),'uuid']
      } 
      
      
      if (answeruuid == "NULL") {
        #print("NULL ANSWER UUIDS for ATTENTION")
        #print(df['openmrs_question'])
        #print(answerValue)
        #print(answer_key_value_list)
      }
      
      
    } else {
      print(paste("****not Spectified****",df['openmrs_question']))
    }
    
    
  }
  return(answerValue)
}

generate_encounter_payload <- function(jsonObject, encounter_ke, patientUuid) {
  
  obs <- jsonObject$obs %>%
    select(kenyaemrquestionuuid,kenyaemranswer) %>%
    rename(concept=kenyaemrquestionuuid,value=kenyaemranswer)
  
  
  encounterProviders <- encounter_ke$encounterProviders %>%
    rename(provider=uuid)
  
  encounterProviders$encounterRole<- "a0b03050-c99b-11e0-9572-0800200c9a66"
  encounterProviders$provider <- "ae01b8ff-a4cc-4012-bcf7-72359e852e14"
  
  
  
  #visit <- list("visitType" = "3371a4d4-f66f-4454-a86d-92c7b3da990c")
  
  patient_uuid <- "be0af8a0-1ef6-487a-b843-aefe48578758" # This should come from the spreadsheet or passed to the programm
  payload <- list("encounterDatetime" = jsonObject$encounterDatetime,
                  "patient" = patient_uuid,
                  "encounterType" = "a0034eee-1940-4e35-847f-97537a35d05e",
                  "location" = encounter_ke$location$uuid, 
                  "form" = "23b4ebbd-29ad-455e-be0e-04aa6bc30798",
                  "encounterProviders" = encounterProviders,
                  "obs" = obs)
  
  return(payload)
}

