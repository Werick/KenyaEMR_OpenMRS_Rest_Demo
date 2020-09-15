# This R script contains R Rest functions to help in accessing OpenMRS
# And KenyaEMR servers using openmrs platform web services module

# library stack
library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library("stringr")

# Helper Functions
checkServerConnection <- function(base_url,userpass) {
  endpoint <- "session"
  url <- paste(base_url,endpoint, sep = "")
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  return(json_object)
}

getPatient_encounters <- function(base_url, patientUuid, startIndex, userpass) {
  endpoint <- "encounter"
  # print(startIndex)
  url <-""
  if (startIndex==0) {
    url <- paste(base_url,endpoint,"?patient=",patientUuid,"&v=custom:(uuid,display)", sep = "")
  } else {
    url <- paste(base_url,endpoint,"?patient=",patientUuid,"&v=custom:(uuid,display)", "&startIndex=", startIndex, sep = "")
  }
  
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  return(json_object)
}


getPatient_visits <- function(base_url, patientUuid, startIndex, userpass) {
  endpoint <- "visit"
  url <- ""
  
  if (startIndex==0) {
    url <- paste(base_url, endpoint, "?patient=",patientUuid,"&v=custom:(uuid,display)",sep = "")
  } else {
    url <- paste(openmrs_base_url,endpoint,"?patient=",patientUuid,"&v=custom:(uuid,display)", "&startIndex=",startIndex ,sep = "")
  }
  
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  return(json_object)
}

getPatient_obs <- function(base_url, patientUuid, startIndex, userpass) {
  endpoint <- "obs"
  url <- ""
  
  if (startIndex==0) {
    url <- paste(base_url, endpoint, "?patient=", patientUuid, "&v=custom:(uuid,display)", sep = "")
  } else {
    url <- paste(base_url, endpoint, "?patient=", patientUuid, "&v=custom:(uuid,display)", "&startIndex=", startIndex, sep = "")
  }
  
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  return(json_object)
}

getAllPatient_encountersList <- function(base_url, patient_uuid, userpass) {
  more_results <- 1
  startIndex <- 0
  encounter_df <- data.frame()
  
  while (more_results==1) {
    encounter_list <- getPatient_encounters(base_url, patient_uuid, startIndex, userpass)
    # print(encounter_list)
    # add the results to a data frame 
    if (nrow(encounter_df) == 0) {
      encounter_df <- encounter_list$results
    } else {
      encounter_df <- rbind(encounter_df,encounter_list$results)
    }
    
    # Check to see if we have an extra page of results
    if ("links" %in% names(encounter_list)) {
      
      # if links is available, loop throu the list to see if there is a next page of results
      nextpg_in <- encounter_list$links %>%
        filter(rel=="next")
      if(nrow(nextpg_in)>0) {
        #print("Fetching more rows")
        more_results <- 1
        startIndex <- startIndex + 50
      } else {
        more_results <- 0
      }
      
    } else {
      # exit loop no extra results to be fetched 
      more_results <- 0
    }
  }
  
  return(encounter_df) 
}

getConceptbyUuid <- function(base_url, concept_uuid, userpass) {
  endpoint <- "concept/"
  format_output <- "?v=custom:(uuid,display,name:(uuid),datatype:(uuid,display),conceptClass:(uuid,display))"
  
  url <-paste(base_url, endpoint,  concept_uuid, format_output,  sep = "")
  
  
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  
  return(json_object)
  
}

# function returns an encounter object with all the properties of the encounter
getEncounterbyUuid <- function(base_url, enc_uuid, userpass) {
  endpoint <- "encounter/"
  
  form_output <- "?v=custom:(uuid,display,encounterDatetime,patient:(uuid),location:(uuid),form:(uuid),encounterType:(uuid),obs:(uuid,display),encounterProviders:(uuid))"
  
  url <-paste(base_url, endpoint,  enc_uuid, form_output,  sep = "")
  
  
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  
  return(json_object)
  
}

# function returns an obs object with all the properties of the obs
getobsbyUuid <- function(base_url, obs_uuid, userpass) {
  endpoint <- "obs/"
  
  url <- paste(base_url, endpoint,  obs_uuid,  sep = "")
  
  feedBack_from_Server <- GET(url, add_headers(Authorization=paste("Basic",userpass)))
  
  #Deserialize the content
  text_content <- content(feedBack_from_Server,"text")
  
  #Convert to json
  json_object <- fromJSON(text_content, flatten = TRUE)
  
  return(json_object)
  
}

post_encounter <- function(kenyaemr_base_url,payload, userpass) {
  post_enpoint <- "encounter"
  url <- paste(kenyaemr_base_url, post_enpoint,  sep = "")
  post_payload_json <- toJSON(post_payload)
  
  # remove the following xters from the "[ and "] so that the payload is correctly formated
  # in order for openmrs to accept
  post_payload_json_formated <- gsub('\\[\\"|\\"\\]','\\"',post_payload_json)
  
  feedBack_from_Server <- POST(url, add_headers(Authorization=paste("Basic",userpass)), content_type_json(),body = post_payload_json_formated) # add verbose(), if you want to see wath is being posted
  
  ## show feedback from the server - uncomment to troubleshoot
  # http_status(feedBack_from_Server)
  #content(feedBack_from_Server)
  return(feedBack_from_Server)
}


