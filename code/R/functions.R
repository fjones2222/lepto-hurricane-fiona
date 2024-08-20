date_fiona <- as.Date("2022-09-17")

pacman::p_load(
        tidyverse,
        readxl,
        # dataRetrieval,
        tidycensus,
        lubridate,
        cowplot,
        glue,
        sf,
        grid
)

# library(EpiNow2)
# library(reactable)



#download the different floodstages for usgs sites
get_flood_stage <- function(sites){
  base_url <- "https://waterwatch.usgs.gov/webservices/floodstage?site="  
  df_all <- data.frame()
  for(i in sites){ #the service can only take 1 site at a time
    full_url <- paste0(base_url, i)    
    list_return <- jsonlite::fromJSON(full_url)    
    df_all <- rbind(df_all, list_return[["sites"]])
  }  
  return(df_all)
}




#takes a column and replace NA's with Missing
make_missing <- function(col){
  
  out <- col
  out[is.na(out)] <- "Missing"
  
  return(out)
}

#gets the hospital name from Hospital.System.Details
extract_hospital_name<- function(string) {
  split <- htm2txt::htm2txt(string) %>%
    str_split("\\n") %>%
    unlist()
  
  split[2]
  
  
}

#take nbs export and get lab table
extract_lab_data <- function(df){
  
  
  results_df <- data.frame()
  
  
  for(i in 1:nrow(df)){
    
    #select a row
    this_row <- df[i,]
    #make the dataframe
    tmp_df <- make_df(this_row$Laboratory.Information)
    #assign the patient ID
    #find the id
    tmp_df <- tmp_df %>% 
      mutate(Patient.Local.ID=this_row$Patient.Local.ID,
             Investigation.Local.ID=this_row$Investigation.Local.ID,
             Event.Date=this_row$Event.Date
      ) %>%
      relocate(Patient.Local.ID,
               Investigation.Local.ID,
               Event.Date)
    
    
    results_df <- bind_rows(results_df,tmp_df)
    
    cat(i,": ",this_row$Patient.Local.ID, "\n\n")
    
  }
  
  return(results_df)
  
  
  
}



#from a Lab infor string, extract the dataframes
make_df <- function(string){
  
  #break up each test into the lists
  string_list <- string %>% str_split("<br><br>") %>%
    unlist() %>%
    htm2txt::htm2txt() %>%
    str_split("\n")
  
  #create a list of dataframes
  out_list <- lapply(string_list,FUN= function(vec){
    
    if(length(vec)!=1){
      colon_split <- vec %>% str_split_fixed(":",2)
      names <- colon_split[,1]
      values <- colon_split[,2] %>% trimws("left")
      
      return(data.frame(names,values) %>%
               mutate(values=ifelse(values=="",NA,values)) %>%
               spread(names,values))
      
    }
    if(length(vec)==1) return(NULL)
    
  }  )
  
  #output one dataframe
  bind_rows(out_list)
  
  
}
