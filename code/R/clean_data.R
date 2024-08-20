source("code/R/functions.R")

options(java.parameters = "-Xmx4g")  # Adjust the memory limit as needed


NBS_linelist <-  xlsx::read.xlsx("data/NBS_exports/20240401/PR Leptospirosis Inv 2021-2024 (04-01-2024) (complete data).xlsx",
                                 sheetIndex = 1,
                                 password = "leptonbs"
) %>%
  mutate(Event.Date=as.Date(Event.Date))

gc()

NBS_labs <- xlsx::read.xlsx("data/NBS_exports/20240401/PR Leptospirosis Lab Reports 2021-2024 (04-01-2024).xlsx",
                            sheetIndex = 1,
                            password = "leptonbs"
)

gc()


## raw files used for clean files
write_rds(NBS_linelist,"data/generated_data/cleaned_data/NBS_linelist.rds")
write_rds(NBS_labs,"data/generated_data/cleaned_data/NBS_labs.rds")


NBS_linelist <- read_rds("data/generated_data/cleaned_data/NBS_linelist.rds")
NBS_labs <- read_rds("data/generated_data/cleaned_data/NBS_labs.rds")


# rgdal::ogrInfo("data/generated_data/2022-10-11-NBS-addresses_afterfiona.gdb/2022-10-11-NBS-addresses_afterfiona.gdb")

# geocode_cases_after1<-rgdal::readOGR("data/geocode/2022-10-11-NBS-addresses_afterfiona_1.gdb/2022-10-11-NBS-addresses_afterfiona.gdb") %>%
#   as.data.frame() %>%
#   select(patient_id=USER_Patient_Local_ID,
#          investigation_id=USER_Investigation_Local_ID,
#          Score, Longitude=coords.x1,Latitude=coords.x2)
# 
# geocode_cases_after2<-rgdal::readOGR("data/geocode/2022-10-11-NBS-addresses_afterfiona_2.gdb/2022-10-11-NBS-addresses_afterfiona.gdb") %>%
#   as.data.frame() %>%
#   select(patient_id=USER_Patient_Local_ID,
#          investigation_id=USER_Investigation_Local_ID,
#          Score, Longitude=coords.x1,Latitude=coords.x2)
# 
# geocode_cases_before<-rgdal::readOGR("data/geocode/2022-10-11-NBS-addresses_beforefiona.gdb/2022-10-11-NBS-addresses_beforefiona.gdb") %>%
#   as.data.frame() %>%
#   select(patient_id=USER_Patient_Local_ID,
#          investigation_id=USER_Investigation_Local_ID,
#          Score, Longitude=coords.x1,Latitude=coords.x2)
# 
# geocode_cases_batch2<-rgdal::readOGR("data/geocode/Geocode_Batch2_Lepto_20221220.gdb/Geocode_Batch2_Lepto_20221220.gdb") %>%
#   as.data.frame() %>%
#   select(patient_id=USER_Patient_Local_ID,
#          investigation_id=USER_Investigation_Local_ID,
#          Score, Longitude=coords.x1,Latitude=coords.x2
#          
#   )
# 
# 
# 
# geocode_cases <- bind_rows(geocode_cases_after1,geocode_cases_before,geocode_cases_batch2)


#bring in clean_name tables 
test_names <- read_rds("data/generated_data/lab_assignment/test_names.rds") %>%
                select(-n) %>% distinct()

result_names <- read_rds("data/generated_data/lab_assignment/result_names.rds")%>%
  select(-n)%>%
  mutate(Numeric.Results=as.character(Numeric.Results))
specimen_names <- read_rds("data/generated_data/lab_assignment/specimen_names.rds")%>%
  select(-n)
lab_names <- read_rds("data/generated_data/lab_assignment/lab_names.rds")%>%
  select(-n)




clean_labs <- NBS_labs %>%
  mutate(limited_comment=substr(Result.Comments,1,20))%>%
  left_join(test_names,by = c("Resulted.Test.Name", "Alternate.Lab.Test.Name", "Text.Result"))%>%
  left_join(result_names,  by = c("Coded.Result", "Test.Result.Code", "Numeric.Results", "Text.Result", "limited_comment", "clean_test")) %>%
  left_join(specimen_names,by = c("Specimen.Src.Desc", "Specimen.Src.Cd", "limited_comment")) %>%
  left_join(lab_names, by = "Reporting.Facility")%>%
  select(
    patient_id=Patient.Local.ID,
    lab_id=Lab.Report.Local.Id,
    Date.Specimen.Collected,
    test_name=clean_test,
    test_result=clean_result,
    Ordering.Facility,
    Reporting.Facility,
    Reporting.Facility.Category=clean_lab,
    Lab.Test.Date,
    specimen_type=clean_specimen
  )

patient_results <- clean_labs %>% 
  filter(test_name %in% c("IgM","PCR"))%>%
  group_by(patient_id,test_name) %>%
  summarize(result=case_when(
    any(test_result == "Positive") ~ "Positive",
    (!any(test_result == "Positive")) &
      any(test_result == "Negative") ~ "Negative",
    TRUE ~ "Missing"
  ) )   %>%
  spread(test_name,result) 


death_raw <- read_excel("data/abigail_linelist/DEATH LINELIST.xlsx")

death_df <- death_raw %>%
  filter(!is.na(CLASIFICATION)) %>%
  mutate(patient_id=glue("PSN1{str_pad(`NBS ID`,7,side='left',pad='0')}PR01"))


symptom_df <-NBS_linelist %>% select(
  
  patient_id=Patient.Local.ID,
  investigation_id=Investigation.Local.ID,
  
  
  hospitalized=Investigation.Hospital.Name,
  #death=Investigation.Patient.Death,
  antibiotic_status=Antibiotics.Given,
  antibiotic_name=Which.Antibiotics,
  
  Fever,
  
  Myalgia, Headache,  Conjunctivitis, Thrombocytopenia,
  Rash, Jaundiced,Persistent.Vomiting, Abdominal.Pain,
  Severe.Bleeding,Nausea, Diarrhea, Skin.lesion,
  
  Additional.Symptoms,
  Renal.failure,
  Dialysis,
  Liver.failure,
  `Risk.Factor.Meningitis`,
  Cardio.Respiratory.Failure,
  Mechanical.Ventilation,
  Other.Symptoms.Specify
  
  
  
  
  # Myalgia, Headache, Jaundiced, Conjunctivitis,
  # Rash,
  # 
  # `Risk.Factor.Meningitis`,
  # Abdominal.Pain, Nausea, Persistent.Vomiting, Diarrhea,
  # Cardio.Respiratory.Failure,#missing pulmonary conditions
  # Renal.failure,
  # Severe.Bleeding,
  # Liver.failure
  
) %>%
  mutate(across(hospitalized:Other.Symptoms.Specify,make_missing)) %>%
  mutate(group1= (Myalgia=="Yes") +
           ( Headache=="Yes") +
           (Jaundiced=="Yes") +
           (Conjunctivitis=="Yes") +
           (Rash=="Yes"),
         
         Gastrointestinal =  (Abdominal.Pain=="Yes") +
           ( Nausea=="Yes") +
           (Persistent.Vomiting=="Yes") +
           (Diarrhea=="Yes") ,
         
         
         group2=    (`Risk.Factor.Meningitis`=="Yes") +
           ( Gastrointestinal>=1) +
           (Renal.failure=="Yes") +
           (Severe.Bleeding=="Yes") +
           (Liver.failure=="Yes")
  ) %>%
  mutate(clinically_compatible = ifelse(
    Fever=="Yes" & (group1 >=2 | group2>=1),
    "Yes","No"  )) %>%
  mutate(death=ifelse(patient_id %in% death_df$patient_id,
                      "Yes","No"
                      ))

exposure_df <-NBS_linelist %>% select(
  
  patient_id=Patient.Local.ID,
  investigation_id=Investigation.Local.ID,
  
  Contact.with.Animals,
  Contact.with.Rodents,
  Contact.with.Dogs,
  Contact.with.Livestock,
  Contact.with.Pigs=Pigs,
  Other.Animal.Contact,
  Animal.Contact.Occurence,
  
  Contact.with.Water,
  
  Flood.Water,
  Lake.Pond.Marsh,
  River.or.Stream,
  Other.Water,
  Water.Contact.Location,
  
  Contact.with.Potentially.Contaminated.Food,
  
  
  Farmer,
  Rancher,
  Fishery.Sectory.Worker,
  Veterinarian,
  Slaughterhouse.Worker,
  Animal.Caretaker
  
) %>%
  mutate(across(Contact.with.Animals:Animal.Caretaker,make_missing)) %>%
  mutate(occupation_risk=ifelse(
    Farmer=="Yes" | Rancher=="Yes" |Fishery.Sectory.Worker=="Yes" |Veterinarian=="Yes" |Slaughterhouse.Worker=="Yes" |Animal.Caretaker=="Yes","Yes","No"
  ))

dengue_merge <- read_rds("data/generated_data/dengue_merge.rds") %>%
  select(patient_id=lepto_id,dengue_status)

clean_linelist <- NBS_linelist %>% as.data.frame() %>%
  filter(Investigation.Case.Status %in% c("Suspect","Probable","Confirmed")) %>%
  #imputed onset date
  mutate(onset_date_imputed=case_when(Event.Date.Type=="Illness Onset Date" ~ as.character(Event.Date),
                                      Event.Date.Type=="Specimen Collection Date of Earliest Associated Lab" ~  as.character(Event.Date-3),
                                      Event.Date.Type=="Hospitalization Admit Date" ~  as.character(Event.Date-3),
                                      Event.Date.Type=="Date of Report" ~  as.character(Event.Date-6), # formerly 7
                                      Event.Date.Type=="Date of Diagnosis" ~  as.character(Event.Date-3), # formerly 0
                                      Event.Date.Type=="Earliest date received by the county/local health department" ~  as.character(Event.Date-9),# formerly 0
                                      TRUE ~ as.character(Event.Date)
  )) %>%
  mutate(onset_date_imputed=as.Date(onset_date_imputed)) %>%
  mutate(case_week=floor_date(onset_date_imputed,"weeks")) %>%
  mutate(case_month=floor_date(onset_date_imputed,"months"))  %>%
  mutate(post_fiona= onset_date_imputed>date_fiona) %>%
  mutate(Period_simple=ifelse(post_fiona,
                              "After Fiona", "Before Fiona"
  ))       %>%
  mutate(Period_simple=fct_rev(Period_simple))%>%
  #location
  mutate(region=str_remove(Jurisdiction.Name," Region")) %>%
  mutate(region= case_when(
    region == "Bayamon" ~"Bayam\u00f3n", # Bayamón
    region %in% c("Aguadilla","Mayaguez") ~ "Mayag\u00fcez", # Mayagüez
    region =="Metro" ~ "Metropolitana",
    region == "Could Not Be Determined" ~ "Missing",
    TRUE ~ region
  ))%>%
  mutate(municipio=str_extract(Puerto.Rico.Municipality,"(?<=Region \\- )(.*)"))%>%
  mutate(municipio=case_when(
    municipio=="San Sebastian" ~"San Sebasti\u00e1n", # San Sebastián
    municipio== "Mayaguez"~"Mayag\u00fcez",   # Mayagüez
    municipio=="Villaba" ~ "Villalba",
    TRUE ~ municipio
  )) %>%
  #get hospital name
  # mutate(hosp_name =  sapply(Hospital.System.Details, FUN=extract_hospital_name))  %>%
  mutate(age_year=ifelse(Patient.Age.Reported<0 | Patient.Age.Reported>120,
                         NA,Patient.Age.Reported)) %>%
  #deal with patient age
  mutate(age_year=case_when(
    Patient.Age.Reported.Units =="Years" ~  age_year,
    Patient.Age.Reported.Units =="Months" ~  age_year/12,
    Patient.Age.Reported.Units =="Days" ~  age_year/365,
    is.na(Patient.Age.Reported.Units) ~ age_year
  )) %>%
  mutate(age_year=floor(age_year)) %>%
  select(patient_id=Patient.Local.ID,
         investigation_id=Investigation.Local.ID,
         Investigation.Case.Status,
         #name
         Patient.First.Name,Patient.Middle.Name, Patient.Last.Name,

         #demographics
         sex=Patient.Current.Sex,
         age_year,

         #time variables
         onset_date_imputed, Event.Date, Event.Date.Type,
         case_week,
         case_month,
         Period_simple,
         Investigation.Create.Date,
         Investigation.Last.Updated.Date,
         Investigation.Illness.Onset.Date,
         Investigation.Hospital.Admission.Date,
         Investigation.Date.of.Death,
         Patient.Date.of.Birth,
         Treatment.Start.Date,

         #location variables
         region, original_region=Jurisdiction.Name,
         municipio,original_municipio=Puerto.Rico.Municipality,
         Patient.Street.Address.1,
         Patient.Street.Address.2,
         Patient.City,
         Patient.Zip#,
         #hosp_name

  ) %>%
  #bring in symptom results 
  left_join(symptom_df, by=c("patient_id", "investigation_id")) %>%
  #bring in exposure data
  left_join(exposure_df, by=c("patient_id", "investigation_id")) %>%
  #bring in lab results
  left_join(patient_results, by="patient_id")%>%
  mutate(IgM=ifelse(is.na(IgM),"Missing",IgM),
         PCR=ifelse(is.na(PCR),"Missing",PCR),
         Tested= !((IgM=="Missing") & (PCR=="Missing")),
         `Fully Tested`= ((IgM!="Missing") & (PCR!="Missing")),
         `Testing Category` = case_when(
           IgM %in% c("Positive","Negative")  & PCR %in% c("Positive","Negative") ~ "Fully Tested",
           IgM %in% c("Positive","Negative")  & PCR =="Missing" ~ "IgM Only",
           PCR %in% c("Positive","Negative")  & IgM =="Missing" ~ "PCR Only",
           IgM =="Missing"  & PCR =="Missing" ~ "No testing")
  ) %>%
  mutate(`Testing Category`=factor(`Testing Category`,levels=c("Fully Tested","PCR Only","IgM Only","No testing"))) %>%
  mutate(
    new_status = ifelse(IgM=="Positive","Probable","Suspect"),
    new_status = ifelse(PCR=="Positive","Confirmed",new_status)
  )%>%
  #add geocoding
  # left_join(geocode_cases, by = c("patient_id", "investigation_id")) %>%
  # mutate(geocoded= !(is.na(Latitude) | is.na(Longitude))) %>%
  #add dengue merge results
  left_join(dengue_merge, by="patient_id")




## clean files
write_rds(clean_labs,"data/generated_data/cleaned_data/clean_labs.rds")
write_rds(clean_linelist,"data/generated_data/cleaned_data/clean_linelist.rds")



