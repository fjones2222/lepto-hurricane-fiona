

#load the raw dengue and leptospirosis datasets
# nbs_complete_raw <-  xlsx::read.xlsx("data/NBS_exports/20230320/PR Leptospirosis Inv 2021-2023 (complete data).xlsx",
#                                      sheetIndex = 1,
#                                      password = "leptonbs"
# ) 

nbs_complete_raw <-  read_rds("data/generated_data/cleaned_data/NBS_linelist.rds")
PADSS_SEDSS <- rio::import("data/DLSDB/20230425_PADSS_SEDSS.xlsx")


#perform the matching algorithm

nbs_complete_raw %>% colnames()

#limit to ID, dob, firt name, last name, region, municipio, date of symptom onset, sex

lepto_limited <- nbs_complete_raw %>%
  mutate(lepto_name= toupper(paste(Patient.First.Name, Patient.Last.Name))) %>%
  select(lepto_id=Patient.Local.ID,BDAY=Patient.Date.of.Birth,lepto_date=Event.Date,
         lepto_name,
         lepto_Region=Jurisdiction.Name,
         lepto_status=Investigation.Case.Status
  ) %>%
  mutate(lepto_date=as.Date(lepto_date)
  ) %>%
  distinct()

dengue_limited <- PADSS_SEDSS %>% 
  mutate(dengue_status=case_when(
    VIRGY_INTERPRETATION_DENV=="P" ~ "Confirmed",
    SERGY_INTERPRETATION_DENV =="P" & 
      VIRGY_INTERPRETATION_DENV!="P" ~ "Probable",
    TRUE ~ "Suspect" )) %>%
  mutate(dengue_status=ifelse(is.na(dengue_status),
                              "Suspect",dengue_status
  )) %>%
  mutate(dengue_name=toupper(paste(GNAME,PNAME,MNAME,SNAME))) %>%    
  select(dengue_id=OBSNUM,
         BDAY , dengue_date=ODATE,
         dengue_name,
         dengue_Region=PRDH_REGION,
         dengue_status
  ) %>%
  mutate(dengue_date=as.Date(dengue_date))%>%
  distinct()



initial_merge <- inner_join(lepto_limited,dengue_limited, by="BDAY") %>%
  mutate(date_difference=abs(lepto_date-dengue_date)) %>%
  filter(date_difference<14) %>%
  mutate(distance=diag(adist(lepto_name,dengue_name)))  %>%
  filter(distance<10) 

merge_duplicates <- filter(initial_merge,
                     lepto_id %in% lepto_id[duplicated(lepto_id)] |
                       dengue_id %in% dengue_id[duplicated(dengue_id)]
                     ) %>%
            select(lepto_id,dengue_id)

cat(nrow(merge_duplicates), "duplicate rows were identified")

#save as an object that can be brought into the cleaning algorithm later
initial_merge %>%
  anti_join(merge_duplicates) %>% 
  write_rds("data/generated_data/dengue_merge.rds")




# filter(merge,lepto_date>as.Date("2022-09-17"),lepto_date<=as.Date("2022-12-31")
# ) %>% 
#   group_by(dengue_date) %>%
#   count() %>%
#   ggplot(aes(x=dengue_date,y=n))+
#   geom_col()
# 
# 
# 
# filter(merge,lepto_date>as.Date("2022-09-17"),lepto_date<=as.Date("2022-12-31")
# ) %>% 
#   group_by(lepto_status,dengue_status,Period) %>% 
#   count() %>%
#   mutate(Period=factor(Period,levels=c("Before Fiona","After Fiona"))) %>%
#   mutate(lepto_status=factor(lepto_status,
#                              levels=c("Confirmed","Probable",
#                                       "Suspect","Not a Case"
#                              ))) %>%
#   ggplot(aes(x=lepto_status,
#              y=dengue_status
#   ))+
#   geom_tile(aes(fill=n))+
#   geom_label(aes(label=n))+
#   facet_wrap(.~Period)






