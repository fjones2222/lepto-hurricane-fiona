
source("code/R/functions.R")

options(java.parameters = "-Xmx4g")  # Adjust the memory limit as needed



NBS_labs <- xlsx::read.xlsx("data/NBS_exports/20231018/PR Leptospirosis Lab Reports 2021-2023.xlsx",
                            sheetIndex = 1,
                            password = "leptonbs"
)

### first the test name

test_names <- read_rds("data/generated_data/lab_assignment/test_names.rds") 



test_table <- NBS_labs %>% 
  count(Resulted.Test.Name,Alternate.Lab.Test.Name,Text.Result) %>%
  arrange(-n) %>%
  anti_join(test_names,by = c("Resulted.Test.Name", "Alternate.Lab.Test.Name", "Text.Result"))

this_test <- test_table[1,]
this_test

# left_join(this_test,NBS_labs) %>% View()

assigned_test<- "IgM"       #say either IgM, IgG, PCR, MAT, Unknown
test_names <- bind_rows(test_names,
                              this_test %>% mutate(clean_test=assigned_test)
)



test_names <- write_rds(test_names,"data/generated_data/lab_assignment/test_names.rds")



## next the test result

result_names <- read_rds("data/generated_data/lab_assignment/result_names.rds") %>%
                mutate(Numeric.Results=as.character(Numeric.Results))


result_table <- NBS_labs %>%
                  left_join(test_names, by = c("Resulted.Test.Name", "Alternate.Lab.Test.Name", "Text.Result")) %>%
                  mutate(limited_comment=substr(Result.Comments,1,20))%>%
                  count(clean_test,Coded.Result,Text.Result,Numeric.Results,Test.Result.Code,limited_comment) %>%
                  arrange(-n) %>%
                  anti_join(result_names,
                            by = c("clean_test", "Coded.Result", "Text.Result", "Numeric.Results", "Test.Result.Code", "limited_comment")
                            )

this_result <- result_table[1,]
this_result

# this_result %>% left_join(NBS_labs) %>% View()

assign_result <- "Positive" ### Positive, Negative, Missing, Inconclusive

result_names <- bind_rows(result_names,
                          this_result %>% mutate(clean_result=assign_result)
)

write_rds(result_names,"data/generated_data/lab_assignment/result_names.rds")



### the lab name

lab_names <- NBS_labs %>%
  count(Reporting.Facility) %>%
  arrange(-n) %>%
  mutate(REPORTING.FACILITY=toupper(Reporting.Facility)) %>%
  mutate(clean_lab=case_when(
        REPORTING.FACILITY %in% c("PUERTO RICO PUBLIC HEALTH LABORATORY",
                                  "DEPARTAMENTO DE SALUD",
                                  "LABORATORIO DE SALUD PUBLICA DE PUERTO RICO",
                                  "DEPARTMENT OF HEALTH  PUERTO RICO PUBLIC HEALTH LA",
                                  "DEPARTAMENTO DE SALUD DE PUERTO RICO"
                                  ) ~ "PRDH Laboratory",
        str_detect(REPORTING.FACILITY,"CDC") ~ "CDC",
        str_detect(REPORTING.FACILITY,"TOLEDO") ~ "Toledo",
        REPORTING.FACILITY %in% c("IMMUNO REFERENCE LAB","INMUNO REFERENCE LAB") ~ "Immuno Reference Lab",
        str_detect(REPORTING.FACILITY,"QUEST") ~ "Quest",
        str_detect(REPORTING.FACILITY,"HOSP|MEDICAL CENTER|MMCN") ~ "Hospital / Medical Center",
        str_detect(REPORTING.FACILITY, "BORINQUEN") ~ "Borinquen",
        TRUE ~ "Missing"
    
    
  ))


write_rds(lab_names,"data/generated_data/lab_assignment/lab_names.rds")

### specimen type

specimen_names <- read_rds("data/generated_data/lab_assignment/specimen_names.rds")


specimen_table <- NBS_labs %>%
  mutate(limited_comment=substr(Result.Comments,1,20))%>%
  count(Specimen.Src.Desc,Specimen.Src.Cd,limited_comment) %>%
  arrange(-n) %>%
  anti_join(specimen_names,
            by = c("Specimen.Src.Desc", "Specimen.Src.Cd","limited_comment")
  )

this_specimen <- specimen_table[1,]
this_specimen

assign_specimen <- "Missing" ### Serum, Blood venous, Urine, cord blood, Missing, CSF, Nasal, Fecal, Muscle, Whole blood,Saliva

specimen_names <- bind_rows(specimen_names,
                            this_specimen %>% mutate(clean_specimen=assign_specimen)
) 

specimen_names %>%
  distinct() %>%
write_rds("data/generated_data/lab_assignment/specimen_names.rds")




##### check with prior methods

clean_labs <- NBS_labs %>%
  mutate(RESULTED.TEST.NAME=toupper(Resulted.Test.Name)) %>%
  # #limit to IgM and PCR tests
  # filter(str_detect(RESULTED.TEST.NAME,"IGM|PCR|DNA|MAT") |
  #          str_detect(Text.Result,"IGM|PCR|DNA|MAT") 
  # ) %>%
  # #figure out what type of test was done
  mutate(test_name=case_when(
    str_detect(RESULTED.TEST.NAME,"IGM") ~ "IgM",
    str_detect(RESULTED.TEST.NAME,"DNA|PCR") ~ "PCR",
    str_detect(RESULTED.TEST.NAME,"MAT") ~ "MAT"
  )) %>%
  mutate(test_name=case_when(
    str_detect(Text.Result,"IGM") & is.na(test_name) ~ "IgM",
    str_detect(Text.Result,"DNA|PCR") & is.na(test_name)~ "PCR",
    str_detect(Text.Result,"MAT") & is.na(test_name) ~ "MAT",
    TRUE ~ test_name
    
  )) %>%
  #figure out the result
  mutate(test_result=Coded.Result) %>%
  mutate(test_result=ifelse(is.na(test_result),Text.Result,test_result))%>%
  mutate(test_result=toupper(test_result)) %>%
  mutate(test_result=str_remove(test_result,"MAT- |MAT-")) %>%
  mutate(test_result=case_when(
    test_result %in% c("BORDERLINE","POSITIVE","REACTIVE","DETECTED",
                       "BORDELINE REACTIVE","BORDERLINE NORMAL","REACTIVE (QUALIFIER VALUE)"
    ) ~ "Positive",
    test_result %in% c("NON-REACTIVE","NOT DETECTED","NEGATIVE","INIT NON-REACTIVE",
                       "NONREACTIVE"
    ) ~ "Negative",
    test_name == "MAT" ~ test_result,
    TRUE ~  "Missing"
  )) %>%
  mutate(limited_comment=substr(Result.Comments,1,20))%>%
  left_join(select(test_names,-n),by=c("Resulted.Test.Name","Alternate.Lab.Test.Name","Text.Result")) %>%
  left_join(select(result_names,-n))


table(clean_labs$clean_test,
      clean_labs$test_name,
      useNA = "always")

table(clean_labs$clean_result,
      clean_labs$test_result,
      useNA = "always")




