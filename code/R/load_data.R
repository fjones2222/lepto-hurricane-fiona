source("code/R/functions.R")


#original dataset
NBS_linelist <- read_rds("data/generated_data/cleaned_data/NBS_linelist.rds")
NBS_labs <- read_rds("data/generated_data/cleaned_data/NBS_labs.rds")

weeks <- floor_date(as.Date("2022-01-02"),"weeks")+7*(0:51)


clean_linelist_original <-  read_rds("data/generated_data/cleaned_data/clean_linelist.rds")
  
clean_linelist <-  clean_linelist_original%>%
  filter(onset_date_imputed>=min(weeks),
         onset_date_imputed<=(max(weeks)+6)
  )


clean_labs_original <- read_rds("data/generated_data/cleaned_data/clean_labs.rds") 

clean_labs <- clean_labs_original %>%
  filter(patient_id %in% clean_linelist$patient_id)

pop20 <- read_rds("data/generated_data/census2020_municipio.rds") %>%
  mutate(municipio=str_remove(NAME," Municipio, Puerto Rico"))

PR_rectangle <- data.frame(xmin= -67.3, ymin= 17.85 ,xmax= -65.2 ,ymax= 18.55)
box <- st_bbox(c(xmin= PR_rectangle$xmin, ymin= PR_rectangle$ymin ,xmax= PR_rectangle$xmax ,ymax= PR_rectangle$ymax), crs = "NAD83") %>%
  st_as_sfc() %>%
  st_as_sf()

pop20 <- st_join(pop20,box,left=FALSE)




