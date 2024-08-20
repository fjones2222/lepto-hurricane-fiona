rm(list=ls())
source("code/R/load_data.R")

#bring in covariate data

# CHIRPS: Rainfall Estimates from Rain Gauge and Satellite Observations
# https://www.chc.ucsb.edu/data/chirps


#do this for all the centroids of every municipio
library(tmap)
tmap_mode("view")

sf::st_crop(pop20, xmin= -67.25, ymin= 17 ,xmax= -65 ,ymax= 19) %>%
  st_centroid() %>%
  tm_shape()+
  tm_dots()


dat <- chirps::get_chirps(sf::st_crop(pop20, xmin= -67.25, ymin= 17 ,xmax= -65 ,ymax= 19) %>%
                            st_centroid(),
                  dates = c("2022-09-01","2022-09-30"),
                  server = "ClimateSERV")
write_rds(dat,"data/chirps/202209_chirps_20231207.rds")

dat <- read_rds("data/chirps/202209_chirps_20231207.rds")

summarized <- dat %>%
  filter(date<=as.Date("2022-09-20"),
         date>=as.Date("2022-09-16")) %>%
  filter(chirps>0)%>%
  group_by(id) %>%
  summarize(total=sum(chirps)/10/2.54,
            n=n(),
            avgd=total/n*5
  ) 

chrps_county <- pop20 %>%
  mutate(id=1:n()) %>%
  left_join(summarized) %>%
  as.data.frame()%>%
  select(municipio,rainfall=total) 

#find the SVI for the municipios
svi_county <- read_sf("data/SVI/PuertoRico_COUNTY") %>%
  as.data.frame()%>%
  rename(municipio=COUNTY) %>%
  select(municipio,RPL_THEMES)

#Percentile ranking values range from 0 to 1, with higher values indicating greater vulnerability.


#find the municipios with reports of floodinng
# NOAA Storm Events Database
# https://www.ncdc.noaa.gov/stormevents/
event_data <- read_csv("data/hydrological_data/event_data/storm_data_search_results.csv")

event_county <- event_data %>% 
  filter(BEGIN_DATE %in% c("09/17/2022","09/18/2022","09/19/2022"))%>%
  filter(str_detect(EVENT_TYPE,"Flood")) %>%
  group_by(BEGIN_LOCATION) %>%
  summarize(events=n())

# count the numbers in each municipio after fiona

cases_by_muni<- clean_linelist %>%
  filter(new_status %in% c("Confirmed","Probable")) %>%
  group_by(municipio,Period_simple) %>%
  summarise(cases=n())%>%
  ungroup()%>%
  spread(Period_simple,cases) %>%
  full_join( as.data.frame(pop20) %>%
               select(municipio,value)
               )%>%
  gather(Period_simple,cases,-c(municipio,value)) %>%
  select(Period_simple,municipio,cases,value)%>%
  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  mutate(inc=ifelse(Period_simple=="Before Fiona",
                    cases/(value*37)*52*10000,cases/(value*15)*52*10000)
  )





library(tmap)

tmap_mode("view")

pop20 %>%
  tm_shape()+
  tm_polygons(alpha=0.1)


cov_by_muni <- cases_by_muni %>%
  select(Period_simple,municipio,inc)%>%
  spread(Period_simple,inc)%>%
  mutate(inc_diff=`After Fiona`-`Before Fiona`) %>%
  left_join(svi_county) %>%
  mutate(BEGIN_LOCATION=toupper(scraEP::unaccent(municipio))) %>%
  left_join(event_county) %>%
  mutate(events=ifelse(is.na(events),0,events)) %>%
  left_join(chrps_county) %>%
  mutate(
        svi_0.25 = RPL_THEMES>0.25,
        events_0 = events>0,
        rf_11=rainfall>11
             )



spearman_df_diff <-data.frame(
  measure=c("Social vulnerability Index","Number of events reported by NOAA","Inches of Rainfall"),
  value = c(0.5,2,12),
  inc_diff=c(10,10,10),
          rho = c(
            cor.test(x=cov_by_muni$inc_diff, y=cov_by_muni$RPL_THEMES, method = 'spearman')$estimate,
            cor.test(x=cov_by_muni$inc_diff, y=cov_by_muni$events, method = 'spearman')$estimate ,
            cor.test(x=cov_by_muni$inc_diff, y=cov_by_muni$rainfall, method = 'spearman')$estimate
          ),
          p_value = c(
            cor.test(x=cov_by_muni$inc_diff, y=cov_by_muni$RPL_THEMES, method = 'spearman')$p.value,
            cor.test(x=cov_by_muni$inc_diff, y=cov_by_muni$events, method = 'spearman')$p.value,
            cor.test(x=cov_by_muni$inc_diff, y=cov_by_muni$rainfall, method = 'spearman')$p.value
          )
)  %>%
  mutate(rho=round(rho,1))



cov_by_muni %>%
  #mutate(rainfall=rainfall*RPL_THEMES)%>%
  select(municipio,inc_diff,`Social vulnerability Index`=RPL_THEMES,
         `Number of events reported by NOAA`=events,
         `Inches of Rainfall`=rainfall) %>%
  gather(measure,value,-c(municipio,inc_diff) ) %>%
  ggplot(aes(x=value,y=inc_diff))+
  geom_point(alpha=0.5)+
  geom_text(data=spearman_df_diff,
            aes(label=rho),col="blue"
            )+
  facet_wrap(.~measure,scales = "free")+
  # geom_smooth(se=FALSE, method="loess")+
  ylab("Difference in cases per 1000 population per year")+
  xlab("Measurement")+
  theme_cowplot()




cov_by_muni %>%
  #mutate(rainfall=rainfall*RPL_THEMES)%>%
  select(municipio,`After Fiona`,`Social vulnerability Index`=RPL_THEMES,
         `Number of events reported by NOAA`=events,
         `Inches of Rainfall`=rainfall) %>%
  gather(measure,value,-c(municipio,`After Fiona`) ) %>%
  ggplot(aes(x=value,y=`After Fiona`))+
  geom_point(alpha=0.5)+
  facet_wrap(.~measure,scales = "free")+
  geom_smooth(se=FALSE)+
  ylab("Cases per 1000 population per year")+
  xlab("Measurement")



cor.test(x=cov_by_muni$`After Fiona`, y=cov_by_muni$RPL_THEMES, method = 'spearman')
cor.test(x=cov_by_muni$`After Fiona`, y=cov_by_muni$events, method = 'spearman')
cor.test(x=cov_by_muni$`After Fiona`, y=cov_by_muni$rainfall, method = 'spearman')


  
# cov_by_group<- cases_by_muni %>%
#   select(-inc)%>%
#   left_join(svi_county) %>%
#   mutate(BEGIN_LOCATION=toupper(scraEP::unaccent(municipio))) %>%
#   left_join(event_county) %>%
#   mutate(events=ifelse(is.na(events),0,events)) %>%
#   left_join(chrps_county) %>%
#   mutate(
#     svi_0.5 = RPL_THEMES>0.5,
#     events_0 = events>0,
#     rf_11=rainfall>11
#   )
# 
# cov_by_group %>%
#   group_by(Period_simple,svi_0.5) %>%
#   summarize(cases=sum(cases),value=sum(value))%>%
#   mutate(inc=ifelse(Period_simple=="Before Fiona",
#                     cases/(value*37)*52*10000,cases/(value*15)*52*10000)
#   ) %>%
#   select(Period_simple,svi_0.5,inc) %>%
#   spread(Period_simple,inc)%>%
#   mutate(inc_diff=`After Fiona`-`Before Fiona`) 
#   
# cov_by_group %>%
#   group_by(Period_simple,events_0) %>%
#   summarize(cases=sum(cases),value=sum(value))%>%
#   mutate(inc=ifelse(Period_simple=="Before Fiona",
#                     cases/(value*37)*52*10000,cases/(value*15)*52*10000)
#   ) %>%
#   select(Period_simple,events_0,inc) %>%
#   spread(Period_simple,inc)%>%
#   mutate(inc_diff=`After Fiona`-`Before Fiona`) 
# 
# cov_by_group %>%
#   group_by(Period_simple,rf_11) %>%
#   summarize(cases=sum(cases),value=sum(value))%>%
#   mutate(inc=ifelse(Period_simple=="Before Fiona",
#                     cases/(value*37)*52*10000,cases/(value*15)*52*10000)
#   ) %>%
#   select(Period_simple,rf_11,inc) %>%
#   spread(Period_simple,inc)%>%
#   mutate(inc_diff=`After Fiona`-`Before Fiona`) 





#calculate a basic poisson model / negative binomial
# library(brms)
# 
# #need to include offset, maybe need to log it
# fit1 <- brms::brm(data = cases_by_muni, cases ~ 1 + offset(log(value)), family =  brms::zero_inflated_poisson())
# fit2 <- brms::brm(cases ~ RPL_THEMES, data = cases_by_muni, family =  brms::zero_inflated_poisson())
# fit3 <- brms::brm(cases ~ RPL_THEMES, data = cases_by_muni, family =  brms::negbinomial())


