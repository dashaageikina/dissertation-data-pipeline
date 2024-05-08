#cleaning sitreps for merging - need data on participating agencies and GACC priority
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data/build_wildfire_reports"

sitreps <- read.csv(paste0(path,"/input/raw/ics209-plus/ics209-plus_sitreps_1999to2014.csv"))
sitreps <- sitreps[sitreps$INCTYP_ABBREVIATION=="WF",]
sitreps2 <- filter(sitreps,is.na(TOTAL_AERIAL))
#substitute empty string with NA
sitreps[sitreps==""] <- NA
sitreps <- sitreps[!is.na(sitreps$DISCOVERY_DATE),]
sitreps <- sitreps[!is.na(sitreps$REPORT_TO_DATE),]
sitreps <- sitreps[!is.na(sitreps$REPORT_DAY_SPAN),]
#creating a variable for the start of the report
#!!!!!!! it might not be correct, will chat with Lise
sitreps$REPORT_TO_DATE <- substr(sitreps$REPORT_TO_DATE,1,10)
sitreps$REPORT_TO_DATE <- as.Date(sitreps$REPORT_TO_DATE,format="%Y-%m-%d")
sitreps$REPORT_START_DATE <- sitreps$REPORT_TO_DATE-sitreps$REPORT_DAY_SPAN

#removing variables that I do not need
sitreps <- subset(sitreps,select=-c(X,EDAMAGE,EST_IM_COST_TO_DATE,EXPECTED_CONTAINMENT_DATE,
                                    GROWTH_POTENTIAL,LL_CONFIDENCE,
                                    LL_UPDATE,POO_LD_PM,POO_LD_QTR_QTR_SEC,
                                    POO_LD_QTR_SEC,POO_LD_RGE,POO_LD_SEC,POO_LD_TWP,
                                    PROJ_INC_AREA_UOM,PROJ_INCIDENT_AREA,PROJ_SIG_RES_DEMOB_START_DATE,
                                    PROJECTED_ACTIVITY_NARR,PROJECTED_FINAL_IM_COST,
                                    RES_BENEFITS,RISK_ASSESSMENT))
sitreps <- sitreps[!duplicated(sitreps),]


#now adding the incident-level variable for the jurisdictions involved in wildfire
incident_jurisdictions <- subset(sitreps,select=c(INCIDENT_ID,ADDTNL_COOP_ASSIST_ORG_NARR))
#there is also INCIDENT_JURISDICTION, but there are only 9 non-empty data points for it
#i will keep this in mind but putting aside for now
incident_jurisdictions <- incident_jurisdictions[!duplicated(incident_jurisdictions),]
incident_jurisdictions <- incident_jurisdictions[!is.na(incident_jurisdictions$ADDTNL_COOP_ASSIST_ORG_NARR),]

#combine all assisting agencies for each incident
for (inc_id in unique(incident_jurisdictions$INCIDENT_ID)) {
  
  incident <- incident_jurisdictions[incident_jurisdictions$INCIDENT_ID==inc_id,]
  
  if (nrow(incident)==1) {
    
    next
    
  } else {
    
    common_expression <- c()
    
    for (each_row in 1:nrow(incident)) {
      
      expression <- incident_jurisdictions$ADDTNL_COOP_ASSIST_ORG_NARR[each_row]
      expression <- gsub(pattern=".",replacement="",x=expression,fixed=TRUE)
      expression <- unlist(strsplit(expression,"  ",fixed=TRUE))

      common_expression <- unique(c(common_expression,expression))
      common_expression <- paste(common_expression,collapse="  ")
      
    }
  }
  
  incident_jurisdictions$ADDTNL_COOP_ASSIST_ORG_NARR[incident_jurisdictions$INCIDENT_ID==inc_id] <- common_expression
  
}

incident_jurisdictions <- incident_jurisdictions[!duplicated(incident_jurisdictions),]
names(incident_jurisdictions)[names(incident_jurisdictions)=="ADDTNL_COOP_ASSIST_ORG_NARR"] <- "addtnl_coop_orgz"

sitreps <- merge(sitreps,incident_jurisdictions,all.x = T)

#now creating incident-level GACC priority variables
incident_gacc <- subset(sitreps,select=c(INCIDENT_ID,GACC_PRIORITY))
incident_gacc$GACC_PRIORITY <- as.numeric(incident_gacc$GACC_PRIORITY)
#adding mean, max and min gacc priority for each incident
incident_gacc <- incident_gacc %>%
  group_by(INCIDENT_ID) %>%
  mutate(
    gacc_priority_max = max(GACC_PRIORITY, na.rm = T),
    gacc_priority_min = min(GACC_PRIORITY, na.rm = T),
    gacc_priority_mean = mean(GACC_PRIORITY, na.rm = T)
  ) %>%
  arrange(INCIDENT_ID)
#some cleaning
incident_gacc <- subset(incident_gacc,select=-GACC_PRIORITY)
incident_gacc <- incident_gacc[!duplicated(incident_gacc),]

#figuring out what REPORT_TO_DATE and REPORT_DAY_SPAN are about

sitreps <- merge(sitreps,incident_gacc,all.x = T)
#creating a sitrep id based on the incident id and the dates
sitreps <- sitreps %>%                                       
  group_by(INCIDENT_ID,REPORT_TO_DATE,REPORT_DAY_SPAN) %>%
  dplyr::mutate(SITREP_ID = cur_group_id())
#if sitrep ids are repeated, remove those - unclear what to do with them
dups <- sitreps$SITREP_ID[duplicated(sitreps$SITREP_ID)]
sitreps <- sitreps[!(sitreps$SITREP_ID %in% dups),]

names(sitreps)[names(sitreps)=="INCIDENT_ID"] <- "incident_id"

write.csv(sitreps,paste0(path,"/output/cleaned_sitreps.csv"))


