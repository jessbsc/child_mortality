library(tibble)
library(EventDetectR)
library(magrittr)
micro_data_csv<- read.csv(file = './data/microdata_mortality.csv')

micro_data_csv$LOCAL_CODE <- as.integer(as.character(micro_data_csv$LOCAL_CODE))

micro_data_csv$DT_DEATH = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH , micro_data_csv$DAY_DEATH, sep = "-")
micro_data_csv$DT_DEATH <- as.Date(micro_data_csv$DT_DEATH)
micro_data_csv$week_num = strftime(micro_data_csv$DT_DEATH, format = "%V")
micro_data_csv$year_week = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$week_num, sep = "-")
micro_data_csv$year_month = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH, sep = "-")

micro_data_csv$year_month_date = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH, "01", sep = "-")
micro_data_csv$year_month_date <- as.Date(micro_data_csv$year_month_date)

estratificacao <- read.csv(file = './data/Cidades_estratificadas_por_grupo.csv')

estratificacao = subset(estratificacao, select = c(LOCAL_CODE, LOCAL_NAME, grupo) )

mortality_estratificado <- merge(x=micro_data_csv, y=estratificacao, 
                                 by.x=c("LOCAL_CODE"), 
                                 by.y=c("LOCAL_CODE"))

library(dplyr)

mortality_grupo_by_month <- mortality_estratificado %>% group_by(LOCAL_CODE, year_month_date, LOCAL_NAME, grupo)

mortality_grupo_by_month <- mortality_grupo_by_month %>% summarise(
  LOCAL_CODE = max(LOCAL_CODE),
  LOCAL_NAME = max(LOCAL_NAME),
  year_month_date = max(year_month_date),
  grupo = max(grupo),
  FL_DEATH_AVOIDABLE = sum(FL_DEATH_AVOIDABLE),
  FL_DEATH_ILLDEFINED = sum(FL_DEATH_ILLDEFINED),
  FL_DEATH_OTHERS = sum(FL_DEATH_OTHERS),
  FL_DEATH_IMMUNOPREV = sum(FL_DEATH_IMMUNOPREV), #morte rara
  FL_DEATH_CAREPREV_ALL = sum(FL_DEATH_CAREPREV_ALL),
  FL_DEATH_CAREPREV_PREG = sum(FL_DEATH_CAREPREV_PREG),
  FL_DEATH_CAREPREV_DELIV = sum(FL_DEATH_CAREPREV_DELIV),
  FL_DEATH_CAREPREV_CHILD = sum(FL_DEATH_CAREPREV_CHILD),
  FL_DEATH_DTPREV = sum(FL_DEATH_DTPREV),
  FL_DEATH_PROMOPREV = sum(FL_DEATH_PROMOPREV),
  FL_DEATH_PNEUMONIA = sum(FL_DEATH_PNEUMONIA),
  FL_DEATH_IID = sum(FL_DEATH_IID),
  FL_DEATH_MALNUTRITION = sum(FL_DEATH_MALNUTRITION),
  FL_DEATH_COVID19 = sum(FL_DEATH_COVID19)
)


mortality_grupo_by_month$all_death <- mortality_grupo_by_month$FL_DEATH_AVOIDABLE +  mortality_grupo_by_month$FL_DEATH_ILLDEFINED +  mortality_grupo_by_month$FL_DEATH_OTHERS

#write.csv(mortality_grupo_by_month, "mortality_grupo_by_month.csv", row.names=TRUE)

######################################################################################################################

#microdata_nascimento<-load("sinasc_clean_microdata_cefet.RData") 
#microdata_nascimento_df <-data.frame(sinasc_clean)

#microdata_nascimento_df$LOCAL_CODE <- as.integer(as.character(microdata_nascimento_df$LOCAL_CODE))
#write.csv(microdata_nascimento_df, "nascimentos_microdata.csv", row.names=TRUE)

########################################################################################################################

micro_data_nascimento_csv<- read.csv(file = './data/nascimentos_microdata.csv')
micro_data_nascimento_csv$year_month_date = paste(micro_data_nascimento_csv$YEAR_BIRTH, micro_data_nascimento_csv$MONTH_BIRTH, "01", sep = "-")
micro_data_nascimento_csv$year_month_date <- as.Date(micro_data_nascimento_csv$year_month_date)

nascimento_grupo_by_month <- micro_data_nascimento_csv %>% group_by(LOCAL_CODE, year_month_date)

nascimento_grupo_by_month <- nascimento_grupo_by_month %>% summarise(
  LOCAL_CODE = max(LOCAL_CODE),
  year_month_date = max(year_month_date),
  FL_BIRTH = sum(FL_BIRTH)
) 

library(dplyr)
mortality_nascimento_estratificado <-  full_join(x=nascimento_grupo_by_month, y=mortality_grupo_by_month, 
                                 by.x=c("LOCAL_CODE", "year_month_date"), 
                                 by.y=c("LOCAL_CODE", "year_month_date"))

write.csv(mortality_nascimento_estratificado, "mortality_and_nasc_grupo_by_month.csv", row.names=TRUE)


data_test <- filter(mortality_nascimento_estratificado, LOCAL_CODE == 110028)

