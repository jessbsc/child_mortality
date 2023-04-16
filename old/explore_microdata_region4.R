#####
# Load the Data

#mortality_micro_vector <- load('/data/mortality-neo/sim_clean_microdata_cefet.RData')
#mortality_micro <- as.data.frame(t(mortality_micro_vector[["sim_clean"]]))
#write.csv(sim_clean, "microdata_mortality.csv")

library(tibble)
library(EventDetectR)
library(magrittr)
micro_data_csv<- read.csv(file = 'microdata_mortality.csv')

micro_data_csv$LOCAL_CODE <- as.integer(as.character(micro_data_csv$LOCAL_CODE))

micro_data_csv$DT_DEATH = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH , micro_data_csv$DAY_DEATH, sep = "-")
micro_data_csv$DT_DEATH <- as.Date(micro_data_csv$DT_DEATH)
micro_data_csv$week_num = strftime(micro_data_csv$DT_DEATH, format = "%V")
micro_data_csv$year_week = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$week_num, sep = "-")
micro_data_csv$year_month = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH, sep = "-")

micro_data_csv$year_month_date = paste(micro_data_csv$YEAR_DEATH, micro_data_csv$MONTH_DEATH, "01", sep = "-")
micro_data_csv$year_month_date <- as.Date(micro_data_csv$year_month_date)

#unique(micro_data_csv$week_num)

estratificacao <- read.csv(file = 'Cidades_estratificadas_por_grupo.csv')
estratificacao = subset(estratificacao, select = c(LOCAL_CODE, LOCAL_NAME, grupo) )

mortality_estratificado <- merge(x=micro_data_csv, y=estratificacao, 
                                 by.x=c("LOCAL_CODE"), 
                                 by.y=c("LOCAL_CODE"))

library(dplyr)
mortality_grupo_4 <- filter(mortality_estratificado, grupo == 4)

mortality_grupo_4_by_week <- mortality_grupo_4 %>% group_by(LOCAL_CODE, year_week, LOCAL_NAME)
mortality_grupo_4_by_month <- mortality_grupo_4 %>% group_by(LOCAL_CODE, year_month_date, LOCAL_NAME)

mortality_grupo_4_by_week <- mortality_grupo_4_by_week %>% summarise(
  LOCAL_CODE = max(LOCAL_CODE),
  LOCAL_NAME = max(LOCAL_NAME),
  year_month_date = max(year_month_date),
  FL_DEATH_AVOIDABLE = sum(FL_DEATH_AVOIDABLE),
  FL_DEATH_ILLDEFINED = sum(FL_DEATH_ILLDEFINED),
  FL_DEATH_OTHERS = sum(FL_DEATH_OTHERS),
  FL_DEATH_IMMUNOPREV = sum(FL_DEATH_IMMUNOPREV),
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
#unique(mortality_grupo_4_by_week)
mortality_grupo_4_by_month <- mortality_grupo_4_by_month %>% summarise(
  LOCAL_CODE = max(LOCAL_CODE),
  LOCAL_NAME = max(LOCAL_NAME),
  year_month_date = max(year_month_date),
  FL_DEATH_AVOIDABLE = sum(FL_DEATH_AVOIDABLE),
  FL_DEATH_ILLDEFINED = sum(FL_DEATH_ILLDEFINED),
  FL_DEATH_OTHERS = sum(FL_DEATH_OTHERS),
  FL_DEATH_IMMUNOPREV = sum(FL_DEATH_IMMUNOPREV),
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

source( "https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R" )

library(ggplot2)
# plot grafic
ggplot(mortality_grupo_4_by_month, aes(x = year_month, y = FL_DEATH_IMMUNOPREV)) +
  geom_line()

SALVADOR_BY_MONTH <- filter(mortality_grupo_4_by_month, LOCAL_CODE == 292740)

train <- SALVADOR_BY_MONTH
ARIMA <- function(data) forecast::auto.arima(data) 
garch11 <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                               mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                               distribution.model = "norm")

unique(mortality_grupo_4_by_month$LOCAL_NAME)
       
for (j in unique(mortality_grupo_4_by_month$LOCAL_NAME))
{
  library(dplyr)
  CITY_BY_MONTH <- filter(mortality_grupo_4_by_month, LOCAL_NAME == "Fortaleza")
  train <- CITY_BY_MONTH
  for ( i in 4:ncol(train)) {
    #print(j)
    print(i)
    test <- subset(train, select=c(year_month_date, i))
    
    events_scp <- evtdet.seminalChangePoint(test, w=10, na.action=na.omit)
    print(evtplot(test,events_scp, mark.cp=TRUE) + ggtitle( paste(colnames(train[i]),"- ",j, "- Change Point")))
    
    #events_cf <- evtdet.changeFinder(test, mdl=ARIMA, m=10)
    #print(evtplot(test, events_cf, mark.cp=TRUE))
    
    events_ed <- evtdet.eventdetect(test)
    
    #events_an <- evtdet.outliers(test, alpha=1.5)
    #print(evtplot(test,events_an) + ggtitle( paste(colnames(train[i]),"-","Salvador", "- Outliers")))
    
    #events_garch_volatility_outlier <- evtdet.garch_volatility_outlier(test,spec=garch11,alpha=1.5)
    #print(evtplot(test, events_garch_volatility_outlier ) + ggtitle( paste(colnames(train[i]),"-",j , "- Volatily outliers")) )
    
  } 
}


city_local_BY_MONTH <- filter(mortality_grupo_4_by_month, LOCAL_CODE == 150140)

train <- city_local_BY_MONTH
test <- subset(train, select=c(year_month_date,FL_DEATH_PNEUMONIA))
               

events_scp <- evtdet.seminalChangePoint(test, w=10, na.action=na.omit)
print(evtplot(test,events_scp, mark.cp=TRUE) + ggtitle( paste("FL_DEATH_PNEUMONIA","- ","Fortaleza", "- Change Point")))
#events_a <- evtdet.anomalize(test,na.action=na.omit)
#events_ed <- evtdet.eventdetect(test)

events_cf <- evtdet.changeFinder(test,mdl=ARIMA,m=10,na.action=na.omit)
print(evtplot(test,events_cf, mark.cp=TRUE)+ ggtitle( paste("FL_DEATH_PNEUMONIA","- ","Fortaleza", "- changeFinder")))

events_an <- evtdet.outliers(test, alpha=1.5)
print(evtplot(test,events_an) + ggtitle( paste("FL_DEATH_PNEUMONIA","- ","Fortaleza", "- outliers")))


#Detect
events_garch_volatility_outlier <- evtdet.garch_volatility_outlier(test,spec=garch11,alpha=1.5)
print(evtplot(test,events_garch_volatility_outlier ) + ggtitle( paste("FL_DEATH_ILLDEFINED","- ","Fortaleza", "- Change Point")))
