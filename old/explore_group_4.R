library(tibble)
library(EventDetectR)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")

mortality <- read.csv(file = 'mortality_data.csv')

mortality_c = subset(mortality, select = -c(X, LOCAL_NAME) )

mortality_c$time = paste(mortality_c$YEAR, "-01-01", sep = "")

mortality_c$time <- as.Date(mortality_c$time)

library(dplyr)

mortality_c <- filter(mortality_c, FL1_OUT == 0)


estratificacao <- read.csv(file = 'Cidades_estratificadas_por_grupo.csv')

estratificacao = subset(estratificacao, select = c(LOCAL_CODE, grupo) )

mortality_estratificado <- merge(x=mortality_c,y=estratificacao, 
             by.x=c("LOCAL_CODE"), 
             by.y=c("LOCAL_CODE"))

mortality_grupo_4 <- filter(mortality_estratificado, grupo == 4)


#events_ed <- evtdet.eventdetect(mortality_grupo_4)
#evaluate(events_ed, reference, metric="confusion_matrix")

col_order <- c('time', 'SCOPE', 'LOCAL_CODE' , 'POPULATION', 'POPULATION_BIRTH', 'MHDI',
               'MHDI_E', 'MHDI_L', 'MHDI_I', 'GINI', 'GDP', 'GDP_PC', 'SANITATION',
               'BIRTH_RATE', 'AGE', 'NU_DEATH_ALL', 'NU_DEATH_AVOIDABLE',
               'NU_DEATH_ILLDEFINED', 'NU_DEATH_OTHERS', 'NU_DEATH_IMMUNOPREV',
               'NU_DEATH_CAREPREV_ALL', 'NU_DEATH_CAREPREV_PREG',
               'NU_DEATH_CAREPREV_DELIV', 'NU_DEATH_CAREPREV_CHILD', 'NU_DEATH_DTPREV',
               'NU_DEATH_PROMOPREV', 'NU_DEATH_PNEUMONIA', 'MORTALITY_IID',
               'MORTALITY_MALNUTRITION', 'MORTALITY_COVID19', 'MORTALITY_COVID19B',
               'FL2_OUT', 'grupo')

mortality_salvador <- filter(mortality_grupo_4, LOCAL_CODE == 292740)
data_order <- mortality_salvador[, col_order]

test <- data.frame(x=data_order)


source( "https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R" )

#events_an <- evtdet.an_outliers(test,w=20,alpha=1.5)
library(tseries)
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/nonstationarity_sym.r")

## exemplo
library(EventDetectR)
library(magrittr)

#train <- geccoIC2018Train[16500:18000,]
#test <- subset(train, select=c(Time, Trueb))
#reference <- subset(train, select=c(Time, EVENT))
#events_scp <- evtdet.seminalChangePoint(test,w=100) 
#print(evtplot(test,events_scp, reference)) 


train <- data_order
test <- subset(train, select=c(time, NU_DEATH_ALL))
#reference <- subset(train, select=c(YEAR, EVENT))
events_scp <- evtdet.seminalChangePoint(test) 
print(evtplot(test,events_scp)) 

events_a <- evtdet.anomalize(test)


events_ed <- evtdet.eventdetect(train,windowSize=5)

events_scp <- evtdet.seminalChangePoint(test, w=5,na.action=na.omit)

print(evtplot(test,events_a, reference))


