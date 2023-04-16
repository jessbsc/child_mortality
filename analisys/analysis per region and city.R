library(tibble)
library(EventDetectR)
library(magrittr)
library(dplyr)

micro_data_csv<- read.csv(file = './data/mortality_and_nasc_by_month_v3.csv')

micro_data_csv_clean = subset(micro_data_csv, select = c(LOCAL_CODE, LOCAL_NAME, grupo, year_month_date, all_death, FL_BIRTH) )
micro_data_csv_clean$year_month_date <- as.Date(micro_data_csv_clean$year_month_date)

micro_data_csv_clean <- filter(micro_data_csv_clean, LOCAL_NAME != "")

uf_list<- read.csv(file = './data/cidades_uf.csv')
uf_list$sub_code = substr(uf_list$code,0,6)

# FALTA TRATAR ALGUMAS CIDADES 
micro_data_csv_clean <- merge(x=micro_data_csv_clean, y=uf_list, 
                              by.x=c("LOCAL_NAME", "LOCAL_CODE"), 
                              by.y=c("name", "sub_code"))
#CITY_BY_MONTH <- filter(teste, LOCAL_NAME == "Água Branca")

micro_data_csv_clean$region <- ifelse(micro_data_csv_clean$uf %in%  list('RJ',"SP","MG","ES" ), "Sudeste", ifelse(micro_data_csv_clean$uf %in%  list('RS',"SC","PR"),"Sul",ifelse(micro_data_csv_clean$uf %in%  list('MS',"MT","GO"),"CentroOeste", ifelse(micro_data_csv_clean$uf %in%  list('AM',"AC","RR","RO","AP","TO" ),"Norte","Nordeste")))) 

#write.csv(micro_data_csv_clean, "mortality_and_nasc_by_month_v4.csv", row.names=TRUE)


mortality_grupo <- filter(micro_data_csv_clean, grupo == 3)
mortality_grupo <- filter(mortality_grupo, year_month_date >= '1998/01/01')
mortality_grupo <- filter(mortality_grupo, region == 'Sul')

library(zoo)
##### taxa geral por grupo e por regiao
nasc_and_death_by_month_without_city <- mortality_grupo %>% group_by(year_month_date)

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% summarise(
  year_month_date = max(year_month_date),
  region = max(region),
  FL_BIRTH = sum(FL_BIRTH),
  all_death = sum(all_death))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(cum_death = cumsum(all_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(cum_nasc = cumsum(FL_BIRTH))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(sum_mov_mortality_12 = rollsum(all_death, k=12, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(sum_mov_nasc_12 = rollsum(FL_BIRTH, k=12, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(avg_mov_mortality_12 = rollmean(all_death, k=12, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(avg_mov_nasc_12 = rollmean(FL_BIRTH, k=12, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(sum_mov_mortality_24 = rollsum(all_death, k=24, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(sum_mov_nasc_24 = rollsum(FL_BIRTH, k=24, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(avg_mov_mortality_24 = rollmean(all_death, k=24, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(avg_mov_nasc_24 = rollmean(FL_BIRTH, k=24, fill=NA, align='right'))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city  %>% mutate(desvio_mortalidade_grupo_12 = rollapply(all_death, width=12, fill=NA, align='right', FUN = sd))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city  %>% mutate(desvio_mortalidade_grupo_24 = rollapply(all_death, width=24, fill=NA, align='right', FUN = sd))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city  %>% mutate(desvio_nasc_grupo_12 = rollapply(FL_BIRTH, width=12, fill=NA, align='right', FUN = sd))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city  %>% mutate(desvio_nasc_grupo_24 = rollapply(FL_BIRTH, width=24, fill=NA, align='right', FUN = sd))
nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_mortality_24 + 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_24
nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_mortality_24 - 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_24
nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_mortality_12 + 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_12
nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_mortality_12 - 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_12
nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_nasc_24 + 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_24
nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_nasc_24 - 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_24
nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_nasc_12 + 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_12
nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_nasc_12 - 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_12
# conta pode ser refeita
nasc_and_death_by_month_without_city$tax_mortality_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_mortality_12 *1000 / nasc_and_death_by_month_without_city$avg_mov_nasc_12
nasc_and_death_by_month_without_city$tax_mortality_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_mortality_24 *1000 / nasc_and_death_by_month_without_city$avg_mov_nasc_24
nasc_and_death_by_month_without_city$tax_mortality_lim_sup_grupo_12<- nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_12 *1000 / nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_12
nasc_and_death_by_month_without_city$tax_mortality_lim_sup_grupo_24<- nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_24 *1000 / nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_24
nasc_and_death_by_month_without_city$tax_mortality_lim_inf_grupo_12<- nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_12 *1000 / nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_12
nasc_and_death_by_month_without_city$tax_mortality_lim_inf_grupo_24<- nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_24 *1000 / nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_24


library(ggplot2)

p <- (ggplot(nasc_and_death_by_month_without_city, aes(x=year_month_date)) + 
        geom_line(aes(y = tax_mortality_grupo_24), color = "darkred") + 
        geom_line(aes(y = tax_mortality_lim_sup_grupo_24), color = "darkred", linetype = "dashed") + 
        geom_line(aes(y = tax_mortality_lim_inf_grupo_24), color="darkred", linetype = "dashed") 
      +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
      +    labs(x = "Year",
                y = "taxa de mortalidade")
      + theme_bw())
print(p)

nasc_and_death_by_month_without_city = subset(nasc_and_death_by_month_without_city, select = c( year_month_date, region, tax_mortality_lim_inf_grupo_24, tax_mortality_lim_inf_grupo_12, tax_mortality_lim_sup_grupo_24, tax_mortality_lim_sup_grupo_12, tax_mortality_grupo_12, tax_mortality_grupo_24, lim_inf_mortality_grupo_12, lim_inf_mortality_grupo_24, lim_sup_mortality_grupo_12, lim_sup_mortality_grupo_24, avg_mov_mortality_24, avg_mov_mortality_12  ) )
mortality_grupo <- merge(x=mortality_grupo, y=nasc_and_death_by_month_without_city, 
                         by.x=c("year_month_date", "region"), 
                         by.y=c("year_month_date", "region"))

mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(cum_death = cumsum(all_death))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(avg_mov_death_12 = rollmean(all_death, k=12, fill=NA, align='right'))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(avg_mov_death_24 = rollmean(all_death, k=24, fill=NA, align='right'))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(cum_nasc = cumsum(FL_BIRTH))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(avg_mov_nasc_12 = rollmean(FL_BIRTH, k=12, fill=NA, align='right'))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(avg_mov_nasc_24 = rollmean(FL_BIRTH, k=24, fill=NA, align='right'))

mortality_grupo$tax_mortality_city_without_trat<- mortality_grupo$all_death *1000 / mortality_grupo$FL_BIRTH
mortality_grupo$tax_mortality_city_cum<- mortality_grupo$cum_death *1000 / mortality_grupo$cum_nasc
mortality_grupo$tax_mortality_city_12<- mortality_grupo$avg_mov_death_12 *1000 / mortality_grupo$avg_mov_nasc_12
mortality_grupo$tax_mortality_city_24<- mortality_grupo$avg_mov_death_24 *1000 / mortality_grupo$avg_mov_nasc_24

# unique(mortality_grupo$LOCAL_NAME)
# for (j in c("Campinas") )
# {
#   CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j  )
#   p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date)) +
#           geom_line(aes(y = all_death), color = "black")
#         +  ggtitle( paste("Mortalidade da cidade - média movel 12 meses"))
#         +    labs(x = "Year",
#                   y = "Média móvel - 12 meses")
#         + theme_bw())
#   print(p)
#   p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date)) +
#           geom_line(aes(y = tax_mortality_city_12), color = "black")
#         +  ggtitle( paste("taxa de mortalidade da cidade - media movel 12 meses"))
#         +    labs(x = "Year",
#                   y = "taxa de mortalidade")+ theme_bw())
#   print(p)
  # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date)) +
  #         geom_line(aes(y = tax_mortality_grupo_12), color = "darkred") +
  #         geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed") +
  #         geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed") +
  #         geom_line(aes(y = tax_mortality_city_12), color = "black")
  #       +  ggtitle( paste("taxa de mortalidade região x cidade - media movel 12 meses"))
  #       +    labs(x = "Year",
  #                 y = "taxa de mortalidade")+ theme_bw())
  # print(p)
  # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date)) +
  #         geom_line(aes(y = avg_mov_death_12), color = "black") +
  #         geom_line(aes(y = lim_sup_mortality_grupo_12), color = "darkred", linetype = "dashed") +
  #         geom_line(aes(y = lim_inf_mortality_grupo_12), color="darkred", linetype = "dashed") +
  #         geom_line(aes(y = avg_mov_mortality_12), color = "darkred")
  #       +  ggtitle( paste("mortalidade região x cidade - media movel 12 meses"))
  #       +    labs(x = "Year",
  #                 y = "mortalidade") + theme_bw())
  # print(p)
# 
 # }

##### usando o novo 
#devtools::install_github("cefet-rj-dal/harbinger")
library(harbinger)

#source("load_harbinger.R")
#loading Harbinger
#load_harbinger() # see ../load_harbinger.R

library(ggplot2)
#unique(mortality_grupo$LOCAL_NAME)
for (j in c("Londrina", "Campo Mourão") )
{
#   CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j )
#   model <- change_point(sw=12)
#   x <- CITY_BY_MONTH$tax_mortality_city_12
# #  names(x) <- format(CITY_BY_MONTH$year_month_date, "%m-%y")  
# #  names(x) <- 1:length(x)
# #  labels <- factor(names(x), levels=names(x), ordered=TRUE)
#   
#   x <- na.omit(x)
#   model <- fit(model, x)
#   detection <- detect(model, x)
#   grf <- plot.harbinger(model, x, detection) 
#   #grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
#  # grf <- grf + scale_x_discrete(breaks = labels[c(T, rep(F, 23))])
# 
#   plot(grf)
#   CITY_BY_MONTH$event <- detection$event
#   CITY_BY_MONTH_event <- filter(CITY_BY_MONTH, event == TRUE)

  # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date))
  #       + geom_line(aes(y = tax_mortality_grupo_12), color = "darkred")
  #       + geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_city_12), color = "black")
  #       + geom_point(data = CITY_BY_MONTH_event, mapping=aes(y=tax_mortality_city_12, x= year_month_date), color = "red")
  #       +  ggtitle( paste("taxa de mortalidade da cidade x regiao-grupo - média movel 12 meses - City: ", j ))
  #       +    labs(x = "Year",
  #                 y = "Taxa de mortalidade - 12 meses"))
  # print(p)
  
  CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j )
  model <- change_point(sw=12)
  model <- fit(model, CITY_BY_MONTH$tax_mortality_city_12)
  detection <- detect(model,  CITY_BY_MONTH$tax_mortality_city_12)
  CITY_BY_MONTH$event_city <- detection$event
  CITY_BY_MONTH_event_city <- filter(CITY_BY_MONTH, event_city == TRUE)

  p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date))
        + geom_line(aes(y = tax_mortality_grupo_12), color = "darkred")
        + geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed")
        + geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed")
        + geom_line(aes(y = tax_mortality_city_12), color = "black")
        + geom_hline(yintercept=12, linetype="dashed", 
                     color = "blue", size=0.5)
       + geom_point(data = CITY_BY_MONTH_event_city, mapping=aes(y=tax_mortality_city_12, x= year_month_date), color = "red")
         +  ggtitle( paste("Taxa de mortalidade - média movel 12 meses - City: ", j ))
        +    labs(x = "Year",
                  y = "Taxa de mortalidade - 12 meses") + theme_bw())
  print(p)
  
   # ## grafico media movel, sem taxa e sem limites
   # CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j )
   # model <- change_point(sw=12)
   #  model <- fit(model, CITY_BY_MONTH$avg_mov_death_12)
   #  detection <- detect(model,  CITY_BY_MONTH$avg_mov_death_12)
   #  grf <- plot.harbinger(model, CITY_BY_MONTH$avg_mov_death_12, detection)
   #  CITY_BY_MONTH$event <- detection$event
   #  CITY_BY_MONTH_event <- filter(CITY_BY_MONTH, event == TRUE)
   # 
   #  p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date)) +
   #          geom_line(aes(y = avg_mov_death_12), color = "black")
   #        + geom_point(data = CITY_BY_MONTH_event, mapping=aes(y=avg_mov_death_12, x= year_month_date), color = "red")
   #        +  ggtitle( paste("Mortalidade da cidade - media movel 12 meses"))
   #        +    labs(x = "Year",
   #                  y = "Média móvel - 12 meses")+ theme_bw() )
   #  print(p)
   #  # 
  #  ## grafico media movel, com taxa e sem limites
  # 
   # CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j )
   # model <- change_point(sw=12)
   # model <- fit(model, CITY_BY_MONTH$tax_mortality_city_12)
   # detection <- detect(model,  CITY_BY_MONTH$tax_mortality_city_12)
   # grf <- plot.harbinger(model, CITY_BY_MONTH$tax_mortality_city_12, detection)
   # CITY_BY_MONTH$event <- detection$event
   # CITY_BY_MONTH_event <- filter(CITY_BY_MONTH, event == TRUE)
   # CITY_BY_MONTH <- filter(mortality_grupo, LOCAL_NAME == j )
   # model <- change_point(sw=12)
   # model <- fit(model, CITY_BY_MONTH$tax_mortality_city_12)
   # detection <- detect(model,  CITY_BY_MONTH$tax_mortality_city_12)
   # grf <- plot.harbinger(model, CITY_BY_MONTH$tax_mortality_city_12, detection)
   # CITY_BY_MONTH$event <- detection$event
   # CITY_BY_MONTH_event <- filter(CITY_BY_MONTH, event == TRUE)
   # 
   # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date)) +
   #         geom_line(aes(y = tax_mortality_city_12), color = "black")
   #       + geom_point(data = CITY_BY_MONTH_event, mapping=aes(y=tax_mortality_city_12, x= year_month_date), color = "red")
   #       +  ggtitle( paste("Taxa de Mortalidade da cidade - media movel 12 meses"))
   #       +    labs(x = "Year",
   #                 y = "Média móvel - 12 meses")+ theme_bw() )
   # print(p)
  #Change finder arima 
  # model <- change_finder_arima()
  # model <- fit(model, CITY_BY_MONTH$tax_mortality_city_12)
  # detection <- detect(model, CITY_BY_MONTH$tax_mortality_city_12)
  # CITY_BY_MONTH$event_city <- detection$event
  # CITY_BY_MONTH_event_city <- filter(CITY_BY_MONTH, event_city == TRUE)
  # model <- change_finder_arima()
  # model <- fit(model, CITY_BY_MONTH$tax_mortality_grupo_12)
  # detection <- detect(model,  CITY_BY_MONTH$tax_mortality_grupo_12)
  # CITY_BY_MONTH$event_grupo <- detection$event
  # CITY_BY_MONTH_event_grupo <- filter(CITY_BY_MONTH, event_grupo == TRUE)
  # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date))
  #       + geom_line(aes(y = tax_mortality_grupo_12), color = "darkred")
  #       + geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_city_12), color = "black")
  #       + geom_point(data = CITY_BY_MONTH_event_city, mapping=aes(y=tax_mortality_city_12, x= year_month_date), color = "red")
  #       + geom_point(data = CITY_BY_MONTH_event_grupo, mapping=aes(y=tax_mortality_grupo_12, x= year_month_date), color = "blue")
  #       
  #       +  ggtitle( paste("Change finder arima: taxa de mortalidade - média movel 12 meses - City: ", j ))
  #       +    labs(x = "Year",
  #                 y = "Taxa de mortalidade - 12 meses") + theme_bw())
  # print(p)
  # 
  # model <-  har_garch()
  # model <- fit(model, CITY_BY_MONTH$tax_mortality_city_12)
  # detection <- detect(model, CITY_BY_MONTH$tax_mortality_city_12)
  # CITY_BY_MONTH$event_city <- detection$event
  # CITY_BY_MONTH_event_city <- filter(CITY_BY_MONTH, event_city == TRUE)
  # model <-  har_garch()
  # model <- fit(model, CITY_BY_MONTH$tax_mortality_grupo_12)
  # detection <- detect(model,  CITY_BY_MONTH$tax_mortality_grupo_12)
  # CITY_BY_MONTH$event_grupo <- detection$event
  # CITY_BY_MONTH_event_grupo <- filter(CITY_BY_MONTH, event_grupo == TRUE)
  # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date))
  #       + geom_line(aes(y = tax_mortality_grupo_12), color = "darkred")
  #       + geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_city_12), color = "black")
  #       + geom_point(data = CITY_BY_MONTH_event_city, mapping=aes(y=tax_mortality_city_12, x= year_month_date), color = "red")
  #       + geom_point(data = CITY_BY_MONTH_event_grupo, mapping=aes(y=tax_mortality_grupo_12, x= year_month_date), color = "blue")
  #       
  #       +  ggtitle( paste("Garch: taxa de mortalidade - média movel 12 meses - City: ", j ))
  #       +    labs(x = "Year",
  #                 y = "Taxa de mortalidade - 12 meses") + theme_bw())
  # print(p)
  # 
  # model <- change_finder_ets()
  # model <- fit(model, CITY_BY_MONTH$tax_mortality_city_12)
  # detection <- detect(model, CITY_BY_MONTH$tax_mortality_city_12)
  # CITY_BY_MONTH$event_city <- detection$event
  # CITY_BY_MONTH_event_city <- filter(CITY_BY_MONTH, event_city == TRUE)
  # 
  # model <- change_finder_ets()
  # model <- fit(model, CITY_BY_MONTH$tax_mortality_grupo_12)
  # detection <- detect(model,  CITY_BY_MONTH$tax_mortality_grupo_12)
  # CITY_BY_MONTH$event_grupo <- detection$event
  # CITY_BY_MONTH_event_grupo <- filter(CITY_BY_MONTH, event_grupo == TRUE)
  # p <- (ggplot(CITY_BY_MONTH, aes(x=year_month_date))
  #       + geom_line(aes(y = tax_mortality_grupo_12), color = "darkred")
  #       + geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed")
  #       + geom_line(aes(y = tax_mortality_city_12), color = "black")
  #       + geom_point(data = CITY_BY_MONTH_event_city, mapping=aes(y=tax_mortality_city_12, x= year_month_date), color = "red")
  #       + geom_point(data = CITY_BY_MONTH_event_grupo, mapping=aes(y=tax_mortality_grupo_12, x= year_month_date), color = "blue")
  #       
  #       +  ggtitle( paste("Change finder ets: taxa de mortalidade - média movel 12 meses - City: ", j ))
  #       +    labs(x = "Year",
  #                 y = "Taxa de mortalidade - 12 meses") + theme_bw())
  # print(p)

  }

#lim_sup_city = filter(mortality_grupo, tax_mortality_lim_sup_grupo_12 < tax_mortality_city_12 )
