library(tibble)
library(EventDetectR)
library(magrittr)
library(dplyr)
library(patchwork)
library(ggplot2)

#Read data
micro_data_csv_clean<- read.csv(file = './data/mortality_and_nasc_by_month_v6.csv')

#Transform data
micro_data_csv_clean$month <- format(as.Date(micro_data_csv_clean$year_month_date, format="%Y-%m-%d"),"%m")
micro_data_csv_clean$year <- format(as.Date(micro_data_csv_clean$year_month_date, format="%Y-%m-%d"),"%Y")

# Created function control chart
control_chart = function (study_year, last_observation_year, city,  data , letter) {
  
  start_observation_year <-  as.numeric(last_observation_year) - 10
  data$death_per_1000<- data$all_death/data$FL_BIRTH*1000
  
  
  mortality_group <- filter(data, year >= start_observation_year )
  mortality_group <- filter(mortality_group, year <= last_observation_year )
  
  mortality_group <- filter(mortality_group, LOCAL_NAME == city)
  
  birth_and_death_by_month_without_city <- mortality_group %>% group_by(month) %>% summarise(
    month = max(month),
    avg_month = mean(death_per_1000),
    sd_month = sd(death_per_1000) 
  )
  
  birth_and_death_by_month_without_city$upper_limit <- birth_and_death_by_month_without_city$avg_month + 1.96*birth_and_death_by_month_without_city$sd_month
  birth_and_death_by_month_without_city$lower_limit <- birth_and_death_by_month_without_city$avg_month - 1.96*birth_and_death_by_month_without_city$sd_month
  
  mortality_group_year <- filter(data, year == study_year)
  mortality_group_year <- filter(mortality_group_year, LOCAL_NAME == city)
  
  mortality_group_year_merge <- merge(x=mortality_group_year, y=birth_and_death_by_month_without_city, 
                                      by.x=c("month"), 
                                      by.y=c("month"))
  
  state <- unique(mortality_group_year$UF)
  
  reordered_categories <- factor(mortality_group_year_merge$death_per_1000, levels = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"))

  p <- ggplot(mortality_group_year_merge, aes(x = month, group = 1)) + 
    geom_line(aes(y = death_per_1000, color = "Observed", linetype = "Observed")) +
    geom_line(aes(y = avg_month, color = "Expected Average", linetype = "Expected Average")) +
    geom_line(aes(y = upper_limit, color = "Upper Limit", linetype = "Upper Limit")) +
    geom_line(aes(y = lower_limit, color = "Lower Limit", linetype = "Lower Limit")) +
    scale_color_manual(
      values = c("Observed" = "black", "Expected Average" = "darkred", "Upper Limit" = "coral", "Lower Limit" = "darkorange4"),
      breaks = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"),
      name = "Diagram Control"
    ) +
    scale_linetype_manual(
      values = c("Observed" = "solid", "Expected Average" = "solid", "Upper Limit" = "dashed", "Lower Limit" = "dashed"),
      breaks = c("Observed", "Expected Average", "Upper Limit", "Lower Limit"),
      name = "Diagram Control"
    ) +
    labs(x = "Month", y = "Infant Mortality Rate") +
    ggtitle(paste(letter, "- Locality: ", city, "-", state)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.justification = "center"
    )
  print(p)
 
}

# Created function historic
historic_by_year = function (city, last_observation_year, study_year, data, letter ) {
  start_observation_year <- as.numeric(last_observation_year)- 10
  mortality_group <- filter(data, year >= start_observation_year )
  mortality_group <- filter(mortality_group, year <= study_year )
  
  city_analysis <- filter(mortality_group, LOCAL_NAME == city)
  
  city_analysis$death_per_1000_month <- city_analysis$all_death / city_analysis$FL_BIRTH *1000
  
  city_analysis_group <- city_analysis %>% group_by(year) %>% summarise(
    year = max(year),
    sum_deaths = sum(all_death), 
    sum_birth = sum(FL_BIRTH)
  )
  
  city_analysis_group$death_per_1000_year <- city_analysis_group$sum_deaths / city_analysis_group$sum_birth *1000
  
  state <- unique(city_analysis$UF)
  
  library(ggplot2)
  
  years_to_show <- c('2010','2012','2014','2016', '2018','2020')
  
  p <- ggplot() +
    geom_line(data = city_analysis, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = death_per_1000_month, color = "deepskyblue1" )) +
    geom_line(data = city_analysis_group, aes(x = as.Date(paste(year, "01", "01", sep = "-")), y = death_per_1000_year, color = "blue")) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +  
    labs(x = "Month/Year", y = "Infant Mortality Rate") +
    ggtitle(paste(letter, "- Locality: ", city, "-", state)) +
    scale_color_identity(guide = "legend", labels = c("Yearly", "Monthly")) + 
    theme_bw() + labs(color = "IMR Historic")  
  
  
  print(p)
  
}

study_year <- '2020'
last_observation_year <- '2019'

### plot graphs 1
p1 <- control_chart(study_year, last_observation_year, 'Rio de Janeiro', micro_data_csv_clean, "A.2")
p2 <- control_chart(study_year, last_observation_year, 'Campo Mourão', micro_data_csv_clean, "B.2")
p3 <- control_chart(study_year, last_observation_year, 'Diamante do Sul', micro_data_csv_clean, "C.2")
p4 <- control_chart(study_year, last_observation_year, 'Gonçalves Dias', micro_data_csv_clean, "D.2")

subplot <- p1 + p2 + p3 + p4 + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
print(subplot)

### plot graphs 2

p5 <- historic_by_year('Rio de Janeiro', last_observation_year, study_year, micro_data_csv_clean, "A.1")
p6 <- historic_by_year('Campo Mourão', last_observation_year, study_year, micro_data_csv_clean, "B.1")
p7 <- historic_by_year('Diamante do Sul', last_observation_year, study_year, micro_data_csv_clean, "C.1")
p8 <- historic_by_year('Gonçalves Dias', last_observation_year, study_year, micro_data_csv_clean, "D.1")

subplot2 <-  p5 + p6 + p7 + p8 + plot_layout(guides = 'collect')
print(subplot2)


### plot graphs combine

subplot3 <- p5 + p1  + p6 + p2 + p7 + p3 + p8 + p4 + plot_layout(ncol = 2 , guides = 'collect') & theme(legend.position = 'bottom')
print(subplot3)


### Created table  
filter_table <- filter(micro_data_csv_clean, year == study_year )
filter_table$group <- ifelse(filter_table$population>=500000, 1, ifelse(filter_table$population>=100000,2,ifelse(filter_table$population>=50000,3,ifelse(filter_table$population>=20000,4,ifelse(filter_table$population>=10000,5,6)))))

filter_table_agg <- filter_table %>% group_by(LOCAL_NAME) %>% summarise(
  all_death_sum = sum(all_death),
  FL_BIRTH_sum = sum(FL_BIRTH),
  LOCAL_NAME = max(LOCAL_NAME),  
  group = max(group), 
  population = max(population), 
  imr = (sum(all_death)/sum(FL_BIRTH))*1000
)
filter_table_total <- filter_table_agg %>% summarise(
  all_death_total = sum(all_death_sum),
  FL_BIRTH_total = sum(FL_BIRTH_sum),
)

filter_table_agg_group <- filter_table_agg %>% group_by(group) %>% summarise(
  all_death_sum = sum(all_death_sum),
  FL_BIRTH_sum = sum(FL_BIRTH_sum),
  all_death_avg = mean(all_death_sum),
  FL_BIRTH_avg = mean(FL_BIRTH_sum),
  group = max(group),
  imr_avg = mean(imr), 
  imr_sd = sd(imr),
)
filter_table_agg_group$imr_group<- filter_table_agg_group$all_death_sum/filter_table_agg_group$FL_BIRTH_sum*1000
filter_table_agg_group$perc_death<- filter_table_agg_group$all_death_sum/filter_table_total$all_death_total*100
filter_table_agg_group$perc_birth<- filter_table_agg_group$FL_BIRTH_sum/filter_table_total$FL_BIRTH_total*100

article_table <- filter_table_agg_group %>%select(group, all_death_sum, perc_death, FL_BIRTH_sum, perc_birth, imr_avg, imr_sd)