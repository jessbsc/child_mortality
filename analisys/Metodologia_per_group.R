
mortality_and_nasc_by_month <- read.csv("~/mortalidade/child_mortality/data/mortality_and_nasc_by_month_2024_v1.csv")

mortality_and_nasc_by_month_clean = subset(mortality_and_nasc_by_month, select = c(year_month_date, LOCAL_CODE, region, UF, NOME.DO.MUNICÍPIO, city_group, all_death, FL_BIRTH) )
mortality_and_nasc_by_month_clean$year_month_date <- as.Date(mortality_and_nasc_by_month_clean$year_month_date)

mortality_and_nasc_by_month_clean <- filter(mortality_and_nasc_by_month_clean, year_month_date >= '1998/01/01')



### tratamento da regiao 

library(zoo)

nasc_and_death_by_month_without_city <- mortality_and_nasc_by_month_clean %>% group_by(year_month_date, region, city_group)

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% summarise(
  year_month_date = unique(year_month_date),
  region = unique(region),
  city_group = unique(city_group),
  sum_birth = sum(FL_BIRTH),
  sum_death = sum(all_death), 
  #sd_death = sd(all_death),
  #sd_birth = sd(FL_BIRTH), 
  #mean_death = mean(all_death),
  #mean_birth = mean(FL_BIRTH) 
  )

nasc_and_death_by_month_without_city[is.na(nasc_and_death_by_month_without_city)] <- 0

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%
  arrange(region, city_group, year_month_date)

soma_rolante_por_grupo_6_month <- function(x) {
  rollapply(x, width = 6, FUN = sum, fill = NA, align = "right")
}

avg_rolante_por_grupo_6_month <- function(x) {
  rollapply(x, width = 6, FUN = mean, fill = NA, align = "right")
}
soma_rolante_por_grupo_12_month <- function(x) {
  rollapply(x, width = 12, FUN = sum, fill = NA, align = "right")
}

avg_rolante_por_grupo_12_month <- function(x) {
  rollapply(x, width = 12, FUN = mean, fill = NA, align = "right")
}

soma_rolante_por_grupo_24_month <- function(x) {
  rollapply(x, width = 24, FUN = sum, fill = NA, align = "right")
}

avg_rolante_por_grupo_24_month <- function(x) {
  rollapply(x, width = 24, FUN = mean, fill = NA, align = "right")
}

sd_rolante_por_grupo_6_month <- function(x) {
  rollapply(x, width = 6, FUN = sd, fill = NA, align = "right")
}
sd_rolante_por_grupo_12_month <- function(x) {
  rollapply(x, width = 12, FUN = sd, fill = NA, align = "right")
}
sd_rolante_por_grupo_24_month <- function(x) {
  rollapply(x, width = 24, FUN = sd, fill = NA, align = "right")
}
calcular_desvio_padrao_acumulado <- function(valores, media_acumulada, num_observacoes) {
  desvios_quadraticos <- (valores - media_acumulada)^2  # Calcula os desvios quadráticos
  soma_desvios_quadraticos <- cumsum(desvios_quadraticos)  # Calcula a soma acumulada dos desvios quadráticos
  desvio_padrao_acumulado <- sqrt(soma_desvios_quadraticos / num_observacoes)  # Calcula o desvio padrão acumulado
  return(desvio_padrao_acumulado)
}

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(sum_mov_mortality_12 = soma_rolante_por_grupo_12_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(sum_mov_nasc_12 = soma_rolante_por_grupo_12_month(sum_birth))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(avg_mov_mortality_12 = avg_rolante_por_grupo_12_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(avg_mov_nasc_12 = avg_rolante_por_grupo_12_month(sum_birth))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(sum_mov_mortality_24 = soma_rolante_por_grupo_24_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(sum_mov_nasc_24 = soma_rolante_por_grupo_24_month(sum_birth))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(avg_mov_mortality_24 = avg_rolante_por_grupo_24_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(avg_mov_nasc_24 = avg_rolante_por_grupo_24_month(sum_birth))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(sum_mov_mortality_6 = soma_rolante_por_grupo_6_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(sum_mov_nasc_6 = soma_rolante_por_grupo_6_month(sum_birth))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(avg_mov_mortality_6 = avg_rolante_por_grupo_6_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(avg_mov_nasc_6 = avg_rolante_por_grupo_6_month(sum_birth))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(desvio_mortalidade_grupo_12 = sd_rolante_por_grupo_12_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(desvio_mortalidade_grupo_24 = sd_rolante_por_grupo_24_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(desvio_mortalidade_grupo_6  = sd_rolante_por_grupo_6_month(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(desvio_nasc_grupo_6  = sd_rolante_por_grupo_6_month(sum_birth))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(desvio_nasc_grupo_12 = sd_rolante_por_grupo_12_month(sum_birth))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(desvio_nasc_grupo_24 = sd_rolante_por_grupo_24_month(sum_birth))

nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>% mutate(cum_death = cumsum(sum_death))
nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% group_by(region, city_group) %>% mutate(cum_nasc = cumsum(sum_birth))
#nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>%group_by(region, city_group) %>%mutate(num_observacoes = cumsum(row_number()))

#nasc_and_death_by_month_without_city$media_acumulada_death <- nasc_and_death_by_month_without_city$cum_death / nasc_and_death_by_month_without_city$num_observacoes   # Média acumulada
#nasc_and_death_by_month_without_city$media_acumulada_nasc <- nasc_and_death_by_month_without_city$cum_nasc / nasc_and_death_by_month_without_city$num_observacoes   # Média acumulada
#nasc_and_death_by_month_without_city <- nasc_and_death_by_month_without_city %>% mutate(desvio_padrao_acumulado_nasc = sqrt((cum_nasc - media_acumulada_nasc)^2 / num_observacoes))
#nasc_and_death_by_month_without_city$desvio_padrao_acumulado_nasc <- sqrt((nasc_and_death_by_month_without_city$cum_nasc - nasc_and_death_by_month_without_city$media_acumulada_nasc)^2 / nasc_and_death_by_month_without_city$num_observacoes)
#nasc_and_death_by_month_without_city$desvio_padrao_acumulado_death <- sqrt((nasc_and_death_by_month_without_city$cum_death - nasc_and_death_by_month_without_city$media_acumulada_death)^2 / nasc_and_death_by_month_without_city$num_observacoes)

# nasc_and_death_by_month_without_city$desvio_padrao_acumulado_nasc <- calcular_desvio_padrao_acumulado(
#   nasc_and_death_by_month_without_city$cum_nasc,
#   nasc_and_death_by_month_without_city$media_acumulada_nasc,
#   nasc_and_death_by_month_without_city$num_observacoes
# )
# 
# nasc_and_death_by_month_without_city$desvio_padrao_acumulado_death <- calcular_desvio_padrao_acumulado(
#   nasc_and_death_by_month_without_city$cum_death,
#   nasc_and_death_by_month_without_city$media_acumulada_death,
#   nasc_and_death_by_month_without_city$num_observacoes
# )

#nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_cum <- nasc_and_death_by_month_without_city$mean_death + 2*nasc_and_death_by_month_without_city$sd_death
#nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_cum <- nasc_and_death_by_month_without_city$mean_death - 2*nasc_and_death_by_month_without_city$sd_death
#nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_cum <- nasc_and_death_by_month_without_city$mean_birth + 2*nasc_and_death_by_month_without_city$sd_birth
#nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_cum <- nasc_and_death_by_month_without_city$mean_birth - 2*nasc_and_death_by_month_without_city$sd_birth

# nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_cum <- nasc_and_death_by_month_without_city$media_acumulada_death + 2*nasc_and_death_by_month_without_city$desvio_padrao_acumulado_death
# nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_cum <- nasc_and_death_by_month_without_city$media_acumulada_death - 2*nasc_and_death_by_month_without_city$desvio_padrao_acumulado_death
# nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_cum <- nasc_and_death_by_month_without_city$media_acumulada_nasc + 2*nasc_and_death_by_month_without_city$desvio_padrao_acumulado_nasc
# nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_cum <- nasc_and_death_by_month_without_city$media_acumulada_nasc - 2*nasc_and_death_by_month_without_city$desvio_padrao_acumulado_nasc
# nasc_and_death_by_month_without_city$tax_mortality_lim_sup_grupo_cum<- nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_cum *1000 / nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_cum
# nasc_and_death_by_month_without_city$tax_mortality_lim_inf_grupo_cum<- nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_cum *1000 / nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_cum


nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_6  <- nasc_and_death_by_month_without_city$avg_mov_mortality_6 + 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_6
nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_6  <- nasc_and_death_by_month_without_city$avg_mov_mortality_6 - 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_6
nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_24  <- nasc_and_death_by_month_without_city$avg_mov_mortality_24 + 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_24
nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_mortality_24 - 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_24
nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_mortality_12 + 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_12
nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_mortality_12 - 2*nasc_and_death_by_month_without_city$desvio_mortalidade_grupo_12
nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_nasc_24 + 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_24
nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_24<- nasc_and_death_by_month_without_city$avg_mov_nasc_24 - 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_24
nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_nasc_12 + 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_12
nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_12<- nasc_and_death_by_month_without_city$avg_mov_nasc_12 - 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_12
nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_6 <- nasc_and_death_by_month_without_city$avg_mov_nasc_6  + 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_6
nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_6 <- nasc_and_death_by_month_without_city$avg_mov_nasc_6  - 2*nasc_and_death_by_month_without_city$desvio_nasc_grupo_6

nasc_and_death_by_month_without_city$tax_mortality_grupo_cum<- nasc_and_death_by_month_without_city$cum_death *1000 / nasc_and_death_by_month_without_city$cum_nasc
nasc_and_death_by_month_without_city$tax_mortality_grupo_6<- nasc_and_death_by_month_without_city$sum_mov_mortality_6 *1000 / nasc_and_death_by_month_without_city$sum_mov_nasc_6
nasc_and_death_by_month_without_city$tax_mortality_grupo_12<- nasc_and_death_by_month_without_city$sum_mov_mortality_12 *1000 / nasc_and_death_by_month_without_city$sum_mov_nasc_12
nasc_and_death_by_month_without_city$tax_mortality_grupo_24<- nasc_and_death_by_month_without_city$sum_mov_mortality_24 *1000 / nasc_and_death_by_month_without_city$sum_mov_nasc_24
nasc_and_death_by_month_without_city$tax_mortality_lim_sup_grupo_6<- nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_6 *1000 / nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_6
nasc_and_death_by_month_without_city$tax_mortality_lim_sup_grupo_12<- nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_12 *1000 / nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_12
nasc_and_death_by_month_without_city$tax_mortality_lim_sup_grupo_24<- nasc_and_death_by_month_without_city$lim_sup_mortality_grupo_24 *1000 / nasc_and_death_by_month_without_city$lim_inf_nasc_grupo_24
nasc_and_death_by_month_without_city$tax_mortality_lim_inf_grupo_6<- nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_6 *1000 / nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_6
nasc_and_death_by_month_without_city$tax_mortality_lim_inf_grupo_12<- nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_12 *1000 / nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_12
nasc_and_death_by_month_without_city$tax_mortality_lim_inf_grupo_24<- nasc_and_death_by_month_without_city$lim_inf_mortality_grupo_24 *1000 / nasc_and_death_by_month_without_city$lim_sup_nasc_grupo_24


###plot 
nasc_and_death_by_month_without_city_plot <- filter(nasc_and_death_by_month_without_city, city_group == 3)
nasc_and_death_by_month_without_city_plot <- filter(nasc_and_death_by_month_without_city_plot, region == 'Nordeste')

library(ggplot2)

p1 <- (ggplot(nasc_and_death_by_month_without_city_plot, aes(x=year_month_date)) + 
        geom_line(aes(y = tax_mortality_grupo_6), color = "darkred") + 
        geom_line(aes(y = tax_mortality_lim_sup_grupo_6), color = "darkred", linetype = "dashed") + 
        geom_line(aes(y = tax_mortality_lim_inf_grupo_6), color="darkred", linetype = "dashed") 
      +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
      +    labs(x = "Year",
                y = "taxa de mortalidade")
      + theme_bw())
print(p1)


p2 <- (ggplot(nasc_and_death_by_month_without_city_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_12), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p2)

p3 <- (ggplot(nasc_and_death_by_month_without_city_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_24), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_24), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_24), color="darkred", linetype = "dashed") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p3)

p3 <- (ggplot(nasc_and_death_by_month_without_city_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_cum), color = "darkred")  
         #geom_line(aes(y = tax_mortality_lim_sup_grupo_cum), color = "darkred", linetype = "dashed") + 
         #geom_line(aes(y = tax_mortality_lim_inf_grupo_cum), color="darkred", linetype = "dashed") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) - acumulado "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p3)


nasc_and_death_by_month_without_city_clean = subset(nasc_and_death_by_month_without_city, select = c( year_month_date, region, city_group , tax_mortality_grupo_cum, tax_mortality_lim_inf_grupo_24, tax_mortality_lim_inf_grupo_12, tax_mortality_lim_inf_grupo_6, tax_mortality_lim_sup_grupo_24, tax_mortality_lim_sup_grupo_12,  tax_mortality_lim_sup_grupo_6 ,tax_mortality_grupo_12,tax_mortality_grupo_6, tax_mortality_grupo_24  ) )

mortality_grupo <- merge(x=nasc_and_death_by_month_without_city_clean, y=mortality_and_nasc_by_month_clean, 
                         by.x=c("year_month_date", "region", "city_group"), 
                         by.y=c("year_month_date", "region", "city_group"))


mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(cum_death = cumsum(all_death))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(sum_mov_death_12_city = soma_rolante_por_grupo_12_month(all_death))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(sum_mov_death_24_city = soma_rolante_por_grupo_24_month(all_death))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(sum_mov_death_6_city = soma_rolante_por_grupo_6_month(all_death))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(cum_nasc = cumsum(FL_BIRTH))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(sum_mov_nasc_12_city = soma_rolante_por_grupo_12_month(FL_BIRTH))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(sum_mov_nasc_24_city = soma_rolante_por_grupo_24_month(FL_BIRTH))
mortality_grupo <- mortality_grupo %>% group_by(LOCAL_CODE)  %>% mutate(sum_mov_nasc_6_city = soma_rolante_por_grupo_6_month(FL_BIRTH))



mortality_grupo$tax_mortality_city_without_trat<- mortality_grupo$all_death *1000 / mortality_grupo$FL_BIRTH
mortality_grupo$tax_mortality_city_cum<- mortality_grupo$cum_death *1000 / mortality_grupo$cum_nasc
mortality_grupo$tax_mortality_city_6<- mortality_grupo$sum_mov_death_6_city *1000 / mortality_grupo$sum_mov_nasc_6_city
mortality_grupo$tax_mortality_city_12<- mortality_grupo$sum_mov_death_12_city *1000 / mortality_grupo$sum_mov_nasc_12_city
mortality_grupo$tax_mortality_city_24<- mortality_grupo$sum_mov_death_24_city *1000 / mortality_grupo$sum_mov_nasc_24_city

### City 1 

mortality_grupo_plot <- filter(mortality_grupo, NOME.DO.MUNICÍPIO == 'Rio de Janeiro')
mortality_grupo_plot <- filter(mortality_grupo_plot, year_month_date >= '2002/01/01')

p4 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_6), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_6), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_6), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_6), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p4)


p5 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_12), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_12), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p5)

p7 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_cum), color = "darkred") + 
         geom_line(aes(y = tax_mortality_city_cum), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p7)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/jupyter.R")

load_library("harbinger")
load_library("daltoolbox")

series <- mortality_grupo_plot$tax_mortality_grupo_12
plot_ts(x = 1:length(series), y = series)

model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, series, detection)
plot(grf)



### City 2

mortality_grupo_plot <- filter(mortality_grupo, NOME.DO.MUNICÍPIO == 'Diamante do Sul')
mortality_grupo_plot <- filter(mortality_grupo_plot, year_month_date >= '2002/01/01')

p4 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_6), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_6), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_6), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_6), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p4)


p5 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_12), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_12), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p5)

p6 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_24), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_24), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_24), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_24), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p6)

p7 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_cum), color = "darkred") + 
         geom_line(aes(y = tax_mortality_city_cum), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p7)


### City 3

mortality_grupo_plot <- filter(mortality_grupo, NOME.DO.MUNICÍPIO == 'Campo Mourão')
mortality_grupo_plot <- filter(mortality_grupo_plot, year_month_date >= '2002/01/01')

p4 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_6), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_6), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_6), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_6), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p4)


p5 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_12), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_12), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p5)

p6 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_24), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_24), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_24), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_24), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p6)

p7 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_cum), color = "darkred") + 
         geom_line(aes(y = tax_mortality_city_cum), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p7)

### City 4

mortality_grupo_plot <- filter(mortality_grupo, NOME.DO.MUNICÍPIO == 'Gonçalves Dias')
mortality_grupo_plot <- filter(mortality_grupo_plot, year_month_date >= '2002/01/01')

p4 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_6), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_6), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_6), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_6), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p4)


p5 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_12), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_12), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_12), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_12), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p5)

p6 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_24), color = "darkred") + 
         geom_line(aes(y = tax_mortality_lim_sup_grupo_24), color = "darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_lim_inf_grupo_24), color="darkred", linetype = "dashed") + 
         geom_line(aes(y = tax_mortality_city_24), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p6)

p7 <- (ggplot(mortality_grupo_plot, aes(x=year_month_date)) + 
         geom_line(aes(y = tax_mortality_grupo_cum), color = "darkred") + 
         geom_line(aes(y = tax_mortality_city_cum), color="black") 
       +  ggtitle( paste("taxa de mortalidade (por mil nascidos) "))
       +    labs(x = "Year",
                 y = "taxa de mortalidade")
       + theme_bw())
print(p7)


series <- mortality_grupo_plot$tax_mortality_city_24
plot_ts(x = 1:length(series), y = series)

model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, series, detection)
plot(grf)


# # Carregar o pacote
# library(stats)
# 
# wma <- stats::filter(series, c(0.2, 0.3, 0.5), sides = 1)
# plot_ts(x = 1:length(wma), y = wma)
# 
# ewma <- stats::filter(series, 0.9, method = "recursive")
# plot_ts(x = 1:length(ewma), y = ewma)
# 
# ewma <- stats::filter(series, 0.9, method = "convolution")
# plot_ts(x = 1:length(ewma), y = ewma)
# 
# mediana_suavizada <- stats::filter(series, rep(2, 12), sides = 1)
# plot_ts(x = 1:length(mediana_suavizada), y = mediana_suavizada)
# 
