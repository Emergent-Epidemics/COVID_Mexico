#SV Scarpino
#July 2020
#COVID in Mexico

###########
#libraries#
###########
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)
library(rgdal)
library(glmulti)
#devtools::install_github("alberto-mateos-mo/datoscovid19mx")
#library(datoscovid19mx)

######
#Data#
######
cases <- read.csv("~/Dropbox/demographic_transitions/200811COVID19MEXICO.csv")
cases$FECHA_SINTOMAS <- as.POSIXct(strptime(cases$FECHA_SINTOMAS, format = "%Y-%m-%d"))
cases$New <- rep(1, nrow(cases))

#add in state name
states_cases <- read.csv("~/Dropbox/demographic_transitions/diccionario_datos_covid19/Catalogos_0412_entidades.csv")
mt_state <- match(cases$ENTIDAD_RES, states_cases$CLAVE_ENTIDAD)
length(which(is.na(mt_state) == TRUE))
cases$state <- states_cases$ENTIDAD_FEDERATIVA[mt_state]

#add in municipio name
muni_cases <- read.csv("~/Dropbox/demographic_transitions/diccionario_datos_covid19/Catalogos_0412_municipios.csv")
muni_cases$full_muni_id <- paste0(muni_cases$CLAVE_ENTIDAD, muni_cases$CLAVE_MUNICIPIO)
cases$full_muni_id <- paste0(cases$ENTIDAD_RES, cases$MUNICIPIO_RES)
mt_muni <- match(cases$full_muni_id, muni_cases$full_muni_id)
length(which(is.na(mt_muni) == TRUE))
cases$muni <- muni_cases$MUNICIPIO[mt_muni]

mobility <- read.csv("~/Dropbox/demographic_transitions/movement/mexico_november_august_municipality.csv")
mobility$week_start <- as.POSIXct(strptime(mobility$week_start, format = "%d/%m/%Y"))
mobility$full_muni_id <- paste0(mobility$destination_state, mobility$destination_municipality)

pop <- read.csv("~/Dropbox/demographic_transitions/shp/population.csv")

map <- readOGR("~/Dropbox/demographic_transitions/shp/merged_usa_guatemala_mexico.shp")

#############
#Growth rate#
#############
#dates <- seq(from = min(cases$FECHA_SINTOMAS, na.rm = TRUE), to = max(cases$FECHA_SINTOMAS, na.rm = TRUE), by = 60*60*24)
dates <- seq(from = min(mobility$week_start[which(mobility$week_start >= as.POSIXct(strptime("01/01/2020", format = "%d/%m/%Y")))]), to = max(mobility$week_start), by = 60*60*24*7)
dates <- c(dates, max(mobility$week_start))

dates <- as.POSIXct(strptime(substr(dates, 1, 10), format = "%Y-%m-%d"))

by_dat <- by(data = cases$New, INDICES = cases[,c("FECHA_SINTOMAS", "full_muni_id")], FUN = sum, na.rm = TRUE)
by_dat_mat <- matrix(as.numeric(by_dat), ncol = ncol(by_dat), nrow = nrow(by_dat))
colnames(by_dat_mat) <- colnames(by_dat)
by_dat_mat_df <- as.data.frame(by_dat_mat)
by_dat_mat_df$Date <- rownames(by_dat)
by_dat_mat_df$Date <- as.POSIXct(by_dat_mat_df$Date)

diff.df <- by_dat_mat_df %>% gather(location, cases, 1:(ncol(by_dat_mat_df)-1)) #last column is the date
colnames(diff.df) <- c("Date", "County", "New")

lag <- 1 #the dates object is weekly, so this is lag in weeks
mob_lag <- 2 #how far back to for mexico mobility
start_i <- max(c(lag, mob_lag))

doubling_prov <- list()
doubling_fixed <- list()
mobility_from_df <- list()
for(i in (start_i+1):length(dates)){
  use.i <- which(diff.df$Date >= dates[i-lag] & diff.df$Date <= dates[i])
  
  data.i <- diff.df[use.i, ]
  
  data.i$DATE <- as.numeric(data.i$Date - dates[i-1], unit = "days")
  mod3.i <- try(lmer(data = data.i, log(New + 1) ~ DATE + (DATE|County)), silent = TRUE)
  
  if(is(mod3.i)[1] == "try-error"){
    fixed.i <- NA
    doubling.i <- NA
    mob.i <- NA
    names.i <- NA
  }else{
    fixed.i <-   fixef(mod3.i)["DATE"]
    doubling.i <- ranef(mod3.i)$County$DATE+fixed.i
    names.i <- rownames(ranef(mod3.i)$County)
    
    use.mob.i <- which(mobility$week_start == dates[i-(mob_lag)] & mobility$origin_state == 9)
    if(length(use.mob.i) == 0){
      stop()
    }
    by_full_muni.i <- by(data = mobility$movement[use.mob.i], INDICES = mobility$full_muni_id[use.mob.i], FUN = sum, na.rm = TRUE)
    mt.mob.i <- match(names.i, names(by_full_muni.i))
    mob.i <- rep(NA, length(names.i))
    mob.i <- as.numeric(by_full_muni.i[mt.mob.i])
  }
  
  doubling_prov[[i-lag]] <- doubling.i
  names(doubling_prov[[i-lag]]) <- names.i
  doubling_fixed[[i-lag]] <- fixed.i
  mobility_from_df[[i-lag]] <- mob.i
}

rates <- unlist(lapply(doubling_prov, function(x) x))
prov <- unlist(lapply(doubling_prov, function(x) names(x)))
times <- rep(dates[(lag+1):(length(dates))], times = unlist(lapply(doubling_prov, function(x) length(x))))
mobs <- unlist(lapply(mobility_from_df, function(x) x))

#######
#Plots#
#######
dat.plot <- data.frame(rates, times, prov, mobs)
dat.plot$time_fact <- as.factor(dat.plot$times)
dat.plot$times <- as.numeric(times-mean(times), unit = "days")
dat.plot$prov <- as.factor(dat.plot$prov)

mod <- lm(rates ~ log(mobs)*time_fact, data = dat.plot)
summary(mod)

mod.plot <- lmer(rates ~ log(mobs) + (log(mobs)|prov) + (times|prov) + (log(mobs)|time_fact), data = dat.plot)

ran_coef <- ranef(mod.plot)$time_fact
min.coef <- min(ranef(mod.plot)$prov)
max.coef <- max(ranef(mod.plot)$prov)
dates_coef <- as.POSIXct(strptime(rownames(ran_coef), format = "%Y-%m-%d"))
week_ef <- ran_coef$`log(mobs)`+ fixef(mod.plot)[2]

plot(dates_coef, week_ef, type = "l", bty = "n", xlab = "2020", ylab = "Growth rate and mobility coefficient", main = "COVID-19 Growth Rate (Municipality) & Mobility from DF", lwd = 3, ylim = c(min(week_ef + min.coef), max(week_ef + max.coef)))
points(dates_coef, week_ef + min.coef, type = "l", lwd = 1, lty = 5)
points(dates_coef, week_ef + max.coef, type = "l", lwd = 1, lty = 5)
abline(h = 0, lty = 3, lwd = 3, col = "#b2182b")

cols <- sample(wes_palette(name = "Zissou1", n = length(unique(dat.plot$times)), type = "continuous"), length(unique(dat.plot$times)))

quartz(width = 8, height = 6)
ggplot(dat.plot, (aes(x = log(mobs), y = rates, color = times, group = time_fact))) + geom_point() + xlab("Mobility from Mexico City") + ylab("COVID19 growth rate (Municipality-level)") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(color = "Week") + ggtitle("COVID-19 Growth Rate and Mobility from Mexico City") + geom_smooth(method = "lm")
