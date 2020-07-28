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

######
#Data#
######
dat <- read.csv("../Data/200721COVID19MEXICO2.csv")
dat$FECHA_SINTOMAS <- as.POSIXct(strptime(dat$FECHA_SINTOMAS, format = "%Y-%m-%d"))
dat$New <- rep(1, nrow(dat))

#############
#Growth rate#
#############
dates <- seq(from = min(dat$FECHA_SINTOMAS, na.rm = TRUE), to = max(dat$FECHA_SINTOMAS, na.rm = TRUE), by = 60*60*24)
lag <- 4

by_dat <- by(data = dat$New, INDICES = dat[,c("FECHA_SINTOMAS", "ENTIDAD_RES")], FUN = sum, na.rm = TRUE)
by_dat_mat <- matrix(as.numeric(by_dat), ncol = ncol(by_dat), nrow = nrow(by_dat))
colnames(by_dat_mat) <- colnames(by_dat)
by_dat_mat_df <- as.data.frame(by_dat_mat)
by_dat_mat_df$Date <- rownames(by_dat)
by_dat_mat_df$Date <- as.POSIXct(by_dat_mat_df$Date)
  
diff.df <- by_dat_mat_df %>% gather(location, cases, 1:32) 
colnames(diff.df) <- c("Date", "County", "New")

doubling_prov <- list()
doubling_fixed <- list()
for(i in (lag+1):(length(dates))){
  use.i <- which(diff.df$Date >= dates[i-lag] & diff.df$Date <= dates[i])
  
  data.i <- diff.df[use.i, ]
  
  data.i$DATE <- as.numeric(data.i$Date - dates[i-lag], unit = "days")
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
  }
  
  doubling_prov[[i-lag]] <- doubling.i
  names(doubling_prov[[i-lag]]) <- names.i
  doubling_fixed[[i-lag]] <- fixed.i
}

rates <- unlist(lapply(doubling_prov, function(x) x))
prov <- unlist(lapply(doubling_prov, function(x) names(x)))
times <- rep(dates[(lag+1):(length(dates))], times = unlist(lapply(doubling_prov, function(x) length(x))))

#######
#Plots#
#######
dat.plot <- data.frame(rates, times, prov)
cols <- sample(wes_palette(name = "Zissou1", n = length(unique(dat.plot$prov)), type = "continuous"), length(unique(dat.plot$prov)))

quartz(width = 8, height = 6)
ggplot(dat.plot, (aes(x = times, y = rates, color = prov))) + geom_line() + geom_line(size = 1.2) + scale_color_manual(values = cols) + xlab("2020") + ylab("COVID19 growth rate (state-level)") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(color = "State") + ggtitle("Mexico COVID-19 (Symptom Onset)")

quartz(width = 10, height = 10)
ggplot(dat.plot, (aes(x = times, y = rates, color = prov))) + geom_line() + scale_color_manual(values = rep("#000000", length(unique(dat.plot$prov)))) + xlab("2020") + ylab("COVID19 growth rate (state-level)") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(color = "State") + ggtitle("Mexico COVID-19 (Symptom Onset)")

quartz(width = 10, height = 10)
ggplot(diff.df, (aes(x = Date, y = New, color = County))) + geom_line() + geom_line(size = 1.2) + scale_color_manual(values = cols) + xlab("2020") + ylab("New COVID19 cases (state-level)") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(color = "State") + ggtitle("Mexico COVID-19 (Symptom Onset)")

ggplot(diff.df, (aes(x = Date, y = New))) + geom_bar(stat = "Identity") + xlab("2020") + ylab("New COVID19 cases (country-level)") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + ggtitle("Mexico COVID-19 (Symptom Onset)")
