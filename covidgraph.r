library(ggplot2)
library(reshape2)
library(scales)
library(tidyverse)
library(lubridate)
library(stringr)



# --------------- Support functions --------------- 

# Moving average
ma <- function(x, order = 7) {
  res <- c()
  for(i in 1:NROW(x)) { 
    if(i < order) res <- append(res, NA)
    else res <- append(res, mean(x[c(i:(i-order+1))], na.rm = TRUE))
  }
  return(res)
}



# --------------- Data on ICU capacity and its usage --------------- 

divi <- read.csv("icu_usage.csv", header = TRUE, sep =";")
divi <- divi[divi$Bundesland == "DEUTSCHLAND",]
divi <- divi[, c(1,4,5,6)]
names(divi) <- c("date", "covid", "used", "free")

divi$date <- ymd(substr(divi$date, 1, 10))
divi$capacity <- divi$used + divi$free
divi$usedp <- (divi$used / divi$capacity) * 100
divi$freep <- 100 - divi$usedp
divi$covidp <- (divi$covid / divi$capacity) * 100
divi$noncovidp <- divi$usedp - divi$covidp

divi <- divi[divi$date >= ymd("2020-10-01"), ]

divi <- divi[,c ("date", "freep", "noncovidp", "covidp")]



# --------------- Data on deaths and incidence --------------- 

deaths <- as_tibble(read.csv("deaths.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE))
deaths <- deaths[,c(1,2,4,5,6)]
names(deaths) <- c("date", "cases", "newcases", "deaths", "newdeaths")
deaths$date <- dmy(deaths$date)
deaths$newdeaths <- ma(deaths$newdeaths)

inc <- as_tibble(read.csv("incidence.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE))[17, -1]
inc <- t(inc)
inc <- cbind(inc, row.names(inc))
inc <- as_tibble(inc)
inc <- inc[!is.na(inc[,1]),]
inc <- inc[2:NROW(inc),]
names(inc) <- c("incidence", "date")
inc$date <- dmy(str_replace(inc$date, "X", ""))
class(inc$incidence) <- "numeric"



# --------------- Merge and melt dataset --------------- 

full <- inner_join(divi, deaths, by =c("date"))
full <- inner_join(full, inc, by =c("date"))
full.long <- melt(full, id.vars = "date", value.name = "val")



# --------------- Graphs --------------- 

coeff1 <- 12
graph <- ggplot() +
  geom_bar(data = subset(full.long, variable %in% c("freep", "covidp", "noncovidp")), aes(x = date, y = val, fill = variable), stat = "identity", width = 1, alpha = 0.75) +
  geom_line(data = subset(full.long, variable %in% c("newdeaths")), aes(x = date, y = val/coeff1, color = "red3"), size = 1.1) +
  geom_line(data = subset(full.long, variable %in% c("incidence")), aes(x = date, y = val/coeff1, color = "darkorange2"), size = 1.1) +
  geom_vline(xintercept = ymd("2020-12-16"), linetype="solid", color = "blue4", size=1) +
  geom_vline(xintercept = ymd("2020-11-02"), linetype="solid", color = "blue4", size=1) +
  geom_label(aes(x = ymd("2020-11-02"), y = 85, label = "Lockdown \"light\""), fill = "white", color = "blue4") +
  geom_label(aes(x = ymd("2020-12-16"), y = 95, label = "Lockdown \"hard\""), fill = "white", color = "blue4") +
  scale_fill_manual(values = c("gray68", "steelblue2", "dodgerblue4"), labels = c("Free capacity","Used capacity (non-Covid-19)","Used capacity (Covid-19)")) +
  scale_color_manual(values = c("darkorange2", "red2"), labels = c("7-day incidence", "Death cases (7-day rolling average)")) +
  scale_y_continuous(breaks = seq(0, 100, 5),
              sec.axis = sec_axis(~.*coeff1, name = "Death cases / 7-day incidence")) +
  scale_x_date(date_breaks = waiver(), date_minor_breaks = "1 week", limits = c(min(full.long$date), max(full.long$date))) +
  labs(title = "Covid-19 Situation in Germany", x = "", y = "Percent of total ICU capacity", fill = "Left scale:", color = "Right scale:") +
  theme(text = element_text(size = 11, face = "bold"), legend.text = element_text(size = 11, colour = "grey33", face = "plain"), legend.position = "bottom", legend.key.size = unit(12, "points"), legend.background = element_rect(fill = "gray94"), axis.title = element_text(size = 13, face = "plain"))
