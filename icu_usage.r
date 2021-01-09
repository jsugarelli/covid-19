library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)


divi <- read.csv("icu_usage.csv", header = TRUE, sep =";")

names(divi) <- c("date", "used", "free", "reserve", "covid")

divi$date <- ymd(substr(divi$date, 1, 10))
divi$capacity <- divi$used + divi$free
divi$usedp <- (divi$used / divi$capacity) * 100
divi$freep <- 100 - divi$usedp
divi$covidp <- (divi$covid / divi$capacity) * 100
divi$noncovidp <- divi$usedp - divi$covidp

divi <- divi[divi$date >= ymd("2020-09-15"), ]

divi <- divi[,c ("date", "freep", "noncovidp", "covidp")]
divi.long <- melt(divi, id.vars = "date", value.name = "pct")

graph <- ggplot(data = divi.long, aes(x = date, y = pct, fill = variable)) +
  geom_bar(stat = "identity", alpha = 0.75) +
  geom_vline(xintercept = ymd("2020-12-16"), linetype="solid", color = "blue4", size=1) +
  geom_vline(xintercept = ymd("2020-11-02"), linetype="solid", color = "blue4", size=1) +
  geom_label(aes(x = ymd("2020-11-02"), y = 85, label = "Lockdown \"light\""), fill = "white", color = "blue4") +
  geom_label(aes(x = ymd("2020-12-16"), y = 95, label = "Lockdown \"hard\""), fill = "white", color = "blue4") +
  scale_fill_manual(values = c("gray68", "steelblue2", "dodgerblue4"), labels = c("Free capacity","Used capacity (non-Covid-19)","Used capacity (Covid-19)")) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_date(date_breaks = waiver(), date_minor_breaks="1 week", limits = c(min(divi.long$date), max(divi.long$date))) +
  labs(title = "ICU Capacity and Its Usage in Germany", x = "", y = "Percent of total capacity", fill = "In % of total adult ICU capacity") +
  theme(legend.position = "bottom")
