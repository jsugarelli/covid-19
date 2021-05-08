library(ggplot2)
library(ggrepel)
library(reshape2)
library(scales)
library(tidyverse)
library(lubridate)
library(stringr)


Sys.setlocale("LC_ALL","English")


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

divi <- as_tibble(read.csv("icu_usage.csv", header = TRUE, sep =";"))
divi <- divi[divi$Bundesland == "DEUTSCHLAND",]
divi <- divi[, c(1,4,5,6)]
names(divi) <- c("date", "covid", "used", "free")
divi$date <- ymd(substr(divi$date, 1, 10))

divi$capacity <- divi$used + divi$free
divi$usedp <- (divi$used / divi$capacity) * 100
divi$covidp <- (divi$covid / divi$capacity) * 100

divi$usedp <- ma(divi$usedp)
divi$covidp <- ma(divi$covidp)

divi$freep <- 100 - divi$usedp
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



# --------------- Data on vaccinations ---------------

# Germany
vacc_de <- as_tibble(read.csv("vaccinations_de.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE))
names(vacc_de)[1] <- "date"
population <- 83190556
vacc_de <- vacc_de[,c("date",
                      "personen_erst_kumulativ",
                      "personen_voll_kumulativ",
                      "impf_quote_erst",
                      "impf_quote_voll")]
names(vacc_de) <- c("date",
                    "firstvacc_de",
                    "secvacc_de",
                    "firstvaccrate_de",
                    "secvaccrate_de")
vacc_de$firstvaccrate_de <- vacc_de$firstvaccrate_de * 100
vacc_de$secvaccrate_de <- vacc_de$secvaccrate_de * 100
vacc_de$date <- dmy(vacc_de$date)

# United States
vacc_us <- as_tibble(read.csv("vaccinations_us.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE))
names(vacc_us) <- c("country", "code", "date", "firstvaccrate_us", "annotations")
vacc_us$date <- ymd(vacc_us$date)
vacc_us$firstvaccrate_us <- vacc_us$firstvaccrate_us
vacc_us <- vacc_us[vacc_us$country == "United States",]
vacc_us <- vacc_us[, c(3:4)]



# --------------- Merge and melt dataset ---------------

full <- inner_join(divi, deaths, by = c("date"))
full <- inner_join(full, inc, by = c("date"))
full <- left_join(full, vacc_de, by = c("date"))
full <- left_join(full, vacc_us, by = c("date"))
full <- full[full$date >= ymd("2020-10-14") &  full$date <= ymd("2021-05-07"), ]
full.long <- melt(full, id.vars = "date", value.name = "val")


# --------------- Graphs ---------------

coeff1 <- 12
graph <- ggplot() +
  geom_bar(data = subset(full.long, variable %in% c("freep", "covidp", "noncovidp")),
           aes(x = date, y = val, fill = variable), stat = "identity", width = 1,
           alpha = 0.75) +
  geom_line(data = subset(full.long, variable %in% c("incidence")),
            aes(x = date, y = val/coeff1, color = "col1"), size = 1.1) +
  geom_line(data = subset(full.long, variable %in% c("newdeaths")),
            aes(x = date, y = val/coeff1, color = "col2"), size = 1.1) +
  geom_line(data = subset(full.long, variable %in% c("firstvaccrate_de")),
            aes(x = date, y = val, color = "col3"), size = 1.1) +
  geom_line(data = subset(full.long, variable %in% c("firstvaccrate_us")),
            aes(x = date, y = val, color = "col4"), size = 1.1) +
  geom_vline(xintercept = ymd("2020-12-16"), linetype="solid", color = "red4",
             size=0.7) +
  geom_vline(xintercept = ymd("2020-11-02"), linetype="solid", color = "red4",
             size=0.7) +
  geom_vline(xintercept = ymd("2021-03-01"), linetype="solid", color = "darkgreen",
             size=0.7) +
  geom_vline(xintercept = ymd("2020-12-26"), linetype="solid", color = "royalblue4",
             size=0.7) +
  geom_vline(xintercept = ymd("2021-01-14"), linetype="solid", color = "royalblue4",
             size=0.7) +
  geom_vline(xintercept = ymd("2021-02-08"), linetype="solid", color = "royalblue4",
             size=0.7) +
  geom_label(aes(x = ymd("2020-11-02"), y = 85, label = "Lockdown \"light\""),
             fill = "white", color = "red4") +
  geom_label(aes(x = ymd("2020-12-16"), y = 95, label = "Lockdown \"hard\""),
             fill = "white", color = "red4") +
  geom_label(aes(x = ymd("2021-03-01"), y = 85, label = "Start of cautious\nre-openings"),
             fill = "white", color = "darkgreen") +
  geom_label(aes(x = ymd("2020-12-26"), y = 85, label = "BioNTech vacc.\navailable"),
             fill = "white", color = "royalblue4") +
  geom_label(aes(x = ymd("2021-01-14"), y = 85, label = "Moderna vacc.\navailable"),
             fill = "white", color = "royalblue4") +
  geom_label(aes(x = ymd("2021-02-08"), y = 85, label = "AstraZeneca vacc.\navailable"),
             fill = "white", color = "royalblue4") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val/coeff1),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "incidence",],
                  size = 3.5, fontface = "bold", color = "darkorange2", nudge_x = 1,
                  segment.colour = "transparent") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val/coeff1),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "newdeaths",],
                  size = 3.5, fontface = "bold", color = "red2", nudge_x = 1,
                  nudge_y = 0, segment.colour = "transparent") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "firstvaccrate_de",],
                  size = 3.5, fontface = "bold", color = "springgreen3", nudge_x = 1,
                  nudge_y = 0, segment.colour = "transparent") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "firstvaccrate_us",],
                  size = 3.5, fontface = "bold", color = "gray52", nudge_x = 1,
                  segment.colour = "transparent") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "covidp",],
                  size = 3.5, fontface = "bold", color = "dodgerblue3", nudge_x = 2,
                  nudge_y = -13, segment.colour = "transparent") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "noncovidp",],
                  size = 3.5, fontface = "bold", color = "steelblue2", nudge_x = 1,
                  segment.colour = "transparent") +
  geom_text_repel(aes(label = round(val, 1), x = date, y = val),
                  data = full.long[full.long$date == max(full.long$date) & full.long$variable == "freep",],
                  size = 3.5, fontface = "bold", color = "gray68", nudge_x = 2, nudge_y = 80,
                  segment.colour = "transparent") +
  scale_fill_manual(values = c("freep" = "gray68",
                               "noncovidp" = "steelblue2",
                               "covidp" = "dodgerblue3"),
                    labels = c("freep" = "Free ICU capacity, 7-day roll. avg.",
                               "noncovidp" = "Used ICU capacity (non-Covid-19), 7-day roll. avg.",
                               "covidp" = "Used ICU capacity (Covid-19), 7-day roll. avg.")) +
  scale_color_manual(values = c(col1 = "darkorange2",
                                col2 = "red2",
                                col3 = "springgreen3",
                                col4 = "gray52"),
                     labels = c("7-day incidence",
                                "Death cases (7-day roll. avg.)",
                                "Vacc. rate (at least one dose) Germany",
                                "Vacc. rate (at least one dose) US")) +
  scale_y_continuous(breaks = seq(0, 100, 5),
                     sec.axis = sec_axis(~.*coeff1, name = "Death cases / 7-day incidence",
                                         breaks = seq(0, 1250, by = 50))) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               limits = c(min(full.long$date), max(full.long$date))) +
  labs(title = "",
       x = "", y = "Percent of total ICU capacity / Vaccination rates",
       fill = "Fill", color = "Lines") +
  theme(text = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11, colour = "grey33", face = "plain"),
        legend.position = "bottom",
        legend.key.size = unit(12, "points"),
        legend.background = element_rect(fill = "gray94"),
        axis.title = element_text(size = 13, face = "plain")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE),
         color = guide_legend(nrow = 2, byrow = TRUE))
