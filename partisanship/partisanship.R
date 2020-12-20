library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

setwd("visualizations/partisanship/")

pop = read.csv("co-est2019-alldata.csv")

pop$COUNTY = sprintf("%03d", pop$COUNTY)
pop$STATE = sprintf("%02d", pop$STATE)

pop$FIPS = paste0(pop$STATE, pop$COUNTY)

pop =  pop%>%
  select(STNAME, CTYNAME, FIPS, POPESTIMATE2019)

# lets filter out the states
pop$test = grepl("County|city|Borough|Area|District|Parish|Municipality",  pop$CTYNAME)

pop = pop%>%
  filter(test == TRUE)%>%
  select(-test)

# creating cleaned fips code
data$FIPS = sprintf("%05d", data$FIPS)

cleaned = data%>%
  filter(party %in% c("republican", "democrat"))%>%
  mutate(election_id = paste(year, FIPS, sep = "-"),
         percent = candidatevotes/totalvotes)%>%
  select(election_id, party, percent)%>%
  group_by(election_id, party)%>%
  summarise(percent = sum(percent))%>%
  ungroup()%>%
  spread(party, percent, fill = 0)%>%
  mutate(margin = abs(democrat - republican),
         dem_margin = democrat - republican,
         year = as.numeric(substr(election_id, 1, 4)),
         FIPS = substr(election_id, 6, 11),
         dem_win = ifelse(dem_margin > 0, 1, 0))%>%
  select(-election_id)%>%
  filter(FIPS != "000NA")

# adding population
cleaned = cleaned%>%
  left_join(pop)


yearly = cleaned%>%
  group_by(year)%>%
  summarise(margin = median(margin))

# lets find dem counties
types = cleaned%>%
  group_by(FIPS)%>%
  summarise(wins = sum(dem_win))

blue = types%>%
  filter(wins == 5)

red = types%>%
  filter(wins == 0)

purple = types %>%
  filter(!wins %in% c(5, 0))

yearly_dem = cleaned%>%
  filter(dem_margin > 0)%>%
  group_by(year)%>%
  summarise(democrat = median(democrat),
            republican = median(republican))%>%
  gather("party", "share", -year)

yearly_rep = cleaned%>%
  filter(dem_margin < 0)%>%
  group_by(year)%>%
  summarise(democrat = median(democrat),
            republican = median(republican))%>%
  gather("party", "share", -year)

# biggest counties
big = cleaned%>%
  arrange(desc(POPESTIMATE2019))%>%
  slice(1:50)%>%
  group_by(year)%>%
  summarise(margin = median(margin))

ggplot(yearly_dem)+
  geom_line(aes(x = year, y = share, group = party, color= party), size = 1.1)+
  theme_minimal()+
  theme(
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent)+
  scale_color_manual(values = c("blue", "red"))+
  labs(y = "Share of Vote", x = "Election Year", title = "Median Vote Shares for Counties with a Democratic Victory")


ggplot(yearly_rep)+
  geom_line(aes(x = year, y = share, group = party, color= party), size = 1.1)+
  theme_minimal()+
  theme(
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent)+
  scale_color_manual(values = c("blue", "red"))+
  labs(y = "Share of Vote", x = "Election Year", title = "Median Vote Shares for Counties with a Republican Victory")



