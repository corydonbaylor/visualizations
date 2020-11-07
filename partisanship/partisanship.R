library(dplyr)
library(tidyr)

pop = read.csv("co-est2019-alldata.csv")

pop$COUNTY = sprintf("%03d", pop$COUNTY)
pop$STATE = sprintf("%02d", pop$STATE)

pop$FIPS = paste0(pop$STATE, pop$COUNTY)

pop =  pop%>%
  select(STNAME, CTYNAME, FIPS, ESTIMATESBASE2010)

# lets filter out the states
pop$test = grepl("County|city|Borough|Area|District|Parish|Municipality",  pop$CTYNAME)

pop = pop%>%
  filter(test == TRUE)

# creating cleaned fips code
data$FIPS = sprintf("%05d", data$FIPS)

cleaned = data%>%
  filter(party %in% c("republican", "democrat"))%>%
  mutate(election_id = paste(year, state_po, county, sep = "-"),
         percent = candidatevotes/totalvotes)%>%
  select(election_id, party, percent)%>%
  group_by(election_id, party)%>%
  summarise(percent = sum(percent))%>%
  ungroup()%>%
  spread(party, percent, fill = 0)%>%
  mutate(margin = abs(democrat - republican),
         dem_margin = democrat - republican,
         year = as.numeric(substr(election_id, 1, 4)))


yearly = cleaned%>%
  group_by(year)%>%
  summarise(margin = median(margin))


yearly_dem = cleaned%>%
  filter(dem_margin > 0)%>%
  group_by(year)%>%
  summarise(margin = median(margin))

yearly_rep = cleaned%>%
  filter(dem_margin < 0)%>%
  group_by(year)%>%
  summarise(margin = median(margin))


counties = data.frame(c("Los Angeles", "Cook", "Harris", "Maricopa", "San Diego", "Orange", "Miami-Dade", "Dallas", "Kings"))

