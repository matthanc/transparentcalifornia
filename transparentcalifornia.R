library(rvest)
library(tidyverse)


tc_scrape <- read_html("https://transparentcalifornia.com/agencies/salaries/")

tc_tables <- tc_scrape %>% html_table(fill = TRUE)

tc_orgs <- bind_rows(tc_tables[[1]], tc_tables[[2]], tc_tables[[3]], tc_tables[[4]], tc_tables[[5]])

tc_orgs$X2 <- substr(tc_orgs$X2,1,4)
tc_orgs <- tc_orgs %>% filter(str_detect(X2,"20"))
tc_orgs <- tc_orgs %>% mutate(url = tolower(tc_orgs$X1))
tc_orgs <- tc_orgs %>% mutate(url = gsub("college of the", "college", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = gsub("college of", "college", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = gsub("the state bar of california", "state bar california", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = gsub("coast community college district", "coast district", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = gsub("foothill deanza community college district", "foothill deanza district", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = gsub("los angeles community college district", "los angeles district", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = gsub(" ", "-", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = sub("\\.", "", tc_orgs$url))
tc_orgs <- tc_orgs %>% mutate(url = paste0("https://transcal.s3.amazonaws.com/public/export/",tc_orgs$url,"-",tc_orgs$X2,".csv"))
tc_orgs <- tc_orgs %>% rename(org = X1, year = X2)

df <- map_df(tc_orgs$url, read.csv)

df <- map_dfr(tc_orgs$url, ~ read_csv(.x) %>%
                mutate(across(everything(), as.character)))

#next steps, make the above workflow 1 of larger function
#need to prepare list of email suffixes - start by filtering only bay area cities and counties, or within 75 miles or so

tc_orgs <- full_join(tc_orgs, read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/coords.csv"), by = "org")

#next write a function that finds and filters distance from specific city on list of tc_orgs
#inputs cityname and distance from in miles or whatever possible

library(gmt)

geodist(-117,34.6,-119,34.2, units = "km")

filter(tc_orgs, org == "San Francisco") %>% select(coordinates.lon)
filter(tc_orgs, org == "San Francisco") %>% select(coordinates.lat)

function(organization, distance){
  lon <- filter(tc_orgs, org == organization) %>% select(coordinates.lon)
  lat <- filter(tc_orgs, org == organization) %>% select(coordinates.lat)
  
}
