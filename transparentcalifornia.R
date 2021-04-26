library(rvest)
library(tidyverse)
library(lubridate)
library(ggmap)
library(gmt)

tc_scrape1 <- read_html("https://transparentcalifornia.com/agencies/salaries/")
tc_scrape2 <- read_html("https://transparentcalifornia.com/agencies/salaries/special-districts/")

tc_tables1 <- tc_scrape1 %>% html_nodes("table") %>% html_table(fill = TRUE)
tc_attrs1 <- tc_scrape1 %>% html_nodes("table td a") %>% html_attrs()
tc_tables2 <- tc_scrape2 %>% html_nodes("table") %>% html_table(fill = TRUE)
tc_attrs2 <- tc_scrape2 %>% html_nodes("table td a") %>% html_attrs()

tc_orgs1 <- tc_tables1 %>% reduce(rbind)
tc_url1 <- tc_attrs1 %>% reduce(rbind)
tc_orgs2 <- tc_tables2 %>% reduce(rbind)
tc_url2 <- tc_attrs2 %>% reduce(rbind)

tc_url1 <- substr(tc_url1,16,100)
tc_url2 <- substr(tc_url2,16,100)

tc_url1 <- as.tibble(tc_url1)
tc_url2 <- as.tibble(tc_url2)

tc_url1 <- unique(tc_url1)
tc_url2 <- unique(tc_url2)

tc_url1 <- tc_url1 %>% mutate(href = gsub("/", "", tc_url1$href))
tc_url2 <- tc_url2 %>% mutate(href = gsub("/", "", tc_url2$href))

tc_orgs1 <- bind_cols(tc_orgs1, tc_url1)
tc_orgs2 <- bind_cols(tc_orgs2, tc_url2)

tc_orgs <- bind_rows(tc_orgs1, tc_orgs2)

tc_orgs$X2 <- substr(tc_orgs$X2,1,4)

tc_orgs <- tc_orgs %>% filter(str_detect(X2,"20"))
tc_orgs <- tc_orgs %>% filter(str_detect(X2, as.character(year(now())-1)) |str_detect(X2, as.character(year(now())-2)))
tc_orgs <- tc_orgs %>% mutate(href = paste0("https://transcal.s3.amazonaws.com/public/export/",tc_orgs$href,"-",tc_orgs$X2,".csv"))
tc_orgs <- tc_orgs %>% rename(org = X1, year = X2, url = href)

tc_orgs <- inner_join(tc_orgs, read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/coords.csv"), by = "org")

tcfilter <- function(organization, filterdist) {
  lon <- as.numeric(filter(tc_orgs, org == organization) %>% select(coordinates.lon))
  lat <- as.numeric(filter(tc_orgs, org == organization) %>% select(coordinates.lat))
  tc_orgs_filtered <- tc_orgs %>% mutate(distance = geodist(lat, lon, tc_orgs$coordinates.lat, tc_orgs$coordinates.lon, units = "km") * 0.62137119) %>%
    filter(distance <= filterdist)
  return(tc_orgs_filtered)
}

tc_orgs_filtered <- tcfilter("San Francisco", 75)

df_filtered <- map_dfr(tc_orgs_filtered$url, ~ read_csv(.x) %>%
                mutate(across(everything(), as.character)))

test <- tail(tc_orgs, n=31)
testdf <- map_df(test$url, read.csv)

testdf <- map_dfr(test$url, ~ read_csv(.x) %>%
                mutate(across(everything(), as.character)))


#next steps, make the above workflow 1 of larger function
#need to prepare list of email suffixes - start by filtering only bay area cities and counties, or within 75 miles or so

#next write a function that finds and filters distance from specific city on list of tc_orgs
#inputs cityname and distance from in miles or whatever possible

