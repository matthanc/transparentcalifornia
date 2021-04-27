library(rvest)
library(tidyverse)
library(lubridate)
library(ggmap)
library(gmt)

tcfilter <- function(organization, filterdist) {
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
  lon <- as.numeric(filter(tc_orgs, org == organization) %>% select(coordinates.lon))
  lat <- as.numeric(filter(tc_orgs, org == organization) %>% select(coordinates.lat))
  tc_orgs_filtered <- tc_orgs %>% mutate(distance = geodist(lat, lon, tc_orgs$coordinates.lat, tc_orgs$coordinates.lon, units = "km") * 0.62137119) %>%
    filter(distance <= filterdist)
  return(tc_orgs_filtered)
}

#Function to filter by distance in miles. Org name must match name on Transparent California
tc_orgs_filtered <- tcfilter("San Francisco", 75)

#Pull employee data from filtered orgs
df_filtered <- map_dfr(tc_orgs_filtered$url, ~ read_csv(.x) %>%
                mutate(across(everything(), as.character)))

#next write function to match email suffix to name
library(humaniformat)

df_filtered <- df_filtered %>% mutate(`Employee Name` = format_reverse(df_filtered$`Employee Name`))
df_filtered <- df_filtered %>% mutate(`Employee Name` = format_period(df_filtered$`Employee Name`))
df_filtered <- df_filtered %>% mutate(parse_names(df_filtered$`Employee Name`))
df_filtered <- left_join(df_filtered, read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/email.csv"), by = "Agency")
df_filtered <- df_filtered %>% separate(email, c("emailstructure", "email suffix"), "@")

df_filtered <- df_filtered %>% mutate(email = ifelse(df_filtered$emailstructure == "flast",paste0(substr(df_filtered$first_name,1,1),df_filtered$last_name,"@",df_filtered$`email suffix`),
                                              ifelse(df_filtered$emailstructure == "firstl",paste0(df_filtered$first_name,substr(df_filtered$last_name,1,1),"@",df_filtered$`email suffix`),
                                                     ifelse(df_filtered$emailstructure == "first.last",paste0(df_filtered$first_name,".",df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                            ifelse(df_filtered$emailstructure == "firstlast",paste0(df_filtered$first_name,df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                                   ifelse(df_filtered$emailstructure == "fmlast",paste0(substr(df_filtered$first_name,1,1),substr(df_filtered$middle_name,1,1),df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                                          ifelse(df_filtered$emailstructure == "first",paste0(df_filtered$first_name,"@",df_filtered$`email suffix`),
                                                                                 ifelse(df_filtered$emailstructure == "last",paste0(df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                                                        ifelse(df_filtered$emailstructure == "first_last",paste0(df_filtered$first_name,"_",df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                                                               ifelse(df_filtered$emailstructure == "f(first 4 letters of last)",paste0(substr(df_filtered$first_name,1,1),substr(df_filtered$last_name,1,4),"@",df_filtered$`email suffix`),
                                                                                                      ifelse(df_filtered$emailstructure == "filast",paste0(substr(df_filtered$first_name,1,2),df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                                                                             ifelse(df_filtered$emailstructure == "lastfirst",paste0(df_filtered$first_name,df_filtered$last_name,"@",df_filtered$`email suffix`),
                                                                                                                    ""))))))))))))
df_filtered$email <- tolower(df_filtered$email)




write.csv(df_filtered, file = "transparentcalifornia-outreach.csv")
