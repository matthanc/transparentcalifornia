library(rvest) #for webs scraping
library(tidyverse) #for tidy code
library(lubridate) #for date manipulation
#library(ggmap) used to get coordinates - no longer needed
library(gmt) #to calcualate distances between coordinates
library(humaniformat) #to parse names

# The purpose of the below function is to filter the Transparent California organizations by distance (in miles) from a specific/specified organization

tcfilter <- function(organization, filterdist) {
  
  #Scrape basic org info from sites
  tc_scrape1 <- read_html("https://transparentcalifornia.com/agencies/salaries/")
  tc_scrape2 <- read_html("https://transparentcalifornia.com/agencies/salaries/special-districts/")
  
  #Scrape org table information and back end attributes (for link structure)
  tc_tables1 <- tc_scrape1 %>% html_nodes("table") %>% html_table(fill = TRUE)
  tc_attrs1 <- tc_scrape1 %>% html_nodes("table td a") %>% html_attrs()
  tc_tables2 <- tc_scrape2 %>% html_nodes("table") %>% html_table(fill = TRUE)
  tc_attrs2 <- tc_scrape2 %>% html_nodes("table td a") %>% html_attrs()
  
  #Reduce is a purrr function that cleans the table and attribute data
  tc_orgs1 <- tc_tables1 %>% reduce(rbind)
  tc_url1 <- tc_attrs1 %>% reduce(rbind)
  tc_orgs2 <- tc_tables2 %>% reduce(rbind)
  tc_url2 <- tc_attrs2 %>% reduce(rbind)
  
  #Clean attribute data from both sites (for urls later)
  tc_url1 <- substr(tc_url1,16,100)
  tc_url2 <- substr(tc_url2,16,100)
  tc_url1 <- as_tibble(tc_url1)
  tc_url2 <- as_tibble(tc_url2)
  tc_url1 <- unique(tc_url1)
  tc_url2 <- unique(tc_url2)
  tc_url1 <- tc_url1 %>% mutate(href = gsub("/", "", tc_url1$href))
  tc_url2 <- tc_url2 %>% mutate(href = gsub("/", "", tc_url2$href))
  
  #Bind table data and cleaned attribute data together
  tc_orgs1 <- bind_cols(tc_orgs1, tc_url1)
  tc_orgs2 <- bind_cols(tc_orgs2, tc_url2)
  
  #Bind all basic org data together
  tc_orgs <- bind_rows(tc_orgs1, tc_orgs2)
  
  #Remove all but most current year for each org and keep data going back a maximum of 2 years (e.g., current year = 2021, only 2019 and newer is used)
  tc_orgs$X2 <- substr(tc_orgs$X2,1,4)
  tc_orgs <- tc_orgs %>% filter(str_detect(X2,"20"))
  tc_orgs <- tc_orgs %>% filter(str_detect(X2, as.character(year(now())-1)) |str_detect(X2, as.character(year(now())-2)))
  
  #Compile urls with attribute data and rename column names
  tc_orgs <- tc_orgs %>% mutate(href = paste0("https://transcal.s3.amazonaws.com/public/export/",tc_orgs$href,"-",tc_orgs$X2,".csv"))
  tc_orgs <- tc_orgs %>% rename(org = X1, year = X2, url = href)
  
  #Filter orgs by distance from specified/specific org location
  tc_orgs <- inner_join(tc_orgs, read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/coords.csv"), by = "org")
  lon <- as.numeric(filter(tc_orgs, org == organization) %>% select(coordinates.lon))
  lat <- as.numeric(filter(tc_orgs, org == organization) %>% select(coordinates.lat))
  tc_orgs_filtered <- tc_orgs %>% mutate(distance = geodist(lat, lon, tc_orgs$coordinates.lat, tc_orgs$coordinates.lon, units = "km") * 0.62137119) %>%
    filter(distance <= filterdist)
  
  #return filtered orgs
  return(tc_orgs_filtered)
}

#Function to filter by distance in miles. Org name must match name on Transparent California
tc_orgs_filtered <- tcfilter("San Francisco", 9999)

tcfilter_rds <- function(tc_orgs_filtered) {
  
  #Pull employee data from filtered orgs (this will take a few minutes depending on filter)
  df_filtered <- map_dfr(tc_orgs_filtered$url, ~ read_csv(.x) %>%
                           mutate(across(everything(), as.character)))
  
  #Remove special characters from names and job titles
  df_filtered$`Employee Name` <-  iconv(df_filtered$`Employee Name`, from = '', to = 'ASCII//TRANSLIT')
  df_filtered$`Job Title` <-  iconv(df_filtered$`Job Title`, from = '', to = 'ASCII//TRANSLIT')
  
  #Remove NA Base Pays and convert pay from num to int
  df_filtered <-  df_filtered %>% filter(!is.na(`Base Pay`))
  df_filtered[4:10] <- lapply(df_filtered[4:10], as.integer)
  
  #Determine if full-time or part-time employee
  df_filtered <- df_filtered %>%
    mutate(full_time = ifelse(`Total Pay` >= 25000 & Benefits > 0, "Yes","No"))
  
  #Filter agencies with less than 50 employees
  df_filtered <- df_filtered %>%
    group_by(Agency) %>%
    filter(n() > 50) %>%
    ungroup()
  
  #Parse employee names with humaniformat package
  df_filtered <- df_filtered %>% mutate(`Employee Name` = format_reverse(df_filtered$`Employee Name`))
  df_filtered <- df_filtered %>% mutate(`Employee Name` = format_period(df_filtered$`Employee Name`))
  df_filtered <- df_filtered %>% mutate(parse_names(df_filtered$`Employee Name`))
  
  #Compile email addresses with known list of formats and suffixes
  df_filtered <- left_join(df_filtered, read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/email.csv"), by = "Agency")
  df_filtered <- df_filtered %>% separate(email, c("emailstructure", "email suffix"), "@")
  df_filtered <- df_filtered %>%
    mutate(email =ifelse(df_filtered$emailstructure == "flast",paste0(substr(df_filtered$first_name,1,1),df_filtered$last_name,"@",df_filtered$`email suffix`),
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
  
  
  
  #Export to RDS file named transparentcalifornia-outreach.rds
  saveRDS(df_filtered, file = "output/transparentcalifornia-outreach.rds")
  
  return(df_filtered)
}

#Run function for filtered data
df_filtered <- tcfilter_rds(tc_orgs_filtered)
