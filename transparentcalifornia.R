library(rvest)         # for web scraping
library(dplyr)         # for data manipulation
library(purrr)         # for functional programming
library(stringr)       # for string operations
library(readr)         # for reading data
library(tidyr)         # for tidying data
library(tibble)        # for tibbles
library(lubridate)     # for date manipulation
library(geosphere)     # to calculate distances between coordinates
library(humaniformat)  # to parse names

# Ensure the 'output' directory exists
if (!dir.exists("output")) {
  dir.create("output")
}

# --- Email Generation Helper Functions ---
# Each function takes first_name (fn), last_name (ln), middle_name (mn), and email_suffix (suffix)
# and returns a formatted email string or NA if required parts are missing.

gen_email_flast <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(substr(fn, 1, 1), ln, "@", suffix)
}

gen_email_firstl <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(fn, substr(ln, 1, 1), "@", suffix)
}

gen_email_first.last <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(fn, ".", ln, "@", suffix)
}

gen_email_firstlast <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(fn, ln, "@", suffix)
}

gen_email_fmlast <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(mn) || is.na(suffix)) return(NA_character_)
  paste0(substr(fn, 1, 1), substr(mn, 1, 1), ln, "@", suffix)
}

gen_email_first <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(suffix)) return(NA_character_)
  paste0(fn, "@", suffix)
}

gen_email_last <- function(fn, ln, mn, suffix) {
  if (is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(ln, "@", suffix)
}

gen_email_first_last <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(fn, "_", ln, "@", suffix)
}

gen_email_f_first_4_last <- function(fn, ln, mn, suffix) { # Name adjusted for R syntax
  if (is.na(fn) || is.na(ln) || is.na(suffix) || nchar(ln) < 4) return(NA_character_)
  paste0(substr(fn, 1, 1), substr(ln, 1, 4), "@", suffix)
}

gen_email_filast <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix) || nchar(fn) < 2) return(NA_character_)
  paste0(substr(fn, 1, 2), ln, "@", suffix)
}

gen_email_lastfirst <- function(fn, ln, mn, suffix) {
  if (is.na(fn) || is.na(ln) || is.na(suffix)) return(NA_character_)
  paste0(ln, fn, "@", suffix) # Original was fn, ln; corrected based on common pattern
}

# Lookup table for email generation functions
email_formatters <- list(
  "flast" = gen_email_flast,
  "firstl" = gen_email_firstl,
  "first.last" = gen_email_first.last,
  "firstlast" = gen_email_firstlast,
  "fmlast" = gen_email_fmlast,
  "first" = gen_email_first,
  "last" = gen_email_last,
  "first_last" = gen_email_first_last,
  "f(first 4 letters of last)" = gen_email_f_first_4_last,
  "filast" = gen_email_filast,
  "lastfirst" = gen_email_lastfirst
)
# --- End Email Generation Helper Functions ---

tcfilter <- function(organization = "San Francisco", filterdist = 9999) {
  
  message(paste0("Starting tcfilter function for organization: '", organization, "' with distance: ", filterdist, " miles."))
  #Scrape basic org info from sites
  tc_scrape1 <- tryCatch(
    read_html("https://transparentcalifornia.com/agencies/salaries/"),
    error = function(e) {
      message(paste("Error reading URL: https://transparentcalifornia.com/agencies/salaries/ -", conditionMessage(e)))
      return(NULL)
    }
  )
  tc_scrape2 <- tryCatch(
    read_html("https://transparentcalifornia.com/agencies/salaries/special-districts/"),
    error = function(e) {
      message(paste("Error reading URL: https://transparentcalifornia.com/agencies/salaries/special-districts/ -", conditionMessage(e)))
      return(NULL)
    }
  )
  
  #Scrape org table information and back end attributes (for link structure)
  tc_tables1 <- list(); tc_attrs1 <- list() # Initialize
  if (!is.null(tc_scrape1)) {
    tryCatch({
      tc_tables1 <- tc_scrape1 %>% html_nodes("table") %>% html_table(fill = TRUE)
      tc_attrs1 <- tc_scrape1 %>% html_nodes("table td a") %>% html_attrs()
    }, error = function(e) {
      message(paste("Error processing HTML from https://transparentcalifornia.com/agencies/salaries/ -", conditionMessage(e)))
    })
  }
  
  tc_tables2 <- list(); tc_attrs2 <- list() # Initialize
  if (!is.null(tc_scrape2)) {
    tryCatch({
      tc_tables2 <- tc_scrape2 %>% html_nodes("table") %>% html_table(fill = TRUE)
      tc_attrs2 <- tc_scrape2 %>% html_nodes("table td a") %>% html_attrs()
    }, error = function(e) {
      message(paste("Error processing HTML from https://transparentcalifornia.com/agencies/salaries/special-districts/ -", conditionMessage(e)))
    })
  }
  
  col_names_orgs <- c("X1", "X2") 
  col_names_url <- c("href")    

  tc_orgs1 <- tibble(!!!setNames(rep(list(character()), length(col_names_orgs)), col_names_orgs))
  tc_url1 <- tibble(!!!setNames(rep(list(character()), length(col_names_url)), col_names_url))
  tc_orgs2 <- tibble(!!!setNames(rep(list(character()), length(col_names_orgs)), col_names_orgs))
  tc_url2 <- tibble(!!!setNames(rep(list(character()), length(col_names_url)), col_names_url))

  if (length(tc_tables1) > 0 && is.list(tc_tables1) && length(tc_tables1[[1]]) > 0) {
    tryCatch(tc_orgs1 <- tc_tables1 %>% reduce(rbind) %>% as_tibble(.name_repair = "minimal"), error = function(e) message(paste("Error reducing tc_tables1 -", conditionMessage(e))))
    if(!all(col_names_orgs %in% names(tc_orgs1))) tc_orgs1 <- tibble(!!!setNames(rep(list(character()), length(col_names_orgs)), col_names_orgs)) 
  }
  if (length(tc_attrs1) > 0 && is.list(tc_attrs1) && length(tc_attrs1[[1]]) > 0) {
    tryCatch(tc_url1 <- tc_attrs1 %>% purrr::map(~as_tibble(setNames(list(.), col_names_url))) %>% reduce(rbind), error = function(e) message(paste("Error reducing tc_attrs1 -", conditionMessage(e))))
    if(!all(col_names_url %in% names(tc_url1))) tc_url1 <- tibble(!!!setNames(rep(list(character()), length(col_names_url)), col_names_url)) 
  }
  if (length(tc_tables2) > 0 && is.list(tc_tables2) && length(tc_tables2[[1]]) > 0) {
    tryCatch(tc_orgs2 <- tc_tables2 %>% reduce(rbind) %>% as_tibble(.name_repair = "minimal"), error = function(e) message(paste("Error reducing tc_tables2 -", conditionMessage(e))))
    if(!all(col_names_orgs %in% names(tc_orgs2))) tc_orgs2 <- tibble(!!!setNames(rep(list(character()), length(col_names_orgs)), col_names_orgs)) 
  }
  if (length(tc_attrs2) > 0 && is.list(tc_attrs2) && length(tc_attrs2[[1]]) > 0) {
    tryCatch(tc_url2 <- tc_attrs2 %>% purrr::map(~as_tibble(setNames(list(.), col_names_url))) %>% reduce(rbind), error = function(e) message(paste("Error reducing tc_attrs2 -", conditionMessage(e))))
     if(!all(col_names_url %in% names(tc_url2))) tc_url2 <- tibble(!!!setNames(rep(list(character()), length(col_names_url)), col_names_url)) 
  }
  
  if ((nrow(tc_orgs1) == 0 && nrow(tc_orgs2) == 0)) {
    message("No data extracted from HTML tables after reduction. Exiting tcfilter function.")
    return(tibble())
  }

  if (nrow(tc_url1) > 0 && "href" %in% names(tc_url1)) {
    tc_url1$href <- substr(tc_url1$href,16,100)
    tc_url1$href <- gsub("/", "", tc_url1$href)
    tc_url1 <- unique(tc_url1)
  } else {
    tc_url1 <- tibble(href = character()) 
  }
  if (nrow(tc_url2) > 0 && "href" %in% names(tc_url2)) {
    tc_url2$href <- substr(tc_url2$href,16,100)
    tc_url2$href <- gsub("/", "", tc_url2$href)
    tc_url2 <- unique(tc_url2)
  } else {
     tc_url2 <- tibble(href = character()) 
  }

  if (nrow(tc_orgs1) > 0 && nrow(tc_url1) > 0) {
      if (nrow(tc_orgs1) == nrow(tc_url1)) {
          tc_orgs1 <- bind_cols(tc_orgs1, tc_url1)
      } else {
          message(paste("Row mismatch between tc_orgs1 (", nrow(tc_orgs1), ") and tc_url1 (", nrow(tc_url1), "). href will be NA for tc_orgs1."))
          tc_orgs1$href <- NA_character_ 
      }
  } else if (nrow(tc_orgs1) > 0) {
      tc_orgs1$href <- NA_character_ 
  }

  if (nrow(tc_orgs2) > 0 && nrow(tc_url2) > 0) {
      if (nrow(tc_orgs2) == nrow(tc_url2)) {
          tc_orgs2 <- bind_cols(tc_orgs2, tc_url2)
      } else {
          message(paste("Row mismatch between tc_orgs2 (", nrow(tc_orgs2), ") and tc_url2 (", nrow(tc_url2), "). href will be NA for tc_orgs2."))
          tc_orgs2$href <- NA_character_
      }
  } else if (nrow(tc_orgs2) > 0) {
      tc_orgs2$href <- NA_character_
  }

  tc_orgs <- bind_rows(tc_orgs1, tc_orgs2)
  
  if (nrow(tc_orgs) == 0) {
    message("No organization data to process after binding. Exiting tcfilter function.")
    return(tibble())
  }
  
  if (!"X2" %in% names(tc_orgs) || !"X1" %in% names(tc_orgs)) {
    message("Warning: Columns X1 or X2 not found in tc_orgs. Cannot filter by year or construct URLs. Returning empty tibble.")
    return(tibble())
  }
  tc_orgs$X2 <- substr(tc_orgs$X2,1,4)
  tc_orgs <- tc_orgs %>% filter(str_detect(X2,"20")) 
  current_year_val <- year(now())
  tc_orgs <- tc_orgs %>% 
    filter(!is.na(X2) & (X2 == as.character(current_year_val - 1) | X2 == as.character(current_year_val - 2) | X2 == as.character(current_year_val)))

  if (nrow(tc_orgs) == 0) {
    message("No organization data after year filtering. Exiting tcfilter function.")
    return(tibble())
  }
  
  if (!"href" %in% names(tc_orgs)) tc_orgs$href <- NA_character_

  tc_orgs <- tc_orgs %>% mutate(url = if_else(!is.na(href) & !is.na(X2) & href != "" & X2 != "", 
                                             paste0("https://transcal.s3.amazonaws.com/public/export/",href,"-",X2,".csv"), 
                                             NA_character_))
  tc_orgs <- tc_orgs %>% rename(org = X1, year = X2)
  
  coords_csv <- tryCatch(
    read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/coords.csv", col_types = cols(.default = "c")),
    error = function(e) {
      message(paste("Error reading coords.csv: -", conditionMessage(e)))
      return(NULL)
    }
  )

  if (is.null(coords_csv)) {
    message("Failed to load coordinates data. Cannot filter by distance. Returning current organizations without distance filter.")
    return(select(tc_orgs, org, year, url)) 
  }
  
  if (!"org" %in% names(tc_orgs) || !"org" %in% names(coords_csv)) {
      message("Critical error: 'org' column missing from tc_orgs or coords_csv. Cannot join. Returning current organizations without distance filter.")
      return(select(tc_orgs, org, year, url))
  }

  tc_orgs <- inner_join(tc_orgs, coords_csv, by = "org")
  
  if (nrow(tc_orgs) == 0) {
    message("No organization data after joining with coordinates. Exiting tcfilter function.")
    return(tibble())
  }

  if (!organization %in% tc_orgs$org) {
    message(paste("Specified organization '", organization, "' not found in the data after joining coordinates. Cannot filter by distance. Returning all orgs found within coordinate file."))
    return(select(tc_orgs, org, year, url, any_of(c("coordinates.lon", "coordinates.lat")))) 
  }
  
  if (!all(c("coordinates.lon", "coordinates.lat") %in% names(tc_orgs))) {
      message("Coordinate columns (coordinates.lon, coordinates.lat) not found after join. Cannot calculate distance. Returning joined data.")
      return(select(tc_orgs, org, year, url))
  }

  org_coords <- tc_orgs %>% filter(org == organization) %>% select(coordinates.lon, coordinates.lat)
  
  if (nrow(org_coords) == 0 || is.na(org_coords$coordinates.lon[1]) || is.na(org_coords$coordinates.lat[1])) {
    message(paste("Could not find valid coordinates for the specified organization:", organization, ". Cannot filter by distance. Returning all orgs found within coordinate file."))
    return(select(tc_orgs, org, year, url, any_of(c("coordinates.lon", "coordinates.lat"))))
  }
  lon <- as.numeric(org_coords$coordinates.lon[1])
  lat <- as.numeric(org_coords$coordinates.lat[1])

  tc_orgs_filtered <- tc_orgs %>% 
    filter(!is.na(coordinates.lon) & !is.na(coordinates.lat) & coordinates.lon != "" & coordinates.lat != "") %>% 
    mutate(distance = distHaversine(c(lon, lat), cbind(as.numeric(coordinates.lon), as.numeric(coordinates.lat))) * 0.000621371) %>%
    filter(distance <= filterdist)
  
  message("tcfilter function completed.")
  return(select(tc_orgs_filtered, org, year, url, distance)) 
}

tcfilter_rds <- function(tc_orgs_filtered) {
  message("Starting tcfilter_rds function...")
  if (is.null(tc_orgs_filtered) || nrow(tc_orgs_filtered) == 0 || !"url" %in% names(tc_orgs_filtered)) {
    message("Input tc_orgs_filtered is empty, NULL, or missing 'url' column. Skipping processing in tcfilter_rds.")
    return(tibble())
  }

  df_filtered <- map_dfr(tc_orgs_filtered$url, function(current_url) {
    if(is.na(current_url) || current_url == "") {
        message(paste("Skipping NA or empty URL."))
        return(NULL)
    }
    tryCatch({
      read_csv(current_url, col_types = cols(.default = "c")) %>%
        mutate(SourceURL = current_url) 
    }, error = function(e) {
      message(paste("Error reading or processing URL:", current_url, "-", conditionMessage(e)))
      return(NULL) 
    })
  })
  
  if (nrow(df_filtered) == 0) {
    message("No data successfully scraped from URLs in tcfilter_rds. Exiting.")
    return(tibble())
  }

  required_cols <- c("Employee Name", "Job Title", "Base Pay", "Total Pay", "Benefits", "Agency", "Year") # Added Year
  for(col_name in required_cols){
      if(!col_name %in% names(df_filtered)){
          message(paste0("Warning: Required column '", col_name, "' not found. Adding as NA column."))
          df_filtered[[col_name]] <- NA_character_ # Ensure it's character, numeric conversion handled later
      }
  }

  if ("Employee Name" %in% names(df_filtered)) {
    df_filtered$`Employee Name` <- iconv(df_filtered$`Employee Name`, from = '', to = 'ASCII//TRANSLIT', sub = " ") 
  }
  if ("Job Title" %in% names(df_filtered)) {
    df_filtered$`Job Title` <- iconv(df_filtered$`Job Title`, from = '', to = 'ASCII//TRANSLIT', sub = " ") 
  }
  
  # Pay column processing
  pay_cols_to_process <- c("Base Pay", "Overtime Pay", "Other Pay", "Total Pay", "Pension Debt", "Benefits")
  pay_cols_present <- intersect(pay_cols_to_process, names(df_filtered))

  if ("Base Pay" %in% pay_cols_present) { # Ensure Base Pay exists before filtering on it
      df_filtered <- df_filtered %>% filter(!is.na(`Base Pay`) & `Base Pay` != "")
  } else {
      message("Warning: 'Base Pay' column not found before numeric conversion. Skipping NA/empty filtering for it.")
  }
  
  df_filtered[pay_cols_present] <- lapply(df_filtered[pay_cols_present], function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x))))
  
  if (all(c("Total Pay", "Benefits") %in% names(df_filtered))) {
    df_filtered <- df_filtered %>%
      mutate(full_time = ifelse(!is.na(`Total Pay`) & `Total Pay` >= 25000 & !is.na(Benefits) & Benefits > 0, "Yes","No"))
  } else {
    message("Warning: 'Total Pay' or 'Benefits' column not found or not numeric. Skipping full-time determination.")
    df_filtered$full_time <- "Unknown" 
  }
  
  if ("Agency" %in% names(df_filtered)) {
    df_filtered <- df_filtered %>%
      group_by(Agency) %>%
      filter(n() > 50) %>%
      ungroup()
  } else {
     message("Warning: Column 'Agency' not found. Skipping agency size filtering.")
  }

  if (nrow(df_filtered) == 0) {
    message("No data remaining after agency size filtering. Exiting tcfilter_rds function.")
    return(tibble())
  }
  
  name_cols_to_add <- c("salutation", "first_name", "middle_name", "last_name", "suffix")
  for(nc in name_cols_to_add) { if(!nc %in% names(df_filtered)) df_filtered[[nc]] <- NA_character_ }

  if ("Employee Name" %in% names(df_filtered)) {
    df_filtered$`Employee Name` <- format_reverse(df_filtered$`Employee Name`) 
    df_filtered$`Employee Name` <- format_period(df_filtered$`Employee Name`)   
    parsed_names <- tryCatch(
      parse_names(df_filtered$`Employee Name`),
      error = function(e) {
        message(paste("Error parsing names with humaniformat:", conditionMessage(e)))
        return(tibble(salutation=NA_character_, first_name=NA_character_, middle_name=NA_character_, last_name=NA_character_, suffix=NA_character_)[1:nrow(df_filtered),]) 
      }
    )
    if(!is.null(parsed_names) && nrow(parsed_names) == nrow(df_filtered)){
        df_filtered <- df_filtered %>% select(-any_of(name_cols_to_add)) %>% bind_cols(parsed_names)
    }
  } else {
    message("Warning: Column 'Employee Name' not found. Skipping name parsing.")
  }
  
  email_csv <- tryCatch(
    read_csv("https://raw.githubusercontent.com/matthanc/transparentcalifornia/main/email.csv", col_types = cols(.default = "c")),
    error = function(e) {
      message(paste("Error reading email.csv:", conditionMessage(e)))
      return(NULL)
    }
  )

  df_filtered$email_final <- NA_character_ 

  if (is.null(email_csv) || !"Agency" %in% names(df_filtered)) {
    message("Failed to load email formats or 'Agency' column missing. Skipping email compilation.")
  } else {
    if (!"Agency" %in% names(email_csv)) {
        message("Warning: 'Agency' column not found in email_csv. Cannot join for email formats.")
    } else {
        df_filtered <- left_join(df_filtered, email_csv, by = "Agency")
        if (!"email" %in% names(df_filtered)) { 
            message("Warning: 'email' column (from email_csv) not found after join. Emails will be NA.")
        } else {
            df_filtered <- df_filtered %>% 
              mutate(email = ifelse(is.na(email), "NA@NA", email)) %>% 
              separate(email, c("emailstructure", "emailsuffix"), "@", fill = "right", extra = "merge") %>%
              mutate(emailstructure = ifelse(emailstructure == "NA", NA_character_, emailstructure),
                     emailsuffix = ifelse(emailsuffix == "NA", NA_character_, emailsuffix))

            df_filtered <- df_filtered %>%
              rowwise() %>%
              mutate(email_final = {
                formatter_func <- NULL
                if (!is.na(emailstructure) && emailstructure %in% names(email_formatters)) {
                  formatter_func <- email_formatters[[emailstructure]]
                }
                
                if (!is.null(formatter_func)) {
                  current_fn <- if ("first_name" %in% names(.)) first_name else NA_character_
                  current_ln <- if ("last_name" %in% names(.)) last_name else NA_character_
                  current_mn <- if ("middle_name" %in% names(.)) middle_name else NA_character_
                  formatter_func(current_fn, current_ln, current_mn, emailsuffix)
                } else {
                  NA_character_ 
                }
              }) %>%
              ungroup() 

            df_filtered$email_final <- tolower(df_filtered$email_final)
        }
    }
  }
  
  df_filtered <- df_filtered %>% select(-any_of(c("email", "emailstructure", "emailsuffix")))

  # --- START DATA VALIDATION ---
  message("Starting data validation...")
  if (nrow(df_filtered) > 0) {
    # 1. Key Column Data Types Validation
    numeric_cols_to_check <- c("Base Pay", "Total Pay", "Benefits")
    for (col_name in numeric_cols_to_check) {
      if (col_name %in% names(df_filtered)) {
        if (!is.numeric(df_filtered[[col_name]])) {
          message(paste0("Warning: Column '", col_name, "' is not numeric. Attempting conversion."))
          # Attempt conversion again if somehow it's not numeric (though prior steps should handle it)
          df_filtered[[col_name]] <- suppressWarnings(as.numeric(df_filtered[[col_name]]))
        }
        # Check percentage of NAs after conversion attempts
        na_percentage <- sum(is.na(df_filtered[[col_name]])) / nrow(df_filtered) * 100
        if (na_percentage > 50) { # Higher threshold for conversion failure
             message(paste0("Warning: Column '", col_name, "' has ", sprintf("%.1f", na_percentage), "% NA values after numeric conversion attempts. This might indicate issues with source data or parsing."))
        }
      } else {
        message(paste0("Warning: Numeric validation skipped for '", col_name, "' as it is not present in the dataframe."))
      }
    }

    character_cols_to_check <- c("Employee Name", "Job Title", "Agency", "Year")
    for (col_name in character_cols_to_check) {
      if (col_name %in% names(df_filtered)) {
        if (!is.character(df_filtered[[col_name]])) {
          message(paste0("Warning: Column '", col_name, "' is not character type. Converting to character."))
          df_filtered[[col_name]] <- as.character(df_filtered[[col_name]])
        }
      } else {
         message(paste0("Warning: Character validation skipped for '", col_name, "' as it is not present in the dataframe."))
      }
    }

    # 2. Email Format Validation
    if ("email_final" %in% names(df_filtered)) {
      # Basic regex: something@something.something
      # Does not cover all edge cases but good for typical formats.
      # Allows alphanumeric, period, underscore, hyphen in user/domain; domain must have at least one dot.
      email_regex <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
      invalid_email_formats <- df_filtered %>%
        filter(!is.na(email_final) & !str_detect(email_final, email_regex)) %>%
        nrow()
      if (invalid_email_formats > 0) {
        percentage_invalid_emails <- invalid_email_formats / sum(!is.na(df_filtered$email_final)) * 100
        if (percentage_invalid_emails > 5) { # If more than 5% of non-NA emails are invalid
            message(paste0("Warning: ", invalid_email_formats, " emails (", sprintf("%.1f", percentage_invalid_emails), "% of non-NA emails) do not match the expected format (e.g., user@domain.com)."))
        }
      }
    } else {
        message("Warning: Email validation skipped as 'email_final' column is not present.")
    }

    # 3. Missing Values Check (Threshold: 10% NA)
    cols_for_na_check <- c("Employee Name", "Job Title", "Base Pay", "email_final")
    for (col_name in cols_for_na_check) {
      if (col_name %in% names(df_filtered)) {
        na_percentage <- sum(is.na(df_filtered[[col_name]])) / nrow(df_filtered) * 100
        if (na_percentage > 10) {
          message(paste0("Warning: Column '", col_name, "' has ", sprintf("%.1f", na_percentage), "% NA values, which exceeds the 10% threshold."))
        }
      } else {
         message(paste0("Warning: Missing value check skipped for '", col_name, "' as it is not present in the dataframe."))
      }
    }
  } else {
    message("Data validation skipped as df_filtered is empty.")
  }
  message("Data validation finished.")
  # --- END DATA VALIDATION ---

  if (nrow(df_filtered) > 0) {
    saveRDS(df_filtered, file = "output/transparentcalifornia-outreach.rds")
    message("df_filtered saved to output/transparentcalifornia-outreach.rds")
  } else {
    message("No data in df_filtered to save. Skipping export to RDS.")
  }
  
  message("tcfilter_rds function completed.")
  return(df_filtered)
}

# Run main functions with checks
message("Starting script execution...")
tc_orgs_filtered_result <- tcfilter() 

if (!is.null(tc_orgs_filtered_result) && nrow(tc_orgs_filtered_result) > 0) {
  message(paste("tcfilter returned", nrow(tc_orgs_filtered_result), "organizations."))
  df_final_result <- tcfilter_rds(tc_orgs_filtered_result)
  if(!is.null(df_final_result) && nrow(df_final_result) > 0){
    message(paste("tcfilter_rds returned", nrow(df_final_result), "records."))
  } else {
    message("tcfilter_rds returned no data or an error occurred.")
  }
} else {
  message("tcfilter returned no data or an error occurred. Skipping tcfilter_rds.")
}
message("Script execution finished.")
