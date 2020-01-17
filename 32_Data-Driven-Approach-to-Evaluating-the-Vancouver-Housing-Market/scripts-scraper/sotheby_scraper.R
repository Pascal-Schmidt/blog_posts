# libraries used for building scraper
library(tidyverse)
library(RSelenium)
library(rvest)
library(xml2)

# RSelenium 
# driver<- rsDriver(browser=c("chrome"))
# remDr <- driver[["client"]]


# creating all urls. Urls will be stored in object urls
sapply(2:41, function(x) {
  url <- "https://sothebysrealty.ca/en/search-results/region-greater-vancouver-british-columbia-real-estate/tloc-1/rloc-3/ptype-condo/price-0-1000000/view-grid/show-mls/sort-featured/pp-60/status-sales/page-"
  paste0(url, x) }) -> urls

# with the RSelenium package, we are scraping all links from the website. 
# Each page had 60 condos listed (60 links) and the website gave us 41 page results
# In total, we collected 2375 links
df_all <- data.frame()
for(i in 1:(length(urls))) {
  remDr$navigate(paste0(urls[[i]]))
  Sys.sleep(3)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'plink']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(3)
  df_all <- rbind(df_all, df)
}
  

# converting links stored in a data.frame() as factors to character type stored in a vector df
df <- sapply(df_all$link, as.character)

# initalize empty data frame where we will be storing our scraped data 
df_all_data <- data.frame()

# write our scraper function
scraper <- function(links) {
  
  # save link in url object
  url <- links
  # parse page url
  page <- xml2::read_html(url)
  Sys.sleep(0.25)
  
  #get house price
  house_price <- page %>% 
    rvest::html_nodes("ul") %>% 
    rvest::html_nodes(xpath = '//*[@class="price_social"]') %>% 
    rvest::html_text() %>%
    .[[1]]
  
  #get street address
  street_address <- page %>% 
    rvest::html_nodes("span") %>% 
    rvest::html_nodes(xpath = '//*[@class="span8"]') %>% 
    rvest::html_nodes("h1") %>%
    rvest::html_text()
  
  # getting the key facts from the condo
  # key facts are: building type, square feet, year built, bedrooms and bathrooms,
  # taxes, and age
  key_facts <- page %>% 
    rvest::html_nodes("ul") %>% 
    rvest::html_nodes(xpath = '//*[@class="key_facts"]') %>% 
    rvest::html_nodes("li") %>%
    rvest::html_text()
  
  # removing unnecessary content from the vector of strings and naming the vector elements
  key_facts %>%
    stringr::str_replace_all(., ".*: ", "") %>%
    purrr::set_names(., nm = stringr::str_replace_all(key_facts, ":.*", "") %>%
                       stringr::str_replace_all(., "[0-9]+", "") %>%
                       stringi::stri_trim_both(.)) -> key_facts
  
  # the following code assigns the scraped data for each condo where applicable
  # if information is not available, we are filling the observation with a NA value
  # for example, there are condos where taxes are not available
  # moreover, some condos are going to get build in the future, so age was not available
  
  # get the building type 
  building_type <- ifelse("Property Type" %in% names(key_facts),
                          key_facts[ grep("Property Type", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                          NA) 
  
  # get square feet
  square_feet <- ifelse("Living Space" %in% names(key_facts),
                        key_facts[ grep("Living Space", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                        NA)
  
  # get the number of bedrooms
  bedrooms <- ifelse("Bedrooms" %in% names(key_facts),
                     key_facts[ grep("Bedrooms", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                     NA)
  
  # get the number of bathrooms
  bathrooms <- ifelse("Bathrooms" %in% names(key_facts),
                      key_facts[ grep("Bathrooms", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                      NA)
  
  # get when the condo was built
  year_built <- ifelse("Year Built" %in% names(key_facts),
                       key_facts[ grep("Year Built", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                       NA)
  
  # get the age of the condo
  age <- ifelse("Approximate Age" %in% names(key_facts),
                key_facts[ grep("Age", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                NA)
  
  # get the taxes (property taxes)
  taxes <- ifelse("Other Taxes" %in% names(key_facts) | "Municipal Taxes" %in% names(key_facts),  
                  key_facts[ grep("taxes", names(key_facts), ignore.case = TRUE, value = TRUE) ], 
                  NA)
  
  # storing individual links in df_individual_page object
  df_individual_page <- data.frame(price = house_price,
                                   address = street_address,
                                   squares = square_feet,
                                   type = building_type,
                                   year = year_built,
                                   age = age,
                                   bed = bedrooms,
                                   bath = bathrooms,
                                   tax = taxes)
  
  # rbinding df_all_data and df_individual_page
  # <<- makes df_all_data a global variable. Making it available in the global environment
  df_all_data <<- rbind(df_all_data, df_individual_page)
}

# looping over all links in the vector and applying scraper function to each link
sapply(df, scraper)

# some data cleaning
df_all_data %>%
  tidyr::separate(., address, 
                  c("street", 
                    "city", 
                    "province"), 
                  sep = ", ") %>%
  tidyr::separate(., street, 
                  c("building_number", 
                    "street_number", 
                    "street_name_one", 
                    "street_name_two", 
                    "street_name_three"), 
                  sep = " ") %>%
  tidyr::unite(., "street_name", 
               c("street_name_one", 
                 "street_name_two", 
                 "street_name_three"), 
               sep = " ") %>%
  dplyr::mutate(., street_name = gsub(" NA", "", street_name)) -> sotheby_data

# write.csv(sotheby_data, "sotheby_data.csv", row.names = FALSE)

