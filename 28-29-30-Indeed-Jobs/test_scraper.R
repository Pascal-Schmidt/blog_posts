first_page_url <- "https://www.indeed.ca/jobs?q=Data+Scientist&l=Burnaby%2C+BC"
library(magrittr)
page_result_start <- 20
page_result_end <- 180
page_results <- seq(from = page_result_start, to = page_result_end, by = 20)

full_df <- data.frame()
for(i in seq_along(page_results)) {
  
  url <- paste0(first_page_url, "&start=", page_results[i])
  page <- xml2::read_html(url)
  
  #get the job title
  job_title <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>%
    rvest::html_attr("title")
  
  #get the company name
  company_name <- page %>% 
    rvest::html_nodes("span")  %>% 
    rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
    rvest::html_text() %>%
    stringi::stri_trim_both() -> company.name 
  
  #get job location
  job_location <- page %>% 
    rvest::html_nodes("span") %>% 
    rvest::html_nodes(xpath = '//*[@class="location"]')%>% 
    rvest::html_text() %>%
    stringi::stri_trim_both()
  
  df <- data.frame(job_title, company_name, job_location)
  full_df <- rbind(full_df, df)
}
View(full_df)

write.csv(full_df, "test.csv")



vec <- c()
for(i in seq_along(page_results)) {
  url <- paste0(first_page_url, "&start=", page_results[i])
  page <- xml2::read_html(url)
  
  links <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")
  
  for(i in seq_along(links)) {
    
    url <- paste0("https://ca.indeed.com/", links[i])
    page <- xml2::read_html(url)
    
    job_description <- page %>%
      rvest::html_nodes("span")  %>% 
      rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
      rvest::html_text() %>%
      stringi::stri_trim_both()
    
    vec <- c(vec, job_description)
  }
  
}

vec_mod <- c(vec, NA, NA, NA)

full_data_set <- cbind(full_df, vec_mod)

write.csv(full_data_set, "scraper.csv")
full_df$job_title[[9]]
