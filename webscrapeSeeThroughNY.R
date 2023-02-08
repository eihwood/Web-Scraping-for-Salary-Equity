library(RSelenium)
library(tidyverse)
library(rvest)
library(xml2)
library(jsonlite)

# Using RSelenium Plus Rvest To Scrape The SeeThroughNY database
# So what we’re going to do here is use RSelenium to identify and navigate to the correct page, 
# then a mishmash of XML and Rvest to download the information on that individual page. 
# Lastly we’ll put everything we’ve done into a mix of functions,
# allowing us to use purrr to automate going through the entire site.

# Follow instructions to launch Docker from Terminal and use RSelenium to webscrape
# https://callumgwtaylor.github.io/post/using-rselenium-and-docker-to-webscrape-in-r-using-the-who-snake-database/


# And access our selenium browser using the RSelenium package:
# Here what we’re doing is creating an object in R that contains the information about the selenium browser we’ve created in a docker container. 
# Then we’re opening the browser.


# PART 1: OPEN BROWSER AND CONFIRM WE'RE AT THE LANDING PAGE
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "firefox")
remDr$open()

# Navigate to page - 88,036 results
numclicks <- round(100000/200, digits = 0) + 1
remDr$navigate("https://www.seethroughny.net/payrolls/227198648") #Entering our URL gets the browser to navigate to the page

remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer

# Find the load button and assign, then send click event.
load_btn <- remDr$findElement(using = "css selector", "#data_loader")



# Wait for elements to load.
for (i in 1:numclicks){
  load_btn$clickElement()
  Sys.sleep(2)
}




# find championship table in html via xpath
elem_chemp <- remDr$findElement(using="xpath", value='//*[@id="tbl_results"]/table')
# scrape the html table as a tibble

results <-read_html(elem_chemp$getElementAttribute('innerHTML')[[1]]) %>%
  html_text() 
# Clean up
results <- gsub("        ", "", results)
results <- gsub("    ", "", results)
results <- gsub(pattern = "\\n", replacement = "--", results)
results
# PARSE 
dat <- map(results, function(x) {
  tibble(text = unlist(str_split(x, pattern = "--"))) 
})

dat <- dat[[1]] 
dat$text[which(dat$text %in% c("", " "))] <- NA

cleandat <- dat %>% filter(text != " ")
cleandat <- cleandat %>% filter(text != " ")
N = length(cleandat$text[cleandat$text == "Pay Basis"])

df <- tibble(Name = rep(NA, N), `Total Pay` = rep(NA,N), `Employer/Agency` = rep(NA, N), `Subagency/Type` = rep(NA, N)) 
            
# Add Title
df$Title <- cleandat$text[which(cleandat$text == "Title")+1]
# get it out of cleandat
cleandat <- cleandat[-c(which(cleandat$text == "Title"), which(cleandat$text == "Title")+1),1]


# Add Rate of Pay
df$`Rate of Pay` <- cleandat$text[which(cleandat$text == "Rate of Pay")+1]
# get it out of cleandat
cleandat <- cleandat[-c(which(cleandat$text == "Rate of Pay"), which(cleandat$text == "Rate of Pay")+1),1]


# Add Pay Year
df$`Pay Year` <- cleandat$text[which(cleandat$text == "Pay Year")+1]
# get it out of cleandat
cleandat <- cleandat[-c(which(cleandat$text == "Pay Year"), which(cleandat$text == "Pay Year")+1),1]


# Add Pay Basis
df$`Pay Basis` <- cleandat$text[which(cleandat$text == "Pay Basis")+1]
# get it out of cleandat
cleandat <- cleandat[-c(which(cleandat$text == "Pay Basis"), which(cleandat$text == "Pay Basis")+1),1]


# Add Branch/Major Category
df$`Branch/Major Category` <- cleandat$text[which(cleandat$text == "Branch/Major Category")+1]
# get it out of cleandat
cleandat <- cleandat[-c(which(cleandat$text == "Branch/Major Category"), which(cleandat$text == "Branch/Major Category")+1),1]

# Add Subagency
df$`Subagency/Type` <- cleandat$text[which(cleandat$text == "SubAgency/Type")+1]
# get it out of cleandat
cleandat <- cleandat[-c(which(cleandat$text == "SubAgency/Type"), which(cleandat$text == "SubAgency/Type")+1),1]


# NOW SKIM OFF FIRST 5 ROWS SO THAT WE ARE JUST LEFT WITH first 4 categories
cleandat <- cleandat[-c(1:5),1]

totalpay <- cleandat$text[grep(pattern = "\\$", x = cleandat$text)]
df$`Total Pay` <- totalpay

df$`Employer/Agency` <- "SUNY"



cleandat <- cleandat[-c(grep(pattern = "\\$", x = cleandat$text)),1]
cleandat <- cleandat[-c(grep(pattern = "SUNY", x = cleandat$text)),1]


Names <- cleandat$text[grep(pattern = ",", x = cleandat$text)]
df$Name <- Names



# R GENDER??? 
library(gender)
install_genderdata_package()

# Get first name
first <- sub('.*,\\s*', '', df$Name)
first <- sapply(strsplit(x = first, split = " "), "[[", 1)
df$FirstName <- first

gender_results <- gender::gender(names = first, years = c(1953,2012))
gender_results <- gender_results %>% mutate(FirstName = name) %>% dplyr::select(-c(name)) %>% group_by(FirstName, gender) %>% summarise()
# Merge back in 
df_gen <- left_join(df, gender_results, by = "FirstName")

#devtools::install_github("kalimu/genderizeR")
library(genderizeR)
notassigned <- df_gen[which(is.na(df_gen$gender)),]

notassigned$genderizerinput <- sapply(strsplit(x = notassigned$Name, split = ","), "[[", 2)

names <- findGivenNames(notassigned$genderizerinput)
newassignment <- genderize(x = notassigned$genderizerinput, genderDB = names)

df_gen$text <- sapply(strsplit(x = df_gen$Name, split = ","), "[[", 2)

newassignment <- newassignment %>% select(text, gender) %>% group_by(text, gender) %>% summarise()

df_gen_full <- left_join(df_gen, newassignment, "text")

write_csv(x = df_gen_full, file = "./SUNY_Salary_Data_2008-2017.csv")
