
library(stringdist)
library(tidyverse)
library(purrr)
#library(comparator)
library(magrittr)
library(RecordLinkage)
# merge title files

seethroughtitles <- read_csv(file = "./SUNY_Salary_Data_2008-2017.csv") %>% select(Title) %>% distinct() # 565 obs


unclassifiedtitles <- readxl::read_xlsx("./SUNY_SBU_Titles.xlsx") # 706 obs

classifiedtitles <- readxl::read_xlsx("./Title_Salary_Plan_ClassifiedCivilService.xlsx")


getBestMatch <- function(word, vector){
  purrr::map_dbl(charvec, ~RecordLinkage::jarowinkler(word, .x)) %>%
    magrittr::set_names(charvec) %>%
    which.max %>%
    names
}
charvec <- c(unclassifiedtitles$Title, classifiedtitles$`TITLE NAME`)
newtitles <- map_chr(seethroughtitles$Title, ~ getBestMatch(.x, charvec))

seethroughtitles$`TITLE NAME` <- newtitles

# Get salary levels 
SL <- gsub("^.* ", "", seethroughtitles$`TITLE NAME`)



seethroughtitles$UnclassifiedSL <- SL

# LEft join with the metadata for classified positions
newx <- left_join(seethroughtitles, classifiedtitles, "TITLE NAME")
SG_UNCL_numbers <- regmatches(newx$UnclassifiedSL, gregexpr("[[:digit:]]+", newx$UnclassifiedSL))  # Apply gregexpr & regmatches

SG_UNCL_numbers[sapply(SG_UNCL_numbers, is_empty)] <- NA

unlist(SG_UNCL_numbers)# Print list with numbers 

newx$SG_UNCLASS <- SG_UNCL_numbers

newx <- newx %>% mutate(SeeThroughNYAbbvTitle = Title) %>% 
  select(SeeThroughNYAbbvTitle, `TITLE NAME`, UnclassifiedSL, `TITLE CODE`, SG, JC, NU, FOC, LEVEL, `STD. NO.`, `AGENCY CODE`, `NBR POS`, `DECNTRL. LVL.`, `DUTIES ONLY`) %>% 
  arrange(`TITLE NAME`)

write_csv(x = newx, file = "./SBU_SeeThrough_Title_Lookup_FORERRORCHECKING.csv")
newx <- read_csv(file = "./SBU_Title_Lookup_metadata.csv")


# READ IN SCRAPED DATA
x <- read_csv("./SUNY_Salary_Data_2008-2017.csv")


x_full <- left_join(x, newx, "Title")
x_full$gender.x[is.na(x_full$gender.x)] <- x_full$gender.y[is.na(x_full$gender.x)]




final <- x_full %>% mutate(TitleAbbrv_SeeThroughNY = Title, 
                           `Total Pay` = as.numeric(gsub(pattern = "\\$|,", replacement = "", x = `Total Pay`)),
                           `Rate of Pay` = as.numeric(gsub(pattern = "\\$|,", replacement = "", x = `Rate of Pay`)),
                           TitleFull = `TITLE NAME`,
                           `Likely Gender[as determined by name]` = gender.x,
                           Classification = ifelse(is.na(SG), "Unclassified", "Classified")) %>%
  dplyr::select(`Name`, `Likely Gender[as determined by name]`,TitleAbbrv_SeeThroughNY, TitleFull, `Pay Year`,`Pay Basis`,`Total Pay`,`Rate of Pay`,
                Classification,UnclassifiedSL, `TITLE CODE`, SG, JC, NU, FOC, LEVEL, `STD. NO.`, `AGENCY CODE`, `NBR POS`, `DECNTRL. LVL.`,
                `Employer/Agency`, `Subagency/Type`, `Branch/Major Category`)



# FIX A FEW
final$UnclassifiedSL[final$Classification == "Classified"] <- NA

# Fix Sr ProgAnal
final[which(final$TitleAbbrv_SeeThroughNY== "Sr ProgAnal"), 4] <- "Senior Programmer-Analyst"
final[which(final$TitleAbbrv_SeeThroughNY== "Sr ProgAnal"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Sr ProgAnal"), 10] <- "(SL4)"


# FIX Visit Assoc Prof Gft
final[which(final$TitleAbbrv_SeeThroughNY== "Visit Assoc Prof Gft"), 4] <- "Visiting Associate Professor (GFT) (G4)"
final[which(final$TitleAbbrv_SeeThroughNY== "Visit Assoc Prof Gft"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Visit Assoc Prof Gft"), 10] <- "(G4)"

# FIX ADMISSIONS ASSISTANT
final[which(final$TitleAbbrv_SeeThroughNY== "Admissions Assistant"), 4] <- "Admissions Assistant (SL2)"
final[which(final$TitleAbbrv_SeeThroughNY== "Admissions Assistant"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Admissions Assistant"), 10] <- "(SL2)"

# FIX Assistant Provost
final[which(final$TitleAbbrv_SeeThroughNY== "Assistant Provost"), 4] <- "Assistant Provost (MP)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assistant Provost"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assistant Provost"), 10] <- "(MP)"


# Fix Assnt Alumni Affairs
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Alumni Affairs"), 4] <- "Assistant for Alumni Affairs (SL3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Alumni Affairs"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Alumni Affairs"), 10] <- "(SL3)"


# FIX Assnt College Regstr
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt College Regstr"), 4] <- "Assistant College Registrar (SL3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt College Regstr"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt College Regstr"), 10] <- "(SL3)"

# FIX Assnt Dir Admissions
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Admissions"), 4] <- "Assistant Director of Admissions (SL4)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Admissions"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Admissions"), 10] <- "(SL4)"

# FIX Assnt Dir College Hsg
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir College Hsg"), 4] <- "Assistant Director of College Housing (SL3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir College Hsg"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir College Hsg"), 10] <- "(SL3)"

# FIX Assnt Dir Comptng Sv
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Comptng Sv"), 4] <- "Assistant Director Computing Services (SL5)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Comptng Sv"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Comptng Sv"), 10] <- "(SL5)"


# FIX Assnt Dir Counseling
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Counseling"), 4] <- "Assistant Director of Counseling (SL5)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Counseling"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Counseling"), 10] <- "(SL5)"

# FIX Assnt Dir Fincl Aide
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Fincl Aide"), 4] <- "Assistant DirectorFinancial Aid (SL4)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Fincl Aide"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Dir Fincl Aide"), 10] <- "(SL4)"

# Assnt Fac Prog Coord
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Fac Prog Coord"), 4] <- "Assistant Facilities Program Coordinator (SL3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Fac Prog Coord"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Fac Prog Coord"), 10] <- "(SL3)"

# FIX 	Assnt Instnl Rsch
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Instnl Rsch"), 4] <- "Assistant for Institutional Research (SL3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Instnl Rsch"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Assnt Instnl Rsch"), 10] <- "(SL3)"

# FIX Visiting Profssr 10Mo
final[which(final$TitleAbbrv_SeeThroughNY== "Visitng Profssr 10mo"), 4] <- "Visitng Professor (10 month) (A3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Visitng Profssr 10mo"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Visitng Profssr 10mo"), 10] <- "(A3)"

# FIX Visiting Profssr 12Mo
final[which(final$TitleAbbrv_SeeThroughNY== "Visitng Profssr 12mo"), 4] <- "Visitng Professor (12 month) (B3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Visitng Profssr 12mo"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Visitng Profssr 12mo"), 10] <- "(B3)"

# FIX Th Cts Spch Pthlgy 2

final[which(final$TitleAbbrv_SeeThroughNY== "Th Cts Spch Pthlgy 2"), 4] <- "T.H. ClinicalTechServices Speech Pathologist II (SL4)"
final[which(final$TitleAbbrv_SeeThroughNY== "Th Cts Spch Pthlgy 2"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Th Cts Spch Pthlgy 2"), 10] <- "(SL4)"

# 	T H Staff Assnt 2
final[which(final$TitleAbbrv_SeeThroughNY== "T H Staff Assnt 2"), 4] <- "T.H. Staff Assistant II (SL1)"
final[which(final$TitleAbbrv_SeeThroughNY== "T H Staff Assnt 2"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "T H Staff Assnt 2"), 10] <- "(SL1)"

# 	T H Staff Assnt 1
final[which(final$TitleAbbrv_SeeThroughNY== "T H Staff Assnt 1"), 4] <- "T.H. Staff Assistant I (SL2)"
final[which(final$TitleAbbrv_SeeThroughNY== "T H Staff Assnt 1"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "T H Staff Assnt 1"), 10] <- "(SL2)"

# FIX Supvg Prgmr Anlst
final[which(final$TitleAbbrv_SeeThroughNY== "Supvg Prgmr Anlst"), 4] <- "Supervising Programmer-Analyst (SL5)"
final[which(final$TitleAbbrv_SeeThroughNY== "Supvg Prgmr Anlst"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Supvg Prgmr Anlst"), 10] <- "(SL5)"


# FIX Student U Assnt Dir
final[which(final$TitleAbbrv_SeeThroughNY== "Student U Assnt Dir"), 4] <- "Student Union Assistant Director (SL3)"
final[which(final$TitleAbbrv_SeeThroughNY== "Student U Assnt Dir"), 9] <- "Unclassified"
final[which(final$TitleAbbrv_SeeThroughNY== "Student U Assnt Dir"), 10] <- "(SL3)"

test <- final %>% select(TitleFull, TitleAbbrv_SeeThroughNY) %>% distinct()


final <- final %>% arrange(TitleAbbrv_SeeThroughNY, `Pay Year`) %>% filter(`Pay Year` != 2020)

write_csv(x = final, file = "./SUNY_SBU_SALARY_DATA_FINAL_2008-2017.csv")
  







