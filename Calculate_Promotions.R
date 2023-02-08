# Load See Through NY Data
library(viridis)
library(hrbrthemes)
all <- read_csv(file = "./SUNY_SBU_SEETHROUGHNYDATA_2008-2020.csv")

# Get at the number of years in service
yearsinservice <- all %>% group_by(Name) %>% summarise(ServiceYears = n()) %>%
  arrange(Name)


promotions <- all %>% group_by(Name) %>% summarise(N_Promotions = n_distinct(TitleAbbrv_SeeThroughNY)-1)


initialSL <- all %>% group_by(Name, `Likely Gender[as determined by name]`) %>% slice(which.min(`Pay Year`))

metrics <- left_join(yearsinservice, promotions, "Name")

all_metrics <- left_join(all, metrics, "Name") %>% arrange(Name)

write_csv(all_metrics, file = "./SUNY_SBU_SEETHROUGHNYDATA_2008-2020.csv")

# plot service years
all_metrics %>% select(Name,`Likely Gender[as determined by name]`, ServiceYears) %>% distinct() %>% 
  ggplot(aes(x=ServiceYears, group = `Likely Gender[as determined by name]`,fill = `Likely Gender[as determined by name]`, color = `Likely Gender[as determined by name]`)) +
  geom_histogram(alpha = 0.3, ) +
  scale_fill_viridis(discrete = T)+
  scale_color_viridis(discrete = T)

# plot promotions
all_metrics %>% filter(!is.na(`Likely Gender[as determined by name]`)) %>% select(Name,`Likely Gender[as determined by name]`, N_Promotions) %>% distinct() %>% 
  ggplot(aes(x=N_Promotions), y=stat(density*width),group = `Likely Gender[as determined by name]`,fill = `Likely Gender[as determined by name]`, color = `Likely Gender[as determined by name]`)) +
  geom_histogram(alpha = 0.3) +
  scale_fill_viridis(discrete = T)+
  scale_color_viridis(discrete = T) +
  theme_bw()


# PLOTTING PROPORTIONS
# Proporation Male & Female

# GRAB THE SLs - unclassified
SL_DATA <- x_full[grep("SL", x = x_full$UnclassifiedSL),]

SL_DATA %>% select(Name, gender.x, SG_UNCLASS, UnclassifiedSL) %>% distinct() %>% mutate(`Gender [assesed by name]`=gender.x) %>% 
  group_by(SG_UNCLASS) %>% mutate(TotalStaff = n()) %>% 
  ungroup() %>% group_by(UnclassifiedSL, gender.x) %>%
  mutate(Prop = length(gender.x)/TotalStaff) %>% 
  
  ggplot(aes(fill=`Gender [assesed by name]`, y=Prop, x=UnclassifiedSL)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_viridis(discrete = T)+
  ylab("Proportion of Employees at SUNY SBU") +
  xlab("Title within Instructional Family")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

SL_DATA$`Total Pay` <- as.numeric(gsub(pattern = "\\$|,", replacement = "", x = SL_DATA$`Total Pay`))
SL_DATA$`Rate of Pay` <- as.numeric(gsub(pattern = "\\$|,", replacement = "", x = SL_DATA$`Rate of Pay`))
instr$`Rate of Pay` <- as.numeric(gsub(pattern = "\\$|,", replacement = "", x = instr$`Rate of Pay`))

SL_DATA %>% mutate(`Gender [assesed by name]`=gender.x) %>% filter(!is.na(gender.x), `Pay Basis` == "Annual") %>% group_by(Name) %>% 
  mutate(meanPayRate = mean(`Rate of Pay`)) %>% 
  ggplot(aes(y=meanPayRate, x=UnclassifiedSL, fill=`Gender [assesed by name]`)) + 
  geom_violin(position=position_dodge(1), scale = "count") + 
  scale_fill_viridis(discrete = T)+
  ylab("Mean Total Pay for years 2018-2021") +
  xlab("Title within Instructional Family (Technical)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

AnnualPayBasis <- x_full %>% filter(`Pay Basis` == "Annual")
AnnualPayBasis$`Rate of Pay` <- as.numeric(gsub(pattern = "\\$|,", replacement = "", x = AnnualPayBasis$`Rate of Pay`))

ggplot(AnnualPayBasis, aes(y=`Rate of Pay`, x=gender.x)) + 
  geom_violin(position=position_dodge(1), scale = "count")