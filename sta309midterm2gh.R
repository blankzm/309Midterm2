###################
## STA 309 Midterm 2
## Zoe Blank
## sta309midterm2gh.R
## Apr. 22

library(tidyverse)
## Turn off the summarize grouping messages...
options(dplyr.summarise.inform = FALSE)

##############
## Data Input
ohioCovid <- read_csv("https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv")
ohioVax <- read_csv("https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv")
ohioPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv") 

#############
## Count cases in each county
##   for different 'surges' -- including times that are not a surge
## The order of the surges here has no real impact on the final dataset
covid_cases <- ohioCovid %>%
  mutate(Surge = case_when(`Onset Date` %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
                           `Onset Date` %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
                           `Onset Date` %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
                           `Onset Date` %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
                           ##`Onset Date` >= as.Date("2021-03-01") ~ "Other, Post Vaccine",
                           TRUE ~ "Other, Pre Vaccine") ) %>%
  group_by(County, Surge) %>%
  summarize(Cases = sum(`Case Count`, na.rm=TRUE) )

# covid_ages <- ohioCovid %>% 
#   group_by(Date=`Onset Date`) %>%
#   summarize(Cases = sum(`Case Count`)) %>%
#   mutate(Surge = case_when(Date %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
#                            Date %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
#                            Date %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
#                            Date %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
#                            ##`Onset Date` >= as.Date("2021-03-01") ~ "Other, Post Vaccine",
#                            TRUE ~ "Other, Pre Vaccine") ) ##%>%
#   ##select(-c(Sex, `Admission Date`, `Date Of Death`)) ##%>% 
#   ##group_by(`Age Range`) 

# county_covid_bydate <- covidDF %>%
#   group_by(Date=`Onset Date`) %>%
#   summarize(Cases = sum(`Case Count`)) %>%
#   mutate(Surge = case_when(Date >= as.Date("2020-10-01") & Date <= as.Date("2021-02-28") ~ "Alpha",
#                            Date >= as.Date("2021-07-01") & Date <= as.Date("2021-10-31") ~ "Delta",
#                            Date >= as.Date("2021-12-01") ~ "Omicron",
#                            Date <  as.Date("2020-10-01") ~ "Initial Cases",
#                            TRUE ~ "Gaps")) %>%
#   mutate(Surge = factor(Surge, levels=c("Initial Cases", "Alpha", "Delta", "Omicron", "Gaps") ) ) 

covid_ages1 <- ohioCovid %>% 
  mutate(Surge = case_when(`Onset Date` %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
                           `Onset Date` %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
                           `Onset Date` %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
                           `Onset Date` %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
                           ##`Onset Date` >= as.Date("2021-03-01") ~ "Other, Post Vaccine",
                           TRUE ~ "Other, Pre Vaccine") ) %>%
  select(-c(Sex, `Admission Date`, `Date Of Death`, `Onset Date`, `County`)) %>% 
  group_by(`Age Range`, Surge)

#################################################
# graph 1: % of total covid cases by age per wave
# ggplot(covid_ages1) +
#   geom_bar(aes(y = `Case Count`, group = Surge, color = `Age Range`))

########## WITH ANNOTATIONS:
## filter for surges of interest
gg1 <- ggplot(covid_ages1 %>% filter(Surge%in%c("Alpha Surge", "Delta Surge", "Omicron Surge")), aes(x=Surge, fill=`Age Range`)) + 
  ## look at proportions
  geom_bar(position="fill") +
  coord_cartesian(expand=FALSE) +
  scale_fill_brewer(palette="YlOrBr") +
  
  ## flip so younger is on bottom - have to reorder labels
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.00), labels=c("100%", "75%", "50%", "25%", "0%"), trans="reverse") +
  
  annotate("text", x="Alpha Surge", y=0.94, label="0-19", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.83, label="20-29", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.71, label="30-39", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.58, label="40-49", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.45, label="50-59", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.31, label="60-69", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.16, label="70-79", size=5, family="serif") +
  annotate("text", x="Alpha Surge", y=0.05, label="Age Group: 80+", size=5, family="serif") +
  
  theme_minimal() +
  theme(legend.position = "none",
        plot.title=element_text(family="serif", size=16),
        plot.subtitle=element_text(family="serif"),
        axis.text = element_text(family="serif"),
        plot.caption=element_text(family="mono"),
        panel.grid.minor=element_blank()) +
  labs(title="Percent of Total Cases Belonging to Each Age Group",
       subtitle = "Grouped by surge.\nAge groups are labelled on the bar corresponding to the Alpha Surge; colors remain consistent across surges.\nWe note that the Delta and Omicron surges had higher impacts on the younger population compared to the Alpha surge.",
       caption="Source: https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
       y=element_blank(),
       x=element_blank())

##gg1

ggsave(filename = "covp1-percByAge.png",
       plot=gg1,
       device="png",
       width=8,
       height=6,
       dpi=600)


# ############ WITH LEGEND:
# ## filter for surges of interest
# ggplot(covid_ages1 %>% filter(Surge%in%c("Alpha Surge", "Delta Surge", "Omicron Surge")), aes(x=Surge, fill=`Age Range`)) + 
#   ## look at proportions
#   geom_bar(position="fill") +
#   coord_cartesian(expand=FALSE) +
#   scale_fill_brewer(palette="YlOrBr") +
#   
#   ## flip so younger is on bottom - have to reorder labels
#   scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.00), labels=c("1.00", 0.75, "0.50", 0.25, "0.00"), trans="reverse") +
#   
#   theme_minimal() +
#   labs(title="Percent of Total Cases Belonging to Each Age Group by Surge",
#        subtitle = "#####",
#        caption="Source: https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
#        y=element_blank(),
#        x=element_blank())


#################################################
# graph 2: num cases in ohio by strain

covid_ages2 <- ohioCovid %>% 
  group_by(Date=`Onset Date`) %>%
  summarize(Cases = sum(`Case Count`)) %>%
  ## add in specific others to avoid random lines in graph
  mutate(Surge = case_when(Date %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
                           Date %in% seq.Date(as.Date("2020-06-01"), as.Date("2020-09-30"), by="day") ~ "Other 1",
                           Date %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
                           Date %in% seq.Date(as.Date("2020-03-01"), as.Date("2020-06-30"), by="day") ~ "Other 2",
                           Date %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
                           Date %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
                           Date >= as.Date("2022-03-01") ~ "Other, Post Vaccine",
                           TRUE ~ "Pre Vaccine") ) ##%>%
##select(-c(Sex, `Admission Date`, `Date Of Death`)) ##%>% 
##group_by(`Age Range`) 
# 
# ohioCovidYoung <- ohioCovid %>% 
#   filter(`Age Range` == "0-19") %>% 
#   group_by(Date=`Onset Date`) %>%
#   summarize(Cases = sum(`Case Count`))
# 
# ohioCovidOld <- ohioCovid %>% 
#   filter(`Age Range` == "80+") %>% 
#   group_by(Date = `Onset Date`) %>% 
#   summarize(Cases = sum(`Case Count`))

library(ggthemes)
# ggplot(covid_ages,
#        aes(x=Date, y=Cases, fill=Surge)) +
#   geom_col(width=1) + 
#   coord_cartesian(expand=FALSE) +
#   scale_fill_manual(values=c("red", "blue", "gray", "green", "gray"))  +
#   theme_minimal() +
#   theme(legend.position="none")
#   ##theme_economist()

gg2 <- ggplot(covid_ages2,
       aes(x=Date, y=Cases)) +
  geom_line(aes(color=Surge)) + 
  
  # geom_line(ohioCovidYoung, mapping=aes(x=Date, y=Cases)) +
  # geom_line(ohioCovidOld, mapping=aes(x=Date, y=Cases)) +
  
  # ## trying something out - add line for 0-19
  # geom_line(covid_ages1 %>% filter(`Age Range` == "0-19"),
  #           mapping = aes(x=Date, y=Cases)) +
  
  annotate("rect", xmin=as.Date("2020-10-01"), xmax=as.Date("2021-02-28"), ymin=0, ymax=33000, fill="firebrick", alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-07-01"), xmax=as.Date("2021-11-30"), ymin=0, ymax=33000, fill="dodgerblue", alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-12-01"), xmax=as.Date("2022-02-28"), ymin=0, ymax=33000, fill="forestgreen", alpha=0.2) +
  
  scale_y_continuous(limits=c(0,35000)) +
  
  coord_cartesian(expand=FALSE) +
  scale_color_manual(values=c("firebrick", "dodgerblue", "gray", "forestgreen", "gray", "gray", "gray"))  +
  ## annotate surges
  annotate("text", x = as.Date("2020-12-15"), y = 33500, color="firebrick", label="Alpha Surge") +
  annotate("text", x = as.Date("2021-09-01"), y = 33500, color="dodgerblue", label="Delta Surge") +
  annotate("text", x = as.Date("2022-01-10"), y = 33500, color="forestgreen", label="Omicron Surge") +
  
  ## testing out significant dates
  ## vax available:*************
  ## geom_vline(aes(xintercept=as.Date("2020-12-14")), linetype="dotdash", alpha=0.5) +
  
  ## easter 2020 ********
  ##geom_vline(aes(xintercept=as.Date("2020-04-12")), alpha=0.5) +
  annotate("text", y=29000, x=as.Date("2020-04-12"), label="Easter 2020", size=3) +
  annotate("segment", x=as.Date("2020-04-12"), xend=as.Date("2020-04-12"), y=0, yend=28000, alpha=0.5) +
  
  ## many students go back to school **
  ##geom_vline(aes(xintercept=as.Date("2020-09-01")), alpha=0.5) +
  annotate("text", y=25000, x=as.Date("2020-09-01"), label="Students go back to school\nafter earlier shutdowns", size=3) +
  annotate("segment", x=as.Date("2020-09-01"), xend=as.Date("2020-09-01"), y=0, yend=24000, alpha=0.5) +
  
  ## second dose timeline:************
  annotate("text", y=29000, x=as.Date("2021-01-01"), label="People begin receiving\nsecond dose of vaccine", size=3) +
  ##geom_vline(aes(xintercept=as.Date("2021-01-01")), alpha=0.5) +
  annotate("segment", x=as.Date("2021-01-01"), xend=as.Date("2021-01-01"), y=0, yend=28000, alpha=0.5) +
  
  ## ohio closes schools:
  ## NOTE: not super significant
  ##geom_vline(aes(xintercept=as.Date("2020-03-12")), alpha=0.5) +
  
  ## covid health orders lifted (including mask mandate)
  ## geom_vline(aes(xintercept=as.Date("2021-06-02")), alpha=0.5) +
  annotate("text", y=25000, x=as.Date("2021-06-02"), label="COVID health orders\nlifted (including\nmask mandate)", size=3) +
  annotate("segment", x=as.Date("2021-06-02"), xend=as.Date("2021-06-02"), y=0, yend=24000, alpha=0.5) +
  
  
  ## school starts up after summer break 2021 ***
  ##geom_vline(aes(xintercept=as.Date("2021-08-01")), alpha=0.5) +
  annotate("text", y=29000, x=as.Date("2021-08-01"), label="School resumes\nafter summer 2021", size=3) +
  annotate("segment", x=as.Date("2021-08-01"), xend=as.Date("2021-08-01"), y=0, yend=28000, alpha=0.5) +
  
  ## thanksgiving 2021
  ##geom_vline(aes(xintercept=as.Date("2021-11-25")), alpha=0.5) +
  annotate("text", y=25000, x=as.Date("2021-11-10"), label="Thanksgiving\n2021", size=3) +
  annotate("segment", x=as.Date("2021-11-25"), xend=as.Date("2021-11-25"), y=0, yend=24000, alpha=0.5) +
  
  ## https://www.datanovia.com/en/blog/ggplot-date-axis-customization/
  scale_x_date(date_labels = "%b '%y") +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        plot.title=element_text(family="serif", size=16),
        plot.subtitle=element_text(family="serif"),
        axis.text = element_text(family="serif"),
        plot.caption=element_text(family="mono")) +
  labs(title="Timeline of Daily Covid Cases in Ohio by Surge",
       subtitle="Notable events are annotated. Data ranges from Jan. 2020 - present.",
       caption="Source:  https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
       y="Daily Cases")

gg2

##############################

# ohioCovidYoung <- ohioCovid %>% 
#   filter(`Age Range` == "0-19") %>% 
#   group_by(Date=`Onset Date`) %>%
#   summarize(Cases = sum(`Case Count`))

covid_cases_yng <- ohioCovid %>%
  filter(`Age Range` == "0-19") %>% 
  mutate(Surge = case_when(`Onset Date` %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
                           `Onset Date` %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
                           `Onset Date` %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
                           `Onset Date` %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
                           `Onset Date` >= as.Date("2021-03-01") ~ "Other, Post Vaccine",
                           TRUE ~ "Other, Pre Vaccine") ) %>%
  group_by(County, Surge) %>%
  summarize(Cases = sum(`Case Count`, na.rm=TRUE) )

# ohioCovidOld <- ohioCovid %>% 
#   filter(`Age Range` == "80+") %>% 
#   group_by(Date = `Onset Date`) %>% 
#   summarize(Cases = sum(`Case Count`))

 ##############
# covid_cases_old <- ohioCovid %>%
#   filter(`Age Range` == "80+") %>% 
#   mutate(Surge = case_when(`Onset Date` %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
#                            `Onset Date` %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
#                            `Onset Date` %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
#                            `Onset Date` %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
#                            `Onset Date` >= as.Date("2021-03-01") ~ "Other, Post Vaccine",
#                            TRUE ~ "Other, Pre Vaccine") ) %>%
#   group_by(County, Surge) %>%
#   summarize(Cases = sum(`Case Count`, na.rm=TRUE) )
# 
# youngpops <- ohioCovid %>% 
#   group_by(`Age Range`) %>% 
#   left_join(Ohio_age_pops, by="Age Range") %>% 
#   filter(`Age Range` == "0-19") %>% 
#   mutate(Case_rate_per1000 = `Case Count`/TotalPop*1000)
# 
# oldpops <- ohioCovid %>% 
#   group_by(`Age Range`) %>% 
#   left_join(Ohio_age_pops, by="Age Range") %>% 
#   filter(`Age Range` == "80+") %>% 
#   mutate(Case_rate_per1000 = `Case Count`/TotalPop*1000)
  ################

# ohio_populations <- ohioPop %>%
#   filter(YEAR==12) %>%
#   select(County=CTYNAME, Population=POPESTIMATE) %>%
#   mutate(County = str_remove_all(County, " County"))
# 
# ratesYoung <- covid_cases_yng %>%
#   left_join(ohio_populations, by="County") %>%
#   mutate(Case_rate_per1000 = Cases/Population*1000) ##%>%
#   # select(County, Surge, Case_rate_per1000) %>%
#   # pivot_wider(id_cols=County, names_from=Surge, values_from=Case_rate_per1000,
#   #             names_prefix="Case Rate ")
# 
# ratesOld <- covid_cases_old %>%
#   left_join(ohio_populations, by="County") %>%
#   mutate(Case_rate_per1000 = Cases/Population*1000)## %>%
#   # select(County, Surge, Case_rate_per1000) %>%
#   # pivot_wider(id_cols=County, names_from=Surge, values_from=Case_rate_per1000,
#   #             names_prefix="Case Rate ")

# ggplot() +
#   geom_smooth(ohioCovidYoung, mapping=aes(x=Date, y=Cases), span=0.1) +
#   geom_smooth(ohioCovidOld, mapping=aes(x=Date, y=Cases), span=0.1) 

# ggplot() +
#   annotate("rect", xmin=as.Date("2020-10-01"), xmax=as.Date("2021-02-28"), ymin=0, ymax=0.0051, fill="firebrick", alpha=0.2) +
#   annotate("rect", xmin=as.Date("2021-07-01"), xmax=as.Date("2021-11-30"), ymin=0, ymax=0.0051, fill="dodgerblue", alpha=0.2) +
#   annotate("rect", xmin=as.Date("2021-12-01"), xmax=as.Date("2022-02-28"), ymin=0, ymax=0.0051, fill="forestgreen", alpha=0.2) +
#   geom_smooth(data=youngpops, aes(x=`Onset Date`, y=Case_rate_per1000), span=0.1, se=FALSE, color="mediumvioletred") +
#   geom_smooth(data=oldpops, aes(x=`Onset Date`, y=Case_rate_per1000), span=0.1, se=FALSE, color="purple")

#################################################
# graph 3: vax rates by strain

## dont run this it breaks r
# vaxdata <- ohioVax %>%
#   left_join(ohioCovid, by=c("county"="County"))


## nvm this also breaks r
# ## merging takes a long time, so we will take the chance to only save the columns we want ahead of time:
# ohioCovidSmall <- ohioCovid %>% 
#   select(`Age Range`, `Onset Date`, `Case Count`)
# 
# # note - we'll work with vaccines_completed - have had 2 shots but no boosters
# ohioVaxSmall <- ohioVax %>% 
#   select(date, vaccines_completed)
# 
# vaxAges <- merge(ohioCovidSmall, ohioVaxSmall,
#                  by.x="Onset Date", by.y="date")

##vaxdata <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv") 


covid_vax <- ohioVax %>% 
  group_by(date) %>%
  summarize(totVax = sum(vaccines_started)) %>%
  ## add in specific others to avoid random lines in graph
  mutate(Surge = case_when(date %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
                           date %in% seq.Date(as.Date("2020-06-01"), as.Date("2020-09-30"), by="day") ~ "Other 1",
                           date %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
                           date %in% seq.Date(as.Date("2020-03-01"), as.Date("2020-06-30"), by="day") ~ "Other 2",
                           date %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
                           date %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
                           date >= as.Date("2022-03-01") ~ "Other, Post Vaccine",
                           TRUE ~ "Pre Vaccine") )


ggplot(covid_vax,
       aes(x=date, y=totVax, fill=Surge)) +
  geom_col(width=1) +
  coord_cartesian(expand=FALSE) +
  scale_fill_manual(values=c("red", "blue", "gray", "green", "gray"))  +
  theme_minimal() +
  theme(legend.position="none") +
  
  ## testing out vax dates
  ## vax available:*************
  geom_vline(aes(xintercept=as.Date("2020-12-14")), linetype="dotdash") +
  
  ## second dose timeline:************
  ##geom_vline(aes(xintercept=as.Date("2021-01-01"))) +
  
  ## 65 +
  geom_vline(aes(xintercept=as.Date("2021-01-18"))) +
  
  ## educators/childcare staff
  geom_vline(aes(xintercept=as.Date("2021-03-02"))) +
  
  ## 16+ (pfizer)/ 18+(jj / moderna)
  geom_vline(aes(xintercept=as.Date("2021-04-15"))) +
  
  ## fda approval(pfizer)
  geom_vline(aes(xintercept=as.Date("2021-09-24"))) +
  
  ## pfizer for 5+
  geom_vline(aes(xintercept=as.Date("2021-11-03"))) 


  ##theme_economist()


## testing w/ rct instead of lines
gg3 <- ggplot(covid_vax,
       aes(x=date, y=totVax)) +
  geom_col(width=1, alpha=0.5) +
  
  annotate("rect", xmin=as.Date("2020-12-12"), xmax=as.Date("2021-02-28"), ymin=0, ymax=108000, fill="firebrick", alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-07-01"), xmax=as.Date("2021-11-30"), ymin=0, ymax=108000, fill="dodgerblue", alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-12-01"), xmax=as.Date("2022-02-28"), ymin=0, ymax=108000, fill="forestgreen", alpha=0.2) +
  
  annotate("text", x = as.Date("2021-01-20"), y = 109000, color="firebrick", label="Alpha Surge") +
  annotate("text", x = as.Date("2021-09-01"), y = 109000, color="dodgerblue", label="Delta Surge") +
  annotate("text", x = as.Date("2022-01-10"), y = 109000, color="forestgreen", label="Omicron Surge") +
  
  geom_smooth(span=0.1, se=FALSE, color="black") +
  coord_cartesian(expand=FALSE) +
  scale_fill_manual(values=c("red", "blue", "gray", "green", "gray"))  +
  theme_minimal() +
  theme(legend.position="none") +
  
  ## testing out vax dates
  ## vax available:*************
  ##geom_vline(aes(xintercept=as.Date("2020-12-14")), linetype="dotdash") +
  annotate("rect", xmin=as.Date("2020-11-01"), xmax=as.Date("2020-12-14"), ymin=75000, ymax=85000, alpha=0) +
  annotate("text", x=as.Date("2020-12-12"), y=82000, label="First Available\nDec. '20", size=2) +
  annotate("segment", x=as.Date("2020-12-14"), xend=as.Date("2020-12-12"), y=80000, yend=0) +

 
  
  ## 65 +
  ##geom_vline(aes(xintercept=as.Date("2021-01-18")), linetype="dotdash") +
  ##annotate("rect", xmin=as.Date("2020-12-15"), xmax=as.Date("2021-01-18"), ymin=90000, ymax=110000, alpha=0.2) +
  annotate("text", x=as.Date("2021-01-18"), y=102000, label="65+ Age Group\nJan. '21", size=2) +
  annotate("segment", x=as.Date("2021-01-18"), xend=as.Date("2021-01-18"), y=100000, yend=0) +
  
  ## educators/childcare staff
  ##geom_vline(aes(xintercept=as.Date("2021-03-02")), linetype="dotdash") +
  ##annotate("rect", xmin=as.Date("2021-02-15"), xmax=as.Date("2021-04-01"), ymin=70000, ymax=90000, alpha=0.2) +
  annotate("text", x=as.Date("2021-02-28"), y=82000, label="Educators and Childcare staff\nMar. '21", size=2) +
  annotate("segment", x=as.Date("2021-03-02"), xend=as.Date("2021-03-02"), y=80000, yend=0) +
  
  ## 16+ (pfizer)/ 18+(jj / moderna)
  ##geom_vline(aes(xintercept=as.Date("2021-04-15")), linetype="dotdash") +
  ##annotate("rect", xmin=as.Date("2021-03-15"), xmax=as.Date("2021-05-15"), ymin=90000, ymax=110000, alpha=0.2) +
  annotate("text", x=as.Date("2021-04-13"), y=102000, label="16+ (Pfizer) or\n18+(J&J + Moderna) Age Groups\nApr. '21", size=2) +
  annotate("segment", x=as.Date("2021-04-15"), xend=as.Date("2021-04-15"), y=100000, yend=0) +
  
  ## fda approval(pfizer)
  ##geom_vline(aes(xintercept=as.Date("2021-09-24")), linetype="dotdash") +
  ##annotate("rect", xmin=as.Date("2021-09-01"), xmax=as.Date("2021-10-01"), ymin=70000, ymax=90000, alpha=0.2) +
  annotate("text", x=as.Date("2021-09-22"), y=82000, label="FDA approval for Pfizer\nSep. '21", size=2) +
  annotate("segment", x=as.Date("2021-09-24"), xend=as.Date("2021-09-24"), y=80000, yend=0) +
  
  ## pfizer for 5+
  ##geom_vline(aes(xintercept=as.Date("2021-11-03")), linetype="dotdash") +
  ##annotate("rect", xmin=as.Date("2021-10-15"), xmax=as.Date("2021-12-15"), ymin=90000, ymax=110000, alpha=0.2) +
  annotate("text", x=as.Date("2021-11-01"), y=102000, label="5+ Age Group (Pfizer)\nNov. '21", size=2)  +
  annotate("segment", x=as.Date("2021-11-03"), xend=as.Date("2021-11-03"), y=100000, yend=0) +
  
  scale_x_date(date_labels = "%b '%y") +
  scale_y_continuous(limits=c(0,110000)) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        plot.title=element_text(family="serif", size=16),
        plot.subtitle=element_text(family="serif"),
        axis.text = element_text(family="serif"),
        plot.caption=element_text(family="mono")) +
  labs(title="Timeline of Daily Vaccinations in Ohio by Surge",
       subtitle="Progression of vaccination guidelines is annotated. Vaccines observed are first doses. Data ranges from Dec. 2020 - present.\nColumns represent daily vaccinations administered. Smooth line represents local averaging performed by the loess method.",
       caption="Source:  https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv\n& https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv",
       y="Total Daily Vaccinations")


gg3



#####
gg3 <- ggplot(covid_vax,
              aes(x=date, y=totVax)) +
  geom_col(width=1, alpha=0.5) +
  
  ## annotate surges
  annotate("rect", xmin=as.Date("2020-12-12"), xmax=as.Date("2021-02-28"), ymin=0, ymax=108000, fill="firebrick", alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-07-01"), xmax=as.Date("2021-11-30"), ymin=0, ymax=108000, fill="dodgerblue", alpha=0.2) +
  annotate("rect", xmin=as.Date("2021-12-01"), xmax=as.Date("2022-02-28"), ymin=0, ymax=108000, fill="forestgreen", alpha=0.2) +
  
  annotate("text", x = as.Date("2021-01-20"), y = 110000, color="firebrick", label="Alpha Surge") +
  annotate("text", x = as.Date("2021-09-01"), y = 110000, color="dodgerblue", label="Delta Surge") +
  annotate("text", x = as.Date("2022-01-10"), y = 110000, color="forestgreen", label="Omicron Surge") +
  
  ## add smooth line to show general trend
  geom_smooth(span=0.1, se=FALSE, color="black") +
  
  ## some formatting:
  coord_cartesian(expand=FALSE) +
  scale_fill_manual(values=c("red", "blue", "gray", "green", "gray"))  +
  theme_minimal() +
  theme(legend.position="none") +
  
  ## testing out vax dates
  ## vax available:
  ## (note: fully transparent rectangle is to nudge axis so annotations will show)
  annotate("rect", xmin=as.Date("2020-11-01"), xmax=as.Date("2020-12-14"), ymin=75000, ymax=85000, alpha=0) +
  annotate("text", x=as.Date("2020-12-12"), y=82000, label="First Available\nDec. '20", size=2) +
  annotate("segment", x=as.Date("2020-12-14"), xend=as.Date("2020-12-12"), y=80000, yend=0) +
  
  
  
  ## available for 65 +
  annotate("text", x=as.Date("2021-01-18"), y=102000, label="65+ Age Group\nJan. '21", size=2) +
  annotate("segment", x=as.Date("2021-01-18"), xend=as.Date("2021-01-18"), y=100000, yend=0) +
  
  ## educators/childcare staff
  annotate("text", x=as.Date("2021-02-28"), y=82000, label="Educators and Childcare staff\nMar. '21", size=2) +
  annotate("segment", x=as.Date("2021-03-02"), xend=as.Date("2021-03-02"), y=80000, yend=0) +
  
  ## 16+ (pfizer)/ 18+(jj / moderna)
  annotate("text", x=as.Date("2021-04-13"), y=102000, label="16+ (Pfizer) or\n18+(J&J + Moderna) Age Groups\nApr. '21", size=2) +
  annotate("segment", x=as.Date("2021-04-15"), xend=as.Date("2021-04-15"), y=100000, yend=0) +
  
  ## fda approval(pfizer)
  annotate("text", x=as.Date("2021-09-22"), y=82000, label="FDA approval for Pfizer\nSep. '21", size=2) +
  annotate("segment", x=as.Date("2021-09-24"), xend=as.Date("2021-09-24"), y=80000, yend=0) +
  
  ## pfizer for 5+
  annotate("text", x=as.Date("2021-11-01"), y=102000, label="5+ Age Group (Pfizer)\nNov. '21", size=2)  +
  annotate("segment", x=as.Date("2021-11-03"), xend=as.Date("2021-11-03"), y=100000, yend=0) +
  
  ## more formatting:
  scale_x_date(date_labels = "%b '%y") +
  scale_y_continuous(limits=c(0,115000)) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        plot.title=element_text(family="serif"),
        plot.subtitle=element_text(family="serif"),
        axis.text = element_text(family="serif"),
        plot.caption=element_text(family="mono")) +
  labs(title="Timeline of Daily Vaccinations in Ohio by Surge",
       subtitle="Progression of vaccination guidelines is annotated. Vaccines observed are first doses.\nData ranges from Dec. 2020 - present. Columns represent daily vaccinations administered.\nSmooth line represents local averaging performed by the loess method.\nWe note that vaccine counts peak after certain group are approved for the vaccine,\nsuch as adults 65 and older, or educators.",
       caption="Source:  https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv\n& https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv",
       y="Total Daily Vaccinations")

ggsave(filename = "vaxTimelineCount.png",
       plot=gg3,
       device="png",
       width=8,
       height=6,
       dpi=600)

############### assembling dashboard

## to create dashboard:
library(patchwork)
# dash <- gg3 / (gg1 | gg2)
##dash <- gg2 + gg1 + gg3 + plot_layout(ncol=2)

# layout <- '
# A#
# BC
# '
# dash <- wrap_plots(A = gg2, B = gg1, C = gg2, design = layout)
# 

# custom_spacer <- ggplot() + theme_void()
# 
# design <- "
# 1#
# 23
# "

##gg1 <- gg1 + theme(aspect.ratio=3/4)
##gg3 <- gg3 + theme(aspect.ratio=3/4)

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, colour = 'black', size = 3))

gg1 <-  gg1 + plot_annotation(theme = theme_border) + theme(plot.margin = margin(1, 1, 1, 1, "cm"))
gg2 <-  gg2 + plot_annotation(theme = theme_border) + theme(plot.margin = margin(1, 1, 1, 1, "cm"))
gg3 <-  gg3 + plot_annotation(theme = theme_border) + theme(plot.margin = margin(1, 1, 1, 1, "cm")) + labs(caption = "Source:  https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv\n& https://www2.census.gov/programs-surveys/popest/datasets\n/2010-2019/counties/asrh/cc-est2019-agesex-39.csv")

dash <- (gg2 / (gg1 | gg3)) +
  theme(
        plot.subtitle = element_text(size=4),
        plot.caption = element_text(size=2)) +
  plot_annotation(
    title = 'COVID-19 Timeline and Age Group Effects in Ohio',
    theme = theme(plot.title = element_text(size = 25, family="serif", hjust=0.5)))
    ##theme = theme_border) ##+
  ##plot_layout(widths = c(3, 1, 2)) ##, design=design)

##dash <- gg1 / gg2 / gg3

# ggsave(filename = "dashTest1.png",
#        plot=dash,
#        device="png",
#        width=8,
#        height=6,
#        dpi=600)

ggsave(filename = "dashtest2.png",
       plot=dash,
       device="png",
       width=16,
       height=12,
       dpi=600)


