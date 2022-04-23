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

ggsave(filename = "covp1-percByAge.png",
       plot=gg1,
       device="png",
       width=8,
       height=6,
       dpi=600)


############ WITH LEGEND:
## filter for surges of interest
ggplot(covid_ages1 %>% filter(Surge%in%c("Alpha Surge", "Delta Surge", "Omicron Surge")), aes(x=Surge, fill=`Age Range`)) + 
  ## look at proportions
  geom_bar(position="fill") +
  coord_cartesian(expand=FALSE) +
  scale_fill_brewer(palette="YlOrBr") +
  
  ## flip so younger is on bottom - have to reorder labels
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.00), labels=c("1.00", 0.75, "0.50", 0.25, "0.00"), trans="reverse") +
  
  theme_minimal() +
  labs(title="Percent of Total Cases Belonging to Each Age Group by Surge",
       subtitle = "#####",
       caption="Source: https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
       y=element_blank(),
       x=element_blank())


#################################################
# graph 2: num cases in ohio by strain

covid_ages <- ohioCovid %>% 
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

library(ggthemes)
# ggplot(covid_ages,
#        aes(x=Date, y=Cases, fill=Surge)) +
#   geom_col(width=1) + 
#   coord_cartesian(expand=FALSE) +
#   scale_fill_manual(values=c("red", "blue", "gray", "green", "gray"))  +
#   theme_minimal() +
#   theme(legend.position="none")
#   ##theme_economist()

ggplot(covid_ages,
       aes(x=Date, y=Cases)) +
  geom_line(aes(color=Surge)) + 
  coord_cartesian(expand=FALSE) +
  scale_color_manual(values=c("red", "blue", "gray", "green", "gray", "gray", "gray"))  +
  ## annotate surges
  annotate("text", x = as.Date("2021-01-01"), y = 15000, color="red", label="Alpha Surge") +
  annotate("text", x = as.Date("2021-09-20"), y = 10000, color="blue", label="Delta Surge") +
  annotate("text", x = as.Date("2021-10-01"), y = 30000, color="green", label="Alpha Surge") +
  theme_minimal() +
  theme(legend.position="none")

#################################################
# graph 3: vax rates by strain