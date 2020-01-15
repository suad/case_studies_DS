#Assignment 3 Case Studies CT 5103
#Marta Olszewska - 13101828
#Suad Al Darra - 17234582

library(dplyr)
library(ckanr)
library(ggplot2)

ckanr_setup(url = "https://data.gov.ie/")

data <- package_show('op-waiting-list-by-group-hospital')

for (i in 2:length(data$resources)){
  year <- 2020-i
  VarName <- paste("data", year, sep = "")
  
  dfTemp <- read.csv(data$resource[[i]]$url, header=T, sep=",")
  #knowing data fo 2018 is unstructured, headers are 
  if(year == 2018){
    dfTemp <- read.csv(data$resource[[i]]$url, header=F, sep=",")
  }
  # if column name is 'ï..Archive.Date' change it to 'Archive.Date' - aesthetic reasons for further processing
  colnames(dfTemp)[names(dfTemp) == "ï..Archive.Date"] <- "Archive.Date"
  dfTemp$Year <- year
  assign(VarName, dfTemp)
  
}

# Rename column names of 2017 dataset to be the same as other datasets column names
colnames(data2017)[names(data2017) == "Age.Profile"] <- "Age.Categorisation"
colnames(data2017)[names(data2017) == "Speciality"] <- "Specialty"
colnames(data2017)[names(data2017) == "Total"] <- "Count"

# Add column names to dataset for 2018
colnames(data2018) <- c('Archive.Date', 'Group', 'Hospital.HIPE', 'Hospital', 'Specialty.HIPE', 
                        'Specialty', 'Adult.Child', 'Age.Categorisation', 'Time.Bands', 'Count', 'Year')

# Bind the datasets into one dataset
data1 <- do.call("rbind", list(data2018, data2017, data2016, data2015, data2014))
head(data1)

#-------- preparing data for the visualizations---------

specialty_counts <- data1 %>% group_by(Year, Specialty) %>%  summarize(Outpatients=sum(Count,na.rm=T)) %>% arrange(desc(Outpatients))
# get top 5 specialties for scale colour manual setting
top5_specialties <- list(unique(specialty_counts$Specialty)[1:5])
tp <- c("Otolaryngology (ENT)", "Orthopaedics", "Dermatology", "Ophthalmology", "General Surgery")
#save the data as a csv file in local directory
write.csv(specialty_counts,'OP_waiting_specialty.csv')

waitTime_outpatients <- data1 %>% group_by(Year, Time.Bands, Adult.Child) %>%  summarize(Outpatients=sum(Count, na.rm=T))
waitTime_outpatients <- waitTime_outpatients[waitTime_outpatients$Adult.Child!=" ",]
#save the data as a csv file in local directory
write.csv(waitTime_outpatients,'OP_waiting.csv')
# example summarised data
head(waitTime_outpatients)

group_summary <- data1 %>% group_by(Year, Time.Bands, Group) %>%  summarize(Outpatients=sum(Count, na.rm=T))
#save the data as a csv file in local directory
write.csv(group_summary,'OP_waiting_HospitalGroup.csv')

age_summary <- data1 %>% group_by(Year, Age.Categorisation, Time.Bands) %>%  summarize(Outpatients=sum(Count, na.rm=T)) 
age_summary <- age_summary[age_summary$Age.Categorisation!="",]
#save the data as a csv file in local directory
write.csv(age_summary,'OP_waiting_AgeGroup.csv')

hospital_outpatient <- data1 %>% group_by(Year, Hospital) %>%  summarize(Outpatients=sum(Count, na.rm=T)) %>% arrange(desc(Outpatients))
# get top 5 hospitals for scale colour manual setting
top5_hospitals <- list(unique(hospital_outpatient$Hospital)[1:8])
th <- c("Galway University Hospital","Mater Misericordiae University Hospital","University Hospital Waterford","University Hospital Limerick","Tallaght Hospital",  "Beaumont Hospital", "Cork University Hospital", "South Infirmary Victoria University Hospital")
#save the data as a csv file in local directory
write.csv(hospital_outpatient ,'OP_waiting_Hospital.csv')

#------end of data preparation-------------------------

# ----------specialty plot (outpatients by year and specialty)-----------
#creating the plot from data and setting all the plot parameters we want/need
specialties_plot<- ggplot(specialty_counts, aes(x=Year, y=Outpatients)) + 
  scale_x_continuous(name="Year", lim=c(2014,2018)) +
  scale_y_continuous(breaks =seq(from = 0, to =805000, by=50000)) + # turns off scientific notation on y axis
  geom_line( aes(group = Specialty),size= 0.25, na.rm = TRUE, color="grey77", alpha =0.6, show.legend = FALSE )

specialties_plot +  
  geom_line(size =1, alpha=0.85, show.legend = TRUE, (aes(x=Year, y=Outpatients, colour= Specialty, group = Specialty))) +
  #highlight only 5 specialties of highest outpatients values
  scale_colour_manual(values = c("green4","royalblue3", "red3","orange4","darkorchid1" ), name = NULL, limits = tp) +
  labs(colour="Specialty") +
  ggtitle("Number of Patients by Specialty")+
  # this theme clears away grid lines, makes backgound white, sets all fonts, etc
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_text(face = "bold"),
        axis.title.y=element_text(face = "bold"),
        axis.text.x = element_text(size=8, angle = 90, hjust=1, vjust = .5),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position= "bottom"
  ) 
# -----waiting time plot (outpatients by year and waiting time)---
ggplot(waitTime_outpatients , aes(x=Year, y=Outpatients, color=Time.Bands, na.rm = TRUE))+
  
  geom_line(na.rm = TRUE, alpha=0.8) +
  geom_smooth(method=lm,se=FALSE, color = "black")  +
  scale_size_area(max_size = 10) +
  scale_x_continuous(name="Year", lim=c(2014,2018)) +
  ggtitle("Number of Patients by Their Waiting Time") +
  labs(colour="Waiting Time") +
  facet_wrap(~ Adult.Child) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
        axis.line = element_line(colour="black", size=0.25), 
        axis.title.x=element_text(face = "bold"), 
        axis.title.y=element_text(face = "bold"), 
        axis.text.x = element_text(size=8, angle = 90, hjust=1, vjust = .5),
        legend.position= "bottom", 
        legend.key = element_rect(fill = NA, colour = NA, size = 0.1), 
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"))


# ----------hospital group plot (outpatients by year and waiting time )-----------
group_plot<- ggplot(group_summary, aes(x=Year, y=Outpatients)) + 
  scale_x_continuous(name="Year", lim=c(2014,2018)) +
  scale_y_continuous(breaks =seq(from = 0, to =400000, by=80000)) + # turns off scientific notation on y axis
  geom_line( aes(group = Group), size= 0.25, na.rm = TRUE, color="grey90", alpha =0.6, show.legend = FALSE )

group_plot +  
  geom_line(size =1, alpha=0.85, show.legend = TRUE, (aes(x=Year, y=Outpatients, colour= Group, group = Group))) +
  labs(colour="Group") +
  ggtitle("Number of Patients by Hospital Group and Waiting Time")+
  facet_wrap(~ Time.Bands) +
  # this theme clears away grid lines, makes backgound white, sets all fonts, etc
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_text(face = "bold"),
        axis.title.y=element_text(face = "bold"),
        axis.text.x = element_text(size=8, angle = 90, hjust=1, vjust = .5),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        legend.text = element_text(size = 5, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position= "bottom") 

# ----------age group plot (outpatients by year and waiting time )-----------
group_plot<- ggplot(age_summary, aes(x=Year, y=Outpatients)) + 
  scale_x_continuous(name="Year", lim=c(2014,2018)) +
  scale_y_continuous(breaks =seq(from = 0, to =1500000, by=80000)) + # turns off scientific notation on y axis
  geom_line( aes(group = Time.Bands), size= 0.25, na.rm = TRUE, color="grey90", alpha =0.6, show.legend = FALSE )

group_plot +  
  geom_line(size =1, alpha=0.85, show.legend = TRUE, (aes(x=Year, y=Outpatients, colour= Time.Bands, group = Time.Bands))) +
  labs(colour="Waiting Time") +
  ggtitle("Number of Patients by Age Group and Waiting Time")+
  facet_wrap(~ Age.Categorisation) +
  # this theme clears away grid lines, makes backgound white, sets all fonts, etc
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_text(face = "bold"),
        axis.title.y=element_text(face = "bold"),
        axis.text.x = element_text(size=8, angle = 90, hjust=1, vjust = .5),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position= "bottom"
  ) 


# ----------hospital plot (outpatients by year )-----------
hospital_plot<- ggplot(hospital_outpatient, aes(x=Year, y=Outpatients)) + 
  scale_x_continuous(name="Year", lim=c(2014,2018)) +
  scale_y_continuous(breaks =seq(from = 0, to =600000, by=50000)) + # turns off scientific notation on y axis
  geom_line( aes(group = Hospital), color="grey77", size= 0.25, na.rm = TRUE, alpha =0.6, show.legend = FALSE )

hospital_plot +  
  geom_line(size =1, alpha=0.85, show.legend = TRUE, (aes(x=Year, y=Outpatients, colour= Hospital, group = Hospital))) +
  scale_colour_manual(values = c("green4","royalblue3", "red3","orange4","darkorchid4", "hotpink2","gold2","lightblue4", "darkturquoise"  ), name = NULL, limits = th) +
  labs(colour="Hospital") +
  ggtitle("Number of Patients by Hospital")+
  # this theme clears away grid lines, makes backgound white, sets all fonts, etc
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_text(face = "bold"),
        axis.title.y=element_text(face = "bold"),
        axis.text.x = element_text(size=8, angle = 90, hjust=1, vjust = .5),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        legend.text = element_text(size = 7, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position= "bottom")
