# Data-Science-Project-in-R-Uber-Data-Analysis-Project
---
-title: "Uber Data Analysis"
-author: "ERICK@Guru"
-output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Project in R – Uber Data Analysis Project
we will analyze the Uber Pickups in the New York City dataset. This is more of a data visualization project that will guide you towards using the ggplot2 library for understanding the data and for developing an intuition for understanding the customers who avail the trips. Talking about our Uber data analysis project, data storytelling is an important component of Machine Learning through which companies can understand the background of various operations. With the help of visualization, companies can avail the benefit of understanding complex data and gain insights that would help them to craft decisions. You will learn how to implement the ggplot2 on the Uber Pickups dataset and at the end, master the art of data visualization in R. Uber Dataset

### IMPORT ESSENTIAL LIBRARIES
```{r warning=FALSE, message=FALSE}
library(tidyverse)#data cleansing/manipulation/wrangling
library(ggplot2)#backbone of this project
library(lubridate)#manipulate date variable
library(ggthemes)#add-on to ggplot2create extra themes
library(DT)# help to interface with javascript
library(dplyr)#data manipulation/wrangling
library(tidyr)#help tidy your data
library(scales)#map data into correct scales
```


## 1.) Reading the Data into their designated variables

Now, we will read several csv files that contain the data from April 2014 to September 2014. We will store these in corresponding data frames like apr_data, may_data, etc. After we have read the files, we will combine all of this data into a single dataframe called ‘data_2014’.

```{r warning=FALSE, message=FALSE}
#import datasets
april <- read_csv("uber-raw-apr data.csv")
may <- read_csv("uber-raw-data-may14.csv")
june <- read_csv("uber-raw-data-jun14.csv")
july <- read_csv("uber-raw-data-jul14.csv")
august <-read_csv("uber-raw-data-aug14.csv")
sept <- read_csv("uber-raw-data-sep14.csv")
```

```{r  warning=FALSE, message=FALSE}
#Manipulating Date variable into correct format
april$`Date/Time` <- mdy_hm(april$`Date/Time`)
may$`Date/Time` <- mdy_hms(may$`Date/Time`)
june$`Date/Time` <- mdy_hms(june$`Date/Time`)
july$`Date/Time` <- mdy_hms(july$`Date/Time`)
august$`Date/Time` <- mdy_hms(august$`Date/Time`)
sept$`Date/Time` <- mdy_hms(sept$`Date/Time`)
```

## 3.) Data Manipulation/ EDA

```{r warning=FALSE, message=FALSE}
#combining data into one
data_2014 <- rbind(
  april, may, june, july, august,sept
)
head(data_2014)#check the first 6 columns
```

```{r}
#check missing values
colSums(is.na(data_2014))#no missing values
sum(is.na(data_2014))#
```

```{r warning=FALSE, message=FALSE}
#CHECK DATA STRUCTURE
# str(data_2014)
glimpse(data_2014)
dim(data_2014)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/1c527069-b64c-4fe5-9653-efa51e3f6cd6)

```{r warning=FALSE, message=FALSE}
names(data_2014)
```

### Lets Create the variables for Day of of the month, hour, day of the week, and the month

```{r warning=FALSE, message=FALSE}

data_2014 <- data_2014 %>%
     mutate(
       hour=factor(hour(`Date/Time`)),
       day=factor(day(`Date/Time`)),
       wday=factor(wday(`Date/Time`, label=TRUE)),
       month=factor(month(`Date/Time`, label=TRUE)),
       year=factor(year(`Date/Time`))
     )
head(data_2014, 4)#view the first 4 Rows
```

### Plotting Trips By the Hour of the Day

We group the data by hour of the day to get the summary total, after , we then visualize to get insights:

```{r warning=FALSE, message=FALSE}
#hourly Trips
hour_data <- data_2014 %>% 
    group_by(hour) %>% 
  dplyr::summarise(total=n())

datatable(hour_data) 
```

```{r warning=FALSE, message=FALSE}
#Trips hourly

ggplot(hour_data, aes(hour, total)) + 
       geom_bar(stat = "identity", 
           fill = "deepskyblue", 
           color = "black") + 
          ggtitle("Trips Every Hour") + 
          theme(legend.position = "none") +  scale_y_continuous(labels = comma)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/80f996a9-c1fd-4d3f-8149-0662952260d1)

```{r warning=FALSE, message=FALSE}
#month Hour Trips
month_hour <- data_2014 %>% 
     group_by(month, hour) %>% 
    summarise(Total=n())
DT::datatable(month_hour)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/7c573a93-3502-4d1b-a02b-4640bfd6bcbb)

```{r  warning=FALSE, message=FALSE}
#visualize

month_hour %>%  ggplot(aes(hour, Total, fill=month))+
     geom_bar(stat = 'identity',)+
    ggtitle('Total Trips Per Hour and Month') +
  scale_y_continuous(labels = comma)

```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/734f30ec-539a-4010-a7a0-fda434f0682a)

## Plotting data by trips during every day of the month 

In this section of R project, we will learn how to plot our data based on every day of the month. We observe from the resulting visualization that the 30th of the month had the highest trips in the year which is mostly contributed by the month of April.

```{r warning=FALSE, message=FALSE}
daily_trips <- data_2014 %>% 
    group_by(day) %>% 
     summarise(Total=n())
 DT::datatable(daily_trips)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/33fdd17a-5c57-4ad8-bfad-f1e4835221ae)

```{r warning=FALSE, message=FALSE}
daily_trips %>% ggplot(aes(day, Total)) +
      geom_bar(stat = 'identity', fill='orange')+
     ggtitle('Total Trips Per Day')+
    theme(legend.position = 'none')+
     scale_y_continuous(labels = comma)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/f08facfa-45f0-4926-8ff0-7e31a41c8ed3)

```{r warning=FALSE, message=FALSE}
#day and month total
day_month <- data_2014 %>% 
    group_by(month, day) %>% 
  summarise(total=n())

DT::datatable(day_month)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/f529d7ca-ef1d-4b5b-b281-08e570ccd445)

```{r warning=FALSE, message=FALSE}
theme_set(theme_test())
day_month %>% 
  ggplot(aes(day, total, fill=month))+
     geom_bar(stat='identity')+
     ggtitle('Trips by Day and Month') +
   scale_y_continuous(labels = comma)+
  theme(legend.position = "none")
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/9337e418-92c5-4297-aeea-c31afd1cfbb2)

### Number of Trips taking place during months in a year

In this section, we will visualize the number of trips that are taking place each month of the year. In the output visualization, we observe that most trips were made during the month of September. Furthermore, we also obtained visual reports of the number of trips that were made every day of the week.

```{r warning=FALSE, message=FALSE}
month_trips <- data_2014 %>% 
  group_by(month) %>% 
  summarize(total=n())

DT::datatable(month_trips)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/2677560f-09ac-4563-abf9-a894517bf990)

```{r warning=FALSE, message=FALSE}
month_trips %>% 
  ggplot(aes(month, total, fill=month))+ 
  geom_bar(stat = 'identity') +
  ggtitle('Trips by Month of the Year')+
  scale_y_continuous(labels = comma)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/2d12bfdb-81de-49d1-a455-e787b8309c26)

```{r warning=FALSE, message=FALSE}
#trips by weekday

day_of_week <- data_2014 %>% 
  group_by(wday) %>% 
  summarise(total=n())

DT::datatable(day_of_week)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/ae99f7bc-7a2a-4c0a-8ad0-279c68eb118c)

```{r warning=FALSE, message=FALSE}
day_of_week %>% 
  ggplot(aes(wday, total, fill=wday))+
  geom_bar(stat = 'identity')+
  scale_y_continuous(labels = comma)+
  ggtitle('Total Week Day Trips')
  
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/3ad8c4b9-ffde-4176-9ec7-466445a3c1a7)

### Finding out the number of Trips by bases 

In the following visualization, we plot the number of trips that have been taken by the passengers from each of the bases. There are five bases in all out of which, we observe that B02617 had the highest number of trips. Furthermore, this base had the highest number of trips in the month B02617. Thursday observed the highest trips in the three bases – B02598, B02617, and B02682.

```{r warning=FALSE, message=FALSE}
base_total <- data_2014 %>% 
  group_by(Base) %>% 
  summarise(total=n()) 

DT::datatable(base_total)

```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/bb780bc6-e5c9-4afc-bab0-0fac6e60fc14)

```{r warning=FALSE, message=FALSE}
base_total %>% 
  ggplot(aes(Base, total, fill=Base))+
  geom_bar(stat = 'identity')+
  ggtitle('Total Trips by Base')+
  scale_y_continuous(labels = comma)
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/1c406eae-5563-4635-b169-fcae10fbe680)

### Creating a map visualization of rides in New York 

In the final section, we will visualize the rides in New York City by creating a geo-plot that will help us to visualize the rides during 2014 (Apr-Sep) and by the bases in the same period.

```{r warning=FALSE, message=FALSE}
min_lat <- 40.5774
max_lat <- 40.9176 
min_long <- -74.15 
max_long <- -73.7004
```

```{r warning=FALSE, message=FALSE}
ggplot(data_2014, aes(x=Lon, y=Lat)) + 
  geom_point(size=1, color = "deepskyblue") + scale_x_continuous(limits=c(min_long, max_long)) + scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map() + 
ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/4160ee95-ced6-4c81-8954-be46e569a2f1)

```{r warning=FALSE, message=FALSE}
ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) + geom_point(size=1) + 
  scale_x_continuous(limits=c(min_long, max_long)) + scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map() + 
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")
```
![image](https://github.com/LangatErick/Data-Science-Project-in-R-Uber-Data-Analysis-Project/assets/124883947/ec734605-9c8a-4532-84ce-ddb9b40c96fd)

# **Summary**
At the end of the Uber data analysis R project, we observed how to create data visualizations. We made use of packages like ggplot2 that allowed us to plot various types of visualizations that pertained to several time-frames of the year. With this, we could conclude how time affected customer trips. Finally, we made a geom plot of New York that provided us with the details of how various users made trips from different bases.
