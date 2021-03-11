#Covid 19 effects on 4 major cities of India
install.packages("ggplot2")
install.packages("IRdisplay")
install.packages("leafpop")
install.packages("treemap")
install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)
library(RColorBrewer)
library(treemap)
library(tidyverse) # metapackage with lots of helpful functions
library(dplyr)
library(plotly)
library(ggplot2)
library(IRdisplay)
library(leaflet)
library(leafpop)
library(dplyr)
library(purrr)
MUMBAI<-read.csv("MUMBAI.csv")
View(MUMBAI)
DELHI<-read.csv("DELHI.csv")
View(DELHI)
CHENNAI<-read.csv("CHENNAI.csv")
View(CHENNAI)
KOLKATA<-read.csv("KOLKATA.csv")
View(KOLKATA)

###air quality box plot#####

boxplot(AQI~Month,
        data=MUMBAI,
        main="MUMBAI",
        xlab="Month",
        ylab="AQI",
        col="orange",
        border="brown"
        )

boxplot(AQI~Month,
        data=KOLKATA,
        main="KOLKATA",
        xlab="Month",
        ylab="AQI",
        col="orange",
        border="brown"
        )

boxplot(AQI~Month,
        data=DELHI,
        main="DELHI",
        xlab="Month",
        ylab="AQI",
        col="orange",
        border="brown"
        )

boxplot(AQI~Month,
        data=CHENNAI,
        main="CHENNAI",
        xlab="Month",
        ylab="AQI",
        col="orange",
        border="brown"
        )



