### mapssss

########################################################
##     First let's load the data we'll need     ######
########################################################


## Incidence

source("cleaning_script_Big5.R")


## shape files

#Here are the spatial libraries we'll need
library(sf)
library(rgeos)
library(GISTools)
library(sp)
library(tmap)
require(dplyr)


#Here's the shape file import:

load("C:/Users/rshea/Desktop/old computer/TB final/new.taiwan.district.rda")

#convert names b/c apparently different branches of 
# the government can't decide if it’s 台 or 臺

new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺東縣",]$COUNTYNAME <- "台東縣"
new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺中市",]$COUNTYNAME <- "台中市"
new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺北市",]$COUNTYNAME <- "台北市"
new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺南市",]$COUNTYNAME <- "台南市"


#this creates a combined city district column
new.taiwan.district$site_id <- paste(new.taiwan.district$COUNTYNAME,
                                     new.taiwan.district$TOWNNAME,
                                     sep = "")


#########################################################
############ make maps   ############################
###########################################################

# attach incidence to shape file
test1 <- new.taiwan.district %>%
  full_join(inc_by_D,
            by=c("site_id"="site_id"))%>%
  dplyr::filter(!is.na(assigned_onset_date))%>% 
  dplyr::filter(Cumul_cases_perCapita >0)%>%
  dplyr::relocate(site_id,.before = value)


# 1. GIF

#a. new cases per 100,000
# i.All of Taiwan
require(gifski)
library(lubridate)
quanguo_facet_inc <- tm_shape(new.taiwan.district)+
  tm_borders()+
  tm_shape(test1[test1$new_perCapita>0,]) +
  tm_polygons("new_perCapita",
              title="New cases per 10^6") +
  tm_layout(title = "Daily New cases per capita by District")+
  tm_facets(along ="assigned_onset_date", as.layers = TRUE)

tmap_animation(quanguo_facet_inc,filename = "quanguo_inc.gif",delay = 100)

#ii. Northern taiwan
test2 <- test1 %>%
  filter(COUNTYNAME %in% c("台北市", "新北市","桃園市","基隆市"))

beibu_facet_inc1 <-  tm_shape(new.taiwan.district%>%
                           filter(COUNTYNAME %in% c("台北市", "新北市","桃園市","基隆市")))+
  tm_borders()+
  # tm_text("TOWNNAME", size = 1)+
  tm_layout(title = "Daily new cases per capita by District")+
  tm_shape(test2[test2$new_perCapita>0,]) +
  tm_polygons("new_perCapita",
              title="New cases per 10^6") 

beibu_facet_inc <- beibu_facet_inc1+ 
  tm_facets(along ="assigned_onset_date", as.layers = TRUE)

tmap_animation(beibu_facet_inc,filename = "beibu_inc.gif",delay = 100)

#b. cumulative cases per 100,000
# i.All of Taiwan
require(gifski)
library(lubridate)
quanguo_facet_prev <- tm_shape(new.taiwan.district)+
  tm_borders()+
  tm_shape(test1) +
  tm_polygons("Cumul_cases_perCapita",
              title="Cumulative cases per 10^6",
              breaks= seq(0,max(test1$Cumul_cases_perCapita),50)) +
  tm_layout(title = "Daily cumulative cases per capita by District")+
  tm_facets(along ="assigned_onset_date", as.layers = TRUE)

tmap_animation(quanguo_facet_prev,filename = "quanguo_prev.gif",delay = 100)

#ii. Northern taiwan
test2 <- test1 %>%
  filter(COUNTYNAME %in% c("台北市", "新北市","桃園市","基隆市"))

beibu_facet_prev1 <-  tm_shape(new.taiwan.district%>%
                           filter(COUNTYNAME %in% c("台北市", "新北市","桃園市","基隆市")))+
  tm_borders()+
  # tm_text("TOWNNAME", size = 1)+
  tm_layout(title = "Daily cumulative cases per capita by District")+
  tm_shape(test2) +
  tm_polygons("Cumul_cases_perCapita",
              title="Cumulative cases per 10^6",
              breaks= seq(0,max(test2$Cumul_cases_perCapita),50)) 

beibu_facet_prev <- beibu_facet_prev1+ 
  tm_facets(along ="assigned_onset_date", as.layers = TRUE)

tmap_animation(beibu_facet_prev,filename = "beibu_prev.gif",delay = 100)


#I'm trying to get two maps side by side in the same
#GIF file, so far it's not working QQ

#beibu <- tmap_arrange(beibu_facet_inc,
 #                     beibu_facet_prev,
  #                    nrow = 2)


#tmap_animation(beibu,
 #              height = 400,
  #             width = 400,
   #            filename = "beibu.gif",
    #           delay = 100)


###################################################
#############                       #################
########            Shiny!!             ############
#############                       ###################
######################################################

############## Shiny 1 ############################

# shiny 
library(shiny)

ui <- fluidPage(
  fluidRow(
  dateInput("day","Choose a date", min = "2021-04-23",
            max=Sys.Date()-1, value = "2021-05-23"),
  selectInput("prevorinc", 
              "Please choose incidence or prevalence",
              choices = c(incidence="new_perCapita",
                          prevalence="Cumul_cases_perCapita"))
  ),
  leafletOutput("inc_map")
)

test3 <- test1 %>%
  filter(new_perCapita>0,
         Cumul_cases_perCapita>0)

server<- function(input, output, session){
  output$inc_map <- renderTmap({tm_shape(new.taiwan.district)+
      tm_borders()+
      tm_shape(test3[test3$assigned_onset_date== input$day,])+
      tm_polygons(input$prevorinc,
                  title="Cases per 10^6")})
}

shinyApp(ui,server)

################ Shiny 2 ##########################




# shiny 
library(shiny)

ui1 <- fluidPage(
    dateInput("day","Choose a date", min = "2021-04-23",
              max=Sys.Date()-1, value = "2021-05-23"),
  tmapOutput("inc_map"),
  tmapOutput("prev_map")
)

test3 <- test1 %>%
  filter(new_perCapita>0,
         Cumul_cases_perCapita>0)

tmap_mode("view")

server1<- function(input, output, session){
  output$inc_map <- renderTmap({tm_shape(new.taiwan.district)+
      tm_borders()+
      tm_shape(test3[test3$assigned_onset_date== input$day,])+
      tm_polygons("new_perCapita",
                  title="New cases per 10^6")})
  
  output$prev_map <- renderTmap({
    tm_shape(new.taiwan.district)+
      tm_borders()+
      tm_shape(test3[test3$assigned_onset_date== input$day,])+
      tm_polygons("Cumul_cases_perCapita",
                  title="Cumulative cases per 10^6")
  })
}

shinyApp(ui1,server1)


####################################################
##############
#######          Graphs                 ##########
#############                      #################


ggplot(test1[test1$TOWNNAME %in% c("萬華區","板橋區"),])+
  geom_col(aes(x=assigned_onset_date,
               y=Cumul_cases_perCapita,
               fill=TOWNNAME), position = "dodge")+
  geom_line(aes(x=assigned_onset_date,
                y=new_perCapita,
                color=TOWNNAME,
                ))+ 
  labs(x="date",
       y="Cases per 100,000",
       title = "Illustration of relationship between incidence and pervalence")



########### chart for ppt
library(ggplot2)
library(dplyr)
library(lubridate)

ggplot(covid_cases %>% 
         dplyr::filter(assigned_onset_date > "2021-04-30",
                       imported=="否")) +
  geom_col(aes(x=assigned_onset_date, y=case_count),fill="cadetblue3")+
  labs(x="Onset date",y= "Confirmed Cases",title = "Epidemic Curve of Covid-19 in Taiwan",subtitle = "2021-05-01 to 2021-06-30")+
  scale_y_continuous(breaks = seq(0, 600, by = 100))+
  scale_x_date(breaks = seq.Date(from = as.Date("2021-05-01"),to=as.Date("2021-06-30"), by = 4),date_labels="%m/%d")+
  theme_classic()
