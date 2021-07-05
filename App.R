# Load libraries ---- 
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(readxl)
library(rintrojs)
library(ggplot2)
library(ggridges)
library(reshape2)
library(ggthemes)
library(ggalluvial)
library(RColorBrewer)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(sf)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(highcharter)
library(fs)
library(magrittr)
library(urltools)
library(readr)
library(shinyWidgets)
library(rgeos)
library(GISTools)
library(sp)
library(lubridate)
library(data.table)
library(googlesheets4)
library(rjson)
library(jsonlite)
library(leaflet)
library(xts)
#library(tmap)
#require(gifski)

gs4_deauth()
# Load Data ----
## 更新日期
update_date <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Update_date")
## Case 原檔 ----
#cases <- fromJSON("https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.json", flatten=TRUE)
cases <- fromJSON("https://od.cdc.gov.tw/eic/Age_County_Gender_day_19Cov.json", flatten=TRUE)

colnames(cases) <- c("disease_type", "assigned_onset_date", "city", "district",
                     "sex", "imported",  "age_range", "case_count")
cases$sex <- ifelse(cases$sex == "F", "女性", "男性")

## 新確診數 ----
new_case <- subset(cases, imported == "否")
new_case <- new_case[c("assigned_onset_date", "city", "sex", "age_range", "case_count")]

new_case$assigned_onset_date <- as.Date(new_case$assigned_onset_date, format = "%Y/%m/%d")

Rt <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Onset_city")
## 每日資訊 ----
daily_info <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Daily_info")
daily_info <- daily_info[, c(1:23)]
colnames(daily_info) <- c("date",
                      "weekend",
                     "confirmed_case",
                     "corrected_case",
                     "death_case",
                     "positive_rate",
                     "pos/screen_vol",
                     "screen_vol",
                     "positive_case",
                     "total_case_0415",
                     "total_case_0420",
                     "severe_case",
                     "severe_rate",
                     "60up_total_case_0415",
                     "60up_total_case_0420",
                     "60up_severe_case",
                     "60up_severe_rate",
                     "icu",
                     "respiratory_failure",
                     "respirator",
                     "ecmo",
                     "cumu_death_case",
                     "death_case_by_date"
                     )
daily_info <- daily_info[,-2]
## 死亡個案 ----
death <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Death")
death <- death[, c(1:11)]
death$sex <- ifelse(death$sex == "female", "女性", "男性")

death$report_date <- as.Date(death$report_date)


# 疫苗 ----
vaccine <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Vaccine_City")
colnames(vaccine) <- c("date", "city", "pop", "vaccination_last_time",
                       "new_vaccination", "cum_vaccination",
                       "cum_vaccination_rate", "cum_delivery", "left_vol")


vaccine_type <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Vaccine_type")

## 發病日 ----
onset <- read_sheet("1mLKyntxDWUV1LDDfdNN3tnk2LA7UYnBmc8_t7WgVENQ",sheet="Onset")
onset <- onset[,c(1:10)]
colnames(onset) <- c("onset_date","onset_confirmed_case","local_case","death_case_deathdate","severe_case","bed","ICU_bed","Rt","Rt_l","Rt_u")
#onset$onset_date <- seq(as.Date("2021-04-01"),as.Date("2021-04-01")+(dim(onset)[1]-1),by="days")
#onset$onset_date <- onset_data[,1]
#onset$onset_date<- as.POSIXct.Date(as.numeric(onset$onset_date)/86400000, origin= "1900-01-01")

## 重症 ----
severe <- data.frame(date = daily_info$date, severe = daily_info$severe_case, day = as.Date(daily_info$date) - as.Date("2021/4/30"))
exponential.model <- lm(log(severe$severe)~ severe$day)
day_from_430 <- as.Date(Sys.time()) - as.Date("2021/4/30")
day <- c(1: day_from_430)
y_exp = exp(exponential.model$coef[1]+exponential.model$coef[2]*day)

y2 <- round(y_exp,digits = 0)
all <- data.frame(day,y2)
combine <- merge(severe,all, by="day", all=TRUE)

severe2 <- onset[ , 1:5]
severe2$cum_case <- cumsum(severe2$onset_confirmed_case)
severe2$cum_death <- cumsum(severe2$death_case_deathdate)
severe2$left_case <- severe2$cum_case - severe2$cum_death
severe2$new_case <- severe2$cum_case - severe2$cum_death - severe2$severe_case
severe2$new_case <- ifelse(is.na(severe2$severe_case)==T, severe2$cum_case - severe2$cum_death, severe2$cum_case - severe2$cum_death - severe2$severe_case)
na_rows <- which(is.na(severe2$severe_case)==T)
na_rows_date <- severe2$onset_date[na_rows]
na_rows_date
## CDC onset ----
CDC_oncet <- read_excel('./data/CDCfrom202141-.xlsx', sheet='CDCfrom202141-')
CDC_onset <- CDC_oncet[,c(1,26:47)]
CDC_onset$Oncetdate <- as.Date(CDC_onset$Oncetdate)

CDC_onset_long <- gather(CDC_onset,key=Confirmdate, value=count, C_20210522:C_20210611)
CDC_onset_long$Confirmdate <- substr(CDC_onset_long$Confirmdate,3,10)
CDC_onset_long$Confirmdate <- as.factor(CDC_onset_long$Confirmdate)

CDC_onset_long <- CDC_onset_long %>% mutate(Confirmgrouprev = fct_rev(as.factor(Confirmdate))) # Convert to factor and set "Missing" and "Other" as top levels to appear on epicurve top


# color
color_list = c("khaki1","lightgoldenrod1","gold1","gold2","goldenrod2","orange1","darkorange","darkorange2","tomato1","orangered1","red3","red4","sienna4")
color_list2 = c("violetred3","violetred1","red1","orangered1","tomato1","darkorange2","darkorange","orange1","goldenrod2","gold2","gold1","lightgoldenrod1","khaki1")
colourCount=length(unique(CDC_onset_long$Confirmgrouprev))


# 人流 ----
url_region <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
temp       <- tempfile()
download.file(url_region, temp)
data_2020  <- read.csv(unz (temp, "2020_TW_Region_Mobility_Report.csv"))
data_2021  <- read.csv(unz (temp, "2021_TW_Region_Mobility_Report.csv"))

# Contributors ----
contributors <- data.frame(Institute=c("Department of Public Health, NTU","Department of Public Health, NTU","Institute of Epidemiology and Prventive Medicine,NTU","Institute of Epidemiology and Prventive Medicine,NTU","Institute of Epidemiology and Prventive Medicine,NTU","Institute of Epidemiology and Prventive Medicine,NTU"),
                           Position= c("Student","Student","Research assistant", "Research associate","PhD candidate","Professor"),
                           Name=c("王敬中","夏琬慈","劉柏辰","伍倢瑩","吳昀麇","林先和"))
acknowledge <- data.frame(Institute=c("Global Health Program, NTU","London School of Hygiene & Tropical Medicine","Global Health Program, NTU"),
                           Position= c("student","Research fellow","Assistant Professor"),
                           Name=c("Russell Shean","Han Fu","Andrei Akhmetzhanov"))

# 地圖 ----
covid_cases <- cases
covid_cases$site_id <- paste(covid_cases$city, 
                             covid_cases$district, 
                             sep = "")
covid_cases$assigned_onset_date <- as.Date(covid_cases$assigned_onset_date, format = "%Y/%m/%d")
covid_cases$case_count <- as.numeric(covid_cases$case_count)

covid_cases[covid_cases$age_range=="0",]$age_range <- "pop_0" 
covid_cases[covid_cases$age_range=="1",]$age_range <- "pop_1" 
covid_cases[covid_cases$age_range=="2",]$age_range <- "pop_2" 
covid_cases[covid_cases$age_range=="3",]$age_range <- "pop_3" 
covid_cases[covid_cases$age_range=="4",]$age_range <- "pop_4" 
covid_cases[covid_cases$age_range=="5-9",]$age_range <- "pop_5to9" 
covid_cases[covid_cases$age_range=="10-14",]$age_range <- "pop_10to14" 
covid_cases[covid_cases$age_range=="15-19",]$age_range <- "pop_15to19" 
covid_cases[covid_cases$age_range=="20-24",]$age_range <- "pop_20to24" 
covid_cases[covid_cases$age_range=="25-29",]$age_range <- "pop_25to29" 
covid_cases[covid_cases$age_range=="30-34",]$age_range <- "pop_30to34" 
covid_cases[covid_cases$age_range=="35-39",]$age_range <- "pop_35to39" 
covid_cases[covid_cases$age_range=="40-44",]$age_range <- "pop_40to44" 
covid_cases[covid_cases$age_range=="45-49",]$age_range <- "pop_45to49" 
covid_cases[covid_cases$age_range=="50-54",]$age_range <- "pop_50to54" 
covid_cases[covid_cases$age_range=="55-59",]$age_range <- "pop_55to59" 
covid_cases[covid_cases$age_range=="60-64",]$age_range <- "pop_60to64" 
covid_cases[covid_cases$age_range=="65-69",]$age_range <- "pop_65to69" 
covid_cases[covid_cases$age_range=="70+",]$age_range <- "pop_over70" 

require(readxl)
pop_by_sex2 <- read_xlsx("./data/pop_by_sex2.xlsx", sheet = 1)
pop_by_sex2 <- as.data.frame(pop_by_sex2)

new.names <-  c("sex","area_code","area_name","total_pop",
                paste("pop_at_age_",
                      seq(0,100,1),
                      sep = ""))
colnames(pop_by_sex2)<- new.names

pop_by_sex2 <- pop_by_sex2 %>%
  slice(5:n())

pop_by_sex2$area_code <- as.numeric(pop_by_sex2$area_code)

pop_by_sex2$city_code <- round(pop_by_sex2$area_code/1000)

Names14 <- data.frame(city = c("新北市", "台北市", "桃園市", "台中市",
                               "台南市", "高雄市", "宜蘭縣", "新竹縣",
                               "苗栗縣", "彰化縣", "南投縣", "雲林縣",
                               "嘉義縣", "屏東縣", "台東縣", "花蓮縣",
                               "澎湖縣", "基隆市", "新竹市", "嘉義市",
                               "金門縣", "連江縣"),
                      area_code = c(65000000,	63000000,68000000,
                                    66000000,67000000,64000000,	
                                    10002000,	10004000,10005000,10007000,10008000,
                                    10009000, 10010000,	10013000,		10014000,10015000,
                                    10016000,10017000,10018000,10020000,9020000,9007000))

Names14$city_code <- round(Names14$area_code/1000)
pop_by_sex2 <- pop_by_sex2 %>% 
  left_join(Names14, by=c("city_code"="city_code")) 

pop_by_sex2$site_id <- paste(pop_by_sex2$city,
                             pop_by_sex2$area_name, 
                             sep = "")

'%notin%' <- function(x,y)!('%in%'(x,y))

pop_by_sex2$pop_at_age_100 <- as.numeric(pop_by_sex2$pop_at_age_100)

pop_by_sex3 <- pop_by_sex2 %>%
  mutate(pop_0 =pop_at_age_0,
         pop_1 =pop_at_age_1,
         pop_2 =pop_at_age_2,
         pop_3 = pop_at_age_3,
         pop_4 = pop_at_age_4,
         pop_5to9 =rowSums(across(pop_at_age_5:pop_at_age_9)),
         pop_10to14=rowSums(across(pop_at_age_10:pop_at_age_14)),
         pop_15to19=rowSums(across(pop_at_age_15:pop_at_age_19)),
         pop_20to24 =rowSums(across(pop_at_age_20:pop_at_age_24)),
         pop_25to29 =rowSums(across(pop_at_age_25:pop_at_age_29)),
         pop_30to34 =rowSums(across(pop_at_age_30:pop_at_age_34)),
         pop_35to39 =rowSums(across(pop_at_age_35:pop_at_age_39)),
         pop_40to44 =rowSums(across(pop_at_age_40:pop_at_age_44)),
         pop_45to49 =rowSums(across(pop_at_age_45:pop_at_age_49)),
         pop_50to54 =rowSums(across(pop_at_age_50:pop_at_age_54)),
         pop_55to59 =rowSums(across(pop_at_age_55:pop_at_age_59)),
         pop_60to64 =rowSums(across(pop_at_age_60:pop_at_age_64)),
         pop_65to69 =rowSums(across(pop_at_age_65:pop_at_age_69)),
         pop_over70 =rowSums(across(pop_at_age_70:pop_at_age_100)))%>%
  dplyr::select(!(pop_at_age_0:pop_at_age_100))%>%
  dplyr::select(!area_code.x)%>%
  dplyr::select(!area_code.y)%>%
  dplyr::select(!city_code)%>%
  dplyr::select(!(area_name:city))%>%
  filter(site_id %notin% c("NA總計", "NA福建省",
                           "新北市新北市", "台北市台北市",
                           "桃園市桃園市", "台中市台中市",
                           "台南市台南市", "高雄市高雄市",
                           "NA臺灣省",
                           "宜蘭縣宜蘭縣", "新竹縣新竹縣", "苗栗縣苗栗縣", "彰化縣彰化縣",
                           "南投縣南投縣", "雲林縣雲林縣", "嘉義縣嘉義縣", "屏東縣屏東縣",
                           "台東縣台東縣", "花蓮縣花蓮縣", "澎湖縣澎湖縣", "基隆市基隆市",
                           "新竹市新竹市", "金門縣金門縣", "連江縣連江縣", "嘉義市嘉義市"))

require(reshape2)            
pop_by_SAD <- reshape2::melt(pop_by_sex3, id.vars = c("sex","site_id"))%>%
  rename(age_range = "variable",
         population = "value")

pop_by_SAD$population <- as.numeric(pop_by_SAD$population)

inc.maker <- function(pop){
  inc <- pop %>%
    full_join(covid_cases,
              by=c("site_id"="site_id",
                   "age_range"="age_range",
                   "sex"="sex"))%>%
    mutate(new_perCapita = case_count/population*100000)
  inc
}

inc_by_SAD <- inc.maker(pop_by_SAD)

pop_by_SA <- aggregate(population~sex+age_range, data= pop_by_SAD, FUN = sum)
covid_cases2 <- covid_cases %>%
  filter(imported=="否",
         assigned_onset_date > "2021-04-01")%>%
  dplyr::select(assigned_onset_date,
                case_count,
                site_id,
                age_range,
                sex)
cases_by_SA <- aggregate(case_count~assigned_onset_date+age_range+sex, data=covid_cases2, FUN = sum)
inc_by_SA <- pop_by_SA %>%
  full_join(cases_by_SA,
            by=c("age_range"="age_range",
                 "sex"="sex"))%>%
  mutate(new_perCapita = case_count/population*100000)

pop_by_SD <- aggregate(population~sex+site_id, data= pop_by_SAD, FUN = sum)
cases_by_SD <- aggregate(case_count~assigned_onset_date+site_id+sex, data = covid_cases2, FUN = sum)
inc_by_SD <- pop_by_SD %>%
  full_join(cases_by_SD,
            by=c("site_id"="site_id",
                 "sex"="sex"))%>%
  mutate(new_perCapita = case_count/population*100000)

pop_by_S <- aggregate(population~sex, data= pop_by_SAD, FUN = sum)
cases_by_S <- aggregate(case_count~assigned_onset_date+sex, data=covid_cases2, FUN = sum)
inc_by_S <- pop_by_S %>%
  full_join(cases_by_S,
            by=c("sex"="sex"))%>%
  mutate(new_perCapita = case_count/population*100000)

pop_by_AD <-  aggregate(population~age_range+site_id, data= pop_by_SAD, FUN = sum)
cases_by_AD <- aggregate(case_count~assigned_onset_date+age_range+site_id, data = covid_cases2, FUN = sum)
inc_by_AD <- pop_by_AD %>%
  full_join(cases_by_AD,
            by=c("age_range"="age_range",
                 "site_id"="site_id"))%>%
  mutate(new_perCapita = case_count/population*100000)

pop_by_A <- aggregate(population~age_range, data= pop_by_SAD, FUN = sum)
cases_by_A <- aggregate(case_count~assigned_onset_date+age_range, data = covid_cases2, FUN=sum)
inc_by_A <- pop_by_A %>%
  full_join(cases_by_A,
            by=c("age_range"="age_range"))%>%
  mutate(new_perCapita = case_count/population*100000)

pop_by_D <-  aggregate(population~site_id, data= pop_by_SAD, FUN = sum)
cases_by_D <- aggregate(case_count~site_id+assigned_onset_date, data = covid_cases2,FUN = sum)
inc_by_D <- pop_by_D %>%
  full_join(cases_by_D,
            by=c("site_id"="site_id"))%>%
  mutate(new_perCapita = case_count/population*100000)

# get epi data
TWshp_sf<- readRDS("./data/TWshp_town.RData")
TWshp_sf%<>%mutate(site_id= paste0(COUNTYNAME, TOWNNAME))

covide_rt_epi<- inc_by_D%>%left_join(TWshp_sf%>%as.data.frame()%>%dplyr::select(TOWNCODE, site_id), by= "site_id")

TWshp_sf%<>%st_transform(4326)

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))

  options <- evalFormula(options, data = data)

  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

setShapeLabel <- function( map, data = getMapData(map), layerId,
                           label = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}

#JS methods
leafletjs <-  tags$head(
  # add in methods from https://github.com/rstudio/leaflet/pull/598
  tags$script(HTML(
    '
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setRadius = function(layerId, radius){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
    radius = [radius];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer("marker", d);
    if (layer){ // or should this raise an error?
      layer.setRadius(radius[i]);
    }
  });
};

window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  //label = HTMLWidgets.dataframeToD3(label);
  //console.log(label);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      // layer.setStyle(style[i]);
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)

TWshp_sf$new_perCapita <- covide_rt_epi[covide_rt_epi$assigned_onset_date=="2021-05-27",]$new_perCapita[match(TWshp_sf$TOWNCODE, covide_rt_epi[covide_rt_epi$assigned_onset_date=="2021-05-27",]$TOWNCODE)]
TWshp_sf$Label_text<- paste0(
  "Town: ", TWshp_sf$COUNTYNAME, TWshp_sf$TOWNNAME, "<br/>",
  "New cases per capita: ", TWshp_sf$new_perCapita%>%round(digits= 3))


# Descriptions ----

Notes_Rt <- "通常我們使用基礎再生數(R0)及有效再生數(Rt)來代表疾病在人群中的傳染能力。\n
在流行剛開始，人群皆沒有免疫力時會使用R0。\n
而當族群開始有免疫力及有防治措施下，再生數會隨時間改變，即使用Rt來代表一名感染者平均能傳染給多少人。若數值小於1，疫情可能可以得到控制。\n
且觀察整體疫情需搭配個案數之流行曲線一起判斷。"

Notes_Mb <- "「Google的社區人流報告呈現了不同場域的移動情形，若與中央疫情指揮中心發布疫情警戒的時間點比較，這些移動趨勢的時序變化能夠在一定程度上反映大眾遵循非藥物介入措施的情形，提供防疫策略調整的方向。」\n
引用自:科學月刊, 2021-06-15, 傅涵。請見原文更詳細資訊: https://reurl.cc/O04vy9。"

#https://www.twreporter.org/a/covid-19-rt

Notes_Newcase <- "從年齡分布可以看到，五月中到六月中，一開始的每日新發生個案中，年齡較大的所佔比例較高，到六月的時候，則是年紀較輕的為大宗。
男性當中，一開始新發生個案60歲以上的占比最多(將近一半)，到六月中則是20-39歲占比最多。女性當中，一開始是40-59歲的個案為最多(將近一半)，到六月中時20-39歲、40-59歲、60歲以上的比例則相當(各約三分之一)。
從美國疾管署的資料可以看到，年紀越高發生重症或死亡的機率越高，因此我們可以推測，在萬華社區傳播的疫情，所造成的重症或死亡比例會比在科技園區所爆發的來得高。"

Notes_Severe <- "造成臺灣此波本土疫情的是B.1.1.7英國變種病毒。從英國和丹麥的研究來看，此變種病毒的重症率或致死率都比先前的病毒株高。不只如此，當病人數突增，超過醫療量能的時候，整體死亡率也會因此提高。因此，我們欲透過重症或死亡比例，了解此病毒株在臺灣所造成的重症或死亡比例，也觀察在疫情高峰時的醫療量能需求。"

Notes_Progression <- "從症狀開始的那一天我們定義為發病日，從發病日到死亡日我們定義為疾病進程。以目前死亡的個案來看，除了少數無症狀感染者以及發病日未知的個案，每個年齡層的疾病進程之中位數介於7天到11天。"

Notes_Choropleth<- "藉由面量圖來呈現每日臺灣各縣市區的每十萬人發生率（例如， 2021年4月23日 桃園市蘆竹區 每十萬人有 0.599 名新確診個案 ; 桃園市蘆竹區人口約有167060人，4月23日該區新增1名確診個案，因此0.599為新增確診數除上該區人口數再乘以十萬而得）。圖例以六種由淺入深的紅色表示發生率由低至高，若該區當日無新確診個案則以透明作區分。利用縣市內各區的行政邊界當作空間上的單位，除了可以令讀者即時掌握新確診個案於臺灣空間上的變化也能使民眾以熟悉的空間認知來辨認位置。相較於僅以新確診人數呈現在地圖上，各區每十萬人發生率更能夠使不同人口數的區站在同一基準線上彼此比較，更有助於讀者在判讀各區單日發生率上的高低。此面量圖所使用的資料皆取自疾管局每日公布之新增確診病例。"
###-------------
# UI forming ----
ui <- navbarPage(title = "CoViz 台灣 COVID-19視覺化", id = "navBar",
                 position = "static-top",
                 theme = "black",
                 inverse = T,
                 collapsible = T,
                 fluid = T, 
                 windowTitle = "COVID-19 Tool",
                 tabPanel("首頁", value="home",
                          fluidRow(
                            
                            column(width = 12, 
                                   shiny::HTML("<br><br><center><h1>CoViz 台灣 COVID-19視覺化</h1> </center>"),
				   shiny::HTML("<center><h2>  —和公衛「疫」起走過</h2> </center><br>"),
                                   shiny::HTML("<center><h5>2021年四月底以來，臺灣爆發COVID-19本土疫情，在一個月內有超過一萬個新確診個案，以及數百人因為COVID-19死亡。</h5></center>"),
                                   shiny::HTML("<center><h5>為了降低疫情傳播，五月中全國進入三級緊戒，很多行業因此停擺，受到影響；醫院前線的量能也非常緊繃，醫護人員都承擔很大的壓力。</h5></center>"),
                                   shiny::HTML("<center><h5>在臺灣島上的每一個人，都希望疫情能夠早日控制，恢復往日正常的生活。</h5></center>"),
                                   shiny::HTML("<center><h5>要能夠將疫情控制，即時的疫情資訊是迫切需要的。</h5></center>"),
                                   shiny::HTML("<center><h5>不但可以幫助判斷未來疫情變化的可能趨勢，也可以看到目前政策的短期效果。</h5></center>"),
                                   shiny::HTML("<center><h5>關於疫情的資訊很多，本網站欲透過視覺化的圖表，淺白的文字說明，來持續更新並呈現即時的疫情狀況，冀能在疫情控制上一起出一份力。</h5></center>")
                                   )
                          ),
                          
                          
                          tags$hr(),
                          
                          fluidRow(
                            
                            column(width = 12, 
                                   shiny::HTML("<br><center><h3>內容說明</h3> </center>"),
                                   shiny::HTML("<br><center><h4>流行病學曲線</h4></center>"),
                                   shiny::HTML("<center><h5>透過每日新發生個案數、有效病例再生數 (Rt)，以及死亡數的變化，看疫情趨勢</h5></center>"),
                                   shiny::HTML("<br><center><h4>新確診個案的年齡分布</h4></center>"),
                                   shiny::HTML("<center><h5>呈現隨時間推移之新確診個案的年齡分布</h5></center>"),
                                   shiny::HTML("<br><center><h4>重症或死亡的比例</h4></center>"),
                                   shiny::HTML("<center><h5>呈現重症或死亡佔所有確診個案的比例，並透過ICU變化量看疫情高峰時的醫療需求</h5></center>"),
                                   shiny::HTML("<br><center><h4>疾病進程</h4></center>"),
                                   shiny::HTML("<center><h5>呈現整體以及各年齡層從發病日到死亡日的時間長度分布</h5></center>"),
                                   shiny::HTML("<br><center><h4>發生率地圖</h4></center>"),
                                   shiny::HTML("<center><h5>透過每日各鄉鎮的發生率，看疫情擴散現象</h5></center>"),
                                   shiny::HTML("<br><center><h4>社區人流趨勢圖</h4></center>"),
                                   shiny::HTML("<center><h5>透過Google的每日人流量監測，看民眾對於三級警戒的政策遵囑性</h5></center>"),
                
                            )
                          ),
                          tags$hr(),
                          ),
                 tabPanel("流行曲線", value="epicurve", 
                         
                          fluidRow(
                               column(width = 12,  h4("每日新發生數 與 有效再生數(Rt)", align = "center")),
                               
                               column(1, actionButton("show_Rt", "如何解讀")),
                               column(1, downloadButton("downloadData_rt", "下載資料"), offset = 10),
                               
                               column(width = 12, highchartOutput("epicurve_case"))
			  ),
                          fluidRow(
                            
                              column(12, downloadButton("downloadData_epicurve_death", "下載資料"), offset = 12),
                              column(width = 12,  h4("每日新死亡數", align = "center"), highchartOutput("epicurve_death"))
			                
			  ),
                          fluidRow(
                              column(width = 12, h6("資料來源: 台灣疾病管制署; 指揮中心記者會", align="center")),
                              column(width = 12, h6("", align="center"))
                          )
                 ),
                 
                 tabPanel("縣市年齡分層", value = "new_case", 
                          
                          fluidRow(
		              column(width = 12, h4("本土個案的年齡分布",align="center")),
                              column(width = 3),
                              column(width = 3,selectInput("epicurve_sex", label = h6("性別"),
                                                           choices = c("所有性別", "女性", "男性"),
                                                           multiple = F), align="center"),
		                          column(width = 3, selectInput("age_city", label = h6("縣市別"),
		                                                        choices = c("所有縣市", levels(factor(new_case$city))),
		                                                        multiple = F), align="center"),
		                          column(width = 3),
		              
		              column(12),
		              column(1,  actionButton("show_Newcase", "如何解讀")),
		              column(1, downloadButton("downloadData_newcase", "下載資料"), offset = 10)
		              
                              
                          ),
                          fluidRow(
                              column(width = 6, h5("每日本土個案數", align = "center"), highchartOutput("new_case_all")),
                              column(width = 6, h5("每日本土個案數之年齡分布", align = "center"), highchartOutput("new_case_age")),
                              
			      column(width = 12)
                              
                          ),
                          fluidRow(
                              column(width = 12, h6("*註記：另團隊有估計北部幾個個案數較多之縣市Rt值(台北市、新北市、桃園市、基隆市)")),
                              column(width = 12, h6("資料來源: 台灣疾病管制署", align="center"))
                          )
                          
                 ),
                 tabPanel("重症個案", value="severe_cases",
                          fluidRow(
                
                              column(width = 12, h4("重症與死亡以外之個數", align = "center")),
                              
                              column(1,  actionButton("show_Severe", "如何解讀")),
                              column(1, downloadButton("downloadData_severe", "下載資料"), offset = 10),
			      
                              column(width = 12, highchartOutput("severe_new")),
                              column(width = 12, h6("*註記：重症個案數自6/24起缺失。",align="center")),
			      column(width = 12, h6("*註記：重症個案的資訊來源是從疫情中心的每日記者會當中取得，若有記者詢問，則可以擷取此資訊，但若沒有記者詢問，則無從得知。",align="center")),
			      column(width = 12, h6("因此，我們利用有資料的時間點，分別使用指數和線性方式來進行補值並呈現，與實際上的數值可能有所出入，但應相差不遠。", align="center")),
			      
			      column(12, downloadButton("downloadData_severe_bed", "下載資料"), offset = 12),
			      column(width = 12, h4("專責病房收治人數", align = "center"), highchartOutput("severe_bed")),
			      
			      column(width = 12)
                          	
                          ),
                          fluidRow(
                
                            column(12, downloadButton("downloadData_severe_res_f_icd_ecmo", "下載資料"), offset = 12),
                            
                              column(width = 6, h6("重症個案=呼吸器數目 ", align = "center"), highchartOutput("severe_res")),
                              column(width = 6, h6("重症個案=呼吸衰竭個案數 ", align = "center"), highchartOutput("severe_res_f")),
                              column(width = 6, h6("重症個案-ICU個案數 ", align = "center"), highchartOutput("severe_icu")),
                              column(width = 6, h6("重症個案-葉克膜數目 ", align = "center"), highchartOutput("severe_ecmo")),
                              
                              
                          ),
                          fluidRow(
                              column(width = 12, h6("灰底資料使用R package 'imputeTS'補值", align = "center")),
                              column(width = 12, h6("資料來源: 台灣疾病管制署; 指揮中心記者會", align="center"))
                          )
                 ),
                 tabPanel("死亡個案", value="death_cases",
                          
                          fluidRow(
                              column(width = 5),
                              column(width = 2,selectInput("death_sex", label = h6("性別"),
                                                           choices = c("所有性別", "女性", "男性"),
                                                           multiple = F), align="center"),
                              column(width = 5),
                              
                              column(12),
                              column(1,  actionButton("show_Progression", "如何解讀")),
                              column(1, downloadButton("downloadData_death", "下載資料"), offset = 10), 
			      
                          ),
                          fluidRow(
                              column(width = 6, h5("疾病進程(發病日至死亡日天數)", align = "center") ,highchartOutput("death_duration")),
                              column(width = 6, h5("死亡數之年齡分布", align = "center"), highchartOutput("death_piechart"))
                          ),
                          fluidRow(
                              column(width = 12, h6("資料來源: 台灣疾病管制署; 指揮中心記者會", align="center"))
                          )
                          ),
                 tabPanel("發生率地圖",
	                 fluidRow(leafletjs,
	                          
	                          column(1,  actionButton("show_choropleth", "如何解讀")),
	                          column(1, downloadButton("downloadData_choropleth", "下載資料"), offset = 10),
	                          
	                              column(3, tags$h6(""),
                   			radioButtons(inputId = "frequency",
                      			label = "計算頻率",
                     			 choices = c("days"),
                     			 selected = "days",
                      				inline = TRUE),
         		# output for timeline control bar
         				uiOutput("dateUI")
  				),
  
  				column(9,
         				tags$h4("每日發生率(每十萬人)"),
        				 leafletOutput("map", width = "100%", height = "540px")
  					),
				column(9,
         				tags$h4("累積發生率(每十萬人)-待更新")
  					)
				)
		
			),
                 tabPanel("人流趨勢", value="mobility",
                          fluidRow(
                              column(width = 12, h4("相較於基準值*人流趨勢的改變比例 (%) - 台灣", align = "center")),
			      column(width = 12, h5("(1):2021/5/11全國二級警戒 ; (2):2021/5/15雙北市三級警戒; (3):2021/5/19全國三級警戒", align = "center"),actionButton("show_Mb", "如何解讀")),
			      column(width = 12, h6("*採用2020年1月3日到2月6日這5週間的資料，算出一週內每天的中位數，以此當作基準值")),	
                              column(width = 6, h6("雜貨店和藥局", align = "center"), highchartOutput("mob_g")),
                              column(width = 6, h6("公園", align = "center"), highchartOutput("mob_p")),
                              column(width = 6, h6("住宅區", align = "center"), highchartOutput("mob_res")),
                              column(width = 6, h6("零售店和休閒設施", align = "center"), highchartOutput("mob_rr")),
                              column(width = 6, h6("大眾運輸站", align = "center"), highchartOutput("mob_tran")),
                              column(width = 6, h6("工作場所", align = "center"), highchartOutput("mob_w")),
                          ),
                          fluidRow(
                              column(width = 12, h6("Google LLC 'Google COVID-19 社區人流趨勢報告'.
					https://www.google.com/covid19/mobility/"))
                          )
                          ),
		tabPanel("疫苗接種", value="vaccine",
		         fluidRow(
		           column(width = 12, h4("累積接種人次與人口覆蓋率", align="center"), highchartOutput("vaccine_cum")),
		           column(width = 12, h4("縣市接種率(統計至6/9)", align = "center"), highchartOutput("vaccine_city"))
		         )
		         ),
 		tabPanel("Q&A小檔案", value="QA",
			fluidRow(
			     column(width = 12, h4("1.為什麼流行曲線要看疾病發生日?", align="center")),
                  column(width = 12,  h5("參考圖:個案數隨時間變化", align = "left"), plotOutput("epicurve_time")),
			     column(width = 12,
			            shiny::HTML("<center><h5>若流行曲線是依研判日或確診日來看，可能會受延遲就醫、檢驗所需時間甚至是行政流程延遲的效果影響。若要判斷疫情趨勢，比較好的方式是依症狀發生日。</h5><center>"),
			            shiny::HTML("<center><h5>從上方參考圖可看出若從5/22~5/27公佈的資訊(黃色)來看，可能會認為疫情高峰已過，疫情有趨緩。</h5><center>"),
			            shiny::HTML("<center><h5>然至5/31左右(淺橘色)，會發現疫情其實尚在發展中。從中也提醒我們近七天的個案通常會被低估，因此觀察流行病學曲線時針對最後幾天的解讀要特別小心。</h5></center>"),
			            shiny::HTML("<center><h5>更詳細資訊歡迎參考台灣大學公共衛生學院的聲明：</h5></center>"),
			            shiny::HTML('<a href="http://coph.ntu.edu.tw/web/message/message.jsp?cp_no=CP1621793950436&lang=tw">台灣大學公共衛生學院的聲明</a>')
			  )
        ),
			tags$hr()
			),
			
      tabPanel("團隊成員", value="contributors",
			  fluidRow(
			     column(width = 12, h4("團隊成員"),tableOutput("contributors"), align="center"),
			     column(width = 12, h4("特別感謝提供建議與原始程式碼"),tableOutput("acknowledgement"), align="center"),
			     column(width = 12, h5(paste("上次更新： 數據:", update_date$Date[1], "APP: ", update_date$Date[2]), align = "center")),
			     column(width = 12, h5("有任何指正請與我們聯繫: 伍小姐 <jenny1004wu@ntu.edu.tw>"),align="center") 	

			     )
                 	  )
                 
)
new_case 

# Server ----
server <- function(input, output, session){
    
  
  # vaccine_city ----
  output$vaccine_city <- renderHighchart({
    tmp <- vaccine[1:22, ]
    
    highchart(type = "chart") %>% 
      hc_yAxis_multiples(
        list(title = list(text = "人數"), type = "linear"),
        list(title = list(text = "比率(%)"), type = "linear",  opposite = TRUE)
      ) %>%
      hc_add_series(name = "累積接種人數", data = tmp, hcaes(x = city, y = cum_vaccination),
                    type = "column", marker = list(symbol = "diamond"), color = "#9CDE9F", yAxis = 0) %>%
     
      hc_add_series(name = "接種比率", data = tmp, hcaes(x = city, y = (cum_vaccination_rate * 100)),
                    type = "line", marker = list(symbol = "diamond"), color = "#495D63", yAxis = 1) %>%
      
      hc_xAxis(categories = as.factor(tmp$city), title = list(text = "縣市"))
  
  })
  # vaccine_cum ----
  output$vaccine_cum <- renderHighchart({
    tmp <- vaccine[, -3:-5]
    tmp <- tmp[, -4:-6]
    
    tmp <- tmp %>%
    group_by(date) %>%
     summarise(Total = sum(cum_vaccination))
    
    
    tmp_pop <- vaccine[, 1:3]
    tmp_pop <- tmp_pop %>%
      group_by(date) %>%
      summarise(Pop_Total = sum(pop))
    tmp_pop$rate <- tmp$Total * 100 / tmp_pop$Pop_Total
    
    
    tmp$partially_vaccinated_total <- tmp$Total
    tmp$partially_pop_coverage <- round(tmp_pop$rate, 3)
    tmp$fully_vaccinated_total <- NA
    tmp$fully_pop_coverage <- NA
    tmp <- tmp[,-2]
    
    vac_cut <- vaccine_type[, -2: -9]
    vac_cut$date <- vac_cut$...1
    vac_cut <- vac_cut[,-1]
    
    tmp <- rbind(tmp, vac_cut)
    
   
    
    highchart(type = "chart") %>% 
      hc_yAxis_multiples(
        list(title = list(text = "人數"), type = "linear"),
        list(title = list(text = "比率(%)"), type = "linear",  opposite = TRUE)
      ) %>%
      hc_add_series(name = "累積接種人數", data = tmp, hcaes(x = as.Date(date), y = partially_vaccinated_total),
                    type = "column", marker = list(symbol = "diamond"), color = "#9CDE9F", yAxis = 0) %>%
      hc_add_series(name = "累積兩劑接種完成人數", data = tmp, hcaes(x = as.Date(date), y = fully_vaccinated_total),
                    type = "column", marker = list(symbol = "diamond"), color = "#80A1D4", yAxis = 0) %>%
      hc_add_series(name = "接種比率", data = tmp, hcaes(x = as.Date(date), y = partially_pop_coverage),
                    type = "line", marker = list(symbol = "diamond"), color = "#495D63", yAxis = 1) %>%
      hc_add_series(name = "兩劑接種完成比率", data = tmp, hcaes(x = as.Date(date), y = fully_pop_coverage),
                    type = "line", marker = list(symbol = "diamond"), color = "#1F5673", yAxis = 1) %>%
      hc_xAxis(type = "datetime", title = list(text = "日期"))
    
     
  })
  # new_case_all ----
    output$new_case_all <- renderHighchart({
      if(input$age_city == "所有縣市"){
        tmp <- new_case
      }
      else{
        tmp <- subset(new_case, new_case$city == input$age_city)
      }  
      
      
      if(input$epicurve_sex == "所有性別"){
            tmp$case_count <- as.numeric(tmp$case_count)
            tmp <- tmp %>%
                group_by(assigned_onset_date) %>%
                summarise(Total = sum(case_count))
            color_newcase <- "#FFCC54"
            color_rt <- "#ff7654"
            color_rt_ci <- "#ffded5"
        }
        else{
            tmp <- subset(tmp, tmp$sex == input$epicurve_sex)
            tmp$case_count <- as.numeric(tmp$case_count)
            tmp <- tmp %>%
              group_by(assigned_onset_date) %>%
              summarise(Total = sum(case_count))
            color_newcase <- ifelse(input$epicurve_sex == "女性", "#fe8081", "#6ca6d6")
            color_rt <- ifelse(input$epicurve_sex == "女性", "#FFC100", "#F26CA7")
            color_rt_ci <- ifelse(input$epicurve_sex == "女性", "#FFEBAD", "#FAC7DD")
        }
        
      city_rt <- c("台北市", "基隆市", "桃園市", "新北市")
        
      tmp_by_date <- onset[-1:-30, 1]
      colnames(tmp_by_date) <- "assigned_onset_date"
      tmp_by_date$assigned_onset_date <- as.Date(tmp_by_date$assigned_onset_date)
      tmp_by_date <- left_join(tmp_by_date, tmp, by="assigned_onset_date")
      na_rows <- is.na(tmp_by_date$Total)
      tmp_by_date[na_rows, 2] <- 0
      
      
      
      if(input$age_city %in% city_rt){
        
        Rt_date <- Rt[, 1]
        if(input$age_city == "台北市"){Rt_tmp <- Rt[, 2:4]}
        else if(input$age_city == "基隆市"){Rt_tmp <- Rt[, 5:7]}
        else if(input$age_city == "桃園市"){Rt_tmp <- Rt[, 8:10]}
        else if(input$age_city == "新北市"){Rt_tmp <- Rt[, 11:13]}
        
        
        Rt_tmp <- cbind(Rt_date, Rt_tmp)
        colnames(Rt_tmp) <- c("onset_date", "Rt", "Rt_upper", "Rt_lower")
        highchart(type = "chart") %>% 
          hc_yAxis_multiples(
            list(title = list(text = "個案數"), type = "linear"),
            list(title = list(text = "Rt"), type = "linear",  opposite = TRUE, plotLines = list(list(value = 1, color = "black", width = 1)))
          ) %>%
          hc_add_series(name = paste("新發生個案:", input$age_city, input$epicurve_sex), data = tmp_by_date, hcaes(x = assigned_onset_date, y = Total),
                        type = "column", marker = list(symbol = "diamond"), color = color_newcase, yAxis = 0) %>%
          hc_add_series(name = paste("Rt值"), data = Rt_tmp, hcaes(x = as.Date(onset_date), y = Rt),
                        type = "line", marker = list(symbol = "diamond"), color = color_rt, yAxis = 1) %>%
          hc_add_series(name = paste("Rt值 95% CI (Upper)"), data = Rt_tmp, hcaes(x = as.Date(onset_date), y = Rt_upper),
                        type = "line", marker = list(symbol = "diamond"), color = color_rt_ci, yAxis = 1) %>%
          hc_add_series(name = paste("Rt值 95% CI (Lower)"), data = Rt_tmp, hcaes(x = as.Date(onset_date), y = Rt_lower),
                        type = "line", marker = list(symbol = "diamond"), color = color_rt_ci, yAxis = 1) %>%
          hc_xAxis(type="datetime", title = list(text = "發生日"))
      }
      
      else{
        
        highchart(type = "chart") %>% 
          hc_add_series(name = paste("新發生個案:", input$age_city, input$epicurve_sex), data = tmp_by_date, hcaes(x = assigned_onset_date, y = Total),
                        type = "column", marker = list(symbol = "diamond"), color = color_newcase) %>%
          hc_xAxis(type = "datetime", title = list(text = "發生日"))%>%
          hc_yAxis(type = "linear", title = list(text = "個案數"))
        }
        
        
    })
    
    # new_case_age ----
    output$new_case_age <- renderHighchart({
      if(input$age_city == "所有縣市"){
        tmp <- new_case
      }
      else{
        tmp <- subset(new_case, new_case$city == input$age_city)
        
      } 
      
      if(input$epicurve_sex == "所有性別"){
        tmp <- tmp
      }
      else{
        tmp <- subset(tmp, tmp$sex == input$epicurve_sex)
        
      }
      tmp$assigned_onset_date <- as.Date(tmp$assigned_onset_date)
      tmp$case_count <- as.numeric(tmp$case_count)
      unknown <- c("0", "1", "2", "3", "4", "5-9")
      tmp$age_range <- ifelse(tmp$age_range %in% unknown, "資料缺失", tmp$age_range)
      gp_10 <- c("10-14", "15-19")
      gp_20 <- c("20-24", "25-29")
      gp_30 <- c("30-34", "35-39")
      gp_40 <- c("40-44", "45-49")
      gp_50 <- c("50-54", "55-59")
      gp_60 <- c("60-64", "65-69")
      gp_70 <- "70+"
      tmp$age_range <- ifelse(tmp$age_range %in% gp_10, "10-19",
                              ifelse(tmp$age_range %in% gp_20, "20-29", 
                                     ifelse(tmp$age_range %in% gp_30, "30-39",
                                            ifelse(tmp$age_range %in% gp_40, "40-49",
                                                   ifelse(tmp$age_range %in% gp_50, "50-59",
                                                          ifelse(tmp$age_range %in% gp_60, "60-69",
                                                                 ifelse(tmp$age_range %in% gp_70, "70+", "資料缺失")))))))
      tmp$age_range <- as.factor(tmp$age_range)
      tmp <- tmp %>%
        group_by(assigned_onset_date, age_range)%>%
        summarise(Total = sum(case_count))
      tmp <- subset(tmp, assigned_onset_date >= as.Date("2021-05-01"))
      tmp$assigned_onset_date <- as.Date(tmp$assigned_onset_date, format="$Y-%m-%d")
      
      
      tmp_1 <- tmp %>%
        group_by(assigned_onset_date) %>%
        summarise(Total = sum(Total))
      
      #case_color_list <- c("#fdf0ec",
       #                    "#fad1c6",
       #                    "#f7b2a1",
       #                    "#f4937b",
       #                    "#f17455",
       #                    "#ee562f",
       #                    "#bd320f",
       #                    "#b0d0d3")
      case_color_list2 <- c("#B0C4DE","#E6E6FA","#FFFF00","#FFD700","#FFA500","#FA8072","#FF4500","#DCDCDC") 
      
      case_color_list2 <- case_color_list2[1: sum(table(levels(tmp$age_range)))]
      
      highchart(type = "chart") %>%
        hc_add_series(data = tmp, hcaes(name = assigned_onset_date, x = assigned_onset_date, y = Total, group = age_range),
                      type = "column", stacking = "percent", marker = list(symbol = "diamond"), color = case_color_list2) %>%
        hc_xAxis(type = "datetime", title = list(text = "發生日"))%>%
        hc_yAxis(type = "linear", title = list(text = "比例")) %>%
        hc_legend(layout = "vertical", align = "right")
    })
    
    #download
    output$downloadData_newcase <- downloadHandler(
      filename = function() {
        paste0("new_case_data.csv")
      },
      content= function(file){
        req(input$epicurve_sex)
        write.csv(
          if(input$epicurve_sex == "所有性別"){
            new_case[]
          }else{
            new_case[new_case$sex==input$epicurve_sex,]
            },
          file, row.names= FALSE)
      }
    )
    
    # epicurve_time ----
    output$epicurve_time <- renderPlot({
      ggplot() +
        geom_bar(data=CDC_onset_long,mapping = aes(x = Oncetdate, y=count, fill=Confirmgrouprev),stat="identity")+
        labs(title = "",x="發病日",y="個案數") +
        scale_fill_manual(values=heat.colors(colourCount))+
        #scale_fill_manual(values=color_list2)+
        # scale_fill_manual(values =colorRampPalette(brewer.pal(colourCount, "Accent"))(colourCount))+
        coord_cartesian(ylim=c(0,650))+scale_x_date(limits = c(as.Date("2021-05-01"), as.Date("2021-06-12")),date_breaks = "week",  date_minor_breaks = "week",date_labels= "%b/%d")+
        theme_bw()+theme(text = element_text(family = "Heiti TC Light"))+labs(fill='')
    })
    
    # epicurve_case ----
    output$epicurve_case <- renderHighchart({
        tmp <- onset[, -4: -7]
        tmp$onset_date <- as.Date(tmp$onset_date)
        highchart(type = "chart") %>% 
            hc_xAxis(type = "datetime", title = list(text = "症狀開始日"))%>%
            hc_yAxis_multiples(
                               list(title = list(text = "個案數"), type = "linear"),
                               list(title = list(text = "Rt"), type = "linear",  opposite = TRUE, plotLines = list(list(value = 1, color = "black", width = 1)))
                               ) %>%
            hc_add_series(name = "個案數", data = tmp, hcaes(x = onset_date, y = local_case),
                          type = "column", marker = list(symbol = "diamond"), yAxis = 0, color = "#FFCC54") %>%
            hc_add_series(name = "Rt (Cori 方法估計)", data = tmp, hcaes(x = onset_date, y = Rt),
                         type = "line", marker = list(symbol = "diamond"), yAxis = 1, color = "#ff7654") %>%
            hc_add_series(name = "Rt 95% CI (upper)", data = tmp, hcaes(x = onset_date, y = Rt_u),
                          type = "line", marker = list(symbol = "arc"), yAxis = 1, color = "#ffded5") %>%
            hc_add_series(name = "Rt 95% CI (lower)", data = tmp, hcaes(x = onset_date, y = Rt_l),
                          type = "line", marker = list(symbol = "arc"), yAxis = 1, color = "#ffded5")      
        
    })
    
    #download
    output$downloadData_rt <- downloadHandler(
      filename = function() {
        paste0("new_case_rt_data.csv")
      },
      content= function(file){
        write.csv(
          onset,
          file, row.names= FALSE)
      }
    )
    
    #interpretation bottom
    observeEvent(input$show_Rt, {
      showModal(modalDialog(
        title = "每日新發生數與Rt",
        Notes_Rt,
        easyClose = TRUE
      ))
    })
    
     observeEvent(input$show_Mb, {
      showModal(modalDialog(
        title = "人流趨勢的改變",
        Notes_Mb,
        easyClose = TRUE
      ))
    })
    observeEvent(input$show_Newcase, {
      showModal(modalDialog(
        title = "新確診個案的年齡分布",
        Notes_Newcase,
        easyClose = TRUE
      ))
    })

     observeEvent(input$show_Severe, {
      showModal(modalDialog(
        title = "重症或死亡的比例",
        Notes_Severe,
        easyClose = TRUE
      ))
    })

     observeEvent(input$show_Progression, {
      showModal(modalDialog(
        title = "疾病進程",
        Notes_Progression,
        easyClose = TRUE
      ))
    })
     
     observeEvent(input$show_choropleth, {
       showModal(modalDialog(
         title = "發生率於空間上的分佈",
         Notes_Choropleth,
         easyClose = TRUE
       ))
     })
    # epicurve_death ----
    output$epicurve_death <- renderHighchart({
        
        tmp <- death %>%
            group_by(death_date) %>%
            count(death_date)
        
        highchart(type = "chart") %>% 
            hc_add_series(name = "死亡數", data = onset, hcaes(x = as.Date(onset_date), y = death_case_deathdate),
                          type = "line", marker = list(symbol = "diamond"), color = "#6C6868") %>%
            hc_xAxis(type="datetime", title = list(text = "死亡日期"))%>%
            hc_yAxis(type = "linear", title = list(text = "人數"))
        
    })
     
     #download
     output$downloadData_epicurve_death <- downloadHandler(
       filename = function() {
         paste0("epicurve_death_data.csv")
       },
       content= function(file){
         write.csv(
           death,
           file, row.names= FALSE)
       }
     )
    
    # severe_icu ----
    output$severe_icu <- renderHighchart({
        tmp <- daily_info
        highchart(type = "chart") %>% 
            hc_add_series(name = "重症個案住ICU數目", data = tmp, hcaes(x = as.Date(date), y = icu),
                          type = "column", marker = list(symbol = "diamond"), color="#05299E") %>%
            hc_xAxis(type="datetime", title = list(text = "公布日期")) %>%
            hc_yAxis(type = "linear", title = list(text = ""))
    })
    
     # severe_res ----
    output$severe_res <- renderHighchart({
        tmp <- daily_info
        highchart(type = "chart") %>% 
            hc_add_series(name = "重症個案使用呼吸器數目", data = tmp,
                          hcaes(as.Date(date), y = respirator),
                          type = "column", marker = list(symbol = "diamond"), color="#947BD3") %>%
            hc_xAxis(type="datetime", title = list(text = "公布日期"),
                     plotBands = list(
                       list(
                            color = "lightgray",
                            from = datetime_to_timestamp(as.Date("2021-05-17"))+60*60*1000,
                            to = datetime_to_timestamp(as.Date("2021-05-18"))+60*60*1000
                            ),
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-20"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-21"))+60*60*1000
                       ),
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-28"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-29"))+60*60*1000
                       ),
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-30"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-31"))+60*60*1000
                       )
                       
           
                     ))%>%
          
            hc_yAxis(type = "linear", title = list(text = ""))
            
    })     
    
   
     # severe_f ----
    output$severe_res_f <- renderHighchart({
        tmp <- daily_info
        highchart(type = "chart") %>% 
            hc_add_series(name = "重症個案呼吸衰竭數目", data = tmp,
                          hcaes(x = as.Date(date), y = respiratory_failure),
                          type = "column", marker = list(symbol = "diamond"), color="#F0A7A0") %>%
            hc_xAxis(type="datetime", title = list(text = "公布日期"))%>%
            hc_yAxis(type = "linear", title = list(text = ""))
    })
    
     # severe_ecmo ----
    output$severe_ecmo <- renderHighchart({
        tmp <- daily_info
        highchart(type = "chart") %>% 
            hc_add_series(name = "重症個案使用葉克膜數目", data = tmp,
                          hcaes(x = as.Date(date), y = ecmo),
                          type = "column", marker = list(symbol = "diamond"), color="#F26CA7") %>%
            hc_xAxis(type="datetime", title = list(text = "公布日期"),
                     plotBands = list(
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-17"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-18"))+60*60*1000
                       ),
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-20"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-22"))+60*60*1000
                       ),
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-23"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-24"))+60*60*1000
                       ),
                       
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-27"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-29"))+60*60*1000
                       ),
                       list(
                         color = "lightgray",
                         from = datetime_to_timestamp(as.Date("2021-05-30"))+60*60*1000,
                         to = datetime_to_timestamp(as.Date("2021-05-31"))+60*60*1000
                       )
                     ))%>%
            hc_yAxis(type = "linear", title = list(text = ""))
    })
     
     #download
     output$downloadData_severe_res_f_icd_ecmo <- downloadHandler(
       filename = function() {
         paste0("severe_data.csv")
       },
       content= function(file){
         write.csv(
           daily_info,
           file, row.names= FALSE)
       }
     )
     
     # death_duration ----
     # done 20210615 
     output$death_duration <- renderHighchart({
    
       if(input$death_sex == "所有性別"){
         tmp <- death[c("age", "sex", "duration")]
         death_sex_color <- "#FFCC54"
         death_total_color <-"#D64326"
       }
       else{
         tmp <- subset(death, death$sex == input$death_sex)
         
         tmp <- tmp[c("age", "sex", "duration")]
         death_sex_color <- ifelse(input$death_sex == "女性", "#fe8081", "#6ca6d6")
         death_total_color <- ifelse(input$death_sex == "女性", "#fe80c0", "#6c71d6")
       }
       
       age_na <- toString(sum(is.na(tmp$age)))
       duration_na <- toString(sum(is.na(tmp$duration)))
       text <- paste("不包含含資訊遺漏之個案",  "(無年齡資訊: ", age_na, "個個案)", "\n", "(無日期資訊: ", duration_na, "個個案 )")
       tmp <- tmp[complete.cases(tmp), ]
       tmp$age <- as.factor(tmp$age)
       
       highchart() %>%
         hc_xAxis(type = "category", title = list(text = text)) %>%
         hc_yAxis(title = list(text = "時間 (天)")) %>%
         hc_add_series_list(data_to_boxplot(data=tmp, duration, name="整體", color=death_total_color)) %>%
         hc_add_series_list(data_to_boxplot(data=tmp, duration, age, name="年齡", color=death_sex_color))
       
       
     })
     
     # death_piechart ----
     # done 20210615
     output$death_piechart <- renderHighchart({
       if(input$death_sex == "所有性別"){
         tmp <- death  
         tmp$age <- ifelse(is.na(tmp$age), "資料缺失", tmp$age)
         tmp <- tmp %>%
           group_by(age)
         tmp <- count(tmp)
         tmp$percent = tmp$n * 100/sum(tmp$n)
       }
       else{
         tmp <- subset(death, death$sex == input$death_sex) 
         tmp$age <- ifelse(is.na(tmp$age), "資料缺失", tmp$age)
         tmp <- tmp %>%
           group_by(age)
         tmp <- count(tmp)
         tmp$percent = tmp$n * 100/sum(tmp$n)
       }
       tmp$x = c(1: dim(tmp)[1])
       tmp$age <- as.factor(tmp$age)
       hchart(name="個案數", tmp, "pie", hcaes(name = age, y = n)) %>%
         hc_plotOptions(pie= list(dataLabels = list(enabled = TRUE,format='<b>{point.name}</b>: {point.percent:.1f} %')))
     })
    
     #download
     output$downloadData_death <- downloadHandler(
       filename = function() {
         paste0("death_data.csv")
       },
       content= function(file){
         req(input$death_sex)
         write.csv(
           death[death$sex==input$death_sex,],
           file, row.names= FALSE)
       }
     )
     
     
    # severe_cumu ----
    output$severe_cumu <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "重症個案", data = severe2, hcaes(x = as.Date(onset_date), y = severe_case),
                          type = "line", marker = list(symbol = "diamond"), color="#FFA154") %>%
            hc_add_series(name = "累積死亡數", data = severe2, hcaes(x = as.Date(onset_date), y = cum_death),
                          type = "line", marker = list(symbol = "diamond"), color = "#EF6351") %>%
            hc_add_series(name = "新個案數", data = severe2, hcaes(x = as.Date(onset_date), y = new_case),
                          type = "line", marker = list(symbol = "diamond"), color = "#C2EABD") %>%
            hc_xAxis(type="datetime", title = list(text = "新個案數(症狀開始日),死亡與重症個案(公布日期)"))%>%
            hc_yAxis(type = "linear", title = list(text = "")) %>%
            hc_legend(layout = "horizen")
        
    })
    
    # severe_new ----
    output$severe_new <- renderHighchart({
      
      severe2$severe_rate <- severe2$severe_case * 100/ severe2$cum_case
      severe2$death_rate <- severe2$cum_death * 100/ severe2$cum_case
      
        highchart(type = "chart") %>%
            hc_yAxis_multiples(
              list(title = list(text = "個案數"), type = "linear"),
              list(title = list(text = "比例(%)"), type = "linear",  opposite = TRUE)
            ) %>%
            hc_add_series(name = paste("重症與死亡以外之個案"), data = severe2, hcaes(x = as.Date(onset_date), y = new_case),
                          type = "column", stacking="container", marker = list(symbol = "diamond"), color="#AFD2E9", yAxis = 0) %>%
            hc_add_series(name = "累積死亡數", data = severe2, hcaes(x = as.Date(onset_date), y = cum_death),
                          type = "column", stacking="container", marker = list(symbol = "diamond"), color = "#EF6351", yAxis = 0) %>%
            hc_add_series(name = "重症個案數", data = severe2, hcaes(x = as.Date(onset_date), y = severe_case),
                          type = "column", stacking="container", marker = list(symbol = "diamond"), color="#FFA154",  yAxis = 0) %>%
            hc_add_series(name = "重症個案比例(%)", data = severe2, hcaes(x = as.Date(onset_date), y = severe_rate), 
                          type = "line", marker = list(symbol = "diamond"), color="#FCB07E", yAxis = 1) %>%
            hc_add_series(name = "累積死亡個案比例(%)", data = severe2, hcaes(x = as.Date(onset_date), y = death_rate), 
                          type = "line", marker = list(symbol = "diamond"), color="#F0877F", yAxis = 1) %>%
            hc_xAxis(type = "datetime", title = list(text = "發生日"))%>%
            
            hc_legend(layout = "horizen")
    })
    
     #download
     output$downloadData_severe <- downloadHandler(
       filename = function() {
         paste0("severe_data.csv")
       },
       content= function(file){
         write.csv(
           severe2,
           file, row.names= FALSE)
       }
     )
     
    # severe_bed ----
    output$severe_bed <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "專責病房收治人數", data = onset, hcaes(x = as.Date(onset_date), y = bed),
                          type = "line", marker = list(symbol = "diamond"),color="#EF798A") %>%
            hc_add_series(name = "專責加護病房收治人數", data = onset, hcaes(x = as.Date(onset_date), y = ICU_bed),
                          type = "line", marker = list(symbol = "diamond"), color = "#D14081") %>%
            hc_xAxis(type = "datetime", title = list(text = "日期"))%>%
            hc_yAxis(type = "linear", title = list(text = "")) %>%
            hc_legend(layout = "horizen")
     })
    
     #download
     output$downloadData_severe_bed <- downloadHandler(
       filename = function() {
         paste0("severe_bed_data.csv")
       },
       content= function(file){
         write.csv(
           onset,
           file, row.names= FALSE)
       }
     )
     
    # mob ----
    output$mob_g <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "雜貨店和藥局", data = data_2021, hcaes(x = as.Date(date), y = grocery_and_pharmacy_percent_change_from_baseline),
                          type = "line", marker = list(symbol = "diamond"), color = "#ea7973") %>%
            hc_xAxis(type="datetime",title = list(text = ""),
                     plotLines=list(list(label=list(text="(1)"),color="#778899",dashStyle="Dash",value=130),
                                    list(label=list(text="(2)"),dashStyle="Dash",color="#778899",value=134),
                                    list(label=list(text="(3)"),dashStyle="Dash",color="#778899",value=138)))%>%
            hc_yAxis(type = "linear", title = list(text = "比例")) %>%
            hc_legend(layout = "horizen")
    })

    output$mob_p <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "公園", data = data_2021, hcaes(x = as.Date(date), y = parks_percent_change_from_baseline),
                          type = "line", marker = list(symbol = "diamond"), color = "#cec162") %>%
            hc_xAxis(type="datetime",title = list(text = ""),plotLines=list(list(color="#778899",dashStyle="Dash",value=130),list(dashStyle="Dash",color="#778899",value=134),list(dashStyle="Dash",color="#778899",value=138)))%>%
            hc_yAxis(type = "linear", title = list(text = "比例")) %>%
            hc_legend(layout = "horizen")
    })
    
    output$mob_res <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "住宅區", data = data_2021, hcaes(x = as.Date(date), y = residential_percent_change_from_baseline),
                          type = "line", marker = list(symbol = "diamond"), color = "#66c187") %>%
            hc_xAxis(type="datetime",title = list(text = ""),plotLines=list(list(color="#778899",dashStyle="Dash",value=130),list(dashStyle="Dash",color="#778899",value=134),list(dashStyle="Dash",color="#778899",value=138)))%>%
            hc_yAxis(type = "linear", title = list(text = "比例")) %>%
            hc_legend(layout = "horizen")
    })

    output$mob_rr <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "零售店和休閒設施", data = data_2021, hcaes(x = as.Date(date), y = retail_and_recreation_percent_change_from_baseline),
                          type = "line", marker = list(symbol = "diamond"), color = "#44bbb6") %>%
            hc_xAxis(type="datetime",title = list(text = ""),plotLines=list(list(color="#778899",dashStyle="Dash",value=130),list(dashStyle="Dash",color="#778899",value=134),list(dashStyle="Dash",color="#778899",value=138)))%>%
            hc_yAxis(type = "linear", title = list(text = "比例")) %>%
            hc_legend(layout = "horizen")
    })
    

    output$mob_tran <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "大眾運輸站", data = data_2021, hcaes(x = as.Date(date), y = transit_stations_percent_change_from_baseline),
                          type = "line", marker = list(symbol = "diamond"), color = "#629dff") %>%
            hc_xAxis(type="datetime",title = list(text = ""),plotLines=list(list(color="#778899",dashStyle="Dash",value=130),list(dashStyle="Dash",color="#778899",value=134),list(dashStyle="Dash",color="#778899",value=138),list(label=list(text="端午連假day1"),dashStyle="Dash",color="#8B4513",value=162)))%>%
            hc_yAxis(type = "linear", title = list(text = "比例")) %>%
            hc_legend(layout = "horizen")
    })
    
    output$mob_w <- renderHighchart({
        highchart(type = "chart") %>%
            hc_add_series(name = "工作場所", data = data_2021, hcaes(x = as.Date(date), y = workplaces_percent_change_from_baseline),
                          type = "line", marker = list(symbol = "diamond"), color = "#ee7add") %>%
            hc_xAxis(type="datetime",title = list(text = ""),plotLines=list(list(color="#778899",dashStyle="Dash",value=130),list(dashStyle="Dash",color="#778899",value=134),list(dashStyle="Dash",color="#778899",value=138)))%>%
            hc_yAxis(type = "linear", title = list(text = "比例")) %>%
            hc_legend(layout = "horizen")
    })
    
  # contributors & acknowledge ----
  output$contributors <- renderTable({contributors})
  output$acknowledgement <- renderTable({acknowledge})


  # map ----
  # define color palette
  # colpalette_new_perCapita<- colorNumeric(palette= "viridis", domain= covide_rt_epi$new_perCapita, na.color= "transparent")
  colpalette_new_perCapita<- colorBin(palette="YlOrBr", domain= covide_rt_epi$new_perCapita, bins= 7, na.color= "#f0f4f780")
  
  #create slider input depending on data frequency
  observe({
    allDates <- seq(min(covide_rt_epi$assigned_onset_date, na.rm= T), max(covide_rt_epi$assigned_onset_date, na.rm= T), by = "days")
    eligibleDates <- allDates[xts::endpoints(allDates, on = input$frequency)]
    
    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  step = 1,
                  timeFormat = "%Y-%m-%d",
                  animate = animationOptions(interval = 500, loop = FALSE)
      )
    })
  })
  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    covide_rt_epi[covide_rt_epi$assigned_onset_date==input$dateSel,]
  })
  # create the base leaflet map
  output$map <- renderLeaflet({
    
    leaflet(TWshp_sf)%>%
      #addTiles(
      addProviderTiles(providers$CartoDB.Positron
      )%>%
      setView(lat=23.6, lng=121 , zoom=8)%>%
      #addPolygons(data= TWshp_ct, layerId= ~COUNTYCODE,
      #            color= "#4d648f", opacity= .8)%>%
      addPolygons(layerId= ~TOWNCODE,
                  stroke= T,
                  fillOpacity= 1,
                  color= "#4d648f",
                  opacity= 1,
                  weight=1,
                  highlight= highlightOptions(
                    stroke= T,
                    color= "#009dff",
                    opacity= 1,
                    weight= 3,
                    sendToBack = TRUE,
                    bringToFront = TRUE))%>%
      leaflet::addLegend(pal= colpalette_new_perCapita,
                         values= covide_rt_epi$new_perCapita,
                         opacity= 0.9,
                         title= "每十萬人發生率",
                         position= "bottomright")
    
  })
  # prepare data depending on selected date and draw update polygons
  observe({
    TWshp_sf$new_perCapita<- filteredData()$new_perCapita[match(TWshp_sf$TOWNCODE, filteredData()$TOWNCODE)]
    
    TWshp_sf$Label_text<- paste0(
      "縣市區: ", TWshp_sf$COUNTYNAME, TWshp_sf$TOWNNAME, "<br/>",
      "每十萬人發生率: ", TWshp_sf$new_perCapita%>%round(digits= 3))
    
    leafletProxy("map", data = TWshp_sf) %>%
      clearMarkers() %>%
      setShapeStyle(layerId = ~TOWNCODE, 
                    fillColor = ~colpalette_new_perCapita(new_perCapita))%>%
      setShapeLabel(layerId = ~TOWNCODE, label= TWshp_sf$Label_text)
  })

  #download
  output$downloadData_choropleth <- downloadHandler(
    filename = function() {
      paste0("stat_in_choropleth.csv")
    },
    content= function(file){
      req(input$dateSel)
      write.csv(
        covide_rt_epi[which(covide_rt_epi$assigned_onset_date==input$dateSel),]%>%
          select(-TOWNCODE, assigned_onset_date, site_id, case_count, population, new_perCapita)%>%
          rename(縣市名= site_id, 區人口數= population, 確診日= assigned_onset_date,
                 新增確診數= case_count, 每十萬人口發生率= new_perCapita),
        file, row.names= FALSE)
    }
  )
}   
shinyApp(ui, server)


