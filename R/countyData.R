library(lfe)
library(stargazer)
library(knitr)
library(plm)
library(latex2exp)
library(ggplot2)
library(ggthemes)
library(mvtnorm)
library(kableExtra)
library(data.table) 
library(readxl)  
library(dplyr)
library(dotwhisker)
library(latex2exp)
library(parallel) 


#' Download and merge together various sources of US-county level Covid
#' related data.
#' @param redo If FALSE, will not redownload and recreate the data if
#' data already exists on disk. If TRUE, it will.
countyData <- function(redo=FALSE) {

  rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  datafile <- paste(rootdir,"data/covidCounty.Rda", sep="/")

  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call countyData(redo=TRUE) to update data.\n")
    load(datafile)
    return(sdf)
  }
  

  df.pop <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata.csv", stringsAsFactors=FALSE)
  df.pop <- df.pop[df.pop$COUNTY!=0 & df.pop$YEAR==12,] #remove state totals and keep only 2019 data. Documentation available here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf
  df.pop$fips <- df.pop$COUNTY + 1000*df.pop$STATE
  
  #PopulationEstimate2019
  df.poptotal <- df.pop[df.pop$AGEGRP==0,] #total county population in 2019
  df.poptotal$PopulationEstimate2019 = df.poptotal$TOT_POP
  df.poptotal$PropMale = df.poptotal$TOT_MALE / df.poptotal$TOT_POP * 100
  df.poptotal <- df.poptotal[c("fips","PopulationEstimate2019","PropMale")]
  
  sdf <- df.poptotal
  
  df.poptotal <- NULL
  
  
  #adding poverty data
  povertyData = read.csv(paste(rootdir,"data/PovertyEstimates.csv",sep="/")) #2019 poverty data from https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/ 
  povertyData$fips = factor(povertyData$FIPStxt)
  povertyData$pctPoverty = povertyData$PCTPOVALL_2019
  names(povertyData)[names(povertyData) == "Stabr"] <- "state"
  df <- povertyData[c("fips","pctPoverty","state")]
  df <- subset(df, df$fips!=0)   
  
  sdf <- merge(sdf,df,by="fips",all.x=TRUE)
  #Kalawao county, Hawaii (FIPS 15005) is not included in this data, so add it
  sdf[is.na(sdf$state),]$state = "HI"
  
  #PropAbove65
  df.popabove65 = aggregate(. ~ fips, data=df.pop[df.pop$AGEGRP>=14,][c("fips","TOT_POP")], FUN=sum)
  names(df.popabove65)[names(df.popabove65) == "TOT_POP"] <- "PopulationEstimate65.2019"
  sdf <- merge(sdf,df.popabove65,by="fips",all.x=TRUE)
  
  df.popabove65 <- NULL
  
  sdf$PropAbove65 = sdf$PopulationEstimate65.2019 / sdf$PopulationEstimate2019 * 100

  #DiabetesPercentage
  df.diabetes <- read.csv(paste(rootdir,"data/AgeAdjustedDiabetes2019.csv",sep="/"))
  names(df.diabetes)[names(df.diabetes) == "CountyFIPS"] <- "fips"
  names(df.diabetes)[names(df.diabetes) == "Percentage"] <- "DiabetesPercentage"
  df.diabetes$DiabetesPercentage = as.numeric(df.diabetes$DiabetesPercentage)
  sdf <- merge(sdf,df.diabetes[c("fips","DiabetesPercentage")],by="fips",all.x=TRUE)
  df.diabetes <- NULL 
  
  #Smokers_Percentge and obesity
  #documentation: https://www.countyhealthrankings.org/2022-measures
  df.brfss <- read.csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2022.csv", stringsAsFactors=FALSE)
  df.brfss <- df.brfss[-1,]
  names(df.brfss)[names(df.brfss) == "X5.digit.FIPS.Code"] <- "fips" 
  df.brfss$obesity = as.numeric(df.brfss$Adult.obesity.raw.value) * 100
  df.brfss$Smokers_Percentage = as.numeric(df.brfss$Adult.smoking.raw.value) * 100
  sdf <- merge(sdf,df.brfss[c("fips","Smokers_Percentage","obesity")],by="fips",all.x=TRUE)
  df.brfss <- NULL
  
  #COPD prevalence
  df.copd <- read.csv(paste(rootdir,"data/County_COPD_prevalence.csv",sep="/"))
  names(df.copd)[names(df.copd) == "LocationID"] <- "fips"
  names(df.copd)[names(df.copd) == "Percent_COPD"] <- "copd"
  sdf <- merge(sdf,df.copd[c("fips","copd")],by="fips",all.x=TRUE)
  df.copd <- NULL
  
  #CHD, stroke, hypertension
  #https://chronicdata.cdc.gov/api/views/i46a-9kgh/rows.csv?accessType=DOWNLOAD
  df.places <- read.csv("https://chronicdata.cdc.gov/api/views/i46a-9kgh/rows.csv?accessType=DOWNLOAD", stringsAsFactors=FALSE)
  names(df.places)[names(df.places) == "CountyFIPS"] <- "fips" 
  names(df.places)[names(df.places) == "BPHIGH_AdjPrev"] <- "hbp" 
  names(df.places)[names(df.places) == "CHD_AdjPrev"] <- "chd" 
  names(df.places)[names(df.places) == "STROKE_AdjPrev"] <- "stroke" 
  sdf <- merge(sdf,df.places[c("fips","chd","stroke","hbp")],by="fips",all.x=TRUE)
  df.places <- NULL
  
  #####Build a dataset for total covid cases and deaths (up to May 12, 2022, when cases and deaths have declined and vaccination rates have plateaued)
  #similar to this ecological study: https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(21)00113-9/fulltext
  nyt = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",stringsAsFactors=FALSE)
  #up to March 31, 2021
  nyt_2020 = aggregate(. ~ fips, data=nyt[as.Date(nyt$date)<=as.Date("2021-03-31"),][c("fips","deaths")], FUN=max)
  nyt_2020$deaths_2020 = nyt_2020$deaths
  #up to Dec 31, 2021
  nyt = aggregate(. ~ fips, data=nyt[as.Date(nyt$date)<=as.Date("2021-12-31"),][c("fips","deaths")], FUN=max)
  sdf = merge(sdf,nyt,by="fips",all.x=TRUE)
  sdf = merge(sdf,nyt_2020[c("fips","deaths_2020")],by="fips",all.x=TRUE)
  sdf$deaths = sdf$deaths/sdf$PopulationEstimate2019 * 100000
  #deaths in 2020
  sdf$deaths_2020 = sdf$deaths_2020/sdf$PopulationEstimate2019 * 100000
  #deaths in 2021-2022
  sdf$deaths_2021 = sdf$deaths - sdf$deaths_2020
  
  #vaccination data from: https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
  options(timeout=1000)
  vax = read.csv("https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD") 
  #up to May 12, 2022
  vax = aggregate(. ~ FIPS, data=vax[as.Date(vax$Date, "%m/%d/%y")<=as.Date("2021-12-31"),][c("FIPS","Series_Complete_Pop_Pct","Booster_Doses_Vax_Pct")], FUN=max)
  vax$FIPS = factor(as.numeric(vax$FIPS))
  names(vax)[names(vax) == "FIPS"] <- "fips"
  vax$Series_Complete_Pop_Pct = as.numeric(vax$Series_Complete_Pop_Pct)
  vax$Booster_Doses_Vax_Pct = as.numeric(vax$Booster_Doses_Vax_Pct)
  #remove 0% vaccination counties
  #as per CDC, vaccination rates for these counties are unavailable
  #source: https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=all_states&list_select_county=all_counties&data-type=Vaccinations&metric=Administered_Dose1_Pop_Pct
  vax[vax$Series_Complete_Pop_Pct==0,]$Series_Complete_Pop_Pct = NA
  vax[vax$Booster_Doses_Vax_Pct==0,]$Booster_Doses_Vax_Pct = NA
  sdf = merge(sdf,vax,by="fips",all.x=TRUE)

  no.college.diploma = read.csv(paste(rootdir,"data/no_college_diploma_2015-2019.csv",sep="/")) 
  sdf = merge(sdf,no.college.diploma,by="fips",all.x=TRUE)
  native = read.csv(paste(rootdir,"data/native_2015-2019.csv",sep="/")) 
  sdf = merge(sdf,native,by="fips",all.x=TRUE)
  asian = read.csv(paste(rootdir,"data/asian_2015-2019.csv",sep="/")) 
  sdf = merge(sdf,asian,by="fips",all.x=TRUE)
  black = read.csv(paste(rootdir,"data/black_2015-2019.csv",sep="/")) 
  sdf = merge(sdf,black,by="fips",all.x=TRUE)
  white = read.csv(paste(rootdir,"data/white_2015-2019.csv",sep="/")) 
  sdf = merge(sdf,white,by="fips",all.x=TRUE)
  hispanic = read.csv(paste(rootdir,"data/hispanic_2015-2019.csv",sep="/")) 
  sdf = merge(sdf,hispanic,by="fips",all.x=TRUE)
  
  sdf$white = ifelse(sdf$white<=100,sdf$white,NA)
  #Los Angeles county (fips 6037) has 262% non-Hispanic white population (typo for 26.2%?) and is thus removed
  
  vax <- NA
  
  save(sdf, file=datafile)
  
  return(sdf)
}

