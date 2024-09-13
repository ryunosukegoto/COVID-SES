varlabels <- data.frame(t(matrix(c(
  "fips",
  "County FIPS",
  "","",
  
  "PopulationEstimate2019", "Population in 2019",
  "see https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata.csv",
  "US Census",

  "PropMale",
  "PopTotalMale2019 / (PopTotalMale2019 + PopTotalFemale2019)",
  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
  "US Census",
  
  "PropAbove65", "PopulationEstimate65.2019 / PopulationEstimate2019",
  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
  "US Census",
  
  "pctPoverty", "Estimated percent of people of all ages in poverty 2019",
  "https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/",
  "USDA",
  
  "DiabetesPercentage", "Diagnosed Diabetes - Total, Adults Aged 20+ Years, Age-Adjusted Percentage, Natural Breaks, All Counties, in 2019",
  "https://gis.cdc.gov/grasp/diabetes/diabetesatlas-surveillance.html",
  "US Diabetes Surveillance System",
  
  "Smokers_Percentage", "age-adjusted percentages of smokers in 2019",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2022.csv",
  "CDC",
  
  "obesity", "age-adjusted obesity (18+ years old) prevalence in 2019 (%)",
  "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2022.csv",
  "CDC",
  
  "copd", "age-adjusted prevalence of COPD among adults aged 18+ years, 2019",
  "https://www.cdc.gov/copd/data-and-statistics/county-estimates.html",
  "CDC",
  
  "hbp", "Estimated high blood pressure prevalence among adults aged 18+ years, 2019",
  "https://chronicdata.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2021-releas/i46a-9kgh",
  "CDC",
  
  "chd", "Estimated coronary heart disease prevalence among adults aged 18+ years, 2019",
  "https://chronicdata.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2021-releas/i46a-9kgh",
  "CDC",
  
  "stroke", "Estimated stroke prevalence among adults aged 18+ years, 2019",
  "https://chronicdata.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2021-releas/i46a-9kgh",
  "CDC",
  
  "deaths_2021", "The total number of deaths from Covid-19 in 2021 per 100,000 population",
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  "New York Times",

  "Series_Complete_Pop_Pct", "Percent of people who are fully vaccinated (have second dose of a two-dose vaccine or one dose of a single-dose vaccine) based on the jurisdiction and county where recipient lives, as of December 31, 2021",
  "https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh",
  "CDC Vaccination data",
  
  "Booster_Doses_Vax_Pct", "Percent of people who are fully vaccinated and have received a booster (or additional) dose, as of December 31, 2021.",
  "https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh",
  "CDC Vaccination data",
  
  "state", "state", "", "",

  "no.college.diploma", "Percentage without 4+ Years College, Ages 25+, 2015-2019 (5-year)",
  "https://nccd.cdc.gov/DHDSPAtlas/?state=County",
  "CDC",
  
  "black", "Black or African American, non-Hispanic Population (%), All Ages, 2015-2019 (5-year)",
  "https://nccd.cdc.gov/DHDSPAtlas/?state=County",
  "CDC",
  
  "hispanic", "Hispanic/Latino Population (%), 2015-2019 (5-year)",
  "https://nccd.cdc.gov/DHDSPAtlas/?state=County",
  "CDC",
  
  "asian", "Asian or Native Hawaiian or Other Pacific Islander, non-Hispanic Population (%), All Ages, 2015-2019 (5-year)",
  "https://nccd.cdc.gov/DHDSPAtlas/?state=County",
  "CDC",
  
  "native", "American Indian/Alaska Native, non-Hispanic Population (%), All Ages, 2015-2019 (5-year)",
  "https://nccd.cdc.gov/DHDSPAtlas/?state=County",
  "CDC",
  
  "white", "White, non-Hispanic Population (%), 2015-2019 (5-year)",
  "https://nccd.cdc.gov/DHDSPAtlas/?state=County",
  "CDC"
  
  ), nrow=4)), stringsAsFactors=FALSE)
names(varlabels) <- c("Variable","Label","Description","Source")


#' Returns the variable label for a variable name or array of variable names
getlabel <- function(v) sapply(v, function(x)
  varlabels[varlabels$Variable==x,"Label"])

#' Replace variable names with variable labels in text tbl
relabel <- function(tbl) {
  vars <- varlabels$Variable
  vars <- vars[order(nchar(vars), decreasing=TRUE)]
  for (v in vars) {
    tbl <- gsub(v, getlabel(v), tbl, fixed=TRUE)
  }
  tbl
}


