#' @importFrom tidycensus get_acs
get_congressional_district_demographics = function(year=2018, survey="acs5")
{
  # get race data
  df = tidycensus::get_acs("congressional district", table="B03002", output="wide", cache_table=TRUE, year=year, survey=survey)
  total_population = df$B03002_001E
  df_race = data.frame(region           = df$GEOID,
                       total_population = total_population,
                       percent_white    = round(df$B03002_003E / total_population * 100),
                       percent_black    = round(df$B03002_004E / total_population * 100),
                       percent_asian    = round(df$B03002_006E / total_population * 100),
                       percent_hispanic = round(df$B03002_012E / total_population * 100))
  
  # per capita income 
  df_income = tidycensus::get_acs("congressional district", table="B19301", output="wide", cache_table=TRUE, year=year, survey=survey)
  df_income = df_income[, c("GEOID", "B19301_001E")]
  colnames(df_income) = c("region", "per_capita_income")
  
  # median rent
  df_rent = tidycensus::get_acs("congressional district", table="B25058", output="wide", cache_table=TRUE, year=year, survey=survey)
  df_rent = df_rent[, c("GEOID", "B25058_001E")]
  colnames(df_rent) = c("region", "median_rent")

  # median age
  df_age = tidycensus::get_acs("congressional district", table="B01002", output="wide", cache_table=TRUE, year=year, survey=survey)
  df_age = df_age[, c("GEOID", "B01002_001E")]
  colnames(df_age) = c("region", "median_age")
  
  df_demographics = merge(df_race        , df_income, all.x=TRUE)
  df_demographics = merge(df_demographics, df_rent  , all.x=TRUE)  
  df_demographics = merge(df_demographics, df_age   , all.x=TRUE)
  

  df_demographics
}