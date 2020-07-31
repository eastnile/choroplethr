utils::globalVariables(c("District", "Member", "Party", "fips.character", "number"))

# The data comes in as table #6 from wikipedia
# this function scrapes that data, puts it into a df,
# and then selects just the relevant columns
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom dplyr select
get_congressional_rep_raw_data = function()
{
  url    = "https://en.m.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives"
  file   = xml2::read_html(url)
  tables = rvest::html_nodes(file, "table")
  
  # Table #6 is titled "Voting members by state"
  # We care about 2 columns: "District" and "Party" 
  # But we include Member (i.e. their name) for debugging purposes
  reps = rvest::html_table(tables[6], fill = TRUE)
  reps = as.data.frame(reps)
  
  # While the spaces will appear as spaces, they are actually encoded as &nbsp; 
  # which leads to problems later on. This code fixes that.
  # See https://stackoverflow.com/questions/63126514/error-subsetting-data-frame-from-xml2-and-rvest/63126714#63126714
  reps$District = gsub("\u00A0", " ", reps$District, fixed = TRUE)
  reps$Member   = gsub("\u00A0", " ", reps$Member  , fixed = TRUE)
  reps$Party    = gsub("\u00A0", " ", reps$Party.1 , fixed = TRUE)
  
  dplyr::select(reps, District, Member, Party)
}

# add a new column, "state.fips", to the df
# the df must have a column named "state", with the state in all lowercase letters
add_state_fips_code = function(df) 
{
  stopifnot("state" %in% colnames(df))
  
  # merge with "state.regions" df in the choroplethrMaps package
  data("state.regions", package="choroplethrMaps", envir=environment())
  state.regions = state.regions[, c("region", "fips.character")]
  colnames(state.regions) = c("state", "fips.character")
  
  merge(df, state.regions)
}

# the "number" column must always have a leading "0"
# and "at" (short for "at-large") must become "00"
#' @importFrom stringr str_pad
process_number_column = function(df) 
{
  df$number[df$number == "at"] = "00"
  
  # now add leading 0
  df$number = stringr::str_pad(df$number, width=2, side="left", pad="0")
  df
}

# The df comes in with a column named District with values like "Alabama 1" or "Alaska at-large"
# We need a column called "region" that has this same information encoded like "0101" (Alabama first district)
# or "0200" ("Alaska at-large"). 
# The pattern is "state fips code" + "district #". In the case of "at large" district # is 00.
# We need this because the Census API uses this, and we want to add demographics
#' @importFrom tidyr separate unite
add_geoid_to_congressional_rep_data = function(df) 
{
  # first isolate the state name and the congressional seate id.
  df = tidyr::separate(df, col="District", into=c("state", "number"))
  df$state = tolower(df$state)
  
  # now do the processing
  df = add_state_fips_code(df)  
  df = process_number_column(df)
  
  # we can now add a new column, "region" that is the state fips code + the processed number column
  tidyr::unite(df, "region", fips.character, number, sep="", remove=FALSE)
}

# revert party affiliations that changed since the inauguration of this congress
# do this because we care about the relationship between demographics and party affiliation
# that people voted for. This is obscured by the following changes that happened since
# citizens elected official
# 1. Michigan 03. Revert from "Libertarian" to "Republican". Rep. Justin Amash changed party affiliation while in office: https://en.wikipedia.org/wiki/Michigan%27s_3rd_congressional_district
# 2. California 50: Revert "" to "Republican". Rep. Duncan Hunter resigned: https://en.wikipedia.org/wiki/California%27s_50th_congressional_district. 
# 3. Georgia 5: Revert "" to "Democrat".Rep. John Lewis died: https://en.wikipedia.org/wiki/Georgia%27s_5th_congressional_district
# 4. North Carolina 11: Revert "" to "Republican". Rep Mark Meadows resigned: https://en.wikipedia.org/wiki/North_Carolina%27s_11th_congressional_district
# 5. Texas 4: Revert "" to "Republican". Rep. John Ratcliffe resigned: https://en.wikipedia.org/wiki/Texas%27s_4th_congressional_district
revert_changes = function(df)
{
  df[df$District == "Michigan 3"       , "Party"] = "Republican"
  df[df$District == "California 50"    , "Party"] = "Republican"
  df[df$District == "Georgia 5"        , "Party"] = "Democratic"
  df[df$District == "North Carolina 11", "Party"] = "Republican"
  df[df$District == "Texas 4"          , "Party"] = "Republican"
  
  df
}

get_congressional_116_party_data = function()
{
  df = get_congressional_rep_raw_data()
  df = revert_changes(df)
  add_geoid_to_congressional_rep_data(df)
}