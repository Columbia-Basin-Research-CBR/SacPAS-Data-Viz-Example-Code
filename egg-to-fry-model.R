#======== About ========
# Example R code for programatically generating egg-to-fry survival results from
# Fish Model version 4.0: https://cbr.washington.edu/sacramento/fishmodel/egg-to-fry
# based on parameters specified by the user.
# Author: W. Nicholas Beer, nickbeer@uw.edu
# Last updated: Aug 2026

#======== Introduction ========
# Familiarity with the Egg-to-Fry Modeling GUI of Fish Model is helpful.
# Familiarity with R is helpful.

# Three "modes" are possible with Egg-to-Fry Modeling: Historical, Forecasts, and Scenarios.
# Historical: hindcasts use token=value pairs to retrieve data from the CBR database.
# Forecasts: when available, forecasts are limited to temperature projections through end of November from CVTEMP.
# Scenarios: effectively unlimited customizations with user-provided temperatures, flows, & redd distributions.

#======== Examples of historical runs ========
# These methods can also be used for forecast and scenario runs.

#==== 1. Manage dependencies ====
# install.packages("tidyverse") # TODO: Uncomment if install needed.
library(tidyverse)

#==== 2. Name a workspace directory ====
my_dir <- "MY1234" # TODO: Edit this value, (numerals and characters only).

# Use paste0() to assemble parts of a query string.
# It is recommended to put the directory ID in the first position.
base <- paste0("https://cbr.washington.edu/sac-bin/fishmodel/getandplottemp.pl?dirUseId=", my_dir)
# Subsequent token=value pairs can appear in any order. Pairs are separated by "&".

#==== 3a. Obtain example URLs programatically ====
# &generatequery is set to 1 in order to obtain example URLs to build on.
print(paste0(base, "&generatequery=1"))
# Paste the printed result into a browser (without quotation marks).
# Any resulting URL will use default values for all input parameters seen on Egg-to-Fry Modeling.

#==== 3b. Obtain example URLs manually / via UI ====
# Open Egg-to-Fry Modeling in a browser.
# Choose Intermediate settings or Full settings.
# Modify (or use default) input parameters.
# Check "Get query string".
# Click "Run".

# 3a. and 3b. above will display all of the "raw" codes, each used to obtain a different model run result.
# This script continues with &raw=13, which collects several key outputs.

#==== 4. Modify input parameters in query strings ====
# For example: historical river temperature year, redd distribution year, run-survey type,...

# This script continues with:
# - historical river temperature: &tempsource=dbtemp
# - 2022 as historical river temperature year: &tempyear=2022
# - historical redd distribution: &redds=historical
# - 2022 as redd distribution year: &reddyear=2022
# - redd distribution based on winter carcass survey: &reddssurvey=dbcarcass

# &generatequery=1 is not included, in order to obtain results directly.
basic_url <- paste0(base, "&tempsource=dbtemp&redds=historical&reddssurvey=dbcarcass&raw=13")
custom_url <- paste0(basic_url, "&tempyear=2022&reddyear=2022")
print(custom_url)
# Paste the printed result into a browser (without quotation marks).

#==== 5. Capture results in R ====
# Most results are multi-line CSVs.
result <- read_csv(custom_url, show_col_types = FALSE)
print(result)

# Results with a single value (e.g. &raw=1, Grand Survival) are captured with scan.
print(scan(paste0(base, "&tempsource=dbtemp&tempyear=2022&redds=historical&reddyear=2022&reddssurvey=dbcarcass&raw=1")))

# Compare Grand Survival result with "Incubation Survival" value from previous result.
print(as.numeric(result$Value[grepl("Incubation Survival", result$What)]))
# Another way of indexing to access the same value.
print(as.numeric(result[11, 2]))

#==== 6. Automate comparison of results ====
# Compare the hatching mortality model and incubation mortality model during "critical" water years.
# New token=value pairs introduced here: &mortality=hatchmort (default, thus explicit use is same as omitting) or &mortality=emergemort
# To accelerate delivery of outputs, if the plots are not needed, add &graphics=off

start <- Sys.time()
hatch_mort_results <- emerge_mort_results <- NULL
years <- c(2008, 2014, 2015, 2021, 2022)
for (mort in c("hatchmort", "emergemort")) {
  out <- NULL
  for (i in 1:length(years)) {
    # basic_url has a lot of what we need already
    custom_url <- paste0(basic_url, "&mortality=", mort, "&reddyear=", years[i], "&tempyear=", years[i], "&graphics=off")
    # print(custom_url) # Uncomment to see URLs as they are generated.
    result <- read_csv(basic_url, show_col_types=FALSE)
    # Rename Value column to currently queried year so multiple years of results
    # can be added without issue.
    names(result) <- c("What", years[i])
    # Glue together multiple years of results.
    if (i == 1) {
      out <- result
    } else {
      out <- cbind.data.frame(out, result[,2])
    }
  }
  if (mort == "hatchmort") {
    hatch_mort_results <- out
  } else {
    emerge_mort_results <- out
  }
}
end <- Sys.time()
secs <- as.numeric(difftime(end, start, units = "secs"))
sprintf("Getting hatch mort and emerge mort results for critical years took: %d min %d sec", secs %/% 60, round(secs %% 60))

#==== 7. Compile and present results ====
hatch_mort_report <- hatch_mort_results %>% filter(What == "TDM mortality") %>% pivot_longer(cols = !What, names_to = "year", values_to = "TDM")
emerge_mort_report <- emerge_mort_results %>% filter(What == "TDM mortality") %>% pivot_longer(cols = !What, names_to = "year", values_to = "TDM")
print("Stage-dependent (days before hatching) TDM Mortality")
print(hatch_mort_report)
print("Stage-independent (whole incubation stage) TDM Mortality")
print(emerge_mort_report)

#==== 8. Automate another comparison of results ====
# Model and examine differences in the mortality models across water year types.

start <- Sys.time()
hatch_mort_results <- emerge_mort_results <- NULL
years <- c("C", "D", "BN", "AN", "W")
for (mort in c("hatchmort","emergemort")) {
  out <- NULL
  for (i in 1:length(years)) {
    custom_url <- paste0(basic_url, "&mortality=", mort, "&reddyear=", paste0(years[i], ".year"), "&tempyear=", paste0(years[i], ".year"), "&graphics=off")
    # print(custom_url) # Uncomment to see URLs as they are generated.
    result <- read_csv(custom_url, show_col_types = FALSE)
    # Rename Value column to currently queried year so multiple years of results
    # can be added without issue.
    names(result) <- c("What", years[i])
    # Glue together multiple years of results.
    if (i == 1) {
      out <- result
    } else {
      out <- cbind.data.frame(out, result[,2])
    }
  }
  if (mort == "hatchmort") {
    hatch_mort_results <- out
  } else {
    emerge_mort_results <- out
  }
}

end <- Sys.time()
secs <- as.numeric(difftime(end, start, units = "secs"))
sprintf("Getting hatch mort and emerge mort results for water year types took: %d min %d sec", secs %/% 60, round(secs %% 60))

print("Stage-dependent (days before hatching) TDM Mortality")
print(hatch_mort_results)
print("Stage-independent (whole incubation stage) TDM Mortality")
print(emerge_mort_results)
