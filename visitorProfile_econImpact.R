# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(noncensus)

# Read in data -- survey file on local machine
survey <- read_csv('survey.csv')

# Create copy for validation
survey_copy <- survey

# Collapse zip codes into one column
survey$Zip <- ifelse(is.na(survey$GNOzip) == FALSE, survey$GNOzip, survey$Zip)
survey$GNOzip <- NULL

# Remove foreign zips. Convert zip to character. Reattach leading "0" to appropriate zips. 
survey$Zip <- ifelse(survey$Country == '', survey$Zip, NA)
survey$Zip <- as.character(survey$Zip)
survey$Zip <- ifelse(nchar(survey$Zip) < 5 & is.na(survey$Zip) == FALSE & survey$Country == '',
                  paste0('0', survey$Zip), survey$Zip)

# Attach geographic information
## Import/clean zip code data
zip <- data(zip_codes)
names(zip_codes)[1] <- 'Zip'
zip_codes$fips <- as.character(zip_codes$fips)
zip_codes$fips <- ifelse(nchar(zip_codes$fips) < 5, paste0('0', zip_codes$fips), zip_codes$fips)
extra_cols_zip <- c('state', 'latitude', 'longitude')
zip_codes <- zip_codes[ , !(names(zip_codes) %in% extra_cols_zip)]

# Import/clean county data
county <- data(counties)
counties$fips <- paste0(counties$state_fips, counties$county_fips)
extra_cols_county <- c('state_fips', 'county_fips', 'fips_class', 'CSA', 'CBSA', 'population')
counties <- counties[, !(names(counties) %in% extra_cols_county)]
geo_info <- merge(zip_codes, counties, by = 'fips')
survey <- left_join(survey, geo_info, by = 'Zip')

# Clean visitor data
## Make sure all known metro residents are coded as LiveGNO = 1
survey$LiveGNO <- ifelse(is.na(survey$Zip) == FALSE & survey$fips %in% c(22051, 22071, 22075, 22087, 22089, 22093, 22095, 22103),
                      1, survey$LiveGNO)

## Make sure all known out-of-town guests are coded as LiveGNO = 2
survey$LiveGNO <- ifelse(is.na(survey$Zip) == FALSE & !(survey$fips %in% c(22051, 22071, 22075, 22087, 22089, 22093, 22095, 22103)),
                      2, survey$LiveGNO)

## Assumption: Anyone with accommodation information is from out of town
survey$LiveGNO <- ifelse(is.na(survey$LiveGNO) == TRUE & (is.na(survey$Overnight) == FALSE | survey$NIGHTS > 0 | is.na(survey$Accommodate) == FALSE),
                      2, survey$LiveGNO)

## Remove complete missing records
survey <- survey[!(is.na(survey$LiveGNO)), ]

# Cleaning out-of-town visitor data
## Delete all accommodations info for locals
for (i in 5:22) {
  survey[[i]] <- ifelse(survey$LiveGNO == 1, NA, survey[[i]])
}
rm(i)

## Delete accommodations info for out-of-towners who did not stay overnight
for (i in c(6:9, 11:12)) {
  survey[[i]] <- ifelse(survey$Overnight == 2, NA, survey[[i]])
}
rm(i)

## Set room rate to '0' for out-of-towners who did not stay overnight
survey$RATEALL <- ifelse(survey$Overnight == 2, 0, survey$RATEALL)

## Correct "Overnight" variable for those with accommodation info
survey$Overnight <- ifelse(is.na(survey$Overnight) == TRUE & survey$NIGHTS > 0, 1, survey$Overnight)
survey$Overnight <- ifelse(is.na(survey$Overnight) == TRUE & is.na(survey$Accommodate) == FALSE, 1, survey$Overnight)

## Correct for those with 1 valid "days" or "nights" entry. 
## ASSUMPTION: Days = Nights + 1
survey$DAYS <- ifelse(is.na(survey$DAYS) == T & is.na(survey$NIGHTS) == F, survey$NIGHTS + 1, survey$DAYS)
survey$NIGHTS <- ifelse(is.na(survey$NIGHTS) == T & is.na(survey$DAYS) == F, survey$DAYS - 1, survey$NIGHTS)

## Remove outliers (if visit is more than 2 weeks)
survey$DAYS <- ifelse(survey$DAYS > 15 | survey$DAYS < 1, NA, survey$DAYS)
survey$NIGHTS <- ifelse(survey$NIGHTS > 14 | survey$NIGHTS < 1, NA, survey$NIGHTS)

## FOR REPORT: CONTINGENCY TABLE OF GUESTS
survey %>%
  group_by(LiveGNO) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  mutate(perc = total / sum(total) * 100, cumulative_perc = cumsum(total) / sum(total) * 100)

## FOR REPORT: Breakdown of locals by parish
survey %>%
  filter(LiveGNO == 1, is.na(county_name) == FALSE) %>%
  group_by(county_name) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  mutate(perc = total / sum(total) * 100, cumulative_perc = cumsum(total) / sum(total) * 100)

## FOR REPORT: Breakdown of out-of-town guests by state
survey %>%
  filter(LiveGNO == 2, is.na(state) == FALSE) %>%
  group_by(state) %>%
  summarize(total=n()) %>%
  arrange(desc(total)) %>%
  mutate(perc = total / sum(total) * 100, cumulative_perc = cumsum(total) / sum(total) * 100)

## FOR REPORT: Breakdown of international guests by country
## Future Analyses might need recoding -- consider dropdown menu for countries
survey %>%
  filter(Country != '') %>%
  group_by(Country) %>%
  summarize(total=n()) %>%
  arrange(desc(total)) %>%
  mutate(perc = total / sum(total) * 100, cumulative_perc = cumsum(total) / sum(total) * 100)

## FOR REPORT: Breakdown of overnight guests
survey %>%
  filter(is.na(Overnight) == FALSE) %>%
  group_by(Overnight) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  mutate(perc = total / sum(total) * 100, cumulative_perc = cumsum(total) / sum(total) * 100)

## FOR REPORT: Breakdown of days/nights
survey %>% 
  filter(Overnight == 1, is.na(DAYS) == F, is.na(NIGHTS) == F) %>%
  summarize(min_days = min(DAYS), max_days = max(DAYS), min_nights = min(NIGHTS), max_nights = max(NIGHTS),
            avg_days = mean(DAYS), avg_nights = mean(NIGHTS), median_days = median(DAYS), median_nights = median(NIGHTS),
            sd_days = sd(DAYS), sd_nights = sd(NIGHTS))
