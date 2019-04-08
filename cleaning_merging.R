## load pkg

library(tidyverse)

med16 <- read_csv("data/unprocessed/medicare_charges_2016.csv", guess_max = 150000)

med15 <- read_csv("data/unprocessed/medicare_charges_2015.csv", guess_max = 150000)

# was running into some issues with parsing at around observation 115,000, so I 
# changed the guess_max

####Little bit of tidying####

# renaming variables

# all I want to do is make the column names lowercase and separated with underscores

rename_cols <- function(df){
  colnames(df) <- str_to_lower(colnames(df))
  colnames(df) <- str_replace_all(colnames(df), " ", "_")
  colnames(df)
}

colnames(med16) <- rename_cols(med16)

colnames(med15) <- rename_cols(med15)


map_chr(med16, class)

map_chr(med15, class)


med16$total_discharges <- as.numeric(med16$total_discharges)


### average_total_payment, average_covered_charges, average_medicare_payments are
## all character vectors because they have dollar signs and commas, going to get rid of those

numeric_vars <- med16 %>% select(average_covered_charges, 
                                 average_total_payments, 
                                 average_medicare_payments)

map_df(numeric_vars, function(x){str_replace(x, "\\$", "")})

map_df(numeric_vars, function(x){str_replace_all(x, ",", "")})


# matched the patterns properly, so now it's time to fix it all in the med16 df

med16 <- map_df(med16, function(x) str_replace(x, "\\$", ""))
med16 <- map_df(med16, function(x) str_replace_all(x, ",", ""))

med16 <- med16 %>% mutate(
  average_covered_charges = as.numeric(average_covered_charges),
  average_total_payments = as.numeric(average_total_payments),
  average_medicare_payments = as.numeric(average_medicare_payments),
  total_discharges = as.integer(total_discharges)
)

map_chr(med16, class)

## let's merge these datasets

#making sure the classes of each df are compatible for merging
# Coercing is sometimes bad form but I know for a fact that zipcodes and provider ids 
# are numbers.

med16$provider_id <- as.integer(med16$provider_id)

med16$provider_zip_code <- as.integer(med16$provider_zip_code)

#renaming columns before joining

med16 <- med16 %>% rename("tot_discharges_16" = "total_discharges",
                          "avg_covered_charges_16" = "average_covered_charges",
                          "avg_tot_payments_16" = "average_total_payments",
                          "avg_medicare_payments_16" = "average_medicare_payments")

med15 <- med15 %>% rename("tot_discharges_15" = "total_discharges",
                          "avg_covered_charges_15" = "average_covered_charges",
                          "avg_tot_payments_15" = "average_total_payments",
                          "avg_medicare_payments_15" = "average_medicare_payments")

med <- inner_join(med16, med15, by = c("drg_definition","provider_name", "provider_state", 
                                       "provider_city", "provider_street_address", "provider_id", 
                                       "provider_zip_code"))


## it worked, nice 

## writing 4 csvs to put into the processed data folder

write_csv(med, "data/processed/medicare_inpatient_15_16.csv")

write_csv(med15, "data/processed/medicare_inpatient_15.csv")

write_csv(med16, "data/processed/medicare_inpatient_16.csv")




