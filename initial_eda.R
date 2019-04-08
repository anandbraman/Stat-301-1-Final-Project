### Load Pkg ###

library(tidyverse)
library(hexbin)
library(ggstance)
library(usmap)

### Reading in data ###

med <- read_csv("data/processed/medicare_inpatient_15_16.csv")

### most commonly diagnosed issue across two years, top 10

med %>% 
  gather(tot_discharges_15,
         tot_discharges_16, 
         key = tot_by_yr,
         value = tot_discharges) %>% 
  group_by(drg_definition) %>% 
  summarise(tot_discharges = sum(tot_discharges)) %>% 
  arrange(desc(tot_discharges)) %>%
  slice(1:10)

### most commonly diagnosed issue by year, top 10

med %>% 
  group_by(drg_definition) %>% 
  summarise( discharges_2015 = sum(tot_discharges_15),
            discharges_2016 = sum(tot_discharges_16)) %>% 
  arrange(desc(discharges_2016, discharges_2015))

## changes in the top 3 most commonly discharged illnesses between 2015 and 2016

top_3 <- med %>% gather(tot_discharges_15,
               tot_discharges_16, 
               key = tot_by_yr,
               value = tot_discharges) %>% 
  mutate(year = case_when(
    tot_by_yr == "tot_discharges_16" ~ "2016",
    tot_by_yr == "tot_discharges_15" ~ "2015"
  )) %>% 
  group_by(drg_definition, year) %>% 
  summarise(tot_discharges = sum(tot_discharges)) %>% 
  arrange(desc(tot_discharges))



top_3 <- top_3[1:6, ]

top_3 %>% 
  ggplot(aes(x = year, y = tot_discharges, color = str_wrap(drg_definition, 40))) +
  geom_point() +
  geom_line(aes(group = drg_definition)) +
  theme(legend.position = "bottom", legend.direction = "vertical",
        legend.text=element_text(size=6), legend.title = element_blank()) +
  xlab("Year") + 
  ylab("Total Discharges")


## calculating percent change in diagnosis between 2015 and 16

### filter for over 25000 cases in 2016 to only consider more common diagnoses

med %>%
  group_by(drg_definition) %>% 
  filter(sum(tot_discharges_16) >= 25000) %>% 
  summarise(percent_chng = (sum(tot_discharges_16)/sum(tot_discharges_15) - 1) * 100) %>% 
  ggplot(aes(x = percent_chng)) +
  geom_histogram(bins = 20) +
  scale_x_continuous("Percent Change 2015-2016", breaks = seq(-40, 40, 20)) +
  scale_y_continuous("Count", breaks = seq(0, 10, 1))
  

med %>%
  group_by(drg_definition) %>% 
  filter(sum(tot_discharges_16) >= 25000) %>% 
  summarise(percent_chng = (sum(tot_discharges_16)/sum(tot_discharges_15)-1) * 100,
            discharges_16 = sum(tot_discharges_16)) %>% 
  arrange(desc(percent_chng)) 

## Summary stats by state

## few columns need to be recalculated based on the methodology

# can't take the average of averages because that weights observations unevenly

# need to be able to construct weighted average with respect to total discharges
## for a certain diagnosis

# constructing total_covered_charges, total_payments and total_medicare_payments
#for each year

med <- med %>% mutate(
  tot_cov_charges_15 = avg_covered_charges_15 * tot_discharges_15,
  tot_cov_charges_16 = avg_covered_charges_16 * tot_discharges_16,
  tot_payments_15 = avg_tot_payments_15 * tot_discharges_15,
  tot_payments_16 = avg_tot_payments_16 * tot_discharges_16,
  tot_medicare_payments_15 = avg_medicare_payments_15 * tot_discharges_15,
  tot_medicare_payments_16 = avg_medicare_payments_16 * tot_discharges_16
)

## highest total payments, across diagnosis by state

avg_total_payments <- med %>% 
  rename("state" = "provider_state") %>% 
  group_by(state) %>% 
  summarise(avg_total_payments_15 = sum(tot_payments_15)/sum(tot_discharges_15),
            avg_total_payments_16 = sum(tot_payments_16)/sum(tot_discharges_16),
            diff = avg_total_payments_16 - avg_total_payments_15
            ) %>% 
  arrange(desc(avg_total_payments_15, avg_total_payments_16))



plot_usmap(data = avg_total_payments, values = "avg_total_payments_15") +
  scale_fill_continuous(str_wrap("Average Total Payments 2015", 15)) +
  theme(legend.position = "right")

plot_usmap(data = avg_total_payments, values = "avg_total_payments_16") + 
  scale_fill_continuous(str_wrap("Average Total Payments 2016", 15)) +
  theme(legend.position = "right")

# mapping change in average total payments

plot_usmap(data = avg_total_payments, values = "diff") +
  scale_fill_gradient2(str_wrap("Difference in Average Total Payments 2015-16", 15)) +
  theme(legend.position = "right")

## difference betweeen total payments and tot med payments gives data about
## copays/individual contributions to the medical bill

med <- med %>% mutate(tot_copay_deductible_16 = tot_payments_16 - tot_medicare_payments_16,
                      tot_copay_deductible_15 = tot_payments_15 - tot_medicare_payments_15)


avg_copay_deductible <- med %>% 
  rename("state" = "provider_state") %>% 
  group_by(state) %>% 
  summarise(avg_copay_16 = sum(tot_copay_deductible_16)/sum(tot_discharges_16),
         avg_copay_15 = sum(tot_copay_deductible_15)/sum(tot_discharges_15),
         diff = avg_copay_16 - avg_copay_15)


avg_copay_deductible %>% 
  arrange(desc(avg_copay_16)) %>% 
  print(n = 51)

plot_usmap(data = avg_copay_deductible, values = "avg_copay_16") +
  scale_fill_continuous(str_wrap("Average Individual Contribution 2016", 15)) +
  theme(legend.position = "right")

plot_usmap(data = avg_copay_deductible, values = "avg_copay_15") +
  scale_fill_continuous(str_wrap("Average Individual Contribution 2015", 15)) +
  theme(legend.position = "right")

plot_usmap(data = avg_copay_deductible, values = "diff") +
  scale_fill_gradient2(str_wrap("Difference in Individual Contribution 2015-16", 15)) +
  theme(legend.position = "right")

#most states saw an increase in average copays/deductibles 
## we should find out if there is an accompanying increase in medicare contributions

med_payments <- med %>% 
  rename("state" = "provider_state") %>% 
  group_by(state) %>% 
  summarise(avg_med_payments_15 = sum(tot_medicare_payments_15)/sum(tot_discharges_15),
            avg_med_payments_16 = sum(tot_medicare_payments_16)/sum(tot_discharges_16),
            diff = avg_med_payments_16 - avg_med_payments_15)

plot_usmap(data = med_payments, values = "avg_med_payments_15") +
  scale_fill_continuous(str_wrap("Average Medicare Contribution 2015", 15)) +
  theme(legend.position = "right")

plot_usmap(data = med_payments, values = "avg_med_payments_16") +
  scale_fill_continuous(str_wrap("Average Medicare Contribution 2016", 15)) +
  theme(legend.position = "right")

plot_usmap(data = med_payments, values = "diff") +
  scale_fill_gradient2(str_wrap("Difference in Avg Medicare Contribution 2015-16", 15)) +
  theme(legend.position = "right")

## did not decrease anywhere!

### most expensive diagnoses to treat

avg_cost <- med %>% 
  group_by(drg_definition) %>% 
  summarise(avg_payments_15 = sum(tot_payments_15)/sum(tot_discharges_15),
            avg_payments_16 = sum(tot_payments_16)/sum(tot_discharges_16),
            avg_medicare_16 = sum(tot_medicare_payments_16)/sum(tot_discharges_16),
            avg_medicare_15 = sum(tot_medicare_payments_15)/sum(tot_discharges_15))

avg_cost %>% arrange(desc(avg_payments_16, avg_tot_payments_15))

## treatments with the biggest difference between total cost and medicare expenditure

copay_deductible <- med %>% 
  group_by(drg_definition) %>% 
  summarise(avg_payments_15 = sum(tot_payments_15)/sum(tot_discharges_15),
            avg_payments_16 = sum(tot_payments_16)/sum(tot_discharges_16),
            avg_medicare_16 = sum(tot_medicare_payments_16)/sum(tot_discharges_16),
            avg_medicare_15 = sum(tot_medicare_payments_15)/sum(tot_discharges_15)) %>% 
  transmute(drg_definition = drg_definition,
            copay_deductible_15 = avg_payments_15 - avg_medicare_15,
            copay_deductible_16 = avg_payments_16 - avg_medicare_16
            ) %>% 
  arrange(desc(copay_deductible_16, copay_deductible_15))
copay_deductible

# most data falls under $15000 

copay_deductible %>% filter(copay_deductible_15 <= 15000) %>% 
  ggplot(aes(x = copay_deductible_15, y = copay_deductible_16)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Individual Contribution 2015") +
  ylab("Individual Contribution 2016")  

copay_deductible %>% 
  mutate(diff = copay_deductible_16 - copay_deductible_15) %>% 
  arrange(diff)

copay_deductible <- copay_deductible %>% 
  mutate(diff = copay_deductible_16 - copay_deductible_15)

copay_deductible %>% 
  filter(diff > -10000) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram(bins = 50) + 
  xlab("Difference in Average Individual Contribution 15-16") +
  ylab("Count")
# shows that a linear model does not fit the data that well, future research could
# investigate the relationship between the two and other factors
  
sd(copay_deductible$diff) 

# how truly massive.



