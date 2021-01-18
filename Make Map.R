library(tidyverse)
library(magrittr)
library(sf)
library(tmap)
library(gt)
library(viridis)

rm(list = ls())
setwd("C:/Users/maran/Documents/Data Projects/ACS Map")
gc()



# Read in data - one time only needed

ReadACS <- function(filepath){
  cat(paste("file is", filepath, "\n"))
  acs_data <- read_csv(filepath, col_types = cols(SERIALNO = col_character()))
  cat("success \n")
  names(acs_data) %<>% tolower
  gc()
  
  # Select variables of interest
  acs_data %<>% select(serialno, division, sporder, puma, region, st, wagp, sex, rac1p, schl, agep, esr)
  gc() # add pobp, place of birth by state & foreign country
  
  state <- filepath %>% str_extract("csv_p..") %>% str_sub(., start = -2, end = -1)
  acs_data$state <- state
  acs_data %<>% mutate(puma = as.character(puma))
  acs_data %<>% mutate(st = as.character(st))
  
  # filter for working-age adults
  acs_data %<>% filter(agep >= 25 & agep <=60)
  gc()
  
  return(acs_data)
  
}

files_to_read <- list.files("./raw data/acs sample", recursive = T)
files_to_read %<>% grep("(zip)|(pdf)|(pus)", ., value = T, invert = T)
files_to_read %<>% paste0("./raw data/acs sample/",.)

acs_data_list <- lapply(files_to_read, ReadACS)
acs_data <- bind_rows(acs_data_list)

save(acs_data, file = "./intermediate/acs_5year_adults.RDa")





# Can load in data from this point forward


load("./intermediate/acs_5year_adults.RDa")
gc()

# filter for working
acs_data %<>% filter(esr <= 2 & wagp > 0)

# Schooling levels
acs_data %<>% mutate(schl = as.numeric(schl))
acs_data %<>% mutate(educ = case_when(
  schl < 16               ~ 1, # Less than HS degree
  schl == 16 | schl == 17 ~ 2, # HS graduate or GED
  schl == 18 | schl == 19 ~ 3, # Some college, no degree
  schl == 20              ~ 4, # 2 Year College
  schl >= 21              ~ 5  # 4 Year College or more
))
# focus only on HS grad or 4 Y college
acs_sample <- acs_data %>% filter(educ == 2 | educ == 5)
acs_sample %<>% mutate(college = if_else(educ == 5, 1, 0))





### Plots

# Density plots
college_density <- acs_sample %>%
  filter(wagp < 250000) %>%
  mutate(college = as.factor(college)) %>%
  ggplot(., aes(x = wagp, fill = college)) +
  geom_density(alpha = 0.5, adjust = 3) +
  scale_x_continuous(name = "Income", labels = scales::comma) +
  labs(
    title = "Distribution of College Diploma and High School Diploma Earnings",
    y = "Density",
    fill = "College"
  )

educ_density <- acs_data %>%
  filter(wagp < 250000) %>%
  mutate(educ = as.factor(educ)) %>%
  ggplot(., aes(x = wagp, fill = educ)) +
  geom_density(alpha = 0.4, adjust = 3) +
  scale_x_continuous(name = "Income", labels = scales::comma) +
  labs(
    title = "Distribution of Earnings by Educational Level",
    y = "Density"
  ) + 
  scale_fill_discrete(name = "Education", labels = c("Less than HS", "HS Graduate", "Some College",
                                                     "2Y Degree", "4Y Degree+"))


ggsave("./graphs/educational_density.png", plot = educ_density)
ggsave("./graphs/college_hs_density.png", plot = college_density)  
  


# Table
# Summary statistics 
educ_stats <- acs_data %>%
  group_by(educ) %>%
  summarize(obs = n(),
            mean = mean(wagp, na.rm = T),
            std = sd(wagp, na.rm = T),
            p10 = quantile(wagp, probs = 0.1),
            p25 = quantile(wagp, probs = 0.25),
            p50 = quantile(wagp, probs = 0.5),
            p75 = quantile(wagp, probs = 0.75),
            p90 = quantile(wagp, probs = 0.9),
            percent_50k = sum(wagp <= 50000, na.rm = T)/obs,
            percent_100k = sum(wagp <= 100000, na.rm = T)/obs) %>%
  mutate(percent_pop = obs/sum(obs),
         educ_label = cut(educ, breaks = c(0:5), labels = c("Less than HS", "HS Graduate", "Some College",
                                                            "2Y Degree", "4Y Degree+"))) %>%
  relocate(percent_pop, .after = obs) %>%
  select(educ_label, everything())


educ_table <- educ_stats %>%
  select(-obs, -educ) %>%
  gt() %>%
  fmt_percent(columns = vars(percent_50k, percent_100k, percent_pop), decimals = 0) %>%
  fmt_number(columns = vars(mean, std, p10, p25, p50, p75, p90), decimals = 0, sep_mark = ",") %>%
  cols_label(educ_label = "Educational Level",
             percent_pop = "% of Population",
             mean = "Average Income",
             std = "Standard Deviation",
             percent_50k = "% Making 50k or less",
             percent_100k = "% Making 100k or less") %>%
  tab_spanner(label = "Percentiles of Distribution", columns = vars(p10, p25, p50, p75, p90)) %>%
  tab_header(title = "Summary Statistics of Income Distribution by Education")
  

educ_table




# Conditional means by PUMA
cond_means <- acs_sample %>% 
  group_by(state, st, puma, educ) %>%
  summarize(mean_income = mean(wagp),
            count = n()) %>%
  pivot_wider(names_from = educ,
              values_from = c("mean_income", "count")) %>%
  rename(income_hs = mean_income_2,
         income_coll = mean_income_5,
         count_hs = count_2,
         count_coll = count_5) %>%
  mutate(premium = income_coll - income_hs) %>%
  mutate(puma_full = paste0(st, puma),
         total_obs = count_hs + count_coll)



# Make map
county_sf <- st_read("./raw data/shapefile/ipums_puma_2010/ipums_puma_2010.shp")
county_sf <- mutate(county_sf, puma_full = paste0(STATEFIP, PUMA))

means_sf <- left_join(county_sf, cond_means, by = "puma_full")

# exclude alaska, hawaii, and puerto rico
means_sf %<>% filter(!(state %in% c("pr", "ak", "hi")))



# state boundaries
state_sf <- st_read("./raw data/shapefile/nhgis0001_shape/nhgis0001_shapefile_tl2018_us_state_2018/US_state_2018.shp")
gc()
state_sf %<>% filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))


#state_sf_short <- state_sf %>% filter(NAME %in% c("Wisconsin", "Illinois", "Indiana", "Michigan"))



# Function to construct labels
MapBreaksLabels <- function(breaks){
  nbins <- length(breaks)-1
  
  map_breaks_labels <- rep(NULL, (length(breaks)-1))
  for (i in 1:nbins){
    map_breaks_labels[i] <- paste(format(breaks[i], big.mark = ","), "to", 
                                  format(breaks[i+1], big.mark = ","))
  }
  map_breaks_labels[nbins] <- paste(format(breaks[nbins], big.mark = ","), "and above")
  return(map_breaks_labels)
}


# College premium map
map_breaks <- c(seq(from = 5000, to = 50000, by = 5000), 20000000)
map_breaks_labels <- MapBreaksLabels(map_breaks)


pal <-viridis(length(map_breaks), direction = -1)

statemap <- means_sf %>%
  #filter(state %in% c("wi", "il", "mi", "in")) %>%
  tm_shape(.) + 
  tm_fill(col="premium", palette = pal, legend.hist = T, breaks = map_breaks, 
          labels = map_breaks_labels, title = "College Wage Premium") + 
  tm_legend(outside = T) +
  tm_shape(state_sf) +
  tm_borders()

tmap_save(statemap, "./maps/college_premium_map.html")
#tmap_save(statemap, "./maps/college_premium_map_test.png")



# High school wages map
#map_breaks <- c(15000, seq(from = 25000, to = 50000, by = 5000), 100000)
map_breaks <- c(seq(from = 20000, to = 100000, by = 10000), 1000000)
map_breaks_labels <- MapBreaksLabels(map_breaks)
pal <-viridis(length(map_breaks), direction = -1)

hsmap <- means_sf %>%
  #filter(state %in% c("wi", "il", "mi", "in")) %>%
  tm_shape(.) + 
  tm_fill(col="income_hs", palette = pal, legend.hist = T, breaks = map_breaks, 
          labels = map_breaks_labels, title = "High School Graduates Earnings") + 
  tm_legend(outside = T) +
  tm_shape(state_sf) +
  tm_borders()

tmap_save(hsmap, "./maps/hs_income_map.png")




# College wages map
map_breaks <- c(seq(from = 20000, to = 100000, by = 10000), 1000000)
map_breaks_labels <- MapBreaksLabels(map_breaks)
pal <-viridis(length(map_breaks), direction = -1)

collmap <- means_sf %>%
  #filter(state %in% c("wi", "il", "mi", "in")) %>%
  tm_shape(.) + 
  tm_fill(col="income_coll", palette = pal, legend.hist = T, breaks = map_breaks, 
          labels = map_breaks_labels, title = "College Graduates Earnings") + 
  tm_legend(outside = T) +
  tm_shape(state_sf) +
  tm_borders()

tmap_save(collmap, "./maps/college_income_map.png")



# Scatter plot
puma_lm <- lm(income_hs ~ income_coll, data = cond_means)
beta <- puma_lm$coefficients[2] %>% round(digits = 3)

puma_scatter <- ggplot(cond_means, aes(x=income_coll, y = income_hs)) + 
  geom_point(color = "darkblue") + 
  scale_x_continuous(name = "Average College Earnings", labels = scales::comma) +
  scale_y_continuous(name = "Average High School Earnings", labels = scales::comma) +
  labs(title = paste("High School vs. College Earnings by PUMA, beta =", beta)) +
  geom_smooth(method = "lm", se = F, color = "darkred") + 
  coord_fixed(xlim = c(0, 180000), ylim = c(0, 180000)) +
  geom_abline(intercept = 0, slope = 1)

puma_scatter







### Under development



### Heat map

# Categorizing PUMAs
num_bins <- 10
breaks <- nrow(cond_means)/num_bins*1:num_bins

cond_means %<>% ungroup() %>%
  mutate(rank_hs = rank(-income_hs),
         rank_coll = rank(-income_coll),
         top_hs = cut(rank_hs, breaks = c(0, breaks), labels = 1:num_bins),
         top_coll = cut(rank_coll, breaks = c(0, breaks), labels = 1:num_bins))

best_places <- cond_means %>%
  group_by(top_hs, top_coll) %>%
  summarize(num = n())

best_places_heatmap <- ggplot(best_places, aes(y = top_coll, x = top_hs, fill = num)) + 
  geom_tile() +
  scale_fill_viridis(direction = -1, option = "B")

best_places_heatmap






# playing with borders
tm_shape(means_sf) + tm_polygons(col="premium", border.col="white") + tm_legend(outside = T)
tm_shape(means_sf) + tm_polygons(col="income_hs", border.col="white") + tm_legend(outside = T)
tm_shape(means_sf) + tm_polygons(col="income_coll", border.col="white") + tm_legend(outside = T)






# Regression approach

for (i in pumas){
acs_sample$puma_dummy = if_else(acs_sample$puma == i, 1, 0)
reg <- lm(pincp ~ college + puma_dummy + college*puma_dummy, acs_sample)
summary(reg2)


reg3 <- lm(pincp ~ college, acs_sample[acs_sample$puma1 == 1,])
summary(reg3)
}
