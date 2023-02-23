#packages 
library(wqr)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fuzzyjoin)
library(stats)
library(magrittr)
library(purrr)
library(reshape2)
library(lattice)
library(base)
library(RColorBrewer)
library(lattice)

site1 <- c(7302, 3907, 3913, 3914, 6001, 6002, 1720, 2713, 1711, 1721, 1716,
           2718, 7204, 7205, 7207, 2502, 1718, 1710, 1719, 7103, 7101, 1608,
           2402, 7401, 7301, 7300, 7303, 1104, 7601, 1303, 4001, 4005, 7302, 
           7200, 7201)

parameters <- list("ATI_Cl2-Primary")

start <- "2022-04-01"
end <- "2022-06-01"



da <- data_accuracy(site = site1, parameter = parameters,
                    start_date = start,
                    end_date = end, owqm_params = TRUE)
  # group_by(site) # what is the purpose of this group by call?
  # I am not sure the intention of this subset.data.frame call but I don't think
  # it is actually doing anything so I am removing it. If you want to subset a 
  # a data frame then I would recommend using dplyr::filter rather than subset
  # subset.data.frame()
da
# you don't need to call print(da)
# print(da)

# try to use all left side assignment so its clear when new variables are being
# defined
primary_off_by_site <- wqr::read_wiski(
  site = site1, 
  parameter = "ATI_Cl2-Primary-On-Off_Signal", 
  start_date = start, 
  end_date = end, production = FALSE
) %>% 
  filter(result == 1) %>%
  arrange(site) # removed right hand assignment here

# print(primary_off_by_site) # don't need to use print
primary_off_by_site

# you can combine a lot of these steps into a couple piped statements
da_clean <- da %>% 
  # separate the date time like you had below
  separate('date_time_lims', into =c('datelims', 'timelims'), sep = ' ') %>% 
  # only keep distinct LIMS numbers as you had
  dplyr::distinct(lims_number, .keep_all = TRUE) %>% 
  # adding column with numeric date time
  mutate(datelims_num = as.numeric(lubridate::ymd(datelims)))

on_off_clean <- primary_off_by_site %>% 
  # separate the date time like you had below
  separate('date_time', into = c("dateoff", "timeoff"), sep = ' ') %>% 
  # adding column with numeric date time
  mutate(dateoff3 = as.numeric(lubridate::ymd(dateoff)))

danew <- da_clean %>% 
  # joining based on closest time (matches what you had below)
  difference_left_join(
    on_off_clean, 
    by = c("datelims_num" = "dateoff3"), 
    max_dist = 100, 
    distance_col = "timediff"
  ) %>% 

  filter(site.x == site.y, datelims_num >= dateoff3) %>%
  # grouping by lims number so that we can execute the next command on every 
  # different sample
  group_by(lims_number) %>% 
  # returning the minimum time difference for each lims_number
  filter(timediff == min(timediff)) %>% 
  # ungrouping so downstream commands are executed as groups
  ungroup() %>% 
  # selecting your desired columns
  select(lims_number, site = site.x, abs_difference, datelims_num, dateoff3,
         timediff) %>% 
  # another distinct call to only have one instance of each LIMS number - this 
  # is probably not necessary with the grouped filter, but doesn't hurt. 
  distinct(lims_number, .keep_all = TRUE)



plot2 <- danew %>%                          
  ggplot(aes(site, timediff), col = "blue") + 
labs(
     title = "data accuracy OWQM sites",
     x = "Sample Location" ) +
  geom_boxplot() +
 geom_jitter(alpha = 0.4)
plot2


# Some plot ideas:
# 1. Bin the timediff values and plot boxplots of the abs_difference for each site
# 2. Look at the regular difference value to see if values tend to drift 
     #up or down as timediff increases

# adding a plot looking at timediff for absdiff
ggplot(danew, aes(timediff, abs_difference)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw()

view(on_off_clean)
#
library(usethis)
use_git()
