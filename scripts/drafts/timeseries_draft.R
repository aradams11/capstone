#load packages
install.packages("plyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggExtra")
install.packages("heatmaply")


library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggExtra)
library(heatmaply)



# changing to numeric for filtering purposes
bottle$line <- as.numeric(bottle$line)
bottle$station <- as.numeric(bottle$station)

###### Time series plot for oxygen

# Modifying original ts plot to show percentile
oxy_ts_plot_percentile <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }

x <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  x %>% 
    ggplot(aes(x = date, 
               y = oxygen_perc, 
               group = depth_fac, 
               color = depth_fac, 
               shape = as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Oxygen Over Time"), 
         subtitle = "Data is from stations on the shelf within the core sampling region",
         x = "Date", 
         y = "Oxygen (mL/L)", 
         color = "Depth Range (m)", 
         shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit = c(as.Date(date_min), as.Date(date_max)), 
                 date_labels = "%Y %b %d", 
                 breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), 
                       expand = c(0.1, 0.1)) +
    theme_bw() 
}

oxy_ts_plot_percentile(3, 5, '1949-02-28', '2020-01-26')


# Time series plot of the standard deviation in oxygen values
oxy_ts_plot_sd <- function(n_ranges, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }
  
  y <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_sd = sd(oxygen, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  y %>% 
    ggplot(aes(x = date, 
               y = oxygen_sd, 
               group = depth_fac, 
               color = depth_fac, 
               shape = as.factor(quarter))) +
    geom_point(na.rm=T) +
    geom_line(linetype='dashed') +
    labs(title = "Standard Deviation in Oxygen Across All On-Shelf Stations Over Time", 
         x = "Date", 
         y = "Oxygen (mL/L)", 
         color = "Depth Range (m)", 
         shape = "Quarter") +
    scale_shape_discrete(name="Quarter",
                         breaks=c("1", "2", "3","4"),
                         labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit = c(as.Date(date_min), as.Date(date_max)), 
                 date_labels = "%Y %b %d", 
                 breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), 
                       expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

oxy_ts_plot_sd(5, '1970-06-14', '2020-01-26')


## Faceting by  quarter

oxy_ts_plot_percentile_quarters <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = rep("[0,500]", length(year)))
  }
  
  z <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  z %>% 
    ggplot(aes(x = date, 
               y = oxygen_perc, 
               group = depth_fac, 
               color = depth_fac, 
               # shape = as.factor(quarter)
               )) +
    geom_point(na.rm = T) +
    facet_wrap(~ quarter,
               nrow = 2) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Oxygen Across All On-Shelf Stations Over Time"), 
         subtitle = "Q1 is Winter, Q2 is Spring, Q3 is Summer, Q4 is Fall",
         x = "Date", 
         y = "Oxygen (mL/L)", 
         color = "Depth Range (m)", 
         # shape = "Quarter"
         ) +
    # scale_shape_discrete(name="Quarter",
                         # breaks=c("1", "2", "3","4"),
                         # labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit = c(as.Date(date_min), as.Date(date_max)), 
                 date_labels = "%Y %b %d", 
                 breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA), 
                       expand = c(0.1, 0.1)) +
    theme_bw() 
  
}

oxy_ts_plot_percentile_quarters(3, 95, '1949-02-28', '2020-01-26')


###### Time Series plot for temperature

temp_ts_plot_percentile_quarters <- function(n_ranges, percentile, date_min, date_max){
  if (n_ranges > 1){
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac = cut(depth, n_ranges))
  }
  else {
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(depth>=0 & depth<=500,
             line >= 76.7 & line <= 93.3) %>%
      mutate(depth_fac= rep("[0,500]",length(year)))
  }
  
  q <- bottle_sub %>%
    group_by(year, quarter, depth_fac) %>%
    summarise(temperature_perc = quantile(temperature, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  q %>% 
    ggplot(aes(x = date, 
               y = temperature_perc, 
               group = depth_fac, 
               color = depth_fac, 
               # shape=as.factor(quarter)
               )) +
    geom_point(na.rm=T) +
    facet_wrap(~ quarter,
               nrow = 2) +
    geom_line(linetype='dashed') +
    labs(title = paste(percentile,
                       "th Percentile Temperature Across All On-Shelf Stations Over Time"), 
         x = "Date", 
         y = "Temperature (°C)", 
         color = "Depth Range (m)", 
         # shape = "Quarter"
         ) +
    # scale_shape_discrete(name="Quarter",
    #                      breaks=c("1", "2", "3","4"),
    #                      labels=c("Winter", "Spring", "Summer","Fall")) +
    scale_x_date(limit=c(as.Date(date_min),as.Date(date_max)), date_labels = "%Y %b %d", breaks = scales::breaks_pretty(7)) +
    scale_y_continuous(limits=c(NA,NA),
                       expand = c(0.1, 0.1)) +
    theme_bw() 
}

temp_ts_plot_percentile_quarters(2, 95, '1949-02-28', '2020-01-26')



#heat map visual ###



## filter bottle data set 

bottle$line<-as.numeric(bottle$line)
bottle_filter<- bottle %>%
  subset(station <= 60) %>%
  filter(depth>=0 & depth<=500,
         line >= 76.7 & line <= 93.3)


bottle_heat<-bottle_filter %>% select(c("quarter","year","oxygen"))
quarter<-bottle_heat$quarter
oxygen<-bottle_heat$oxygen
year<-bottle_heat$year

#Assign color variables
col1 = "#d8e1cf" 
col2 = "#438484"

#original heatmap 

heat_map<-ggplot(bottle_heat, aes(year,quarter)) + geom_tile(aes(fill = oxygen),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Oxygen Levels")) +
  theme_bw() + theme_minimal() + 
  labs(title="Oxygen Levels From 1949-2020 over 4 quarters",x = "Year", y = "Quarter")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heat_map



#histogram of oxygen levels to help decide how to break up colors on heatmap
hist(bottle_heat$oxygen)


# function to get list of oxygen for heat map
# Modifying original ts plot to show percentile
oxy_hm <- function( percentile, date_min, date_max){
 
    bottle_sub <- bottle %>% 
      subset(station <= 60) %>%
      filter(line >= 76.7 & line <= 93.3, depth<=300) 
  
  h <- bottle_sub %>%
    group_by(year, quarter) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  return(h)
}

hm_data<- oxy_hm(50, '1949-02-28', '2020-01-26')
hm_data<- hm_data %>% group_by(quarter,year)

#heat map with original color sca;e and without breaks 
heat_map<-ggplot(hm_data, aes(year,quarter)) + geom_tile(aes(fill = oxygen_perc),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Oxygen Levels")) +
  theme_bw() + theme_minimal() + 
  labs(title="Oxygen Levels From 1949-2020 over 4 quarters",x = "Year", y = "Quarter")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heat_map

#updated heatmap with different oxygen scale,  different colors, and added units to legend

heat_map2<-ggplot(hm_data, aes(year,quarter)) + geom_tile(aes(fill = oxygen_perc),colour = "white", na.rm = TRUE) +
     scale_fill_gradientn(colours = c("red", "black", "blue"), breaks= c(.25,.5,.75),labels=c(".25 mg/L ",".4 mg/L","6 mg/L")) +  
     guides(fill=guide_legend(title="Oxygen Levels"))+
     theme_bw() + theme_minimal() + 
     labs(title = "Oxygen Levels over quarters from 1949-2020 ",
                   x = "Year", y = "Quarter") +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heat_map2


# interactive heatmap with year and quarter
interactive_test<-plot_ly(x=hm_data$year, y=hm_data$quarter,z=hm_data$oxygen_perc, type="heatmap")
interactive_test



# heatmap without quarters
oxy_hm_wo_quarter <- function( percentile, date_min, date_max){
  
  bottle_sub <- bottle %>% 
    subset(station <= 60) %>%
    filter(line >= 76.7 & line <= 93.3, depth<=300) 
  
  h <- bottle_sub %>%
    group_by(year) %>%
    summarise(oxygen_perc = quantile(oxygen, probs = percentile/100, na.rm = TRUE),
              date = median(date, na.rm = T))
  
  return(h)
}

hm2_data<- oxy_hm_wo_quarter(50, '1949-02-28', '2020-01-26')
hm2_data<- hm2_data %>% group_by(year)

hm2_data$test<-rep(0,71)

interactive_test2<-plot_ly(x=hm2_data$year, y=hm2_data$test, z=hm2_data$oxygen_perc, type="heatmap")
interactive_test2



