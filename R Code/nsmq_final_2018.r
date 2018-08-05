## 9th July 2018 ##

### Libraries ###
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggrepel)

### Import NSMQ 2018 Final Data into R

nsmq_data <- read_csv("C:\\Users\\DAVID\\Documents\\nsmq\\nsmq_final_2018.csv")

nsmq_data$n <- 1:nrow(nsmq_data)

## Cummulative Data ## Reshape data into long format ####

data <- nsmq_data %>% 
  mutate(cum_persco = cumsum(PERSCO), cum_wass = cumsum(WASS), cum_adisco = cumsum(ADISCO)) %>% 
  select(-WASS, -PERSCO, -ADISCO) %>% #View()
  gather(key = school, value = points, -Round, -n)




## Plot cummulative plots

data %>%
  ggplot(aes(n, points)) + 
  geom_line(aes(col = school), size = 2) + 
  geom_vline(xintercept = c(12, 24, 25, 33, 37), linetype = 2, col = "red4") +
  ggtitle(label = "NSMQ2018 Finals Cummulative Points", 
          subtitle = "How did all 3 teams accumulate points towards to the Final Question of the fifth round?") + 
  labs(x = NULL, 
       y = "Points", 
       caption = "By: David Quartey (@DaveQuartey)  \n                Source: NSMQ                               ") + 
  scale_color_manual(values = c("cum_adisco" = "black", "cum_persco" = "mediumturquoise", "cum_wass" = "khaki1")) +
  theme(legend.position= "None",  
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        title = element_text(size = 20),
        text = element_text(size = 20)) + 
  geom_text_repel(data = data.frame(school = c("PERSCO", "WASS", "ADISCO"),
                                    points = c(45, 44, 40), 
                                    n = c(37, 37, 37)),
                  aes(label = school),
                  nudge_x = 5) + 
  geom_point(aes(n, points), size = 0.5) +
  annotate("text", x = 6, y = 55, label = "Round 1", size = 7, col = "blue") +
  annotate("text", x = 18, y = 55, label = "Round 2", size = 7, col = "blue") + 
  annotate("text", x = 24.555, y = 55, label = "Round 3", size = 7, col = "blue") +
  annotate("text", x = 30, y = 55, label = "Round 4", size = 7, col = "blue") +
annotate("text", x = 35, y = 55, label = "Round 5", size = 7, col = "blue")

  #geom_text_repel(data = data.frame(school = c("PERSCO", "WASS", "ADISCO"), 
 #                                   points = c(45, 44, 40), 
  #                                  n = c(37, 37, 37)), 
  #                aes(label = school), 
  #                nudge_x = 5)





# Compare schools scores and lead times ####

max_number_data <- nsmq_data %>% 
  mutate(cum_persco = cumsum(PERSCO), cum_wass = cumsum(WASS), cum_adisco = cumsum(ADISCO)) %>% 
  select(-WASS, -PERSCO, -ADISCO)

for(i in 1:nrow(max_number_data)) {
  max_number <- max(max_number_data[i, 3:5])
  
  
  max_number_data$cum_persco[i] <- ifelse(max_number == max_number_data$cum_persco[i], 1, 0)
  max_number_data$cum_adisco[i] <- ifelse(max_number == max_number_data$cum_adisco[i], 1, 0)
  max_number_data$cum_wass[i] <- ifelse(max_number == max_number_data$cum_wass[i], 1, 0)
  
  
}

max_number_data %>% select(-Round, -n) %>% purrr::map_df(sum)



### Compare points of 1st and 3rd schools at each stage ####

compare_1st_3rd_schools <- nsmq_data %>% 
  mutate(cum_persco = cumsum(PERSCO), cum_wass = cumsum(WASS), cum_adisco = cumsum(ADISCO)) %>% 
  select(-WASS, -PERSCO, -ADISCO)

for(i in 1:nrow(compare_1st_3rd_schools)) {
  max_number <- max(compare_1st_3rd_schools[i, 3:5])
  
  min_number <- min(compare_1st_3rd_schools[i, 3:5])
  
  
  compare_1st_3rd_schools$gap[i]   <- max_number - min_number
  
}

print(max(compare_1st_3rd_schools$gap))

compare_1st_3rd_schools %>% group_by(Round) %>% summarise(mean(gap))

compare_1st_3rd_schools %>% ggplot(aes("1", gap)) + geom_point(aes(col = factor(Round)))



# Gap between NSMQ results from 2017 to 2015
nsmq_data <- read_csv("C:\\Users\\DAVID\\Documents\\nsmq\\nsmq.csv") 


gap <- vector(mode = "double", length = (nrow(nsmq_data)/3))
stage <- vector(mode = "double", length = (nrow(nsmq_data)/3))
year <- vector(mode = "double", length = (nrow(nsmq_data)/3))
school <- vector(mode = "double", length = (nrow(nsmq_data)/3))

for(i in 1:(nrow(nsmq_data)/3)){
  
  
  
  gap_between_schools <- nsmq_data %>% 
    mutate(n = rep(1:(nrow(nsmq_data)/3), rep(3, nrow(nsmq_data)/3))) %>%
    filter(n == i) %>%
    mutate(max_points = max(points), min_points = min(points), gap = max_points - min_points)
  
  gap[[i]] <- gap_between_schools[["gap"]][1]
  
  stage[[i]] <- gap_between_schools[["stage"]][1]
 
  year[[i]] <- gap_between_schools[["year"]][1] 
  
  school[[i]] <- paste(gap_between_schools[["school"]][1], "vs", gap_between_schools[["school"]][2], "vs", gap_between_schools[["school"]][3])
  
}

data.frame(gap = c(gap, 6), stage = c(stage, "Finals"), year = c(year, 2018), school = c(school, "West Africa SHS vs Adisadel College vs St. Peters SHS")) %>% 
  arrange(-desc(gap)) %>% 
  slice(1:20) %>% 
  mutate(status = case_when(school == "West Africa SHS vs Adisadel College vs St. Peters SHS" ~ "TRUE",
                                                                    school != "West Africa SHS vs Adisadel College vs St. Peters SHS" ~ "FALSE")) %>% 
  ggplot(aes(x = reorder(school, -gap))) + 
  geom_bar(aes(weight = gap, fill = status)) + 
  coord_flip() + 
  labs(x = "Schools", y = "Point Gap Between 1st and 3rd Team", caption = "By: David Quartey (@DaveQuartey) \n  Source: NSMQ                                  ") + 
  ggtitle(label = "Comparing Top 20 Closest NSMQ Contests (2015 - 2017) to NSMQ2018 Final") + 
  scale_y_continuous(breaks = c(0, 3, 6, 9)) + 
  theme(title = element_text(size = 15), 
        text = element_text(size = 15)) + 
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "gray")) + 
  theme(legend.position = "None")
  

data.frame(gap = gap, stage = stage, year = year) %>% ggplot(aes(x = gap)) + geom_histogram()+facet_wrap(stage~year)

data.frame(gap = gap, stage = stage, year = year) %>% group_by(year) %>% summarise(mean(gap))

data.frame(gap = gap, stage = stage) %>% ggplot(aes(x = gap)) + geom_histogram()+facet_wrap(~stage)

data.frame(gap = gap, stage = stage) %>%group_by(stage) %>% summarise(mean(gap))

nsmq_data$gap <- gap

nsmq_data %>% ggplot(aes(x = gap)) + geom_histogram()+facet_wrap(~year)

nsmq_data %>% ggplot(aes(x = gap)) + geom_histogram()+facet_wrap(~stage)

nsmq_data %>% group_by(stage) %>% summarise(mean(gap))

