library(dplyr)
library(scales)
library(sf)
library(tidyverse)
library(ggrepel)
library(ggsci)
library(patchwork)
library(data.table)

load("/Users/mac/Desktop/GBD.Rdata")
load("/Users/mac/Desktop/GBDpop1990_2100.Rdata")
load("/Users/mac/Desktop/population231.Rdata")


df <- read.csv("21+1+5SDI.csv")
head(df)
dfx <- df %>%
  filter(measure_name == "Incidence") %>%
  filter(!cause_name == "Tuberculosis") %>%
  filter(location_name %in%c("Global",
                             "Andean Latin America", "Australasia", "Caribbean", "Central Asia", "Central Europe", 
                             "Central Latin America", "Central Sub-Saharan Africa", "East Asia", "Eastern Europe", 
                             "Eastern Sub-Saharan Africa", "High-income Asia Pacific", "High-income North America", 
                             "North Africa and Middle East", "Oceania", "South Asia", "Southeast Asia", 
                             "Southern Latin America", "Southern Sub-Saharan Africa", "Tropical Latin America", 
                             "Western Europe", "Western Sub-Saharan Africa")) %>%
  filter(sex_name=="Both") %>% 
  filter(age_name=="<20 years") %>%
  filter(metric_name == "Rate") %>% 
  filter(year %in% c(1990,2021))  


p <- ggplot(dfx, aes(x = val, y = location_name, fill = cause_name)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) + 
  facet_wrap(~ year, scales = "free_y") +  
  labs(x = "Proportion", y = "") +  
  theme_classic() +
  scale_x_continuous(labels = scales::percent_format()) +  
  
  scale_fill_npg(name = "") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16, face = "bold"), 
        axis.title.x = element_text(size = 14, face = "bold"),  
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),  
        axis.title.y = element_text(size = 14, face = "bold"),  
        legend.position = "right",
        legend.text =element_text(size = 14))


print(p)

ggsave(paste0("Fig3-proportion-Incidence.pdf"), width = 24, height = 12)
write.csv(dfx, paste0("Fig3-proportion-Incidence.csv"))

