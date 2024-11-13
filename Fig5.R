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
  filter(measure_name == "Deaths") %>%
  filter(cause_name == "Tuberculosis") %>%
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
  filter(year>1989)
SDI21fig=function(dfx=dfx,labx="xxx"){
  # Define the specific order for the location names
  if (nrow(dfx) == 0) {
    p1=  ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "Oops, your data is not suitable for this plot"), size = 12) +
      theme_void()
    spearx = "No data"
    
    return(list(p=p1,spearx=spearx))
  }
  
  
  
  location_order <- c(
    "Global", "High-income Asia Pacific", "High-income North America", "Western Europe",
    "Australasia", "Andean Latin America", "Tropical Latin America", "Central Latin America",
    "Southern Latin America", "Caribbean", "Central Europe", "Eastern Europe", "Central Asia",
    "North Africa and Middle East", "South Asia", "Southeast Asia", "East Asia", "Oceania",
    "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa", "Central Sub-Saharan Africa",
    "Southern Sub-Saharan Africa"
  )
  
  colors <- c(  pal_npg("nrc", alpha = 0.7)(9),
                pal_aaas("default", alpha = 0.7)(9),
                pal_nejm("default", alpha = 0.7)(8),
                pal_jama("default", alpha = 0.7)(7))
  ######### 1.1  Incidence SDI 2019 in 21
  ############### ############### ############### ############### ############### 
  #bin data
  DALY_2017=dfx %>% select(location_id,val,year)
  df11 = left_join(DALY_2017,SDI2019) 
  # get df
  df3=df11 %>% filter(sdi>0.2)
  #plot
  # Filter out rows with NA in 'sdi' or 'val'
  df11_filtered <- df11 %>% 
    filter(!is.na(sdi) & !is.na(val)) %>%
    mutate(location_name = factor(location_name, levels = location_order))
  
  p1=ggplot(df11_filtered, aes(x = sdi, y = val, color = location_name, shape = location_name)) +
    geom_point() +  # Plot points
    geom_smooth(method = "loess", se = T, aes(group = 1), color = "#92A8D1") +  # Add a smoothing line
    scale_shape_manual(values = 1:22,breaks = location_order ,labels = location_order) +  # Use manually defined shapes
    scale_color_manual(values = colors,breaks = location_order ,labels = location_order) +
    labs(x = "SDI", 
         shape="",color="",x=paste0("SDI"),
         y=paste0(labx," per 100,000 population"))+
    theme_bw() +
    theme(legend.key.size = unit(0.03,"line"),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.title=element_text(size=12),
          axis.line = element_line(colour = "black"),
          legend.text=element_text(size=10)) +
    guides(shape = guide_legend(nrow = 22))
  
  
  # Calculate Spearman's correlation
  spearman_cor <- cor.test(df11_filtered$sdi, df11_filtered$val, method = "spearman")
  # Extract the correlation coefficient (R) and p-value
  r <- spearman_cor$estimate;p <- spearman_cor$p.value
  # Print the results
  cat("Spearman's Correlation Coefficient (R):", r, "\n")
  cat("p-value:", p, "\n")
  # Create the combined string using sprintf()
  spearx <- sprintf("r=%.4f, p=%.3e", r, p)
  
  return(list(p=p1,spearx=spearx))
}

labx <- "ASMR"
xa=SDI21fig(dfx = dfx,labx =labx)
print(xa)
ggsave(paste0("21+1-SDI-ASR-Deaths.pdf"),plot = xa$p,width = 12,height =8)
write.csv(dfx,paste0("21+1-SDI-ASR-Deaths.csv"))   

