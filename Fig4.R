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


file_paths <- paste0("IHME-GBD_2021_DATA-7061c5eb-", 1:7, ".zip")
data_list <- list()
for (file_path in file_paths) {
  csv_file <- unzip(file_path, list = TRUE)$Name[1]
  data <- read.csv(unz(file_path, csv_file))
  data_list[[file_path]] <- data
}

merged_data <- do.call(rbind, data_list)

head(merged_data)


write.csv(merged_data, "204.csv", row.names = FALSE)






mapfigure1=function(GBDdf27=x,titlex="Pre",color_scheme=1){
  #load("Map204.Rdata")
  ################################################################################################
  ### 1. Print table Reginal level
  ################################################################################################
  df= GBDdf27  %>% #filter(location_id %in% namex$location_id ) %>% 
    select(location_id,val) %>% left_join(.,namex)
  color1 <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
              "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")
  # Define the base colors for each gradient
  base_colors2 <- c("#457B9D", "#800080")
  base_colors3 <- c("#d6604d", "#92c5de")
  base_colors4 <- c("#fddbc7", "#4393c3")
  base_colors5 <- c("#d1e5f0", "#FA8072")
  
  # Create color gradients using colorRampPalette
  color2 <- colorRampPalette(base_colors2)(10)
  color3 <- colorRampPalette(base_colors3)(10)
  color4 <- colorRampPalette(base_colors4)(10)
  color5 <- colorRampPalette(base_colors5)(10)
  colorx=list(color1,color2,color3,color4,color5)
  colorx1=colorx[as.numeric(color_scheme)]
  ################################################################################################
  ########################
  # 1.1 Incidence
  ########################
  # Incidence
  index="Incidence"
  ASR=df %>%  select(location_id,location,val) %>% 
    mutate(val=val/1)
  df_asr=left_join(df_world,ASR)
  
  
  xmin=min(na.omit(df_asr$val))
  # Calculate the quantile breaks
  breaks <- quantile(df_asr$val, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
  if(xmin==0){
    breaks=breaks[-1]
  } else{
    breaks=breaks
  }
  
  print(breaks)
  breaks100=1*breaks
  # Initialize an empty vector to store the formatted breaks
  formatted_breaks = vector("character", length(breaks100))
  # Set the first break manually to start from 0
  formatted_breaks[1] = paste(paste0(round(xmin,3)," -"), format(breaks100[1], digits = 4, nsmall = 1))
  # Iterate over the breaks and format them
  for (i in 2:length(breaks100)) {
    formatted_breaks[i] = paste(paste0(format(breaks100[i - 1], digits = 4, nsmall = 1)), "-",paste0( format(breaks100[i], digits = 4, nsmall = 1)))
  }
  # Print the formatted breaks
  #print(formatted_breaks)
  # Print the formatted breaks
  unb=unique(breaks)
  if(unb[1]==xmin){
    unb=unb[-1]
  } else{
    unb=unb
  }
  xmin=xmin +0.001
  print(formatted_breaks)
  x=cut(df_asr$val, breaks = c(xmin, unb))
  print(na.omit(unique(x)))
  xlen=length(na.omit(unique(x)))
  xstar=length(formatted_breaks)-xlen+1
  formatted_breaks=formatted_breaks[xstar:length(formatted_breaks)]
  # Cut break for DALYs
  
  breakxxx <- quantile(df_asr$val, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
  values <- c(unique(breaks))
  strings <- sapply(1:(length(values)-1), function(i) paste(values[i], "-", values[i+1], sep=" "))
  
  # Try executing the first code block
  df_asr1 <- tryCatch({
    df_asr %>%
      mutate(asr = replace_na(val, -99)) %>%
      mutate(asr_cut = cut(asr, breaks = c(xmin, unb),
                           labels = formatted_breaks)) %>%
      filter(!is.na(asr_cut))
  }, error = function(e) {
    # If the first code block fails, execute the second code block
    tryCatch({
      df_asr %>%
        mutate(asr = replace_na(val, -99)) %>%
        mutate(asr_cut = cut(asr, breaks = c(unique(breakxxx)),
                             labels = strings)) %>%
        filter(!is.na(asr_cut))
    }, error = function(e) {
      # If both code blocks fail, return a NULL value
      message("Both attempts failed.")
      NULL
    })
  })
  
  
  ggplot(df_asr1 %>% na.omit()) +
    geom_sf(aes(geometry = geometry, fill = asr_cut),size = 0.1)+
    #labs(title = paste0("Age-standardized ",index," Rate (Per 100,000),both sexes in 2017")) +
    scale_fill_manual(#name="Anaemia prevalence",
      values = rev(colorx1[[1]]),
      guide = guide_legend(reverse=T))+
    guides(fill = guide_legend(ncol = 2, title = titlex))->p
  
  p1=p+ theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              legend.position = c(0.13, 0.29),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.title=element_text(size=8),
              legend.text=element_text(size=8),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
  return(p1)
}

df <- fread("204.csv")
head(df)
dfx <- df %>%
  filter(measure_name == "Incidence") %>%
  filter(cause_name == "Tuberculosis") %>%
  filter(sex_name=="Both") %>% 
  filter(age_name=="<20 years") %>%
  filter(metric_name == "Number") %>% 
  filter(year == 2021)  


titlex = "Incidence"
xa=mapfigure1(GBDdf27 = dfx,titlex =titlex,color_scheme = 1)
print(xa)

ggsave(paste0("Fig4-2021map-ASR-Incidence.pdf"),plot = xa,width = 16,height =8)
write.csv(dfx,paste0("Fig4-2021map-ASR-Incidence.csv"))
