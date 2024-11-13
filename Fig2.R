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

age_twofig=function(dfx=dfx,labx="Number of cases",color_scheme = 2){
  #agex=c(1,5:20,30,31,32,235)
  # Define the color palettes as dataframes
  color_palettes <- data.frame(
    palette = c(1, 2, 3, 4, 5),
    color1 = c("#E25A90", "lightpink", "#FF7E67", "#FFC0CB", "#EC7063"),
    color2 = c("#58C1B2", "skyblue", "#15B8A0", "#D8BFD8", "#2471A3"),
    legend_fill2 = rep("Male(number and 95%UI)",5),
    legend_fill1 = rep("Female(number and 95%UI)",5),
    legend_color2 = rep("Male(rate and 95%UI)",5),
    legend_color1 = rep("Female(rate and 95%UI)",5)
  )
  
  dfx=dfx %>% mutate(age_name = if_else(age_name %in% c("80-84", "85-89", "90-94"), 
                                        paste(age_name, "years"), 
                                        age_name)) 
  dfage=dfage %>% mutate(age_name = if_else(age_name %in% c("80-84", "85-89", "90-94"), 
                                            paste(age_name, "years"), 
                                            age_name)) 
  library(ggplot2)
  library(dplyr)
  # Check the number of unique location_id and cause_id
  if (length(unique(dfx$age_id)) < 2 || length(unique(dfx$year)) > 1) {
    p=  ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "Oops, your data is not suitable for this plot"), size = 12) +
      theme_void()
    return(p)
  }
  
  dfid=dfx %>% select(age_id,age_name) %>% distinct(age_id,.keep_all = T) %>% 
    left_join(.,dfage) %>% arrange(id)
  dfx=left_join(dfx,dfid) %>% filter(!is.na(id))
  dfx$age_name <- factor(dfx$age_name, levels = unique(dfid$age_name[order(dfid$id)]))
  df1=dfx #%>% filter(age_id %in% agex) %>% mutate(age_name=factor(age_name,levels=x8$age_name)) 
  # Load the ggplot2 package
  # First, let's prepare the data by dividing it into two separate data frames: one for Number and one for Rate.
  df1_number <- df1 %>% filter(metric_name == "Number") #%>% left_join(.,x8)
  df1_rate <- df1 %>% filter(metric_name == "Rate") #%>% left_join(.,x8)
  
  # We will normalize the 'val' for Rate to the same scale as Number for visual purposes.
  max_number <- max(df1_number$val)
  max_rate <- max(df1_rate$val)
  scaling_factor <- max_number / max_rate
  
  # Start the plot
  x1=color_palettes %>% filter(palette==color_scheme)
  sexx=unique(dfx$sex_name)
  # Create a function to select the appropriate color palette
  if ("Male" %in% sexx && "Female" %in% sexx) {
    x1=x1
  } else if (sexx == "Male") {
    x1$legend_fill2 <- "Male(number and 95%UI)"
    x1$legend_fill1 <- "Male(number and 95%UI)"
    x1$legend_color2 <-  "Male(rate and 95%UI)"
    x1$legend_color1 <- "Male(number and 95%UI)"
  } else {
    x1$legend_fill2 <- "Female(number and 95%UI)"
    x1$legend_fill1 <- "Female(number and 95%UI)"
    x1$legend_color2 <-  "Female(rate and 95%UI)"
    x1$legend_color1 <- "Female(number and 95%UI)"
  }
  
  p=ggplot() +
    geom_bar(data = df1_number, aes(x = age_name, y = val, fill = sex_name), 
             stat = "identity", position = position_dodge(), color = "black") +
    geom_errorbar(data = df1_number, aes(x = age_name, ymin = lower, ymax = upper, group = sex_name),
                  position = position_dodge(0.9), width = 0.25) +
    geom_line(data = df1_rate, aes(x = age_name, y = val * scaling_factor, group = sex_name, color = sex_name),
              position = position_dodge(0.9)) +
    geom_ribbon(data = df1_rate, aes(x = age_name, ymin = lower * scaling_factor, ymax = upper * scaling_factor, group = sex_name, fill = sex_name), 
                alpha = 0.5, position = position_dodge(width = 0.9)) +  # Ensure ribbons are displayed for rates
    labs(x="",y = labx) +
    # Set primary and secondary y-axes
    scale_y_continuous(labels = label_number(unit = "K"),sec.axis = sec_axis(~./scaling_factor, name = "Incidence Rate per 100,000 population")) +
    #ggsci::scale_fill_aaas(name = "  ")+
    scale_fill_manual(values =  c(x1$color1,x1$color2),labels =c(x1$legend_fill1,x1$legend_fill2),name=" ") +
    scale_color_manual(values =  c(x1$color1,x1$color2),labels = c(x1$legend_color1,x1$legend_color2),name="  ") +
    theme_classic() +
    theme(legend.position = "top",
          # 调整字体大小和加粗
          axis.text.x = element_text(angle = 65, hjust = 1, size = 12, face = "bold"),  # X轴标签
          axis.text.y = element_text(size = 12, face = "bold"),  # Y轴标签
          axis.title.x = element_text(size = 14, face = "bold"),  # X轴标题
          axis.title.y = element_text(size = 14, face = "bold"),  # Y轴标题
          
          # 调整图例的字体大小和加粗
          legend.text = element_text(size = 12, face = "bold"),  # 图例文本
          legend.title = element_text(size = 14, face = "bold")  # 图例标题
    )
  return(p)
}
df <- read.csv("21+1+5SDI.csv")
head(df)

dfx <- df %>%
  filter(measure_name == "Incidence") %>%
  filter(cause_name == "Tuberculosis") %>%
  filter(location_name == "Global") %>%
  filter(sex_name!="Both") %>% 
  filter(age_id %in% c(1, 5:20, 30, 31, 32, 235)) %>%
  filter(metric_name != "Percent") %>% 
  filter(year==2021) %>% 
  
  mutate(age_name=factor(age_name,levels=c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
                                           "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years", 
                                           "50-54 years", "55-59 years","60-64 years" ,"65-69 years", "70-74 years", 
                                           "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95+ years")))

xa = age_twofig(dfx = dfx, labx = paste0("Number of cases"), color_scheme = 2)

print(xa)
ggsave(paste0("Fig2-2021-agegroup-Incidence.pdf"), width = 12, height = 8)
write.csv(dfx, paste0("Fig2-2021-agegroup-Incidence.csv"))