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

dfxs1 = df %>%
  filter(measure_name =="DALYs") %>%
  filter(location_name %in% loc215) %>%
  filter(sex_name == "Both") %>%
  filter(age_name %in% c("Age-standardized","All ages")) %>% 
  filter(!metric_name == "Percent") %>%
  mutate(a=paste0(age_name,"-", metric_name)) %>% 
  filter(!a=="All ages-Rate") %>% 
  select(-a) %>% 
  filter(year %in% c(1990, 2021)) %>%
  mutate(location_name = factor(location_name, levels = c("Global", "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI",
                                                          "Andean Latin America", "Australasia", "Caribbean", "Central Asia", "Central Europe", 
                                                          "Central Latin America", "Central Sub-Saharan Africa", "East Asia", "Eastern Europe", 
                                                          "Eastern Sub-Saharan Africa", "High-income Asia Pacific", "High-income North America", 
                                                          "North Africa and Middle East", "Oceania", "South Asia", "Southeast Asia", 
                                                          "Southern Latin America", "Southern Sub-Saharan Africa", "Tropical Latin America", 
                                                          "Western Europe", "Western Sub-Saharan Africa"))) %>%
  arrange(location_name) %>% 
  
  # 根据 metric_name 处理数据格式，Number 保留整数，Rate 保留两位小数
  mutate(num = if_else(metric_name == "Number", 
                       sprintf("%.0f (%.0f, %.0f)", val, lower, upper), 
                       sprintf("%.2f (%.2f, %.2f)", val, lower, upper))) %>%
  
  mutate(yearmeasure_name = paste0(year, measure_name, "-", metric_name)) %>%
  select(location_id,location_name, yearmeasure_name, cause_name, age_name, num)

dfntd21 = dfxs1 %>%
  dplyr::select(-"age_name") %>%
  pivot_wider(
    names_from = yearmeasure_name,
    values_from = num
  ) %>%
  select("location_id","location_name", "cause_name",
         "1990DALYs-Number", "1990DALYs-Rate", "2021DALYs-Number", "2021DALYs-Rate" )
write.csv(dfntd21,paste0("./TableS2/TableS2-DALYs.csv"))

for (i in unique(df$measure_name)) {
  
  
  dfx <- df %>%
    filter(measure_name==i) %>%
    filter(cause_name=="Tuberculosis") %>% 
    filter(location_name=="Global") %>% 
    filter(age_name=="<20 years") %>% 
    filter(year %in%c(1990,2021)) %>% 
    filter(!metric_name=="Percent") %>%
    mutate(x = paste0(year,"-", sex_name,"-",metric_name)) %>% 
    mutate(num = if_else(metric_name == "Number", 
                         sprintf("%.2f (%.2f, %.2f)", val, lower, upper), 
                         sprintf("%.2f (%.2f, %.2f)", val, lower, upper))) %>% 
    select(2,4,6,8,10,12,14:18)
  
  write.csv(dfx,paste0("global-sex-",i,".csv"))
}


for (i in unique(df$measure_name)) {
  
  
  dfx <- df %>%
    filter(measure_name==i) %>%
    filter(cause_name=="Tuberculosis") %>% 
    filter(location_name=="Global") %>% 
    filter(sex_name=="Both") %>% 
    filter(year %in%c(1990,2021)) %>% 
    filter(!metric_name=="Percent") %>%
    mutate(x = paste0(year,"-", age_name,"-",metric_name)) %>% 
    mutate(num = if_else(metric_name == "Number", 
                         sprintf("%.2f (%.2f, %.2f)", val, lower, upper), 
                         sprintf("%.2f (%.2f, %.2f)", val, lower, upper))) %>% 
    select(2,4,6,8,10,12,14:18)
  
  write.csv(dfx,paste0("global-age_name-",i,".csv"))
}



for (i in unique(df$measure_name)) {
  
  
  dfx <- df %>%
    filter(measure_name==i) %>%
    filter(cause_name=="Tuberculosis") %>% 
    filter(location_id %in% name_5) %>% 
    filter(sex_name=="Both") %>% 
    filter(age_name=="<20 years") %>% 
    filter(year %in%c(1990,2021)) %>% 
    filter(!metric_name=="Percent") %>%
    mutate(x = paste0(year,"-", location_name,"-",metric_name)) %>% 
    mutate(num = if_else(metric_name == "Number", 
                         sprintf("%.2f (%.2f, %.2f)", val, lower, upper), 
                         sprintf("%.2f (%.2f, %.2f)", val, lower, upper))) %>% 
    select(2,4,6,8,10,12,14:18)
  
  write.csv(dfx,paste0("global-5SDI-",i,".csv"))
}







library(dplyr)

# 创建一个空的数据框，用于存储所有结果
final_results <- data.frame()

# 遍历不同的 measure_name 和 sex_name
for (i in unique(df$measure_name)) {
  for (j in unique(df$sex_name)) {
    
    # 过滤数据并清理
    df2 <- df %>%
      filter(measure_name == i) %>%
      filter(sex_name == j) %>%
      filter(location_name == "Global") %>% 
      filter(age_name == "<20 years") %>%
      filter(metric_name != "Percent") %>%
      filter(!is.na(val), !is.infinite(val), val > 0)
    
    if (nrow(df2) > 1) {
      lm_fit <- lm(log(val) ~ year, data = df2)
      beta <- coef(lm_fit)["year"]
      se_beta <- summary(lm_fit)$coefficients["year", "Std. Error"]
      EAPC <- (exp(beta) - 1) * 100
      lower_CI <- (exp(beta - 1.96 * se_beta) - 1) * 100
      upper_CI <- (exp(beta + 1.96 * se_beta) - 1) * 100
      eapc_formatted <- sprintf("%.2f (%.2f, %.2f)", EAPC, lower_CI, upper_CI)
      final_results <- rbind(final_results, data.frame(
        Measure = i,
        Sex = j,
        EAPC = eapc_formatted
      ))
    }
  }
}

# 保存结果为 CSV 文件
write.csv(final_results, file = "EAPC-sex_results.csv", row.names = FALSE)

# 输出结果到控制台
print(final_results)










library(dplyr)

# 创建一个空的数据框，用于存储所有结果
final_results <- data.frame()

# 遍历不同的 measure_name 和 sex_name
for (i in unique(df$measure_name)) {
  for (j in unique(df$age_name)) {
    
    # 过滤数据并清理
    df2 <- df %>%
      filter(measure_name == i) %>%
      filter(age_name == j) %>%
      filter(location_name == "Global") %>% 
      filter(sex_name == "Both") %>%
      filter(metric_name != "Percent") %>%
      filter(!is.na(val), !is.infinite(val), val > 0)
    
    if (nrow(df2) > 1) {
      lm_fit <- lm(log(val) ~ year, data = df2)
      beta <- coef(lm_fit)["year"]
      se_beta <- summary(lm_fit)$coefficients["year", "Std. Error"]
      EAPC <- (exp(beta) - 1) * 100
      lower_CI <- (exp(beta - 1.96 * se_beta) - 1) * 100
      upper_CI <- (exp(beta + 1.96 * se_beta) - 1) * 100
      eapc_formatted <- sprintf("%.2f (%.2f, %.2f)", EAPC, lower_CI, upper_CI)
      final_results <- rbind(final_results, data.frame(
        Measure = i,
        Agegroup = j,
        EAPC = eapc_formatted
      ))
    }
  }
}

# 保存结果为 CSV 文件
write.csv(final_results, file = "EAPC-agegroup_results.csv", row.names = FALSE)

# 输出结果到控制台
print(final_results)




library(dplyr)

# 创建一个空的数据框，用于存储所有结果
final_results <- data.frame()

# 遍历不同的 measure_name 和 sex_name
for (i in unique(df$measure_name)) {
  for (j in unique(df$location_name)) {
    
    # 过滤数据并清理
    df2 <- df %>%filter(location_id %in% name_5) %>% 
      filter(measure_name == i) %>%
      filter(location_name == j) %>%
      filter(age_name == "<20 years") %>% 
      filter(sex_name == "Both") %>%
      filter(metric_name != "Percent") %>%
      filter(!is.na(val), !is.infinite(val), val > 0)
    
    if (nrow(df2) > 1) {
      lm_fit <- lm(log(val) ~ year, data = df2)
      beta <- coef(lm_fit)["year"]
      se_beta <- summary(lm_fit)$coefficients["year", "Std. Error"]
      EAPC <- (exp(beta) - 1) * 100
      lower_CI <- (exp(beta - 1.96 * se_beta) - 1) * 100
      upper_CI <- (exp(beta + 1.96 * se_beta) - 1) * 100
      eapc_formatted <- sprintf("%.2f (%.2f, %.2f)", EAPC, lower_CI, upper_CI)
      final_results <- rbind(final_results, data.frame(
        Measure = i,
        SDI = j,
        EAPC = eapc_formatted
      ))
    }
  }
}

# 保存结果为 CSV 文件
write.csv(final_results, file = "EAPC-SDI_results.csv", row.names = FALSE)

# 输出结果到控制台
print(final_results)






df <- read.csv("pc.csv")
head(df)

dfx <- df %>%
  #filter(measure_name %in% c("Deaths","Incidence")) %>%
  filter(cause_name=="Tuberculosis") %>% 
  filter(location_name=="Global") %>% 
  filter(age_name=="<20 years") %>% 
  filter(!metric_name=="Percent") %>%
  mutate(PC = sprintf("%.2f%%", val*100)) %>% 
  select(2,4,6,8,10,12,14:18)

write.csv(dfx,paste0("PC-sex.csv"))


dfx <- df %>%
  #filter(measure_name %in% c("Deaths","Incidence")) %>%
  filter(cause_name=="Tuberculosis") %>% 
  filter(location_name=="Global") %>% 
  filter(sex_name=="Both") %>% 
  filter(!metric_name=="Percent") %>%
  mutate(PC = sprintf("%.2f%%", val*100)) %>% 
  select(2,4,6,8,10,12,14,18)

write.csv(dfx,paste0("PC-agegroup.csv"))


dfx <- df %>%
  #filter(measure_name %in% c("Deaths","Incidence")) %>%
  filter(cause_name=="Tuberculosis") %>%
  filter(sex_name=="Both") %>% 
  filter(location_id %in% name_5) %>% 
  filter(age_name=="<20 years") %>% 
  filter(!metric_name=="Percent") %>%
  mutate(PC = sprintf("%.2f%%", val*100)) %>% 
  select(2,4,6,8,10,12,14,18)

write.csv(dfx,paste0("PC-5SDI.csv"))

