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
  filter(cause_name == "Tuberculosis") %>%
  filter(location_name == "Global") %>%
  filter(age_name == "<20 years") %>%
  filter(metric_name == "Rate")


byx <- 5  
p <- ggplot(dfx, aes(x = year, y = val, group = sex_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), alpha = 0.2, show.legend = TRUE) + 
  geom_line(aes(color = sex_name, linetype = sex_name)) +
  theme_classic() +
  scale_y_continuous(labels = label_number(unit = "K")) +
  scale_x_continuous(
    breaks = seq(min(dfx$year, na.rm = TRUE), max(dfx$year, na.rm = TRUE), by = byx)
  ) + 
  labs(x = "Year", y = "ASIR per 100,000 population", color = "Sex", linetype = "Sex") +
  # 使用 ggsci 的调色板
  scale_color_lancet() +  # 应用 ggsci 的 npg 调色板到线条颜色
  scale_fill_lancet() +   # 应用 ggsci 的 npg 调色板到填充颜色
  guides(color = guide_legend(title = "Sex"), linetype = guide_legend(title = "Sex"), fill = guide_legend(title = "Sex")) +
  theme(strip.background = element_blank())

# 打印图表
print(p)

write.csv(dfx,paste0("Fig1-2021-ASIR.csv"),row.names = FALSE)
ggsave(paste0("Fig1-2021-ASIR.pdf"),plot = p,width = 8,height = 6)

