#Plots and stats for Figure 8C

library(data.table)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(knitr)
library(systemfonts)
library(kableExtra)
library(sinaplot)
library(ggforce)
library(rstatix)

#read in tidy data

tidyData <- fread("./tables/Figure_8C_tidy_data.csv")

tidyData$Group <- factor(tidyData$Group, levels = c("WT", "deltaAH"))

# Normalization to WT  ---------------------------------------

WT.mean <- tidyData %>% 
  group_by(Group) %>% 
  summarise_at(vars(NE.mean), mean) %>% 
  filter(Group == "WT")

#divide all Norm.Nuc.NE.Ratios by Cdc5mn Pre Mean Ratio value
final.df <- tidyData %>% 
  mutate(WT.Norm.NE.mean = NE.mean/WT.mean$NE.mean)

# Plotting ----------------------------------------------------------------

#bw plot
ggplot(final.df, aes(x=Group, y=WT.Norm.NE.mean)) +
  geom_sina(color="black", alpha=0.2, size=2, shape=16) +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=0.7) +
  stat_compare_means(method="wilcox") +
  xlab("")+
  ylab("Normalized Mean NE Intensity (a.u.)") +
  theme_cowplot(font_size = 18) +
  scale_y_continuous(breaks = c(0,0.5, 1.0, 1.5), limits=c(0,1.5)) +
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(legend.position='none') +
  theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
  theme(strip.text = element_text(color = 'black', face='bold', size=14)) 
ggsave("./plots/Figure_8_panel_C.pdf", width=4.5, height=3.5, units="in", useDingbats=FALSE)

#statistics - for each group, one test (paired), check for normality
shapiro <- final.df %>% 
  group_by(Group) %>% 
  shapiro_test(WT.Norm.NE.mean)
shapiro
write.csv(shapiro, "./stats/Figure_8C_shapiro_test.csv")

# Groups are non-normally distributed. will use non-parametric Wilcoxon test for comparison 
wilcox <- final.df %>% 
  wilcox_test(WT.Norm.NE.mean ~ Group, paired = FALSE)
wilcox
write.csv(wilcox, "./stats/Figure_8C_wilcox_test.csv")

#save out summary stats by Group/Timepoint
summaryStats <- final.df %>% 
  group_by(Group) %>% 
  get_summary_stats(WT.Norm.NE.mean, type="common")
write.csv(summaryStats, "./stats/Figure_8C_summary_stats.csv")
