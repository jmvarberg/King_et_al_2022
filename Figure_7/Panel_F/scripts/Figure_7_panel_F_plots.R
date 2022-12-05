#plots and stats for Figure 7 panel F

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

tidyData <- fread("./tables/Figure_7_panel_F_tidydata.csv")

# Normalization to Cdc5mn Pre (4hr) ---------------------------------------

cdc5.pre.mean <- tidyData %>% 
  group_by(Sample) %>% 
  filter(Time.point == "Pre") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean) %>% 
  filter(Sample == "Cdc5mn")

#divide all Norm.Nuc.NE.Ratios by Cdc5mn Pre Mean Ratio value
final.df <- tidyData %>% 
  mutate(Cdc5pre.Norm.Nuc.NE.Ratio = Norm.Nuc.NE.Ratio/cdc5.pre.mean$Norm.Nuc.NE.Ratio,
         Sample = factor(Sample, levels = c("Cdc5mn", "Cdc20mn", "Cdc20mn_Cdc5mn")),
         Time.point = factor(Time.point, levels=c("Pre", "Metaphase I")))


# Plotting ----------------------------------------------------------------
#'For this graph, I was thinking that the order could be: Cdc5mn 4 and 8, Cdc20mn 4 and 8, Cdc5mn Cdc20mn 4 and 8. 
#'Stats can be done to compare 4 and 8 for each strain. All values can be normalized to the Cdc5mn mutant at 4 hours. 
#'The goal will be to show that Nup60-delta2-47-GFP detaches in the Cdc20mn (when Cdc5 is expressed), but not in the Cdc5mn or Cdc5mn Cdc20mn. 

ggplot(final.df, aes(x=Time.point, y=Cdc5pre.Norm.Nuc.NE.Ratio)) +
  geom_sina(alpha=0.2, size=2, shape=16, color="black") +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=1) +
  facet_wrap(vars(Sample)) +
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0,0.5,1,1.5,2.0), limits=c(0,2)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
  theme(strip.text = element_text(color = 'black', face='bold', size=14)) 
ggsave("./plots/Figure_7_panel_F.pdf", width=7, height=5, units="in", useDingbats=FALSE)

# Statistics --------------------------------------------------------------

summ_stats <- final.df %>% 
  group_by(Time.point, Sample) %>% 
  get_summary_stats(Cdc5pre.Norm.Nuc.NE.Ratio, type="common")
write.csv(summ_stats, "./stats/Figure_7_panel_F_summary_stats.csv")

#check normality by Shapiro test

norm_check <- final.df %>% 
  group_by(Time.point, Sample) %>% 
  shapiro_test(Cdc5pre.Norm.Nuc.NE.Ratio)
norm_check
write.csv(norm_check, "./stats/Figure_7_panel_F_shapiro.csv")

#data for failed normality check (p < 0.05). Will do stats using non-parametric methods.

#non-parametric Wilcoxon signed rank (non-parametric paired t-test equivalent)
wilcox_signed_rank <- final.df %>% 
    group_by(Sample) %>% 
    wilcox_test(Cdc5pre.Norm.Nuc.NE.Ratio ~ Time.point, paired=TRUE)
wilcox_signed_rank
write.csv(wilcox_signed_rank, "./stats/Figure_7_panel_F_wilcoxon_signed_rank.csv")

