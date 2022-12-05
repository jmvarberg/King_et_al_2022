#Ploting and stats for Figure 3 Panel E

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

#read in tidy dataframe

tidyData <- fread("../tables/Figure_3_panel_E_tidy_dataframe.csv")



# Normalization to Cdc5mn Pre (4hr) ---------------------------------------

cdc5.pre.mean <- tidyData %>% 
  group_by(Sample) %>% 
  filter(Time.point == "Pre") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean) %>% 
  filter(Sample == "Cdc5mn")

#divide all Norm.Nuc.NE.Ratios by Cdc5mn Pre Mean Ratio value
final.df <- tidyData %>% 
  mutate(Cdc5pre.Norm.Nuc.NE.Ratio = Norm.Nuc.NE.Ratio/cdc5.pre.mean$Norm.Nuc.NE.Ratio)


# Plotting ----------------------------------------------------------------
#'For this graph, I was thinking that the order could be: Cdc5mn 4 and 8, Cdc20mn 4 and 8, Cdc5mn Cdc20mn 4 and 8. 
#'Stats can be done to compare 4 and 8 for each strain. All values can be normalized to the Cdc5mn mutant at 4 hours. 
#'The effect here is likely subtle, but there may be slightly more detachment in the Cdc20mn strain but not the Cdc5mn or Cdc5mn Cdc20mn strains. 
#'Again, progression into metaphase I arrest will be heterogenous (~70% of cells).

final.df$Sample <- factor(final.df$Sample, levels = c("Cdc5mn", "Cdc20mn", "Cdc20mn.Cdc5mn"))
final.df$Time.point <- factor(final.df$Time.point, levels = c("Pre", "Metaphase I")) 

my_comparisons = list(c("Pre", "Metaphase I"))
ggplot(final.df, aes(x=Time.point, y=Cdc5pre.Norm.Nuc.NE.Ratio)) +
  geom_sina(alpha=0.2, size=2, shape=16, color="black") +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=1) +
  #stat_compare_means(comparisons = my_comparisons, data = final.df, method="wilcox.test", label = "p.signif") +
  scale_y_continuous(breaks= c(0,0.5,1.0, 1.5, 2.0), limits=c(0,2.0)) +
  facet_wrap(vars(Sample)) +
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  theme_cowplot()
ggsave("../plots/Figure_3_panel_E.pdf", width=8, height=5, units="in", useDingbats=FALSE)


# Statistics --------------------------------------------------------------

#save out summary stats

summ_stats <- final.df %>% 
  group_by(Time.point, Sample) %>% 
  get_summary_stats(Cdc5pre.Norm.Nuc.NE.Ratio, type="common")
write.csv(summ_stats, "../stats/Figure_3_panel_E_summary.csv")

#statistic comparisons

#check normality by shapiro test

norm_check <- final.df %>% 
  group_by(Time.point, Sample) %>% 
  shapiro_test(Cdc5pre.Norm.Nuc.NE.Ratio)
norm_check
write.csv(norm_check, "../stats/Figure_3_panel_E_shapiro.csv")

#data failed normality check. Do stats using non-parametric methods

#non-parametric Wilcoxon sign rank test (paired samples)
wilcox_sign_rank <- final.df %>% 
    group_by(Sample) %>% 
    wilcox_test(Cdc5pre.Norm.Nuc.NE.Ratio ~ Time.point, paired=TRUE)
wilcox_sign_rank #null hypothesis failed only for the WT samples, now use post-hoc test to find significant differences between groups
write.csv(wilcox_sign_rank, "../stats/Figure_3_panel_E_wilcoxon_sign_rank.csv")

