#Plots and stats for Figure 7 panel B

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


#'From 201211, there are files corresponding to WT Nup60-GFP (14646) and Nup60-delta2-47-GFP (25731). 
#'I measured the nuclei at -45 min before anaphase I, at -10 min before anaphase I, at anaphase I, and at 10 min after anaphase I. 
#'I was thinking, for this plot, the order could be WT and then Nup60-delta2-47, one time point at a time. 
#'Stats can be done between the two strains for each time point. The values can all be normalized to WT Nup60. 
#'The goal will be to show that the Nup60-delta2-47 mutant (without its N-terminal amphipathic helix) detaches earlier and remains detached. 
#'(NOTE that the mutant is more nucleoplasmic from the getgo).

# Data Input and Cleanup --------------------------------------------------


#read in tidy dataframe
tidyData <- fread("./tables/Figure_7_panel_B_tidy_data.csv")


tidyData$Sample <- factor(tidyData$Sample, levels = c("Nup60-GFP_WT", "Nup60-GFP_delta2-47"))
tidyData$Time.point <- factor(tidyData$Time.point, levels = c("-45 min", "-10 min", "Anaphase I", "+10 min"))


# Normalization to Nup60-WT -45 min  ---------------------------------------

WT.pre.mean <- tidyData %>% 
  group_by(Sample) %>% 
  filter(Time.point == "-45 min") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean) %>% 
  filter(Sample == "Nup60-GFP_WT")

#divide all Norm.Nuc.NE.Ratios by Cdc5mn Pre Mean Ratio value
final.df <- tidyData %>% 
  mutate(WTpre.Norm.Nuc.NE.Ratio = Norm.Nuc.NE.Ratio/WT.pre.mean$Norm.Nuc.NE.Ratio)


# Plotting ----------------------------------------------------------------
#'I was thinking, for this plot, the order could be WT and then Nup60-delta2-47, one time point at a time. 
#'Stats can be done between the two strains for each time point. The values can all be normalized to WT Nup60. 
#'The goal will be to show that the Nup60-delta2-47 mutant (without its N-terminal amphipathic helix) detaches earlier and remains detached. 
#'(NOTE that the mutant is more nucleoplasmic from the getgo).

ggplot(final.df, aes(x=Time.point, y=WTpre.Norm.Nuc.NE.Ratio)) +
  geom_sina(alpha=0.2, size=2, shape=16, color="black") +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=0.8) +
  facet_wrap(vars(Sample)) +
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  theme_cowplot(font_size=18) +  
  scale_y_continuous(breaks = c(0,1,2,3), limits=c(0,3)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
  theme(strip.text = element_text(color = 'black', face='bold', size=14)) 
ggsave("./plots/Figure_7_panel_B.pdf", width=8, height=5, units="in", useDingbats=FALSE)

# Statistics --------------------------------------------------------------

summ_stats <- final.df %>% 
  group_by(Time.point, Sample) %>% 
  get_summary_stats(WTpre.Norm.Nuc.NE.Ratio, type="common")
write.csv(summ_stats, "./stats/Figure_7_panel_B_summary_stats.csv")

#check normality by Shapiro test

norm_check <- final.df %>% 
  group_by(Time.point, Sample) %>% 
  shapiro_test(WTpre.Norm.Nuc.NE.Ratio)
norm_check
write.csv(norm_check, "./stats/Figure_7_panel_B_shapiro.csv")

#data for WT +10 barely failed normality check (p < 0.05). Will do stats using non-parametric methods.

krusk <- final.df %>% 
  group_by(Sample) %>% 
  kruskal_test(WTpre.Norm.Nuc.NE.Ratio ~ Time.point)
krusk
write.csv(krusk, "./stats/Figure_7_panel_B_kruskall.csv")

#posthoc test, planned contrasts using Wilcoxon signed-rank, compare paired between time points.
wilcoxon_signed_rank <- final.df %>% 
    group_by(Sample) %>% 
    wilcox_test(formula = WTpre.Norm.Nuc.NE.Ratio ~ Time.point,
           comparisons = list(c("-45 min", "-10 min"), c("-10 min", "Anaphase I"), c("Anaphase I", "+10 min")),
           p.adjust.method = "holm", 
           paired = TRUE)
write.csv(wilcoxon_signed_rank, "./stats/Figure_7_panel_B_wilcoxon_signed_rank.csv")



