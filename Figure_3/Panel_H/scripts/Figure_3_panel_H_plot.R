#Plots and stats for Figure 3 Panel H - Nup60-GFP Nuc/NE ratios with Cdc5 KD or WT with or without CuSO4

#load packages
library(data.table)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(systemfonts)
library(sinaplot)
library(ggforce)
library(rstatix)

#read in tidy dataframe

tidyData <- fread("./tables/Figure_3_panel_H_tidy_dataframe.csv")

#'Plot objective: Normalize to pCUP-Cdc5KD control sample mean. Facet plot by Cdc5 group (KD or WT), compare Pre vs. Post for control and CuSO4 treatments. 

#make grouping variable combining timepoint and treatment
final.df <- tidyData %>% 
    mutate(Group = paste(Time.point, Treatment, sep="."))

final.df$Group = factor(final.df$Group, levels = c("Pre.Control", "Post.Control", "Pre.CuSO4", "Post.CuSO4"))

#Calculate mean value for "Pre" timepoint of control treated pCUP-Cdc5[KD] sample
final.df.means <- final.df %>% 
    group_by(Sample, Treatment) %>% 
    filter(Time.point == "Pre") %>% 
    summarise_at(vars(Norm.Nuc.NE.Ratio), mean) %>% 
    filter(Sample == "pCUP-Cdc5[KD]", Treatment == "Control")

colnames(final.df.means) <- c("Sample", "Treatment", "Norm.Nuc.NE.mean.Ratio.pre")

#try normalizing each data set to the "pre" mean value
final.df.norm <- final.df %>% 
  mutate(Norm.Nuc.NE.mean.Ratio = Norm.Nuc.NE.Ratio/final.df.means$Norm.Nuc.NE.mean.Ratio.pre)

#remake plots with normalized data 
final.df.norm$Group = factor(final.df$Group, levels = c("Pre.Control", "Post.Control", "Pre.CuSO4", "Post.CuSO4"))

ggplot(final.df.norm, aes(x=Group, y=Norm.Nuc.NE.mean.Ratio)) +
    geom_sina(aes(color=Treatment), alpha=0.5, size=2, shape=16) +
    stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge") +
    scale_y_continuous(breaks=c(0,0.5,1.0,1.5,2.0, 2.5, 3.0), limits=c(0,3)) +
    facet_wrap(vars(Sample)) +
    xlab("") +
    ylab("Nucleoplasm/NE Ratio (a.u.)") +
    scale_color_manual(values = c("darkslategrey", "darkslateblue")) +
    theme_cowplot(font_size=10) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #uncomment if want vertical to rotate tick labels
ggsave("./plots/Figure_3_panel_H_Nup60_cdc5.pdf", width=5, height=4, units="in", useDingbats=FALSE)

#save out summary stats

summ_stats <- final.df.norm %>% 
    group_by(Group, Sample) %>% 
    get_summary_stats(Norm.Nuc.NE.mean.Ratio, type="common")
write.csv(summ_stats, "./stats/Figure_3_panel_H_summary.csv")

#statistic comparisons - within Cdc5 and treatment groups, does the Nuc/NE change between timepoints?

#check normality by shapiro test
norm_check <- final.df.norm %>% 
    group_by(Group, Sample) %>% 
    shapiro_test(Norm.Nuc.NE.mean.Ratio)
norm_check
write.csv(norm_check, "./stats/Figure_3_panel_H_shapiro.csv")

#data failed normality check. Do stats using non-parametric methods

#wilcoxon signed rank tests (paired non-parametric t-test) within Sample and Treatment groups.
wilcox <- final.df.norm %>% 
    group_by(Sample, Treatment) %>% 
    wilcox_test(Norm.Nuc.NE.mean.Ratio ~ Time.point, paired=TRUE)
wilcox
write.csv(wilcox, "./stats/Figure_3_panel_H_wilcox_signed_rank.csv")
