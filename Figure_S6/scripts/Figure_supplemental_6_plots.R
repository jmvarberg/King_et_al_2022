#Plots and stats for Figure S6G

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

tidyData <- fread("./tables/Figure_S6G_tidy_data.csv")

tidyData$Group <- factor(tidyData$Group, levels = c("pCUP-Cdc5[KD]", "pCUP-Cdc5[WT]"))
tidyData$Time.point <- factor(tidyData$Time.point, levels = c("Pre", "Post"))
tidyData$Treatment <- factor(tidyData$Treatment, levels = c("Control", "CuSO4"))

# Normalization to Cdc5[KD] Control Pre ---------------------------------------

cdc5.pre.mean <- tidyData %>% 
  group_by(Group) %>% 
  filter(Time.point == "Pre", Treatment == "Control") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean) %>% 
  filter(Group == "pCUP-Cdc5[KD]")

#divide all Norm.Nuc.NE.Ratios by Cdc5mn Pre Mean Ratio value
final.df <- tidyData %>% 
  mutate(Cdc5KDctrlpre.Norm.Nuc.NE.Ratio = Norm.Nuc.NE.Ratio/cdc5.pre.mean$Norm.Nuc.NE.Ratio,
         Sample = paste(Time.point, Treatment, sep="."))

final.df$Sample <- factor(final.df$Sample, levels = c("Pre.Control", "Post.Control", "Pre.CuSO4", "Post.CuSO4"))

# Plotting ----------------------------------------------------------------
#'I was hoping for a plot with the following order: 29073 + N/A pre and post; 29073 + CuSO4 pre and post; 29071 + N/A pre and post; 29071 + CuSO4 pre and post. 
#'For the normalization, you can normalize all to the 29073 + N/A pre time point. 
#'Stats can be done to compare all strains pre and post induction, and also to compare 29073 + CuSO4 and 29071 + CuSO4 post induction. 
#'As per usual, this graph should show that Nup60-delta2-47-GFP detaches when Cdc5 is induced.

#bw plot
# ggplot(final.df, aes(x=Sample, y=Cdc5KDctrlpre.Norm.Nuc.NE.Ratio)) +
#   geom_sina(color="black", alpha=0.2, size=2, shape=16) +
#   stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=0.7) +
#   facet_wrap(vars(Group)) +
#   xlab("")+
#   ylab("Nucleoplasm/NE Ratio (a.u.)") +
#   theme_cowplot() +
#   scale_y_continuous(breaks = c(0,1,2,3), limits=c(0,3)) +
#   #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#   theme(legend.position='none') +
#   theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
#   theme(strip.text = element_text(color = 'black', face='bold', size=14)) 
# ggsave("./plots/Figure_S6_panel_G.pdf", width=6, height=3, units="in", useDingbats=FALSE)

#colored_plot
ggplot(final.df, aes(x=Sample, y=Cdc5KDctrlpre.Norm.Nuc.NE.Ratio)) +
  geom_sina(aes(color=Treatment), alpha=0.5, size=2, shape=16) +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=0.7) +
  facet_wrap(vars(Group)) +
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0,1,2,3), limits=c(0,3)) +
  scale_color_manual(values = c("darkslategrey", "darkslateblue")) +
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(legend.position='none') +
  theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
  theme(strip.text = element_text(color = 'black', face='bold', size=14)) 
ggsave("./plots/Figure_S6_panel_G_color_by_treatment.pdf", width=6, height=3, units="in", useDingbats=FALSE)

#statistics - for each group, one test (paired), check for normality
shapiro <- final.df %>% 
  group_by(Group, Sample) %>% 
  shapiro_test(Cdc5KDctrlpre.Norm.Nuc.NE.Ratio)
shapiro
write.csv(shapiro, "./stats/Figure_S6_shapiro_test.csv")

#5/6 groups are not normally distributed. will use non-parametric paired test - Wilcoxon signed-rank test
myComps <- list(c("Pre.Control", "Post.Control"), c("Pre.CuSO4", "Post.CuSO4"))
wilcox <- final.df %>% 
  group_by(Group) %>% 
  wilcox_test(Cdc5KDctrlpre.Norm.Nuc.NE.Ratio ~ Sample, comparisons =myComps, paired=TRUE, p.adjust.method = "holm")
write.csv(wilcox, "./stats/Figure_S6_wilcox_signed_rank_paired_test.csv")

#save out summary stats by Group/Timepoint
summaryStats <- final.df %>% 
  group_by(Group, Time.point, Treatment) %>% 
  get_summary_stats(Cdc5KDctrlpre.Norm.Nuc.NE.Ratio, type="common")
write.csv(summaryStats, "./stats/Figure_S6_summary_stats.csv")


#question: within group and treatments, does DI ratio change between timepoints?

#statistics - for each group, one test (paired), check for normality
shapiro <- final.df %>% 
    group_by(Group, Treatment, Time.point) %>% 
    shapiro_test(Cdc5KDctrlpre.Norm.Nuc.NE.Ratio)
shapiro
write.csv(shapiro, "./stats/Figure_S6_shapiro_test.csv")

#5/8 groups are not normally distributed. will use non-parametric paired test - Wilcoxon signed-rank test
wilcox <- final.df %>% 
    group_by(Group, Treatment) %>% 
    wilcox_test(Cdc5KDctrlpre.Norm.Nuc.NE.Ratio ~ Time.point, paired=TRUE, p.adjust.method = "holm")
write.csv(wilcox, "./stats/Figure_S6_wilcox_signed_rank_paired_test.csv")

