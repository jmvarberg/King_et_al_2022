#Ploting and stats for Figure 5 Panel E
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

pcup <- fread("./tables/Figure_5_panel_E_tidy_data.csv")


#Plot Order: 29129 (WT), 29560 (S89A), 29636 (Nterm3A), 29638 (Nterm5A), 29562 (Cterm4A), and 29564 (9A). 
#We can try alternating pre and post values, or grouping them all together.

#Normalizing everything to Nup60-WT Pre mean and add Nup60-WT reference line, facet by time point

pcup.means <- pcup %>% 
  filter(Time.point == "Pre", Sample == "Nup60-WT") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean)

colnames(pcup.means) <- c("Norm.Nuc.NE.mean.Ratio.Nup60pre")

#normalize each data set to the Nup60-WT "pre" mean value
pcup.norm <- pcup %>% 
  mutate(Norm.Nuc.NE.mean.Ratio.Nup60pre = pcup.means$Norm.Nuc.NE.mean.Ratio.Nup60pre,
         Norm.Nuc.NE.mean.Ratio = Norm.Nuc.NE.Ratio/Norm.Nuc.NE.mean.Ratio.Nup60pre)

pcup.norm$Sample <- factor(pcup.norm$Sample, levels=c("Nup60-WT", "Nup60-S89A", "Nup60-Nterm3A", "Nup60-Nterm5A", "Nup60-Cterm4A", "Nup60-9A"), labels=c("WT", "S89A", "Nterm3A", "Nterm5A", "Cterm4A", "9A"))
pcup.norm$Time.point <- factor(pcup.norm$Time.point, levels=c("Pre","Post"))


ggplot(pcup.norm, aes(x=Sample, y=Norm.Nuc.NE.mean.Ratio, color=Sample)) +
  geom_sina(alpha=0.1, size=1, shape=16) +
  scale_color_manual(values=c("WT" = "#000000",
                              "S89A" = "#e3272d",
                              "Nterm3A" = "#ed6826",
                              "Nterm5A" = "#f7d902",
                              "Cterm4A" = "#0d9d49",
                              "9A" = "#224792")) +
  stat_summary(geom="pointrange", fun.data=mean_sd, position=position_dodge(0.95), size=0.6) +
  geom_hline(
    data = . %>%
      group_by(Time.point) %>%
      filter(Sample == "WT") %>% 
      summarise_at(c("Norm.Nuc.NE.mean.Ratio"), list(line=mean)),
    mapping = aes(yintercept = line),
    color="black", 
    linetype="dashed"
  ) +
  facet_wrap(vars(Time.point), dir="v") +
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0,1,1,2,3), limits=c(0,3)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(legend.position='none') +
  theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
  theme(strip.text = element_text(color = 'black', face='bold', size=14)) 
ggsave("./plots/Figure_5_panel_E.pdf", width=3.5, height=5, units="in", useDingbats=FALSE)

# Statistics --------------------------------------------------------------

summ_stats <- pcup.norm %>% 
  group_by(Time.point, Sample) %>% 
  get_summary_stats(Norm.Nuc.NE.mean.Ratio, type="common")
write.csv(summ_stats, "./stats/Figure_5_panel_E_summary_stats.csv")

#statistic comparisons

#check normality by shapiro test

norm_check <- pcup.norm %>% 
  group_by(Time.point, Sample) %>% 
  shapiro_test(Norm.Nuc.NE.mean.Ratio)
norm_check
write.csv(norm_check, "./stats/Figure_5_panel_E_shapiro.csv")

#data failed normality check. Do stats using non-parametric methods

krusk <- pcup.norm %>% 
  group_by(Time.point) %>% 
  kruskal_test(Norm.Nuc.NE.mean.Ratio ~ Sample)
krusk
write.csv(krusk, "./stats/Figure_5_panel_E_kruskall_test.csv")

#Dunn's post-hoc pairwise multiple comparisons
dunn <- pcup.norm %>% 
  group_by(Time.point) %>% 
  dunn_test(Norm.Nuc.NE.mean.Ratio ~ Sample)
dunn

#'The dunn_test function performs all pair-wise comparisons, and so the adjusted p.value uses all tests for this correction. 
#'Since we only want to ask if any are different than Nup60-WT, we subset those comparisons, 
#'then re-calculate adjusted p-values using the number of tests actually done.

dunncorrect <- dunn %>% 
  filter(grepl("WT", group1)) %>% 
  group_by(Time.point) %>% 
  mutate(Corr.p.adjust = p.adjust(p, method="holm"),
         Corr.p.signif = case_when(Corr.p.adjust <= 0.0001 ~ "****",
                                   Corr.p.adjust <= 0.001 ~ "***",
                                   Corr.p.adjust <= 0.01 ~ "**",
                                   Corr.p.adjust <= 0.05 ~ "*",
                                   TRUE ~ "ns"))
write.csv(dunncorrect, "./stats/Figure_5_panel_E_dunn.csv")
