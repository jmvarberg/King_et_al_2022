#Ploting and stats for Figure 6 Panel C
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

tidyData <- fread("./tables/Figure_6_panel_C_tidy_data.csv")


#Plot the area-normalized Nuc/NE ratios, with the following order: 14646 (WT), 29265 (S89A), 29441 (Nterm3A), 29443 (Nterm5A), 29267 (Cterm4A), and 29358 (9A). 
#We can probably group the cells by pre-anaphase I, mid-anaphase I, and post-anaphase I. 
#We can normalize all the values to 14646 pre-anaphase I.

#want to normalize everything to mean value for Nup60-WT Pre timepoint

nup60wt.pre.mean <- tidyData %>% 
  filter(Time.point == "Pre", Sample == "Nup60-WT") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean)

meiosis_timelapse.norm <- tidyData %>% 
  mutate(Norm.Nuc.NE.mean.Ratio = Norm.Nuc.NE.Ratio/nup60wt.pre.mean$Norm.Nuc.NE.Ratio)

meiosis_timelapse.norm$Sample <- factor(meiosis_timelapse.norm$Sample, levels=c("Nup60-WT", "Nup60-S89A", "Nup60-Nterm3A", "Nup60-Nterm5A", "Nup60-Cterm4A", "Nup60-9A"), labels=c("WT", "S89A", "Nterm3A", "Nterm5A", "Cterm4A", "9A"))
meiosis_timelapse.norm$Time.point <- factor(meiosis_timelapse.norm$Time.point, levels=c("Pre", "Anaphase","Post"))


ggplot(meiosis_timelapse.norm, aes(x=Sample, y=Norm.Nuc.NE.mean.Ratio, color=Sample)) +
  geom_sina(alpha=0.15, size=1, shape=16) +
  scale_color_manual(values=c("WT" = "#000000",
                              "S89A" = "#e3272d",
                              "Nterm3A" = "#ed6826",
                              "Nterm5A" = "#f7d902",
                              "Cterm4A" = "#0d9d49",
                              "9A" = "#224792")) +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge", size=0.3) +
  scale_y_continuous(breaks = c(0,1,2,3), limits=c(0,3)) +
  geom_hline(
    data = . %>%
      group_by(Time.point) %>%
      filter(Sample == "WT") %>% 
      summarise_at(c("Norm.Nuc.NE.mean.Ratio"), list(line=mean)),
    mapping = aes(yintercept = line),
    color="black", 
    linetype="dashed"
  ) +
  facet_wrap(vars(Time.point), dir="v", strip.position = "left") + #option based on convo with Grant 2/19/22
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(legend.position='none') +
  theme(strip.background =element_rect(fill=alpha("gray", 0.5))) +
  theme(strip.text = element_text(color = 'black', face='bold', size=14)) +
  theme(strip.placement = "outside") #to move strips outside of y axis
ggsave("./plots/Figure_6_panel_C.pdf", width=3.5, height=5, units="in", useDingbats=FALSE)

# Statistics --------------------------------------------------------------


summ_stats <- meiosis_timelapse.norm %>% 
  group_by(Time.point, Sample) %>% 
  get_summary_stats(Norm.Nuc.NE.mean.Ratio, type="common")
write.csv(summ_stats, "./stats/Figure_6_panel_C_summary_stats.csv")

#statistic comparisons

#check normality by shapiro test

norm_check <- meiosis_timelapse.norm %>% 
  group_by(Time.point, Sample) %>% 
  shapiro_test(Norm.Nuc.NE.mean.Ratio)
norm_check
write.csv(norm_check, "./stats/Figure_6_panel_C_shapiro.csv")

#data failed normality check. Do stats using non-parametric methods

krusk <- meiosis_timelapse.norm %>% 
  group_by(Time.point) %>% 
  kruskal_test(Norm.Nuc.NE.mean.Ratio ~ Sample)
krusk
write.csv(krusk, "./stats/Figure_6_panel_C_kruskall_test.csv")

#Dunn's post-hoc pairwise multiple comparisons
dunn <- meiosis_timelapse.norm %>% 
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
write.csv(dunncorrect, "./stats/Figure_6_panel_C_dunn.csv") 

