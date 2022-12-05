#Figure 1 panel C- Nups by subcomplex through meiosis

#load required packages
library(data.table)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(systemfonts)
library(sinaplot)
library(ggforce)
library(rstatix)

#read in tidy dataframe

tidyData <- fread("./Final_figures/Figure_1_Nups_subcomplex_Meiosis/tables/Figure_1_panel_C_tidy_dataframe.csv")

#plot objective - multiple nups, colored and organized by subcomplex, through meiosis. Each Nup normalized to its 'Pre' mean value,
#with a reference line to Pom34. Facet by timepoint

#make dataframe of mean Pre values by sample
final.df.means <- tidyData %>% 
  group_by(Sample) %>% 
  filter(Time.point == "Pre") %>% 
  summarise_at(vars(Norm.Nuc.NE.Ratio), mean)

colnames(final.df.means) <- c("Sample", "Norm.Nuc.NE.mean.Ratio.pre")

#normalizing each data set to the "pre" mean value
final.df.norm <- left_join(tidyData, final.df.means) %>% 
  mutate(Norm.Nuc.NE.mean.Ratio = Norm.Nuc.NE.Ratio/Norm.Nuc.NE.mean.Ratio.pre)

#remake plots with normalized data 

final.df.norm$Sample <- factor(final.df.norm$Sample, levels = c("Pom34-GFP", "Nup170-GFP", "Nup120-GFP", "Nup49-GFP", "Mlp1-GFP", "Nup1-GFP", "Nup2-GFP", "Nup60-GFP"))
final.df.norm$Subcomplex <- factor(final.df.norm$Subcomplex, levels = c("Basket", "Channel", "Y-complex", "Inner ring", "Transmembrane"))
final.df.norm$Time.point <- factor(final.df.norm$Time.point, levels =c("Pre", "Anaphase", "Post"))

#Norm.Nuc.NE.Ratio normalized to Pre values with Pom34 reference line, no stats
ggplot(final.df.norm, aes(x=Sample, y=Norm.Nuc.NE.mean.Ratio, color=Subcomplex)) +
  geom_hline(
    data = . %>%
      group_by(Time.point) %>%
      filter(Sample == "Pom34-GFP") %>% 
      summarise_at(c("Norm.Nuc.NE.mean.Ratio"), list(line=mean)),
    mapping = aes(yintercept = line),
    color="black", 
    linetype="dashed"
  ) +
  geom_sina(alpha=0.2, size=1, shape=16, aes(color=Subcomplex)) +
  scale_color_manual(values=c("Basket" = "#e3292f",
                              "Channel" = "#e78b25",
                              "Y-complex" = "#f8dd28",
                              "Inner ring" = "#0d9d49",
                              "Transmembrane" = "#2075b6")) +
  stat_summary(geom="pointrange", fun.data=mean_sd, position="dodge") +
  #stat_compare_means(label="p.signif", ref.group="Pom34-GFP") +
  facet_wrap(vars(Time.point)) +
  xlab("")+
  ylab("Nucleoplasm/NE Ratio (a.u.)") +
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(0,3)) +
  #ggtitle("Area Normalized Nucleoplasm/NE Ratio") +
  theme_cowplot() +
  coord_flip() +
  theme(legend.position="none")
ggsave("./Final_figures/Figure_1_Nups_subcomplex_Meiosis/plots/Figure1_panel_C_Nups_subcomplexes_meiosis.pdf", width=8, height=6, units="in", useDingbats = FALSE)


#save out summary stats

summ_stats <- final.df.norm %>% 
  group_by(Time.point, Sample) %>% 
  get_summary_stats(Norm.Nuc.NE.mean.Ratio, type="common")
write.csv(summ_stats, "./Final_figures/Figure_1_Nups_subcomplex_Meiosis/stats/Figure_1_panel_C_summary.csv")

#statistic comparisons

#check normality by shapiro test
norm_check <- final.df.norm %>% 
  group_by(Time.point, Sample) %>% 
  shapiro_test(Norm.Nuc.NE.mean.Ratio)
norm_check
write.csv(norm_check, "./Final_figures/Figure_1_Nups_subcomplex_Meiosis/stats/Figure_1_panel_C_shapiro.csv")

#some samples/time points failed normality check. Do stats using non-parametric methods

#non-parametric ANOVA 
krusk <- final.df.norm %>% 
  group_by(Time.point) %>% 
  kruskal_test(Norm.Nuc.NE.mean.Ratio ~ Sample)
krusk #null hypothesis failed, now use post-hoc test to find significant differences between groups
write.csv(krusk, "./Final_figures/Figure_1_Nups_subcomplex_Meiosis/stats/Figure_1_panel_C_kruskall.csv")

#Dunn's post-hoc pairwise multiple comparisons
dunn <- final.df.norm %>% 
  group_by(Time.point) %>% 
  dunn_test(Norm.Nuc.NE.mean.Ratio ~ Sample, p.adjust.method = "holm")

#'The dunn_test function performs all pair-wise comparisons, and so the adjusted p.value uses all tests for this correction. 
#'Since we only want to ask if any are different than Pom34-GFP, we subset those comparisons, 
#'then re-calculate adjusted p-values using the number of tests actually done.

dunncorrect <- dunn %>% filter(grepl("Pom34-GFP", group1)) %>% 
  group_by(Time.point) %>% 
  mutate(Corr.p.adjust = p.adjust(p, method="holm"),
         Corr.p.signif = case_when(Corr.p.adjust <= 0.0001 ~ "****",
                                   Corr.p.adjust <= 0.001 ~ "***",
                                   Corr.p.adjust <= 0.01 ~ "**",
                                   Corr.p.adjust <= 0.05 ~ "*",
                                   TRUE ~ "ns"))
write.csv(dunncorrect, "./Final_figures/Figure_1_Nups_subcomplex_Meiosis/stats/Figure_1_panel_C_dunn.csv")
