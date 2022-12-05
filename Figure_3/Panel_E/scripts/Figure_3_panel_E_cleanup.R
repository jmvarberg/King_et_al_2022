#Data clean up for Figure 3 panel G

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

#'For 210331, these are files corresponding to Nup60-GFP in three different mutant backgrounds: Cdc20mn (28211), Cdc5mn (28492), Cdc20mn Cdc5mn (28614). 
#'For these files, you'll have to screen out both ROIs designated as bad and ROIs that are small (nuclear masks <2.5 for either pre or post). 
#'I measured the nuclei at around 4 hours (prior to metaphase I arrest) and at around 8 hours (after metaphase I arrest). 
#'For this graph, I was thinking that the order could be: Cdc5mn 4 and 8, Cdc20mn 4 and 8, Cdc5mn Cdc20mn 4 and 8. 
#'Stats can be done to compare 4 and 8 for each strain. All values can be normalized to the Cdc5mn mutant at 4 hours. 
#'The effect here is likely subtle, but there may be slightly more detachment in the Cdc20mn strain but not the Cdc5mn or Cdc5mn Cdc20mn strains. 
#'Again, progression into metaphase I arrest will be heterogenous (~70% of cells).

#read in design file

design <- fread("../King_Figure_3_Panel_E_design.csv")
data <- lapply(paste0("../data/",design$File), fread)
names(data) <- design$Sample

df <- plyr::ldply(data, data.frame)

#clean up data frame

cleanup <- function(x) {
  df <- x %>% mutate(Time.point = case_when(str_detect(Image, regex("frame_1")) ~ "Pre",
                                            str_detect(Image, regex("frame_2")) ~ "Metaphase I",
                                            TRUE ~ "other"),
                     Nucleus.ID = str_remove(Image, pattern = "_substack.*"),
                     BAD = ifelse(is.na(BAD),0, BAD),
                     NE.mean = Nuclear.Envelope/(WholeNuc.Area-Nucleoplasmic.Area),
                     Nuc.mean = Nucleoplasm/Nucleoplasmic.Area,
                     Norm.Nuc.NE.Ratio = Nuc.mean/NE.mean,
                     Norm.NE.Nuc.Ratio = NE.mean/Nuc.mean) %>% 
    group_by(Nucleus.ID) %>% 
    add_tally() %>% 
    filter(!any(Nuclear.Envelope == 0 | BAD == 1 | Nuc.Area <= 2.5 | Time.point == "other" | n != length(unique(.$Time.point))))
  return(df) 
}

cleandf<- cleanup(df) %>% select(-V1) %>% dplyr::rename(Sample = .id)

cleandf$Sample <- factor(cleandf$Sample, levels = c("Cdc5mn", "Cdc20mn", "Cdc20mn.Cdc5mn"))
cleandf$Time.point <- factor(cleandf$Time.point, levels = c("Pre", "Metaphase I"))

#write out cleaned up, tidy dataframe
write.csv(cleandf, "../tables/Figure_3_panel_E_tidy_dataframe.csv")

