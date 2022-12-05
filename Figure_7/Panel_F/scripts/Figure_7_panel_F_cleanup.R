#Data clean up for Figure 7 panel F

library(data.table)
library(tidyverse)

#'From 210401, there are files corresponding to Nup60-delta2-47-GFP in three different mutant backgrounds: Cdc20mn (28213), Cdc5mn (28494), and Cdc5mn Cdc20mn (28616).
#'For these files, you'll have to screen out both ROIs designated as bad and ROIs that are small (nuclear masks <2.5 for either pre or post). 
#'I measured the nuclei at around 4 hours (prior to metaphase I arrest) and at around 8 hours (after metaphase I arrest). 
#'For this graph, I was thinking that the order could be: Cdc5mn 4 and 8, Cdc20mn 4 and 8, Cdc5mn Cdc20mn 4 and 8. 
#'Stats can be done to compare 4 and 8 for each strain. All values can be normalized to the Cdc5mn mutant at 4 hours. 
#'The goal will be to show that Nup60-delta2-47-GFP detaches in the Cdc20mn (when Cdc5 is expressed), but not in the Cdc5mn or Cdc5mn Cdc20mn. 
#'NOTE, since I have no way of confirming progression into metaphase, I think that ~70% of cells in the Cdc20mn show the nucleoplasmic relocalization and ~30% of the cells do not.


# Data Input and Cleanup --------------------------------------------------


#read in design file

design <- fread("./Final_figures/Figure_7_Nup60_delta2-47/Panel_F/King_Figure_7_panel_F_design.csv")
data <- lapply(paste0("./Final_figures/Figure_7_Nup60_delta2-47/Panel_F/data/",design$File), fread)
names(data) <- design$File

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
    filter(!any(Nuclear.Envelope == 0 | BAD == 1 | Nuc.Area <= 2.5 | Time.point=="other"))
  return(df) 
}

cleandf<- cleanup(df) %>% select(-V1) %>% dplyr::rename(Data.Set = .id)

#make and join sample and data set IDs
sampledf <- data.frame(design$File, design$Sample)
colnames(sampledf) <- c("Data.Set", "Sample")

final.df <- left_join(cleandf, sampledf)
#write out tidy data
write.csv(final.df, "./Final_figures/Figure_7_Nup60_delta2-47/Panel_F/tables/Figure_7_panel_F_tidydata.csv")
