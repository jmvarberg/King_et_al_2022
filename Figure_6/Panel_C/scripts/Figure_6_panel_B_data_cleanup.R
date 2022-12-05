#Data clean up for Figure 6 panel C

library(data.table)
library(tidyverse)

#read in Figure 6 design

fig6.design <- fread("./King_figure_6_design.csv")

#read in data
files <- fig6.design$File
samples <- fig6.design$Sample
exp <- fig6.design$Experiment

import <- function(x) {
  path <- paste0("./data/", x)
  df <- fread(path)
  return(df)
}

data <- lapply(files, import)
names(data) <- files

#data cleanup
cleanup <- function(x) {
  df <- x %>% mutate(Time.point = case_when(str_detect(Image, regex("frame_1")) ~ "Pre",
                                                                str_detect(Image, regex("frame_2")) ~ "Anaphase",
                                                                str_detect(Image, regex("frame_3")) ~ "Post",
                                                                TRUE ~ "other"),
                     Nucleus.ID = str_remove(Image, pattern = "_substack.*"),
                     BAD = ifelse(is.na(BAD),0, BAD),
                     NE.mean = Nuclear.Envelope/(WholeNuc.Area-Nucleoplasmic.Area),
                     Nuc.mean = Nucleoplasm/Nucleoplasmic.Area,
                     Norm.Nuc.NE.Ratio = Nuc.mean/NE.mean,
                     Norm.NE.Nuc.Ratio = NE.mean/Nuc.mean) %>% 
    group_by(Nucleus.ID) %>% 
    add_tally() %>% 
    filter(!any(Nuclear.Envelope == 0 | BAD == 1 | Time.point == "other" | n != length(unique(.$Time.point)))) #remove any nuclei where NE is 0 from manually skipping in ImageJ, annotated as Bad, or not full set of images anlyzed for each timepoint. If any conditions met, then entire nucleus set is removed.
  return(df) 
}

cleandf<- lapply(data, cleanup)

combdf <- plyr::ldply(cleandf, data.frame)
combdf <- combdf %>% dplyr::rename(Data.Set = .id) %>% select(-V1)

sampledf <- data.frame(files, samples, exp)
colnames(sampledf) <- c("Data.Set", "Sample", "Experiment")

final.df <- left_join(combdf, sampledf)

#write out csv tidy data for figure 6 panel B
write.csv(final.df, "./tables/Figure_6_panel_C_tidy_data.csv")




