#Data clean up for Figure 7 panel B

library(data.table)
library(tidyverse)

#read in figure 4 design

fig7B.design <- fread("./Final_figures/Figure_7_Nup60_delta2-47/Panel_B/King_Figure_7_panel_B_design.csv")

#read in data
files <- fig7B.design$File
samples <- fig7B.design$Sample

import <- function(x) {
  path <- paste0("./Final_figures/Figure_7_Nup60_delta2-47/Panel_B/data/", x)
  df <- fread(path)
  return(df)
}

data <- lapply(files, import)
names(data) <- files

df <- plyr::ldply(data, data.frame)

cleanup <- function(x) {
  df <- x %>% mutate(Time.point = case_when(str_detect(Image, regex("frame_1")) ~ "-45 min",
                                            str_detect(Image, regex("frame_2")) ~ "-10 min",
                                            str_detect(Image, regex("frame_3")) ~ "Anaphase I",
                                            str_detect(Image, regex("frame_4")) ~ "+10 min",
                                            TRUE ~ "other"),
                     Nucleus.ID = str_remove(Image, pattern = "_substack.*"),
                     BAD = ifelse(is.na(BAD),0, BAD),
                     NE.mean = Nuclear.Envelope/(WholeNuc.Area-Nucleoplasmic.Area),
                     Nuc.mean = Nucleoplasm/Nucleoplasmic.Area,
                     Norm.Nuc.NE.Ratio = Nuc.mean/NE.mean,
                     Norm.NE.Nuc.Ratio = NE.mean/Nuc.mean) %>% 
    group_by(Nucleus.ID) %>% 
    filter(!any(Nuclear.Envelope == 0 | BAD == 1 | Time.point == "other"))
  return(df) 
}

#make clean df
cleandf<- cleanup(df) %>% select(-V1) %>% dplyr::rename(Data.Set = .id)

#make and join sample and data set IDs
sampledf <- data.frame(files, samples)
colnames(sampledf) <- c("Data.Set", "Sample")

final.df <- left_join(cleandf, sampledf)

#write out csv tidy data for figure 5 panel F
write.csv(final.df, "./Final_figures/Figure_7_Nup60_delta2-47/Panel_B/tables/Figure_7_panel_B_tidy_data.csv")
