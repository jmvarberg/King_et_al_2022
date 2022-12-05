#Data read in and clean up for Figure 3 panel H - Cdc5 WT or KD with CuSO4

library(data.table)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(knitr)
library(systemfonts)
library(kableExtra)
library(sinaplot)
library(ggforce)


# Data Input --------------------------------------------------------------


#read in figure 3 design

fig3_h.design <- fread("./King_Figure_3_Panel_H_design.csv")

#read in data
files <- fig3_h.design$File
samples <- fig3_h.design$Sample
treatment <- fig3_h.design$Treatment


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
                                            str_detect(Image, regex("frame_2")) ~ "Post",
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

cleandf<- lapply(data, cleanup)

combdf <- bind_rows(cleandf, .id = "Data.Set")
combdf <- combdf %>% 
  select(-V1)

sampledf <- data.frame(files, samples, treatment)
colnames(sampledf) <- c("Data.Set", "Sample", "Treatment")

final.df <- left_join(combdf, sampledf)

#write out cleaned up, tidy dataframe
write.csv(final.df, "./tables/Figure_3_panel_H_tidy_dataframe.csv")
