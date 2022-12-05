#Data import and clean up for Figure 1 panel C - Nups-GFP through meiosis


#load required packages
library(data.table)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(systemfonts)
library(sinaplot)
library(ggforce)
library(rstatix)

#data input

design <- fread("./Final_figures/Figure_1_Nups_subcomplex_Meiosis/King_figure_1_design.csv")
files <- design$File
samples <- design$Sample
subcomplex <- design$Subcomplex

import <- function(x) {
  path <- paste0("./Final_figures/Figure_1_Nups_subcomplex_Meiosis/data/", x)
  df <- fread(path)
  return(df)
}

data <- lapply(files, import)
names(data) <- files

# Data Cleanup ------------------------------------------------------------

cleanup <- function(x) {
  df <- x %>% mutate(Time.point = case_when(str_detect(Image, regex("frame_1")) ~ "Pre",
                                            str_detect(Image, regex("frame_2")) ~ "Anaphase",
                                            str_detect(Image, regex("frame_3")) ~ "Post",
                                            TRUE ~ "other"),
                     Nucleus.ID = str_remove(Image, pattern = "_substack.*"),
                     BAD = ifelse(is.na(BAD),0, BAD),
                     NE.mean = Nuclear.Envelope/(WholeNuc.Area-Nucleoplasmic.Area), #divide Nuclear.Envelope signal by NE area (whole - nucleoplasmic)
                     Nuc.mean = Nucleoplasm/Nucleoplasmic.Area, #divide nucleoplasmic signal by nucleoplasmic area
                     Norm.Nuc.NE.Ratio = Nuc.mean/NE.mean, #calculate area-normalized Nuc/NE ratio
                     Norm.NE.Nuc.Ratio = NE.mean/Nuc.mean) %>% #calculate area-normalized NE/Nuc ratio
    group_by(Nucleus.ID) %>% 
    add_tally() %>% 
    filter(!any(Nuclear.Envelope == 0 | BAD == 1 | Time.point == "other" | n != length(unique(.$Time.point)))) #remove any nuclei where NE is 0 from manually skipping in ImageJ, annotated as Bad, or not full set of images anlyzed for each timepoint. If any conditions met, then entire nucleus set is removed.
  return(df) 
}

cleandf<- lapply(data, cleanup)

combdf <- bind_rows(cleandf, .id = "Data.Set")

combdf <- combdf %>% 
  select(where(~ !(all(is.na(.)) | all(. == "")))) %>% #added selection to remove all columns with all NA/empty values, issue from reading in empty columns in csv file
  select(-V1) 

sampledf <- data.frame(files, samples, subcomplex)
colnames(sampledf) <- c("Data.Set", "Sample", "Subcomplex")

final.df <- left_join(combdf, sampledf)
final.df <- final.df %>% 
  filter(Sample != "Ulp1-GFP") %>% 
  mutate(Time.point = factor(Time.point, levels=c("Pre", "Anaphase", "Post"))) %>% 
  arrange(Time.point)

#write out cleaned up, tidy dataframe
write.csv(final.df, "./Final_figures/Figure_1_Nups_subcomplex_Meiosis/tables/Figure_1_panel_C_tidy_dataframe.csv")

