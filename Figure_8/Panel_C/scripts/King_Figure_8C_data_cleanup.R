#Data clean up for Figure 8 panel C
library(data.table)
library(tidyverse)

#read in figure 7 S3 design

fig8.design <- fread("./King_Figure_8_panel_C_design.csv")

#read in data
files <- fig8.design$File
group <- fig8.design$Group

import <- function(x) {
  path <- paste0("./data/", x)
  df <- fread(path)
  return(df)
}

data <- lapply(files, import)
names(data) <- files

df <- plyr::ldply(data, data.frame)

cleanup <- function(x) {
  df <- x %>% mutate(Nucleus.ID = str_remove(Image, pattern = "_substack.*"),
                     BAD = ifelse(is.na(BAD),0, BAD),
                     NE.mean = Nuclear.Envelope/(WholeNuc.Area-Nucleoplasmic.Area),
                     Nuc.mean = Nucleoplasm/Nucleoplasmic.Area,
                     Norm.Nuc.NE.Ratio = Nuc.mean/NE.mean,
                     Norm.NE.Nuc.Ratio = NE.mean/Nuc.mean) %>% 
    group_by(Nucleus.ID) %>% 
    filter(!any(Nuclear.Envelope == 0 | BAD == 1))
  return(df) 
}

#make clean df
cleandf<- cleanup(df) %>% select(-V1) %>% dplyr::rename(Data.Set = .id)

#make and join sample and data set IDs
sampledf <- data.frame(files, group)
colnames(sampledf) <- c("Data.Set", "Group")

final.df <- left_join(cleandf, sampledf)

#write out csv tidy data for Figure S4
write.csv(final.df, "./tables/Figure_8C_tidy_data.csv")
