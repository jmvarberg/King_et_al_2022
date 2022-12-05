#Data clean up for Figure S6G

library(data.table)
library(tidyverse)

#read in figure 7 S3 design

figS6Gdesign <- fread("./King_Figure_S6_panel_G_design.csv")

#read in data
files <- figS6Gdesign$File
group <- figS6Gdesign$Group
treatment <- figS6Gdesign$Treatment

import <- function(x) {
  path <- paste0("./data/", x)
  df <- fread(path)
  return(df)
}

data <- lapply(files, import)
names(data) <- files

df <- plyr::ldply(data, data.frame)

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
    filter(!any(Nuclear.Envelope == 0 | BAD == 1 | Nuc.Area <= 2.5 | Time.point == "other"))
  return(df) 
}

#make clean df
cleandf<- cleanup(df) %>% select(-V1) %>% dplyr::rename(Data.Set = .id)

#make and join sample and data set IDs
sampledf <- data.frame(files, group, treatment)
colnames(sampledf) <- c("Data.Set", "Group", "Treatment")

final.df <- left_join(cleandf, sampledf)

#write out csv tidy data for Figure S3
write.csv(final.df, "./tables/Figure_S6G_tidy_data.csv")
