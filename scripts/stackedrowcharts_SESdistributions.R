library(tidyverse)
library(hrbrthemes)


housecount_ses <- c(15369, 32289, 39843, 39729, 36242, 5955)  # A, B, C, D, E, Undef
casecount_ses <- c(6, 21, 46, 120, 129, 23)
#sampcount_ses <- c(107, 352, 614, 583, 431, 32)
sum(housecount_ses)
sum(casecount_ses)
#sum(sampcount_ses)
prop.table(housecount_ses)*100
prop.table(casecount_ses)*100
#prop.table(sampcount_ses)*100

ses_df <- data.frame(ses = c("A", "B", "C", "D", "E", "Undef"),
                     houses = housecount_ses,
                     cases = casecount_ses)
ses_long <- gather(ses_df, type, count, houses, cases)
ses_long$type <- factor(ses_long$type, ordered = T, levels = c("cases", "samples", "houses"))

pdf("figures/R_output/stackedrowchart.pdf", width = 8, height = 4)

ggplot(ses_long) +
  geom_bar(aes(x = type, y = count, fill = ses), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  theme_ipsum() +
  coord_flip()

dev.off()
