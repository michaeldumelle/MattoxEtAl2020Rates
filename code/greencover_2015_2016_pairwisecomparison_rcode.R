library(tidyverse)

responsey1 <- read_csv("C:/Users/dumellem/Desktop/turf_analyzer/turf_analyzer_y1_pcomps.csv") 
head(responsey1)
responsey1_sub <- responsey1 %>% filter(month1 == month2)
responsey1_sub$p_val = 2 * pt(abs(responsey1_sub$t_stat), df = responsey1_sub$df, lower.tail = F)


responsey1_sub_nov <- responsey1_sub %>% filter(month1 == 1)
responsey1_sub_nov$p_val_adj = p.adjust(responsey1_sub_nov$p_val, method = "holm")
responsey1_sub_nov_sub = responsey1_sub_nov %>% filter(responsey1_sub_nov$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "nov")



responsey1_sub_dec <- responsey1_sub %>% filter(month1 == 2)
responsey1_sub_dec$p_val_adj = p.adjust(responsey1_sub_dec$p_val, method = "holm")
responsey1_sub_dec_sub = responsey1_sub_dec %>% filter(responsey1_sub_dec$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "dec")


responsey1_sub_jan <- responsey1_sub %>% filter(month1 == 3)
responsey1_sub_jan$p_val_adj = p.adjust(responsey1_sub_jan$p_val, method = "holm")
responsey1_sub_jan_sub = responsey1_sub_jan %>% filter(responsey1_sub_jan$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "jan")



responsey1_sub_feb <- responsey1_sub %>% filter(month1 == 4)
responsey1_sub_feb$p_val_adj = p.adjust(responsey1_sub_feb$p_val, method = "holm")
responsey1_sub_feb_sub = responsey1_sub_feb %>% filter(responsey1_sub_feb$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "feb")


turf_analyzer_y1_pcomps_sub <- bind_rows(responsey1_sub_nov_sub, responsey1_sub_dec_sub, 
                                         responsey1_sub_jan_sub, responsey1_sub_feb_sub)

#write_csv(turf_analyzer_y1_pcomps_sub, "C:/Users/dumellem/Desktop/turf_analyzer/turf_analyzer_y1_pcomps_sub.csv")


responsey2 <- read_csv("C:/Users/dumellem/Desktop/turf_analyzer/turf_analyzer_y2_pcomps.csv") 
head(responsey2)
responsey2_sub <- responsey2 %>% filter(month1 == month2)
responsey2_sub$p_val = 2 * pt(abs(responsey2_sub$t_stat), df = responsey2_sub$df, lower.tail = F)


responsey2_sub_nov <- responsey2_sub %>% filter(month1 == 1)
responsey2_sub_nov$p_val_adj = p.adjust(responsey2_sub_nov$p_val, method = "holm")
responsey2_sub_nov_sub = responsey2_sub_nov %>% filter(responsey2_sub_nov$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "nov")



responsey2_sub_dec <- responsey2_sub %>% filter(month1 == 2)
responsey2_sub_dec$p_val_adj = p.adjust(responsey2_sub_dec$p_val, method = "holm")
responsey2_sub_dec_sub = responsey2_sub_dec %>% filter(responsey2_sub_dec$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "dec")


responsey2_sub_jan <- responsey2_sub %>% filter(month1 == 3)
responsey2_sub_jan$p_val_adj = p.adjust(responsey2_sub_jan$p_val, method = "holm")
responsey2_sub_jan_sub = responsey2_sub_jan %>% filter(responsey2_sub_jan$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "jan")



responsey2_sub_feb <- responsey2_sub %>% filter(month1 == 4)
responsey2_sub_feb$p_val_adj = p.adjust(responsey2_sub_feb$p_val, method = "holm")
responsey2_sub_feb_sub = responsey2_sub_feb %>% filter(responsey2_sub_feb$p_val_adj < .05) %>%
  select(-month1, -month2) %>% 
  mutate(month = "feb")


turf_analyzer_y2_pcomps_sub <- bind_rows(responsey2_sub_nov_sub, responsey2_sub_dec_sub, 
                                         responsey2_sub_jan_sub, responsey2_sub_feb_sub)

write_csv(turf_analyzer_y2_pcomps_sub, "C:/Users/dumellem/Desktop/turf_analyzer/turf_analyzer_y2_pcomps_sub.csv")
