library(tidyverse)
library(stringr)
library(dunn.test)

qualy1 <- read_csv("C:/Users/dumellem/Desktop/turf_analyzer/turfquality_2015_data.csv")
qualy1_index <- qualy1 %>% select(trt, min, fert) %>% distinct()

dcomp_y1_fn <- function(mon){
  dunny1 <- dunn.test(x = unlist(qualy1[, mon]), g = qualy1$trt, altp = T)
  dunny1_dcomp <- data.frame(p_val = p.adjust(dunny1$altP, method = "holm"), 
                                 comp_1 = as.numeric(str_sub(dunny1$comparisons, start = 1, end = 1)), 
                                 comp_2 = as.numeric(str_sub(dunny1$comparisons, start = -1, end = -1)))
  dunny1_dcomp_re <- left_join(dunny1_dcomp, qualy1_index, by = c("comp_1" = "trt")) %>% 
    rename(min1 = min, fert1 = fert) %>% 
    select(-comp_1)
  dunny1_dcomp_re <- left_join(dunny1_dcomp_re, qualy1_index, by = c("comp_2" = "trt")) %>% 
    rename(min2 = min, fert2 = fert) %>% 
    select(-comp_2)
  dunny1_dcomp <- dunny1_dcomp_re %>% mutate(month = mon)
  return(dunny1_dcomp)
}

dunny1_dcomps <- bind_rows(dcomp_y1_fn(mon = "nov"), dcomp_y1_fn(mon = "dec"), dcomp_y1_fn(mon = "jan"),
                           dcomp_y1_fn(mon = "feb"), dcomp_y1_fn(mon = "mar"), dcomp_y1_fn(mon = "apr"))
dunny1_dcomps_sub <- dunny1_dcomps %>% filter(p_val <= 0.05)
dunny1_dcomps_sub
write_csv(dunny1_dcomps_sub, "C:/Users/dumellem/Desktop/turf_analyzer/dunny1_dcomps_sub.csv")


qualy2 <- read_csv("C:/Users/dumellem/Desktop/turf_analyzer/turfquality_2016_data.csv")
qualy2_index <- qualy2 %>% select(trt, min, fert) %>% distinct()

dcomp_y2_fn <- function(mon){
  dunny2 <- dunn.test(x = unlist(qualy2[, mon]), g = qualy2$trt, altp = T)
  dunny2_dcomp <- data.frame(p_val = p.adjust(dunny2$altP, method = "holm"), 
                             comp_1 = as.numeric(str_sub(dunny2$comparisons, start = 1, end = 1)), 
                             comp_2 = as.numeric(str_sub(dunny2$comparisons, start = -1, end = -1)))
  dunny2_dcomp_re <- left_join(dunny2_dcomp, qualy2_index, by = c("comp_1" = "trt")) %>% 
    rename(min1 = min, fert1 = fert) %>% 
    select(-comp_1)
  dunny2_dcomp_re <- left_join(dunny2_dcomp_re, qualy2_index, by = c("comp_2" = "trt")) %>% 
    rename(min2 = min, fert2 = fert) %>% 
    select(-comp_2)
  dunny2_dcomp <- dunny2_dcomp_re %>% mutate(month = mon)
  return(dunny2_dcomp)
}

dunny2_dcomps <- bind_rows(dcomp_y2_fn(mon = "nov"), dcomp_y2_fn(mon = "dec"), dcomp_y2_fn(mon = "jan"),
                           dcomp_y2_fn(mon = "feb"), dcomp_y2_fn(mon = "mar"), dcomp_y2_fn(mon = "apr"))
dunny2_dcomps_sub <- dunny2_dcomps %>% filter(p_val <= 0.05)
dunny2_dcomps_sub
write_csv(dunny2_dcomps_sub, "C:/Users/dumellem/Desktop/turf_analyzer/dunny2_dcomps_sub.csv")