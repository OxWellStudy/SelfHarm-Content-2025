##############################################################
#  This analysis uses OxWell data (oxwell.org; osf.io/sekhr) #
#     to explore exposure to self-harm content online        #
##############################################################

library(cyphr)
library(data.table)
library(dplyr)
library(tidyr)
library(compareGroups)
library(writexl)
library(readxl)
library(nnet)
library(gtsummary)
library(parameters)
library(ggplot2)
library(stringr)
library(emmeans)
library(broom)
library(mice)
library(VIM)
library(nnet)
library(psych)
library(epitools)

######################### DATA IMPORT ######################### 
## Data are imported using cyphr:
if ( 0 ) {
  OXWELL.DATA.DIR <- "" # Path to OxWell data
  PERSONAL.SSH.DIR <- "" # Path to personal SSH key (for decrypting OxWell data)
  PERSONAL.DIR <- "" # Path to wd 
} else {
  source("local.R")
}

KEY <- cyphr::data_key(path_data=OXWELL.DATA.DIR,path_user=PERSONAL.SSH.DIR)
DATA <- cyphr::decrypt_object(key=KEY, data=file.path(OXWELL.DATA.DIR, "DATA2025/OXWELL2025.R13-analysis.rds-cyphr") )
setwd(PERSONAL.DIR)

#########################  DATA PREP  #########################
# N.B. Exact wording for all variables can be found in the 2025 variable guide on the OSF (osf.io/sekhr)

df <- subset(DATA, SURVEY.CORE == TRUE)

df <- df[,c('OXWELL.GENDER.TGD', 'OXWELL.YEARGROUP', 'X1040', 'X1500', 'X1425', 'X1780', 'X2800', 'X2870', 'RCADS.ANXIETY.IMPUTEtscore', 'RCADS.DEPRESSION.IMPUTEtscore', 
            'X2370', 'X2371a', 'X2371b', 'X2371c', 'X2371d', 'X2371e', 'X2371f',
            'X1501', 'X1502', 'X2810', 'X2820', 'X2830', 'X2840', 'X2850', 'X2860', 'X1070', 'X1090', 'X1082', 'X1130', 'X2120', 'SWEMWBS.SCORE')] 

# For the following columns, 'NoResponse(Item)' = No, as per OxWell coding of tickbox style questions, and 
  # all other types of non-response (incl. skipped) = NA
cols <- c('X2371a', 'X2371b', 'X2371c', 'X2371d', 'X2371e', 'X2371f')

for (col in cols) {
  df[[col]] <- ifelse(df[[col]] == 'NoResponse(Item)', 'No',
                      ifelse(df[[col]] %in% c('NoResponse(Stopped_vB)', 'Skipped(GW_NoResponse)', 'Skipped(GW_Choice)'), NA, 'Ticked (Yes)'))
}

# OxWell codes missing data differently according to the 'type' of missingness - for the purposes of
  # these analyses, re-code all forms of NoResponse to NA
df[df=='NoResponse(Item)' | df=='NoResponse(Stopped_vB)'] <- NA 

# Variable prep/re-labelling/re-levelling, etc.
df$gender <- df$OXWELL.GENDER.TGD # This is a derived variable with a specific 'trans and gender diverse' category derived from qs X1020, X1022, X1023
df$gender <- factor(df$gender, levels=c("Boy / Man", "Girl / Woman", "Trans/Gender Diverse", 
                                        "Don't know / Not sure", "Prefer not to say"))

df$yeargroup <- df$OXWELL.YEARGROUP
df$yeargroup <- as.factor(df$yeargroup)
df$yeargroup <- droplevels(df$yeargroup)

# Re-categorise ethnicity according to ONS categorisation 6a (Arab -> Other ethnic group)
df$ethnicity <- df$X1040
df$ethnicity[df$ethnicity == "Arab"] <- "Other Ethnic Group"
df$ethnicity <- factor(df$ethnicity, levels=c("White", "Asian/Asian British", "Black/African/Caribbean/Black British", 
                                              "Mixed/Multiple Ethnic Groups", "Other Ethnic Group"))

df$hungry <- df$X1500 # 'At home, do you go to bed hungry because there is not enough food in the house?'
df$hungry <- factor(df$hungry, levels = c("Never or hardly ever", "Some of the time", "Often"))

df$loneliness <- df$X1780 # 'How often do you feel lonely?'
df$loneliness <- factor(df$loneliness, levels= c("Hardly ever or never", "Some of the time", "Often"))

df$bullying <- df$X1425 # 'How often have you been bullied at school?'
df$bullying <- factor(df$bullying, levels=c("I have not been bullied in the past couple of months", "Once, twice or a few times",
                                            "2 or 3 times a month", "About once a week", "Several times a week"))

df$aggression <- df$X2800 # 'I have received threatening or aggressive messages online'
df$aggression <- factor (df$aggression, levels = c("No", "Yes", "Prefer not to say"))

df$coercion <- df$X2870 # 'I have been pressured to do something online I didn’t want to do'
df$coercion <- factor (df$coercion, levels = c("No", "Yes", "Prefer not to say"))

df$anxiety <- df$RCADS.ANXIETY.IMPUTEtscore 
df$anxiety_scaled <- scale(df$anxiety) # Scaled to aid interpretability 

df$depression <- df$RCADS.DEPRESSION.IMPUTEtscore 
df$depression_scaled <- scale(df$depression) # Scaled to aid interpretability 

# Generate 'exposure' factor variable w/ ref: No exposure
df[, exposure := fcase(
  X2370 == "No, I haven’t come across self-harm content in the past month", "No exposure",
  X2370 == "Yes, once or twice", "Once or twice",
  X2370 == "Yes, a few times", "A few times",
  X2370 == "Yes, several times", "Several times"
)]

df[, exposure := factor(exposure,
                        levels = c("No exposure","Once or twice","A few times","Several times"))]

# Exposure proportions (with CIs as requested by reviewer (R1))
exp_props95CI<- df %>%
  filter(!is.na(exposure)) %>%
  count(exposure) %>%
  mutate(
    prop = n / sum(n),
    percent = prop * 100,
    CI_L = (prop - 1.96 * sqrt(prop * (1 - prop) / sum(n))) * 100,
    CI_U = (prop + 1.96 * sqrt(prop * (1 - prop) / sum(n))) * 100
  )

write_xlsx(exp_props95CI, "R1/Tables/Exposure with CIs.xlsx")

df$any_exp <- ifelse(df$exposure == "No exposure", "None", "Exposure")

any_exp_props95CI<- df %>%
  filter(!is.na(any_exp)) %>%
  count(any_exp) %>%
  mutate(
    prop = n / sum(n),
    percent = prop * 100,
    CI_L = (prop - 1.96 * sqrt(prop * (1 - prop) / sum(n))) * 100,
    CI_U = (prop + 1.96 * sqrt(prop * (1 - prop) / sum(n))) * 100
  )

write_xlsx(any_exp_props95CI, "R1/Tables/Any exposure with CIs.xlsx")

# Mode of exposure (re-label)
df$search <- df$X2371a
df$feed <- df$X2371b
df$accident <- df$X2371c
df$embedded <- df$X2371d
df$shared <- df$X2371e
df$other <- df$X2371f

# Pathway categorised as follows: 
 # NA = ticked none
 # Passive only = ticked one or more of feed, accident, embedded ONLY
 # Active search only = ticked search ONLY 
 # Active receive only = ticked shared ONLY 
 # Other only = ticked other ONLY 
 # Mixed = all other combos

df[exposure == "No exposure", pathway := "No exposure"]

df[exposure != "No exposure",
   pathway := fcase(
     rowSums(.SD == "Ticked (Yes)", na.rm = TRUE) == 0, NA_character_,  # Maintain NA where appropriate
     search == "Ticked (Yes)" & rowSums(.SD == "Ticked (Yes)", na.rm = TRUE) == 1, "Active-searched",
     shared == "Ticked (Yes)" & rowSums(.SD == "Ticked (Yes)", na.rm = TRUE) == 1, "Active-received",
     other  == "Ticked (Yes)" & rowSums(.SD == "Ticked (Yes)", na.rm = TRUE) == 1, "Other-only",
     !(search %in% "Ticked (Yes)" | shared %in% "Ticked (Yes)" | other %in% "Ticked (Yes)") &
       rowSums(.SD[, c("feed", "accident", "embedded")] == "Ticked (Yes)", na.rm = TRUE) >= 1, "Passive-only",
     default = "Mixed"
   ), .SDcols = c("search", "feed", "accident", "embedded", "shared", "other")]

table(df$pathway)

df$pathway <- factor(df$pathway, levels=c("No exposure", "Passive-only", 
                                          "Active-searched", "Active-received", "Mixed", "Other-only"))

# Pathway proportions (with CIs as requested by reviewer (R1))
path_props95CI <- df %>%
  filter(!is.na(pathway) & pathway != "No exposure") %>%
  count(pathway) %>%
  mutate(
    prop = n / sum(n),
    percent = prop * 100,
    CI_L = (prop - 1.96 * sqrt(prop * (1 - prop) / sum(n))) * 100,
    CI_U = (prop + 1.96 * sqrt(prop * (1 - prop) / sum(n))) * 100
  )

write_xlsx(path_props95CI, "R1/Tables/Pathways with CIs.xlsx")

######################### DESCRIPTIVES  #########################
# Examine missingness across all variables 
colSums(is.na(df[, c("gender", "yeargroup", "ethnicity", "hungry", 
                      "bullying", "loneliness", "aggression", "coercion",
                      "anxiety_scaled", "depression_scaled", "pathway", "exposure")]))

#### TABLE 1 Part 1: Overall descriptives + by exposure ####
# Overall sample descriptives
table <- compareGroups(~ gender + yeargroup + ethnicity + hungry + loneliness + bullying +
                         aggression + coercion + anxiety + depression,
                       data = df,
                       byrow = TRUE, include.miss= TRUE) 
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)

write_xlsx(output, "R1/Tables/Full sample.xlsx")

# Sample descriptives by exposure
table <- compareGroups(exposure ~  gender + yeargroup + ethnicity + hungry + loneliness + bullying + 
                         aggression + coercion + anxiety + depression,
                       data = df,
                       byrow = TRUE, include.miss= TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)

write_xlsx(output, "R1/Tables/By exposure freq.xlsx")

#### TABLE 2: Descriptives by exposure pathway, barring those with no reported pathway ####
df_t2 <- subset(df, pathway != "No pathway reported" & pathway != "No exposure")

table <- df_t2 %>%
  pivot_longer(
    cols = c(search, feed, accident, embedded, shared, other),
    names_to = "mode",
    values_to = "response"
  ) %>%
  filter(response == "Ticked (Yes)") %>%
  count(mode) %>%
  mutate(percent = n / nrow(df_t2) * 100)

write_xlsx(table, "R1/Tables/Mode of exposure.xlsx")

#### TABLE 1 Part 2: Descriptives by pathway ####
# For those who report exposure, examine characteristics by exposure pathway
table <- compareGroups(pathway ~  gender + yeargroup + hungry + ethnicity + loneliness + bullying + 
                         aggression + coercion + anxiety + depression,
                       data = df_t2,
                       byrow = TRUE, include.miss= TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)

write_xlsx(output, "R1/Tables/By pathway.xlsx")

#### TABLE S1: Descriptives by pathway and frequency ####
table <- compareGroups(exposure ~  pathway,
                       data = df,
                       byrow = TRUE, include.miss= TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)

write_xlsx(output, "R1/Tables/Pathway & frequency.xlsx")

#### Cronbach's alpha calculation for RCADS subscales ####
# Requested by peer reviewer (R1)
df_rel <- subset(DATA, SURVEY.CORE == TRUE)

dep <- c("X2000","X1970","X2024","X2026","X1940","X2031","X1990","X2034","X2020","X2038")
anx <- c("X2011","X2012","X2021","X2022","X2023","X2025","X2027","X2028","X2029","X2032","X2033","X2035","X2036","X2037","X2039")
score_rcads <- function(x) {
  case_when(
    as.character(x) == "Never"     ~ 0,
    as.character(x) == "Sometimes" ~ 1,
    as.character(x) == "Often"     ~ 2,
    as.character(x) == "Always"    ~ 3,
    TRUE ~ NA_real_
  )
}

dep_numeric <- as.data.frame(lapply(as.data.frame(df_rel)[, dep], score_rcads)) 
anx_numeric <- as.data.frame(lapply(as.data.frame(df_rel)[, anx], score_rcads))

alpha_depression <- psych::alpha(dep_numeric)
alpha_anxiety <- psych::alpha(anx_numeric)

######################### REGRESSION ANALYSES (COMPLETE CASES) #########################
#### MULTINOMIAL REGRESSION MODEL ####
model <- multinom(pathway ~ gender + yeargroup + ethnicity + hungry + bullying + 
                    loneliness + aggression + coercion + anxiety_scaled + depression_scaled,
          data = df, maxit = 500)

glance(model)

# Calculate McFadden's R^2 (requested by reviewer (R1))
null_model <- multinom(pathway ~ 1, data = df)
1 - (logLik(model) / logLik(null_model))

#### TABLE S4: Full regression output ####
results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

results <- results %>%
  mutate(
    p.value = sprintf("%.3f", p.value),
    across(c(estimate, conf.low, conf.high), ~ sprintf("%.2f", .x))
  )

results[["Coefficient [95% CI]"]] <-
  paste0(
    results$estimate, " [", results$conf.low,", ", results$conf.high, "]"
  )

write_xlsx(results, "R1/Regression output/Findings - Complete Cases.xlsx")

#### TABLE S5: Predicted probabilities  ####
# Generate predicted probabilities to facilitate interpretation 
# First, two more 'manual', explicit examples, then looped over for efficiency 
predprob_gender <-emmeans(object = model, 
               specs = ~ pathway | gender, 
               at = list(yeargroup = "Y07", 
                   ethnicity = "White",
                   hungry = "Never or hardly ever",
                   bullying = "I have not been bullied in the past couple of months", 
                   loneliness = "Hardly ever or never",
                   aggression = "No",
                   coercion = "No", 
                   anxiety_scaled = 0, 
                   depression_scaled = 0))

predprob_yeargroup <-emmeans(object = model, 
                    specs = ~ pathway | yeargroup, 
                    at = list(gender = "Boy / Man", 
                              ethnicity = "White", 
                              hungry = "Never or hardly ever",
                              bullying = "I have not been bullied in the past couple of months", 
                              loneliness = "Hardly ever or never",
                              aggression = "No",
                              coercion = "No", 
                              anxiety_scaled = 0, 
                              depression_scaled = 0))

## Looped version (cross-checked with above for accuracy)
preds <- attr(terms(model), "term.labels")
results_list <- list()

for(p in preds){
  x <- df[[p]]
   
  # Hold other variables constant - use mean for dep/anx (i.e. 0, as scaled variable) & reference level of factors
  at_list <- list()
  for(q in preds[preds != p]) {
    others <- df[[q]]
    at_list[[q]] <- if(is.numeric(others)) mean(others, na.rm = TRUE) else levels(as.factor(others))[1] 
  }
  
  # For dep/anx, calculate pred probs for values of 0 (= mean) and +/- 1 (= +/- 1sd), 
    # otherwise evaluate all factor levels (and throw error message if neither numeric nor factor)
  focal_vals <- if (is.numeric(x)) c(-1,0,1) else if (is.factor(x)) (levels(x)) else stop("Error in col:", p)
  
  # set focal values (list of values you want to evaluate probability at.)
  at_list[[p]] <- focal_vals
  
  emm <- emmeans(model, as.formula(paste0("~ pathway | ", p)),
                 type = "response", at = at_list, infer = TRUE)
  df1 <- as.data.frame(emm) 
  
  df1$final_results <- sprintf("%.2f [%.2f, %.2f]",
                  df1$prob,
                  df1$lower.CL,
                  df1$upper.CL)
  
  results_list[[p]] <- df1
}

fulltable <- do.call(rbind, results_list)
rownames(fulltable) <- NULL
#write_xlsx(fulltable, "R1/Regression output/Predicted probabilities - Complete Cases.xlsx")

##### FIGURE S21: Plotted regression coefficients  #####
pal <- c("Passive-only" = "#377eb8", "Active-searched" = "#e41a1c",
         "Active-received" = "#ff7f00", "Mixed" = "#4daf4a",
         "Other-only" = "#984ea3")

shape_map <- c("Passive-only" = 16, "Active-searched" = 17,
               "Active-received" = 15, "Mixed" = 18,
               "Other-only" = 8)

df_plot <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  transmute(
    Predictor = term,
    aOR       = estimate,
    CI_L      = conf.low,
    CI_U      = conf.high,
    Outcome   = y.level
  )

df_plot <- df_plot %>%
  mutate(
    predictor_label = Predictor %>%
      str_replace("gender", "Gender: ") %>%
      str_replace("yeargroup", "Year group: ") %>%
      str_replace("ethnicity", "Ethnicity: ") %>%
      str_replace("hungry", "Hungry: ") %>%
      str_replace("bullying", "Bullying: ") %>%
      str_replace("loneliness", "Loneliness: ") %>%
      str_replace("aggression", "Online aggression: ") %>%
      str_replace("coercion", "Online coercion: ") %>%
      str_replace("anxiety_scaled", "Anxiety (scaled RCADS subscale score)") %>%
      str_replace("depression_scaled", "Depression (scaled RCADS subscale score)")
  )

df_plot <- df_plot %>%
  mutate(
    var_group = case_when(
      str_detect(Predictor, "gender") ~ "Gender",
      str_detect(Predictor, "yeargroup") ~ "Year group",
      str_detect(Predictor, "ethnicity") ~ "Ethnicity",
      str_detect(Predictor, "hungry") ~ "Hungry",
      str_detect(Predictor, "bullying") ~ "Bullying",
      str_detect(Predictor, "loneliness") ~ "Loneliness",
      str_detect(Predictor, "aggression") ~ "Online aggression",
      str_detect(Predictor, "coercion") ~ "Online coercion",
      str_detect(Predictor, "anxiety_scaled") ~ "Anxiety",
      str_detect(Predictor, "depression_scaled") ~ "Depression",
      TRUE ~ "Other"
    )
  )

df_plot <- df_plot %>% filter(Predictor != "(Intercept)")

group_order <- c("Gender","Year group","Ethnicity", "Hungry", "Bullying","Loneliness",
                 "Online aggression","Online coercion","Anxiety","Depression","Other")

df_plot$var_group <- factor(df_plot$var_group, levels = group_order)

df_plot$predictor_label <- factor(df_plot$predictor_label, levels = rev(unique(df_plot$predictor_label)))
df_plot$Outcome <- factor(df_plot$Outcome, levels = c("Passive-only", "Active-searched", "Active-received", "Mixed", "Other-only"))

# Spacing prep
ordered <- df_plot %>%
  distinct(predictor_label) %>%
  pull(predictor_label) %>%
  as.character()

counts  <- c(4,6,4,2,4,2,2,2)

new <- character(); i <- 1; s <- 1
for (c in counts) {
  end <- min(i + c - 1, length(ordered))
  if (i <= end) { new <- c(new, ordered[i:end]); i <- end + 1 }
  if (i <= length(ordered)) { new <- c(new, paste0("SP", s)); s <- s + 1 }
}
if (i <= length(ordered)) new <- c(new, ordered[i:length(ordered)])

spacers <- grep("^SP", new, value = TRUE)
spacer_rows <- expand.grid(predictor_label = spacers, Outcome = levels(df_plot$Outcome), stringsAsFactors = FALSE) %>%
  mutate(Predictor = NA, aOR = NA, CI_L = NA, CI_U = NA, var_group = "Spacer") %>%
  select(Predictor, aOR, CI_L, CI_U, predictor_label, var_group, Outcome)

df_plot <- bind_rows(df_plot %>% mutate(predictor_label = as.character(predictor_label)), spacer_rows)
df_plot$predictor_label <- factor(df_plot$predictor_label, levels = rev(new))

df_plot$Outcome <- factor(
  df_plot$Outcome,
  levels = c("Passive-only", "Active-searched", "Active-received", "Mixed", "Other-only")
)

p <- ggplot(df_plot, aes(x = aOR, y = predictor_label, colour = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 1, linetype = "solid", colour = "grey70") +
  geom_errorbar(aes(xmin = CI_L, xmax = CI_U),
                width = 0.18,
                orientation = "y",
                position = position_dodge(width = 0.6),
                linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 2, position = position_dodge(width = 0.6), na.rm = TRUE) +
  scale_colour_manual(values = pal, name = "Pathway") +
  scale_shape_manual(values = shape_map, name = "Pathway") +
  scale_x_log10(limits = c(.125, 8.3),
                breaks = c(.125, 0.25, 0.5, 1, 2, 4, 8),
                labels = c(".125",".25",".5","1","2","4","8")) +
  labs(title = "Adjusted odds ratios from multinomial model (complete cases)",
       x = "Adjusted odds ratio (95% CI)",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_grid(. ~ Outcome, switch = "x", space = "free_x", scales = "free_x") +
  scale_y_discrete(labels = function(l) ifelse(grepl("^SP", l), "", l)) +
  theme(panel.spacing.x = unit(1.2, "cm"))

p

ggsave (p, filename="R1/Plots/Figure 1 - COMPLETE CASES.png", width = 15, height = 10, dpi = 600 )

######################### MULTIPLE IMPUTATION ###############
# Have wrapped most of these in if(0) as time-intensive & only needs to be run once 
 # (Work with existing MIDS500_clean (loaded below) if replicating within OxWell team)

#### MI Part 1. Data prep, missingness pattern exploration, and selection of auxiliary variables ####
if(0){
  # 1.1 DATA PREP: Create new data_MI that excludes all levels containing 'NoResponse' (even though they're empty per above processing)
  df_MI <- df %>%
    mutate(across(where(is.factor), ~ {
      keep <- levels(.)[!str_detect(levels(.), 'NoResponse')]
      droplevels(factor(.,  levels = keep))
    }))
  
  # 1.2. EXPLORE MISSINGNESS PATTERNS
  data_MI_pattern <-df_MI[, c('gender', 'yeargroup', 'ethnicity', 'hungry', 'bullying', 
                              'loneliness', 'aggression', 'coercion', 'anxiety_scaled', 'depression_scaled', 
                              'pathway', 'exposure')]
  md.pattern(data_MI_pattern)
  aggr(data_MI_pattern, col=c('darkblue','pink'), numbers=TRUE, sortVars=TRUE, labels=names(data_MI_pattern), 
       cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
  
  # 1.3. SELECT AUXILIARY VARS
  # To select which variables to include in the model, we've used cut-point of 15% 'new info' (i.e. to be added, 
    # a variable has to have ≥15% non-NA values where the main variable has NAs)
  
  # Go to bed hungry
  xtabs (~ is.na(hungry) + is.na(X1501), data = df) # How many times did you travel away..? --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(hungry) + is.na(X1502), data = df) # Free school meals --> <15% new info (DO NOT ADD)
  
  # Bullying
  xtabs (~ is.na(bullying) + is.na(X1130), data = df) # School management of bullying --> <15% new info (DO NOT ADD)
  
  # Aggression
  xtabs (~ is.na(aggression) + is.na(X2810), data = df) # Excluded online --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(aggression) + is.na(X2820), data = df) # Rumours online --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(aggression) + is.na(X2830), data = df) # Teased online --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(aggression) + is.na(X2840), data = df) # Sexual comments/images --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(aggression) + is.na(X2850), data = df) # Private messages shared --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(aggression) + is.na(X2860), data = df) # Harassed/stalked--> <15% new info (DO NOT ADD)
  
  # Coercion
  xtabs (~ is.na(coercion) + is.na(X2810), data = df) # Excluded online --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(coercion) + is.na(X2820), data = df) # Rumours online --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(coercion) + is.na(X2830), data = df) # Teased online --><15% new info (DO NOT ADD)
  xtabs (~ is.na(coercion) + is.na(X2840), data = df) # Sexual comments/images --> <15% new info (DO NOT ADD)
  xtabs (~ is.na(coercion) + is.na(X2850), data = df) # Private messages shared --> <15% new info (DO NOT ADD) 
  xtabs (~ is.na(coercion) + is.na(X2860), data = df) # Harassed/stalked --> <15% new info (DO NOT ADD)
  
  # RCADS anxiety
  xtabs (~ is.na(anxiety_scaled) + is.na(SWEMWBS.SCORE), data = df) # SWEMWBS --> >15% new info (INCLUDE) 
  xtabs (~ is.na(anxiety_scaled) + is.na(X1070), data = df) # Long term conditions --> >15% new info (INCLUDE)
  xtabs (~ is.na(anxiety_scaled) + is.na(X1090), data = df) # SEN/EHCP --> >15% new info (INCLUDE) 
  xtabs (~ is.na(anxiety_scaled) + is.na(X1081), data = df) # Self-ID'ed neurodivergence --> >15% new info (INCLUDE)
  xtabs (~ is.na(anxiety_scaled) + is.na(X2120), data = df) # Self-harm --> >15% new info (INCLUDE) 
  
  # RCADS depression
  # All above variables already included in imputation model 
  
}

#### MI Part 2. Running imputation model ####
if(0){
  # 2.1. MI PREP: Setting up predictor matrix, method, etc.
  
  TEMP <- df_MI[, c('gender', 'yeargroup', 'ethnicity', 'hungry', 'bullying', 'loneliness', 'aggression', 'coercion', 
                  'anxiety_scaled', 'depression_scaled', 
                  'pathway', # Impute the pathway as a single outcome rather than imputing each individual item  
                  'X1070', 'X1090', 'X1082', 'X2120', 'SWEMWBS.SCORE',
                  'exposure')] 
  
  METHOD <- make.method(TEMP)
  METHOD["exposure"] <- "" # Don't want to impute exposure - will use ONLY to identify participants for post-processing below
  
  PRED <- make.predictorMatrix(TEMP)
  PRED[, "exposure"] <- 0 # Don't want to use exposure within imputation
  PRED["exposure", ] <-0 # Don't want to use exposure within imputation
  PRED

  # 2.2. RUN IMPUTATION MODEL (or load from existing files)
  # Run in small batches of m=5 as !VERY! time intensive!
  if(0){
    for (i in 1:100) { 
      
      file <- paste0("MIDS", ((i-1)*5+1), "_", 5*i) # MIDS1_5, MIDS6_10, etc.
      
      cat("Running batch:", file, "\n")
      
      MIDS <- mice(data = TEMP,
                   method = METHOD,
                   predictorMatrix = PRED,
                   m = 5,
                   maxit = 15,
                   seed = 123 + i) # Different seed for each batch
      
      saveRDS(MIDS, file = paste0("MICE/", file, ".rds"))
    }
    
  } else {
    path <- ("MICE/")
    
    rds_files <- list.files(
      path = path,
      pattern = "^MIDS.*\\.rds$",
      full.names = TRUE
    )
    
    for (file in rds_files) {
      obj_name <- tools::file_path_sans_ext(basename(file))
      obj <- readRDS(file)
      assign(obj_name, obj)
    }
    
    mids_list <- mget(ls(pattern = "^MIDS"))
    MIDS500 <- Reduce(ibind, mids_list)
  }
  saveRDS(MIDS500, file = "MICE/MIDS500.rds")
}

#### MI Part 3. Imputation post-processing ####
if(0){
  
  # 3.1. LOAD IN MIDS500 FROM ABOVE 
  MIDS500 <- readRDS(file = "MICE/MIDS500.rds")
  
  # 3.2. APPLY POST-PROCESSING 
  
  # n = 138 (0.4%) had 'NA' for pathway but DID report exposure, meaning if mice has assigned them a
  # pathway of 'No exposure', this is not accurate. Rather than introducing complexity via 
  # conditional imputation, here we post-process these as follows: 
    # a. Determine whether any of the imputed datasets contain any participants with 'Yes' 
          # answers to exposure and 'No exposure' to pathway
    # b. Randomly assign these to one of the four pathways using weighted probabilities 
          # calculated from complete cases
  
  exposed <- subset(df, (pathway != "No exposure" & !is.na(pathway)))
  prob_passive <- sum(exposed$pathway == "Passive-only") / nrow(exposed)
  prob_activesearch <- sum(exposed$pathway == "Active-searched") / nrow(exposed)
  prob_activereceive <- sum(exposed$pathway == "Active-received") / nrow(exposed)
  prob_mixed <- sum(exposed$pathway == "Mixed") / nrow(exposed)
  prob_otheronly <- sum(exposed$pathway == "Other-only") / nrow(exposed)
  
  probs <- c(prob_passive, prob_activesearch, prob_activereceive, prob_mixed, prob_otheronly)
  pathways <- c("Passive-only", "Active-searched", "Active-received", "Mixed", "Other-only")
  
  # Test with one dataset
  MIDStest <- MIDS500
  test <- complete(MIDS500, action = 1)
  
  conflict <- which(test$exposure != "No exposure" & test$pathway == "No exposure")
  length(conflict)
  table(test$exposure, test$pathway, useNA="always")
  
  set.seed(118)
  test$pathway[conflict] <- sample((pathways), 
                                        size = length(conflict), 
                                        replace = TRUE, 
                                        prob = probs)
  
  table(test$exposure, test$pathway, useNA="always") # Checking that all 'Yes' exposures are now not 'No exposure' (confirmed)
  
  # Apply to all 500 datasets within MIDS500 (and then convert back to new MIDS500_clean)
  MIDS500_post <- lapply(1:500, function(i) {
    
    TEMP <- complete(MIDS500, action = i)
    
    conflict <- which(TEMP$exposure != "No exposure" & 
                        TEMP$pathway == "No exposure")
    
    TEMP$pathway[conflict] <- sample(pathways, 
                                     size = length(conflict), 
                                     replace = TRUE, 
                                     prob = probs)
    
    TEMP$.imp <- i
    TEMP$.id  <- seq_len(nrow(TEMP))
    
    return(TEMP)
  })
  
  original <- complete(MIDS500, action = 0) # Need to bind in the original data in order to convert back to MIDS
  original$.imp <- 0
  original$.id  <- seq_len(nrow(original))
  
  all_data <- rbind(original, do.call(rbind, MIDS500_post))
  MIDS500_clean <- as.mids(all_data)
  
  saveRDS(MIDS500_clean, file = "MICE/MIDS500_clean.rds")
} else{ MIDS500_clean <- readRDS(file = "MICE/MIDS500_clean.rds") # Load in existing 
}
  
######################### REGRESSION ANALYSES (IMPUTED DATA) #########################
#### MULTINOMIAL REGRESSION MODEL ####
# Test with one dataset
test1 <- complete(MIDS500_clean, action = 1)
model1 <- multinom(pathway ~ gender + yeargroup + ethnicity + hungry +
                            bullying + loneliness + aggression + coercion +
                            anxiety_scaled + depression_scaled, data = test1,
                          model = T, maxit = 500)

summary(model1, conf.int = TRUE, exponentiate = TRUE)

# Calculate McFadden's R^2 (requested by reviewer (R1)) - use first imputed dataset as indicative
null_model <- multinom(pathway ~ 1, data = test1)
1 - (logLik(model1) / logLik(null_model))

# Run with all 500 imputed datasets + pool results using Rubin's rules 
model <- with(MIDS500_clean, multinom(pathway ~ gender + yeargroup + ethnicity + hungry +
                                          bullying + loneliness + aggression + coercion +
                                          anxiety_scaled + depression_scaled,
                                        model = T, maxit= 500))

pooled <- pool(model)

summary(pooled, conf.int = TRUE, exponentiate = TRUE)

#### TABLE S2: Full regression output ####
results <- tidy(pooled, conf.int = TRUE, exponentiate = TRUE)
results$pathway <- pooled$pooled$y.level

results <- results %>%
  mutate(
    p.value = sprintf("%.3f", p.value),
    across(c(estimate, conf.low, conf.high), ~ sprintf("%.2f", .x))
  )

results[["Coefficient [95% CI]"]] <-
  paste0(
    results$estimate, " [", results$conf.low,", ", results$conf.high, "]"
  )

write_xlsx(results, "R1/Regression output/Findings - MI.xlsx")

#### TABLE S3: Predicted probabilities  ####
# Predicted probabilities to facilitate interpretation 
# Can't calculate pred probs for a MIPO, so will use an exemplar dataset (first one) instead (from above)

preds1 <- attr(terms(model1), "term.labels")
results_list <- list()

for(p in preds1){
  x <- df[[p]]
  
  # Hold other variables constant - use mean for dep/anx (i.e. 0, as scaled variable) & reference level of factors
  at_list <- list()
  for(q in preds1[preds1 != p]) {
    others <- df[[q]]
    at_list[[q]] <- if(is.numeric(others)) mean(others, na.rm = TRUE) else levels(as.factor(others))[1] 
  }
  
  # For dep/anx, calculate pred probs for values of 0 (= mean) and +/- 1 (= +/- 1sd), 
  # otherwise evaluate all factor levels (and throw error message if neither numeric nor factor)
  focal_vals <- if (is.numeric(x)) c(-1,0,1) else if (is.factor(x)) (levels(x)) else stop("Error in col:", p)
  
  # set focal values (list of values you want to evaluate probability at.)
  at_list[[p]] <- focal_vals
  
  emm <- emmeans(model1, as.formula(paste0("~ pathway | ", p)),
                 type = "response", at = at_list, infer = TRUE)
  df1 <- as.data.frame(emm) 
  
  df1$final_results <- sprintf("%.2f [%.2f, %.2f]",
                               df1$prob,
                               df1$lower.CL,
                               df1$upper.CL)
  
  results_list[[p]] <- df1
  
}

fulltable <- do.call(rbind, results_list)
rownames(fulltable) <- NULL

write_xlsx(fulltable, "R1/Regression output/Predicted probabilities - MI action =1.xlsx")

#### FIGURE 1. Plotted regression coefficients ####
raw <- summary(pooled, conf.int = TRUE, exponentiate = TRUE)
raw$y.level <- pooled$pooled$y.level

df_plot <- raw %>%
  transmute(
    Predictor = term,
    aOR  = estimate,
    CI_L = `2.5 %`,
    CI_U = `97.5 %`,
    Outcome = y.level
  )

df_plot <- df_plot %>%
  mutate(
    predictor_label = Predictor %>%
      str_replace("gender", "Gender: ") %>%
      str_replace("yeargroup", "Year group: ") %>%
      str_replace("ethnicity", "Ethnicity: ") %>%
      str_replace("hungry", "Hungry: ") %>%
      str_replace("bullying", "Bullying: ") %>%
      str_replace("loneliness", "Loneliness: ") %>%
      str_replace("aggression", "Online aggression: ") %>%
      str_replace("coercion", "Online coercion: ") %>%
      str_replace("anxiety_scaled", "Anxiety (scaled RCADS subscale score)") %>%
      str_replace("depression_scaled", "Depression (scaled RCADS subscale score)")
  )

df_plot <- df_plot %>%
  mutate(
    var_group = case_when(
      str_detect(Predictor, "gender") ~ "Gender",
      str_detect(Predictor, "yeargroup") ~ "Year group",
      str_detect(Predictor, "ethnicity") ~ "Ethnicity",
      str_detect(Predictor, "hungry") ~ "Hungry",
      str_detect(Predictor, "bullying") ~ "Bullying",
      str_detect(Predictor, "loneliness") ~ "Loneliness",
      str_detect(Predictor, "aggression") ~ "Online aggression",
      str_detect(Predictor, "coercion") ~ "Online coercion",
      str_detect(Predictor, "anxiety_scaled") ~ "Anxiety",
      str_detect(Predictor, "depression_scaled") ~ "Depression",
      TRUE ~ "Other"
    )
  )

df_plot <- df_plot %>% filter(Predictor != "(Intercept)")

group_order <- c("Gender","Year group","Ethnicity", "Hungry", "Bullying","Loneliness",
                 "Online aggression","Online coercion","Anxiety","Depression","Other")

df_plot$var_group <- factor(df_plot$var_group, levels = group_order)

df_plot$predictor_label <- factor(df_plot$predictor_label, levels = rev(unique(df_plot$predictor_label)))
df_plot$Outcome <- factor(df_plot$Outcome, levels = c("Passive-only", "Active-searched", "Active-received", "Mixed", "Other-only"))

## Spacing between variables
ordered <- df_plot %>%
  distinct(predictor_label) %>%
  pull(predictor_label) %>%
  as.character()

counts  <- c(4,6,4,2,4,2,2,2)

new <- character(); i <- 1; s <- 1
for (c in counts) {
  end <- min(i + c - 1, length(ordered))
  if (i <= end) { new <- c(new, ordered[i:end]); i <- end + 1 }
  if (i <= length(ordered)) { new <- c(new, paste0("SP", s)); s <- s + 1 }
}
if (i <= length(ordered)) new <- c(new, ordered[i:length(ordered)])

spacers <- grep("^SP", new, value = TRUE)
spacer_rows <- expand.grid(predictor_label = spacers, Outcome = levels(df_plot$Outcome), stringsAsFactors = FALSE) %>%
  mutate(Predictor = NA, aOR = NA, CI_L = NA, CI_U = NA, var_group = "Spacer") %>%
  select(Predictor, aOR, CI_L, CI_U, predictor_label, var_group, Outcome)

df_plot <- bind_rows(df_plot %>% mutate(predictor_label = as.character(predictor_label)), spacer_rows)
df_plot$predictor_label <- factor(df_plot$predictor_label, levels = rev(new))

df_plot$Outcome <- factor(
  df_plot$Outcome,
  levels = c("Passive-only", "Active-searched", "Active-received", "Mixed", "Other-only")
)

p <- ggplot(df_plot, aes(x = aOR, y = predictor_label, colour = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 1, linetype = "solid", colour = "grey70") +
  geom_errorbar(aes(xmin = CI_L, xmax = CI_U),
                width = 0.18,
                orientation = "y",
                position = position_dodge(width = 0.6),
                linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 2, position = position_dodge(width = 0.6), na.rm = TRUE) +
  scale_colour_manual(values = pal, name = "Pathway") +
  scale_shape_manual(values = shape_map, name = "Pathway") +
  scale_x_log10(limits = c(.25, 8.3),
                breaks = c(.25, 0.5, 1, 2, 4, 8),
                labels = c(".25",".5","1","2","4","8")) +
  labs(title = "Adjusted odds ratios from multinomial model",
       x = "Adjusted odds ratio (95% CI)",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_grid(. ~ Outcome, switch = "x", space = "free_x", scales = "free_x") +
  scale_y_discrete(labels = function(l) ifelse(grepl("^SP", l), "", l)) +
  theme(panel.spacing.x = unit(1.2, "cm"))

p
ggsave (p, filename="R1/Plots/Figure 1 - MI.png", width = 15, height = 10, dpi = 600 )
