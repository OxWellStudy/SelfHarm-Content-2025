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

######################### DATA IMPORT ######################### 
# Data are imported using cyphr:
if ( 0 ) {
  OXWELL.DATA.DIR <- "" # Path to OxWell data
  PERSONAL.SSH.DIR <- "" # Path to personal SSH key (for decrypting OxWell data)
  PERSONAL.DIR <- "" # Path to personal wd 
} else {
  source("local.R")
}

KEY <- cyphr::data_key(path_data=OXWELL.DATA.DIR,path_user=PERSONAL.SSH.DIR)
DATA <- cyphr::decrypt_object(key=KEY, data=file.path(OXWELL.DATA.DIR, "DATA2025/OXWELL2025.R13-analysis.rds-cyphr") )
setwd(PERSONAL.DIR)

#########################  DATA PREP  #########################
# N.B. Exact wording for all variables can be found in the variable guides on the OSF (osf.io/sekhr)

# Subset to (1) those who had responses for the core survey & then (2) to those who likely reached the question on 
  # exposure to self-harm content (N.B. this is the latest survey question used in the analyses)

df <- DATA[which(DATA$SURVEY.CORE == TRUE & DATA$X2370!= "NoResponse(Stopped_vB)"), ]

table(df$X2370) # Including item-level non-response = likely saw question, didn't answer
df %>% 
  count(X2370) %>%
  mutate(percent = n / sum(n) * 100)

df <- df[which(df$X2370 != "NoResponse(Item)")] # Excluding item-level non-response
df %>% 
  count(X2370) %>%
  mutate(percent = n / sum(n) * 100)

# Re-categorise ethnicity according to ONS 6a (Arab -> Other ethnic group)
df$gender <- df$OXWELL.GENDER.TGD
df$yeargroup <- df$OXWELL.YEARGROUP

df$ethnicity <- df$X1040
df$ethnicity[df$ethnicity == "Arab"] <- "Other Ethnic Group"

df$loneliness <- df$X1780 # 'How often do you feel lonely?'
df$bullying <- df$X1425 # 'How often have you been bullied at school…'
df$aggression <- df$X2800 # 'I have received threatening or aggressive messages online'
df$coercion <- df$X2870 # 'I have been pressured to do something online I didn’t want to do'

df$anxiety <- df$RCADS.ANXIETY.IMPUTEtscore 
df$depression <- df$RCADS.DEPRESSION.IMPUTEtscore 

######################### DESCRIPTIVES  #########################
# FIRST: Examine characteristics by (1) whether or not they were exposed to self-harm content
  # & (2) how frequently (N.B. in OxWell this is a single question)

# Generate 'exposure' factor variable w/ ref: No exposure
df[, exposure := fcase(
  X2370 == "No, I haven’t come across self-harm content in the past month", "No exposure",
  X2370 == "Yes, once or twice", "Once or twice",
  X2370 == "Yes, a few times", "A few times",
  X2370 == "Yes, several times", "Several times"
)]

df[, exposure := factor(exposure,
                        levels = c("No exposure","Once or twice","A few times","Several times"))]

#### TABLE 1 Part 1: Generate tables for overall descriptives + by exposure ####
# Overall sample descriptives
table <- compareGroups(~ gender + yeargroup + ethnicity + loneliness + bullying + 
                         aggression + coercion + anxiety + depression,
                       data = df,
                       byrow = TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)
write_xlsx(output, "Tables/Full sample.xlsx")

# Sample descriptives by exposure
table <- compareGroups(exposure ~  gender + yeargroup + ethnicity + loneliness + bullying + 
                         aggression + coercion + anxiety + depression,
                       data = df,
                       byrow = TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)
write_xlsx(output, "Tables/By exposure freq.xlsx")

# For those who report exposure, examine characteristics by exposure pathway (generated from
  # tickbox questions on how they came across the content)

## Generate n reporting each mode of exposure
df$search <- df$X2371a
df$feed <- df$X2371b
df$accident <- df$X2371c
df$embedded <- df$X2371d
df$shared <- df$X2371e
df$other <- df$X2371f

# Categorise as follows: 
 # No pathway reported = ticked none - exclude from counts
 # Passive only = ticked one or more of feed, accident, embedded ONLY
 # Active search only = ticked search ONLY 
 # Active receive only = ticked shared ONLY 
 # Other only = ticked other ONLY 
 # Mixed = all other combos

df[exposure == "No exposure", pathway := "No exposure"]

df[exposure != "No exposure",
   pathway := fcase(
     rowSums(.SD == "Ticked (Yes)") == 0, "No pathway reported",
     search == "Ticked (Yes)" & rowSums(.SD == "Ticked (Yes)") == 1, "Active-searched",
     shared == "Ticked (Yes)" & rowSums(.SD == "Ticked (Yes)") == 1, "Active-received",
     other  == "Ticked (Yes)" & rowSums(.SD == "Ticked (Yes)") == 1, "Other-only",
     (feed == "Ticked (Yes)" | accident == "Ticked (Yes)" | embedded == "Ticked (Yes)") & 
       !(search == "Ticked (Yes)" | shared == "Ticked (Yes)" | other == "Ticked (Yes)"), "Passive-only",
     default = "Mixed"
     ), .SDcols = c("search","feed","accident","embedded","shared","other")]

table(df$pathway)

df$pathway <- factor(df$pathway, levels=c("No exposure", "No pathway reported", "Passive-only", 
                                          "Active-searched", "Active-received", "Mixed", "Other-only"))

# n (%) by pathway 
df %>%
  filter(pathway != "No pathway reported" & pathway != "No exposure") %>%
  count(pathway) %>%
  mutate(percent = n / sum(n) * 100)

#### TABLE 2: Descriptives by exposure pathway, barring those with no reported pathway ####
df_t2 <- subset(df, pathway != "No pathway reported" & pathway != "No exposure")

df_t2 %>%
  pivot_longer(
    cols = c(search, feed, accident, embedded, shared, other),
    names_to = "mode",
    values_to = "response"
  ) %>%
  filter(response == "Ticked (Yes)") %>%
  count(mode) %>%
  mutate(percent = n / nrow(df_t2) * 100)

#### TABLE 1 Part 2: Descripties by pathway ####
# Descrtiptives by pathway
table <- compareGroups(pathway ~  gender + yeargroup + ethnicity + loneliness + bullying + 
                         aggression + coercion + anxiety + depression,
                       data = df_t2,
                       byrow = TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)
write_xlsx(output, "Tables/By pathway.xlsx")

#### TABLE S1: Descriptives by pathway and frequency ####
table <- compareGroups(exposure ~  pathway,
                       data = df,
                       byrow = TRUE)
table<-createTable(table,show.ratio = TRUE)
output <- as.data.frame(table$descr)
output<-cbind(" "=rownames(output), output)
write_xlsx(output, "Tables/Pathway & frequency.xlsx")

######################### REGRESSION ANALYSES  #########################
# Remove participants without a pathway and with the 'only-other' pathway (not interpretable)
df_reg <- df[df$pathway != "No pathway reported" & df$pathway != "Other-only"]
df_reg$pathway <- droplevels(df_reg$pathway)

# For analysis, change item-level non-response to NA & restrict to complete cases
df_reg <- df_reg %>%
  mutate(across(
    c(gender, yeargroup, ethnicity, bullying, loneliness, aggression, coercion),
    ~ na_if(as.character(.), "NoResponse(Item)"))) %>%
  filter(
    !if_any(
      c(gender, yeargroup, ethnicity, bullying,
        loneliness, aggression, coercion,
        anxiety, depression),
      is.na))

# Re-level variables to ensure accurate reference levels
df_reg$yeargroup <- as.factor(df_reg$yeargroup)

df_reg$gender <- factor(df_reg$gender, levels=c("Boy / Man", "Girl / Woman", "Trans/Gender Diverse", 
                                                   "Don't know / Not sure", "Prefer not to say"))

df_reg$ethnicity <- factor(df_reg$ethnicity, levels=c("White", "Asian/Asian British", "Black/African/Caribbean/Black British", 
                                                      "Mixed/Multiple Ethnic Groups", "Other Ethnic Group"))

df_reg$bullying <- factor(df_reg$bullying, levels=c("I have not been bullied in the past couple of months", "Once, twice or a few times",
                                                      "2 or 3 times a month", "About once a week", "Several times a week"))

df_reg$loneliness <- factor(df_reg$loneliness, levels= c("Hardly ever or never", "Some of the time", "Often"))

df_reg$aggression <- factor (df_reg$aggression, levels = c("No", "Yes", "Prefer not to say"))

df_reg$coercion <- factor (df_reg$coercion, levels = c("No", "Yes", "Prefer not to say"))

df_reg$anxiety_scaled <- scale(df_reg$anxiety)

df_reg$depression_scaled <- scale(df_reg$depression)

#### MULTINOMIAL REGRESSION MODEL ####
model <- multinom(pathway ~ gender + yeargroup + ethnicity + bullying + 
                    loneliness + aggression + coercion + anxiety_scaled + depression_scaled,
          data = df_reg)

#### TABLE S2: Full regression output ####
results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

results[] <- lapply(results, function(x) {######THIS HAS SOMEHOW BROKEN _ CHECK ######
  if (is.numeric(x)) sprintf("%.2f", x) else x
})

results[["Coefficient [95% CI]"]] <-
  paste0(
    results$Coefficient, " [", results$conf.low,", ", results$conf.high, "]"
  )

write_xlsx(results, "Regression output/Findings (final).xlsx")

#### TABLE S3: Predicted probabilities  ####
## Predicted probabilities to facilitate interpretation 
# First, two more 'manual', explicit examples, then looped over for efficiency 

predprob_gender <-emmeans(object = model, 
               specs = ~ pathway | gender, 
               at = list(yeargroup = "Y07", 
                   ethnicity = "White", 
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
  x <- df_reg[[p]]
  
  # Hold other variables constant - use mean for dep/anx (i.e. 0, as scaled variable) & reference level of factors
  at_list <- list()
  for(q in preds[preds != p]) {
    others <- df_reg[[q]]
    at_list[[q]] <- if(is.numeric(others)) mean(others) else levels(as.factor(others))[1] 
  }
  
  # For dep/anx, calculate pred pros for values of 0 (= mean) and +/- 1 (= mean +/- 1sd), 
    # otherwise evaluate all factor levels (and throw error message if neither numeric nor factor)
  focal_vals <- if (is.numeric(x)) c(-1,0,1) else if (is.factor(x)) (levels(x)) else stop("Error in col:", p)
  
  # set focal values (list of values you want to evaluate probability at.)
  at_list[[p]] <- focal_vals
  
  emm <- emmeans(model, as.formula(paste0("~ pathway | ", p)),
                 type = "response", at = at_list, infer = TRUE)
  df <- as.data.frame(emm) 
  
  df$final_results <- sprintf("%.2f [%.2f, %.2f]",
                  df$prob,
                  df$lower.CL,
                  df$upper.CL)
  
  results_list[[p]] <- df

}

fulltable <- do.call(rbind, results_list)
rownames(fulltable) <- NULL
write_xlsx(fulltable, "Regression output/Predicted probabilities (final).xlsx")

##### FIGURE 1: Plotted regression coefficients  #####
pal <- c("Passive-only" = "#377eb8", "Active-searched" = "#e41a1c",
         "Active-received" = "#ff7f00", "Mixed" = "#4daf4a")
shape_map <- c("Passive-only" = 16, "Active-searched" = 17,
               "Active-received" = 15, "Mixed" = 18)

raw <- read_xlsx("/Users/emmasoneson/Dropbox/Oxford/OxWell/Analyses/Algorithms - HB code check/Regression output/Findings (final).xlsx")

df_plot <- raw %>%
  transmute(
    Predictor = .data[["term"]],
    aOR = as.numeric(.data[["estimate"]]),
    CI_L = as.numeric(.data[["conf.low"]]),
    CI_U = as.numeric(.data[["conf.high"]]),
    Outcome = as.character(.data[["y.level"]])
  )

# Relabel predictors
df_plot <- df_plot %>%
  mutate(
    predictor_label = Predictor %>%
      str_replace("gender", "Gender: ") %>%
      str_replace("yeargroup", "Year group: ") %>%
      str_replace("ethnicity", "Ethnicity: ") %>%
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

group_order <- c("Gender","Year group","Ethnicity","Bullying","Loneliness",
                 "Online aggression","Online coercion","Anxiety","Depression","Other")

df_plot$var_group <- factor(df_plot$var_group, levels = group_order)

df_plot$predictor_label <- factor(df_plot$predictor_label, levels = rev(unique(df_plot$predictor_label))) 
df_plot$Outcome <- factor(df_plot$Outcome, levels = c("Passive-only", "Active-searched", "Active-received", "Mixed"))


## Prepare to add spacing between variables
ordered <- df_plot %>% 
  distinct(predictor_label) %>% 
  pull(predictor_label) %>% 
  as.character()

counts  <- c(4,6,4,4,2,2,2) # Could not get spaces in nicely so have to do it manually...

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
  levels = c("Passive-only", "Active-searched", "Active-received", "Mixed")
)

p <- ggplot(df_plot, aes(x = aOR, y = predictor_label, colour = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 1, linetype = "solid", colour = "grey70") +
  geom_errorbarh(aes(xmin = CI_L, xmax = CI_U),
                 height = 0.18,
                 position = position_dodge(width = 0.6),
                 size = 0.7, na.rm = TRUE) +
  geom_point(size = 2, position = position_dodge(width = 0.6), na.rm = TRUE) +
  scale_colour_manual(values = pal, name = "Pathway") +
  scale_shape_manual(values = shape_map, name = "Pathway") +
  scale_x_log10(limits = c(.125, 8.3),
                breaks = c(.125, .25, 0.5, 1, 2, 4, 8),
                labels = c(".125", ".25",".5","1","2","4","8")) +
  labs(title = "Figure 1. Adjusted odds ratios from multinomial model",
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
ggsave (p, filename="Figures/Figure 1 (final).jpg", width = 15, height = 10, dpi = 600 )
