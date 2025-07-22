### GPT social decision-making
### Analysis for decision scenarios
### Programmed by Feng XIAO (2025.7.20)
### This R script requires one excel file: 'scenario_study1'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Proportion z-test
z.prop = function(x1, x2, n1, n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1 + x2) / (n1 + n2)
  denominator = sqrt(p.common * (1 - p.common) * (1/n1 + 1/n2))
  z.prop.ris = numerator / denominator
  p_value = 2 * (1 - pnorm(abs(z.prop.ris)))
  margin_of_error = qnorm(0.975) * denominator
  ci_lower = z.prop.ris - margin_of_error
  ci_upper = z.prop.ris + margin_of_error
  return(list(z = z.prop.ris, p = p_value, ci_low = ci_lower, ci_up = ci_upper))
}

### Analysis
########################################################################################################
## Humans
# Human lives: 6, 60, 600, 6000 lives
HL_human <- read_excel('scenario_study1.xlsx', sheet = 'HL_human', na = '---')
summary_data <- HL_human %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
  #Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
  #Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_Human_ScenarioHL.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_Human_PairwiseHL.xlsx',
           rowNames = FALSE)

# Public properties: 6, 60, 600, 6000 paintings
PP_human <- read_excel('scenario_study1.xlsx', sheet = 'PP_human', na = '---')
summary_data <- PP_human %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_Human_ScenarioPP.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_Human_PairwisePP.xlsx',
           rowNames = FALSE)

# Personal stock shares: 6, 60, 600, 6000 dollars
PS_human <- read_excel('scenario_study1.xlsx', sheet = 'PS_human', na = '---')
summary_data <- PS_human %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_Human_ScenarioPS.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_Human_PairwisePS.xlsx',
           rowNames = FALSE)
########################################################################################################
## GPT-3.5
# Human lives: 6, 60, 600, 6000 lives
HL_gpt3.5 <- read_excel('scenario_study1.xlsx', sheet = 'HL_gpt3.5', na = '---')
summary_data <- HL_gpt3.5 %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_GPT3.5_ScenarioHL.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_GPT3.5_PairwiseHL.xlsx',
           rowNames = FALSE)

# Public properties: 6, 60, 600, 6000 paintings
PP_gpt3.5 <- read_excel('scenario_study1.xlsx', sheet = 'PP_gpt3.5', na = '---')
summary_data <- PP_gpt3.5 %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_GPT3.5_ScenarioPP.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_GPT3.5_PairwisePP.xlsx',
           rowNames = FALSE)

# Personal stock shares: 6, 60, 600, 6000 dollars
PS_gpt3.5 <- read_excel('scenario_study1.xlsx', sheet = 'PS_gpt3.5', na = '---')
summary_data <- PS_gpt3.5 %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_GPT3.5_ScenarioPS.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_GPT3.5_PairwisePS.xlsx',
           rowNames = FALSE)
########################################################################################################
## GPT-4
# Human lives: 6, 60, 600, 6000 lives
HL_gpt4 <- read_excel('scenario_study1.xlsx', sheet = 'HL_gpt4', na = '---')
summary_data <- HL_gpt4 %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_GPT4_ScenarioHL.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_GPT4_PairwiseHL.xlsx',
           rowNames = FALSE)

# Public properties: 6, 60, 600, 6000 paintings
PP_gpt4 <- read_excel('scenario_study1.xlsx', sheet = 'PP_gpt4', na = '---')
summary_data <- PP_gpt4 %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_GPT4_ScenarioPP.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_GPT4_PairwisePP.xlsx',
           rowNames = FALSE)

# Personal stock shares: 6, 60, 600, 6000 dollars
PS_gpt4 <- read_excel('scenario_study1.xlsx', sheet = 'PS_gpt4', na = '---')
summary_data <- PS_gpt4 %>%
  group_by(Group_size) %>%
  summarise(
    Count_b = sum(Option == "b"), #risk-seeking
    Total = n(),
    .groups = 'drop'
  )
binom_results <- summary_data %>%
  rowwise() %>%
  mutate(
    z_test = list(z.prop(Count_b, Total / 2, Total, Total)),
    z = z_test$z,
    p_value = z_test$p,
    ci_low = z_test$ci_low,
    ci_up = z_test$ci_up
  ) %>%
  select(Group_size, Count_b, Total, z, p_value, ci_low, ci_up) #comparisons with 50-50 for each scenario
group_pairs <- combn(unique(summary_data$Group_size), 2, simplify = FALSE)
pairwise_results <- lapply(group_pairs, function(pair) {
  data1 <- summary_data %>% filter(Group_size == pair[1])
  data2 <- summary_data %>% filter(Group_size == pair[2])
  
  res <- z.prop(data1$Count_b, data2$Count_b, data1$Total, data2$Total)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    z = res$z,
    p_value = res$p,
    ci_low = res$ci_low,
    ci_up = res$ci_up
  )
}) %>%
  bind_rows()

binom_results <- binom_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (vs. 50-50)

pairwise_results <- pairwise_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
#Benjamini每Hochberg procedure for multiple comparison corrections (pairwise)

write.xlsx(binom_results,
           file = 'Outputs_study1/WithinAnalysis_GPT4_ScenarioPS.xlsx',
           rowNames = FALSE)
write.xlsx(pairwise_results,
           file = 'Outputs_study1/WithinAnalysis_GPT4_PairwisePS.xlsx',
           rowNames = FALSE)