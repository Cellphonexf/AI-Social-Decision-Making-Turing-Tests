### GPT social decision-making
### Visualization: within-group analysis for replication
### Programmed by Feng XIAO (2025.7.20)
### This R script requires one excel file: 'rawdata_replication'

### Preparation
package_list <- c('tidyr','dplyr','readxl', 'ggplot2','scales','cowplot',
                  'reshape2','ggbreak','patchwork','gridExtra')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Visualization for within-group analysis
## Replication with GPT-4o
# Human lives
rd1 <- read_excel('rawdata_replication.xlsx', sheet = 'Human lives', na = '---')
rd1$frame <- factor(rd1$frame, levels = c('6 lives','60 lives',
                                          '600 lives','6000 lives'))
p1a<-ggplot(data=rd1, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p1c<-ggplot(data=rd1, aes(x=frame, y=gpt4o, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4o,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4o), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4o') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p1 <- grid.arrange(p1a, p1c, ncol = 1)
# Kinship group
rd2 <- read_excel('rawdata_replication.xlsx', sheet = 'Kinship group', na = '---')
rd2$frame <- factor(rd2$frame, levels = c('1 kin','2 kin','3 kin','6 kin'))
p2a<-ggplot(data=rd2, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p2c<-ggplot(data=rd2, aes(x=frame, y=gpt4o, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4o,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4o), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4o') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p2 <- grid.arrange(p2a, p2c, ncol = 1)

p_replic <- grid.arrange(p1, p2, ncol = 2)
ggsave('WithinViso3.pdf', plot = p_replic, width = 2.8, height = 2.2)