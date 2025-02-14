### GPT social decision-making
### Visualization: within-group analysis for Study 2
### Programmed by Feng XIAO (2024.12.6)
### This R script requires one excel file: 'Plotting_WithinGroup'

### Preparation
package_list <- c('tidyr','dplyr','readxl', 'ggplot2','scales','cowplot',
                  'reshape2','ggbreak','patchwork','gridExtra')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Visualization for within-group analysis
## Study 2: decision premises
# Sense of fairness
rd1 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Sense of fairness', na = '---')
rd1$frame <- factor(rd1$frame, levels = c('6 lives','600 lives'))
p1a<-ggplot(data=rd1, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
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
p1b<-ggplot(data=rd1, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
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
p1c<-ggplot(data=rd1, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p1 <- grid.arrange(p1a, p1b, p1c, ncol = 1)
# Age1: YSO-6 kin vs. YSY-6 kin
rd2 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Age1', na = '---')
rd2$frame <- factor(rd2$frame, levels = c('YSO-6 kin','YSY-6 kin'))
p2a<-ggplot(data=rd2, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
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
p2b<-ggplot(data=rd2, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
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
p2c<-ggplot(data=rd2, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p2 <- grid.arrange(p2a, p2b, p2c, ncol = 1)
# Age2: MSO-6 kin vs. MSY-6 kin
rd3 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Age2', na = '---')
rd3$frame <- factor(rd3$frame, levels = c('MSO-6 kin','MSY-6 kin'))
p3a<-ggplot(data=rd3, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
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
p3b<-ggplot(data=rd3, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
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
p3c<-ggplot(data=rd3, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p3 <- grid.arrange(p3a, p3b, p3c, ncol = 1)
# Group-dependent goal settings (inequal EV)
rd4 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Goal setting1', na = '---')
rd4$frame <- factor(rd4$frame, levels = c('6 kin','6 lives','600 lives'))
p4a<-ggplot(data=rd4, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
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
p4b<-ggplot(data=rd4, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
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
p4c<-ggplot(data=rd4, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('RA'='#4169E1','RS'='#B22222')) +
  scale_shape_manual(values = c('RA' = 15, 'RS' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p4 <- grid.arrange(p4a, p4b, p4c, ncol = 1)
# Group-dependent goal settings (equal EV)
rd5 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Goal setting2', na = '---')
rd5$frame <- factor(rd5$frame, levels = c('6 kin','6 lives','600 lives'))
p5a<-ggplot(data=rd5, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('one-third'='#4169E1','two-thirds'='#B22222')) +
  scale_shape_manual(values = c('one-third' = 15, 'two-thirds' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
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
p5b<-ggplot(data=rd5, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('one-third'='#4169E1','two-thirds'='#B22222')) +
  scale_shape_manual(values = c('one-third' = 15, 'two-thirds' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
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
p5c<-ggplot(data=rd5, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('one-third'='#4169E1','two-thirds'='#B22222')) +
  scale_shape_manual(values = c('one-third' = 15, 'two-thirds' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p5 <- grid.arrange(p5a, p5b, p5c, ncol = 1)
p_stu2 <- grid.arrange(p1, p2, p3, p4, p5, ncol = 5)
ggsave('WithinViso2.pdf', plot = p_stu2, width = 7, height = 3)
