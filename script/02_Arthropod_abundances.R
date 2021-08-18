#----------------------------------------------------------#
#
#
#          BBC - Bird/bat/control exclosures in PNG
#
#
#                    Sreekar Rachakonda
#                         2020
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 2.1 BBC effect of treatment on arthropod abundances   ----
#----------------------------------------------------------#

dataset_arth <-  
  readxl::read_xlsx("data/input/Arthropods_abundances_BBC_PNG.xlsx")

# set the structure of the table
dataset_arthfull_BBC$Patrol<-factor(dataset_arthfull_BBC$Patrol) 
dataset_arthfull_BBC$TotalAbundance<-as.integer(dataset_arthfull_BBC$TotalAbundance)
dataset_arthfull_BBC$Treatment<-factor(dataset_arthfull_BBC$Treatment) 
dataset_arthfull_BBC$Elev<-as.factor(dataset_arthfull_BBC$Elev) 
dataset_arthfull_BBC$Plant_name<-as.factor(dataset_arthfull_BBC$Plant_name)
summary(dataset_arthfull_BBC)

# reorganize the dataset
dataset_arthfull_BBC$Treatment <- ordered(dataset_arthfull_BBC$Treatment, levels=c("CN2", "BAT", "BIR"))
dataset_arthfull_BBC_model <-
  dataset_arthfull_BBC %>%
  dplyr::select(Elev, Treatment, Plant_name, TotalAbundance) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor)
summary (dataset_arthfull_BBC_model)

# run the model
glm_arthfull_BBC_model<-glmmTMB(TotalAbundance~Treatment * Elev+(1|Plant_name),dataset_arthfull_BBC_model,family = nbinom2())
Anova(glm_arthfull_BBC_model)

# select the best model
glm_arthfull_BBC_model_dd <- 
  MuMIn::dredge(glm_arthfull_BBC_model,
                trace = TRUE)

# view the results
glm_arthfull_BBC_model_dd %>% 
  as_tibble() %>% 
  filter(delta <2 ) %>% 
  View()

# glm1Tx is better
glm_arthfull_BBC_select <-glmmTMB(TotalAbundance~Treatment +(1|Plant_name),dataset_arthfull_BBC_model,family = nbinom2())

# see the results of the original table 
Anova(glm_arthfull_BBC_select)
plotResiduals(glm_arthfull_BBC_select)
summary(glm_arthfull_BBC_select)

#--------------------------------------------------------------------------------------------------------------------------------------#
# 2.2 plot the results of the model (it is not really coorect, as the elevation is significantt but just to show)   ----
#--------------------------------------------------------------------------------------------------------------------------------------#
# 
glm_arthfull_BBC_show <-glmmTMB(TotalAbundance~Treatment * Elev +(1|Plant_name),dataset_arthfull_BBC_model,family = nbinom2())
(x1<-plot_model(glm_arthfull_BBC_show ,'emm',terms = c('Elev','Treatment'),dodge = 150) + xlab('Elevation (m)') + ylab('Arthropod abundance (indiv/m2)') + ggtitle('') + 
    theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 22)))


#--------------------------------------------------------------------------------------------------------------------------------------#
# 2.3 plot the same thing in ggplot   ----
#--------------------------------------------------------------------------------------------------------------------------------------#
glm_arthfull_BBC_show_emmeans <-
  emmeans(
    glm_arthfull_BBC_show,
    pairwise ~ Elev*Treatment,
    type = "response")

(model_plot_05 <- 
    glm_arthfull_BBC_show_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x=Elev,
        y = response,
        col = Treatment,
        fill= Treatment)) + 
    
    geom_point(
      data = dataset_arthfull_BBC_model,
      aes(y = TotalAbundance),
      alpha = 0.3,
      size = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.1)) +
    
    ylim(0,60) +
    scale_x_discrete(limits=c("200", "700", "1200", "1700", "2200", "2700")) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.3,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 2)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 4) +
    
    labs(
      x = "Elevation (m)",
      y = expression(paste("Invertebrates abundance per m" ^ 2)) )+
    scale_fill_manual(values = c("#4DAF4A","#A65628", "#377EB8" ))+
    scale_color_manual(values = c("#4DAF4A", "#A65628", "#377EB8"))+
    theme(
      text = element_text(size = text_size),
      axis.text=element_text(color="black"), 
      legend.position = "top")) +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))

display.brewer.pal(n = 8, name = 'Set1')
brewer.pal(n = 8, name = "Set1")

# save pdf
ggsave(
  "fig/Fig_3a_BBC_model.pdf",
  model_plot_05,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# plot the correct figure where significant treatments are shown only
glm_arthfull_BBC_select_emmeans <-
  emmeans(
    glm_arthfull_BBC_select,
    pairwise ~ Treatment,
    type = "response")

(model_plot_05b <- 
    glm_arthfull_BBC_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x=Treatment,
        y = response)) + 
    
    ylim(0,60) +
   
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.3,
      size = 1.2)+
    
    geom_point(
      shape = 0,
      size = 4) +
    
    labs(
      x = "Treatment",
      y = expression(paste("Invertebrates abundance per m" ^ 2))) +
    theme(
      text = element_text(size = text_size),
      axis.text=element_text(color="black"), 
      legend.position = "top")) +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))

# save pdf
ggsave(
  "fig/Fig_3b_BBC_model.pdf",
  model_plot_05b,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_arthfull_BBC_select_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/Fig_3_BBC_pairwise_contrast.csv")

glm_arthfull_BBC_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/Fig_3_BBC_emmeans.csv")
