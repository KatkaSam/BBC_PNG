#----------------------------------------------------------#
#
#
#    BBC - Bird/bat/control exclosures in PNG
#
#         Sreekar Rachakonda & Katerina Sam 
#                     2020 - 2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 3.1 BBC effect of treatment on plant herbivory        ----
#----------------------------------------------------------#

dataset_herb <-  
  readxl::read_xlsx("data/input/Herbivory_LeafArea_BBC_PNG.xlsx")
summary(dataset_herb)

# set the structure of the table
dataset_herb$Treatment<-factor(dataset_herb$Treatment) 
dataset_herb$Elev<-as.factor(dataset_herb$Elev) 
dataset_herb$Plant_species <-as.factor(dataset_herb$Plant_species )
summary(dataset_herb)

# reorganize the dataset
dataset_herb$Treatment <- ordered(dataset_herb$Treatment, levels=c("CN2", "BAT", "BIR"))
dataset_herb_model <-
  dataset_herb %>%
  dplyr::select(Elev, Treatment, Plant_species, Herbivory) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor)
summary (dataset_herb_model)

# run the model
glm_herb_model<-glmmTMB(Herbivory~Treatment * Elev+(1|Plant_species),dataset_herb_model,family=beta_family(link = 'logit'))
Anova(glm_herb_model)

# select the best model
glm_herb_model_dd <- 
  MuMIn::dredge(glm_herb_model,
                trace = TRUE)

# view the results
glm_herb_model_dd %>% 
  as_tibble() %>% 
  filter(delta <2 ) %>% 
  View()

# make the best model
glm_herb_select <-glmmTMB(Herbivory ~ Treatment + Elev + Treatment:Elev + (1|Plant_species), dataset_herb_model, family=beta_family(link = 'logit'))

# see the results of the original table 
Anova(glm_herb_select)
plotResiduals(glm_herb_select)
summary(glm_herb_select)

#----------------------------------------------------------------------------------------------------------------------------#
# 3.2 plot the results of the model (it is not really correct, as the elevation is significant but just to show)          ----
#----------------------------------------------------------------------------------------------------------------------------#

(x1<-plot_model(glm_herb_select ,'emm',terms = c('Elev','Treatment'),dodge = 150) + xlab('Elevation (m)') + ylab('Herbivory') + ggtitle('') + 
    theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 22)))


#-------------------------------------------------#
# 3.3 plot the same thing in ggplot          ----
#-------------------------------------------------#

glm_herb_select_emmeans <-
  emmeans(
    glm_herb_select,
    pairwise ~ Treatment|Elev,
    type = "response")

(model_plot_09 <- 
    glm_herb_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x=Elev,
        y = response,
        col = Treatment,
        fill= Treatment)) + 
    
    scale_x_discrete(limits=c("200", "700", "1200", "1700", "2200", "2700")) +
    
    geom_point(
      data = dataset_herb_model,
      aes(y = Herbivory),
      alpha = 0.3,
      size = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.1)) +
    
    ylim(0,0.15) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.3,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1.2)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 4) +
    
    labs(
      x = "Elevation (m)",
      y = expression(paste("Herbivory")) )+
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
  "fig/Fig_9_model_herbivory.pdf",
  model_plot_09,
  width = PDF_width,
  height = PDF_height,
  units = "in")


# save the pairwise test 
glm_herb_select_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/Fig_9_BBC__herbivory_pairwise_contrast.csv")

glm_herb_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/Fig_9_BBC__herbivory_emmeans.csv")
