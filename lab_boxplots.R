################################################################################
################################################################################
##                                                                            ##
##   Sponsor                      : GEM                                       ##
##   Study                        : Test Study                                ##
##                                                                            ##
##   Program name                 : lab_boxplots.R                            ##
##   Description                  : Four boxplots of the ALP, ALT, AST, and   ##                                  
##                                  BILI liver function tests (for poster)    ##
##                                                                            ##
##   Input Dataset(s)             : ADLB.sas7bdat                             ##
##                                                                            ##
##   Program author               : Sophie Shapcott                           ##
##   Date Completed               : 23/07/2021                                ##
##   Version Number               : v1.0                                      ##
##                                                                            ##
##   Maintenance / History        :                                           ##
##                                                                            ##
################################################################################
################################################################################

#####################################
### loading the required packages ###
#####################################

#############################################################################
### package versions used are as follows: tidyverse v1.3.1, haven v2.4.1, ###
### gifski v1.4.3.1, and gganimate v1.0.7                                 ###
#############################################################################
pkgs <- c("tidyverse", "haven", "gifski", "gganimate")
lapply(pkgs, library, character.only=TRUE)

#######################################
### importing the datasets required ###
#######################################
adlb <- read_sas(paste0(Sys.getenv("DATA_PATH"), "\\ADAM\\adlb.sas7bdat"))
head(adlb); dim(adlb)

################################################################################

#########################
### data manipulation ###
#########################

### only want the records post start of treatment ###
positive_data <- adlb[adlb["ADY"]> 0,
                      c("TRT01A", "PARAMCD", "PARAM", "AVAL", "ADY")]

### only want the four liver function tests ###
reqd_data <- filter(positive_data, 
                    PARAMCD %in% c("ALP", "ALT", "AST", "BILI"))

### grouping and arranging the data in order for ggplot to plot correctly ###
final <- arrange(group_by(reqd_data, TRT01A, ADY, PARAMCD, PARAM), ADY)
head(final); dim(final)

### obtaining the mean for each of the 'boxes' ###
data_means <- summarise(final, MEANS=mean(AVAL))
head(data_means)

################################################################################

######################
### theme for plot ###
######################
plot_theme <- theme(text = element_text(family='sans', face='plain'),
                    
                    plot.title = element_text(face='bold', size=7, hjust=0.5),
                    plot.tag = element_text(face='bold', size=6),
                    plot.tag.position = c(0.09, 1),
                    plot.background = element_rect(colour = 'whitesmoke', fill='ghostwhite'),
                    plot.caption = element_text(family = "sans", face = "italic", size = 6, hjust=0),
                    plot.caption.position = "plot",
  
                    panel.background = element_rect(fill='whitesmoke', colour='#B0B7BB'),
                    panel.spacing = unit(0, 'cm'),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
  
                    legend.key = element_rect(fill=NA),
                    legend.key.size = unit(0.02, 'npc'),
                    legend.background = element_rect(colour='whitesmoke'),
                    legend.text = element_text(size=6),
                    legend.title = element_text(size=6),
                    legend.margin = margin(rep(3, 4)),
                    legend.position = 'bottom',
  
                    axis.text = element_text(size=5),
                    axis.title = element_text(size=6),
                    axis.ticks = element_line(colour='#B0B7BB'),
                    
                    strip.background = element_rect(fill="#FAFBFE", colour="#B0B7BB"),
                    strip.text = element_text(size=6))

################################################################################

#######################
### initial boxplot ###
#######################
plot <- ggplot(data=final, 
               aes(as.factor(ADY), AVAL, fill=TRT01A, colour=TRT01A)) + 
  geom_boxplot(notch=TRUE,
               width=0.5,
               alpha=0.5, 
               position=position_dodge(width=0.6), 
               key_glyph=draw_key_rect)  +
  stat_boxplot(geom="errorbar", 
               width=0.2, 
               position=position_dodge(width=0.6), 
               show.legend = FALSE) +
  geom_point(data=data_means, 
             aes(as.factor(ADY), MEANS, fill=TRT01A), 
             shape=21, 
             colour="grey47",
             position=position_dodge(width=0.6), 
             show.legend = FALSE) +
  geom_line(data=data_means, 
            aes(as.factor(ADY), MEANS, group=TRT01A), 
            colour="blue", 
            alpha=0.6,
            position=position_dodge(width=0.6), 
            show.legend = FALSE) + 
  facet_wrap(vars(PARAM))

##############################################################
### additional features, labels, and theme (customisation) ###
##############################################################
static_plot <- plot + 
  scale_y_continuous(name="Analysis Value (Unit)", 
                     limits=c(0, 162), 
                     breaks = seq(0, 160, 20)) + 
  scale_x_discrete(name="Study Day") + 
  scale_fill_manual(values = c("mediumseagreen", "cornflowerblue"), 
                    name="Actual Treatment") +
  scale_colour_manual(values = c("mediumseagreen", "cornflowerblue"), 
                      name="Actual Treatment") + 
  labs(title = "Boxplots of the ALP, ALT, AST, and BILI liver function tests", 
       tag = "GEM Test Study") + 
  plot_theme

###########################################################################
### adding in the animation to the plot, and rendering it to a gif file ###
###########################################################################
animated_plot <- static_plot + transition_reveal(ADY, keep_last = TRUE)

animate(animated_plot, 
        nframes=6, 
        fps=0.5, 
        renderer = gifski_renderer(paste0(Sys.getenv("CLIENT_PATH"), 
                                          "\\Poster-2021\\Outputs\\lab_boxplots_R.gif")), 
        height=3.75, 
        width=5, 
        units="in", 
        res=200)

################################################################################
