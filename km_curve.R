################################################################################
################################################################################
##                                                                            ##
##   Sponsor                      : GEM                                       ##
##   Study                        : Test Study                                ##
##                                                                            ##
##   Program name                 : km_curve.R                                ##
##   Description                  : A Kaplan-Meier Survival Curve (for poster)##
##                                                                            ##
##                                                                            ##
##   Input Dataset(s)             : ADTTE.sas7bdat                            ##
##                                                                            ##
##   Program author               : Sophie Shapcott                           ##
##   Date Completed               : 06/08/2021                                ##
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
### survival v3.2.11, survminer v0.4.9, and gridExtra v2.3                ###
#############################################################################
pkgs <- c("tidyverse", "haven", "survival", "survminer", "gridExtra")
lapply(pkgs, library, character.only=TRUE)

######################################
### importing the dataset required ###
######################################
adtte <- read_sas(paste0(Sys.getenv("DATA_PATH"), "\\ADAM\\adtte.sas7bdat"))
head(adtte); dim(adtte)

################################################################################

######################
### theme for plot ###
######################
plot_theme <- theme(text = element_text(family='sans', face='plain'),
                    
                    plot.title = element_text(face='bold', size=6, hjust=0.5),
                    plot.tag = element_text(face='bold', size=5),
                    plot.tag.position = "topleft",
                    plot.background = element_rect(colour = 'whitesmoke', fill='ghostwhite'),
                    plot.caption = element_text(family = "sans", face = "italic", size = 4, hjust=0),
                    plot.caption.position = "plot",
                    
                    panel.background = element_rect(fill='whitesmoke', colour=NA),
                    panel.spacing = unit(0, 'cm'),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    
                    legend.key = element_rect(fill=NA),
                    legend.key.size = unit(0.02, 'npc'),
                    legend.background = element_rect(colour='whitesmoke'),
                    legend.text = element_text(size=4),
                    legend.title = element_text(size=4),
                    legend.margin = margin(rep(2, 4)),
                    legend.position = 'bottom',
                    
                    axis.text = element_text(size=4),
                    axis.title = element_text(size=5),
                    axis.ticks = element_line(colour='#B0B7BB'),
                    
                    strip.background = element_rect(fill="#FAFBFE", colour="#B0B7BB"))

################################################################################

##########################################################
### KM computation of estimates and plotting the curve ###
##########################################################

### obtaining the KM estimates ###
km_data <- survfit(Surv(SURVIVAL_TIME, CENSOR)~TRT01P, data = adtte)

### obtaining the plot and first table (risk table) ###
surv_plot <- ggsurvplot(km_data, 
                        data=adtte,
                        palette=c("mediumseagreen", "cornflowerblue"),
                        linetype=c("solid", "dashed"),
                        risk.table=TRUE, 
                        surv.median.line="hv", 
                        pval=TRUE,
                        pval.method = TRUE,
                        pval.method.coord = c(0.07, 0.05),
                        pval.method.size=1.5,
                        pval.size = 1.5,
                        pval.coord = c(7, 0.05),
                        tables.height=0.1,
                        size=0.3,
                        ggtheme=plot_theme + 
                          theme(plot.background = element_rect(colour = NA)),
                        tables.theme=plot_theme +
                          theme(axis.text.x = element_blank(), 
                                axis.ticks = element_blank(),
                                panel.background = element_blank(),
                                axis.title=element_blank(),
                                axis.text = element_text(size=4),
                                plot.title = element_text(hjust=0, 
                                                          face="italic", 
                                                          size=5),
                                panel.grid = element_blank(),
                                plot.background = element_rect(colour=NA),
                                plot.margin = unit(c(0, 0.2, 0.1, 0.2), 
                                                   units = "cm"),
                                legend.position = "none"),
                        title="Kaplan-Meier Survival Curve",
                        ylab="Probability of Survival",
                        xlab="Days to Event",
                        legend=c(0.825, 0.925),
                        legend.title="Planned Treatment",
                        legend.labs=c("Treatment A", "Treatment B"),
                        censor.shape=16,
                        censor.size=0.7,
                        tables.col="strata",
                        risk.table.y.text.col=TRUE,
                        fontsize=1.5
                        )
						

### manipulating the table data output from the function above in order to include the cumulative events and cumulative censors ###
### in one table                                                                                                                ###
surv_plot$data.survtable$cum.n.event <- paste(surv_plot$data.survtable$cum.n.event, 
                                              " (", 
                                              surv_plot$data.survtable$cum.n.censor,
                                              ")")
											  
### creating the second (bottom) table as a separate output ###
event_tbl <- ggsurvtable(fit=surv_plot$data.survtable, 
                         survtable = "cumevents",
                         title = "Cumulative number of events (cumulative number of censored events)",
                         palette = c("mediumseagreen", "cornflowerblue"),
                         color = "TRT01P",
                         tables.height=0.1,
                         ggtheme=plot_theme + theme(legend.position="none"),
                         tables.theme=plot_theme +
                           theme(axis.text.x = element_blank(), 
                                 axis.ticks = element_blank(),
                                 panel.background = element_blank(),
                                 axis.title=element_blank(),
                                 axis.text = element_text(size=4),
                                 plot.title = element_text(hjust=0, 
                                                           face="italic", 
                                                           size=5),
                                 panel.grid = element_blank(),
                                 plot.background = element_rect(colour=NA),
                                 plot.margin = unit(c(0, 0.2, 0, 0.2), 
                                                    units = "cm"),
                                 legend.position = "none"),
                         fontsize=1.5,
                         legend.labs=c("Treatment A", "Treatment B"),
                         tables.height=0.1
                         )

### isolating the plot part of the output from the ggsurvplot function and adding in additional customisations ###
### geom_tile creates a border for the p-value; geom_text displays the median values at the appropriate places ###
### near the reference lines; labs adds the GEM Test Study header to the plot                                  ###
plt_ext <- surv_plot$plot + 
  labs(tag = "GEM Test Study") +
  guides(colour=guide_legend(nrow=1)) +
  theme(plot.tag.position = c(0.1, 1), 
        plot.margin = unit(c(0.2, 0.2, 0.0, 0.4), 
                           units = "cm"),
        axis.ticks = element_line(colour='#B0B7BB')) + 
  geom_tile(data=data.frame(x=7.04, y=0.05, w=14.2, h=0.06), 
            aes(x=x, y=y, width=w, height=h), 
            colour="black",
            fill=NA,
            inherit.aes=FALSE) + 
  geom_text(data=attr(surv_plot$data.survplot, "table"), 
            aes(x=median, y=0, label=round(median, 0)),
            nudge_x =1.5,
            size=1.5)

### creates three grobs (grid graphical objects) from the plot and two tables in order to arrange onto one page ###
plt <- ggplotGrob(plt_ext)
rsk_tbl <- ggplotGrob(surv_plot$table) 
evt_tbl <- ggplotGrob(event_tbl)

### arranges the multiple grobs onto one page ###
final <- arrangeGrob(plt, 
                     rsk_tbl,
                     evt_tbl,
                     heights = c(5, 0.8, 0.8))

#####################
### save the plot ###
#####################
ggsave(plot=final, 
       filename=paste0(Sys.getenv("CLIENT_PATH"), 
                       "\\Poster-2021\\Outputs\\km_curve_R.png"), 
       width = 3, 
       height = 2.75, 
       units = "in")

################################################################################
