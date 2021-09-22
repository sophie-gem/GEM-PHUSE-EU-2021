********************************************************************************************;
********************************************************************************************;
**                                                                                        **;
**   Sponsor                : GEM Programming Solutions                                   **;
**   Study                  : -                                                           **;
**                                                                                        **;
**   Program name           : lab_boxplots.sas                                            **;
**   Description            : Creating animated boxplots (for the liver function tests)   **;
**                                                                                        **;
**   Input dataset(s)       : adlb                                                        **;
**                                                                                        **;
**   Output dataset(s)      : -                                                           **;
**                                                                                        **;
**   List macros used       : -                                                           **;
**                                                                                        **;
**   Program author         : Jack Finch                                                  **;
**   Date completed         : 10/08/2021                                                  **;
**   SAS version number     : 9.4                                                         **;
**                                                                                        **;
**   Maintenance            :                                                             **;
**   History                :                                                             **;
**                                                                                        **;
********************************************************************************************;
********************************************************************************************; 

proc datasets lib=work nolist kill;
run;

/* create attributes adataset to control the colours of the output */
data trtmt_attr;
length id   value fillcolor markersymbol markercolor linecolor textcolor $20 filltransparency 8;
  id=                'trtmt';
  value=             'Treatment A';
  fillcolor=         'MediumSeaGreen';
  filltransparency=   0.5;
  markersymbol=      'CircleFilled';
  markercolor=       'MediumSeaGreen';
  linecolor=         'MediumSeaGreen';
  textcolor=         'MediumSeaGreen';
  output;
  id=                'trtmt';
  value=             'Treatment B';
  fillcolor=         'CornFlowerBlue';
  filltransparency=   0.5;
  markersymbol=      'CircleFilled';
  markercolor=       'CornFlowerBlue';
  linecolor=         'CornFlowerBlue';
  textcolor=         'CornFlowerBlue';
  output;
run;

/* Create a file reference for the printer output */
filename prtout "&_root.\gemprogramming.com\GEM Dev - Documents\Standard code library\Poster-2021\Outputs\lab_boxplots_SAS.gif"; /* Specify the output filename */

/* Set the system animation options */
options printerpath=gif /* Specify the GIF universal printer */
  animduration=1        /* Wait 1 seconds between graphs */ 
  animloop=yes          /* Play continuously */
  noanimoverlay         /* Display graphs sequentially */
  nobyline              /* Suppress the BY-line */
  nonumber nodate;      /* Suppress the page number and date */

/* Close all currently open ODS destinations */
ods _all_ close;

/* Start the animation output */
options animate=start;

/* Clear the titles and footnotes */
title;
footnote;


/* Create a variable to generate the graphs by each frame of the GIF */
data lb_plot;
   set adam.adlb;
   where paramcd in ('ALT' 'AST' 'ALP' 'BILI') and ady ge 0;
   if ady eq 1  then do;build_day = 1;output;end;
   if ady le 15 then do;build_day = 2;output;end;
   if ady le 29 then do;build_day = 3;output;end;
   if ady le 43 then do;build_day = 4;output;end;
   if ady le 57 then do;build_day = 5;output;end;
   if ady le 71 then do;build_day = 6;output;end;
run;

proc sort data = lb_plot;
   by build_day trt01a;
run;

ods graphics on / width=25.94cm;
/* Open the ODS PRINTER destination */
ods printer file=prtout style=htmlblue;

title1 justify=left "GEM Test Study" ;
title3 "Boxplots of the ALP, ALT, AST, and BILI liver function tests";

*** addition of some style attributes to the plot and title to the legend ***;
proc sgpanel data = work.lb_plot dattrmap=trtmt_attr;
   panelby param / layout = panel;

   rowaxis values = (0 to 160 by 20)   label = 'Analysis Value (Units)' grid;
   colaxis values = (1 15 29 43 57 71) label = 'Study Day';

   vbox aval / category     = ady 
               connect      = mean connectattrs = (color = blue)
               group        = trt01a
               attrid       = trtmt 
               fill 
               dataskin     = sheen 
               capshape     = serif
               capscale     = 1
               tip          = (N STD MIN MAX MEAN MEDIAN Q1 Q3)
               notches;
    styleattrs         backcolor   = ghostwhite 
                       wallcolor   = whitesmoke;
  
    keylegend  /       title       = "Actual Treatment"
                       sortorder   = ascending;
    by build_day;
run;

/* Stop the animation output */
options animate=stop;

title;

/* Close the ODS PRINTER destination */
ods printer close;
ods graphics off;
