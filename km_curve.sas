********************************************************************************************;
********************************************************************************************;
**                                                                                        **;
**   Sponsor                : GEM Programming Solutions                                   **;
**   Study                  : Test Study                                                  **;
**                                                                                        **;
**   Program name           : km.sas                                                      **;
**   Description            : Creating a Kaplan-Meier Curve                               **;
**                                                                                        **;
**   Input dataset(s)       : adsl  sandbox.trtmt_attr                                    **;
**                                                                                        **;
**   Output dataset(s)      : -                                                           **;
**                                                                                        **;
**   List macros used       : -                                                           **;
**                                                                                        **;
**   Program author         : Jack Finch                                                  **;
**   Date completed         : 09/08/2021                                                  **;
**   SAS version number     : 9.4                                                         **;
**                                                                                        **;
**   Maintenance            :                                                             **;
**   History                :                                                             **;
**                                                                                        **;
********************************************************************************************;
********************************************************************************************;
 
/* delete datasets in the 'work' library */
proc datasets library=work nolist kill;
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

/* selecet ODS output dataset names for the proc lifetest */
ods output Survivalplot = SurvivalPlotData; /* data for KM plot **/
ods output homtests=tests;                  /* p-value */
ods output quartiles=quartiles;             /* median survival time for reflines*/

proc lifetest data = adam.adtte plots = survival ( atrisk=0  to 60 by 10) alpha=0.05 method=km;
   time survival_time * censor(0);
   strata trt01p /test=logrank;
run; 

/* finding where probability of survival = 0.5  for each treatment group for median reference drop lines */
data _null_;
  set quartiles;
  if percent=50 and trt01p = 'Treatment A' then do;
    call symput('ref_a',strip(put(estimate,8.)));
  end;
  if percent=50 and trt01p = 'Treatment B' then do;
    call symput('ref_b',strip(put(estimate,8.)));
  end;
run;

/* create macro variable for p-value to be used later */
proc sql noprint;
  select PROBCHISQ into: pval trimmed
  from tests;
quit; 

/* manipulate the data for the 2nd x-axis table and add in values for the reference line annotations */
data survivalplotdata_2; 
  set survivalplotdata;
  by stratum;
  if first.stratum=1 then
    do;
      event_cumul=0;
      cens_cumul=0;
    end;
    else do;
      if event=1 then event_cumul+1;
      if censored ne . then cens_cumul+1;
    end;
  /* to get the values at the correct x-axis tick mark values */
  if tatrisk ne . then cumul = strip(put(event_cumul,best.)) || '(' || strip(put(cens_cumul,best.)) || ')'; 

  /* _n_=1 so only one value is annotated on to the figure. Without this, the same value would be overlayed and the annotation would appear bold */
  if _n_=1 then do;
  ylabel=0;
  xlabel_a=&ref_a.;
  xlabel_b=&ref_b.;
  lab_a="      &ref_a";
  lab_b="      &ref_b";
  end;
run;


options
        orientation  = landscape
        nonumber nodate /* Suppress the page number and date */
        nobyline;       /* Suppress the BY-line */

/* reset titles and footnotes */
title;
footnote;


/* set ODS output destination and set options */
ods listing gpath="&_root.\gemprogramming.com\GEM Dev - Documents\Standard code library\Poster-2021\Outputs\";
ods graphics / imagemap = on noborder width =  25.94cm  height = 19.59cm imagefmt=png imagename='km';
ods pdf style = gem_plot file = "&_root.\gemprogramming.com\GEM Dev - Documents\Standard code library\Poster-2021\Outputs\km_curve_SAS.pdf" nogtitle nogfootnote;
ods output SGPlot = work.sgplot_output_data;

/* addition of overall titles and slight style changes */
title1 justify=left "GEM Test Study";
title3 "Kaplan-Meier Survival Curve";

proc sgplot data        = survivalplotdata_2
            noborder
            dattrmap = trtmt_attr;

   styleattrs backcolor = ghostwhite
              wallcolor = whitesmoke;

   step    x = time y = survival / group = stratum 
                                   name  = 's'
                                   attrid = trtmt;

   scatter x = time y = censored / group = stratum
                                   attrid = trtmt
                                   dataskin = sheen;

   xaxis label          = 'Days to Event'
         values         = (0 to 60 by 10)
         offsetmin      = .05;

   yaxis label          = "Probability of Survival" 
         values         = (0 .25 .5 .75 1)
         grid;

   /* add in reference droplines*/
   dropline y=0.5 x=&ref_a. / dropto=both lineattrs=(pattern=shortdash) ;
   dropline y=0.5 x=&ref_b. / dropto=both lineattrs=(pattern=shortdash) ;
 
   /* add in median values near the x axis */
   text x=xlabel_a y=ylabel text=lab_a / TEXTATTRS=(Size=8);
   text x=xlabel_b y=ylabel text=lab_b / TEXTATTRS=(Size=8);

   /* add in p=value annotation*/
   inset "Log-rank p=&pval." / position=bottomleft textattrs=(size=12) border;

   /* x axis table for number at risk */
   xaxistable atrisk / x=tatrisk class=stratum  colorgroup=stratum attrid=trtmt location=outside
    title="Number at risk"
   titleattrs=(family=arial style=italic weight=bold)
   valueattrs=(size=9); 

   /* x axis table for events ( censored ) */
   xaxistable cumul / x=tatrisk class=stratum  colorgroup=stratum attrid=trtmt location=outside
    title="Cumulative number of events(cumulative number of censored events)"
   titleattrs=(family=arial style=italic weight=bold)
   valueattrs=(size=9); 

   keylegend 's' / location = inside position = topright title = "Actual Treatment" sortorder = ascending linelength = 20;
run;
title;

ods listing close;
ods pdf close;
ods graphics off;
