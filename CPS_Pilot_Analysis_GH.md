CPS_Pilot_Analysis
================
2024-06-14

##### The purpose of this analysis is to compare the risk of secondary occurrences of physical child abuse among the intervention groups (pilot program and standard program) while controlling for age, race, minority status and household risk factors.

###### LOAD LIBRARIES

``` r
library(survival)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(cobalt)
library(MatchIt)
library(vcd)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(labelled)
library(gtsummary)
library(webshot2)
library(Hmisc)
library(survminer)
```

###### LOAD DATA

``` r
load("~/Biostatistics/AbuseStudy.Rdata")
```

###### EXAMINE DATA FRAME

``` r
summary(dat)
```

    ##        id             time            event          program         
    ##  Min.   :  1.0   Min.   :  2.00   Min.   :0.0000   Length:432        
    ##  1st Qu.:108.8   1st Qu.: 93.75   1st Qu.:0.0000   Class :character  
    ##  Median :216.5   Median :245.00   Median :1.0000   Mode  :character  
    ##  Mean   :216.5   Mean   :226.79   Mean   :0.6412                     
    ##  3rd Qu.:324.2   3rd Qu.:365.00   3rd Qu.:1.0000                     
    ##  Max.   :432.0   Max.   :365.00   Max.   :1.0000                     
    ##       age             sex               minority         poverty      
    ##  Min.   : 1.000   Length:432         Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.: 5.000   Class :character   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median : 7.000   Mode  :character   Median :0.0000   Median :0.0000  
    ##  Mean   : 6.852                      Mean   :0.4005   Mean   :0.3681  
    ##  3rd Qu.: 8.250                      3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :15.000                      Max.   :1.0000   Max.   :1.0000  
    ##   subst.abuse       crim.hist          first           region         
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Length:432        
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   Class :character  
    ##  Median :0.0000   Median :0.0000   Median :0.0000   Mode  :character  
    ##  Mean   :0.1181   Mean   :0.1134   Mean   :0.3264                     
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000                     
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000                     
    ##     agency             removal       time.removal        return      
    ##  Length:432         Min.   :0.000   Min.   :  2.00   Min.   :0.0000  
    ##  Class :character   1st Qu.:0.000   1st Qu.: 68.75   1st Qu.:0.0000  
    ##  Mode  :character   Median :0.000   Median :148.50   Median :0.0000  
    ##                     Mean   :0.419   Mean   :169.93   Mean   :0.1435  
    ##                     3rd Qu.:1.000   3rd Qu.:265.00   3rd Qu.:0.0000  
    ##                     Max.   :1.000   Max.   :365.00   Max.   :1.0000  
    ##   time.return    
    ##  Min.   :  2.00  
    ##  1st Qu.: 93.75  
    ##  Median :209.50  
    ##  Mean   :211.97  
    ##  3rd Qu.:365.00  
    ##  Max.   :365.00

``` r
glimpse(dat)
```

    ## Rows: 432
    ## Columns: 17
    ## $ id           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ time         <dbl> 297, 365, 263, 365, 365, 290, 307, 365, 365, 216, 365, 13…
    ## $ event        <dbl> 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, …
    ## $ program      <chr> "Pilot", "Standard", "Pilot", "Pilot", "Standard", "Stand…
    ## $ age          <dbl> 8, 7, 3, 9, 11, 6, 6, 9, 6, 10, 8, 8, 5, 6, 8, 3, 5, 8, 8…
    ## $ sex          <chr> "Male", "Female", "Female", "Female", "Male", "Female", "…
    ## $ minority     <int> 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, …
    ## $ poverty      <dbl> 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, …
    ## $ subst.abuse  <int> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
    ## $ crim.hist    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ first        <int> 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, …
    ## $ region       <chr> "North", "North", "North", "North", "North", "North", "No…
    ## $ agency       <chr> "State", "Local", "Local", "Local", "Local", "Local", "St…
    ## $ removal      <dbl> 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, …
    ## $ time.removal <dbl> 109, 258, 84, 365, 139, 76, 37, 171, 131, 216, 365, 94, 3…
    ## $ return       <dbl> 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, …
    ## $ time.return  <dbl> 188, 365, 263, 365, 288, 290, 140, 283, 276, 216, 365, 13…

###### CHECK FOR MISSING VALUES

``` r
sum(is.na(dat))
```

    ## [1] 0

##### Table 1 summarizes the overall characteristics of all enrolled participants and characteristics by the intervention subgroups, Pilot (n=222) and Standard (n=210).

###### PARTICIPANT CHARACTERISTICS TABLE

``` r
 dat %>% select(event,  program, age, sex, minority, poverty, subst.abuse, crim.hist, first)  %>%
     tbl_summary(
      by = program,
      statistic = list(all_continuous()~"{mean} ({sd})",
                       all_categorical()~"{n} ({p}%)"),
      type = list(age ~ "continuous"),
      label = list(age ~ "Age (years)")
    ) %>%
    modify_header(
      #label = "**Variable**",
      all_stat_cols()~"**{level}**<br>n = {n} ({style_percent(p, digits=1)}%)"
    )%>%
    add_overall(
      last = F,
      col_label = "**All**<br>n = {N}"
    )%>%
    modify_caption("Table 1: Participant Characteristics By Invervention") %>%
    bold_labels()
```

<div id="fmqgrjfxvo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fmqgrjfxvo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#fmqgrjfxvo thead, #fmqgrjfxvo tbody, #fmqgrjfxvo tfoot, #fmqgrjfxvo tr, #fmqgrjfxvo td, #fmqgrjfxvo th {
  border-style: none;
}
&#10;#fmqgrjfxvo p {
  margin: 0;
  padding: 0;
}
&#10;#fmqgrjfxvo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#fmqgrjfxvo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#fmqgrjfxvo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#fmqgrjfxvo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#fmqgrjfxvo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#fmqgrjfxvo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#fmqgrjfxvo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#fmqgrjfxvo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#fmqgrjfxvo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#fmqgrjfxvo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#fmqgrjfxvo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#fmqgrjfxvo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#fmqgrjfxvo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#fmqgrjfxvo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#fmqgrjfxvo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fmqgrjfxvo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#fmqgrjfxvo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#fmqgrjfxvo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#fmqgrjfxvo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fmqgrjfxvo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#fmqgrjfxvo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fmqgrjfxvo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#fmqgrjfxvo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fmqgrjfxvo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fmqgrjfxvo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fmqgrjfxvo .gt_left {
  text-align: left;
}
&#10;#fmqgrjfxvo .gt_center {
  text-align: center;
}
&#10;#fmqgrjfxvo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#fmqgrjfxvo .gt_font_normal {
  font-weight: normal;
}
&#10;#fmqgrjfxvo .gt_font_bold {
  font-weight: bold;
}
&#10;#fmqgrjfxvo .gt_font_italic {
  font-style: italic;
}
&#10;#fmqgrjfxvo .gt_super {
  font-size: 65%;
}
&#10;#fmqgrjfxvo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#fmqgrjfxvo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#fmqgrjfxvo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#fmqgrjfxvo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#fmqgrjfxvo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#fmqgrjfxvo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#fmqgrjfxvo .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption>Table 1: Participant Characteristics By Invervention</caption>
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;All&lt;/strong&gt;&lt;br&gt;n = 432&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>All</strong><br>n = 432<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Pilot&lt;/strong&gt;&lt;br&gt;n = 222 (51.4%)&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Pilot</strong><br>n = 222 (51.4%)<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Standard&lt;/strong&gt;&lt;br&gt;n = 210 (48.6%)&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Standard</strong><br>n = 210 (48.6%)<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">event</td>
<td headers="stat_0" class="gt_row gt_center">277 (64%)</td>
<td headers="stat_1" class="gt_row gt_center">119 (54%)</td>
<td headers="stat_2" class="gt_row gt_center">158 (75%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Age (years)</td>
<td headers="stat_0" class="gt_row gt_center">6.85 (2.51)</td>
<td headers="stat_1" class="gt_row gt_center">6.92 (2.61)</td>
<td headers="stat_2" class="gt_row gt_center">6.78 (2.42)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">sex</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_0" class="gt_row gt_center">200 (46%)</td>
<td headers="stat_1" class="gt_row gt_center">105 (47%)</td>
<td headers="stat_2" class="gt_row gt_center">95 (45%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_0" class="gt_row gt_center">232 (54%)</td>
<td headers="stat_1" class="gt_row gt_center">117 (53%)</td>
<td headers="stat_2" class="gt_row gt_center">115 (55%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">minority</td>
<td headers="stat_0" class="gt_row gt_center">173 (40%)</td>
<td headers="stat_1" class="gt_row gt_center">87 (39%)</td>
<td headers="stat_2" class="gt_row gt_center">86 (41%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">poverty</td>
<td headers="stat_0" class="gt_row gt_center">159 (37%)</td>
<td headers="stat_1" class="gt_row gt_center">79 (36%)</td>
<td headers="stat_2" class="gt_row gt_center">80 (38%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">subst.abuse</td>
<td headers="stat_0" class="gt_row gt_center">51 (12%)</td>
<td headers="stat_1" class="gt_row gt_center">21 (9.5%)</td>
<td headers="stat_2" class="gt_row gt_center">30 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">crim.hist</td>
<td headers="stat_0" class="gt_row gt_center">49 (11%)</td>
<td headers="stat_1" class="gt_row gt_center">25 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">24 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">first</td>
<td headers="stat_0" class="gt_row gt_center">141 (33%)</td>
<td headers="stat_1" class="gt_row gt_center">74 (33%)</td>
<td headers="stat_2" class="gt_row gt_center">67 (32%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Mean (SD)</td>
    </tr>
  </tfoot>
</table>
</div>

###### FACTORIZE VARIABLES (FOR CONTINGENCY TABLES)

``` r
dat$minorityf <- factor(dat$minority, labels = c("Non-minority", "Minority"))
var_label(dat$minorityf) <- "Minority"
dat$povertyf <- factor(dat$poverty, labels = c("No", "Yes"))
var_label(dat$povertyf) <- "Poverty"
dat$substf <- factor(dat$subst.abuse, labels = c("No","Yes"))
var_label(dat$substf) <- "Substance Abuse"
dat$crimhistf <- factor(dat$crim.hist, labels = c("No","Yes"))
var_label(dat$crimhistf) <- "Criminial History"
dat$firstf <- factor(dat$first, labels = c("No", "Yes"))
var_label(dat$firstf) <- "First Contact with CPS"
```

###### APPLY VARIABLE ATTRIBUTE LABLES TO WIDE-FORM DATA SET

``` r
var_label(dat$event) <- "Event"
var_label(dat$program) <- "Program"
var_label(dat$age) <- "Age"
var_label(dat$sex) <- "Sex"
var_label(dat$minority) <- "Minority"
var_label(dat$poverty) <- "Family Below Poverty Threshold"
var_label(dat$subst.abuse) <- "Substance Abuse in Household"
var_label(dat$crim.hist) <- "Criminal History in Household"
var_label(dat$first) <- "First CPS Contact"
var_label(dat$region) <- "Region"
var_label(dat$agency) <- "Agency"
```

#### ASSESSING TREATMENT BALANCE AMONG COVARIATES

#### CONTINUOUS VARIABLES:AGE

##### A two-sample t-test determined that mean age did not significantly differ between intervention groups.

``` r
t.test(age~program, data = dat)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  age by program
    ## t = 0.60917, df = 429.83, p-value = 0.5427
    ## alternative hypothesis: true difference in means between group Pilot and group Standard is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3278158  0.6222817
    ## sample estimates:
    ##    mean in group Pilot mean in group Standard 
    ##               6.923423               6.776190

##### The distribution of age by intervention group is visualized in a density plot and side-by-sidebboxplots which show similar distribution among groups.

###### AGE-PROGRAM DENSITY PLOT

``` r
ggplot(dat, aes(x=age, color = program))+
  geom_density() +
  labs(x = "Age", y = "Density", color = "Program")+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size=10)
  )
```

![](CPS_Pilot_Analysis_GH_files/figure-gfm/Age-program%20density%20plot-1.png)<!-- -->

###### BOXPLOT-AGE BY PROGRAM

``` r
ageboxplot <- ggplot(dat, aes(x=program, y=age, fill=program))+
  geom_boxplot() +  ggtitle("Age by Program") +
  labs(x = "Program", y="Age") +
  theme(legend.position = "none") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size=10)
  ) 

ageboxplot
```

![](CPS_Pilot_Analysis_GH_files/figure-gfm/Age%20x%20program%20boxplot-1.png)<!-- -->

#### DICHOTOMOUS VARIABLES

##### Pearson’s Chi-squared tests were performed to assess the homogeneity of the distribution of the dichotomous variables among the intervention groups.

####### PROGRAM X SEX

``` r
dat %>%
  tbl_cross(
    row =sex,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
```

<div id="swbsglaofa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#swbsglaofa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#swbsglaofa thead, #swbsglaofa tbody, #swbsglaofa tfoot, #swbsglaofa tr, #swbsglaofa td, #swbsglaofa th {
  border-style: none;
}
&#10;#swbsglaofa p {
  margin: 0;
  padding: 0;
}
&#10;#swbsglaofa .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#swbsglaofa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#swbsglaofa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#swbsglaofa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#swbsglaofa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#swbsglaofa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#swbsglaofa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#swbsglaofa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#swbsglaofa .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#swbsglaofa .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#swbsglaofa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#swbsglaofa .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#swbsglaofa .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#swbsglaofa .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#swbsglaofa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#swbsglaofa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#swbsglaofa .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#swbsglaofa .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#swbsglaofa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#swbsglaofa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#swbsglaofa .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#swbsglaofa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#swbsglaofa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#swbsglaofa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#swbsglaofa .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#swbsglaofa .gt_left {
  text-align: left;
}
&#10;#swbsglaofa .gt_center {
  text-align: center;
}
&#10;#swbsglaofa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#swbsglaofa .gt_font_normal {
  font-weight: normal;
}
&#10;#swbsglaofa .gt_font_bold {
  font-weight: bold;
}
&#10;#swbsglaofa .gt_font_italic {
  font-style: italic;
}
&#10;#swbsglaofa .gt_super {
  font-size: 65%;
}
&#10;#swbsglaofa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#swbsglaofa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#swbsglaofa .gt_indent_1 {
  text-indent: 5px;
}
&#10;#swbsglaofa .gt_indent_2 {
  text-indent: 10px;
}
&#10;#swbsglaofa .gt_indent_3 {
  text-indent: 15px;
}
&#10;#swbsglaofa .gt_indent_4 {
  text-indent: 20px;
}
&#10;#swbsglaofa .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Program&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Program</strong></span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Pilot">Pilot</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Standard">Standard</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Sex</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.668</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_1" class="gt_row gt_center">105</td>
<td headers="stat_2" class="gt_row gt_center">95</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_1" class="gt_row gt_center">117</td>
<td headers="stat_2" class="gt_row gt_center">115</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Total</td>
<td headers="stat_1" class="gt_row gt_center">222</td>
<td headers="stat_2" class="gt_row gt_center">210</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Pearson’s Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

###### PROGRAM X MINORITY

``` r
dat %>%
  tbl_cross(
    row = minorityf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
```

<div id="ykrnccqdmv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ykrnccqdmv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ykrnccqdmv thead, #ykrnccqdmv tbody, #ykrnccqdmv tfoot, #ykrnccqdmv tr, #ykrnccqdmv td, #ykrnccqdmv th {
  border-style: none;
}
&#10;#ykrnccqdmv p {
  margin: 0;
  padding: 0;
}
&#10;#ykrnccqdmv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ykrnccqdmv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ykrnccqdmv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ykrnccqdmv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ykrnccqdmv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ykrnccqdmv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ykrnccqdmv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ykrnccqdmv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ykrnccqdmv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ykrnccqdmv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ykrnccqdmv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ykrnccqdmv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ykrnccqdmv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ykrnccqdmv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ykrnccqdmv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ykrnccqdmv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ykrnccqdmv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ykrnccqdmv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ykrnccqdmv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ykrnccqdmv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ykrnccqdmv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ykrnccqdmv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ykrnccqdmv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ykrnccqdmv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ykrnccqdmv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ykrnccqdmv .gt_left {
  text-align: left;
}
&#10;#ykrnccqdmv .gt_center {
  text-align: center;
}
&#10;#ykrnccqdmv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ykrnccqdmv .gt_font_normal {
  font-weight: normal;
}
&#10;#ykrnccqdmv .gt_font_bold {
  font-weight: bold;
}
&#10;#ykrnccqdmv .gt_font_italic {
  font-style: italic;
}
&#10;#ykrnccqdmv .gt_super {
  font-size: 65%;
}
&#10;#ykrnccqdmv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ykrnccqdmv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ykrnccqdmv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ykrnccqdmv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ykrnccqdmv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ykrnccqdmv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ykrnccqdmv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Program&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Program</strong></span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Pilot">Pilot</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Standard">Standard</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Minority</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.709</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Non-minority</td>
<td headers="stat_1" class="gt_row gt_center">135</td>
<td headers="stat_2" class="gt_row gt_center">124</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Minority</td>
<td headers="stat_1" class="gt_row gt_center">87</td>
<td headers="stat_2" class="gt_row gt_center">86</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Total</td>
<td headers="stat_1" class="gt_row gt_center">222</td>
<td headers="stat_2" class="gt_row gt_center">210</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Pearson’s Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

###### PROGRAM X POVERTY

``` r
dat %>%
  tbl_cross(
    row = povertyf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
```

<div id="xtyjehkwvk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xtyjehkwvk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xtyjehkwvk thead, #xtyjehkwvk tbody, #xtyjehkwvk tfoot, #xtyjehkwvk tr, #xtyjehkwvk td, #xtyjehkwvk th {
  border-style: none;
}
&#10;#xtyjehkwvk p {
  margin: 0;
  padding: 0;
}
&#10;#xtyjehkwvk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xtyjehkwvk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#xtyjehkwvk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#xtyjehkwvk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#xtyjehkwvk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#xtyjehkwvk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xtyjehkwvk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xtyjehkwvk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#xtyjehkwvk .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xtyjehkwvk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#xtyjehkwvk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#xtyjehkwvk .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xtyjehkwvk .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xtyjehkwvk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#xtyjehkwvk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xtyjehkwvk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#xtyjehkwvk .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xtyjehkwvk .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xtyjehkwvk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xtyjehkwvk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xtyjehkwvk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xtyjehkwvk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xtyjehkwvk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xtyjehkwvk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xtyjehkwvk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xtyjehkwvk .gt_left {
  text-align: left;
}
&#10;#xtyjehkwvk .gt_center {
  text-align: center;
}
&#10;#xtyjehkwvk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xtyjehkwvk .gt_font_normal {
  font-weight: normal;
}
&#10;#xtyjehkwvk .gt_font_bold {
  font-weight: bold;
}
&#10;#xtyjehkwvk .gt_font_italic {
  font-style: italic;
}
&#10;#xtyjehkwvk .gt_super {
  font-size: 65%;
}
&#10;#xtyjehkwvk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xtyjehkwvk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xtyjehkwvk .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xtyjehkwvk .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xtyjehkwvk .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xtyjehkwvk .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xtyjehkwvk .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Program&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Program</strong></span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Pilot">Pilot</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Standard">Standard</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Poverty</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.589</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">143</td>
<td headers="stat_2" class="gt_row gt_center">130</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">79</td>
<td headers="stat_2" class="gt_row gt_center">80</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Total</td>
<td headers="stat_1" class="gt_row gt_center">222</td>
<td headers="stat_2" class="gt_row gt_center">210</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Pearson’s Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

###### PROGRAM X SUBSTANCE ABUSE IN HOUSEHOLD

``` r
dat %>%
  tbl_cross(
    row = substf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
```

<div id="ieyhajinre" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ieyhajinre table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ieyhajinre thead, #ieyhajinre tbody, #ieyhajinre tfoot, #ieyhajinre tr, #ieyhajinre td, #ieyhajinre th {
  border-style: none;
}
&#10;#ieyhajinre p {
  margin: 0;
  padding: 0;
}
&#10;#ieyhajinre .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ieyhajinre .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ieyhajinre .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ieyhajinre .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ieyhajinre .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ieyhajinre .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ieyhajinre .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ieyhajinre .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ieyhajinre .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ieyhajinre .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ieyhajinre .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ieyhajinre .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ieyhajinre .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ieyhajinre .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ieyhajinre .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ieyhajinre .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ieyhajinre .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ieyhajinre .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ieyhajinre .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ieyhajinre .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ieyhajinre .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ieyhajinre .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ieyhajinre .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ieyhajinre .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ieyhajinre .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ieyhajinre .gt_left {
  text-align: left;
}
&#10;#ieyhajinre .gt_center {
  text-align: center;
}
&#10;#ieyhajinre .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ieyhajinre .gt_font_normal {
  font-weight: normal;
}
&#10;#ieyhajinre .gt_font_bold {
  font-weight: bold;
}
&#10;#ieyhajinre .gt_font_italic {
  font-style: italic;
}
&#10;#ieyhajinre .gt_super {
  font-size: 65%;
}
&#10;#ieyhajinre .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ieyhajinre .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ieyhajinre .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ieyhajinre .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ieyhajinre .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ieyhajinre .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ieyhajinre .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Program&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Program</strong></span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Pilot">Pilot</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Standard">Standard</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Substance Abuse</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.120</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">201</td>
<td headers="stat_2" class="gt_row gt_center">180</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">21</td>
<td headers="stat_2" class="gt_row gt_center">30</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Total</td>
<td headers="stat_1" class="gt_row gt_center">222</td>
<td headers="stat_2" class="gt_row gt_center">210</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Pearson’s Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

###### PROGRAM X CRIMINAL HISTORY IN HOUSEHOLD

``` r
dat %>%
  tbl_cross(
    row = crimhistf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
```

<div id="djleqnwhej" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#djleqnwhej table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#djleqnwhej thead, #djleqnwhej tbody, #djleqnwhej tfoot, #djleqnwhej tr, #djleqnwhej td, #djleqnwhej th {
  border-style: none;
}
&#10;#djleqnwhej p {
  margin: 0;
  padding: 0;
}
&#10;#djleqnwhej .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#djleqnwhej .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#djleqnwhej .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#djleqnwhej .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#djleqnwhej .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#djleqnwhej .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#djleqnwhej .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#djleqnwhej .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#djleqnwhej .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#djleqnwhej .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#djleqnwhej .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#djleqnwhej .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#djleqnwhej .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#djleqnwhej .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#djleqnwhej .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#djleqnwhej .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#djleqnwhej .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#djleqnwhej .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#djleqnwhej .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#djleqnwhej .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#djleqnwhej .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#djleqnwhej .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#djleqnwhej .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#djleqnwhej .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#djleqnwhej .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#djleqnwhej .gt_left {
  text-align: left;
}
&#10;#djleqnwhej .gt_center {
  text-align: center;
}
&#10;#djleqnwhej .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#djleqnwhej .gt_font_normal {
  font-weight: normal;
}
&#10;#djleqnwhej .gt_font_bold {
  font-weight: bold;
}
&#10;#djleqnwhej .gt_font_italic {
  font-style: italic;
}
&#10;#djleqnwhej .gt_super {
  font-size: 65%;
}
&#10;#djleqnwhej .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#djleqnwhej .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#djleqnwhej .gt_indent_1 {
  text-indent: 5px;
}
&#10;#djleqnwhej .gt_indent_2 {
  text-indent: 10px;
}
&#10;#djleqnwhej .gt_indent_3 {
  text-indent: 15px;
}
&#10;#djleqnwhej .gt_indent_4 {
  text-indent: 20px;
}
&#10;#djleqnwhej .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Program&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Program</strong></span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Pilot">Pilot</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Standard">Standard</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Criminial History</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.956</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">197</td>
<td headers="stat_2" class="gt_row gt_center">186</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">25</td>
<td headers="stat_2" class="gt_row gt_center">24</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Total</td>
<td headers="stat_1" class="gt_row gt_center">222</td>
<td headers="stat_2" class="gt_row gt_center">210</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Pearson’s Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

###### PROGRAM X FIRST CONTACT WITH CPS

``` r
dat %>%
  tbl_cross(
    row = firstf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
```

<div id="rjeoybozon" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rjeoybozon table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rjeoybozon thead, #rjeoybozon tbody, #rjeoybozon tfoot, #rjeoybozon tr, #rjeoybozon td, #rjeoybozon th {
  border-style: none;
}
&#10;#rjeoybozon p {
  margin: 0;
  padding: 0;
}
&#10;#rjeoybozon .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rjeoybozon .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#rjeoybozon .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#rjeoybozon .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#rjeoybozon .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#rjeoybozon .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rjeoybozon .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rjeoybozon .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#rjeoybozon .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rjeoybozon .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#rjeoybozon .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#rjeoybozon .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rjeoybozon .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rjeoybozon .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#rjeoybozon .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rjeoybozon .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#rjeoybozon .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rjeoybozon .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rjeoybozon .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rjeoybozon .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rjeoybozon .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rjeoybozon .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rjeoybozon .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rjeoybozon .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#rjeoybozon .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rjeoybozon .gt_left {
  text-align: left;
}
&#10;#rjeoybozon .gt_center {
  text-align: center;
}
&#10;#rjeoybozon .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rjeoybozon .gt_font_normal {
  font-weight: normal;
}
&#10;#rjeoybozon .gt_font_bold {
  font-weight: bold;
}
&#10;#rjeoybozon .gt_font_italic {
  font-style: italic;
}
&#10;#rjeoybozon .gt_super {
  font-size: 65%;
}
&#10;#rjeoybozon .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rjeoybozon .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rjeoybozon .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rjeoybozon .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rjeoybozon .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rjeoybozon .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rjeoybozon .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id=""></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="&lt;strong&gt;Program&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Program</strong></span>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Pilot">Pilot</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Standard">Standard</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">First Contact with CPS</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.752</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">148</td>
<td headers="stat_2" class="gt_row gt_center">143</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">74</td>
<td headers="stat_2" class="gt_row gt_center">67</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Total</td>
<td headers="stat_1" class="gt_row gt_center">222</td>
<td headers="stat_2" class="gt_row gt_center">210</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Pearson’s Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

##### The results of all Pearson’s Chi-squared tests were not statistically significant (p \> .05).

##### Treatment groups were found to be balanced among the measured socio-demographic variables and household risk factor indicator variables.

#### VARIABLE EFFECT ON TIME TO EVENT (NON SURVIVAL CONTEXT)

###### BINARY LOGISTIC MODELS

``` r
mod.base <- glm(event~1, data=dat[,3:11], family = "binomial")
mod.prog1 <- update(mod.base,~.+program, data = dat) #p<.001
summary(mod.prog1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ program, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       0.1444     0.1346   1.073    0.283    
    ## programStandard   0.9670     0.2090   4.627 3.71e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 541.68  on 430  degrees of freedom
    ## AIC: 545.68
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.age1 <- update(mod.base,~.+age, data = dat) #p<.001
summary(mod.age1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ age, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  2.67313    0.34356   7.781 7.21e-15 ***
    ## age         -0.29626    0.04526  -6.546 5.91e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 514.86  on 430  degrees of freedom
    ## AIC: 518.86
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.sex1 <- update(mod.base,~.+sex, data = dat) #p = 0.394
summary(mod.sex1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ sex, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.4895     0.1457   3.360 0.000778 ***
    ## sexMale       0.1714     0.2010   0.853 0.393781    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 563.23  on 430  degrees of freedom
    ## AIC: 567.23
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.min1 <- update(mod.base,~.+minority, data = dat) #p = 0.672
summary(mod.min1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ minority, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.54599    0.12893   4.235 2.29e-05 ***
    ## minority     0.08705    0.20528   0.424    0.672    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 563.77  on 430  degrees of freedom
    ## AIC: 567.77
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.pov1 <- update(mod.base,~.+poverty, data = dat) #p = .007
summary(mod.pov1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ poverty, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.7938     0.1307   6.073 1.25e-09 ***
    ## poverty      -0.5536     0.2064  -2.682  0.00731 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 556.76  on 430  degrees of freedom
    ## AIC: 560.76
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.subst1 <- update(mod.base,~.+subst.abuse, data = dat) #p = .307
summary(mod.subst1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ subst.abuse, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.5432     0.1063   5.111  3.2e-07 ***
    ## subst.abuse   0.3323     0.3252   1.022    0.307    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 562.87  on 430  degrees of freedom
    ## AIC: 566.87
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.ch1 <- update(mod.base,~.+crim.hist, data = dat) #p = .002
summary(mod.ch1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ crim.hist, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.4624     0.1049   4.406 1.05e-05 ***
    ## crim.hist     1.3294     0.4215   3.154  0.00161 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 551.21  on 430  degrees of freedom
    ## AIC: 555.21
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mod.firstCPS1 <- update(mod.base,~.+first, data = dat) #p=.004
summary(mod.firstCPS1)
```

    ## 
    ## Call:
    ## glm(formula = event ~ first, family = "binomial", data = dat)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.7875     0.1264   6.228 4.74e-10 ***
    ## first        -0.6025     0.2112  -2.853  0.00433 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 555.84  on 430  degrees of freedom
    ## AIC: 559.84
    ## 
    ## Number of Fisher Scoring iterations: 4

###### MAIN EFFECTS BINARY LOGISTIC MODEL(EXCL REGION, AGENCY) & SUMMARY TABLE

``` r
mod <- glm(event~., data = dat[,3:11], family = "binomial")
summary(mod)
```

    ## 
    ## Call:
    ## glm(formula = event ~ ., family = "binomial", data = dat[, 3:11])
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      2.58257    0.42599   6.062 1.34e-09 ***
    ## programStandard  1.10060    0.23659   4.652 3.29e-06 ***
    ## age             -0.35105    0.05113  -6.865 6.63e-12 ***
    ## sexMale          0.25097    0.23334   1.076  0.28213    
    ## minority         0.25993    0.24583   1.057  0.29036    
    ## poverty         -0.65562    0.24803  -2.643  0.00821 ** 
    ## subst.abuse      0.94552    0.39826   2.374  0.01759 *  
    ## crim.hist        2.01052    0.49264   4.081 4.48e-05 ***
    ## first           -0.79248    0.24463  -3.239  0.00120 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 563.95  on 431  degrees of freedom
    ## Residual deviance: 451.33  on 423  degrees of freedom
    ## AIC: 469.33
    ## 
    ## Number of Fisher Scoring iterations: 5

###### LOGISTIC REGRESSION MAIN EFFECTS MODEL SUMMARY TABLE

``` r
tbl_regression(mod, exponentiate = T)%>%
  modify_caption("Table 2: Logistic Regression-Main Effects Model Summary")
```

<div id="phatzkjfhg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#phatzkjfhg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#phatzkjfhg thead, #phatzkjfhg tbody, #phatzkjfhg tfoot, #phatzkjfhg tr, #phatzkjfhg td, #phatzkjfhg th {
  border-style: none;
}
&#10;#phatzkjfhg p {
  margin: 0;
  padding: 0;
}
&#10;#phatzkjfhg .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#phatzkjfhg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#phatzkjfhg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#phatzkjfhg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#phatzkjfhg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#phatzkjfhg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#phatzkjfhg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#phatzkjfhg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#phatzkjfhg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#phatzkjfhg .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#phatzkjfhg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#phatzkjfhg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#phatzkjfhg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#phatzkjfhg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#phatzkjfhg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phatzkjfhg .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#phatzkjfhg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#phatzkjfhg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#phatzkjfhg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phatzkjfhg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#phatzkjfhg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phatzkjfhg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#phatzkjfhg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phatzkjfhg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#phatzkjfhg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#phatzkjfhg .gt_left {
  text-align: left;
}
&#10;#phatzkjfhg .gt_center {
  text-align: center;
}
&#10;#phatzkjfhg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#phatzkjfhg .gt_font_normal {
  font-weight: normal;
}
&#10;#phatzkjfhg .gt_font_bold {
  font-weight: bold;
}
&#10;#phatzkjfhg .gt_font_italic {
  font-style: italic;
}
&#10;#phatzkjfhg .gt_super {
  font-size: 65%;
}
&#10;#phatzkjfhg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#phatzkjfhg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#phatzkjfhg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#phatzkjfhg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#phatzkjfhg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#phatzkjfhg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#phatzkjfhg .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption>Table 2: Logistic Regression-Main Effects Model Summary</caption>
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Program</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Pilot</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Standard</td>
<td headers="estimate" class="gt_row gt_center">3.01</td>
<td headers="ci" class="gt_row gt_center">1.90, 4.82</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="estimate" class="gt_row gt_center">0.70</td>
<td headers="ci" class="gt_row gt_center">0.63, 0.78</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="estimate" class="gt_row gt_center">1.29</td>
<td headers="ci" class="gt_row gt_center">0.81, 2.03</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Minority</td>
<td headers="estimate" class="gt_row gt_center">1.30</td>
<td headers="ci" class="gt_row gt_center">0.80, 2.11</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Family Below Poverty Threshold</td>
<td headers="estimate" class="gt_row gt_center">0.52</td>
<td headers="ci" class="gt_row gt_center">0.32, 0.84</td>
<td headers="p.value" class="gt_row gt_center">0.008</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Substance Abuse in Household</td>
<td headers="estimate" class="gt_row gt_center">2.57</td>
<td headers="ci" class="gt_row gt_center">1.20, 5.78</td>
<td headers="p.value" class="gt_row gt_center">0.018</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Criminal History in Household</td>
<td headers="estimate" class="gt_row gt_center">7.47</td>
<td headers="ci" class="gt_row gt_center">3.02, 21.2</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">First CPS Contact</td>
<td headers="estimate" class="gt_row gt_center">0.45</td>
<td headers="ci" class="gt_row gt_center">0.28, 0.73</td>
<td headers="p.value" class="gt_row gt_center">0.001</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

###### LOGISTIC REGRESSION MODEL-ANALYSIS OF VARIANCE

``` r
aov(mod)
```

    ## Call:
    ##    aov(formula = mod)
    ## 
    ## Terms:
    ##                  program      age      sex minority  poverty subst.abuse
    ## Sum of Squares   5.05105 10.55731  0.05524  0.03168  1.26310     0.38877
    ## Deg. of Freedom        1        1        1        1        1           1
    ##                 crim.hist    first Residuals
    ## Sum of Squares    3.46422  1.73115  76.84406
    ## Deg. of Freedom         1        1       423
    ## 
    ## Residual standard error: 0.4262211
    ## Estimated effects may be unbalanced

###### GOODNESS OF FIT-DEVIANCE

``` r
1-pchisq(q=mod$null.deviance - mod$deviance, df=length(coef(mod)))
```

    ## [1] 0

###### COEFFICIENT COMPARISON

``` r
drop1(mod, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## event ~ program + age + sex + minority + poverty + subst.abuse + 
    ##     crim.hist + first
    ##             Df Deviance    AIC    LRT  Pr(>Chi)    
    ## <none>           451.33 469.33                     
    ## program      1   474.17 490.17 22.840 1.761e-06 ***
    ## age          1   507.55 523.55 56.213 6.503e-14 ***
    ## sex          1   452.49 468.49  1.159  0.281590    
    ## minority     1   452.46 468.46  1.125  0.288767    
    ## poverty      1   458.40 474.40  7.070  0.007840 ** 
    ## subst.abuse  1   457.35 473.35  6.022  0.014130 *  
    ## crim.hist    1   473.01 489.01 21.675 3.229e-06 ***
    ## first        1   461.99 477.99 10.660  0.001095 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###### RESPONSE RESIDUALS

``` r
dat$rsp_resmod <- resid(mod, type = "response")
mean(dat$rsp_resmod)
```

    ## [1] 6.532664e-15

##### A density plot of the response residuals showed the residuals were somewhat bimodal, but otherwise normally distributed and are centered around zero.

###### DENSITY PLOT OF RESPONSE RESIDUALS

``` r
ggplot(dat, aes(x=rsp_resmod))+
  geom_density()+
  geom_vline(xintercept = 0, linetype="dotted", linewidth = 1.3)+
  ggtitle("Logistic Regression Response Residuals-Main Effects Model")+
  labs(x = "Response Residuals", y = "Density")+
  geom_density(aes(x=rsp_resmod))+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size=10)
  )
```

![](CPS_Pilot_Analysis_GH_files/figure-gfm/log%20reg%20response%20residuals%20density%20plot-1.png)<!-- -->

##### The bimodal nature of the response residual distribution can also be seen in a normal QQ plot.

###### NORMAL QQ PLOT

``` r
res <- resid(mod)
qqnorm(res)
qqline(res)
```

![](CPS_Pilot_Analysis_GH_files/figure-gfm/normal%20QQ%20plot-log%20reg%20model-1.png)<!-- -->

``` r
range(dat$age)
```

    ## [1]  1 15

#### SURVIVAL ANALYSIS

##### Cox regression analysis was used to assess which covariates were associated with longer time-to-event (i.e. which covariates were associated with reduced risk of occurrence of secondary child abuse events). A time-dependent covariate “outofhome” was defined based on the variables measuring removal from and return to the home. The variable is an indicator taking value 1 when the child is outside of the home and has been placed in foster care and 0 when the child is in the home.

##### CONSTRUCTING TIME-DEPENDENT DATA SET FOR COX MODEL

###### WIDE TO LONG/COUNTING FORMAT DATA FRAME TRANSFORMATION

``` r
dat.long <- data.frame(id=numeric(length=0), start=numeric(length=0), stop=numeric(length=0), 
                       outofhome=numeric(length=0), event=numeric(length=0))
for(i in 1:nrow(dat)) { 
  start <- 0
  stop <- numeric(length=0)
  outofhome <- 0
  event <- numeric(length=0)
  if (dat$removal[i]==1) {
    start <- c(start, dat$time.removal[i])
    stop <- c(stop, dat$time.removal[i])
    outofhome <- c(outofhome,1)
    event <- c(event, 0)
  }
  if (dat$return[i]==1) {
    start <- c(start, dat$time.return[i])
    stop <- c(stop, dat$time.return[i])
    outofhome <- c(outofhome,0)
    event <- c(event, 0)
  }
  stop <- c(stop, dat$time[i])
  event <- c(event, dat$event[i])
  temp.frame <- data.frame(id=dat$id[i],start,stop,event,outofhome)
  dat.long <- rbind(dat.long,temp.frame)
}
```

###### MERGE DATA FRAMES ON ID

``` r
dat.demo <- dat[,c(1,4:13)]
dat.new <- merge(dat.long,dat.demo, on = "id") 
```

###### SURV OBJECT

``` r
abuse.surv <- Surv(dat.new$start, dat.new$stop, dat.new$event, type="counting")
```

##### A main effects model was built using the time to event (experiencing secondary instance of child abuse) as the response variable and the out of home indicaton, intervention program, socio-demographic variables, and indicator variables for household risk factors as predictor variables.

###### MAIN EFFECTS COX MODEL

``` r
mod.cox.main <- coxph(abuse.surv~outofhome+program+age+sex+minority+
                        poverty+subst.abuse+crim.hist+first, data=dat.new)
summary(mod.cox.main)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + sex + 
    ##     minority + poverty + subst.abuse + crim.hist + first, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                     coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.44662   1.56303  0.15237  2.931  0.00338 ** 
    ## programStandard  0.69773   2.00918  0.12348  5.650 1.60e-08 ***
    ## age             -0.21376   0.80754  0.02689 -7.949 1.89e-15 ***
    ## sexMale          0.08465   1.08834  0.12256  0.691  0.48975    
    ## minority         0.22699   1.25482  0.13226  1.716  0.08613 .  
    ## poverty         -0.28495   0.75205  0.13898 -2.050  0.04034 *  
    ## subst.abuse      0.49549   1.64131  0.18935  2.617  0.00887 ** 
    ## crim.hist        0.84032   2.31711  0.17225  4.878 1.07e-06 ***
    ## first           -0.42301   0.65507  0.13670 -3.094  0.00197 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.5630     0.6398    1.1595    2.1070
    ## programStandard    2.0092     0.4977    1.5773    2.5593
    ## age                0.8075     1.2383    0.7661    0.8512
    ## sexMale            1.0883     0.9188    0.8559    1.3838
    ## minority           1.2548     0.7969    0.9683    1.6262
    ## poverty            0.7521     1.3297    0.5727    0.9875
    ## subst.abuse        1.6413     0.6093    1.1324    2.3788
    ## crim.hist          2.3171     0.4316    1.6532    3.2476
    ## first              0.6551     1.5265    0.5011    0.8564
    ## 
    ## Concordance= 0.697  (se = 0.016 )
    ## Likelihood ratio test= 138.7  on 9 df,   p=<2e-16
    ## Wald test            = 135.1  on 9 df,   p=<2e-16
    ## Score (logrank) test = 140.4  on 9 df,   p=<2e-16

##### A backward stepwise cox regression was used to determine risk factors of the response out of the candidate predictor variables included in the main effects model. At each step, variables were chosen based on Akaike information criterion (AIC).

###### BACKWARD STEPWISE VARIABLE SELECTION

``` r
mod.cox.s <- step(mod.cox.main, scope = ~outofhome+program+age+sex+minority+poverty+subst.abuse
                  +crim.hist+first, data=dat.new, direction = "backward")
```

    ## Start:  AIC=3005.95
    ## abuse.surv ~ outofhome + program + age + sex + minority + poverty + 
    ##     subst.abuse + crim.hist + first
    ## 
    ##               Df    AIC
    ## - sex          1 3004.4
    ## <none>           3005.9
    ## - minority     1 3006.9
    ## - poverty      1 3008.2
    ## - subst.abuse  1 3010.2
    ## - outofhome    1 3012.1
    ## - first        1 3014.1
    ## - crim.hist    1 3023.9
    ## - program      1 3036.3
    ## - age          1 3072.2
    ## 
    ## Step:  AIC=3004.43
    ## abuse.surv ~ outofhome + program + age + minority + poverty + 
    ##     subst.abuse + crim.hist + first
    ## 
    ##               Df    AIC
    ## <none>           3004.4
    ## - minority     1 3005.5
    ## - poverty      1 3006.8
    ## - subst.abuse  1 3008.4
    ## - outofhome    1 3010.9
    ## - first        1 3013.1
    ## - crim.hist    1 3022.1
    ## - program      1 3035.0
    ## - age          1 3070.8

``` r
summary(mod.cox.s)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     poverty + subst.abuse + crim.hist + first, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                     coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.45443   1.57528  0.15195  2.991  0.00278 ** 
    ## programStandard  0.70038   2.01451  0.12347  5.672 1.41e-08 ***
    ## age             -0.21373   0.80757  0.02687 -7.955 1.79e-15 ***
    ## minority         0.23273   1.26205  0.13212  1.762  0.07814 .  
    ## poverty         -0.28758   0.75008  0.13902 -2.069  0.03859 *  
    ## subst.abuse      0.48622   1.62616  0.18890  2.574  0.01006 *  
    ## crim.hist        0.83372   2.30186  0.17194  4.849 1.24e-06 ***
    ## first           -0.43248   0.64890  0.13598 -3.180  0.00147 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.5753     0.6348    1.1695    2.1218
    ## programStandard    2.0145     0.4964    1.5815    2.5661
    ## age                0.8076     1.2383    0.7661    0.8512
    ## minority           1.2620     0.7924    0.9741    1.6351
    ## poverty            0.7501     1.3332    0.5712    0.9850
    ## subst.abuse        1.6262     0.6149    1.1230    2.3548
    ## crim.hist          2.3019     0.4344    1.6433    3.2243
    ## first              0.6489     1.5411    0.4971    0.8471
    ## 
    ## Concordance= 0.697  (se = 0.016 )
    ## Likelihood ratio test= 138.3  on 8 df,   p=<2e-16
    ## Wald test            = 133.8  on 8 df,   p=<2e-16
    ## Score (logrank) test = 138.9  on 8 df,   p=<2e-16

``` r
cox.zph(mod.cox.s) #poverty violates PH assumption
```

    ##              chisq df     p
    ## outofhome   1.2693  1 0.260
    ## program     0.2454  1 0.620
    ## age         0.0641  1 0.800
    ## minority    1.3819  1 0.240
    ## poverty     4.1124  1 0.043
    ## subst.abuse 1.5075  1 0.220
    ## crim.hist   0.0537  1 0.817
    ## first       0.1816  1 0.670
    ## GLOBAL      7.3877  8 0.495

##### This procedure reduced the predictor variables to the out of home indicator, age, minority race, poverty status, history of substance abuse, criminal history in the household, and whether the index event was the first contact with CPS. The Schoenfield test showed that the poverty status variable violated the proportional hazards assumption (p = .04) and was therefore used as a stratifying variable.

###### STRATIFY BY POVERTY

``` r
mod.povs <- coxph(abuse.surv~outofhome+program+age+minority+subst.abuse+crim.hist+first+strata(poverty), data=dat.new)
summary(mod.povs)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty), data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                     coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.47900   1.61445  0.15283  3.134  0.00172 ** 
    ## programStandard  0.73167   2.07854  0.12461  5.871 4.32e-09 ***
    ## age             -0.21519   0.80639  0.02694 -7.987 1.39e-15 ***
    ## minority         0.23508   1.26501  0.13246  1.775  0.07595 .  
    ## subst.abuse      0.48892   1.63055  0.18916  2.585  0.00975 ** 
    ## crim.hist        0.81641   2.26237  0.17200  4.747 2.07e-06 ***
    ## first           -0.43999   0.64404  0.13617 -3.231  0.00123 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.6145     0.6194    1.1966    2.1783
    ## programStandard    2.0785     0.4811    1.6281    2.6536
    ## age                0.8064     1.2401    0.7649    0.8501
    ## minority           1.2650     0.7905    0.9758    1.6400
    ## subst.abuse        1.6306     0.6133    1.1254    2.3624
    ## crim.hist          2.2624     0.4420    1.6149    3.1693
    ## first              0.6440     1.5527    0.4932    0.8411
    ## 
    ## Concordance= 0.696  (se = 0.017 )
    ## Likelihood ratio test= 135.2  on 7 df,   p=<2e-16
    ## Wald test            = 129.6  on 7 df,   p=<2e-16
    ## Score (logrank) test = 134.9  on 7 df,   p=<2e-16

###### TEST REMOVAL VS STRATIFICATION

``` r
mod.cox.removepov <-  coxph(abuse.surv ~ outofhome + program + age + minority + 
  subst.abuse + crim.hist + first, data = dat.new)
summary(mod.cox.removepov)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                     coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.44866   1.56621  0.15215  2.949  0.00319 ** 
    ## programStandard  0.69043   1.99457  0.12316  5.606 2.07e-08 ***
    ## age             -0.21627   0.80552  0.02661 -8.128 4.38e-16 ***
    ## minority         0.13342   1.14273  0.12371  1.079  0.28080    
    ## subst.abuse      0.51454   1.67287  0.18852  2.729  0.00635 ** 
    ## crim.hist        0.85374   2.34841  0.17172  4.972 6.63e-07 ***
    ## first           -0.44484   0.64092  0.13585 -3.274  0.00106 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.5662     0.6385    1.1624    2.1104
    ## programStandard    1.9946     0.5014    1.5668    2.5391
    ## age                0.8055     1.2414    0.7646    0.8486
    ## minority           1.1427     0.8751    0.8967    1.4563
    ## subst.abuse        1.6729     0.5978    1.1561    2.4207
    ## crim.hist          2.3484     0.4258    1.6773    3.2880
    ## first              0.6409     1.5602    0.4911    0.8365
    ## 
    ## Concordance= 0.696  (se = 0.016 )
    ## Likelihood ratio test= 133.9  on 7 df,   p=<2e-16
    ## Wald test            = 130.1  on 7 df,   p=<2e-16
    ## Score (logrank) test = 134.8  on 7 df,   p=<2e-16

``` r
mod.cox.removepov$loglik
```

    ## [1] -1563.341 -1496.402

``` r
mod.povs$loglik
```

    ## [1] -1386.314 -1318.711

``` r
extractAIC(mod.cox.removepov)
```

    ## [1]    7.000 3006.803

``` r
extractAIC(mod.povs) #lower AIC
```

    ## [1]    7.000 2651.421

##### Additionally, the model was stratified by both the region of the metropolitan

area in which the index event occurred (North and South) and the agency
to which the index event was reported (State or Local). I

###### COX MOD STRATIFIED BY REGION

``` r
mod.cox.region <- update(mod.povs, ~.+strata(region))
summary(mod.cox.region)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region), 
    ##     data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                    coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.4984    1.6461   0.1540  3.236 0.001212 ** 
    ## programStandard  0.7312    2.0775   0.1250  5.850 4.93e-09 ***
    ## age             -0.2184    0.8038   0.0271 -8.060 7.61e-16 ***
    ## minority         0.2342    1.2639   0.1335  1.755 0.079331 .  
    ## subst.abuse      0.4965    1.6429   0.1908  2.603 0.009254 ** 
    ## crim.hist        0.8065    2.2400   0.1731  4.660 3.16e-06 ***
    ## first           -0.4595    0.6316   0.1371 -3.350 0.000807 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.6461     0.6075    1.2172    2.2261
    ## programStandard    2.0775     0.4813    1.6261    2.6542
    ## age                0.8038     1.2441    0.7622    0.8476
    ## minority           1.2639     0.7912    0.9730    1.6420
    ## subst.abuse        1.6429     0.6087    1.1304    2.3878
    ## crim.hist          2.2400     0.4464    1.5957    3.1446
    ## first              0.6316     1.5833    0.4827    0.8264
    ## 
    ## Concordance= 0.7  (se = 0.017 )
    ## Likelihood ratio test= 136  on 7 df,   p=<2e-16
    ## Wald test            = 129  on 7 df,   p=<2e-16
    ## Score (logrank) test = 134.9  on 7 df,   p=<2e-16

``` r
anova(mod.cox.s, mod.cox.region)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + poverty + subst.abuse + crim.hist + first
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region)
    ##    loglik  Chisq Df Pr(>|Chi|)    
    ## 1 -1494.2                         
    ## 2 -1127.3 733.84  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(mod.cox.s, mod.cox.region, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + poverty + subst.abuse + crim.hist + first
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region)
    ##    loglik  Chisq Df Pr(>|Chi|)    
    ## 1 -1494.2                         
    ## 2 -1127.3 733.84  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod.cox.region$loglik
```

    ## [1] -1195.291 -1127.293

###### COX MOD STRATIFIED BY AGENCY

``` r
mod.cox.agency <- update(mod.povs, ~.+strata(agency))
summary(mod.cox.agency)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(agency), 
    ##     data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                     coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.48244   1.62003  0.15614  3.090  0.00200 ** 
    ## programStandard  0.73953   2.09495  0.12501  5.916  3.3e-09 ***
    ## age             -0.21254   0.80853  0.02691 -7.900  2.8e-15 ***
    ## minority         0.21783   1.24338  0.13289  1.639  0.10117    
    ## subst.abuse      0.47690   1.61108  0.18904  2.523  0.01164 *  
    ## crim.hist        0.84632   2.33106  0.17366  4.874  1.1e-06 ***
    ## first           -0.44113   0.64331  0.13634 -3.235  0.00121 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.6200     0.6173    1.1929    2.2000
    ## programStandard    2.0950     0.4773    1.6397    2.6766
    ## age                0.8085     1.2368    0.7670    0.8523
    ## minority           1.2434     0.8043    0.9583    1.6133
    ## subst.abuse        1.6111     0.6207    1.1123    2.3336
    ## crim.hist          2.3311     0.4290    1.6586    3.2762
    ## first              0.6433     1.5545    0.4925    0.8404
    ## 
    ## Concordance= 0.698  (se = 0.018 )
    ## Likelihood ratio test= 134  on 7 df,   p=<2e-16
    ## Wald test            = 128  on 7 df,   p=<2e-16
    ## Score (logrank) test = 133.8  on 7 df,   p=<2e-16

``` r
anova(mod.cox.s,mod.cox.agency, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + poverty + subst.abuse + crim.hist + first
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(agency)
    ##    loglik  Chisq Df Pr(>|Chi|)    
    ## 1 -1494.2                         
    ## 2 -1137.9 712.64  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(mod.cox.region,mod.cox.agency)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(agency)
    ##    loglik  Chisq Df Pr(>|Chi|)    
    ## 1 -1127.3                         
    ## 2 -1137.9 21.201  0  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod.cox.agency$loglik
```

    ## [1] -1204.893 -1137.893

###### COX MOD STRATIFIED BY REGION AND AGENCY

``` r
mod.cox.RA <- update(mod.povs, ~.+strata(region)+strata(agency))
summary(mod.cox.RA)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency), data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                    coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.4539    1.5744   0.1587  2.860  0.00423 ** 
    ## programStandard  0.7302    2.0755   0.1256  5.812 6.17e-09 ***
    ## age             -0.2159    0.8058   0.0271 -7.967 1.62e-15 ***
    ## minority         0.2171    1.2425   0.1344  1.616  0.10610    
    ## subst.abuse      0.4920    1.6355   0.1916  2.567  0.01026 *  
    ## crim.hist        0.8662    2.3779   0.1764  4.910 9.10e-07 ***
    ## first           -0.4417    0.6430   0.1381 -3.198  0.00138 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.5744     0.6352    1.1536    2.1488
    ## programStandard    2.0754     0.4818    1.6225    2.6549
    ## age                0.8058     1.2410    0.7641    0.8498
    ## minority           1.2425     0.8048    0.9548    1.6169
    ## subst.abuse        1.6355     0.6114    1.1234    2.3812
    ## crim.hist          2.3780     0.4205    1.6828    3.3602
    ## first              0.6429     1.5553    0.4905    0.8428
    ## 
    ## Concordance= 0.703  (se = 0.018 )
    ## Likelihood ratio test= 133.4  on 7 df,   p=<2e-16
    ## Wald test            = 126.1  on 7 df,   p=<2e-16
    ## Score (logrank) test = 132.7  on 7 df,   p=<2e-16

``` r
anova(mod.cox.s, mod.cox.RA)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + poverty + subst.abuse + crim.hist + first
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##     loglik  Chisq Df Pr(>|Chi|)    
    ## 1 -1494.21                         
    ## 2  -948.42 1091.6  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod.cox.RA$loglik
```

    ## [1] -1015.1454  -948.4248

``` r
anova(mod.povs, mod.cox.RA, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##     loglik  Chisq Df Pr(>|Chi|)    
    ## 1 -1318.71                         
    ## 2  -948.42 740.57  0  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###### (RENAME)

``` r
mod.cox.main <- update(mod.povs, ~.+strata(region)+strata(agency))
summary(mod.cox.main)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency), data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                    coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome        0.4539    1.5744   0.1587  2.860  0.00423 ** 
    ## programStandard  0.7302    2.0755   0.1256  5.812 6.17e-09 ***
    ## age             -0.2159    0.8058   0.0271 -7.967 1.62e-15 ***
    ## minority         0.2171    1.2425   0.1344  1.616  0.10610    
    ## subst.abuse      0.4920    1.6355   0.1916  2.567  0.01026 *  
    ## crim.hist        0.8662    2.3779   0.1764  4.910 9.10e-07 ***
    ## first           -0.4417    0.6430   0.1381 -3.198  0.00138 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                 exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome          1.5744     0.6352    1.1536    2.1488
    ## programStandard    2.0754     0.4818    1.6225    2.6549
    ## age                0.8058     1.2410    0.7641    0.8498
    ## minority           1.2425     0.8048    0.9548    1.6169
    ## subst.abuse        1.6355     0.6114    1.1234    2.3812
    ## crim.hist          2.3780     0.4205    1.6828    3.3602
    ## first              0.6429     1.5553    0.4905    0.8428
    ## 
    ## Concordance= 0.703  (se = 0.018 )
    ## Likelihood ratio test= 133.4  on 7 df,   p=<2e-16
    ## Wald test            = 126.1  on 7 df,   p=<2e-16
    ## Score (logrank) test = 132.7  on 7 df,   p=<2e-16

``` r
cox.zph(mod.cox.main)
```

    ##              chisq df    p
    ## outofhome   1.6216  1 0.20
    ## program     0.0675  1 0.79
    ## age         0.0207  1 0.89
    ## minority    0.2618  1 0.61
    ## subst.abuse 1.3773  1 0.24
    ## crim.hist   0.1611  1 0.69
    ## first       0.4105  1 0.52
    ## GLOBAL      4.2844  7 0.75

##### Interactions between the intervention program and other covariates were examined by performing a likelihood ratio test comparing the model including the main effects of the intervention program, the covariate, and the interaction term of the intervention program and the covariate to the reduced model without the covariate and interaction term. The likelihood ratio tests were not significant for the interaction between program and out of home (p = .793), program and age (p = .538), program and sex (p = .681), program and minority race (p = .759), program and substance abuse (p = .971), program and criminal history (p = .235), or program and first contact with CPS (p = .793).

##### INTERACTIONS

###### PROGRAM X AGE

``` r
cox.int.age <- update(mod.cox.main, ~.+program*age)
summary(cox.int.age)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + program:age, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                         coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome            0.45444   1.57530  0.15840  2.869  0.00412 ** 
    ## programStandard      0.93414   2.54503  0.35434  2.636  0.00838 ** 
    ## age                 -0.19840   0.82004  0.03901 -5.086 3.65e-07 ***
    ## minority             0.21767   1.24318  0.13442  1.619  0.10538    
    ## subst.abuse          0.49884   1.64680  0.19201  2.598  0.00938 ** 
    ## crim.hist            0.87669   2.40292  0.17733  4.944 7.66e-07 ***
    ## first               -0.44509   0.64077  0.13822 -3.220  0.00128 ** 
    ## programStandard:age -0.03278   0.96775  0.05316 -0.617  0.53751    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                     exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome              1.5753     0.6348    1.1549    2.1488
    ## programStandard        2.5450     0.3929    1.2708    5.0969
    ## age                    0.8200     1.2195    0.7597    0.8852
    ## minority               1.2432     0.8044    0.9552    1.6179
    ## subst.abuse            1.6468     0.6072    1.1303    2.3993
    ## crim.hist              2.4029     0.4162    1.6974    3.4016
    ## first                  0.6408     1.5606    0.4887    0.8401
    ## programStandard:age    0.9678     1.0333    0.8720    1.0740
    ## 
    ## Concordance= 0.704  (se = 0.018 )
    ## Likelihood ratio test= 133.8  on 8 df,   p=<2e-16
    ## Wald test            = 128.8  on 8 df,   p=<2e-16
    ## Score (logrank) test = 141.1  on 8 df,   p=<2e-16

``` r
anova(mod.cox.main, cox.int.age)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:age
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.24 0.3793  1      0.538

``` r
anova(mod.cox.main, cox.int.age, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:age
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.24 0.3793  1      0.538

###### PROGRAM X OUTOFHOME

``` r
cox.int.out <- update(mod.cox.main, ~.+program*outofhome)
summary(cox.int.out)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + outofhome:program, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                               coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome                  0.51886   1.68011  0.21928  2.366  0.01797 *  
    ## programStandard            0.75937   2.13693  0.14354  5.290 1.22e-07 ***
    ## age                       -0.21602   0.80572  0.02711 -7.969 1.60e-15 ***
    ## minority                   0.21694   1.24228  0.13434  1.615  0.10633    
    ## subst.abuse                0.50336   1.65427  0.19339  2.603  0.00925 ** 
    ## crim.hist                  0.87364   2.39562  0.17731  4.927 8.34e-07 ***
    ## first                     -0.43598   0.64663  0.13872 -3.143  0.00167 ** 
    ## outofhome:programStandard -0.12555   0.88201  0.29633 -0.424  0.67180    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                           exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome                    1.6801     0.5952    1.0932    2.5822
    ## programStandard              2.1369     0.4680    1.6129    2.8312
    ## age                          0.8057     1.2411    0.7640    0.8497
    ## minority                     1.2423     0.8050    0.9547    1.6165
    ## subst.abuse                  1.6543     0.6045    1.1324    2.4167
    ## crim.hist                    2.3956     0.4174    1.6924    3.3911
    ## first                        0.6466     1.5465    0.4927    0.8487
    ## outofhome:programStandard    0.8820     1.1338    0.4934    1.5766
    ## 
    ## Concordance= 0.703  (se = 0.018 )
    ## Likelihood ratio test= 133.6  on 8 df,   p=<2e-16
    ## Wald test            = 125.5  on 8 df,   p=<2e-16
    ## Score (logrank) test = 132.9  on 8 df,   p=<2e-16

``` r
anova(mod.cox.main, cox.int.out)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + outofhome:program
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.34 0.1794  1     0.6719

``` r
anova(mod.cox.main, cox.int.out, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + outofhome:program
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.34 0.1794  1     0.6719

###### PROGRAM X SEX

``` r
cox.int.sex <- update(mod.cox.main, ~.+program*sex)
summary(cox.int.sex)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + sex + program:sex, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                             coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome                0.44274   1.55696  0.15919  2.781  0.00541 ** 
    ## programStandard          0.67943   1.97275  0.18550  3.663  0.00025 ***
    ## age                     -0.21627   0.80551  0.02716 -7.962 1.70e-15 ***
    ## minority                 0.20900   1.23244  0.13459  1.553  0.12046    
    ## subst.abuse              0.50579   1.65829  0.19214  2.632  0.00848 ** 
    ## crim.hist                0.87358   2.39548  0.17701  4.935 8.01e-07 ***
    ## first                   -0.42733   0.65225  0.13906 -3.073  0.00212 ** 
    ## sexMale                  0.04980   1.05106  0.18754  0.266  0.79059    
    ## programStandard:sexMale  0.08868   1.09274  0.25060  0.354  0.72342    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                         exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome                  1.5570     0.6423    1.1397    2.1271
    ## programStandard            1.9728     0.5069    1.3714    2.8377
    ## age                        0.8055     1.2414    0.7638    0.8496
    ## minority                   1.2324     0.8114    0.9467    1.6045
    ## subst.abuse                1.6583     0.6030    1.1379    2.4166
    ## crim.hist                  2.3955     0.4175    1.6932    3.3889
    ## first                      0.6522     1.5332    0.4966    0.8566
    ## sexMale                    1.0511     0.9514    0.7278    1.5180
    ## programStandard:sexMale    1.0927     0.9151    0.6687    1.7858
    ## 
    ## Concordance= 0.703  (se = 0.018 )
    ## Likelihood ratio test= 134.2  on 9 df,   p=<2e-16
    ## Wald test            = 127.4  on 9 df,   p=<2e-16
    ## Score (logrank) test = 134.6  on 9 df,   p=<2e-16

``` r
anova(mod.cox.main, cox.int.sex)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + sex + program:sex
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.04 0.7689  2     0.6808

``` r
anova(mod.cox.main, cox.int.sex, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + sex + program:sex
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.04 0.7689  2     0.6808

###### PROGRAM X MINORITY

``` r
cox.int.min <- update(mod.cox.main, ~.+program*minority)
summary(cox.int.min)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + program:minority, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                              coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome                 0.45403   1.57465  0.15873  2.860  0.00423 ** 
    ## programStandard           0.69863   2.01100  0.16224  4.306 1.66e-05 ***
    ## age                      -0.21637   0.80544  0.02716 -7.967 1.62e-15 ***
    ## minority                  0.17479   1.19100  0.19309  0.905  0.36535    
    ## subst.abuse               0.49523   1.64087  0.19191  2.581  0.00986 ** 
    ## crim.hist                 0.87098   2.38926  0.17708  4.919 8.72e-07 ***
    ## first                    -0.44010   0.64397  0.13823 -3.184  0.00145 ** 
    ## programStandard:minority  0.07727   1.08033  0.25229  0.306  0.75940    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                          exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome                   1.5746     0.6351    1.1536    2.1493
    ## programStandard             2.0110     0.4973    1.4632    2.7638
    ## age                         0.8054     1.2416    0.7637    0.8495
    ## minority                    1.1910     0.8396    0.8157    1.7389
    ## subst.abuse                 1.6409     0.6094    1.1265    2.3902
    ## crim.hist                   2.3893     0.4185    1.6886    3.3806
    ## first                       0.6440     1.5529    0.4911    0.8444
    ## programStandard:minority    1.0803     0.9256    0.6589    1.7714
    ## 
    ## Concordance= 0.703  (se = 0.018 )
    ## Likelihood ratio test= 133.5  on 8 df,   p=<2e-16
    ## Wald test            = 126.3  on 8 df,   p=<2e-16
    ## Score (logrank) test = 133  on 8 df,   p=<2e-16

``` r
anova(mod.cox.main, cox.int.min)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:minority
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.38 0.0938  1     0.7593

``` r
anova(mod.cox.main, cox.int.min, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:minority
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.38 0.0938  1     0.7593

###### PROGRAM X SUBST.ABUSE

``` r
cox.int.subst <- update(mod.cox.main, ~.+program*subst.abuse)
summary(cox.int.subst)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + program:subst.abuse, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                                coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome                    0.4532    1.5733   0.1597  2.837  0.00455 ** 
    ## programStandard              0.7285    2.0720   0.1334  5.463 4.68e-08 ***
    ## age                         -0.2159    0.8058   0.0271 -7.966 1.63e-15 ***
    ## minority                     0.2171    1.2424   0.1344  1.615  0.10623    
    ## subst.abuse                  0.4822    1.6197   0.3270  1.475  0.14033    
    ## crim.hist                    0.8665    2.3785   0.1765  4.908 9.19e-07 ***
    ## first                       -0.4413    0.6432   0.1385 -3.186  0.00144 ** 
    ## programStandard:subst.abuse  0.0145    1.0146   0.3942  0.037  0.97067    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                             exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome                      1.5733     0.6356    1.1504    2.1517
    ## programStandard                2.0720     0.4826    1.5955    2.6910
    ## age                            0.8058     1.2410    0.7641    0.8498
    ## minority                       1.2424     0.8049    0.9548    1.6168
    ## subst.abuse                    1.6197     0.6174    0.8532    3.0748
    ## crim.hist                      2.3785     0.4204    1.6828    3.3618
    ## first                          0.6432     1.5547    0.4903    0.8438
    ## programStandard:subst.abuse    1.0146     0.9856    0.4685    2.1972
    ## 
    ## Concordance= 0.703  (se = 0.018 )
    ## Likelihood ratio test= 133.4  on 8 df,   p=<2e-16
    ## Wald test            = 126  on 8 df,   p=<2e-16
    ## Score (logrank) test = 132.7  on 8 df,   p=<2e-16

``` r
anova(mod.cox.main,cox.int.subst)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:subst.abuse
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.42 0.0014  1     0.9706

``` r
anova(mod.cox.main, cox.int.subst, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:subst.abuse
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.42 0.0014  1     0.9706

###### PROGRAM X CRIMINAL HISTORY

``` r
cox.int.crim <- update(mod.cox.main, ~.+program*crim.hist)
summary(cox.int.crim)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + program:crim.hist, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                               coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome                  0.46097   1.58561  0.15863  2.906 0.003662 ** 
    ## programStandard            0.79187   2.20751  0.13637  5.807 6.37e-09 ***
    ## age                       -0.21342   0.80782  0.02707 -7.883 3.19e-15 ***
    ## minority                   0.20400   1.22630  0.13465  1.515 0.129754    
    ## subst.abuse                0.47314   1.60502  0.19231  2.460 0.013884 *  
    ## crim.hist                  1.09514   2.98959  0.25354  4.319 1.56e-05 ***
    ## first                     -0.45873   0.63209  0.13888 -3.303 0.000956 ***
    ## programStandard:crim.hist -0.42162   0.65599  0.35417 -1.190 0.233871    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                           exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome                    1.5856     0.6307    1.1619    2.1638
    ## programStandard              2.2075     0.4530    1.6898    2.8839
    ## age                          0.8078     1.2379    0.7661    0.8518
    ## minority                     1.2263     0.8155    0.9419    1.5967
    ## subst.abuse                  1.6050     0.6230    1.1010    2.3398
    ## crim.hist                    2.9896     0.3345    1.8189    4.9139
    ## first                        0.6321     1.5821    0.4815    0.8298
    ## programStandard:crim.hist    0.6560     1.5244    0.3277    1.3133
    ## 
    ## Concordance= 0.702  (se = 0.018 )
    ## Likelihood ratio test= 134.8  on 8 df,   p=<2e-16
    ## Wald test            = 126  on 8 df,   p=<2e-16
    ## Score (logrank) test = 132.9  on 8 df,   p=<2e-16

``` r
anova(mod.cox.main,cox.int.crim)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:crim.hist
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -947.72 1.4122  1     0.2347

``` r
anova(mod.cox.main, cox.int.crim, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:crim.hist
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -947.72 1.4122  1     0.2347

###### PROGRAM X FIRST CONTACT WITH CPS

``` r
cox.int.first <- update(mod.cox.main, ~.+program*first)
summary(cox.int.first)
```

    ## Call:
    ## coxph(formula = abuse.surv ~ outofhome + program + age + minority + 
    ##     subst.abuse + crim.hist + first + strata(poverty) + strata(region) + 
    ##     strata(agency) + program:first, data = dat.new)
    ## 
    ##   n= 675, number of events= 277 
    ## 
    ##                           coef exp(coef) se(coef)      z Pr(>|z|)    
    ## outofhome              0.45789   1.58073  0.15942  2.872  0.00408 ** 
    ## programStandard        0.75116   2.11946  0.14907  5.039 4.68e-07 ***
    ## age                   -0.21613   0.80563  0.02712 -7.969 1.60e-15 ***
    ## minority               0.21683   1.24213  0.13441  1.613  0.10671    
    ## subst.abuse            0.48883   1.63041  0.19203  2.546  0.01091 *  
    ## crim.hist              0.86119   2.36598  0.17749  4.852 1.22e-06 ***
    ## first                 -0.39993   0.67036  0.20995 -1.905  0.05680 .  
    ## programStandard:first -0.07335   0.92928  0.27959 -0.262  0.79305    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                       exp(coef) exp(-coef) lower .95 upper .95
    ## outofhome                1.5807     0.6326    1.1565    2.1605
    ## programStandard          2.1195     0.4718    1.5825    2.8387
    ## age                      0.8056     1.2413    0.7639    0.8496
    ## minority                 1.2421     0.8051    0.9545    1.6165
    ## subst.abuse              1.6304     0.6133    1.1190    2.3755
    ## crim.hist                2.3660     0.4227    1.6708    3.3503
    ## first                    0.6704     1.4917    0.4442    1.0116
    ## programStandard:first    0.9293     1.0761    0.5372    1.6074
    ## 
    ## Concordance= 0.702  (se = 0.018 )
    ## Likelihood ratio test= 133.5  on 8 df,   p=<2e-16
    ## Wald test            = 126.6  on 8 df,   p=<2e-16
    ## Score (logrank) test = 134.1  on 8 df,   p=<2e-16

``` r
anova(mod.cox.main,cox.int.first)
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:first
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.39 0.0687  1     0.7932

``` r
anova(mod.cox.main, cox.int.first, test = "Chisq")
```

    ## Analysis of Deviance Table
    ##  Cox model: response is  abuse.surv
    ##  Model 1: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency)
    ##  Model 2: ~ outofhome + program + age + minority + subst.abuse + crim.hist + first + strata(poverty) + strata(region) + strata(agency) + program:first
    ##    loglik  Chisq Df Pr(>|Chi|)
    ## 1 -948.42                     
    ## 2 -948.39 0.0687  1     0.7932

##### The full model includes the following covariates: out of home indicator, intervention program, age, minority race, indicator of substance abuse in the household, indicator of criminal history in the household, indicator of whether the index event was the first contact with CPS, and the following stratifying variables: poverty, region, and agency.

###### MODEL SUMMARY

``` r
tbl_regression(mod.cox.main, exponentiate = T)%>%
  modify_caption("Table 3: Time-Dependent Cox Model Summary<br>Stratified by Poverty, Region, and Agency")
```

<div id="fvtjbntzpb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fvtjbntzpb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#fvtjbntzpb thead, #fvtjbntzpb tbody, #fvtjbntzpb tfoot, #fvtjbntzpb tr, #fvtjbntzpb td, #fvtjbntzpb th {
  border-style: none;
}
&#10;#fvtjbntzpb p {
  margin: 0;
  padding: 0;
}
&#10;#fvtjbntzpb .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#fvtjbntzpb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#fvtjbntzpb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#fvtjbntzpb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#fvtjbntzpb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#fvtjbntzpb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#fvtjbntzpb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#fvtjbntzpb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#fvtjbntzpb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#fvtjbntzpb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#fvtjbntzpb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#fvtjbntzpb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#fvtjbntzpb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#fvtjbntzpb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#fvtjbntzpb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fvtjbntzpb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#fvtjbntzpb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#fvtjbntzpb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#fvtjbntzpb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fvtjbntzpb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#fvtjbntzpb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fvtjbntzpb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#fvtjbntzpb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fvtjbntzpb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fvtjbntzpb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fvtjbntzpb .gt_left {
  text-align: left;
}
&#10;#fvtjbntzpb .gt_center {
  text-align: center;
}
&#10;#fvtjbntzpb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#fvtjbntzpb .gt_font_normal {
  font-weight: normal;
}
&#10;#fvtjbntzpb .gt_font_bold {
  font-weight: bold;
}
&#10;#fvtjbntzpb .gt_font_italic {
  font-style: italic;
}
&#10;#fvtjbntzpb .gt_super {
  font-size: 65%;
}
&#10;#fvtjbntzpb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#fvtjbntzpb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#fvtjbntzpb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#fvtjbntzpb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#fvtjbntzpb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#fvtjbntzpb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#fvtjbntzpb .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption>Table 3: Time-Dependent Cox Model Summary<br>Stratified by Poverty, Region, and Agency</caption>
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;HR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>HR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">outofhome</td>
<td headers="estimate" class="gt_row gt_center">1.57</td>
<td headers="ci" class="gt_row gt_center">1.15, 2.15</td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">program</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Pilot</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Standard</td>
<td headers="estimate" class="gt_row gt_center">2.08</td>
<td headers="ci" class="gt_row gt_center">1.62, 2.65</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">age</td>
<td headers="estimate" class="gt_row gt_center">0.81</td>
<td headers="ci" class="gt_row gt_center">0.76, 0.85</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">minority</td>
<td headers="estimate" class="gt_row gt_center">1.24</td>
<td headers="ci" class="gt_row gt_center">0.95, 1.62</td>
<td headers="p.value" class="gt_row gt_center">0.11</td></tr>
    <tr><td headers="label" class="gt_row gt_left">subst.abuse</td>
<td headers="estimate" class="gt_row gt_center">1.64</td>
<td headers="ci" class="gt_row gt_center">1.12, 2.38</td>
<td headers="p.value" class="gt_row gt_center">0.010</td></tr>
    <tr><td headers="label" class="gt_row gt_left">crim.hist</td>
<td headers="estimate" class="gt_row gt_center">2.38</td>
<td headers="ci" class="gt_row gt_center">1.68, 3.36</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">first</td>
<td headers="estimate" class="gt_row gt_center">0.64</td>
<td headers="ci" class="gt_row gt_center">0.49, 0.84</td>
<td headers="p.value" class="gt_row gt_center">0.001</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> HR = Hazard Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

##### Based on the results of the model summary, the intervention program was a highly significant predictor of experiencing a secondary occurrence of abuse (p \< .001). Children enrolled in the Standard intervention program were twice as likely on average to experience secondary abuse events compared to children enrolled in the Pilot intervention program while holding all other covariates constant.Age and indication that the index event was the first CPS contact for the child were also associated with lower risk of secondary occurrences of abuse. For each additional year of age, the risk of secondary occurrence of abuse decreased by 19% on average while holding all other covariates constant (HR = 0.81, \[95% CI 0.76, 0.85\]). Children for whom the index event was the first CPS contact were 36% less likely to experience secondary occurrences of abuse on average while holding all other covariates constant (HR = 0.64, \[95% CI 0.49, 0.84\]).

###### SURVIVAL & CUMULATIVE HAZARD PLOTS BY PROGRAM

``` r
progfit <- survfit(abuse.surv~dat.new$program)
ggsurvplot(progfit, data=dat.new, ggtheme = theme_bw(), legend.title=element_blank()) 
```

![](CPS_Pilot_Analysis_GH_files/figure-gfm/Survival%20&%20Hazard%20Plots-1.png)<!-- -->

``` r
ggsurvplot(progfit, data=dat.new, ggtheme = theme_bw(), fun = "cumhaz",
           axis.title = element_text(size=14),
           axis.text = element_text(size = 12),
           legend.title=element_blank() 
)
```
![Survival Plot]([Survival & Hazard Plots-1.png](https://github.com/a0toml01/PortfolioProjects/blob/ac067b6661f5162d8b758c0a8670907fabbea50b/Survival%20%26%20Hazard%20Plots-1.png))
![](CPS_Pilot_Analysis_GH_files/figure-gfm/Survival%20&%20Hazard%20Plots-2.png)<!-- -->
