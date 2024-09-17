hcap_weighted_summary
================
2024-09-11

``` r
# Load libraries
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}
```

    ## Loading required package: pacman

``` r
p_load("janitor", "broom", "tidyverse", "magrittr", "plyr", "haven", "glue", 
       "survey", "labelled", "gtsummary", "srvyr","gt")
```

``` r
#load data and filter for variables
data <- read.csv('/Users/novak/ZhangYS Dropbox/Shared/HCAP/python file/hrs-hcap-full.csv')
data <- data %>% 
  select(hhid, pn, id_hrs, hrs, mex, wgt,
         binf1csidmental, binf1csidmemory, binf1csidput, binf1csidkept,
         binf1csidfrdname, binf1csidfamname, binf1csidconvers, binf1csidwordfind,
         binf1csidwordwrg, binf1csidpast, binf1csidlastsee, binf1csidlastday,
         binf1csidorient, binf1csidlostout, binf1csidlostin, binf1chores,
         binf1hobby, binf1money, binf1change, binf1bl2feed, bcsid,
         fgcp, fmem, fexf, flang, forient, mmse,
         rage, rfemale, reduc, iage, ifemale, ieduc,
         relation, ispouse, ichild, iothfam, inonfam, coresi, hcap16wgtr, secu, stratum)
```

``` r
summary_data <- data %>% 
  select(rage, rfemale, reduc, iage, ifemale, ieduc, coresi, 
                 ispouse, ichild, iothfam, inonfam, hcap16wgtr, secu, stratum)
  
options(survey.lonely.psu = "adjust")


#create a survey design object 
svydes <- summary_data %>% 
  as_survey_design(
    weights = hcap16wgtr,
    strata = stratum,
    ids = secu,
    nest = TRUE
    )
```

``` r
create_wtgsumm <- function(vars) {
  svydes %>% 
    tbl_svysummary(
      include = all_of(vars),  
      type = list(all_continuous() ~ "continuous2"),
      statistic = list(
        all_continuous() ~ c("{mean}", "({sd})", "{median}", "({p25}, {p75})"),
        all_categorical() ~ "{n} ({p}%)"
      ), 
      digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
    ) %>% 
    bold_labels()  # No add_overall() if no grouping
}
```

``` r
create_wtgsumm(c("rage", "rfemale", "reduc", "iage", "ifemale", "ieduc", "coresi", 
                 "ispouse", "ichild", "iothfam", "inonfam"))
```

<div id="atbwrnvcto" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#atbwrnvcto table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#atbwrnvcto thead, #atbwrnvcto tbody, #atbwrnvcto tfoot, #atbwrnvcto tr, #atbwrnvcto td, #atbwrnvcto th {
  border-style: none;
}
&#10;#atbwrnvcto p {
  margin: 0;
  padding: 0;
}
&#10;#atbwrnvcto .gt_table {
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
&#10;#atbwrnvcto .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#atbwrnvcto .gt_title {
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
&#10;#atbwrnvcto .gt_subtitle {
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
&#10;#atbwrnvcto .gt_heading {
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
&#10;#atbwrnvcto .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#atbwrnvcto .gt_col_headings {
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
&#10;#atbwrnvcto .gt_col_heading {
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
&#10;#atbwrnvcto .gt_column_spanner_outer {
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
&#10;#atbwrnvcto .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#atbwrnvcto .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#atbwrnvcto .gt_column_spanner {
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
&#10;#atbwrnvcto .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#atbwrnvcto .gt_group_heading {
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
&#10;#atbwrnvcto .gt_empty_group_heading {
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
&#10;#atbwrnvcto .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#atbwrnvcto .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#atbwrnvcto .gt_row {
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
&#10;#atbwrnvcto .gt_stub {
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
&#10;#atbwrnvcto .gt_stub_row_group {
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
&#10;#atbwrnvcto .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#atbwrnvcto .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#atbwrnvcto .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atbwrnvcto .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#atbwrnvcto .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#atbwrnvcto .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#atbwrnvcto .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atbwrnvcto .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#atbwrnvcto .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#atbwrnvcto .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#atbwrnvcto .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#atbwrnvcto .gt_footnotes {
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
&#10;#atbwrnvcto .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atbwrnvcto .gt_sourcenotes {
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
&#10;#atbwrnvcto .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#atbwrnvcto .gt_left {
  text-align: left;
}
&#10;#atbwrnvcto .gt_center {
  text-align: center;
}
&#10;#atbwrnvcto .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#atbwrnvcto .gt_font_normal {
  font-weight: normal;
}
&#10;#atbwrnvcto .gt_font_bold {
  font-weight: bold;
}
&#10;#atbwrnvcto .gt_font_italic {
  font-style: italic;
}
&#10;#atbwrnvcto .gt_super {
  font-size: 65%;
}
&#10;#atbwrnvcto .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#atbwrnvcto .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#atbwrnvcto .gt_indent_1 {
  text-indent: 5px;
}
&#10;#atbwrnvcto .gt_indent_2 {
  text-indent: 10px;
}
&#10;#atbwrnvcto .gt_indent_3 {
  text-indent: 15px;
}
&#10;#atbwrnvcto .gt_indent_4 {
  text-indent: 20px;
}
&#10;#atbwrnvcto .gt_indent_5 {
  text-indent: 25px;
}
&#10;#atbwrnvcto .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#atbwrnvcto div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;span class='gt_from_md'&gt;&lt;strong&gt;Characteristic&lt;/strong&gt;&lt;/span&gt;"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;span class='gt_from_md'&gt;&lt;strong&gt;N = 42,691,637&lt;/strong&gt;&lt;/span&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><span class='gt_from_md'><strong>N = 42,691,637</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">rage</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean</td>
<td headers="stat_0" class="gt_row gt_center">74.64</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (SD)</td>
<td headers="stat_0" class="gt_row gt_center">(7.65)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_0" class="gt_row gt_center">73.00</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (Q1, Q3)</td>
<td headers="stat_0" class="gt_row gt_center">(68.00, 80.00)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">rfemale</td>
<td headers="stat_0" class="gt_row gt_center">23,976,890 (56.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">reduc</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean</td>
<td headers="stat_0" class="gt_row gt_center">13.22</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (SD)</td>
<td headers="stat_0" class="gt_row gt_center">(2.99)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_0" class="gt_row gt_center">13.00</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (Q1, Q3)</td>
<td headers="stat_0" class="gt_row gt_center">(12.00, 16.00)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">12,813</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">iage</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean</td>
<td headers="stat_0" class="gt_row gt_center">64.16</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (SD)</td>
<td headers="stat_0" class="gt_row gt_center">(13.83)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_0" class="gt_row gt_center">67.00</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (Q1, Q3)</td>
<td headers="stat_0" class="gt_row gt_center">(57.00, 73.00)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">4,557</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">ifemale</td>
<td headers="stat_0" class="gt_row gt_center">28,546,687 (66.9%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">ieduc</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean</td>
<td headers="stat_0" class="gt_row gt_center">15.02</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (SD)</td>
<td headers="stat_0" class="gt_row gt_center">(10.52)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_0" class="gt_row gt_center">14.00</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    (Q1, Q3)</td>
<td headers="stat_0" class="gt_row gt_center">(12.00, 16.00)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">coresi</td>
<td headers="stat_0" class="gt_row gt_center">22,216,862 (52.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">ispouse</td>
<td headers="stat_0" class="gt_row gt_center">21,208,913 (49.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">ichild</td>
<td headers="stat_0" class="gt_row gt_center">9,550,944 (22.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">iothfam</td>
<td headers="stat_0" class="gt_row gt_center">5,564,788 (13.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">inonfam</td>
<td headers="stat_0" class="gt_row gt_center">6,366,992 (14.9%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span> <span class='gt_from_md'>n (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
