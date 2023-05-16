
# Subscreen <img src='inst/www/SubgroupExplorer_Logo_final.png' align="right" height="100">

#### May 16, 2022 
<!-- badges: start -->

<!--[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) -->

<!-- badges: end -->

***subscreen*** (***sub***group ***screen***ing) package has been developed to systematically analyze data, e.g., from clinical trials, for subgroup effects and visualize the outcome for all evaluated subgroups simultanously.
The visualization is done by a shiny application called subgroup explorer. Typically shiny applications are hosted on a dedicated shiny server, but due to the sensitivity of patient data in clinical trials, which are usually protected by informed consents, the upload of this data to an external server is prohibited. Therefore we provide our tool as a stand-alone application that can be launched from any local machine on which the data is stored. 

Table of content:
<ul>
  <li> <a href='#chap1'> 1. Description </a></li>
  <li> <a href='#chap2'> 2. Functionality </a></li>
    <li style='padding-left:2em'> <a href='#chap21'> 2.1 `subscreencalc` </a></li>
    <li style='padding-left:2em'> <a href='#chap22'> 2.2 `subscreenvi` </a></li>
    <li style='padding-left:2em'> <a href='#chap23'> 2.3 `subscreenshow` </a></li>
  <li> <a href='#chap3'> 3. Subgroup Explorer </a></li>
    <li style='padding-left:2em'> <a href='#chap31'> 3.1 Explorer </a></li>
    <li style='padding-left:4em'> <a href='#chap311'> 3.1.1 Diagram </a></li>
    <li style='padding-left:4em'> <a href='#chap312'> 3.1.2 Lists </a></li>
    <li style='padding-left:4em'> <a href='#chap313'> 3.1.3 Interaction Plot </a></li>
    <li style='padding-left:4em'> <a href='#chap314'> 3.1.4 Options </a></li>
    <li style='padding-left:2em'> <a href='#chap32'> 3.2 Comparer </a></li>
    <li style='padding-left:2em'> <a href='#chap33'> 3.3 Mosaic </a></li>
    <li style='padding-left:2em'> <a href='#chap34'> 3.4 ASMUS </a></li>
  <li> <a href='#chap4'> 4. Additional information </li>
    <li style='padding-left:2em'> <a href='#chap41'> 4.1 Important points to consider </a></li>
  <li> <a href='#chap5'> 5. Version History </a></li>
</ul>

<div id='chap1'>

## 1. Description

Identifying outcome relevant subgroups has now become as simple as possible! The formerly lengthy and tedious search for the needle in a haystack will be replaced by a single, comprehensive and coherent presentation.

The central result of a subgroup screening is a diagram in which each single dot stands for a subgroup. The diagram may show thousands of them. The position of the dot in the diagram is determined by the sample size of the subgroup and the statistical measure of the treatment effect in that subgroup. The sample size is shown on the horizontal axis while the treatment effect is displayed on the vertical axis. Furthermore, the diagram shows the line of no effect and the overall study results. For small subgroups, which are found on the left side of the plot, larger random deviations from the mean study effect are expected, while for larger subgroups only small deviations from the study mean can be expected to be chance findings.
So for a study with no conspicuous subgroup effects, the dots in the figure are expected to form a kind of funnel. Any deviations from this funnel shape hint to conspicuous subgroups.

<img src='inst/www/subscreenshow_Explorer_Plot_red.png'>

<div id='chap2'>
## 2. Functionality

The subscreen package consists of three major functions, which are `subscreencalc`, `subscreenvi` and `subscreenshow`.
In the first function generates an object of class `SubScreenResult`, which is required for the shiny application.
The second function performs an variable importance calculation via random forests. This calculation is optional and
wil unlock the *Variable importance*-tab in the Subgroup Explorer. The third function starts the shiny application Subgroup Explorer.

<div id='chap21'>

### 2.1 `subscreencalc`

The function subscreencalc returns an list object of class SubscreenResult. This list contains all subgroup information required in the shiny application. The following function parameter can be adjusted:
<pre>
<b>data               </b> dataframe with study data
<b>eval_function      </b> name of the function for data analysis
<b>treat              </b> name of variable in data that contains the treatment identfier, defaults to trtp
<b>subjectid          </b> name of variable in data that contains the subject identifier, defaults to subjid
<b>factors            </b> vector containg the names of variables that define the subgroups, defaults to NULL.
                      If set to NULL, all variables in data are used that are not included in subjectid,
                      treat, and endpoints
<b>min_comb           </b> minimum number of factor combination levels to define subgroups, defaults to 1
<b>max_comb           </b> maximum number of factor combination levels to define subgru
oups, defaults to 3
<b>nkernel            </b> number of kernels for parallelization (defaults to 1)
<b>par_functions      </b> vector of names of functions used in eval_function to be exported
                      to cluster (needed only if nkernel > 1)
<b>verbose            </b> switch on/off output of computational information
<b>factorial          </b> switch on/off calculation of factorial contexts
<b>use_complement     </b> switch on/off calculation of complement subgroups
</pre>

#### data

The input data frame should have one row per subject/patient/observation.
As columns the following are required
  1. variable(s) needed to derive the endpoint/outcome/target variable(s) 
  2. treatment/group/reference variable (only if comparison will be performed)
  3. subgroup factors, i.e. categorized baseline/demographic variables
  
#### eval_function

The input function eval-funtion() needs to be defined by the user.
This function will calculate the endpoint(s) for each subgroup, 
e.g. number, rate, mean, odds ratio, hazard ratio, confidence limit, p-value, ...
The results will be returned as a numerical vector. Each element of the vector represents 
an endpoint (outcome/treatment effect/result).

#### factors

#### min_comb

#### max_comb

#### nkernel

#### verbose

<img src='inst/www/subscreencalc_verbose.png'>

#### factorial

#### use_complement
 
#### Example

## Output

### results$sge

### results$nfactors

The output object of subsreencalc will be the input for subscreenshow.

<div id='chap22'>

### 2.2 `subscreenvi`
The function subscreenvi performs an variable importance calculation via random forests.
For this the package ranger is used.
The following function parameter can be adjusted:
<pre>
<b>data               </b> The data frame containing the dependent and independent variables.
<b>y                  </b> The name of the column in data that contains the dependent variable.
<b>cens               </b> The name of the column in data that contains the censoring variable,
                      if y is an event time (default=NULL).
<b>trt                </b> The name of the column in data that contains the treatment variable (default=NULL).
<b>x                  </b> Vector that contains the names of the columns in data with the independent
                      variables (default=NULL, i.e. all remaining variables)
</pre>

#### data

#### y

#### cens 

#### trt

#### x 

<div id='chap23'>

### 2.3 `subscreenshow`
The function subscreenshow starts the app Subgroup Explorer. The following function parameter can be adjusted:
<pre>
<b>scresults          </b> SubScreenResult object with results from a subscreencalc call
<b>variable_importance</b> variable importance object calculated via subscreenvi to unlock 
                      'variable importance'-tab in the app
<b>host               </b> host name or IP address for shiny display
<b>port               </b> port number for shiny display
<b>asmus_version      </b> select the version of asmus tab used (default: 2) in the app,
                      1 for 'automatic' and 2 for 'advanced'
<b>windowTitle        </b> title which is shown for the browser tab
</pre>

#### scresults

#### variable_importance

#### host

#### port

#### asmus_version

The app itself will be explained in more detailed version in chapter 3.

<div id='chap3'>

## 3. Subgroup Explorer

The app consists of four main tabs, which are Explorer, Comparer, Mosaic and ASMUS (Automatic/Advanced Screening of one- or Multi-factorial Subgroups).

<div id='chap31'>

#### 3.1 Explorer
The Explorer-tab is the main part of the Subgroup Explorer. It can be divided up in four parts. 

The graph in the middle is the main part of the Explorer. It shows a diagram in which each single dot stands for a subgroup. 

By clicking the dots several lists shown up below the plot. They include more information about a selected subgroup.

Right beside the plot an interaction plot appears if a subgroup has an complete (or pseudo-complete) factorial context.
For more details about the concept of a factorial context see the chapter 'Factorial Context'. 

<div id='chap311'>

##### 3.1.1 Diagram
Every subgroup is represented by a single dot in the plot. Subgroups may be defined by a single factor (e.g. sex=female) or by a combination  of different factors (e.g. agegroup=young AND smoker=no AND right-handed=yes). The one-level subgroup will be the most right dot.

<img src='inst/www/subscreenshow_Explorer_Plot_red_hover.png'>

By clicking on a single dot a subgroup is selected and appears in red. For colored points a information box can be shown by using mouse hover. Multiple options are available on the left hand side.

<div id='chap312'>

##### 3.1.2 Lists

Clicking on a dot will lead to a table display at the bottom listing all subgroups in that area (tab "Selected Subgroups"). In a second tab ("Filtered Subgroups") subgroups chosen by the drop-down combo box will be listed.

<img src='inst/www/subscreenshow_Explorer_table_selected.png'>

<img src='inst/www/subscreenshow_Explorer_Plot_gold.png'>

<div id='chap313'>

##### 3.1.3 Interaction Plot

<div id='chap314'>

##### 3.1.4 Options

<img src='inst/www/subscreenshow_variableOptions.png' align = "left">

The drop-down combo boxes allow for switching between different target variable (y-axis), changing the reference variable (x-axis, in general the number of subjects/observations), or selecting a specific subgroub factor and a corresponding value to be highlighted in the plot ('Subgroup Filter'). More information below.

Which level of detail with regard to subgroup factors should be displayed can be chosen in the via the 'Subgroup level(s)'-slider.
The minimun and maximum of this slider can be changed with the parameter `min_comb` and `max_comb` in `subscreencalc()`.
The brigthness of the gray dots coresponds to the number of factors in the graph. Dots with more subgroups are more brighter.

There is also the possibility to change the plot type (y-axis) to logarithmic and set the limits of the target variable.

All Widgets are provided with small help texts which can be shown be hovering over the question mark symbol next to.
At the bottom of the variable Options-tab are help texts which appear by hovering over the symbols.

<img src='inst/www/subscreenshow_Explorer_filter_green.png'>

Also all combination will be highlighted. Combinations will be shown left from this dot. The plot type can be switched between linear (standard) and logarithmic. The range of the y-axis to be displayed can be reduced to zoom in.

<img src='inst/www/subscreenshow_displayOptions.png'>

<img src='inst/www/subscreenshow_colourOptions.png'>


<div id='chap32'>

#### 3.2 Comparer

<div id='chap33'>

#### 3.3 Mosaic

<div id='chap34'>

#### 3.4 ASMUS

<div id='chap4'>
## 4. Additional information 

<div id='chap41'>
#### 4.1 Important points to consider

<div id='chap5'>
## 5. Version History 

<pre>
# subscreen 1.0.0

## Bug fixes subscreencalc

* Fixed the example to make it work properly. Added some data pre-processing and handling of NAs. 
* Fixed the problem with max_comb=1 in combination with nkernel=1. Function sapply needed the option simplify=FALSE to keep the data structure  
* No error anymore if nkernel>1 and par_functions=""

## Bug fixes subscreenshow

* Factor levels 6 to 8 will now be displayed. They had no color assigned before.
* Reference line for overall result is now exact on the right place. The slider input has been removed as this caused inappropriate rounding in some cases.


## Enhancements subscreenshow

* The slider for the y-range is now improved. It will use nice numbers for the range selection. Thanks to Tommy (662787) from StackOverflow for roundDownNice(). And you can actually give a set of numbers you think of being nice in the new parameter NiceNumbers.
* Background shading including marks can be set by the new parameter StripesBGplot. The program will aim for the given number of stripes/marks but the actual display may differ to have nice intervals
* On the x-axis percentages of the total are shown

## Editorial changes

* Updated the description
* Packages shiny and DT are now imports although only needed in subscreenshow 
* Spelling errors corrected
* Changed some internal function and variable names for better readability
* Translated the rest of the German comments into English
* Deleted unused functions and program code
* Removed NA from the subgroup filter drop-down selection

# subscreen 2.0.1

* New Layout
* Added Subscreen Comparer
* Added Subscreen Mosaic
* Added Variable Importance calculation
* Added Colour Options Panel
* Added Display Options Panel
* Added identification of parent subgroups 
* Added option to memorize subgroups

# subscreen 3.0.0

## subscreencalc
* Add parameter 'factorial' for factorial context calculations
* Add parameter 'use_complement' for Subgroup complement calculations

## subscreen.vi
*Added possibility to calculate importance for multiple target variables

## subscreenshow
*Improve coloring running time
*Bug fix in function parents() for descending ID's
*Bug fix 'Infinite option'-loop
* Add 'Factorial Contexts'-calculation
* Add optional 'Subgroup Complement'-calculation
* Add Subscreen ASMUS-tab
*Add Subscreen Logo
*Change default background color
*Add Flexible Plot sizes
*Add Mousehover info for all plots
*Change Table header colour
*Add flexible Plot Legends

##Subscreen Explorer-tab
###Variable Options-tab
*Add logarithmic Slider 

###Importance-tab
*Add 'Select Variable'-option to select target variable
*Bug fix importance color 

###Display Options-tab
*Add 'Adjust point brightness'-option
*Remove 'Choose number of Stripes background'-option
*Add 'Plotting character'-option

###Colour Options
*Add 'Colour for factorial Context'-option
*Remove 'Choose Background Colour (Plot stripes)'-option
*Remove 'Choose font colour'-option

###Plots
*Add interaction Plot

###Tables
*Add 'Factorial Context'-table
*Add 'Subgroup Complement'-table

##Subscreen Comparer-tab
###Sidebarpanel
*Remove 'Subgroup Filter'-option
*Remove 'Subgroup level(s)'-option
###Compare-plots
*Add legend
###Bubble plot
*Add legend

##Subscreen Mosaic-tab
###Sidebarpanel
*Add help text

###Hovertable
*Change font color for hovertable

# subscreen 3.0.1

## Bug fixes subscreenshow

* An update of the package shinyjs required a change in the call of the extendShinyjs function to avoid an error while trying to start subscreenshow. This was corrected.

# subscreen 3.0.2

## General
* Dependency to package V8 removed
* Time report of subscreencalc() including the calculation of the factorial context

## Bug fix subscreenshow()
* reastablish compatibility with R version 3.6.2

## Bug fix Subscreen ASMUS-tab
* label for reference line corrected

# subscreen 3.0.3

## Bug fix Subscreen Comparer-tab
* label for top reference line corrected

## Bug fix Subscreen ASMUS-tab
* fixed initial subgroup information text reactivity after target variable change

# subscreen 3.0.4

## subscreencalc
* optimized calculation of complement included in regular calculation step to reduce computing time
* stepwise verbose output with additional details

# subscreen 3.0.5

## subscreencalc
* supress output of "joining by ..." while running R version 3.X.X
</pre>


  
## Things to consider

There should be no "NA" values in the input data set. If there are values "NA" consider replacing them by "No data" or a certain value. The eval-function() should include exception handling for functions that require  a certain input to assure a valid return value (NA is also valid). For example the coxph() function should only be executed if there is at least one observation in each treatment arm (see example). This can be achieved by using trycatch() or a condition. Otherwise the program will abort with error. 

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("")
```

and start the app with:

``` r
#subscreenshow()
```
