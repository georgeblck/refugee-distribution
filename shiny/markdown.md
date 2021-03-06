[![german flag][2]][1] [![uk flag][4]][3] 
  [1]: http://statistikwerkstatt.org/shiny/refugee-verteilung/shiny/
  [2]: german_flag_small.png (German Version)
  [3]: http://statistikwerkstatt.org/shiny/refugee-distribution/shiny/
  [4]: uk_flag_small.png (English Version)
Acronyms:
* AA: Asylum Applications
* DK: Distribution Key/Quota

## About this visualization
___
In September 2015 the European Commission proposed a plan to relocate 120'000 refugees from Greece, Italy & Hungary to all European countries.
This was supposed to be done by a *mandatory distribution key using objective and quantifiable criteria*, consisting of the following variables and weightings:
> * the size of the population (40%, positive weight),
> * the GDP (40%, positive weight),
> * the average number of past asylum applications (10%, negative weight) and
> * the unemployment rate (10%, negative weight).

The formula has since been corrected by the swiss scientist Philip Grech, as it had some mathematical flaws.

With this visualization you can see the results of applying this quota on different time periods and compare it to the reality.

### Motivation

There are two main reasons for this visualization. We want to:

1. provide an easily accessible overview of the distribution of (accepted) AA in the EU
2. provide a sensible and changeable benchmark of how many/what share of refugees each country should accept

**Summed up, we wanted to put the omnipresent discussion of certain European countries taking too few/many refugees on a more factual basis.**

Naturally, this premise depends a lot on the quality of the data. That is why we were very dilligent in choosing the data sets (see the section further below).

### What this is not about
* **Politics**: The political intricacies and consequences of this proposal are in itself very interesting but are not the focus of this visualization. If you are interested, start by reading the [original press release](http://europa.eu/rapid/press-release_MEMO-15-5597_en.htm).

* **Creating a better formula**: Even after Philip Grechs mathematical corrections, there are still some obvious flaws in calculating a DK this way (e.g. the variable selection). Because of this, a few other different approaches have been proposed. If you are interested, there is a collection of links in the section *Other sources*.

## How to use this visualization
___
#### The Main Options

* The first slider determines both
  * **how many** refugees are to be distributed and
  * on which data basis the DK is calculated: the year **prior** to the chosen time range.
* The second choice concerns the variables shown in the plot or table. There are three options:
  * Show the **EU-wide ratio** of refugees/AA that each country has or should accept
  * Show the **total number** of refugees/AA that each country has or should accept
  * Show the **total number *per 1000 inhabitants*** of refugees/AA that each country has or should accept
  * **Notes on the ratio**:
      * The ratios are good because they are independent of _how many_ AA are to be distributed! However, most should find the absolute numbers more interesting.
      * A countries share of _accepted_ AA can be higher than its share of general AA even if it only accepts a small percentage of AA.
* Adding more countries is self-explanatory

#### Adjusting the weights of the key

The four sliders can be used to adjust the weightings of the DK. The original setting - 0.4/0.4/0.1/0.1 - is the one proposed by the EU. The values and limits of the sliders are reactive to each other and can sometimes cause problems!
It is possible to hide these sliders.

#### The Advanced Options

* Data Sources: Because it is possible, we provide different data sources for the data on Asylum Applications (_not the Accepted ones_).
* Which DK to use: The Grech-Key should always be used as it corrected two big flaws of the EU key (read up in Grech's article). We provide the original only because it is the original.

## Data sources
___
All of the data was accessed between the 11th and 14th of June, 2016 and again on the 10th of October, 2016. We haven't made an effort to incorporate the most recent data but this will happen in the future.

#### Eurostat
The biggest part of data came from [Eurostat](http://ec.europa.eu/eurostat). We used the following data sets:
* Annual **GDP** in current prices in Euro (code: nama_10_gdp)
* **Population** on the 1st of January (code: demo_pjan)
* Annual average of the **unemployment rate** (code: une_rt_a)
* **Asylum Applications** (both All Instances and only First Instances)
	* Monthly data from 2008 up to May 2016 (code: migr_asyappctzm)
	* Monthly data from 1999 till 2007 (code: migr_asyctzm & migr_asyctz)
* Quarterly rounded data on **Accepted Asylum Applications** (code: migr_asydcfstq)

#### UNHCR
To provide another source, data on AA was also taken from the [UNHCR database on Population Statistics](http://popstats.unhcr.org/en/overview). Specifically the monthly data on Asylum-Seekers from 1999 to April 2016.

Regarding the distinction between novel and repeated AA, we quote from the UNHCR webpage:
> Where possible, figures exclude repeat/re-opened asylum applications and applications lodged on appeal or with courts.

#### Missing data (Liechtenstein & Switzerland)

* Unemployment data for Liechtenstein was taken from [here](http://www.llv.li/#/11432/arbeitslosenstatistik)
* Unemployment data for Switzerland was taken from [here](http://www.bfs.admin.ch/bfs/portal/de/index/themen/03/03/blank/data/01.html)
* GDP data for Liechtenstein was taken from [here](http://etab.llv.li/PXWeb/pxweb/de/?rxid=3bd05ee8-fc11-4982-a9af-aae15ce602c4). As the currency is CHF, exchange rates from Eurostat (code: ert_bil_eur_a) were needed.
* GDP data for Switzerland in the year 2015 is still missing!

## Other sources (sorted by relevance)
___
[Press release of the EU on the distribution key with numbers and explanations](http://europa.eu/rapid/press-release_MEMO-15-5597_en.htm)

[The original formula of the EU](http://ec.europa.eu/dgs/home-affairs/what-we-do/policies/european-agenda-migration/proposal-implementation-package/docs/proposal_for_regulation_of_ep_and_council_establishing_a_crisis_relocation_mechanism_-_annex_en.pdf)

[The original journal article by Philip Grech (read this to understand everything)](http://eup.sagepub.com/content/early/2016/05/26/1465116516649244.abstract)

[A recent blog post on the subject, written by Philip Grech](http://blogs.lse.ac.uk/europpblog/2016/08/30/why-the-eu-still-requires-a-fairer-formula-for-distributing-refugees/)

[NY Times article on the subject with nice visualizations (September 2015)](http://www.nytimes.com/interactive/2015/09/04/world/europe/europe-refugee-distribution.html?_r=1)

[The Königstein Key by Daniel Thym](http://verfassungsblog.de/germanys-domestic-koenigstein-quota-system-and-eu-asylum-policy/#.VXdawucRXnp)

[A different but unused distribution key by Luc Bovens & Anna Bartsch](http://www.voxeurop.eu/en/content/article/5041680-towards-fairer-distribution-asylum-seekers)

## Thank-yous
___

We couldn't have done this without

* [R](https://www.r-project.org/)
* [Rstudio](https://www.rstudio.com/)
* [Shiny](http://shiny.rstudio.com/)
* [Stackoverflow](https://stackoverflow.com/)
* [Git](https://git-scm.com/)
* [The EU and eurostat](http://ec.europa.eu/eurostat)
* [The UNHCR](http://www.unhcr.de/)
* [manitu](https://manitu.de/)
* [Philip Grech](http://www.necom.ethz.ch/people/person-detail.html?persid=115265)

## Contact
___

If you have suggestions how the app could be improved, feel free to write us at:

statistikwerkstatt-at-posteo.de
