# CGM-project
##Visualizing my your own Medtronic CGM data in an RShiny app.

The RShiny webapp allows a diabetic with a Medtronic continuous glucose monitor (CGM) 
to upload their raw data and analyse it. Medtronic's [Carelink](https://carelink.minimed.eu/) 
website allows users to download their recorded CGM data in csv format which can then
be uploaded to this webapp. The webapp offers users:
* Analysis as far back as the data that they have uploaded (Medtronic's reports only go back as far as three months)
* Analysis between time periods
* The percent of time readings were in range
* Average glucose readings over the course of the day
* Insight into blood sugar control has changed over time
* Insight into blood sugar control varies by day of the week
* Insight into how exercise affects their blood sugar

The dashboard relies on the following R libraries:
* tidyverse
* shiny
* plotly
