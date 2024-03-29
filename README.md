# CGM-project
## Visualizing your own Medtronic CGM data in an RShiny app.

An [RShiny web app](https://firdaleconsulting.shinyapps.io/CGM-app/) that allows a diabetic with a Medtronic continuous glucose monitor (CGM) 
to upload their raw data and analyse it. Medtronic's [Carelink](https://carelink.minimed.eu/) 
website allows users to download their recorded CGM data in csv format which can then
be uploaded to this web app. The web app offers users:
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

I have uploaded two csvs containing my own CGM data to this repository. The one contains 
glucose data in mmol/L and the other in mg/dL, showing that both units of measurement
work with this web app.
