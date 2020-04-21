# AMR_forecast
A quick/dirty forecasting model for incidence of AMR across selected countries

You'll need all files to run the program. Don't forget to update "file_location" at the top to ensure you read the IHME data in. Apart from that, the WHO and World Bank data should read straight in, and US/EU numbers are manually created from specific sources (inc. in comments).

This is a quick and dirty model that computes an ARIMA forecast of recent numbers of multi-drug- and extensively-drug-resistant tuberculosis. It also takes the 'per case' (direct) cost from sources, or computes it as an average from self-reported WHO data. Costs are extrapolated out assuming a three-year-annual average inflation rate, and multiplied by forecasts of cases to arrive at an estimate of total costs to treat drug-resistant TB in 94 countries.

Goes without saying that there's a serious status-quo bias in the model, and forecasts should be taken as illustrative.
