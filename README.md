# United States Drugs Prescription

## Data Source

The data is downloaded from the dataset [Medicare Part D Spending by Drug- Excel Reports including Historical Data 2023](https://data.cms.gov/sites/default/files/2025-05/Medicare%20Part%20D%20Spending%20by%20Drug-Excel%20Reports%20including%20Historical%20Data%20RY25.zip), from [data.cms.gov](https://data.cms.gov/summary-statistics-on-use-and-payments/medicare-medicaid-spending-by-drug/medicare-part-d-spending-by-drug).

Every sub-folder, unzipped, must be placed in the "data" folder to run the script. You should therefore have:
`data/Medicare Part D Spending by Drug DYT2016`
`data/Medicare Part D Spending by Drug DYT2017`
`data/Medicare Part D Spending by Drug DYT2018`
`data/Medicare Part D Spending by Drug DYT2019`
`data/Medicare Part D Spending by Drug DYT2020`
`data/Medicare Part D Spending by Drug DYT2021`
`data/Medicare Part D Spending by Drug DYT2022`
`data/Medicare Part D Spending by Drug DYT2023`

## Data Extraction

The data is then extracted by 'extract_medicare_partd.R', which results in a single .CSV file [medicare_partd_dosage_units_2012_2023.csv]().