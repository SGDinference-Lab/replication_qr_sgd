
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SGDinference

The **replication_qr_sgd** repo provides the replication files for the
following paper:

- Lee, S., Liao, Y., Seo, M.H. and Shin, Y., 2023. Fast inference for
  quantile regression with tens of millions of observations.
  Forthecoming in *Journal of Econometrics*
  <https://doi.org/10.1016/j.jeconom.2024.105673>.

## Applications

### Data Files

Because of the large file sizes, the Github repo cannot hold the data
files. To replicate the application results, please download a zip file:
[data.zip
(1.6GB)](https://drive.google.com/uc?export=download&id=18MFUXX7nxd_Bq3XHds50GJeKNjHLo6yN).

You need to extract it under the working directory. All data files
should be located at **application/data/**

We use the following 6 files for estimating the model:

- data_ipums_1980.csv
- data_ipums_1990.csv
- data_ipums_2000.csv
- data_ipums_2005.csv
- data_ipums_2010.csv
- data_ipums_2015.csv

These files are contained in the above **data.zip** file. You can also
generate those 6 files from the scratch by running **data_cleaning.R**
file over

- PCEPI.csv
- usa_00005.cbk
- usa_00005.csv.gz
- usa_00005.xml.

All of these files are also contained in the **data.zip** file.

### Source Files

Four R files are located in the **application/src/** folder:

- **fn_estimate_model.R**: conatins an R function to estimate the model.
- **make_table_04.R**: generates Table 4 (summary statistics) in the
  paper.
- **make_tables_and_charts.R**: generates all other Tables and graphs in
  the paper.
- **run_qr_sgd_app.R**: is the main source code to estimate the model.

All results are saved under the **application/results/** folder

## Simulations
