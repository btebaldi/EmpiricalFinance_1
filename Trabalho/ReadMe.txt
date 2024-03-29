************************************************
*               FILE DESCRIPTION               *
************************************************


Name                                        Description
------------------------------------------- ---------------------------------
Database                                    Directory with all Database files
Plots                                       Directory with all plots and graphics generated by the scripts
Tables                                      Directory with all tables generated by the scripts
ggplot_Acf_Pacf.R                           R function used to plot ACF and PACF graphics
Empirical_Finance_2_Presentation.pdf        PDF presentation
Empirical_Finance_2.pdf                     PDF report
ReadMe.txt                                  (This file) Contains description on files and execution order
Script1_DataPrepare.R                       Script for preparing the data Read the data from IBOV.csv and save it as a RData file
Script2_Local_Level.r                       Script for executing a local level (state space) for filling missing values
Script3_GarchForecast.R                     Script for executing the GARCH models fit and forecasting.
Script4_ForecastEvaluation.R                Script for evaluation of the forecasting.
Script5_HighFrequency.R                     Script for reading and determining the high frequency data
Script6_RealizedVol_Forecast.r              Script for executing the GARCH models fit and forecasting on High frequency data window.
Script7_HighFrequencyEvaluation.r           Script for evaluation of the forecasting using High Frequency data.
Script8_DMTest_mov5_norm.r                  Script for Diebold-Mariano test on the data (Normal distribution, moving window of 5 years)
Script9_DieboldMarianoNormalVsT_252.R       Script for Diebold-Mariano of t-Student vs Normal (252 period re-estimation)
Script9_DieboldMarianoNormalVsT_5.R         Script for Diebold-Mariano of t-Student vs Normal (5 period re-estimation)
Script10_e-Garch Parameters.r               Script for E-GARCH parameters


************************************************
*                  DATABASES                   *
************************************************

The following databases are initially provided with this package

Name
------------------------------------------------
GarchForecast.RData
GarchModels.RData
Garch_HF_Forecast.RData
IBOV.csv
Ibovespa.RData
IBOVESPA15m.xlsx
Ibovespa_HighFreq.RData
Ibovespa_SemBuracao.RData




************************************************
*                  EXECUTION                   *
************************************************

All files may be executed separately if the database folder has all databases listed in the Database section of this file.

>>> ATENTION: Script3_GarchForecast.R TAKES A LONG TIME TO EXECUTE (+/- 6 hours) <<<

Recommended execution order is:
	(01) Script1_DataPrepare.R
	(02) Script2_Local_Level.r
	(03) Script3_GarchForecast.R	   <<<<<<< ATENTION
	(04) Script4_ForecastEvaluation.R
	(05) Script5_HighFrequency.R
	(06) Script6_RealizedVol_Forecast.r
	(07) Script7_HighFrequencyEvaluation.r
	(08) Script8_DMTest_mov5_norm.r
	(09) Script9_DieboldMarianoNormalVsT_252.R
	(10) Script9_DieboldMarianoNormalVsT_5.R
	(11) Script10_e-Garch Parameters.r

