#European Network
Copyright &copy; 2020 Sarah Corinne Kramer

Directory Structure
-------------------
* src
    * mainCode: All code for running forecasting/fitting and for analyzing results
    * syntheticTests: Code for generating synthetic "observations" and running synthetic tests
    * formatTravelData: Code for formatting air and commuting data
* data
    * Absolute humidity data: "ah_Europe_07142019.csv"
    * Country population counts: "popcounts_02-07.csv"
    * Country centroids: "country_centroids_az8.csv" (see "Data Sources" below for information on how to download)
    * Scaled, (sub)type-specific syndromic+ data: "WHO_data_[SUBTYPE]_SCALED.csv"
    * Unscaled, (sub)type-specific syndromic+ data: "WHO_data_[SUBTYPE].csv"
    * Percent positivity data: "WHO_posprop_[SUBTYPE].csv"
    * "scaling_frames/"
        * Consists of csv files containing country- and season-specific scaling factors here (calculated using code in "src/mainCode/format_obs/calculate_subtypeSpecific_scalings.R")
    * "python_init/"
        * (Will be created by "src/mainCode/create_files_for_python.R")
        * Will contain all required initial states/parameters and travel data, stored as txt files for use in python forecasting/fitting code
* results (Note: most of these folders will be automatically created during the forecasting/analysis process)
    * "by_subtype": (Sub)type-specific forecast results will be stored here after processing (see Step 3 below)
    * "fits": Stores results of model fitting (see Step 4)
    * "isolated" and "network": Store forecasting results for all 3 (sub)types combined (Step 5)
    * "plots": Stores all generated figures (Step 6)
    * "temp": Stores initial forecasting results output by python code (Steps 1 and 2)
        * "PROCESS": Results from each individual model run (6 in total) should be moved here for further processing (Step 3)

Running and assessing forecasts
-------------------------------
1. Run "core_code_network.py" for all (sub)types (set variable "strain")
2. Run "core_code_ISOLATED.py" for all (sub)types
3. For each set of results files (6):
    * Move to "results/temp/PROCESS/"
    * Run "process_results.R" (set "model.type" and "strain")
4. Also get model fits by running "core_code_network_trainOnly.py" and "core_code_ISOLATED_trainOnly.py"
5. Run "combine_results_files.R"
6. Run "generate_figs.R"

#### Additional notes
* Further statistical analyses can be conducted using individual code files in "src/mainCode/analyses/"
    * Code for Friedman tests is found in "plot_logScores.R"
* Additional code found in "src/mainCode/explore/" can be used to explore the potential of different ways of parameterizing observational error variance, as well as run quick preliminary comparisons between different model runs
* Code in "src/mainCode/explore_obs/" produces simple plots/analyses of observed geographic transmission patterns

Synthetic Testing
-----------------
1. Generate synthetic "data" using "generate_synthetic_data_RED.R"
    * Note: You may want to go through the generated outbreaks and choose others to fit, or you may want to change inputs.
2. Add random noise to the synthetic data using "add_noise.R"
3. Use code in "create_files_for_python.R" to convert "data" into the form used by the python fitting/forecasting code
4. Run "core_code_network_SYNTH.py"
5. Run "analyze/calculate_betaR0Re.R" to calculate true and inferred values of composite parameters
6. The main synthetic testing results are plotted in "src/mainCode/analyses/synthetic_error.R"
    * However, files in "syntheticTests/analyze/" allow for additional exploration, including examining the geographical transmission patterns of synthetic outbreaks, exploring parameters that generate realistic outbreaks, and calculating and plotting additional error metrics.

#### Additional notes
* Code to run and assess an older synthetic analysis can be found in "20yr_synthetic/." As the name implies, this allows one to run multistrain synthetic outbreaks over a period of 20 years, in order to explore geographic patterns and strain dynamics. However, this code is old and unmaintained.

Data Sources
------------
* Raw syndromic and virologic data were obtained from the World Health Organization's [FluNet][1] and [FluID][2] databases.
    * Code used to process original FluNet and FluID data, and to calculate and apply scalings, can be found in "src/mainCode/format_obs/". However, as the format of FluNet and FluID data does change over time, the code in "calculate_subtype_plus.R" may no longer be applicable to newly-downloaded code.
* Absolute humidity climatologies were generated using data from NASA's [Global Land Data Assimilation System][3] (GLDAS).
* Country population data were adapated from the United Nations data cited below, copyright &copy; 1992-2019 by United Nations, licensed under a [Creative Commons Attribution 3.0 IGO License][4]. Original data can be found [here][5].
    > United Nations, Department of Economic and Social Affairs, Population Division (2017). *World Population Prospects 2017, archive.*
* Country centroid data can be downloaded directly from Harvard University's [WorldMap][6] platform.
    * Note: These data are only used for "src/formatTravelData/analysis_and_visualize/analyze_by-distance.R," "src/formatTravelData/analysis_and_visualize/plot_contact_matrices.R," and "src/mainCode/explore_obs/checkGeo_humid+pop.R"

#### Travel Data
* Raw air travel data can be obtained [here][7].
    * Data can then be formatted using "src/formatTravelData/format_air-data.R"
* Commuting data are not publicly available and must be requested from Eurostat. More information on placing an ad-hoc request for aggregated data can be requested [here][8].
    * Code used to format commuting data can be found in "src/formatTravelData/format_commuting-data.R"

[1]: https://www.who.int/influenza/gisrs_laboratory/flunet/en/
[2]: https://www.who.int/influenza/surveillance_monitoring/fluid/en/
[3]: https://ldas.gsfc.nasa.gov/gldas
[4]: https://creativecommons.org/licenses/by/3.0/igo/
[5]: https://population.un.org/wpp/Download/Archive/Standard/
[6]: https://worldmap.harvard.edu/data/geonode:country_centroids_az8
[7]: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=avia_paocc&lang=en
[8]: https://ec.europa.eu/eurostat/help/support
