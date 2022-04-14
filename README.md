

# trustsurveys

For decades, trust in others has been measured across societies around the world via the survey question:

**"Generally speaking, would you say that most people can be trusted or that you can't be too careful in dealing with people?"**

The proportion of people in a country responding that *"Most people can be trusted"* has been used as a measure of *generalized*, or *impersonal* trust, and has been related to all sorts of economic, political, health, social or historical characteristics on the country level.

While the most prominent survey routinely asking above question in representative samples is the [World Values Survey (WVS)](https://www.worldvaluessurvey.org/), many regional surveys exist which also use the instrument and also aim for representativeness via probability sampling. Among those are the [European Values Survey (EVS)](https://europeanvaluesstudy.eu/), [Afrobarometer](https://afrobarometer.org/), [Arabbarometer](https://www.arabbarometer.org/), [Latinobarómetro](https://www.latinobarometro.org/) and [Asianbarometer](http://www.asianbarometer.org/). Nevertheless, results from these different surveys are not available in a centralized manner. 

This repository has gathered data on generalized trust from all waves of all of above surveys - provided that the survey wave included the instrument, of course. That way, an aggregated table with countries' generalized trust estimates for different surveys and waves can be provided. Note that the respective original owners of the data have their own rules and procedures for data usage, which need to be consulted before making any use of the data for whatever purpose.

### Data integration

The most recent data sets from all survey websites (see above) have been downloaded between January and February 2022. Data sets were then integrated via the code in the R script `R/integrate_trust_data.R`, which cleans and harmonizes the data sets and returns a single table with country-level scores of generalized trust by estimating the proportion of people who think that *"most prople can be trusted"*, using weighting factors provided by the survey operators. These weights aim at compensating small deviations from the targeted demographic quota in the realized sample. Observations with missing values (e.g., not asked, response declined or the reponse *"don't know"*) were removed - but the proportion of missings is documented in the resulting table (see below). Note that the original raw data to rerun the script cannot be provided in this repository.

Data from the [Global Preference Survey](https://www.briq-institute.org/global-preferences/home), which measured generalized trust with a different survey item,<sup>1</sup> was also included.

### Output

The integrated data can be found in the file `output/df_trust.csv`. The table has the following simple structure:

| country.name    | wave | survey        | n    | p_response | trust     | years                                       
|-----------------|------|---------------|------|------------|-----------|----------------
| Botswana        | 1    | Afrobarometer | 1200 | 0.9441...  | 0.1473... | 1999-2001
| Lesotho         | 1    | Afrobarometer | 1177 | 0.9847...  | 0.0396... | 1999-2001
| Malawi          | 1    | Afrobarometer | 1208 | 0.9776...  | 0.4479... | 1999-2001
| Mali            | 1    | Afrobarometer | 2089 | 0.9918...  | 0.1277... | 1999-2001
| ...

Here, `country.name` refers to the country, `wave` to the wave of the survey, `survey` to the name of the survey, `n` to the number of respondents sampeld in the respective country and survey wave, `p_response` to the proportion of non-missing responses to the generalized trust question among all `n` respondents, `trust` to the estimated proportion of people who think that *"most prople can be trusted"* and `years` to the time frame of the survey wave. For observations from the Global Preference Survey (`survey == "GPS"`), the number in the `trust` column refers to the standardized trust score as calculated by the original authors of the survey - without any rescaling, it is **not** directly comparable to the other values in the same column.

The table contains 1,067 observations from 146 countries.

<sup>1</sup><sub>The trust measure is based on an item which asks respondents whether they assume that other people only have the best intentions on a scale from 1 to 10, see Falk, A., Becker, A., Dohmen, T. J., Huffman, D., & Sunde, U. (2016). *The Preference Survey Module: A Validated Instrument for Measuring Risk, Time, and Social Preferences* (SSRN Scholarly Paper No. 2725874). Social Science Research Network. https://doi.org/10.2139/ssrn.2725874
