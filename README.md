## EM Bruna Biotropica article: "Is there really such a thing as _Tropical_ Biology?

This repository contains the code and data used in the manuscript published in Biotropica based on EB's 2022 Presidential Plenary talk. 

### Reproducing the analyses and article

To reproduce the analyses and paper (including figures and tables) carry out the following steps:

1. Run the script `code/ms_data_prep.R`. 

This script pulls the complete data set of keywords and title bigrams from the ['Tropical Bibliometrics'](https://github.com/BrunaLab/tropical_bibliometrics) repository and save it in a folder called `data/data_original`. It will also pull two functions necessary to 'clean' the data for analysis (`keywords_depluralizer.R, keyword_editor.R`) from the same repository and save them in the `code` folder. The 'clean' data used for all analyses, figures, and tables in this manuscript are then saved in the folder `data/data_ms`. 

2. Open and knit the file `Bruna_plenary_MS.Rmd`. 

Knitting the file will import the 'clean' data, carry out all calculations presented in the paper (e.g., number of articles in each article category, number of terms in each category) using the embedded R code chunks, and generate the figures and tables. Figures are generated using two functions found in the `code` folder (`plot_bigrams_bar.R`, `plot_kw_bar.R`). There is no need to open and render the `bruna_supplement.Rmd` file -- it is automatically knit and included in the .pdf file of the paper when rendering `Bruna_plenary_MS.Rm`.

## Citation

If you use these data in your own manuscript, please be sure to cite both the _Biotropica_ and the dataset: 

Emilio M. Bruna. (2024). Code and Data for Bruna (Biotropica, 2024). (v0.9). Zenodo. https://doi.org/10.5281/zenodo.13821266



[![DOI](https://zenodo.org/badge/800521350.svg)](https://zenodo.org/doi/10.5281/zenodo.13821265)



