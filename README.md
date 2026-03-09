## EM Bruna Biotropica article: "Is there really such a thing as _Tropical_ Biology?

This repository contains the code and data used in the following article based on EB's 2022 ATBC Presidential Plenary talk:

>Bruna, E. M. (2025). Is there really such a thing as Tropical Biology? _Biotropica_, 57, e13387. <https://doi.org/10.1111/btp.13387>

### Reproducing the analyses and article

To reproduce the analyses and paper (including figures and tables) carry out the following steps:

1. Run the script `code/ms_data_prep.R`. 

This script pulls the complete data set of keywords and title bigrams from the ['Tropical Bibliometrics'](https://github.com/BrunaLab/tropical_bibliometrics) repository and save it in a folder called `data/data_original`. It will also pull two functions necessary to 'clean' the data for analysis (`keywords_depluralizer.R, keyword_editor.R`) from the same repository and save them in the `code` folder. The 'clean' data used for all analyses, figures, and tables in this manuscript are then saved in the folder `data/data_ms`. 

2. Open and knit the file `Bruna_plenary_MS.Rmd`. 

Knitting the file will import the 'clean' data, carry out all calculations presented in the paper (e.g., number of articles in each article category, number of terms in each category) using the embedded R code chunks, and generate the figures and tables. Figures are generated using two functions found in the `code` folder (`plot_bigrams_bar.R`, `plot_kw_bar.R`). There is no need to open and render the `bruna_supplement.Rmd` file -- it is automatically knit and included in the .pdf file of the paper when rendering `Bruna_plenary_MS.Rm`.

## Citation

If you use these data in your own manuscript, please be sure to cite both the _Biotropica_ article and the archived dataset: 

### Article citation

Bruna, E. M. (2025). Is there really such a thing as Tropical Biology? _Biotropica_, 57, e13387. <https://doi.org/10.1111/btp.13387>

```
@article{https://doi.org/10.1111/btp.13387,
author = {Bruna, Emilio M.},
title = {Is there really such a thing as Tropical Biology?},
journal = {Biotropica},
volume = {57},
number = {1},
pages = {e13387},
keywords = {bibliometrics, collaboration, colonialism, global south, scholarly societies, scientometrics, temperate, text-mining},
doi = {https://doi.org/10.1111/btp.13387},
url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/btp.13387},
eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/btp.13387},
note = {e13387 BITR-24-143.R1},
abstract = {Abstract The ecosystems of The Tropics comprise a majority of the planet's biodiversity, approximately 40\% of its terrestrial surface area, and half the human population. Despite this, Tropical Biology has historically been conceptualized as a specialized subdiscipline of the Biological Sciences. I assessed the validity of this assumption and conclude that it depends on the framework and evidence used to evaluate it. I suggest that the way forward as a discipline is not for Tropical Biologists to drop the geographic adjective that unites them, but to recenter The Tropics as the foundation of ecology and evolutionary biology. Abstract in Spanish is available with online material.},
year = {2025}
}
```

### Data set citation 

Emilio M. Bruna. (2024). Code and Data for Bruna 2024 _Biotropica_ (v1.0). Zenodo. <https://doi.org/10.5281/zenodo.13821266>

```
@software{emilio_m_bruna_2024_13821266,
  author       = {Emilio M. Bruna},
  title        = {Code and Data for Bruna (Biotropica, 2024).},
  month        = sep,
  year         = 2024,
  publisher    = {Zenodo},
  version      = {v0.9},
  doi          = {10.5281/zenodo.13821266},
  url          = {https://doi.org/10.5281/zenodo.13821266},
}
```

[![DOI](https://zenodo.org/badge/800521350.svg)](https://zenodo.org/doi/10.5281/zenodo.13821265)



