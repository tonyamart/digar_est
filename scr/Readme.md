# digar project

Short descriptions of the scriptis  

### 1_test_digar_pckg.ipynb
Notebook used `digar` package for exporting metadata and texts for newspapers in Russian:  
* `get_digar_overview()`  - used for looking into the whole dataset, exploring Russian data inside  
* subsets all title names marked as Russian, get metadata with `get_subset_meta(subset, source = "pages")`
* NB 'sections' metadata for this subset were also retrieved and stored as `test/meta_rus_subset_sections.tsv`  
* part "get_matching_texts" creates an output file with texts  
* texts are written as a single output `.txt` file  
  
### 2 - text retrieval
Notebooks that show how the texts (pages/sections) were retrieved via `digar` package and connected to the metadata.  
  
### 3 - language detection
Code used for language detection with R package `textcat`; `3_3_sections_languages_analysis.ipynb` shows some additional steps for analysis described in the report.

### 4 - text normalisation and preprocessing
Notebooks used for normalisation (orthography standartisation and lemmatization) of Russian and preprocessing (tokenisation, selection of MFW) needed for the topic modelling.  

### 5 - topic models creation
* take MFW counts;  
* filter stoplist;  
* create dtm for topic modeling;  
* create LDA tm, store results in `data/models/`  

### 6 & 7
Code used for the preparation of the reports (exploratory, the actual code used in the reports are stored as `.Rmd`)

#### preprocessing_rus
Additional notebooks prepared for the text preprocessing.
