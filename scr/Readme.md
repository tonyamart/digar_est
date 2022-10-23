# digar project
#DS

files descr

### 1_test_digar_pckg.ipynb
Notebook used `digar` package for exporting metadata and texts for newspapers in Russian:
	* `get_digar_overview()`  - used for looking into the whole dataset, exploring Russian data inside
	* subsets all title names marked as Russian, get metadata with `get_subset_meta(subset, source = "pages")`
	* NB 'sections' metadata for this subset were also retrieved and stored as `test/meta_rus_subset_sections.tsv` 
	* part "get_matching_texts" creates an output file with texts
	* texts are written as a single output `.txt` file

### 2_1_retrieve_rus_subset.ipynb
	* filter only pre-revolutionary newspapers (1851 < year < 1918);
	* putting together texts from an output (`test/dorevol.txt`) file and metadata; 
	* cleaning the resulting dataset to the format one row = one page text; 
	* the result is stored as `test/dorevol.csv` , each page supplied with PageID

### 2_2_retrieve_texts.ipynb
	* Joining full metadata (meta got from `get_digar_overview`) and pages metadata; 
	* adding the texts; 
	* the result stored as `test/texts_dorevol_all.csv` 

### 3_lang_detect.ipynb
	* package `textcat`  used for language detection for each page;
	* `test/dorevol.csv`  was taken as an input, joining it with subset meta (`test/meta_rus_subset_pages.tsv`, by = 'PageID')
	* applying language detection function for the table;
	* language detection output stored as `test/dorevol_langs.csv`
	* joined with full metadata as  `test/dorevol_meta_lang.csv` (`lang` column contains the new language labels)

### 4_text_analysis.ipynb & 6_np_1880.ipynb
	* find the longest issued newspapers;
	* input: table with PageID, text, language + joined metadata for each page/issue, stored as `rus_subset_merged.csv`;
	* select newspapers with more than 100 issues;
	* filter the longest: EstGoubVed & LivGoubVed;
		* filter only one decade (1880s) 
		* (6_np_1880.ipynb) - randomly selecting 3000 pages from in German and in Russian
		* fast clean from non-words & count number of tokens
		* select only 8k MFW
		* store for lemmatization in `mfw/`
	* filter the newspapers of 1917:
		* extract data for the month of issuing;
		* same word counting & filtering operations as above

### 5_text_analysis
	* preprocessing: 
		* count lemmata

### 7_topic_models_creation.ipynb
	* take MFW counts;
	* filter stoplist;
	* create dtm for topic modeling; 
	* create LDA tm, store results as `beta_1917_50.csv`  and `gamma_1917_50.csv` (50 topics)
	* OR for 1880s: `beta_1880_20.csv` and `gamma_1880_20.csv`, (20 topics)

### rus_lemmatization.ipynb
`pymystem3` lemmatization for .csv input

### rus_new_orf.py
`russpelling` package used for overwriting in modern Russian orthography