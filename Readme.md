### Study on the Official newspapers published in Estonia and Latvia before 1918

#### Repository description

Current repository contains data, scripts and reports regarding the exploratory research project on the DIGAR collection.  
The materials considered in the reports are the Russian-language newspapers issued before 1918. The aim of the reports are to answer questions related  to the current state of the data and metadata in the collection (report 1), as well as the ones from historical perspective, i.e. if it is possible to see major historical shifts and trends in newspaper's language of the western counties of the Russian empire (report 2).
  
`data` folder comprises the results of the language detection and complementary files used in reports. The folder also contains two subfolders: preprocessed tables of MFW used for models creation as well as the LDA models output.  
  
`code` folder includes all code used for data retrieval and further examination of data.  
  
`docs` folder comprises two reports and the way they are created (as `.Rmd`).  
  
#### Summary
The first report is dedicated to the use of languages in the newspapers marked as "Russian" in the metadata and published before 1918. As it is known that both Estonian and Livonian governorates were primarily German-language areas of the Russian empire, this study focuses on the digitized newspapers of the 19th century reassessing the languages used in "Russian" or, as it will be revealed, rather official newspapers of the Russian empire.  
  
The second report is focused on the content changes of these newspapers caused by two major historical evens: Russification of the western governorates in the 1880s and two revolutions of 1917. The aim of the study was to test whether the current state and amount of data is enough to spot content changes using topic modelling (LDA).  