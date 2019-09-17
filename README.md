# Shiny Microbiome Analysis

The purpose of this app is to provide some extremely basic 
analysis tools for microbiome data, specifically the type of
microbiome which results in abundance tables recording the
number of genomic fragments (e.g. 16S ribosomal NGS reads)
identified from each sample, which are assigned to a given
organism.

When combined with a _metadata sheet_ noting which specimens
belong to which experimental groups, this app provides some
very simple options for analyzing and visualizing those data.

### Input Data

Input data consists of two files -- a _taxon table_ and a
_metadata sheet_. Examples of each file can be found in the
`example_data/` folder. The _metadata sheet_ must be a CSV
in which the first column corresponds to the list of specimens
in this particular experiment. The _taxon table_ must be a
CSV in which each row corresponds to an organism (or OTU, or 
ASV) and each column corresponds to a biological specimen
(the same names as in the metadata sheet). The first column in
the  _taxon table_ should be `tax_name`, and the file may also
contain columns labeled `tax_id` and `rank` (those will be
removed for any further analysis). 

Note that the taxon table file format is exactly the format
output by the 16S amplicon analysis tool `maliampi` 
([link](https://github.com/jgolob/maliampi/)). If you have raw
16S data and would like to generate these taxon tables, consider
using MaLiAmPi. Within the output folder, the most appropriate
CSV to use for this visualization is likely `classify/tables/tallies_wide.species.csv`.


*Taxon Table (CSV)*

| tax_name | tax_id | rank | Sample_A | Sample_B | Sample_C | Sample_D | 
| --- | --- | --- | --- | --- | --- | --- |
| Dialister | 39948 | genus | 283 | 289 | 228 | 594 |
| Lachnospiraceae | 186803 | family | 193 | 0 | 0 | 200 |
| Saccharofermentans | 1200657 | genus | 289 | 384 | 105 | 456 |
| Streptobacillus | 34104 | genus | 165 | 0 | 0 | 348 |
| Atopobium | 1380 | genus | 405 | 192 | 76 | 619 |

*Metadata Sheet (CSV)*

| name | Group | Treatment |
| --- | --- | --- |
| Sample_A | A | Control |
| Sample_B | A | Control |
| Sample_C | B | Control |
| Sample_D | B | Treatment |

### Analyses

The basic visualization provided by this app encompasses the following:

  * Stacked bar graphs showing the most abundant organisms
  * Plots showing the total number of reads per sample
  * Plots showing the estimated number of organisms in each sample (as calculated by [breakaway](https://github.com/adw96/breakaway))
  * Estimated coefficients for the differences in abundance of individual organisms across groups (as calculated by [corncob](https://github.com/bryandmartin/corncob))

### Screenshots

**Upload Data**

![upload modal](https://raw.githubusercontent.com/FredHutch/shinyMicrobiomeAnalysis/master/www/upload_modal.png)

**Stacked Bar Graph**
![stacked bar graph](https://github.com/FredHutch/shinyMicrobiomeAnalysis/blob/master/www/stacked_bar_graph.png?raw=true)

**Number of Reads**
![sequencing depth](https://github.com/FredHutch/shinyMicrobiomeAnalysis/blob/master/www/sequencing_depth.png?raw=true)

**Alpha Diversity**
![alpha diversity](https://github.com/FredHutch/shinyMicrobiomeAnalysis/blob/master/www/alpha_diversity.png?raw=true)

**Differential Abundance**
![differential abundance](https://github.com/FredHutch/shinyMicrobiomeAnalysis/blob/master/www/differential_abundance.png?raw=true)

