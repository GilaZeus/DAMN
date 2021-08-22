# DAMN: Drawn Array’s Multistep Normalisation
**Goal**: Visualisation of  expressions of spots in 2-channel microarrays (or any other type of two-channel data).
**Dependancies**:
- tibble (>= 2.1.1),
- dplyr (>= 0.8.0.1),
- shiny (>= 1.6.0),
- shinyFiles (>= 0.9.0),
- ggplot2 (>= 3.3.5)
- magrittr (>= 2.0.1)

**Input**: delimited text file(s) containing four columns at least: CH1I, CH1B, CH2I, CH2B.

After importing the data, DAMN allows different ways to visualise your data:
1. **Raw**: No normalisation was performed. Logarithmized spot intensities are plotted. 
2. **MA-regression**: MA-plot („minus“ against „average“) is built and normalised with regression.
3. **Centering**: Results from MA-regression normalisation are centered, i.e. means of all distributions become 0.
4. **Scaling**: Centered distributions are scaled, i.e. their standard deviations become 1.
5. **Distribution normalisation**: one distribution is created from all other distributions.

For further information please open readme.pdf
