# <img src="https://idahocrews.org/wp-content/uploads/2023/12/icrews-logo-final-circle-768x768.png" width="120" height="120"><br/>Idaho Community-engaged Resillience in Water-Energy Systems
repository for ICREWS projects

## Social-Ecological-Technological Systems Archetypes in Idaho
- The goal of this repo is to practice setting up a reproducible and professional project page on GitHub.

## Installation Instructions:
1. Load the required packages needed for this project:
- Run the `packages.R` script found in the /scripts/utilities/ directory. 
Please note that this script **will install** any required packages you do not
currently have. Please make sure that you have permissions to download and use 
these packages.

## Usage Instructions:
Provide clear examples of how to use the scripts, templates, or functions included in the repository.

Within the `/scripts/final/` you will find all of the scripts used for creating the SETS archetypes for Idaho.\
The scripts are named in the order they should be run. The workflow is as follows:
1. scripts starting with `01` are used for processing the spatial data to 3km and saving as a raster (.tif) file.
2. scripts starting with `02` are used to stack the rasters and also to perform optional exploratory data analysis
3. the script starting with `03` goes through the parameterization of the fuzzy c-mean cluster models
4. the script starting with `04` runs the user decided final model/cluster analysis
5. the script starting with `05` generates maps of the resulting archetype clusters
6. the script starting with `06` calculates the variable distributions for each archetype
7. the script starting with `07` calculates the entropy of each pixel based on the cluster belongings matrix

The parameterization `03`, analysis `04`, and entropy `07` rely heavily on the `cmeans` package from Jeremy Gelb (add link).  

## License:
- [Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/deed.en)

## Contact Information:
- For questions related to the code please email Katie Murenbeeld at 
[katiemurenbeeld@boisestate.edu](mailto:katiemurenbeeld@boisestate.edu)


