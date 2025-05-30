# BIOL406-2025-Final-Project
Tsuga't to be kidding me

This project set out to analyze zoochorous dispersal of non-native plant species along high and low use trails in Pacific Spirit Park. Transects were laid out along two trails, one with relatively high traffic, and one with relatively low traffic. Percent cover of non-native species was recorded at various quadrats along each transect. Relative traffic was determined using Strava Heat Map. 

**List of scripts**
1) **Analysis.R**: This script runs generalized linear models (GLMs) to test non-native species richness and percent cover along distance from trail and between trail uses according to hypotheses. Located in Scripts/.
2) **ordination_script.R**:This script runs an NMDS ordination for non-native species assemblage at sites along 250m-long transects along a high-use and a low-use trail in Pacific Spirit Park, BC. Located in Scripts/.
3) **bayesian_traffic_nonnative.R** This script runs Bayesian generalized linear mixed-effects models (using the brms package) to evaluate the relationship between (1) non-native species richness and (2) non-native percent cover and distance from trailhead. Located in Scripts/.

**List of datasets**
1) **cleaned_dataframe.csv** = cleaned dataset from Data_Collection - Sheet1.csv with proper formatting and notes removed. Located in data_cleaned/.

- QuadratID = Specific quadrat where data was collected Ranges from H1-10, L1-10
- Traffic = Pertains to which trail data was collected. (high/low)
- distance_m = distance into the trail from start of trail in metres (m)
- angle = Angle of quadrat from the trail (degrees)
- lesser_periwinkle_cover = percent cover of Vinca minor (%)
- rumex_cover =  percent cover of Rumex (%)
- creeping_buttercup_cover = percent cover of Ranunculus repens (%)
- spanish_bluebell_cover = percent cover of Hyacinthoides hispanica (%)
- grass_cover	= percent cover of Graminoids (%)
- english_ivy_cover = percent cover of Hedera helix (%)
- himalayan_blackberry_cover = percent cover of Rubus armeniacus (%)
- english_laurel_cover = percent cover of Prunus laurocerasus (%)
- nipplewort_cover = percent cover of Lapsana communis (%)
- english_holly_cover = percent cover of Ilex aquifolium (%)
- cutleaf_blackberry_cover = percent cover of Rubus laciniatus (%)
- other_cover = percent cover of other species (%) 
- richness_non_native = richness of non native species
- dominant_tree_species = richness of dominant tree species
- canopy_cover = percent cover of canopy (%)
- bare_ground.litter = percent cover of bare_ground or litter if >10% (%)



2) **Data_Collection - Sheet1.csv** = data collected from two trails, data collected under the 'high' traffic category were collected from Sasamat trail in Pacific Spirit Park, data collected under the 'low' traffic category was collected from Top Trail from PSP. Located in data_raw/.

- QuadratID = Specific quadrat where data was collected Ranges from H1-10, L1-10
- Traffic = Pertains to which trail data was collected. (high/low)
- distance_m = distance into the trail from start of trail in metres (m)
- angle = Angle of quadrat from the trail (degrees)
- lesser_periwinkle_cover = percent cover of Vinca minor (%)
- rumex_cover =  percent cover of Rumex (%)
- creeping_buttercup_cover = percent cover of Ranunculus repens (%)
- spanish_bluebell_cover = percent cover of Hyacinthoides hispanica (%)
- grass_cover	= percent cover of Graminoids (%)
- english_ivy_cover = percent cover of Hedera helix (%)
- himalayan_blackberry_cover = percent cover of Rubus armeniacus (%)
- english_laurel_cover = percent cover of Prunus laurocerasus (%)
- nipplewort_cover = percent cover of Lapsana communis (%)
- english_holly_cover = percent cover of Ilex aquifolium (%)
- cutleaf_blackberry_cover = percent cover of Rubus laciniatus (%)
- other_cover = percent cover of other species (%) 
- richness_non_native = richness of non native species
- dominant_tree_species = richness of dominant tree species
- canopy_cover = percent cover of canopy (%)
- bare_ground/litter = percent cover of bare_ground or litter if >10% (%)
- notes = misc. notes during data collection

3) **species_dispersal.csv** = data from literature on reported dispersal syndromes for plants indentified as non-native species within plots along two trails in Pacific Spirit Park. Located in data_raw/.

- species = Species name as identified in the data collection process
- animal = Whether the species has been documented to be animal-dispersed (1) or not (0)
- wind = Whether the species has been documented to be wind-dispersed (1) or not (0)
- water = Whether the species has been documented to be water-dispersed (1) or not (0)
- sprawling = Whether the species has been documented to disperse by sprawling (1) or not (0)
- unspecified = Whether the species has no documented dispersal syndrome (1) or if it has been identified (0)

**List of figures**
1) biol406_sitemap.jpeg = Map of quadrats along transects in Pacific Spirit Park, BC.
2) brms_richness_distance.png = Predicted values and 95% credible intervals from a Bayesian GLM with Poisson family modelling non-native species richness along distance from trailhead and trail use (high, low).
3) distancepctcover_brat.PNG = Predicted values and 95% confidence intervals from a GLM modelling non-native species percent cover along distance from trailhead and trail use (high, low).  
4) distancerichness_brat.PNG = Predicted values and 95% confidence intervals from a GLM modelling non-native species richness cover along distance from trailhead and trail use (high, low).
5) distanceanimalprop_brat.PNG = = Predicted values and 95% confidence intervals from a GLM modelling proportion of animal-dispersed non-native species along distance from trailhead and trail use (high, low).
6) nmds_plot.png = Non-native plant assemblage at sites along a high and low use trail, with arrows connecting sites according to sequential distance from trailhead.

