### Generate upstream and downstream

This step identifies the areas upstream and downstream of the Analysis AOI and calculates their associated intactness and hydrology statistics.

### Step 1. Generate upstream and downstream areas

Click **View upstream and downstream areas** in the left-side panel. Depending on the size of the study area, this may take several minutes. When done, the following areas are added to the map:

- **Upstream area**: catchments draining into the Analysis AOI.
- **Downstream stem areas**: catchments along the main stream exiting the Analysis AOI.
- **Downstream area**: the downstream stem and all catchments draining into it, excluding the upstream area.

The Analysis AOI is excluded from all three areas. Catchments are coloured by their intactness (see the **Percent intactness** legend). Layers can 
be turned on and off using the legend in the top-right corner. The **Downstream area** is hidden by default.

&#x1F4CC; Note:  Upstream and downstream areas are not computed beyond the extent of the catchments layer.

<br>

### Step 2. Review the statistics

The tables on the right are updated with the following statistics:

- **Area Intactness and Hydrology statistics**: area (km²) and mean area-weighted intactness (AWI, %) of the upstream, downstream stem, 
and downstream areas.
- **Feature statistics** (if a feature to track was set): area (km²) and percent of the feature within the Analysis AOI, the upstream, downstream 
stem, and overall downstream areas.
- **Dendritic Connectivity Index (DCI)**: longitudinal connectivity of the stream network within the Analysis AOI, ranging from 0 (fragmented) 
to 1 (connected) (Cote et al. 2009):

| DCI | Rating |
|---|---|
| ≥ 0.9 | Very High |
| 0.8 - 0.9 | High |
| 0.7 - 0.8 | Moderate |
| < 0.7 | Low |

All statistics are combined in the **Summary statistics** tab, located across the top, and can be downloaded as a CSV file using
**Download summary statistics table (.csv)**.

<br>

From here, proceed to **Download results** in the left-side panel.

#### References

Cote, D., Kehler, D.G., Bourne, C. et al. A new measure of longitudinal connectivity for stream networks. Landscape Ecol 24, 101–113 (2009). https://doi.org/10.1007/s10980-008-9283-y
