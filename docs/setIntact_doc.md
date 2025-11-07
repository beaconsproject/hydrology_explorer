### Set fires and intactness

In this step, the user specifies the source for (1) fire spatial layer used to calculate the fire statistics and (2) intactness value or spatial layer used to specify or calculate catchment intactness (0-1 or 0-100% intact). 

**Select source for fires** offers two options: 

1. **Use existing fire layer** - If a Disturbance Explorer GeoPackage was uploaded in the previous step, it may include a fire layer. 
   
3. **Upload fire layer** - If selected, the user can upload a shapefile or GeoPackage of the fire spatial layer.

If a fire layer is not uploaded, the **Fire statistics** table will not populate.

**Select source for intactness** offers three options: 

1. **Use existing undisturbed layer** - If a Disturbance Explorer GeoPackage was uploaded in the previous step, it will include an undisturbed layer. The app will use this spatial layer to calculate the proportion of the catchment intact. Note: This value will be added to the catchment dataset contained in the Download GeoPackage.
   
2. **Value in catchment dataset** - If it exists, select the attribute in the catchment dataset that contains values for the proportion of the catchment undisturbed or intact. Values will range from 0 to 1, with 1 = 100% intact. 

   📌 Select this option when using the embedded Demo dataset. The intactness attribute is called "intact".

3. **Upload intactness layer** -  The intactness layer is a polygonal feature and can be uploaded via a GeoPackage or Shapefile. If a GeoPackage is used, the user must specify which layer contains the intactness data. The app will use this spatial layer to calculate the proportion of the catchment intact. Note: This value will be added to the catchment dataset contained in the Download GeoPackage.

Press the **Confirm** button. If an intactness layer is provided, it will be displayed on the map.
