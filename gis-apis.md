ERS GIS Map Services and API user guide.
All of the ERS mapping applications such as the Food Environment Atlas and the Food Access Research Atlas use map services developed and hosted by ERS as the source for their map content.  These map services are open and freely available for use outside of the ERS map applications.  Developers can include ERS maps in applications through the use of the map service REST API, and desktop GIS users can use the maps by connecting to the map server directly.

General map service documentation
Over 30 ERS map services have been created, each containing anywhere from 4 to 70 map layers. The majority of the services consist of national maps displaying data at the county-level.  The Food Access Atlas service is Census-tract based.  A good way to discover and browse the available services and maps is to use the ERS map applications themselves.  The Food Environment Atlas, the Rural Atlas, The Farm Program Atlas, and the SNAP Data System all use a similar data organization scheme of ‘Categories’ and ‘Maps’.  Each category in the application corresponds to a map service; each map is a layer in that map service.  The Food Access Research Atlas is the exception to this scheme as it only uses a single ERS map service, plus an ESRI-hosted background map.
The REST Services Directory listing all available map services is here:  http://gis.ers.usda.gov/arcgis/rest/services
A summary of the map services, and where they are used:

|Map Service | Description, type of content |	Where Used |
| ---------- | ---------------------------- | ---------- |
|`background_cache`<br>`background` | Generic landmass/waterbody used for map backgrounds | All ERS Atlases except the Food Access Research Atlas |
|`fa_access`<br>`fa_assistance`<br>`fa_health`<br>`fa_insecurity`<br>`fa_local`<br>`fa_prices_taxes`<br>`fa_restaurants` <br>`fa_socioeconomic`<br>`fa_stores` | Access and Proximity to Grocery Stores<br>Food Assistance<br>Health and Physical Activity<br>Food Insecurity<br>Local Foods<br>Food Prices and Taxes<br>Restaurant Availability and Expenditures<br>Socioeconomic Characteristics<br>Store Availability | The Food Environment Atlas |
|`Foodaccess` |	Food Access measures used in the Food Access Research Atlas | The Food Access Research Atlas |
|`fsn_acre`<br>`fsn_ci`<br>`fsn_crp`<br>`fsn_crssprg`<br>`fsn_dcp`<br>`fsn_milc`<br>`fsn_mlb`<br>`fsn_sure` | Average Crop Revenue Election Program<br>Crop Insurance Program<br>Conservation Reserve Program<br>Cross Program Comparisons<br>Direct and Countercyclical Program<br>Milk Income Loss Contract<br>Marketing Assistance Loan Program<br>Supplemental Revenue Assistance Program | The Farm Program Atlas |
|`ra_cntyclass`<br>`ra_filter`<br>`ra_income`<br>`ra_jobs`<br>`ra_people`<br>`ra_veterans`<br>`ra_query` | County classifications<br>Used to mask metro/non metro counties<br>Income<br>Jobs<br>People<br><br>All Rural Atlas map variables | Atlas of Rural and Small Town America |
|`snap_benefits`<br>`snap_participation_poverty`<br>`snap_participation` | SNAP Benefits<br>SNAP Participation per Person in Poverty<br>SNAP Participation | Supplemental Nutrition Assistance Program (SNAP) Data System |

Reference2
referenceWM
State and county boundaries and labels
Web Mercator version	All ERS Map Atlases

Desktop User instructions
To use the ERS Map Services in a desktop application such as ArcMap or ArcGIS Online, use the service description and REST directory to determine which services and maps you would like to use, and follow the instructions here to add the maps to your document:  
http://resources.arcgis.com/en/help/main/10.1/index.html#/Adding_ArcGIS_for_Server_map_services/00sp00000019000000/

Developer API instructions
The services can be accessed through ArcGIS Server REST API.  JSON and SOAP interfaces are also available.   Detailed documentation including general usage instructions, and service properties and methods is available here: 
http://gis.ers.usda.gov/arcgis/sdk/rest/index.html?catalog.html


