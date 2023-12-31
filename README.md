
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10033314.svg)](https://doi.org/10.5281/zenodo.10033314)

# Zenodo Community Statistics
I was interested in getting some general Zenodo community-level metrics (Views and Downloads)
Unfortunately, Zenodo lacks an integrated dashboard for community-level usage statistics; the API queries do not gather these statistics (or I couldn´t figure it out). 

I know this is an embarrassing, complicated approach to get Zenodo Community-level Statistics by scraping values from a list of records of a specific Zenodo Community. 



# Description
The script goal is to show some basic statistics regarding views and downloads of the Zenodo Community.
The statistics cannot be 100% trusted as sometimes data contains NaNs, or not all the records can be harvested.

# Main steps
1) The script collects the list of records from a Zenodo Community using the [oai package](https://cran.r-project.org/web/packages/oai/index.html).
```
   record_list<- list_records("https://zenodo.org/oai2d",metadataPrefix="oai_datacite",set="user-YourCommunity")
```
3) It extracts some specific fields like (URL, title, creator, and datestamp)
```
   df <- record_list %>% select(identifier.3,title,creator, datestamp)%>% rename(url = identifier.3)  
```
5) Remove eventually some duplicates
6) It loops through a list of URLs. Scrape the View and Download data from each Zenodo record (URL).
It might take some time, depending on the size of your Community, because a time delay of 0.5 seconds is introduced
to  prevent the **HTTP Error 429**: "Too Many Requests". It might not work for large communities.
More info on rate limits are here: https://developers.zenodo.org/?python#how-to-verify-your-quot-zenodo-json-quot-file
7) It executes some data summary and plots
   
![ZenodoStats](https://github.com/sdellachiesa/ZenodoCommunityStats/blob/main/ZenodoStatistics.png)

