Code and data for manuscript submitted to **JGR: Atmospheres**

## **Comprehensive analysis of the NOAA National Water Model: A call for heterogeneous formulations and diagnostic model selection**

_J. Michael Johnson, Shiqi Fang, A.Sankarasubramanian, Arash Modaresi Rad, Luciana Kindl da Cunha, Keith S Jennings, Keith C. Clarke, Amir Mazrooei, Lilit Yeghiazarian_


# Data Creation

### 0. Precomputed
 - `nhdplus_nwis_mapping.parquet` 
   - This file stores the `GAGE_ID`/`feature_id` relation found with the NLDI
 - `data/usgs_metadata.parquet` 
   - This files stores the metadata about NWIS flow records found with `dataRetrieval::readNWISdata()` 
 
### 1. Get basins: `data/R/01_basin_boundaries.R`
 - This file downloads the `gagesII` shapediles from the USGS and joins it to precomputed data (0)
 - The output is a 680MB geopackage called `basins.gpkg`
 
### 2. Get flows: `02_flow_comparisions.R`
  - This file uses the `GAGE_ID`/`feature_id` sets in `basins.gpkg` to query _daily_ timeseries data from a NWIS and NWM2.0 NetCDF file.
  - All stats are computed based on the comparison of these  series
  - The stats are joined to the basins and re-exported to the `basins.gpkg`
  
### 3. Get NLDAS data: `03_nldas_data_download.R`
  - This file downloads all requisite NLDAS data from the NASA GES DISC based on the file `data/subset_NLDAS_FORA0125_M_20230907_224958_.txt`
  - Directions for generating that file are in the notes of the R file, however, it should work for everyone as is.
  
### 4. Process NLDAS data: `04_nldas_data_processing.R`
  - This file prepares basin summarized NLDAS data
  - The stats are joined to the basins and re-exported to the `basins.gpkg`

### 5. Process NLCD data: `05_nlcd_data_processing.R`
  - This file prepares  basin summarized NLCD data
  - The stats are joined to the basins and re-exported to the `basins.gpkg`

### 6. Process NID data: `06_nid_data_processing.R`
  - This file prepares the basin summarized NID data
  - The stats are joined to the basins and re-exported to the `basins.gpkg`
  
### 7. Export Table: `07_table_export.R`
   - This simply reads the `basins.gpkg` and exports the non-spatial data as a parquet file. This file is called `data/v20_summary_stats.parquet` and is used in all image creation (see section called **Create all images**)

### 7. Format NextGen outputs: `08_ngen_experiment.R`
  - This file re-formates the data for the NextGen experiment from CSV files produced by co-authors from [`ngen`](https://github.com/NOAA-OWP/ngen) simulation.

# Key Data Artifacts
  The key artifacts used in the analysis of this paper are:
    - `data/v20_summary_stats.parquet` - this is the basins.gpkg stripped of geometry and saved as a parquet file 

# Create all images 

Run this from the terminal (in this directory):

```bash
bash ./make_images.sh
```

All images will be generated in the `./images/png` directory.

