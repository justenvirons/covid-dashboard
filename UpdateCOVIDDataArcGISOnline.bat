:loop

C:\"Program Files"\R\R-4.0.2\bin\Rscript.exe COVID-19_TestingFacilities_ArcGISOnline.R

# timeout /t 60 /nobreak

cd ../layers
7z a ChicagoBoundary.zip ChicagoBoundary.*
7z a ZCTA_map.zip ZCTA_map.*
7z a ZCTA_profile.zip ZCTA_profile.*
7z a ZCTA_select.zip ZCTA_select.*

cd ..

# python ArcGISOnlineCOVID19DashboardUpdate_Daily.py

# timeout /t 86400 /nobreak
goto :loop

