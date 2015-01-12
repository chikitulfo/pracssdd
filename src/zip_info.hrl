%% Archivo de encabezado que contiene información
%% utilizada por todos los módulos del servicio.


-record(zip,{postalcode, placename, state, stateabbreviation, county, latitude, longitude}).

-define(CsvHeader, "Postal Code,Place Name,State,State Abbreviation,County,Latitude,Longitude,\n").
-define(CsvFile, "data/us_postal_codes.csv").
-define(ZIPSERVER, zipserver).
-define(QUERYSOLVER, querysolver).