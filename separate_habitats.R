###########################################################################
# This script has been create for use in the EMODnet Seabed Habitats      #
# habitat map ingestion process, enabling a user to separate concatenated #
# values within a shapefile field into multiple features, to conform with #
# EMODnet and INSPIRE standards.                                          #
#                                                                         #
# Created by Graeme Duncan, JNCC, UK.                                     #
###########################################################################

###############################################################
### IF YOU WISH TO JUST RUN THE SCRIPT, CHANGE THESE VALUES ###
###############################################################

#Change to the folder where the shapefile is located
file_location <- "C:/this_folder/this_subfolder"
#Change to the filename
file_name <- "this_file.shp"

#Change to the character used to separate habitats in the original (if required)
#For example, for mosaic "Sand+Mud", the separator would be "+"
separator <- "+"
#Change to the field name containing the habitat values that you would like to split (if required)
habitat_field <- "ORIG_HAB"

#Change to the output folder where you want to save your shapefile
out_folder <- "D:/my_folder"

###############################################################
###                     THEN RUN ALL BELOW                  ###
###############################################################

separate_habitats <- function(input.dataset, fieldname = "ORIG_HAB",separator = "/"){
  library(sf)
  library(tidyr)  
  
  #Set Polygon values
  input.dataset$POLYGON <- seq.int(nrow(input.dataset))
  
  
  #remove geometry becuase sf lists ruin separate_rows and other things as spatial column is sticky v:( THIS IS WHY WE CANT HAVE NICE THINGS!
  no.spatial <- input.dataset
  st_geometry(no.spatial) <- NULL
  
  #Flag mosaic habitats in VAL_COMM
  no.spatial$VAL_COMM[grepl(separator,no.spatial[,fieldname])] <- paste("Habitat originally mosaic:",no.spatial[grepl(separator,no.spatial[,fieldname]),fieldname])
  
  #Separate out the mosaics based on the identified separator
  split.dataset <- separate_rows(no.spatial,fieldname, sep=separator)
  
  
  #Clean up and join polygons back in
  final.df <- left_join(x=split.dataset[,c("GUI","POLYGON","ORIG_HAB","ORIG_CLASS","HAB_TYPE","VERSION","DET_MTHD","DET_NAME","DET_DATE","TRAN_COM","T_RELATE","VAL_COMM")],y=input.dataset[,c("POLYGON","Shape")], by="POLYGON")
  
  #Resort by POLYGON value
  final.df <- final.df[order(final.df$POLYGON),]
  
  #Reset as a simple features dataframe
  final.df <- st_as_sf(final.df)
  
  return(final.df)
}

sf.dataset <- st_read(dsn=file_location, layer=file_name, stringsAsFactors = FALSE)
output.dataset <- separate_habitats(sf.dataset, fieldname=habitat_field, separator=separator)

out_name <- paste("split_",file_name,sep="")
if (!(file_ext(out_name) == "shp")){
  out_name <- paste(out_name,".shp",sep="")
}
st_write(output.dataset,dsn=file.path(out_location,out_name), layer=out_name, driver="ESRI Shapefile")
