
#############################################################################################
### NB: THE FORMATTING (TAB/LINE) STRUCTURE IN THIS SCRIPT IS IMPORTANT - DON'T CHANGE IT ###
#############################################################################################

path <- 'Z:/Marine/Evidence/HabitatMapping/EMODnetSeabedHabitats/WP4_Portal/Freemarker/20200122_TemplateFilled.xlsx'
outpath <- 'Z:/Marine/Evidence/HabitatMapping/EMODnetSeabedHabitats/WP4_Portal/Freemarker/output/'
install.packages( "readxl")
install.packages( "getPass" ) 
makeFTL_raster <- function(path, outpath, sheet = 1, filter=T, omit, staging = T){
  
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(readxl))
  
  pwd <- getPass::getPass(forcemask = T, msg = 'Please enter geoserver "emodnet_admin" password:')
  
  tab <- read_excel(path, sheet = sheet)
  
  if(filter){
    tab <- dplyr::filter(tab, Upload=='y')
    tab <- dplyr::filter(tab, is.na(`Done?`))
  }
  
  if(staging){
    url <- 'https://staging.ows.emodnet-seabedhabitats.eu/rest/workspaces/'
  } else {
    url <- 'https://ows.emodnet-seabedhabitats.eu/rest/workspaces/'
  }
  
  for(i in 1:nrow(tab)){
    print(i)
    tab2 <- as.data.frame(tab[i,])
      
    metadata <- GET(paste0(url, tab2$Workspace, '/coveragestores/',
                           tab2$Datastore, '/coverages/', tab2$LayerName, '.xml'),
                    config = authenticate(user = 'emodnet_admin', password = pwd))
   
    if(status_code(metadata)!=404){
      metadata <- xml2::as_list(xml2::read_xml(metadata))
      
      title <- metadata$coverage$title[[1]]
      if(!is.null(metadata$coverage$metadataLinks[grepl(metadata$coverage$metadataLinks, pattern = 'text/html')]$metadataLink$content[[1]])){
        metadataURL <- metadata$coverage$metadataLinks[grepl(metadata$coverage$metadataLinks, pattern = 'text/html')]$metadataLink$content[[1]]
      }else{
        metadataURL <- NULL
      }
      if(!is.null(metadata$coverage$abstract[[1]])){
        abstract <- metadata$coverage$abstract[[1]]
      }else{
        abstract <- NULL
      }
      
  
      if(!is.null(metadataURL)){
        mdL <- paste0('
                      <p><a href="',metadataURL,'" title="View layer metadata" target="_blank">(View layer metadata)</a></p>')
      }else{
        mdL <- NULL
      }
      if(!is.null(abstract)){
        abst <- paste0('
                       <p>', abstract,'</p>')
      }else{
        abst <- NULL
      }
      
      capt <- NULL
      if(tab2$HR_BandName=='Confidence'){
        capt <- 'High confidence = 3, Moderate confidence = 2, Low confidence = 1'
      }
      
      
a<-paste0('<#macro nullhider confscore>
		<#if confscore lt -9998>
			<div class="hidden">
		<#else>
		  <div>
	</#if>
</#macro>

<#list features as feature>
<@nullhider confscore=feature.', tab2$BandName, '.rawValue />
<h2>',title,'</h2>',
    mdL,'
    <table class=featureInfo>
    <caption><caption>
    <tr>
      <th scope="row">', tab2$HR_BandName,'</th>
      <td>',paste0('${feature.', tab2$BandName, '.rawValue?string["0.00"]}'),'</td>
    </tr>
  </table>
<p>',capt,'</p>
</hr>
</div>
</#list>')
      write.table(a, paste0(outpath, tab2$Workspace, '_', tab2$LayerName,'.ftl'), row.names = F, col.names = F, quote = F)
      
    if(is.null(abstract)){
      print(paste0(i,' - abstract'))
    }
    if(is.null(metadataURL)){
      print(paste0(i,' - metadataURL'))
    }
    } else {
      print(paste0(i, ' - ERROR 404'))
    }
  }
  
}

makeFTL_raster(path, outpath, sheet = 1, staging = F)
