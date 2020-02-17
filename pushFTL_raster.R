path <- 'Z:/Marine/Evidence/HabitatMapping/EMODnetSeabedHabitats/WP4_Portal/Freemarker/output/'
config <- 'Z:/Marine/Evidence/HabitatMapping/EMODnetSeabedHabitats/WP4_Portal/Freemarker/20200122_TemplateFilled.xlsx'

pushFTL <- function(path, config, sheet = 1, staging = F){

  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(readxl))

  pwd <- getPass::getPass(forcemask = T, msg = 'Please enter geoserver "emodnet_admin" password:')
  fls <- list.files(path, pattern = '.ftl$')
  cnfg <- readxl::read_excel(config, sheet = sheet)

  cnfg <- subset(cnfg, Upload=='y')
  cnfg <- subset(cnfg, is.na(`Done?`))
  
  if(staging){
    url <- 'https://staging.ows.emodnet-seabedhabitats.eu/rest/workspaces/'
  } else {
    url <- 'https://ows.emodnet-seabedhabitats.eu/rest/workspaces/'
  }
  
  df <- NULL
  for(i in 1:nrow(cnfg)){
    cnfg1 <- cnfg[i,]
    fl_up <- fls[grepl(fls, pattern=paste0(cnfg1$Workspace, '_', cnfg1$LayerName))]
    status <- PUT(url = paste0(url, cnfg1$Workspace,'/coveragestores/',cnfg1$Datastore,
                               '/coverages/',cnfg1$LayerName,'/templates/content.ftl'),
                  config = authenticate(user = 'emodnet_admin', password = pwd),
                  body = upload_file(paste0(path, fl_up), type = 'text/plain'))

    df[[i]] <- data.frame(File=fl_up, Status=status$status_code)
  }

  df <- do.call('rbind', df)
  return(df)
}

pushFTL(path, config, sheet = 1, staging = F)
