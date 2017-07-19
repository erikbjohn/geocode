#' \code{geocode} package
#'
#' geocode
#'
#' See the Vignette on 
#'
#' @docType package
#' @name geocode
#' @importFrom dplyr %>% select
#' @importFrom data.table :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title geocode
#'
#' @description Creates and loads points.address file
#' @param data.root dropbox root pkg.data dir
#' @param GEO data.table with one row (missing)
#' @param api.key google api.key character
#' @param source data source example: mmed.20101215
#' @param l.study.extent list of character vectors (zips, cities, states)
#' @keywords points clean geocode
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     foreign
#'     parcels
#'     googleway
#'     points
#'     pkg.data.paths
#' @importFrom dplyr select one_of
geocode <- function(data.root, GEO, api.key, source, l.study.extent){
  dt.pkg.path <- pkg.data.paths::dt(data.root)
  points.address.location <- unique(dt.pkg.path[pkg.name=='points']$sys.path)
  zip <- NULL; match.rank <- NULL; street <- NULL; gLong <- NULL; gLat <- NULL
  match.descr <- NULL; file.name <- NULL; pkg.name <- NULL
  geo.data.id <- GEO$address.id
  # Load loookup data
  states.abbrev <- l.study.extent$states
  study.zips <- l.study.extent$zips
  GEO.list <- list()
  regex.street.body <- paste0('(?<=(', paste(as.character(methods.string::abbrev[class=='suffix', search]), collapse=')|('), '))( |,).+')
  regex.street.num <- paste0('(?<=(^|, ))(',GEO$street.num,')(?= )')
  
  g.address.head <- paste(GEO$street.num, GEO$street.direction.prefix, GEO$street.body, GEO$street.type)
  g.address.tail <- paste0(GEO$street.direction.suffix, ', ', GEO$city, ', ', GEO$state)
  g.address.head.alt <- paste(GEO$street.num, str_replace(GEO$street.body, regex(regex.street.body,perl=TRUE),''))
  g.address.tail.alt <- paste0(GEO$street.type, ', ', GEO$city, ', ', GEO$state, sep=' ')
  
  gQuery<- list()
  gQuery[[1]] <- paste0(g.address.head, g.address.tail)
  gQuery[[2]] <- paste(g.address.head.alt, g.address.tail.alt, zip, ', USA')
  gQuery[[3]] <- paste(g.address.head, zip, sep=', ')
  
  # Last Ditch string fixing
  regex.unit <- paste0(' (',paste0(c(methods.string::abbrev[class=='unit' & search!='#', search], 'Units', 'Unit', 'Aka'),collapse='|'),')( |-|[0-9]{1,1})')
  street.temp <- sapply(str_split(GEO$street, regex.unit), '[', 1)
  if (!str_detect(street.temp, regex('(?=^)[0-9]{1,}-[0-9]{1,}', perl=TRUE))){
    regex.alphaNum <- '\\s[0-9]{1,1}[A-z]{1,1}(?=( |$))|\\s[A-z]{1,1}[0-9]{1,1}(?=( |$))'
    street.temp <- str_replace_all(street.temp, regex.alphaNum, '')
    street.temp <- str_replace(street.temp, '\\s[A-z]{0,}[0-9]{1,}-[0-9]{1,}(?=$)', '')
    street.temp <- str_replace(street.temp, 'Rd314', 'RD 314')
    gQuery[[4]] <- paste(street.temp, g.address.tail)
  }
  
  gQuery <- sapply(gQuery, str_replace_all, pattern='[\\s]{1,},[\\s]{1,}', replacement=', ')
  gQuery <- sapply(gQuery, str_replace_all, pattern='[\\s]{1,}', replacement=' ',simplify=TRUE)
  gQuery <- unique(unlist(gQuery))
  
  iter <- 1; gGood <- FALSE; l <- list()
  
  while(gGood==FALSE && iter <= length(gQuery)){
    g <- list()
    address.query <- str_replace(gQuery[[iter]], '[\\s]{1,}', ' ')
    gReturn <- try(google_geocode(address = address.query, key=api.key))
    if (gReturn$status!='ZERO_RESULTS'){
      col.names <- sapply(gReturn$results$address_components, function(x) sapply(t(x[,3]), paste0, collapse=' '))
      short.names <- sapply(gReturn$results$address_components, '[[', 2)
      l.dt<-list()
      if (class(short.names)=='list'){
        for (i in 1:length(short.names)){
          col.name <- col.names[[i]]
          row.data <- as.data.table(t(short.names[[i]]))
          state.index <- which(row.data %in% states.abbrev)
          col.name[state.index] <- 'state'
          setnames(row.data, col.name)
          l.dt[[i]]<-row.data
        }
      } else {
        for (i in 1:dim(short.names)[2]){
          col.name <- col.names[,i]
          row.data <- as.data.table(t(short.names[,i]))
          state.index <- which(row.data %in% states.abbrev)
          col.name[state.index] <- 'state'
          setnames(row.data, col.name)
          l.dt[[i]]<-row.data
        }
      }
      DT.address <- rbindlist(l.dt, use.names=TRUE, fill=TRUE)
      zips <- DT.address$postal_code
      states <- DT.address$state
      id <- list()
      id$state.match <- which(states == 'CO')
      id$zip.match <- which(zips %in% study.zips)
      # Restrict matches study state and zip
      if (length(id$state.match) > 0 & length(id$zip.match)>0){
        id$full.match <- which(is.null(gReturn$results$partial_match))
      } else {
        id$full.match <- integer()
      }
      
      id$rooftop <- which(gReturn$results$geometry$location_type=='ROOFTOP')
      id$interpolated <- which(gReturn$results$geometry$location_type=='RANGE_INTERPOLATED')
      i.full <- which(names(id)=='full.match')
      i.zip <- which(names(id)=='zip.match')
      i.state <- which(names(id)=='state.match')
      i.rooftop <- which(names(id)=='rooftop')
      i.interp <- which(names(id)=='interpolated')
      l.comb <- list()
      l.comb[[1]] <- data.table(i=i.full, j=i.rooftop)
      l.comb[[2]] <- data.table(i=i.zip, j=i.rooftop)
      l.comb[[3]] <- data.table(i=i.state, j=i.rooftop)
      l.comb[[4]] <- data.table(i=i.full, j=i.interp)
      l.comb[[5]] <- data.table(i=i.zip, j=i.interp)
      l.comb[[6]] <- data.table(i=i.state, j=i.interp)
      DT.comb <- rbindlist(l.comb, use.names=TRUE)
      match.check <- FALSE
      i.comb <- 0
      while(match.check == FALSE & i.comb <= length(l.comb)){
        i.comb <- i.comb + 1
        i <- DT.comb[i.comb, i]
        j <- DT.comb[i.comb, j]
        intersect.id <- intersect(id[[i]], id[[j]])
        if (length(intersect.id)>0){
          intersect.id <- intersect.id[1]
          match.check<-TRUE
        }
      }
      if(match.check==FALSE){
        intersect.id <- 1
      }
      g$address.return <- gReturn$results$formatted_address[[intersect.id]]
      g$street <- geocode.street.create(gReturn$results$address_components[[intersect.id]])
      g$cityStateZip <- geocode.cityStateZip.create(gReturn$results$address_components[[intersect.id]])
      g$gLong <- gReturn$results$geometry$location$lng[[intersect.id]]
      g$gLat <- gReturn$results$geometry$location$lat[[intersect.id]]
      if (match.check==TRUE){
        g$match.descr <- paste(names(id)[i], names(id)[j], sep='-')
        g$match.rank <- i.comb
      } else {
        g$match.descr <- 'No Match'
        g$match.rank <- 'No Match'
      }
      iter
      l[[iter]] <- as.data.table(g)
      cat(c(paste('Search Address: ', address.query),
            paste('Return Address: ', g$address.return),
            paste('Match Description:', g$match.descr)),
          paste('Match.rank:', g$match.rank),'\n', sep='\n')
      gGood <- i.comb==1 
    }
    # Prepare for next iteration
    iter <- iter + 1
  }
  if (length(l)==0){
    geocodes.bad.address <- geocode::geocodes.bad.address(data.root, GEO, source) 
    DT.geo <- data.table(match.rank='No Match')
  } else {
    DT.geo <- rbindlist(l, use.names=TRUE, fill=TRUE)
    DT.geo <- DT.geo[match.rank!='No Match' & street !='']
    DT.geo$zip <- methods.string::explode.cityStateZip(DT.geo, l.study.extent$cities)
    DT.geo <- DT.geo[match.rank==1 | (match.rank==2 & zip %in% l.study.extent$zips)]
    if(nrow(DT.geo)>0){
      DT.geo <- DT.geo[order(as.integer(match.rank)),.(long=gLong, lat=gLat, match.descr, match.rank)][1]
      DT.geo <- cbind(GEO, DT.geo)
      DT.geo$data.source <- source
      points::address.update(dt.pkg.path, DT.geo, points.source=source)
    }
    if(nrow(DT.geo)==0){
      DT.geo <- rbindlist(list(DT.geo, data.table(match.rank='No Match')), use.names=TRUE, fill=TRUE)
      geocodes.bad.address <- geocode::geocodes.bad.address(data.root, GEO, source)
      DT.geo$long <- 0
      DT.geo$lat <- 0
    }
    return(DT.geo)
  }
}
#' @title geocodes.bad.address
#'
#' @description Creates and loads points.address file
#' @param data.root dropbox root pkg.data dir
#' @param geocodes.bad.address.new new bad geocoded address
#' @param location.source location source
#' @keywords points, clean, geocode
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     foreign
#'     parcels
#'     googleway
#'     pkg.data.paths
geocodes.bad.address <- function(data.root, geocodes.bad.address.new, location.source){
  file.name <- NULL
  pkg.name <- NULL
  dt.pkg.path <- pkg.data.paths::dt(data.root)
  geocodes.bad.address.location <- dt.pkg.path[file.name=='geocodes.bad.address.rdata']$sys.path
  if (file.exists(geocodes.bad.address.location)){
    load(geocodes.bad.address.location)
  } else {
    parcels.address <- parcels::address(unique(dt.pkg.path[pkg.name=='parcels']$pkg.root))
    geocodes.bad.addresss <- data.table::copy(parcels.address[FALSE])
    setnames(geocodes.bad.address, 'street.num.low', 'street.num')
    cols <- names(geocodes.bad.address)[grep('.low|.hi', names(geocodes.bad.address), invert=TRUE)]
    geocodes.bad.address <- geocodes.bad.address[, (cols), with=FALSE]
    save(geocodes.bad.address, file=geocodes.bad.address.location)
  }
  if (!missing(geocodes.bad.address.new)){
    id.start <- max(as.integer(geocodes.bad.address$location.id))+1
    id.end <- id.start + nrow(geocodes.bad.address.new)-1
    geocodes.bad.address.new$location.id <- as.character(id.start:id.end)
    geocodes.bad.address.new$location.source <- location.source
    geocodes.bad.address.new$location.type <- 'geocodes.bad'
    cols <- names(geocodes.bad.address.new)[grep('record.id|address.id', names(geocodes.bad.address.new), invert=TRUE)]
    geocodes.bad.address.new <- geocodes.bad.address.new[, (cols), with=FALSE]
    geocodes.bad.address.new <- geocodes.bad.address.new[, (names(geocodes.bad.address)), with=FALSE]
    geocodes.bad.address <- rbindlist(list(geocodes.bad.address, geocodes.bad.address.new), use.names=TRUE)
    setkeyv(geocodes.bad.address, names(geocodes.bad.address)[!(names(geocodes.bad.address) %in% 'location.id')])
    geocodes.bad.address <- unique(geocodes.bad.address)
    save(geocodes.bad.address, file=geocodes.bad.address.location)
    # For bad hand coded matches
    ## geocodes.bad.address.new <- points.address[(match.rank==2 & !(zip %in% study.zips))|match.rank %in% c(3,5,6)]
  }
  return(geocodes.bad.address)
}
#' @title geocodes.cityStateZip.create
#'
#' @description creates cityStateZip from google query
#' @param x google api address results
#' @keywords google, api, cityStateZip
#' @export
#' @import stringr
#'     data.table
geocode.cityStateZip.create <- function(x){
  types <- NULL; short_name <- NULL; 
  x <- as.data.table(x)
  city <-  x[grep('locality', types),short_name]
  state <- x[grep('administrative_area_level_1', types), short_name]
  zip <-  x[types=='postal_code', short_name]
  cityStateZip <- paste0(city, ', ', state, ' ', zip)
  return(cityStateZip)
}
#' @title geocodes.street.create
#'
#' @description creates street from google query
#' @param x google api address results
#' @keywords google, api, street
#' @export
#' @import stringr
#'     data.table
geocode.street.create <- function(x){
  short_name <- NULL; types <- NULL
  x <- as.data.table(x)
  street.num <- x[types=='street_number', short_name]
  route <- x[types=='route', short_name]
  street <- paste(street.num, route)
  if (length(street)==0){
    street <- ''
  }
  return(street)
}
