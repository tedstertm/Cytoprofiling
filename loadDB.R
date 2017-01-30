#This function uses the 'RSQLite' package to read in .db files
#That were produced by CellProfiler for CellProfiler Analyst


#This does is for pattern matching. It makes everything nice and neat.
clean.variable.name <- function(variable.name)
{
	variable.name <- gsub('^[^a-zA-Z0-9]+', '', variable.name, perl = TRUE)
	variable.name <- gsub('[^a-zA-Z0-9]+$', '', variable.name, perl = TRUE)
	variable.name <- gsub('_+', '.', variable.name, perl = TRUE)
	variable.name <- gsub('-+', '.', variable.name, perl = TRUE)
	variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
	variable.name <- gsub('\\.+', '.', variable.name, perl = TRUE)
	variable.name <- gsub('[\\\\/]+', '.', variable.name, perl = TRUE)
	variable.name <- make.names(variable.name)
	return(variable.name)
}


#Here's the meaties of the stuff. It returns everything  in the db
#for some reason, but if Jay can help you out it should return 
#whichever data you ask for. The default data that you need for CPA 
#is MyExpt.Per.Object

load.db <- function(file.name,  return.variable = NULL)
{
	require('RSQLite')
	
	sqlite.driver <- dbDriver("SQLite")
	connection <- dbConnect(sqlite.driver,
					    dbname = file.name)
	
	tables <- dbListTables(connection)
	for (table in tables)
	{
		message(paste('  Loading table:', table))
		
		data.parcel <- dbReadTable(connection,
							  table,
							  row.names = NULL)
		
		assign(clean.variable.name(table),
			  data.parcel,
			  envir = .GlobalEnv)
	}
	
	disconnect.success <- dbDisconnect(connection)
	if (! disconnect.success)
	{
		warning(paste('Unable to disconnect from database:', filename))
	}
	
	if(is.null(return.variable) == TRUE){
		
	}
	else{
		return(return.variable)
	}
	
		
}

