shortesttimepath <-
function(g, startvertexname, startvertextime, stopvertexname)
{
	if (length(startvertexname) != 1 | length(stopvertexname) != 1)
	{
		stop("must provide single startvertex and stop vertex")	
	}
	startvertex <- V(g)[Name==startvertexname & Time==startvertextime]
	stopvertices <- V(g)[Name==stopvertexname & Time>=startvertextime]	
	paths <- get.shortest.paths(g, startvertex, stopvertices, mode="out", weights=E(g)$TimeCost)
	
	shortestpath  <- paths[[which.min(lapply(paths, function(x) { tail(V(g)[x]$Time,1) })) ]]
	return(V(g)[shortestpath])
}

