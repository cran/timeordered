shortesthoppath <-
function(g, startvertexname, startvertextime, stopvertexname, stopvertextime)
{
	if (length(startvertexname) != 1 | length(stopvertexname) != 1)
	{
		stop("must provide single startvertex and stop vertex")	
	}
	startvertex <- V(g)[Name==startvertexname & Time==startvertextime]
	stopvertex <- V(g)[Name==stopvertexname & Time==stopvertextime]
	vertices <- get.shortest.paths(g, startvertex, stopvertex, mode="out", weights=E(g)$HopCost)
	
	return(V(g)[vertices[[1]] ])
}

