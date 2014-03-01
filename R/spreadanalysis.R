spreadanalysis <-
function(g, timedelays, numsamples)
{
	vertices <- V(g)
	vertexnames <- unique(V(g)$Name)
	
	numreached <- matrix(NA, nrow=numsamples, ncol=length(timedelays))
	
	verticestosample <- sample(vertices, numsamples)
	startvertexnames <- verticestosample$Name
	
	for (i in 1:numsamples)
	{
		sg <- induced.subgraph(graph=g,vids=subcomponent(graph=g, v=verticestosample[[i]],mode="out"))
		
		for (j in 1:length(timedelays))
		{
			starttime <- min(V(sg)$Time)
			
			numreached[i,j] <- length(unique(V(sg)[V(sg)$Time < (starttime + timedelays[j])]$Name))	
		}
		
		print(i/numsamples)
	}
	
	tempresult <- data.frame(startvertexnames, numreached/length(vertexnames))
	names(tempresult) <- c("startvertex",paste(timedelays))

	return(tempresult)
	

}

