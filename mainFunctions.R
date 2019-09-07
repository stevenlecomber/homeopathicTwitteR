#' Carry out serial dilutions
#'
#' Carries out serial dilutions of text strings.
#'
#' @param input original text
#' @param dilution dilution strength
#' @param reps number of serial dilutions
#' @param intermediates whether or not to show intermediate steps
#' 
#' @export
#' @examples
#' serialDilute("blah", 100, 30, FALSE)

serialDilute <- function(input, dilution, reps, intermediates=FALSE)
	{	
		# first define function for single dilution
		diluteTweet <- function(original, dilution)
			{
				inputTweet <- original
				stringLength <- nchar(inputTweet)
	
				# dilution solution
				dilution <- paste(sample(c(" ", ". ", letters), (dilution*stringLength), replace=TRUE), collapse="")
				# add tweet to solution
				diluted <- paste(inputTweet, dilution, collapse="")
				lengthDiluted <- nchar(diluted)
	
				# extract new string to same length as original tweet
				toSelect <- sample(1:lengthDiluted, nchar(inputTweet), replace = FALSE)
				selected <- rep(NA,nchar(inputTweet))
				for(i in 1:nchar(inputTweet))
					{
						selected[i] <- substr(diluted, toSelect[i], toSelect[i])
					}
				newTweet <- paste(selected, collapse="")
				return(newTweet)
			}

		# loop for number of serial dilutions
		current <- input
		for(i in 1:reps)
			{
				current <- diluteTweet(current, dilution)
				if(intermediates==TRUE) print(current)
			}
		finalTweet <- current
		return(finalTweet)
	}