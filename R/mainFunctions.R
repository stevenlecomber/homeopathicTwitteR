#' Single dilution
#'
#' Carries out single dilution of text strings.
#'
#' @param original original text
#' @param diluteBy dilution strength
#' 
#' @export diluteTweet
#' @examples
#' diluteTweet("blah", 20)

diluteTweet <- function(original, diluteBy)
			{
				inputTweet <- original
				stringLength <- nchar(inputTweet)
	
				# dilution solution
				dilution <- paste(sample(c(" ", ". ", letters), (diluteBy*stringLength), replace=TRUE), collapse="")
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

#------------------------------------------------
#' Serial dilution
#'
#' Carries out single dilution of text strings.
#'
#' @param input original text
#' @param dilution dilution strength
#' @param reps number of serial dilutions
#' @param intermediates whether or not to show intermediate steps
#' 
#' @export serialDilution
#' @examples
#' serialDilute("blah", 100, 30, FALSE)

serialDilute <- function(input, dilution, reps, intermediates=FALSE)
	{	
		
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
