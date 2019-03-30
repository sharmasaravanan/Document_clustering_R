#required libraries
library('tm')
library('wordcloud')

#dataset directory
dir <- "./20-newsgroups/"
files <- list.files(path = dir,pattern = '*.txt')
fileList <- paste(dir,"/",files,sep="")

#reading all the files 
content <- lapply(fileList,FUN = readLines)
corpusText <- lapply(content, FUN = paste, collapse=" ")

#preprocessing
puntuationFree <- gsub(pattern = "\\W",replace = " ",corpusText)
numberFree <- gsub(pattern = "\\d",replace = " ",puntuationFree)
underscoreFree <- gsub(pattern = "_",replace = " ",numberFree)
lowerCase <- tolower(underscoreFree)
stopwordsFree <- removeWords(lowerCase,stopwords("english"))
onewordFree <- gsub(pattern = "\\b[A-z]\\b{1}",replace =" ",stopwordsFree)
spaceFree <- stripWhitespace(onewordFree)

#wordcloud
wordcloud(spaceFree, random.order = FALSE, col=rainbow(7))

#vectorisation
vectorSource <- Corpus(VectorSource(spaceFree))
print(vectorSource)

#document term matrix
termMatrix <- TermDocumentMatrix(vectorSource)
TDMatrix <- as.matrix(termMatrix)
print("Displaying first 5 rows of Term-Document Matrix")
print(TDMatrix[1:5,])
comparison.cloud(TDMatrix)

norm_eucl <- function(mat){
	mat/apply(mat,1,function(x) sum(x^2)^.5)
}
