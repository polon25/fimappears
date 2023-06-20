#' Function returns a vector containing information about the coat color of MLP:FiM characters
#'
#' @param char.names A vector with characters names
#' @return  Function returns a graph containing the RGB values of the coat colors of the MLP:FiM characters
#' @examples
#' char.colors(c("Twilight Sparkle", "Rarity"))
#' @export

char.colors <- function(char.names){
    colors.vec <- sapply(1:length(char.names), function(i){
        #Pobierz stronę o postaci
        char.name<-stringr::str_replace(char.names[i], fixed(" "), "_")
        download.file(url=stringr::str_c("https://mlp.fandom.com/wiki/",char.name),destfile="mlp_fim_char.html")
        webpage <- readLines("mlp_fim_char.html")
        stopifnot(is.character(webpage),webpage[1]=="<!DOCTYPE html>")
        file.remove("mlp_fim_char.html") #Usuń plik ze stroną

        #Pobierz informację o kolorze
        color.line<-which(stringr::str_detect(webpage,"<td><b>Coat</b>"))
        color.line<-color.line+2
        color.line.start<-stringr::str_locate(webpage[color.line],fixed("#"))[1]
        color.name<-substr(webpage[color.line],color.line.start,color.line.start+6)
        return(color.name)
    })
}
