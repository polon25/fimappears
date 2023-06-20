#' Function plots the presence of MLP:FiM characters in episodes of each season of the series
#'
#' @param char.appears.df VA data frame containing information on the percentage of presence of characters in MLP:FiM seasons
#' @param col.palette color palette in the chart, a vector containing strings with color values stored in RGB hexadecimal, having the same length as the number of characters
#' @return  graph of percentage attendance of characters in MLP:FiM series
#' @examples
#' char.appears.plot(char.appears.df, col.palette)
#' @export

char.appears.plot <- function(char.appears.df, col.palette){

    stopifnot(colnames(char.appears.df)[1]=="Season",
              is.character(col.palette),
              length(char.appears.df)==length(col.palette)+1,
              stringr::str_detect(col.palette,"#"))

    char.appears.df <- reshape2::melt(char.appears.df, id="Season")
    g<-ggplot2::ggplot(data=char.appears.df,aes(y = value*100))+
        ggiraph::geom_col_interactive(aes(x = factor(Season),
                                 fill=variable,
                                 onclick=paste('window.open("https://mlp.fandom.com/wiki/',variable,'")'),
                                 tooltip=variable),
                             position="dodge2")+
        ggplot2::scale_fill_manual(values=col.palette)+
        #geom_line(aes(x = Season, color=variable),size = 2)+
        ggplot2::scale_color_manual(values=col.palette)+
        labs(x="Season",y="Percentage appearance in episodes",color="Character",fill="Character",
             title='Appearance of the main characters in "MLP: Friendship is Magic"')+
        theme_dark()

    ggiraph::girafe(ggobj = g)
}
#Testy jednostkowe
testthat::test_that("char.appears.plot works",{
    testthat::expect_error(char.appears.plot(data.frame("Season"=c(1),"TW"=c(2))))
    testthat::expect_error(char.appears.plot(data.frame("Season"=c(1),"TW"=c(2)),c("#123456"),c(1)))
    testthat::expect_error(char.appears.plot(1,c("#123456")))
    testthat::expect_error(char.appears.plot(data.frame("Season"=c(1),"TW"=c(2)),c(1)))
    testthat::expect_error(char.appears.plot(data.frame("ABC"=c(1),"TW"=c(2)),c("#123456")))
    testthat::expect_error(char.appears.plot(data.frame("Season"=c(1),"TW"=c(2),"RD"=c(3)),c("#123456")))
    testthat::expect_error(char.appears.plot(data.frame("Season"=c(1),"TW"=c(2),"RD"=c(3)),c("#123456","234567")))
    testthat::expect_error(char.appears.plot(data.frame("Season"=c(1),"TW"=c(2)),c("#123456","#234567")))
})
