installedP <- match(c("httr","xml2"), row.names(installed.packages()))
toInstall<- is.na(installedP)
if(!all(toInstall)){
    install.packages(installedP[toInstall])    
}

getCoronaPage <- function(pageNumber){
    respons <- GET(sprintf("https://koronavirus.gov.hu/elhunytak?page=%s",pageNumber))
    text_Respons <- content(respons,"text")
    # converting text_respons to xml

    # read_xml(textRespons) is not good, because it is not a valid xml :(
    xml_Respons <- read_html(text_Respons)
    # The content contains only one table... <table>...</table>
    extracted_table<- xml_find_all(xml_Respons,".//table")

    korona <- do.call(rbind,lapply(xml_find_all(extracted_table,".//tr"),function(row){
                                            sapply(xml_find_all(row,".//td"),function(item){
                                                       trimws(xml_text(item)) 
                                            })
                                        })
                    )

    colnames(koronaFirst) <- c("id","nem","kor","illnesses")
    korona <- as.data.frame(korona)
    korona$id <- as.integer(unlist(korona[,1]))
    korona$nem <- as.factor(unlist(korona[,2]))
    korona$kor <- as.integer(unlist(korona[,3]))
    korona$illnesses <- unlist(korona$illnesses)
    korona
}

getCoronaAll <- function(){
    koronaFirst <- getCoronaPage(0)
    pages <- 1:(max(koronaFirst$id) %/% (nrow(koronaFirst)))
    i<- 1
    koronaAll <- lapply(pages,function(page){
                cat(sprintf("Downloading page %s ...\n", i))
                i <<- i+1
                getCoronaPage(page)
    })
    koronaAll <- do.call(rbind.data.frame,koronaAll)    
    koronaAll <- rbind.data.frame(koronaFirst, koronaAll)
    koronaAll
}
