installAndLoad <- function(packages){
    ##This function installs and loads multiple packages
    loadablePackages <- packages
    sapply(packages, function(x){
        if(!is.element(x,installed.packages())){
            tryCatch(install.packages(x), error = warning(paste0("The package: ",x, "is not installable")))
            loadablePackages <<-  setdiff(loadablePackages,x)
        }  
    })
    print(loadablePackages)
    sapply(loadablePackages,function(x){library(x, character.only = TRUE)})
}
