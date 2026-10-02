# Generate R6 Class
FilePaths <- R6::R6Class(
  classname = "FilePaths",
  public =  list(
    fijipath = normalizePath("C:/Users/Etudiants/Documents/Fiji.app/ImageJ-win64.exe"),
    fijimacropath=normalizePath("C:/Users/Etudiants/Documents/shinySpotApp/macro-crop-enhance.ijm"),
    ilastikpath = normalizePath("C:/Program Files/ilastik-1.4.0rc8/ilastik.exe"),
    fileTable = tibble::tibble(picpaths=character(),
                               expepath=character(),
                               croppedfilespathPNG=character(),
                               croppedfilespathH5=character(),
                               pixelclassresultpath=character(),
                               objectclasstablepath=character(),
                               outputggplotpath=character(),
                               outputtablecsvpath=character()),
    inputFileFormat = "format_1",
    # picpaths = NULL,
    # expepath = NULL,
    resultspath = NULL,
    # croppedfilespathPNG = NULL,
    # croppedfilespathH5 = NULL,
    rawImageIndex=1,
    processedImageIndex=1,
    autocontextpath=normalizePath("C:/Users/Etudiants/Documents/shinySpotApp/pixel-autocontext.ilp"),
    objectclasspath=normalizePath("C:/Users/Etudiants/Documents/shinySpotApp/object-classification.ilp"),
    # pixelclassresultpath=NULL,
    # objectclasstablepath=NULL,
    # outputggplotpath=NULL,
    # outputtablecsvpath=NULL,
    processingStartTime=NULL,
    processingEndTime=NULL
  )
)
