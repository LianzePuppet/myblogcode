bitrdqd<-function (geneID, fromType, toType, OrgDb, drop = TRUE) 
{
    idTypes <- idType(OrgDb)
    msg <- paste0("should be one of ", paste(idTypes, collapse = ", "), 
        ".")
    if (!fromType %in% idTypes) {
        stop("'fromType' ", msg)
    }
    if (!all(toType %in% idTypes)) {
        stop("'toType' ", msg)
    }
    geneID %<>% as.character %>% unique
    db <- load_OrgDb(OrgDb)
    res <- suppressWarnings(AnnotationDbi::select(db, keys = geneID, 
        keytype = fromType, columns = c(fromType, toType)))
    ii <- which(is.na(res[, 2]))
    if (length(ii)) {
        n <- res[ii, 1] %>% unique %>% length
        if (n) {
            warning(paste0(round(n/length(geneID) * 100, 2), 
                "%"), " of input gene IDs are fail to map...")
        }
        if (drop) {
            res <- res[-ii, ]
        }
    }
    return(res)
}