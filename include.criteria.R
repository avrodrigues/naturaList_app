require(dplyr)
require(shiny)
require(shinyWidgets)

abrev.pttn <- function(df, linha){
  
  abv.num <- grep("Abbrev",colnames(df))
  
  abv1 <- abv.num[1]
  first.L <- paste0("\\<", df[linha, abv1])
  
  nonblank <- df[linha,-1]!=""
  nonblank <- c(FALSE, nonblank)
  
  pttn <- paste(unlist(df[linha ,nonblank]), collapse = '|')
  
  if(length(df[linha ,nonblank]) < 2)  pttn <- df[linha,abv1]
  
  
  res <- list(first.L, pttn)
  return(res)
}
# função 2

func.det.by.esp <- function(sp.df,i, specialist){
  
  padr.det <- abrev.pttn(specialist, i)
  
  g.det <- unique(grep(paste(specialist[i,1]), ignore.case = T,
                       sp.df$determined.by))     
  
  g.det.1 <- unique(grep(padr.det[1], ignore.case = F,
                         sp.df$determined.by[g.det]))
  
  g.det.ok <- unique(grep(padr.det[2], ignore.case = F,
                          sp.df$determined.by[g.det[g.det.1]]))
  
  g.det[g.det.1[g.det.ok]]
  
}




# função 3
# seleciona da lista de especialistas aqueles que foram identificados no data.frame 
pttn.all.specialist <- function(df){
  
  pttn <- character(nrow(df))
  for(i in 1:nrow(df)){
    
    nonblank <- df[i,]!=""
    pttn[i] <- paste(df[i ,nonblank], collapse = '|')
    
  }
  
  pttn.all <- paste(pttn, collapse = '|')
  pttn.all
}



# função 4


verify.specialist <- function(pattern, string){
  require(stringr)
  collection.new <- gsub(pattern, "", string, ignore.case = T)
  
  g_zero <- str_replace_all(collection.new, pattern = "[[:punct:]]", "")
  
  zero <- nchar(str_replace_all(g_zero, pattern = "\\s+", "")) == 0
  
  if(zero == T) return("")
  if(zero == F) return("_verify")
  
}
# função 5

specialist.conference <- function(pt.df, especialistas){
  spe.obs <- which(lapply(especialistas[,1], 
                          function(x) grep(x, pt.df["determined.by"], ignore.case = T)) == 1)
  
  pttn.all <- pttn.all.specialist(especialistas[spe.obs,])
  verify <- verify.specialist(pttn.all, pt.df["determined.by"])
  
  crt <- paste0("Level_1", verify)
}

# função 6
has.det.ID <- function(sp.df){
  
  sem.det <- paste(c("Sem determinador","^NA$", "RRC ID Flag", "^NA $","^ $","^$", "^-$", "^_$"), collapse = '|')
  g.sem.det <- grep(sem.det, sp.df$determined.by)
  sem.det.ID <- c(which(is.na(sp.df$determined.by)), g.sem.det) 
  
  ID <- !seq_along(sp.df$determined.by) %in%  sem.det.ID
  
  which(ID)
}

# função 7
include.criteria <- function(df, spec.list = NULL){
  
  vetor.criterio <- rep("Level_6", nrow(df))
  
  basis.human <- which(df$basis.of.rec %in% "HUMAN_OBSERVATION")
  
  vetor.criterio[basis.human] <- "Level_5"
  
  basis.preserved <- which(df$basis.of.rec %in% "PRESERVED_SPECIMEN")
  
  vetor.criterio[basis.preserved] <- "Level_4"
  
  image <- which(df$media.type %in% "STILLIMAGE")
  
  vetor.criterio[image] <- "Level_3"
  
  det.ID_teste <- has.det.ID(df)
  
  vetor.criterio[det.ID_teste] <- "Level_2"
  
  if(!is.null(spec.list)){
  det.by.esp <- unlist(lapply(1:nrow(spec.list), 
                              function(x) func.det.by.esp(df, x, spec.list)))
  
  vetor.criterio[det.by.esp] <- apply(df[det.by.esp,], 1, 
                                      function(x) specialist.conference(x, spec.list))
  }
  df$vetor.criterio <- vetor.criterio
  
  od1 <- df[order(df$year.event, decreasing = T),]
  od2 <- od1[order(od1$dateIdentified, decreasing = T),]
  od3 <- od2[order(od2$vetor.criterio),]
  
  
  row.names(od3) <- 1:nrow(od3)
  
  od3
}