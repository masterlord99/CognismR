pcaPlot <- function(tokens=NULL,tok_space=NULL ,embedding_matrix,numb=NULL,perp=10,pca=T,rand=T,topics=F){
  if(is.null(numb) | numb > nrow(embedding_matrix)){
    numb <- nrow(embedding_matrix)
  }
  if(!is.null(tokens)){
    vec <- find_similar_words(tokens,tok_space)$vectorized
    if(!topics){
      embedding_matrix <- rbind(embedding_matrix,vec)
      embedding_matrix <- embedding_matrix[!duplicated(embedding_matrix),]
    }
  }


  tsne <- Rtsne(embedding_matrix, perplexity =perp, pca = pca)

  tsne_plot <- tsne$Y %>%
    as.data.frame() %>%
    mutate(word = row.names(embedding_matrix))

  if(!topics){
    dist_mat <- rdist(tsne_plot[,1:2]) ## euclid
    ## for each word find best match
    rownames(dist_mat) <- rownames(embedding_matrix)
    search <- dist_mat[rownames(vec),]
    names(search) <- rownames(dist_mat)
    search <- search[order(search)]

    if(nrow(embedding_matrix) > (numb +11)){
      if(rand){
        nnn <- sample(11:nrow(embedding_matrix),numb)
        nnn <- c(1:10,nnn)
      }else{
        nnn <- 1:(numb+10)
      }
    }else{
      nnn <- 1:nrow(embedding_matrix)
    }
  }

  if(topics){
    match <- find_similar_words(tokens,embedding_matrix = tok_space,embedding_matrix2 = embedding_matrix,n=3)$matches
    cord <- (tsne_plot[tsne_plot$word %in% names(match),] %>% na.omit() %>% .[,1:2] * (match/sum(match))) %>% apply(.,2,sum)
    cord <- as.data.frame(t(cord))
    tsne_plot %>%
      ggplot(aes(x = V1, y = V2, label = word)) +
      geom_point(color="darkred",lwd=1.5) +
      geom_point(x=cord$V1,y=cord$V2,pch=21, fill=NA, size=75, colour="red", stroke=4) +
      geom_text_repel()
  }else{
    cord <- tsne_plot[tsne_plot$word == rownames(vec),] %>% na.omit() %>% .[,1:2]
    tsne_plot[tsne_plot$word %in% names(search)[nnn],] %>%
      ggplot(aes(x = V1, y = V2, label = word)) +
      geom_point(color="darkred",lwd=1.5) +
      geom_point(x=cord$V1,y=cord$V2,pch=21, fill=NA, size=30, colour="red", stroke=3) +
      geom_text_repel()
  }
}


dend <- function(xIn){
  distProstor <- sim2(xIn)
  distProstor <- (distProstor + abs(min(distProstor))) / (1+abs(min(distProstor)))
  distProstor <- 1 - distProstor
  distProstor <- as.dist(distProstor)

  drevo <- hclust(distProstor)
  dend <- as.dendrogram(drevo)
  memb <- cutree(drevo,h=0.5)
  ggdendrogram(dend, rotate = TRUE, theme_dendro = F,labels = T,segments = T)
}


find_similar_words <- function(words, embedding_matrix=tokensEmbedding,embedding_matrix2=NULL, n = 10) {
  if(is.null(embedding_matrix2)){
    embedding_matrix2 <- embedding_matrix
  }
  similarities <- embedding_matrix[c(words), , drop = FALSE]
  if(nrow(similarities) > 1){
    similarities <- colSums(similarities) / nrow(similarities)
    similarities <- as.matrix(similarities) %>% t(.)
    rownames(similarities) <- paste0(words,collapse = " + ")
  }
  vec <- similarities
  similarities <- similarities %>%  sim2(embedding_matrix2, y = ., method = "cosine")

  v <- list(query=paste0(sort(words),collapse = " + "),matches = similarities[,1] %>% sort(decreasing = TRUE) %>% head(n),vectorized = vec)
  v
}



createGroups <- function(query,space1=tokensEmbedding,space2=vectorizedSpace){
  a <-lapply(colnames(query), function(x){
    x1 <- find_similar_words(x,embedding_matrix =space1,embedding_matrix2 = space2,n=10 )$matches
    lapply(1:length(x1), function(x){
      paste0(names(x1)[x]," = ",round(x1[x],3))
    }) %>% unlist()
  }) %>% stringi::stri_list2matrix(.)
  a <- data.table(a)
  colnames(a) <- colnames(query)
  a
}


createTopics <- function(tokens,space1,space2){
  data.table(t(round(find_similar_words(tokens,embedding_matrix = space1,
                                        embedding_matrix2 = space2,n = 5)$matches,3)))
}

compareTitles <- function(title1,title2,embedding_matrix=tokensEmbedding){
  tokens1 <- assignRoleTokens(title1,prior_ = prior)[,-1] %>% unlist()
  tokens2 <- assignRoleTokens(title2,prior_ =prior)[,-1] %>% unlist()
  vec1 <- find_similar_words(tokens1,embedding_matrix)$vectorized
  vec2 <- find_similar_words(tokens2,embedding_matrix)$vectorized

  find1 <- find_similar_words(tokens1,rbind(embedding_matrix,vec2),n=5)[1:2]
  find1[[2]] <- round(find1[[2]],3)
  find2 <- find_similar_words(tokens2,rbind(embedding_matrix,vec1),n=5)[1:2]
  find2[[2]] <- round(find2[[2]],3)
  qqq <- round(psim2(vec2,vec1),3)
  list(find1,find2,qqq)

}

get_embedding_matrix <- function(){
  tokensEmbedding
}


get_vectorized_space <- function(){
  vectorizedSpace
}

get_space_distances <- function(){
  sim2(vectorizedSpace,vectorizedSpace,method = "cosine")
}


get_embedded_titles <- function(query){
  tokens <- assignRoleTokens(query)
  if(length(query)==1){
    if(is.na(tokens$token1)){
      warning("Unable to tokenize!\n")
      return(matrix(NA,1,ncol(tokensEmbedding)))
    }else{
      tmp <- tokensEmbedding[(tokens[1,grepl(colnames(tokens),pattern = "token")] %>% unlist()) ,]
      if(is.matrix(tmp)){
        return((as.matrix(apply(tmp, 2, mean)) %>% t))
      }else{
        return(as.matrix(tmp) %>% t)
      }
    }
  }else{
     vectors <-  apply(tokens[!is.na(tokens$token1),grepl(colnames(tokens),pattern = "token"),drop=F],1,function(x){
        tmp <- tokensEmbedding[na.omit(x),]
        if(is.matrix(tmp)){
          return(apply(tmp,2,mean))
        }else{
          return(tmp)
        }
      }) %>% t

      out <- matrix(NA,length(query),ncol(tokensEmbedding))
      out[!is.na(tokens$token1),] <- vectors
      if(sum(!is.na(tokens$token1)) < length(query)){
        warning("Unable to tokenize some queries!\n")
      }
      return(out)

  }

}


get_tokens_matching <- function(query,n=10){

  similarities <- get_embedded_titles(query)
  ind <- !apply(similarities,1,function(x)any(is.na(x)))
  out <- rep(list(NULL),length(query))

  similarities2 <- similarities %>%  sim2(tokensEmbedding, y = ., method = "cosine")

  for(i in which(ind)){
    out[[i]] <- list(query=query[i],matches = similarities2[,i] %>% sort(decreasing = TRUE) %>% head(n),vectorized = similarities[i,])
  }

  names(out) <- query
  return(out)
}

get_space_matching <- function(query,n=10){

  similarities <- get_embedded_titles(query)
  ind <- !apply(similarities,1,function(x)any(is.na(x)))
  out <- rep(list(NULL),length(query))

  similarities2 <- similarities %>%  sim2(vectorizedSpace, y = ., method = "cosine")

  for(i in which(ind)){
    out[[i]] <- list(query=query[i],matches = similarities2[,i] %>% sort(decreasing = TRUE) %>% head(n),vectorized = similarities[i,])
  }

  names(out) <- query
  return(out)
}













