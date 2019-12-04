#a <- tsne_plot$word[tsne_plot$Year==2018] %>% gsub(.,pattern = " 2018",replacement = "")
rtsne_plot_ <- function(x,pca=T,max_iter = 10000,prepared=T,query=NULL,
                        space_=space,
                        colors= c("brown4","darkgreen","darkblue","darkviolet","red3",'green3', "midnightblue","firebrick2","slateblue4","blue1","violetred3")){
  if(prepared){
    x$Year <- as.integer(x$Year)
    x$lwd <- (x$lwd -3) * 6 + 4
    text___ <- suppressWarnings(x$word %>% as.numeric() %>% is.na())
    x$word[!text___] <- NA
    ind <- rep(F,nrow(x))
    col <- rep("a",nrow(x))
    q <- 1
    for(i in query){
      eee <- grepl(pattern = i,x = row.names(x))
      ind <- ind | eee
      col[eee] <- colors[q]
      q <- q+1
      if(q > length(colors)){
        q <- 1
      }
    }
    eee <- grepl(pattern = "Topic",x = row.names(x))
    ind <- ind | eee
    col[eee] <- "black"
    x$col <- col
    tsne_plot <- x[ind,]
    suppressWarnings({
      
    tsne_plot %>%
      ggplot(aes(x = V1, y = V2, label = word)) + 
      geom_point(color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha*0.6) + 
      #geom_smooth(aes(x=line_x,y=line_y),col="darkred",se=0,formula = y~x**3 + x**2+x) +
      #geom_point(x=cord$V1,y=cord$V2,pch=21, fill=NA, size=75, colour="red", stroke=4) +
      geom_text_repel(size=(tsne_plot$lwd_)*1.2,colour="black",alpha=tsne_plot$alpha,segment.alpha = 0.7,max.iter = 2000,segment.color = tsne_plot$col) +
      ylab("") + xlab("") + ggtitle("TSNE projection of 128 dimensions.")
    
    })
    
  }else{
    if(length(x)==1){
      x = x[[1]]
      x_ <- rbind((x[[1]]),space_)
      perp <- 10
      description <- x[[4]]
      tsne <- Rtsne(x_[!duplicated(x_),], perplexity =perp, pca = pca,max_iter = max_iter)
      
      tsne_plot <- tsne$Y %>%
        as.data.frame() %>%
        mutate(word = row.names(x_[!duplicated(x_),]))
      
      tsne_plot <- tsne_plot[order(tsne_plot$word),]
      ind <- grepl(tsne_plot$word,pattern = "Topic")
      cece <- strsplit(tsne_plot$word[!ind],split = "_") %>% unlist() 
      a <- anytime::anytime(cece[c(F,T)]) %>% lubridate::year()
      a[(diff(a)==0 )%>% which +1] <- a[(diff(a)==0 )%>% which +1] +1 
      tsne_plot$word[!ind] <- paste0(strsplit(tsne_plot$word[!ind],split = "_") %>% unlist() %>% .[c(T,F)],"-",a)
      tsne_plot$col <- "a"
      tsne_plot$col[ind] <- "#2F4F4F"
      tsne_plot$col[!ind] <- "darkred"
      tsne_plot$lwd <- 3
      tsne_plot$lwd[!ind] <- seq(3,4,length.out = sum(!ind))
      tsne_plot$lwd_ <- 4.5
      tsne_plot$lwd_[!ind] <- seq(5,6,length.out = sum(!ind))
      tsne_plot$alpha <- 0.6
      tsne_plot$alpha[!ind] <- seq(0.65,1,length.out = sum(!ind))
      tsne_plot$Year <- 2000
      tsne_plot$Year[!ind] <- a
      
      static <- tsne_plot
      static[!ind,] <- NA
      anim <- tsne_plot
      anim[ind,1:4] <- NA
      
      tsne_plot %>%
        ggplot(aes(x = V1, y = V2, label = word)) + 
        geom_point(color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha) + 
        #geom_smooth(aes(x=line_x,y=line_y),col="darkred",se=0,formula = y~x**3 + x**2+x) +
        #geom_point(x=cord$V1,y=cord$V2,pch=21, fill=NA, size=75, colour="red", stroke=4) +
        geom_text_repel(size=(tsne_plot$lwd_),colour=tsne_plot$col,alpha=tsne_plot$alpha) +
        ylab("") + xlab("") + ggtitle("TSNE projection of 128 dimensions.")
      
      # plot <- ggplot(data = tsne_plot) + 
      #   geom_point(aes(V1,V2),color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha) + 
      #   
      #   geom_text_repel(aes(V1,V2,label=word),size=(tsne_plot$lwd_),colour=tsne_plot$col,alpha=tsne_plot$alpha) +
      #   transition_filter(
      #     transition_length = 1,filter_length = 1,Year==2000, 2000) 
      # geom_point(data=tsne_plot,aes(V1,V2),color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha) +
      # geom_text_repel(data=anim,aes(V1,V2,label=word),size=(tsne_plot$lwd_),colour=tsne_plot$col,alpha=tsne_plot$alpha) +
      # transition_time(tsne_plot$Year)
      # 
      # plot
      # animate(last_plot())
      
      #   geom_point(data=anim,aes(V1,V2),color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha) + 
      #   geom_text_repel(data=anim,aes(V1,V2,label=word),size=(tsne_plot$lwd_),colour=tsne_plot$col,alpha=tsne_plot$alpha) +
      #   ylab("") + xlab("") + transition_time(Year)
      # animate(last_plot())
      
      
      
      
    }else{
      
      z <- lapply(x,function(z)z[[1]] %>% as.data.frame)
      x_ <- do.call(rbind, unname(z))
      x_ <- rbind(x_,space_)
      perp <- 17 + length(x)*5
      
      tsne <- Rtsne(x_[!duplicated(x_),], perplexity =perp, pca = pca,max_iter = max_iter)
      
      tsne_plot <- tsne$Y %>%
        as.data.frame() %>%
        mutate(word = row.names(x_[!duplicated(x_),]))
      
      
      tsne_plot <- tsne_plot[order(tsne_plot$word),]
      ind <- grepl(tsne_plot$word,pattern = "Topic")
      cece <- strsplit(tsne_plot$word[!ind],split = "_") %>% unlist() 
      a <- anytime::anytime(cece[c(F,T)]) %>% lubridate::year()
      a[(diff(a)==0 )%>% which +1] <- a[(diff(a)==0 )%>% which +1] +1 
      tsne_plot$word[!ind] <- paste0(strsplit(tsne_plot$word[!ind],split = "_") %>% unlist() %>% .[c(T,F)],"-",a)
      tsne_plot$col <- "a"
      tsne_plot$col[ind] <- "black"
      tsne_plot$lwd <- 3
      tsne_plot$lwd_ <- 4.5
      tsne_plot$alpha <- 0.6
      tsne_plot$Year <- 2000
      
      
      for(i in 1:length(x)){
        tmp = grepl(tsne_plot$word,pattern = x[[i]]$name) 
        while(i > length(colors)){
          i <- i - length(colors)
        }
        tsne_plot$col[tmp] <- colors[i]
        tsne_plot$lwd[tmp] <- seq(3,4,length.out = sum(tmp))
        tsne_plot$lwd_[tmp] <- seq(4,5,length.out = sum(tmp))
        tsne_plot$alpha[tmp] <- seq(0.65,1,length.out = sum(tmp))
        cece <- strsplit(tsne_plot$word[tmp],split = "_") %>% unlist() 
        a <- anytime::anytime(cece[c(F,T)]) %>% lubridate::year()
        a[(diff(a)==0 )%>% which +1] <- a[(diff(a)==0 )%>% which +1] +1 
        tsne_plot$Year[tmp] <- a
      }
      
      
      tsne_plot %>%
        ggplot(aes(x = V1, y = V2, label = word)) + 
        geom_point(color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha) + 
        #geom_smooth(aes(x=line_x,y=line_y),col="darkred",se=0,formula = y~x**3 + x**2+x) +
        #geom_point(x=cord$V1,y=cord$V2,pch=21, fill=NA, size=75, colour="red", stroke=4) +
        geom_text_repel(size=(tsne_plot$lwd_),colour=tsne_plot$col,alpha=tsne_plot$alpha) +
        ylab("") + xlab("") + ggtitle("TSNE projection of 128 dimensions.")
    }
  }
}


# library(gganimate)
# 
# p <- tsne_plot %>%
#   ggplot(aes(x = V1, y = V2, label = word)) + 
#   geom_point(color=tsne_plot$col,lwd=tsne_plot$lwd,alpha=tsne_plot$alpha) +
#   transition_filter(keep = F,wrap=T,
#                     Year < 2001,
#                     Year < 2002,
#                     Year < 2003,
#                     Year < 2004,
#                     Year < 2005
#                     )
# p
# 

  