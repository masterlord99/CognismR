topic_description <- lapply(rownames(vectorizedSpace),function(z){
  a <- find_similar_words(z,embedding_matrix = vectorizedSpace,embedding_matrix2 = tokensEmbedding)$matches
  paste0(names(a),"=",round(a,3))
} ) %>% stringi::stri_list2matrix(.,byrow = F)

colnames(topic_description) = rownames(vectorizedSpace)


icona_narobe <- fluidRow(
  column(12,align="center",
         icon("fas fa-exclamation-triangle", lib = "font-awesome")
  ))

icona_ok <- fluidRow(
  column(12,align="center",
         icon("fas fa-check-circle", lib = "font-awesome")
  ))

ui <- shinyUI(
  navbarPage(title="Job Titles",id="jt",
             tabPanel("Companies",value="dict",
                      fluidPage(
                        tags$style(type="text/css", "
                                   #loadmessage {
                                   position: fixed;
                                   top: 30%;
                                   left: 0px;
                                   width: 100%;
                                   padding: 5px 0px 5px 0px;
                                   text-align: center;
                                   font-weight: bold;
                                   font-size: 400%;
                                   color: '';
                                   background-color: ;
                                   z-index: 105;
                                   }
                                   "),
                        tags$head(
                          tags$style(HTML("hr {border-top: 1px solid #7f7f87;}"))
                        ),
                        sidebarPanel(width = 3,align="center",
                                     
                                     conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                      tags$div(withSpinner(h1(""),7,"black"),id="loadmessage")),
                                     h1("Company position and evolutions through time."),
                                     br(),
                                     hr(),
                                     helpText("Projection is focused on whole space and company position in it. Animation provides information about company evolution thourgh time."),
                                     br(),
                                     helpText("Please keep in mind that we plot 128 dimensions to 2 dimensional space with stohastic TSNE algorithm."),
                                     hr(),
                                     selectInput("input_companies","Select companies:",choices = izbire,
                                                 selected = izbire,multiple = T),
                                     selectInput("input_companies_anime","Select companies:",choices = izbire,
                                                 selected = "Wells Fargo & Company",multiple = F),
                                     helpText("Select one or multiple companies."),
                                     selectInput("AI_display","Display AI related jobs only?",choices = c(T,F),selected = F,multiple = F),
                                     selectInput("input_anime","Select display:",choices = c("Static"=1,"Animation"=0),
                                                 selected = "Static",multiple = F),
                                     helpText(id="anime_text","Since proportion of leading job titles is huge compared to others, we are plotting biggest changes in company structure trough time (we made time series stationary).",style="color:maroon"),
                                     actionButton("compare_companies","Plot",icon("paint-brush")),
                                     br(),
                                     hr()
                                     
                                     
                        ),
                        mainPanel(align="right",
                                  column(12,align="center",
                                         h3(textOutput("anime_header"),style="color:maroon"),
                                         h4(id="sub__","Biggest changes in cosine similarities to cluster's embeddings"),
                                         imageOutput("companies_plot_ANIME",height = 550,width = 1000),
                                         plotOutput("companies_plot_OUTPUT",height = 900),
                                         
                                         selectInput("companies_select",label = "Select topics for detailed display (Default AI related clusters):",choices = colnames(topic_description),
                                                     selected = AI_names,multiple = T,width = 750),  # c("Topic26","Topic34","Topic40","Topic50","Topic7")
                                         DT::dataTableOutput("companies_OUTPUT"),
                                         br(),
                                         br()
                                  )
                                  
                        )
                      )
             ),
             tabPanel("Classifier",value="group",
                      fluidPage(
                        theme= shinytheme("flatly"),
                        tags$style(type="text/css", "
                                              #loadmessage {
                                              position: fixed;
                                              top: 30%;
                                              left: 0px;
                                              width: 100%;
                                              padding: 5px 0px 5px 0px;
                                              text-align: center;
                                              font-weight: bold;
                                              font-size: 400%;
                                              color: '';
                                              background-color: ;
                                              z-index: 105;
                                              }
                                              "),
                        tags$head(
                          tags$style(HTML("hr {border-top: 1px solid #7f7f87;}"))
                        ),
                        # App title ----
                        
                        
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                          # Sidebar panel for inputs ---
                          sidebarPanel(width = 3,align="center",
                                       
                                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                        tags$div(withSpinner(h1(""),7,"black"),id="loadmessage")),
                                       h1("Job Title Classifier"),
                                       helpText("Classifier is conditioned on tokenizer working correctly!"),
                                       actionLink("link","Click here for the proof of concept.",icon("question")),
                                       hr(),
                                       h3(actionLink("optionsToggle","Options",icon("cogs"))),
                                       helpText("Click on options to toggle and change parameters."),
                                       hr(),
                                       column(12,align="center",id="options",
                                              sliderInput(inputId = "spaceWeightID","Weight allocator:",0,1,1,0.01),
                                              helpText("Weight for combining vectorized spaces. You can observe the different representaion of topics on first plot
                                                       (enable plotting). If we put the weight bellow 0.85, the space get kinda disorted."),
                                              checkboxGroupInput("spaceID", label = "Select the nature of embedding (disabled):", 
                                                                 choices = list("Tokens embedding" = 1, "Topics embedding" = 2),
                                                                 selected = 1),
                                              #helpText("Feature is irrelevant. You can observe which topics are close in 2D projection (enable plots)."),
                                              h3("Plot Options:"),
                                              helpText(" Barnes-Hut t-Distributed Stochastic Neighbor Embedding.
                                                       t-SNE is a method for constructing a low dimensional embedding
                                                       of high-dimensional data, distances or similarities."),
                                              selectInput("pcaID","Shall we use PCA?",choices = c(T,F),T),
                                              numericInput("nPlot","Numbers of tokens to plot:",200),
                                              helpText("Relevant only for second plot."),
                                              selectInput("plotID","Enable plotting?",choices = c(T,F),F),
                                              helpText("Plotting requires a lot of calculations. You can speed up the classification by disabling it."),
                                              hr()
                                       ),
                                       actionButton("randomToken", "Random Select",icon("paper-plane")
                                                    # style="color: #f0edf5; background-color: #000; border-color: #f0edf5"
                                       ),
                                       helpText("Randomly select one job title from the sample."),
                                       hr(),
                                       textInput("inputToken","Input one arbitrary Job Title:",placeholder = "doctor"),
                                       hr(),
                                       actionButton("class","Classify",icon("wrench")),
                                       br(),
                                       hr()
                                       
                                       
                                       
                                       # actionButton("reset_button", "Zamenjaj podatke")
                                       
                                       
                          ),
                          # Main panel for displaying outputs ----
                          mainPanel(align="center",
                                    h3("Tokenizer:"),
                                    DT::dataTableOutput("tokenizerOUTPUT"),
                                    br(),
                                    h3("Cosine similarity matches:"),
                                    DT::dataTableOutput("cosineOUTPUT"),
                                    br(),
                                    h3("Tokens closest to topics vectors:"),
                                    DT::dataTableOutput("groupsOUTPUT"),
                                    br(),
                                    column(12,align="center",id="ww",
                                           h3("2D topics projection:"),
                                           helpText("Our query is somewhere inside of circle. Please keep in mind that this is only the projection."),
                                           plotOutput("topicRtsne"),
                                           br(),
                                           h3("2D tokens projection:"),
                                           helpText("Our query is in center of the circled area. Again, keep in mind that we shrank 128 dimensions into 2."),
                                           plotOutput("rtsne")
                                    ),
                                    br()
                                    
                                    
                          )
                        )
                      )
             ),
             tabPanel("Tokenizer",value="tokenize",
                      fluidPage(
                        useShinyjs(),
                        theme= shinytheme("flatly"),
                        tags$style(type="text/css", "
                                   #loadmessage {
                                   position: fixed;
                                   top: 30%;
                                   left: 0px;
                                   width: 100%;
                                   padding: 5px 0px 5px 0px;
                                   text-align: center;
                                   font-weight: bold;
                                   font-size: 400%;
                                   color: '';
                                   background-color: ;
                                   z-index: 105;
                                   }
                                   "),
                        tags$head(
                          tags$style(HTML("hr {border-top: 1px solid #7f7f87;}"))
                        ),
                        # App title ----
                        
                        
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                          
                          # Sidebar panel for inputs ---
                          sidebarPanel(align="center",id="side",width = 4,
                                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                        tags$div(withSpinner(h1(""),7,"black"),id="loadmessage")),
                                       h1("Tokenizer"),
                                       br(),
                                       hr(),
                                       br(),
                                       actionButton("rand", "Random Select",icon("paper-plane")
                                                    # style="color: #f0edf5; background-color: #000; border-color: #f0edf5"
                                       ),
                                       helpText("Randomly select 20 job titles from the sample."),
                                       helpText("Function will use thresholds that were set below."),
                                       br(),
                                       hr(),
                                       br(),
                                       textInput("text","Input arbitrary job title:",placeholder = "director, manager"),
                                       helpText("Separate additional titles with comma"),
                                       h3("Parameters:"),
                                       br(),
                                       sliderInput("th1","Distance threshold 1:",min = 0.01,max = 0.15,step = 0.005,value = 0.075),
                                       helpText("Lower value means stricter matching condition"),
                                       sliderInput("th2","Distance threshold 2:",min = 0.01,max = 0.15,step = 0.005,value = 0.075),
                                       helpText("Lower value means stricter matching condition"),
                                       br(),
                                       actionButton("Tokenize","Tokenize",icon("wrench")
                                                    # style="color: #f0edf5; background-color: #000; border-color: #f0edf5"
                                       ),
                                       hr(),
                                       br()
                                       
                                       
                                       # actionButton("reset_button", "Zamenjaj podatke")
                          ),
                          # Main panel for displaying outputs ----
                          mainPanel(align="center",
                                    
                                    DT::dataTableOutput("tabela"),
                                    br(),
                                    h4("Please let me know any feedback or possible errors. Everyday
                                       I correct something from the dictionary and it is far from perfect.")
                                    
                          )
                        )
                      )),
             
             navbarMenu("Data & Documentaion",
                        tabPanel("Dictionary",value="dict",
                                 fluidPage(
                                   tags$style(type="text/css", "
                                   #loadmessage {
                                   position: fixed;
                                   top: 30%;
                                   left: 0px;
                                   width: 100%;
                                   padding: 5px 0px 5px 0px;
                                   text-align: center;
                                   font-weight: bold;
                                   font-size: 400%;
                                   color: '';
                                   background-color: ;
                                   z-index: 105;
                                   }
                                   "),
                                   tags$head(
                                     tags$style(HTML("hr {border-top: 1px solid #7f7f87;}"))
                                   ),
                                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                    tags$div(withSpinner(h1(""),7,"black"),id="loadmessage")),
                                   column(12,align="center",
                                          br(),
                                          DT::dataTableOutput("prior"))
                                 )
                        ),
                        tabPanel("Documented Workflow",value="wf",
                                 fluidPage(
                                   includeCSS("report/report.html")
                                 )
                        )
                        
             )
  )
)


server <- shinyServer(function(input, output,session){
  ## toggle
  hide("options")
  hide("spaceID")
  disable("spaceID")
  disable("spaceWeightID")
  #disable("input_anime")
  
  observeEvent(input$plotID,{
    if(input$plotID){
      show("ww")
    }else{
      hide("ww")
    }
  })
  
  observeEvent(input$optionsToggle,{
    toggle("options",anim=T)
  })
  
  
  
  ### LDA
  LDAtabel <- reactiveValues(
    out= data.table(from="Med. doctor",token1="doctor")
  )
  
  COSINEtabel <- reactiveValues(
    out= cosTabel
    
  )
  
  GROUPStabel <- reactiveValues(
    out= groupTabel
  )
  
  embedd <- reactiveValues(
    space= vectorizedSpace
  )
  
  observeEvent(input$spaceWeightID,{
    embedd$space <- vectorizedSpace
  })
  
  output$tokenizerOUTPUT <- DT::renderDataTable({
    LDAtabel$out
  },
  rownames = F,
  options = list(
    dom="t",
    lengthMenu = c(20,25),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  ))
  
  output$cosineOUTPUT <- DT::renderDataTable({
    COSINEtabel$out
  },
  rownames = F,
  options = list(
    dom="t",
    lengthMenu = c(20,25),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  ))
  
  output$groupsOUTPUT <- DT::renderDataTable({
    GROUPStabel$out
  },
  rownames = F,
  options = list(
    dom="t",
    lengthMenu = c(20,25),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  ))
  
  
  observeEvent(input$randomToken,{
    base <- sample(titles[,1],size = 1,prob = titles[,2])
    updateTextInput(session, "inputToken",value=base )
    click("class")
  })
  
  observeEvent(input$class,{
    if(input$inputToken != ""){
      base <- input$inputToken
      base <- tolower(base)
      base <- trimws(base)
      base <- assignRoleTokens(base,prior,cores=1,thresh = 0.1,thresh2 = 0.1,maxCol = 11)
      if(!is.na(base$token1)){
        base[,2:ncol(base)] <- apply(base[,2:ncol(base),drop=F],2,function(x){
          if(x %in% rownames(tokensEmbedding)){
            x
          }else{
            NA
          }
        })
        base <- base[,apply(base,2,function(x)!is.na(x))]
        if(class(base) != "data.frame"){
          base <- data.frame(from=base,token1=NA)
        }
      }
      LDAtabel$out <- base
      if(!is.na(base$token1)){
        COSINEtabel$out <- createTopics(tokens = c(base[,2:ncol(base)] %>% unlist()),space1 = tokensEmbedding,space2 = embedd$space )
        GROUPStabel$out <- createGroups(COSINEtabel$out,space1=embedd$space,space2=tokensEmbedding)
      }else{
        COSINEtabel$out <- data.table(Topic1=NA,Topic2=NA,Topic3=NA,Topic4=NA,Topic5=NA)
        GROUPStabel$out <- data.table(Topic1=NA,Topic2=NA,Topic3=NA,Topic4=NA,Topic5=NA)
        showModal(
          modalDialog(
            title=h2(icona_narobe),
            fluidRow(column(12,align="center",
                            br(),
                            h4("I am sorry, I was not able to categorize that specific job title."),
                            br())),
            easyClose = T,
            size = "m",
            footer=column(12,align="center",
                          modalButton("Close",icon=icon("fas fa-check-circle")))
            
          ) 
        )
      }
      
    }else{
      showModal(
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("Please provide at least one job title."),
                          br())),
          easyClose = T,
          size = "m",
          footer=column(12,align="center",
                        modalButton("Close",icon=icon("fas fa-check-circle")))
          
        ) 
      )
    }
  })
  
  
  ## rtsne plot
  
  output$rtsne <- renderPlot({
    if(input$plotID){
      if(!is.na(LDAtabel$out$token1[1])){
        tmpPerp <- 40
        pcaPlot(
          tokens= c(LDAtabel$out[,2:ncol(LDAtabel$out)]) %>% unlist(),
          tok_space = tokensEmbedding,
          embedding_matrix = tokensEmbedding,
          numb=input$nPlot,
          perp = tmpPerp,
          pca= input$pcaID,
          rand=T
          
        )
      }
    }
    
  },height = 750)
  
  
  output$topicRtsne <- renderPlot({
    if(input$plotID){
      if(!is.na(LDAtabel$out$token1[1])){
        tmpPerp <- 10
        pcaPlot(
          tokens= c(LDAtabel$out[,2:ncol(LDAtabel$out)]) %>% unlist(),
          tok_space=tokensEmbedding,
          embedding_matrix = embedd$space,
          numb=input$nPlot,
          perp = tmpPerp,
          pca= input$pcaID,
          rand=F,
          topic=T
          
        )
      }
    }
    
  },height = 450)
  
  
  ## proof of concept
  
  compareTable <- reactiveValues(
    out = compareInt
  )
  
  
  modal <- modalDialog(
    title=column(12,align="center",h3("Proof of concept:",style="color:maroon")),
    fluidRow(column(12,align="center",
                    helpText("You could say that a basketball teacher and a high school coach are
                              similiar job titles by its meaning. Also teacher and coach could mean the same thing with some sport
                              adjective attached. So cosine similarity should be high (0.76 is reasonably high). But if you swap 
                              the two, high school teacher and basketball coach are completely different jobs now. So cosine similarity should 
                             be near 0. If that is the case, we can take that as proof of concept."),
                    textInput("com1","First job title:","basketball teacher"),
                    textInput("com2","Second job title:","high school coach"),
                    actionButton("com","Calculate similarity",icon("wrench")),
                    helpText("Similarity of two job titles with similiar meaning should be near 1."),
                    h3("I was not able to tokenize titles!",style="color:red",id="error"),
                    column(12,align="center",id="comparission",
                           br(),
                           DT::dataTableOutput("similarities"),
                           helpText("Comparission of the first one to the base tokens:"),
                           DT::dataTableOutput("compareTab1"),
                           helpText("Comparission of the second one to the base tokens:"),
                           DT::dataTableOutput("compareTab2")
                    ),
                    br())),
    easyClose = T,
    size = "m",
    footer=column(12,align="center",
                  modalButton("Close",icon=icon("fas fa-check-circle")))
  )
  
  observeEvent(input$link,{
    showModal(modal)
    hide("error")
    #hide("comparission")
  })
  observeEvent(input$com,{
    out <- NULL
    try({
      out <- compareTitles(input$com1,input$com2,tokensEmbedding,prior)
    },silent = T)
    if(!is.null(out)){
      compareTable$out <- compareTitles(input$com1,input$com2,tokensEmbedding,prior)
      hide("error")
      show("comparission",anim=F)
    }else{
      show("error")
      hide("comparission")
    }
  })
  
  
  output$similarities <- DT::renderDataTable({
    x23 <- data.frame(Similarity = compareTable$out[[1]][[1]],milarity=compareTable$out[[3]])
    colnames(x23)[2] <- (compareTable$out[[2]][[1]])
    x23
  },
  rownames=F,
  options = list(
    dom="t",
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  )
  )
  
  output$compareTab1 <- DT::renderDataTable({
    x23 <- data.frame(1,t(compareTable$out[[1]][[2]]))
    colnames(x23) <- c(compareTable$out[[1]][[1]],names(compareTable$out[[1]][[2]]))
    rownames(x23) <- "Similarities"
    x23[, !duplicated(colnames(x23))]
  },
  rownames=F,
  options = list(
    dom="t",
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  )
  )
  output$compareTab2 <- DT::renderDataTable({
    x23 <- data.frame(1,t(compareTable$out[[2]][[2]]))
    colnames(x23) <- c(compareTable$out[[2]][[1]],names(compareTable$out[[2]][[2]]))
    rownames(x23) <- "Similarities"
    x23[, !duplicated(colnames(x23))]
  },
  rownames=F,
  options = list(
    dom="t",
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  )
  )
  ## tokenizer
  
  tabela <- reactiveValues(
    out= data.frame(from="med. doctor",token1="doctor",token2=NA,token3=NA)
  )
  
  output$tabela <- DT::renderDataTable({
    tabela$out
  },
  rownames = F,
  options = list(
    lengthMenu = c(20,25),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  ))
  
  observeEvent(input$rand,{
    base <- sample(titles[,1],size = 20,prob = titles[,2])
    tabela$out <- assignRoleTokens(base,prior,cores=1,thresh = input$th1,thresh2 = input$th2,maxCol = 11)
  })
  
  observeEvent(input$Tokenize,{
    if(input$text != ""){
      base <- stringr::str_split(input$text,",") %>% unlist()
      base <- tolower(base)
      base <- trimws(base)
      tabela$out <- assignRoleTokens(base,prior,cores=1,thresh = input$th1,thresh2 = input$th2,maxCol = 11)
      
    }else{
      showModal(
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("Please provide at least one job title."),
                          br())),
          easyClose = T,
          size = "m",
          footer=column(12,align="center",
                        modalButton("Close",icon=icon("fas fa-check-circle")))
          
        ) 
      )
    }
  })
  
  ## Data
  
  output$prior <- DT::renderDataTable({
    prior[order(prior$from,decreasing = T),c(-2,-4)]
  },
  filter = 'top',
  rownames = F,
  options = list(
    lengthMenu = c(10,25,100,250),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  ))
  
  
  
  ### evolution
  
  r_baza <- reactiveValues(baza=baza)
  
  r_root <- reactiveValues(root="anim")
  
  r_vec <- reactiveValues(vec=vec)
  
  observeEvent(
    input$AI_display,
    {
      if(input$AI_display){
        r_baza$baza <- baza_AI
        r_root$root <- "anim_AI/"
        r_vec$vec <- vec_AI
      }else{
        r_baza$baza <- baza
        r_root$root <- "anim/"
        r_vec$vec <- vec
      }
      
      companies_out$out <- input$input_companies
      companies_anime$list$path <- paste0(r_root$root,input$input_companies_anime,".gif")
      iiii_ <- input$input_companies_anime == izbire
      companies_anime$list$topics <- r_vec$vec[iiii_][[1]]$ime
      updateSelectInput(session,"companies_select",selected = companies_anime$list$topics)
      #print(companies_anime$list$path)
      ime$ime <- input$input_companies_anime
    }
  )
  
  
  # hide("anime_text")
  # hide("anime_header")
  observeEvent(input$input_anime,{
    if(input$input_anime==1){
      show("companies_plot_OUTPUT") 
      show("input_companies")
      hide("input_companies_anime")
      hide("companies_plot_ANIME")
      hide("anime_text")
      hide("sub__")
      hide("anime_header")
      updateSelectInput(session,"companies_select",selected = AI_names)
    }else{
      hide("companies_plot_OUTPUT")
      show("companies_plot_ANIME")
      hide("input_companies")
      show("input_companies_anime")
      show("anime_text")
      show("sub__")
      show("anime_header")
      updateSelectInput(session,"companies_select",selected = companies_anime$list$topics)
    }
    companies_out$out <- input$input_companies
    companies_anime$list$path <- paste0(r_root$root,input$input_companies_anime,".gif")
    iiii_ <- input$input_companies_anime == izbire
    companies_anime$list$topics <- r_vec$vec[iiii_][[1]]$ime
    updateSelectInput(session,"companies_select",selected = companies_anime$list$topics)
    #print(companies_anime$list$path)
    ime$ime <- input$input_companies_anime
  })
  
  observeEvent(input$compare_companies,{
    if(length(input$input_companies)==0){
      showModal(
        modalDialog(
          title=h2(icona_narobe),
          fluidRow(column(12,align="center",
                          br(),
                          h4("Please select at least one company."),
                          br())),
          easyClose = T,
          size = "m",
          footer=column(12,align="center",
                        modalButton("Close",icon=icon("fas fa-check-circle")))
          
        ) 
      )
    }else{
      
      companies_out$out <- input$input_companies
      companies_anime$list$path <- paste0(r_root$root,input$input_companies_anime,".gif")
      iiii_ <- input$input_companies_anime == izbire
      companies_anime$list$topics <- r_vec$vec[iiii_][[1]]$ime
      updateSelectInput(session,"companies_select",selected = companies_anime$list$topics)
      #print(companies_anime$list$path)
      ime$ime <- input$input_companies_anime
    }
    
    
  })
  
  
  observeEvent(input$input_companies_anime,{
    click("compare_companies")
  })
  
  companies_anime <- reactiveValues(
    list = list(
      path="anim/Wells Fargo & Company.gif",
      topics = vec$`Wells Fargo & Company`$ime
    )
  )
  
  
  
  companies_out <- reactiveValues(
    out=izbire
  )
  
  output$companies_OUTPUT <- DT::renderDataTable({
    topic_description[,colnames(topic_description) %in% input$companies_select,drop=F]
  },
  filter = "none",
  rownames = F,
  options = list(
    lengthMenu = c(10,25,100,250),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2D3E50', 'color': '#f0edf5'});",
      "}")
  ))
  
  
  output$companies_plot_OUTPUT <- renderPlot({
    rtsne_plot_(x = r_baza$baza,prepared = T,query = companies_out$out,space_ = vectorizedSpace)
    
    
  })
  
  ime <- reactiveValues(ime="Wells Fargo & Company")
  output$anime_header <- renderText({
    toupper(ime$ime)
  })
  
  output$companies_plot_ANIME <- renderImage({
    #print(companies_anime$list$path)
    list(
      src= companies_anime$list$path,
      contentType = "image/gif",
      width=1000
    )
    
    
  },deleteFile = F)
  
})

shinyApp(ui, server)