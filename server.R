server<-shinyServer(function(input, output){
  output$Des1<-renderText({
    "ERMA (Everyone's Rasch Measurement Analyzer) for Dichotomous item responses"
  })
  
  output$Des2<-renderText({
    "ERMA was created using R. As an R program, ERMA provides free and open source code. 
    ERMA uses a pairwise algorithm for calibrating item parameters, and maximum likelihood 
    estimation for person measures. The results of ERMA have been validated with other Rasch 
    software. The simple structure of ERMA makes it a useful teaching tool for users to gain 
    an understanding of Rasch measurement theory, and also provides the opportunity to modify 
    the program based on their needs. In addition to the traditional R program, ERMA also is 
    available as a ShinyApp. ShinyApp is supported by RStudio, and it is a web application 
    framework for R programs. ShinyERMA provides a user-friendly interface. ShinyERMA uses 
    click-and-go buttons, and provides major outputs found in typical Rasch analyses. The 
    development of ERMA program is still in progress. We plan to extend its use for wider 
    applications in the future, such as rater-mediated assessments."
  })
  
  output$Des3<-renderText({
    "For the input dataset, headings (first row) and person ID (first column) are required.
    Please make sure the data structure matches the example data."
  })
  
  output$Exdata<-renderDataTable({
    read.csv("https://raw.githubusercontent.com/jwang1223/ShinyERMA/master/HOME.csv",header=TRUE)
  })
  
  output$Dat<-renderDataTable({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    read.table(inFile$datapath,header=TRUE,sep=input$sep)
  })
  
  output$ItemTable<-renderTable({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    mat <- mat[,-1]
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    
    if (is.null(index)==FALSE) {mat<-mat[-index,]}
    
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    
    B <- matrix(0,NGitem, NGitem)
    
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    
    D <- t(B2)/B2
    Logit <- log(D)
    
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    Items.summary=data.frame("Raw Score"=c(mean(colSums(mat)),sd(colSums(mat)),max(colSums(mat)),min(colSums(mat))),Measure=c(mean(G_items),sd(G_items),max(G_items),min(G_items)))
    
    # Theta Estimates
    A<-matrix(0,dim(mat)[1],1)
    
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    
    #Precision error (Information)
    p=matrix(0,Nperson,NGitem)
    for (i in 1:Nperson){
      for (j in 1:NGitem){
        p[i,j]=exp(A[i,1]-G_items[j])/(1+exp(A[i,1]-G_items[j]))
      }
    }
    Q=p*(1-p)
    Items.infor=colSums(Q) #Item information
    I.se<-1/sqrt(Items.infor)# Items S.E.
    
    for (i in 1: dim(mat)[2]){
      if (sum(mat[,i])==dim(mat)[1]){
        I.se[i]<-NA
      }
    }
    
    # Reliability of item separation
    # 
    SEi=mean(I.se)**2
    SDi=var(G_items)
    Rel_i=(SDi-SEi)/SDi
    
    Items.summary<-rbind(Items.summary,c(NA,NGitem),c(NA,Rel_i))
    row.names(Items.summary)<-c("MEAN","S.D.","MAX","MIN","COUNT","Reliability")
    Items.summary
  })
  
  output$PersonTable<-renderTable({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    mat <- mat[,-1]
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    
    if (is.null(index)==FALSE) {mat<-mat[-index,]}
    
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    
    B <- matrix(0,NGitem, NGitem)
    
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    
    D <- t(B2)/B2
    Logit <- log(D)
    
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    # Theta Estimates
    
    A<-matrix(0,dim(mat)[1],1)
    
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    
    # Standard errors for person estimates
    
    SE=matrix(0,Nperson,1)
    
    for (i in 1:Nperson){
      sum=0
      for (j in 1: NGitem){
        p=(exp(A[i,1]-G_items[j]))/(1+exp(A[i,1]-G_items[j]))
        sum=sum+p*(1-p)
      }
      SE[i,1]=sqrt(1/sum)
    }
    SE_Persons <- SE
    
    # Reliability of person separation
    # 
    SEp=mean(SE)**2
    SDp=var(A)
    Reliability=(SDp-SEp)/SDp
    
    Persons.summary=data.frame("Raw Score"=c(mean(rowSums(mat)),sd(rowSums(mat)),max(rowSums(mat)),min(rowSums(mat))),Measure=c(mean(A),sd(A),max(A),min(A)))
    Persons.summary<-rbind(Persons.summary,c(NA, Nperson),c(NA,Reliability))
    row.names(Persons.summary)<-c("MEAN","S.D.","MAX","MIN","COUNT","Reliability")
    Persons.summary
  })
  
  output$Item<-renderTable({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    mat <- mat[,-1]
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    if (is.null(index)==FALSE) {mat<-mat[-index,]}
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    percent<-NULL
    for (i in 1:NGitem) {percent<-c(percent, sum(x[,i])/Nperson)}
    B <- matrix(0,NGitem, NGitem)
    
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    
    D <- t(B2)/B2
    Logit <- log(D)
    
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    # Theta Estimates
    
    A<-matrix(0,dim(mat)[1],1)
    
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    
    # Standard errors for person estimates
    
    SE=matrix(0,Nperson,1)
    
    for (i in 1:Nperson){
      sum=0
      for (j in 1: NGitem){
        p=(exp(A[i,1]-G_items[j]))/(1+exp(A[i,1]-G_items[j]))
        sum=sum+p*(1-p)
      }
      SE[i,1]=sqrt(1/sum)
    }
    SE_Persons <- SE
    # Reproduce expected person responses based on "A" and "Items"
    
    P.exp=matrix(0,Nperson,NGitem)
    
    for (i in 1:Nperson){
      for (j in 1:NGitem){
        P.exp[i,j]=exp(A[i,1]-G_items[j])/(1+exp(A[i,1]-G_items[j]))
        
      }
    }
    
    #Precision error (Information)
    
    p=matrix(0,Nperson,NGitem)
    for (i in 1:Nperson){
      for (j in 1:NGitem){
        p[i,j]=exp(A[i,1]-G_items[j])/(1+exp(A[i,1]-G_items[j]))
      }
    }
    Q=p*(1-p)
    Items.infor=colSums(Q) #Item information
    I.se<-1/sqrt(Items.infor)# Items S.E.
    
    for (i in 1: dim(mat)[2]){
      if (sum(mat[,i])==dim(mat)[1]){
        I.se[i]<-NA
      }
    }
    
    for (i in 1: dim(mat)[2]){
      if (sum(mat[,i])==dim(mat)[1]){
        Items.infit[i]<-NA
      }
    }
    
    
    # Score residual
    Y=mat-P.exp
    Items.dis=colSums(Y) #Item score discrepancy
    
    #Infit square residual
    Infit=Y**2
    Items.infit=colSums(Infit)/colSums(Q) # Item infit
    
    #Outfit square residual
    Z.2=Infit/Q
    Items.outfit=colMeans(Z.2)  # Item outfit
    Z=sqrt(Z.2) # Response misfit
    ItemTable <- data.frame(percent,G_items,I.se,Items.infit,Items.outfit)
    names(ItemTable)<-c("Percent correct","Estimates","Std.err","Infit","Outfit")
    ItemTable
  })
  
  output$Person<-renderTable({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    row.names(mat)<-mat[,1]
    mat <- mat[,-1] 
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    if (is.null(index)==FALSE) {
      mat<-mat[-index,]}
    
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    percent<-NULL
    for (i in 1:Nperson) {percent<-c(percent, sum(x[i,])/NGitem)}
    B <- matrix(0,NGitem, NGitem)
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    
    D <- t(B2)/B2
    Logit <- log(D)
    
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    # Theta Estimates
    
    A<-matrix(0,dim(mat)[1],1)
    
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    
    # Standard errors for person estimates
    
    SE=matrix(0,Nperson,1)
    
    for (i in 1:Nperson){
      sum=0
      for (j in 1: NGitem){
        p=(exp(A[i,1]-G_items[j]))/(1+exp(A[i,1]-G_items[j]))
        sum=sum+p*(1-p)
      }
      SE[i,1]=sqrt(1/sum)
    }
    SE_Persons <- SE
    # Reproduce expected person responses based on "A" and "Items"
    #
    
    P.exp=matrix(0,Nperson,NGitem)
    
    for (i in 1:Nperson){
      for (j in 1:NGitem){
        P.exp[i,j]=exp(A[i,1]-G_items[j])/(1+exp(A[i,1]-G_items[j]))
        
      }
    }
    
    #Precision error (Information)
    
    p=matrix(0,Nperson,NGitem)
    for (i in 1:Nperson){
      for (j in 1:NGitem){
        p[i,j]=exp(A[i,1]-G_items[j])/(1+exp(A[i,1]-G_items[j]))
      }
    }
    Q=p*(1-p)
    
    Person.infor=rowSums(Q) # Person information
    P.se<-1/sqrt(Person.infor)# Person S.E.
    
    # Score residual
    Y=mat-P.exp
    Person.dis=rowSums(Y) #Person score discrepancy
    
    #Infit square residual
    Infit=Y**2
    Person.infit=rowSums(Infit)/rowSums(Q) # Person infit
    
    #Outfit square residual
    Z.2=Infit/Q
    Person.outfit=rowMeans(Z.2) # Person outfit
    Z=sqrt(Z.2) # Response misfit
    PersonTable <- data.frame(percent,Persons, P.se,Person.infit,Person.outfit)
    names(PersonTable)<-c("percent correct","Estimates","Std.err","Infit","Outfit")
    PersonTable
  })
  
  
  output$HistI<-renderPlot({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    mat <- mat[,-1]  
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    if (is.null(index)==FALSE) {mat<-mat[-index,]}
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    B <- matrix(0,NGitem, NGitem)
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    D <- t(B2)/B2
    Logit <- log(D)
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    # Theta Estimates
    A<-matrix(0,dim(mat)[1],1)
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    c1<-min(G_items,Persons)
    c2<-max(G_items,Persons)
    hist(G_items,breaks=15,xlim=c(c1-1,c2+1),main="Histogram of Items",xlab="Items")
  })
  
  output$HistP<-renderPlot({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    mat <- mat[,-1]  
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    if (is.null(index)==FALSE) {mat<-mat[-index,]}
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    B <- matrix(0,NGitem, NGitem)
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    D <- t(B2)/B2
    Logit <- log(D)
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    # Theta Estimates
    A<-matrix(0,dim(mat)[1],1)
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    c1<-min(G_items,Persons)
    c2<-max(G_items,Persons)
    hist(Persons,breaks=15,xlim=c(c1-1,c2+1))
  })
  
  output$Stem<-renderPlot({
    inFile<-input$Data
    if (is.null(inFile)) return(NULL)
    mat<-read.table(inFile$datapath,header=TRUE,sep=input$sep)
    # Drop item columns
    IMean <- colMeans(mat)
    mat <- mat[,IMean!=1 & IMean!=0]
    # Drop person ID column
    Nitem<-ncol(mat)
    mat <- mat[,-1]  
    #Delete perfect scores
    perfect<-NULL
    index<-NULL
    for (i in 1: dim(mat)[1]){
      if (sum(mat[i,])==dim(mat)[2]||sum(mat[i,])==0){
        perfect<-rbind(perfect,mat[i,])
        index<-c(index,i)
      }}
    if (is.null(index)==FALSE) {mat<-mat[-index,]}
    #Pairwise comparison
    x <- mat
    Nperson <- dim(x)[1]
    NGitem <- dim(x)[2]
    B <- matrix(0,NGitem, NGitem)
    for(k in 1:Nperson){
      for(i in 1:NGitem){
        for(j in 1:NGitem){
          if(is.na(x[k,i])==FALSE & is.na(x[k,j])==FALSE){
            if(x[k,i]>x[k,j]){B[i,j] <- B[i,j]+1}
          }}}}
    
    B2 <- B %*% B 
    B2[B2==0]<-1/(2*length(mat))
    D <- t(B2)/B2
    Logit <- log(D)
    G_items=rowMeans(Logit)
    Mean <-mean(G_items)
    SD <-sd(G_items)
    # Theta Estimates
    A<-matrix(0,dim(mat)[1],1)
    for (i in 1:dim(mat)[1]){
      NEWA=0 
      OLDA=50
      Count=0
      while (abs(OLDA-NEWA)>0.001 & Count<100){
        Count=Count+1
        NEWA=A[i,1]
        p=exp(A[i,1]-G_items)/(1+exp(A[i,1]-G_items))
        Adj=sum(mat[i,]-p)/sum(p*(1-p))
        NEWA=NEWA+Adj
        OLDA=A[i,1]
        A[i,1]=NEWA
      }
    }
    Persons <- A
    par(mfrow=c(1,2))
    plot.new()
    tmp1 <- capture.output(stem(Persons))
    text( 0,1, paste("Persons",paste(tmp1,collapse='\n')), adj=c(0,1), family='mono' )
    plot.new()
    tmp2 <- capture.output(stem(G_items))
    text( 0,1, paste("Items",paste(tmp2, collapse='\n')), adj=c(0,1), family='mono' )	
  }) 
  })