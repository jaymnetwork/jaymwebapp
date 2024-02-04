body(filled.contour)=expression({
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0))
    stop("increasing 'x' and 'y' values expected")
  
  mar.orig <- (par.orig <- par(c("mar","las","mfrow")))$mar
  on.exit(par(par.orig))
  
  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  par(las = las)
  
  ## Plot the 'plot key' (scale):
  mar <- mar.orig
  mar[4L] <- mar[2L]*1.5
  mar[2L] <- 1/2
  par(mar = mar)
  plot.new()
  key.title= title(main="\n \n \n  \n  Teams Value Increase",cex.main=0.5)
  plot.window(xlim = c(0,1), ylim = range(levels), xaxs = "i", yaxs = "i")
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
  if (missing(key.axes)) {
    if (axes)
      axis(4)
  }
  else key.axes
  box()
  if (!missing(key.title))
    
    ## Plot contour-image::
    mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  plot.title = {par(cex.lab=0.8);
    title(xlab="CRS Efficiency", ylab="Revenue")}
  .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  
  else plot.axes
  if (frame.plot) box()
  if (missing(plot.title))
    title(...)
  else
    plot.title
  invisible()
})

rdd_object<-function(data){
  rdd_data(y = data$percentage, 
                     x = data$pointCluster, 
           cutpoint = 0)
}

grouping <- function(data,quarter,number){
  data2 <- data[data[quarter]==number,]
  win <- 0
  lose <- 0
  for (i in data2$WorL2){
    if (i=='W'){
      win = win +1
    }
    else{
      lose = lose + 1 
    }
  }
  #return(data2)
  return(win/length(data2$total))
}

grouping2 <- function(data,quarter,number){
  data2 <- data[data[quarter]==number,]

  return(nrow(data2))
}

kbl_match<-function(data, quarter, n){
  pointCluster<-c()
  percentage<-c()
  games<-c()

  
  for(i in n:1) { 
    pointCluster<-append(pointCluster,-i)
    
    percentage<-append(percentage,grouping(data,quarter,-i))
    games<-append(games, grouping2(data, quarter,-i))
  }
  
  for(i in 1:n) {
    pointCluster<-append(pointCluster,i)
    
    percentage<-append(percentage,grouping(data,quarter,i))
    games<-append(games, grouping2(data,quarter,i))
  }
  
  discontinuity <- data.frame(pointCluster, percentage,games) 
  discontinuity <-discontinuity %>% mutate(win = pointCluster > 0) 
  #discontinuity$win[n+2] <- TRUE
  return(discontinuity)
}
RDDtext<-function(){
  HTML("
  <p> 
This study was conducted to refine and verify the argument by Berger & Pope (2011). Based on the expectancy 
theory, we expected that the existence or type of regression discontinuity could vary depending on the 
situation and time in the context of the Korean professional basketball league. We conducted a regression 
discontinuity design using the R packages on the data of 4,531 games played during the 17 regular seasons (2006-2007 to 2022-2023) 
of the Korean basketball league. We divided the sample into top-tier teams (1st and 2nd place) / mid to lower-tier teams (3rd to 10th place). 
Also, we segregated the sample into the early phase of the regular season (1st and 2nd round)/mid to final phase of the regular season (3rd to 6th round) 
to specifically delve into the research question. First, a discontinuity of the regression was 
found after the third quarter. Second, the regression discontinuity appeared differently depending on the performance level. 
More specifically, discontinuity effects did not appear among the top teams whereas the regression discontinuity effects were activated among mid to 
lower-tier team samples. Third, the regression discontinuity appeared differently depending on the time point of the regular season (round). 
A discontinuity of regression line appearing from the 3rd to 6th rounds in the case of the Korean professional basketball league. 
This research brings theoretical and practical contributions by systematically exploring that the 
probability of winning can vary even with the same score difference depending on the time point 
and situations perceived by the players.
 </p>
")
}

DEAtext3<-function(){
  HTML("
  <p> 
The main purpose of this research is to empirically analyze the determinants of organizational performance using National Basketball Association (NBA) team data. Based on the resource-based theory (RBT) of the firm, previous studies argue that operational efficiency refers to the capabilities of professional sports teams to transfer their resources into creating outputs of wins. NBA teams should strengthen organizational performance in the market when any teams possess valuable, rare, inimitable, and non-substitutable resources and capabilities. In this sense, operational efficiencies of NBA teams correspond to the concept of core competence providing a team to achieve competitive advantages through superior performance. Exploring the level of operating efficiency of NBA teams and its role on organizational performance is beyond essential. In this study, we conceptualize operating efficiency referring to the degree of professional sports team competence based on the comprehensive game-related statistics and financial-related performance rendered from the human assets and budgets invested in a team. To relate the theory into empirical investigation, we collected data comprising 6 seasons (2015-2016 to 2020-2021) of NBA teams. The results indicate that 29 out of 180 NBA teams have outstanding organizational efficiency that significantly contributes to franchise value.
 </p>
")
}


DEAtext<-function(){
  HTML("
  <p> 
In this research, we conduct a DEA (Data Envelopment Analysis) assessment 
to evaluate the efficiencies of Korean Basketball League (KBL) players to 
distinguish types of efficient players based on the concept of possession input. 
The objective of this paper is to provide an index that accurately measures the 
performance efficiencies of Korean professional basketball players in the Korea 
Basketball League by typically utilizing the DEA methodology. The proposed DEA 
approach requires DMUs (Decision Making Units) to assess the relative efficiencies 
of a homogeneous group of basketball players who perform to transform the same 
inputs into the same outputs. Their on-court performance and efficiencies should 
vary based on the level of ball possessions. Thus, the purpose of this research is 
to configure a precise evaluation to measure professional basketball player 
efficiencies and descriptively understand the fundamental segmentation of 
conceptualization of ‘extreme-level of ball possession efficiencies (ball hog)’
and ‘effective-level ball possession efficiencies (blue-collar worker)’.
 </p>
")
}

DEAtext2<-function(){
  HTML("
  <p> 
The study identifies different types of players through results from the DEA model using possession and productivity. Productivity per possession provides additional information from cumulative stats. Players who spent less possession, but significant productivity show higher production than other players in restricted minutes. Such players can be utilized in various plays in clutch moments. On the other hand, players who had both high possession and productivity can be translated that there is more than meets the eye as their team can stably depend on their play throughout the game. These implications can suggest specific criteria for evaluating whether a player is overpaid or underpaid. Furthermore, constructing a team in various ways with possession-oriented productivity can lead to a different magnitude of impact in winning. Such information can provide insights to teams at the professional level as well as stakeholders who wish to predict winners in the game of basketball.
 </p>
")
}

DEA_plot1<-function(data){
  ggplot(data, aes(x= possplayer, y= DEA_vrs,family = "NanumGothic",
                       label=ifelse(possplayer>21.1 &DEA_vrs==1,paste(as.character(PLAYER),SEASON),                                  ifelse(possplayer>5.861 & possplayer<9.1 & DEA_vrs==1,paste(as.character(PLAYER),SEASON),""))))+
    labs(x="Possesion",
         y="Efficiency (VRS)",
         title="The Efficiency of Player's Productivity in KBL History ") +
    geom_point(aes(colour=.data[[var]]))+
    scale_color_viridis(option = "plasma", name="Selected Variable") +
    geom_text_repel(point.padding = 0.6,
                    nudge_x = .5,
                    segment.size=0.2,
                    segment.color="grey50",
                    arrow = arrow(length = unit(0.015, "npc")),size=2.5,max.overlaps=Inf,
                    colour="black")+expand_limits(y = c(0, 1.19), x=c(0,20))+
    geom_mark_hull(data = hnc_kbl2[hnc_kbl2$possplayer>21.1 & hnc_kbl2$DEA_vrs == 1,], 
                   aes(x= possplayer, y= DEA_vrs, label="Extreme Productivity"),
                   label.fill=NA,
                   label.family = "NanumGothic",
                   label.fontsize = 7,
                   con.cap =1,
                   con.type="straight",
                   color="darkgreen",
                   fill = "darkgreen",
                   expand=0.000001,linetype = 2)+
    geom_mark_hull(data = hnc_kbl2[hnc_kbl2$possplayer>5.861 &
                                     hnc_kbl2$possplayer<9.1& hnc_kbl2$DEA_vrs == 1,], 
                   aes(x= possplayer, y= DEA_vrs, label="Cost Effective"),
                   label.fill=NA,
                   label.family = "NanumGothic",
                   label.fontsize = 7,
                   con.cap =1,
                   con.type="straight",
                   color="darkgreen",
                   fill ="darkgreen",
                   expand=0.00001,linetype = 2)+
    expand_limits(y = c(-0.2, 1.35), x=c(0,38))+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))+
    scale_x_continuous(breaks = c(0,3.86,10,20,30))+
    geom_vline(xintercept=3.86, linetype=3)+
    theme(plot.title=element_text(face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7))
}

DEA_plot2<-function(data){
  ggplot(hnc_kbl2,aes(x=possplayer,y=DEA_vrs, family = "NanumGothic",
                      label=ifelse(possplayer>21 &DEA_vrs==1,paste(as.character(PLAYER),SEASON),
                                   ifelse(possplayer>5.861 & possplayer<9.1 & DEA_vrs==1,
                                          paste(as.character(PLAYER),SEASON),"")))) +
    stat_density_2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE) +
    scale_fill_viridis_c(name="빈도") +
    labs(x="포제션",
         y="VRS 효율",
         title="2022 KBL 효율성 상대적 비교 등고선 ") +
    geom_point(data=hnc_kbl2[hnc_kbl2$DEA_vrs==1 & hnc_kbl2$possplayer>21|
                               hnc_kbl2$DEA_vrs==1 & hnc_kbl2$possplayer>5.861 & hnc_kbl2$possplayer<9.1,],
               colour = "red",alpha=0.6)+
    geom_text_repel(point.padding = 0.6,
                    nudge_x = .5,
                    segment.size=0.2,
                    segment.color="white",
                    arrow = arrow(length = unit(0.015, "npc")),size=2,max.overlaps=Inf,
                    colour="white")+
    expand_limits(y = c(-0.2, 1.3), x=c(0,35))+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))+
    theme_minimal(base_family = "NanumGothic")+
    theme(plot.title=element_text(face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7))
}


DEA_plot3<-function(data){
  dea.plot.frontier(x1, y1, RTS="vrs", fex=0.3, cex.axis = 0.5, font.main=2,GRID=T,
                    cex=ifelse(hnc_kbl$possplayer>5.861 & hnc_kbl$possplayer<9.1 & hnc_kbl$DEA_vrs==1,1.5,
                               ifelse(hnc_kbl$possplayer>21.1 & hnc_kbl$DEA_vrs==1, 1.5,0.5)),
                    xlab = "포제션", ylab = "생산성",
                    col=ifelse(hnc_kbl$possplayer>5.861 & hnc_kbl$possplayer<9.1 & hnc_kbl$DEA_vrs==1,"red",
                               ifelse(hnc_kbl$possplayer>21.1 & hnc_kbl$DEA_vrs==1, "orange","black")),
                    pch=ifelse(hnc_kbl$possplayer>5.861 & hnc_kbl$possplayer<9.1 & hnc_kbl$DEA_vrs==1,8,
                               ifelse(hnc_kbl$possplayer>21.1 & hnc_kbl$DEA_vrs==1, 8,21)),
                    bg=ifelse(hnc_kbl$possplayer>5.861 & hnc_kbl$possplayer<9.1 & hnc_kbl$DEA_vrs==1,"red",
                              ifelse(hnc_kbl$possplayer>21.1 & hnc_kbl$DEA_vrs==1, "blue","grey")))
  title(main = 'KBL 역대 선수 상대적 효율성', font =2,
        adj = 0.35, line = 0.5)
  legend(x=29.5,y=30, legend =c("극한의 생산성","가성비 생산성"), fill= c("orange","red"),
         col = c("orange","red"),
         cex=0.6)
}


shotloc<-function(data){
  court_points <- court_points %>% mutate_if(is.numeric,~.*10)
  
  ggplot(data, aes(x=locationX, y=locationY+45)) + 
    scale_fill_manual(values = c("darkred","darkred"))+
    geom_point(aes(fill=isShotMade),pch=ifelse(data$isShotMade==TRUE,1,4),
               size=2,color=ifelse(data$isShotMade==TRUE,"darkred","grey")) +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = "black")+
    scale_y_continuous(NULL, expand = c(0, 0)) + 
    scale_x_continuous(NULL, expand = c(0, 0)) + 
    labs(x="",
         y="")+
    theme(text = element_text(size = 10),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
}

shotloc_d<-function(data){

  court_points <- court_points %>% mutate_if(is.numeric,~.*10)
  
  ggplot(data, aes(x=locationX, y=locationY+45)) + 
    stat_density_2d(
      data = data,
      aes(x = locationX, y = locationY, fill = stat(density / max(density))),
      geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
    )+
    geom_path(data = court_points,
              aes(x = x, y = y-50, group = desc),
              color = "white")+
    scale_fill_viridis_c(
      "Shot Frequency",
      option = "inferno",
    )+
    scale_y_continuous(NULL, expand = c(0, 0)) + 
    scale_x_continuous(NULL, expand = c(0, 0)) + 
    labs(x="",
         y="")+
    theme(text = element_text(size = 10),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
}

rdd<-function(data){
  data1<-data[data$pointCluster<0,]
  fit<-lm(percentage ~ pointCluster+I(pointCluster^2), data=data1)
  data1_pred<-data.frame(pointCluster=c(min(data$pointCluster):0))
  data1<-rbind(data1,c(0, NA, NA, NA))
  data1$predict <- predict(fit, newdata=data1_pred)
  
  data2<-data[data$pointCluster>0,]
  fit2<-lm(percentage ~ pointCluster+I(pointCluster^2), data=data2)
  data2_pred<-data.frame(pointCluster=c(0:max(data$pointCluster)))
  data2<-rbind(c(0, NA, NA, NA),data2)
  data2$predict <- predict(fit2, newdata=data2_pred)
  
  ggplot(data, aes(x=pointCluster, y=percentage))+
    geom_vline(xintercept = 0, col="black",linetype=3)+
    geom_point(alpha = 0.9,aes(size = games, colour =games)) +
    guides(color= guide_legend())+
    scale_color_viridis(option='cividis',
                        discrete=F, direction=-1)+
    labs(x="Score difference based on the home team",
         y="Winning percentage")+
    theme(plot.title=element_text(face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7)) +
    ylim(0,1)+
    stat_smooth(method="lm",formula = y ~ x + I(x^2),data=data1, aes(y=predict),se=F,
                col="orange",lwd=0.6)+
    stat_smooth(method="lm",formula = y ~ x + I(x^2),data=data2, aes(y=predict),se=F,
                col="orange",lwd=0.6)
}

dtf<-function(data,month1,month2,input1,input2,input3,input4){
  
  if(month1 > 4 & month2<=4){
    newmonths<-month2+12
    data<-data[data$new_months<= newmonths & data$new_months >= month1,]
  }
  else if(month1<=4 & month2<=4){
    newmonths1<-month1+12
    newmonths2<-month2+12
    data<-data[data$new_months<= newmonths2 & data$new_months >= newmonths1,]
  }
  else if(month1<=4 & month2>4){
    newmonths1<-month1+12
    data<-data[data$new_months<= newmonths2 & data$new_months >= newmonths1,]
  }
  else{
    data<-data[data$new_months <= month2 & data$new_months >= month1,]
  }
  data<-data %>% filter(Winp >= input1 & Winp <= input2)
  data<-data[(data$Round %in% input3),]
  data<-data[(data$rank %in% input4),]
  return(data)
}

deaplotly<-function(data, z){
  
  plot_ly(x = data$x, y = data$y,colors = viridis_pal(option = "A")(5),
                lighting=list(ambient=1,
                              diffuse=1,
                              fresnel=4,        
                              specular=0.5,
                              roughness=0.5),
                lightposition=list(x=100,
                                   y=100,
                                   z=2000)) %>% 
    add_surface(z=z)  %>%
    layout(
      scene = list(xaxis=list(title="CRS efficiencies"), yaxis=list(title="Revenue"),
                   zaxis=list(title="Team value increase"),camera = list(eye = list(x = -2.5, y = -1.2, z = 0.6))),
      scale_colour_viridis(option = "plasma")
    ) 

}

deaplotgg<-function(data){
  par(family="NanumGothic", cex=0.8)
  data$Diff_revenue<-as.numeric(data$Diff_revenue)
  ggplot(data, aes(x= Revenue, y= WINP,family = "NanumGothic",
                   label=                     
                     ifelse(DEA_crs3==1,paste(as.character(TEAM),SEASON),"")))+
    labs(x="Total revenue",
         y="Winning percentage",
         title="NBA Operational Efficiency") +
    geom_point(alpha=0.8,aes(size=DEA_crs3, colour=DEA_crs3))+
    scale_colour_viridis(guide = "legend",option = "inferno",
                         limits=c(0.5, 1), breaks=seq(0.5, 1, by=0.1)) +
    guides(color=guide_legend(title="CRS efficiency"),
           size=guide_legend(title="CRS efficiency")
    )+
    geom_text_repel(point.padding = 1,max.time = 10, force = 70, force_pull = 1,
                    nudge_x = .5,
                    segment.size=0.2,
                    segment.color="grey50",
                    arrow = arrow(length = unit(0.015, "npc")),size=2.5,max.overlaps=Inf,
                    colour="black")+expand_limits(y = c(0, 1.19), x=c(0,20))+
    ylim(0,1)+
    xlim(75,450)+
    theme(plot.title=element_text(face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
}

deacaveat<-function(data){
  plot_ly(data,
          x = ~SEASON, 
          y = ~TEAM, 
          z = ~DEFLECTIONS+SCREEN_ASSISTS_PTS+LOOSE_BALLS_RECOVERED+CONTESTED_SHOTS, 
          type = 'scatter3d', 
          mode = 'lines',
          split = ~TEAM) %>%
    layout(
      scene = list(xaxis=list(title="Seasons"),yaxis=list(title="Teams",showticklabels=FALSE),
                   zaxis=list(title="Tracking stat"),camera = list(eye = list(x = -1.5, y = -1.7, z = 0.25)))
    )
}

threeDim<-function(data){
  
  x0 <- data[data$win==0,]$pointCluster
  y0 <- data[data$win==0,]$percentage
  z0 <- data[data$win==0,]$games
  x1 <- data[data$win==1,]$pointCluster
  y1 <- data[data$win==1,]$percentage
  z1 <- data[data$win==1,]$games
  
  rgl_init()
  rgl.bg(color = "white")
  
  plot3d(x0, y0, z0, col="#FF5733", box = FALSE,
         type ="s", radius = 0.8*sum(z1)/300,xlab="Points difference",ylab="Percentage",zlab="Games")
  
  rgl.spheres(x1, y1, z1, radius = 0.8*sum(z1)/300, color = "blue") 
  
  fit <- lm(y0 ~ x0+I(x0^2))
  fit2 <- lm(y1 ~ x1+ I(x1^2))
  
  # predict values on regular xz1020d
  variable_speed <- data.frame(x0 = c(-10:10))
  pred1<-predfunc(predict(fit, newdata = variable_speed))
  grid.lines1 = length(pred1)
  y.pred0 <- matrix(pred1, 
                    nrow = grid.lines1, ncol = grid.lines1)
  
  x.pred0 <- variable_speed[predfunc2(predict(fit, newdata = variable_speed)),]
  z.pred0 <- seq(min(z0), max(z0), length.out = grid.lines1)
  
  
  # Add regression surface
  rgl.surface(x.pred0, z.pred0, y.pred0, color = "#FF5733",
              alpha = 0.5, lit = FALSE)
  
  variable_speed2 <- data.frame(x1 = c(-10:10))
  pred2<-predfunc(predict(fit2, newdata = variable_speed2))
  grid.lines2 = length(pred2)
  y.pred1 <- matrix(pred2, 
                    nrow = grid.lines2, ncol = grid.lines2)
  
  x.pred1 <- variable_speed2[predfunc2(predict(fit2, newdata = variable_speed2)),]
  z.pred1 <- seq(min(z1), max(z1), length.out = grid.lines2)
  
  
  rgl.surface(x.pred1, z.pred1, y.pred1, color = "blue",
              alpha = 0.5, lit = FALSE)
  # Add grid lines
  rgl.surface(x.pred0, z.pred0, y.pred0, color = "black",
              alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
  rgl.surface(x.pred1, z.pred1, y.pred1, color = "black",
              alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
}

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) {
  
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl package needed for this function to work. Please install it.")
  
  if( new.device | rgl::rgl.cur() == 0 ) {
    rgl::rgl.open()
    rgl::par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl::rgl.bg(color = bg )
  }
  rgl::rgl.clear(type = c("shapes", "bboxdeco"))
  rgl::rgl.viewpoint(theta = 45, phi = 30, fov= 60)
}

predfunc2<-function(list){
  lis<-c()
  index<-1
  for (i in list){
    if (i<=1 & i>=0){
      lis<-append(lis,index)
    }
    index<-index+1
  }
  return(lis)
}

predfunc<-function(list){
  lis<-c()
  for (i in list){
    if (i<=1 & i>=0){
      lis<-append(lis,i)
    }
  }
  return(lis)
}
showLabels3d <- function(x, y, z, labels,
                         id.method = "identify", id.n=length(x), col=c("blue"), 
                         res=y - mean(y), range.x=range(x), range.z=range(z), 
                         offset = ((100/length(x))^(1/3)) * 0.02) {
  if (!requireNamespace("rgl")) stop("rgl package is missing")
  if (id.method == "none") return(NULL)
  if(id.n > 0L) {
    if (missing(labels))
      labels <- as.character(seq(along=x))
    getPoints <- function(w) {
      names(w) <- labels
      iid <- seq(length=id.n)
      ws <- w[order(-w)[iid]]
      match(names(ws), labels)
    }
    ind <-  switch(id.method,
                   xz = getPoints(rowSums(qr.Q(qr(cbind(1, x, z))) ^ 2)),
                   y = getPoints(abs(res)),
                   xyz = union(getPoints(abs(x - mean(x))), union(abs(z - mean(z)),
                                                                  getPoints(abs(res)))),
                   mahal= getPoints(rowSums(qr.Q(qr(cbind(1, x, y, z))) ^ 2)))
    rgl::text3d(x[ind], y[ind] + offset, z[ind], labels[ind],
                color = col)
    return(labels[ind])
  } 
}

ellipsoid <- function(center=c(0, 0, 0), radius=1, shape=diag(3), n=30){
  if (!requireNamespace("rgl")) "rgl package is missing"
  # adapted from the shapes3d demo in the rgl package
  degvec <- seq(0, 2*pi, length.out=n)
  ecoord2 <- function(p) c(cos(p[1])*sin(p[2]), sin(p[1])*sin(p[2]), cos(p[2]))
  v <- t(apply(expand.grid(degvec,degvec), 1, ecoord2))
  v <- center + radius * t(v %*% chol(shape))
  v <- rbind(v, rep(1,ncol(v)))
  e <- expand.grid(1:(n-1), 1:n)
  i1 <- apply(e, 1, function(z) z[1] + n*(z[2] - 1))
  i2 <- i1 + 1
  i3 <- (i1 + n - 1) %% n^2 + 1
  i4 <- (i2 + n - 1) %% n^2 + 1
  i <- rbind(i1, i2, i4, i3)
  rgl::qmesh3d(v, i)
}

nice <- function(x, direction=c("round", "down", "up"), lead.digits=1){
  direction <- match.arg(direction)
  if (length(x) > 1) return(sapply(x, nice, direction=direction, lead.digits=lead.digits))
  if (x == 0) return(0)
  power.10 <- floor(log(abs(x),10))
  if (lead.digits > 1) power.10 <- power.10 - lead.digits + 1
  lead.digit <- switch(direction,
                       round=round(abs(x)/10^power.10),
                       down=floor(abs(x)/10^power.10),
                       up=ceiling(abs(x)/10^power.10))
  sign(x)*lead.digit*10^power.10
}

applyDefaults <- function(args, defaults, type=""){
  if (isFALSE(args)) return(FALSE)
  names <- names(args)
  names <- names[names != ""]
  if (!isTRUE(args) && !is.null(args) && length(names) != length(args)) warning("unnamed ", type, " arguments, will be ignored")
  if (isTRUE(args) || is.null(names)) defaults
  else defaults[names] <- args[names]
  as.list(defaults)
}


scatter3dim <- function(x, y, z,
                        xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                        zlab=deparse(substitute(z)), axis.scales=TRUE, axis.ticks=FALSE,
                        revolutions=0, bg.col=c("white", "black"),
                        axis.col=if (bg.col == "white") c("darkmagenta", "black", "darkcyan")
                        else c("darkmagenta", "white", "darkcyan"),
                        surface.col=carPalette()[-1],
                        surface.alpha=0.5,
                        neg.res.col="red", pos.res.col="blue",
                        square.col=if (bg.col == "gray") "black" else "gray", point.col=ifelse(neg.res.col=="red","orange","blue"),
                        text.col=axis.col, grid.col=if (bg.col == "white") "black" else "gray",
                        fogtype=c("exp2", "linear", "exp", "none"),
                        residuals=(length(fit) == 1), surface=TRUE, fill=TRUE, grid=TRUE, grid.lines=26,
                        df.smooth=NULL, df.additive=NULL,
                        sphere.size=1, radius=1, threshold=0.01, speed=1, fov=60, 
                        fit="linear", groups=NULL, parallel=TRUE, ellipsoid=FALSE, level=0.5, ellipsoid.alpha=0.1,
                        # id.method=c("mahal", "xz", "y", "xyz", "identify", "none"), 
                        # id.n=if (id.method == "identify") Inf else 0,
                        # labels=as.character(seq(along=x)), offset = ((100/length(x))^(1/3)) * 0.02,
                        id=FALSE, model.summary=FALSE, 
                        reg.function, reg.function.col=surface.col[length(surface.col)], 
                        mouseMode=c(none="none", left="polar", right="zoom", middle="fov", wheel="pull"), ...){
  if (!requireNamespace("rgl")) stop("rgl package missing")
  if (!requireNamespace("mgcv")) stop("mgcv package missing")
  if (!requireNamespace("MASS")) stop("MASS package missing")
  rgl::par3d(mouseMode=mouseMode)
  id <- applyDefaults(id, defaults=list(method="mahal", n=2,
                                        labels=as.character(seq(along=x)), offset = ((100/length(x))^(1/3))*0.02), type="id")
  if (isFALSE(id)){
    id.n <- 0
    id.method <- "mahal"
    labels <- NULL
  }
  else{
    labels <- id$labels
    id.method <- id$method
    id.n <- if ("identify" %in% id.method) Inf else id$n
    offset <- id$offset
  }
  
  
  #    id.method <- match.arg(id.method)
  if (residuals == "squares"){
    residuals <- TRUE
    squares <- TRUE
  }
  else squares <- FALSE
  summaries <- list()
  if ((!is.null(groups)) && (nlevels(groups) > length(surface.col)))
    stop(sprintf("Number of groups (%d) exceeds number of colors (%d)",
                 nlevels(groups), length(surface.col)))
  if ((!is.null(groups)) && (!is.factor(groups))) stop("groups variable must be a factor")
  counts <- table(groups)
  if (any(counts == 0)){
    levels <- levels(groups)
    warning("the following groups are empty: ", paste(levels[counts == 0], collapse=", "))
    groups <- factor(groups, levels=levels[counts != 0])
  }
  bg.col <- match.arg(bg.col)
  fogtype <- match.arg(fogtype)
  if ((length(fit) > 1) && residuals && surface)
    stop("cannot plot both multiple surfaces and residuals")
  xlab  # cause these arguments to be evaluated
  ylab
  zlab
  rgl::next3d()
  rgl::view3d(fov=fov)
  rgl::bg3d(color=bg.col, fogtype=fogtype)
  if (id.method == "identify"){
    xg <- x
    yg <- y
    zg <- z
    ggroups <- groups
    glabels <- labels
  }
  valid <- if (is.null(groups)) complete.cases(x, y, z)
  else complete.cases(x, y, z, groups)
  x <- x[valid]
  y <- y[valid]
  z <- z[valid]
  labels <- labels[valid]
  minx <- min(x)
  maxx <- max(x)
  miny <- min(y)
  maxy <- max(y)
  minz <- min(z)
  maxz <- max(z)
  if (axis.scales){
    lab.min.x <- nice(minx)
    lab.max.x <- nice(maxx)
    lab.min.y <- nice(miny)
    lab.max.y <- nice(maxy)
    lab.min.z <- nice(minz)
    lab.max.z <- nice(maxz)
    minx <- min(lab.min.x, minx)
    maxx <- max(lab.max.x, maxx)
    miny <- min(lab.min.y, miny)
    maxy <- max(lab.max.y, maxy)
    minz <- min(lab.min.z, minz)
    maxz <- max(lab.max.z, maxz)
    min.x <- (lab.min.x - minx)/(maxx - minx)
    max.x <- (lab.max.x - minx)/(maxx - minx)
    min.y <- (lab.min.y - miny)/(maxy - miny)
    max.y <- (lab.max.y - miny)/(maxy - miny)
    min.z <- (lab.min.z - minz)/(maxz - minz)
    max.z <- (lab.max.z - minz)/(maxz - minz)
    if (axis.ticks){
      if (axis.scales) {
        x.labels <-  seq(lab.min.x, lab.max.x, 
                         by=diff(range(lab.min.x, lab.max.x))/4)
        x.at <- seq(min.x, max.x, by=nice(diff(range(min.x, max.x))/4))
        rgl::text3d(x.at, -0.05, 0, x.labels, col = axis.col[1])
        
        z.labels <-  seq(lab.min.z, lab.max.z, 
                         by=diff(range(lab.min.z, lab.max.z))/4)
        z.at <- seq(min.z, max.z, by=diff(range(min.z, max.z))/4)
        rgl::text3d(0, -0.1, z.at, z.labels, col = axis.col[3])
        
        y.labels <-  seq(lab.min.y, lab.max.y, 
                         by=diff(range(lab.min.y, lab.max.y))/4)
        y.at <- seq(min.y, max.y, by=diff(range(min.y, max.y))/4)
        rgl::text3d(-0.05, y.at, -0.05, y.labels, col = axis.col[2])
      }
    }
    else {
      rgl::text3d(min.x, -0.05, 0, lab.min.x, col=axis.col[1])
      rgl::text3d(max.x, -0.05, 0, lab.max.x, col=axis.col[1])
      rgl::text3d(0, -0.1, min.z, lab.min.z, col=axis.col[3])
      rgl::text3d(0, -0.1, max.z, lab.max.z, col=axis.col[3])
      rgl::text3d(-0.05, min.y, -0.05, lab.min.y, col=axis.col[2])
      rgl::text3d(-0.05, max.y, -0.05, lab.max.y, col=axis.col[2])
    }
  }
  if (!is.null(groups)) groups <- groups[valid]
  x <- (x - minx)/(maxx - minx)
  y <- (y - miny)/(maxy - miny)
  z <- (z - minz)/(maxz - minz)
  size <- sphere.size*((100/length(x))^(1/3))*0.015
  radius <- radius/median(radius)
  if (is.null(groups)){
    if (size > threshold) rgl::spheres3d(x, y, z, color=point.col, radius=size*radius)
    else rgl::points3d(x, y, z, color=point.col)
  }
  else {
    if (size > threshold) rgl::spheres3d(x, y, z, color=surface.col[as.numeric(groups)], radius=size*radius)
    else rgl::points3d(x, y, z, color=surface.col[as.numeric(groups)])
  }
  if (!axis.scales) axis.col[1] <- axis.col[3] <- axis.col[2]
  rgl::segments3d(c(0,1), c(0,0), c(0,0), color=axis.col[1])
  rgl::segments3d(c(0,0), c(0,1), c(0,0), color=axis.col[2])
  rgl::segments3d(c(0,0), c(0,0), c(0,1), color=axis.col[3])
  rgl::text3d(1, 0, 0, xlab, adj=1, color=axis.col[1])
  rgl::text3d(0, 1.05, 0, ylab, adj=1, color=axis.col[2])
  rgl::text3d(0, 0, 1, zlab, adj=1, color=axis.col[3])
  # if (axis.scales){
  #     rgl::text3d(min.x, -0.05, 0, lab.min.x, col=axis.col[1])
  #     rgl::text3d(max.x, -0.05, 0, lab.max.x, col=axis.col[1])
  #     rgl::text3d(0, -0.1, min.z, lab.min.z, col=axis.col[3])
  #     rgl::text3d(0, -0.1, max.z, lab.max.z, col=axis.col[3])
  #     rgl::text3d(-0.05, min.y, -0.05, lab.min.y, col=axis.col[2])
  #     rgl::text3d(-0.05, max.y, -0.05, lab.max.y, col=axis.col[2])
  # }
  if (ellipsoid) {
    dfn <- 3
    if (is.null(groups)){
      dfd <- length(x) - 1
      ell.radius <- sqrt(dfn * qf(level, dfn, dfd))
      ellips <- ellipsoid(center=c(mean(x), mean(y), mean(z)),
                          shape=cov(cbind(x,y,z)), radius=ell.radius)
      if (fill) rgl::shade3d(ellips, col=surface.col[1], alpha=ellipsoid.alpha, lit=FALSE)
      if (grid) rgl::wire3d(ellips, col=surface.col[1], lit=FALSE)
    }
    else{
      levs <- levels(groups)
      for (j in 1:length(levs)){
        group <- levs[j]
        select.obs <- groups == group
        xx <- x[select.obs]
        yy <- y[select.obs]
        zz <- z[select.obs]
        dfd <- length(xx) - 1
        ell.radius <- sqrt(dfn * qf(level, dfn, dfd))
        ellips <- ellipsoid(center=c(mean(xx), mean(yy), mean(zz)),
                            shape=cov(cbind(xx,yy,zz)), radius=ell.radius)
        if (fill) rgl::shade3d(ellips, col=surface.col[j], alpha=ellipsoid.alpha, lit=FALSE)
        if (grid) rgl::wire3d(ellips, col=surface.col[j], lit=FALSE)
        coords <- ellips$vb[, which.max(ellips$vb[1,])]
        if (!surface) rgl::text3d(coords[1] + 0.05, coords[2], coords[3], group,
                                  col=surface.col[j])
      }
    }
  }        
  
  vals <- seq(0, 1, length.out=grid.lines)
  dat <- expand.grid(x=vals, z=vals)
  
  if (surface){
    for (i in 1:length(fit)){
      f <- match.arg(fit[i], c("linear", "quadratic", "smooth", "additive", "robust"))
      if (is.null(groups)){
        mod <- switch(f,
                      linear = lm(y ~ x + z),
                      quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2)),
                      smooth = if (is.null(df.smooth)) mgcv::gam(y ~ s(x, z))
                      else mgcv::gam(y ~ s(x, z, fx=TRUE, k=df.smooth)),
                      additive = if (is.null(df.additive)) mgcv::gam(y ~ s(x) + s(z))
                      else mgcv::gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                       s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1))),
                      robust = MASS::rlm(y ~ x + z, method = "MM")
        )
        if (model.summary) summaries[[f]] <- summary(mod)
        yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
        if (fill) rgl::surface3d(x = vals, z = vals, y = yhat, color=surface.col[i], alpha=surface.alpha, lit=FALSE)
        if(grid) rgl::surface3d(x = vals, z = vals, y = yhat, color=if (fill) grid.col
                                else surface.col[i], alpha=surface.alpha, lit=FALSE, front="lines", back="lines")
        if (residuals){
          n <- length(y)
          fitted <- fitted(mod)
          colors <- ifelse(residuals(mod) > 0, pos.res.col, neg.res.col)
          rgl::segments3d(as.vector(rbind(x,x)), as.vector(rbind(y,fitted)), as.vector(rbind(z,z)),
                          color=as.vector(rbind(colors,colors)))
          if (squares){
            res <- y - fitted
            xx <- as.vector(rbind(x, x, x + res, x + res))
            yy <- as.vector(rbind(y, fitted, fitted, y))
            zz <- as.vector(rbind(z, z, z, z))
            rgl::quads3d(xx, yy, zz, color=square.col, alpha=surface.alpha, lit=FALSE)
            rgl::segments3d(xx, yy, zz, color=square.col)
          }
        }
      }
      else{
        if (parallel){
          mod <- switch(f,
                        linear = lm(y ~ x + z + groups),
                        quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2) + groups),
                        smooth = if (is.null(df.smooth)) mgcv::gam(y ~ s(x, z) + groups)
                        else mgcv::gam(y ~ s(x, z, fx=TRUE, k=df.smooth) + groups),
                        additive = if (is.null(df.additive)) mgcv::gam(y ~ s(x) + s(z) + groups)
                        else mgcv::gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                         s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)) + groups),
                        robust = MASS::rlm(y ~ x + z + groups, method = "MM")
          )
          if (model.summary) summaries[[f]] <- summary(mod)
          levs <- levels(groups)
          for (j in 1:length(levs)){
            group <- levs[j]
            select.obs <- groups == group
            yhat <- matrix(predict(mod, newdata=cbind(dat, groups=group)), grid.lines, grid.lines)
            if (fill) rgl::surface3d(x = vals, z = vals, y = yhat, color=surface.col[j], alpha=surface.alpha, lit=FALSE)
            if (grid) rgl::surface3d(x = vals, z = vals, y = yhat, color=if (fill) grid.col
                                     else surface.col[j], alpha=surface.alpha, lit=FALSE, front="lines", back="lines")
            rgl::text3d(1, predict(mod, newdata=data.frame(x=1, z=1, groups=group)), 1,
                        paste(group, " "), adj=1, color=surface.col[j])
            if (residuals){
              yy <- y[select.obs]
              xx <- x[select.obs]
              zz <- z[select.obs]
              fitted <- fitted(mod)[select.obs]
              res <- yy - fitted
              rgl::segments3d(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                              col=surface.col[j])
              if (squares) {
                xxx <- as.vector(rbind(xx, xx, xx + res, xx + res))
                yyy <- as.vector(rbind(yy, fitted, fitted, yy))
                zzz <- as.vector(rbind(zz, zz, zz, zz))
                rgl::quads3d(xxx, yyy, zzz, color=surface.col[j], alpha=surface.alpha, lit=FALSE)
                rgl::segments3d(xxx, yyy, zzz, color=surface.col[j])
              }
            }
          }
        }
        else {
          levs <- levels(groups)
          for (j in 1:length(levs)){
            group <- levs[j]
            select.obs <- groups == group
            mod <- switch(f,
                          linear = lm(y ~ x + z, subset=select.obs),
                          quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2), subset=select.obs),
                          smooth = if (is.null(df.smooth)) mgcv::gam(y ~ s(x, z), subset=select.obs)
                          else mgcv::gam(y ~ s(x, z, fx=TRUE, k=df.smooth), subset=select.obs),
                          additive = if (is.null(df.additive)) mgcv::gam(y ~ s(x) + s(z), subset=select.obs)
                          else mgcv::gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                           s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)), subset=select.obs)
            )
            if (model.summary) summaries[[paste(f, ".", group, sep="")]] <- summary(mod)
            yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
            if (fill) rgl::surface3d(x = vals, z = vals, y = yhat, color=surface.col[j], alpha=surface.alpha, lit=FALSE)
            if (grid) rgl::surface3d(x = vals, z = vals, y = yhat, color=if (fill) grid.col
                                     else surface.col[j], alpha=surface.alpha, lit=FALSE, front="lines", back="lines")
            rgl::text3d(1, predict(mod, newdata=data.frame(x=1, z=1, groups=group)), 1,
                        paste(group, " "), adj=1, color=surface.col[j])
            if (residuals){
              yy <- y[select.obs]
              xx <- x[select.obs]
              zz <- z[select.obs]
              fitted <- fitted(mod)
              res <- yy - fitted
              rgl::segments3d(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                              col=surface.col[j])
              if (squares) {
                xxx <- as.vector(rbind(xx, xx, xx + res, xx + res))
                yyy <- as.vector(rbind(yy, fitted, fitted, yy))
                zzz <- as.vector(rbind(zz, zz, zz, zz))
                rgl::quads3d(xxx, yyy, zzz, color=surface.col[j], alpha=surface.alpha, lit=FALSE)
                rgl::segments3d(xxx, yyy, zzz, color=surface.col[j])
              }
            }
          }
        }
      }
    }
  }
  else levs <- levels(groups)
  
  # plot an arbitrary regression function
  if (!missing(reg.function)){
    x <- seq(minx, maxx, length.out=grid.lines)
    z <- seq(minz, maxz, length.out=grid.lines)
    D <- expand.grid(x=x, z=z)
    x <- D$x
    z <- D$z
    ys <- eval(substitute(reg.function))
    ys <- (ys - miny)/(maxy - miny)
    ys <- matrix(ys, grid.lines, grid.lines)
    
    if (fill) rgl::surface3d(x = vals, z = vals, y = ys, color=reg.function.col, alpha=surface.alpha, lit=FALSE)
    if(grid) rgl::surface3d(x = vals, z = vals, y = ys, color=if (fill) grid.col
                            else reg.function.col, alpha=surface.alpha, lit=FALSE, front="lines", back="lines")
  }
  
  if (id.method == "identify"){
    Identify3d(xg, yg, zg, axis.scales=axis.scales, groups=ggroups, labels=glabels, 
               col=surface.col, offset=offset)
  }
  else if (id.method != "none"){
    if (is.null(groups)) 
      showLabels3d(x, y, z, labels, id.method=id.method, id.n=id.n, col=surface.col[1])
    else {
      for (j in 1:length(levs)){
        group <- levs[j]
        select.obs <- groups == group
        showLabels3d(x[select.obs], y[select.obs], z[select.obs], labels[select.obs], 
                     id.method=id.method, id.n=id.n, col=surface.col[j])
      }
    }
  }
  if (revolutions > 0) {
    for (i in 1:revolutions){
      for (angle in seq(1, 360, length.out=360/speed)) rgl::view3d(-angle, fov=fov)
    }
  }
  if (model.summary) return(summaries) else return(invisible(NULL))
}

filled.contour2 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)),
            z,
            xlim = range(x, finite=TRUE),
            ylim = range(y, finite=TRUE),
            zlim = range(z, finite=TRUE),
            levels = pretty(zlim, nlevels), nlevels = 20,
            color.palette = cm.colors,
            col = color.palette(length(levels) - 1),
            plot.title, plot.axes, key.title, key.axes,
            asp = NA, xaxs = "i", yaxs = "i", las = 1, axes = TRUE,
            frame.plot = axes, ...)
  {
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
      stop("increasing 'x' and 'y' values expected")
    
    mar.orig <- (par.orig <- par(c("mar","las","mfrow")))$mar
    on.exit(par(par.orig))
    
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    
    ## Plot the 'plot key' (scale):
    mar <- mar.orig
    mar[4L] <- mar[2L]*1.5
    mar[2L] <- 1/2
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0,1), ylim = range(levels), xaxs = "i", yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes)
        axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title))
      key.title
    
    ## Plot contour-image::
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    
    .filled.contour2(x, y, z, levels, col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) box()
    if (missing(plot.title))
      title(...)
    else
      plot.title
    invisible()
  }

.filled.contour2 <- function(x, y, z , levels, col)
{
  if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L)
    stop("no proper 'z' matrix specified")
  .External.graphics(C_filledcontour, x, y, z, levels, col)
  invisible()
}