# shiny server side code for each call
shinyServer(function(input, output, session){
		
  output$type <- renderUI({ 
    obj <- pel
    var.opts <- namel(levels(obj$tipo))
    selectInput("type","Tipo:", c("Todos", var.opts))})

  output$caption <- renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram"  =	"Histograma",
           "density" 	=	"Densidade",
           "bar" 		=	"Barras",
           "regression" =   "Regressao")})
	
  output$plot <- renderUI({
    plotOutput("p")})
		
  #plotting function using ggplot2
  output$p <- renderPlot({
    plot.obj <<- list()
    if (input$type == "Todos") {
      plot.obj$data <<- pel 
    } else {
      plot.obj$data <<- pel[tipo == input$type] 
    }
    plot.obj$variable_y <<- with(plot.obj$data,get(input$variable_y)) 
    plot.obj$variable_x <<- with(plot.obj$data,get(input$variable_x)) 
    plot.obj$group <<- with(plot.obj$data,get(input$group)) 
	
    #dynamic plotting options
    plot.type <- switch(input$plot.type,
        "boxplot" 	 = 	geom_boxplot(),
        "histogram"  =	geom_histogram(alpha=0.5,position="identity"),
        "density" 	 =	geom_density(alpha=.75),
        "bar" 		 =	geom_bar(position="dodge"),
        "regression" =  geom_point(aes(color=plot.obj$group))
		)
		
    #plotting theme
    .theme<- theme(
      axis.line = element_line(colour = 'gray', size = .75),
      panel.background = element_blank(),
      plot.background = element_blank())	 
	if(input$plot.type=="boxplot") {
      p <- ggplot(plot.obj$data,
                  aes(
                    x = plot.obj$group, 
					y = plot.obj$variable_y,
					fill = as.factor(plot.obj$group))) + plot.type
				
      if(input$show.points==TRUE) {
        p <- p + geom_point(color='black',alpha=0.5,position = 'jitter')
      }
    } else if(input$plot.type == "regression") {
      p <- ggplot(plot.obj$data,
                  aes(
                    x = plot.obj$variable_x, 
					y = plot.obj$variable_y)) + plot.type +
           geom_smooth(aes(color=plot.obj$group), method="lm", se=FALSE)
    } else {
      p <- ggplot(plot.obj$data,
                  aes(
                    x = plot.obj$variable_y,
					fill = as.factor(plot.obj$group),
					group = as.factor(plot.obj$group))) + plot.type
    }
    p <- p + labs(
               fill = input$group,
               x = "",
               y = input$variable_y) + .theme
    print(p)
	})	
})

