observeEvent(input$glossaireOPEN,{
	hideElement(id = "main_content")
	hideElement(id = "sidebarItemExpanded")
	showElement(
		id="glossaire",
		anim = T,
  		animType = "fade",
  		time = 0.5
  	)
	hideElement(id="glossaireOPEN")
	showElement(id="glossaireCLOSE")
})

observeEvent(input$glossairePREV,{
	current_article$name <- NULL
	hideElement(id="glossairePREV")
})

observeEvent(input$glossaireCLOSE,{
	current_article$name <- NULL
	hideElement(id = "glossairePREV")
	hideElement(id = "glossaire")
	showElement(id = "sidebarItemExpanded")
	showElement(
		id="main_content",
		anim = T,
  		animType = "fade",
  		time = 0.5
  	)
	hideElement(id = "glossaireCLOSE")
	showElement(id = "glossaireOPEN")
})

ls <- list.files("module/glossaire") 
paths <- ls %>% lapply(function(x){paste0("module/glossaire/", x)})
isdir <- sapply(paths, function(p){file.info(p)$isdir})
article_list <- ls[isdir]

df <- article_list %>% lapply(function(article){
	data.frame(
		name = article,
		date_created = file.info(paste0("module/glossaire/", article))$ctime,
		date_last_modifed = file.info(paste0("module/glossaire/", article))$mtime
	) %>% mutate(
		date_created = date_created %>% str_replace_all("T"," "),
		date_last_modifed = date_last_modifed %>% str_replace_all("T"," ")
	) 
}) %>% bind_rows() %>% arrange(desc(date_created))

df$name <- as.character(df$name)

current_article <- reactiveValues(
	name = NULL
)

for(i in 1:nrow(df)){
	name <- df$name[i]
	eval(parse(text=paste0("onclick('glossaire-",name,"',{current_article$name<-'",name,"'})")))
}

output$glossaireList <- renderReactable({
	reactable(
		df,
		columns = list(
			name = colDef(
				name = "Article",
				cell = function(value, index, name){
					if(is.na(file.info((paste0("module/glossaire/",value,"/icon.png")))$size)){
						img_src <- knitr::image_uri(paste0("module/glossaire/default_icon.png"))
					}else{
						img_src <- knitr::image_uri(paste0("module/glossaire/",value,"/icon.png"))
					}
				    image <- img(src = img_src, height = "50px", alt = value, class = "icon_glossaire")
				    tagList(
						div(style = list(display = "inline-block", width = "50px"), image),
						div(
							style = list(
								display = "inline-block", 
								`font-size` = "20px",
								position =  "relative",
								bottom =  "15px",
								left =  "10px",
								color = "steelblue"
							), 
							actionButton(
								class = "glossaire_link",
								paste0('glossaire-',value)
								,value %>% str_replace_all("_"," ") %>% str_to_title()
							)
						)
				    )
				}
			),
			date_last_modifed = colDef(
				style = list(`padding-top` = "25px"),
				name = "Dernière Modification",
				cell = function(value){
					day <- substr(value,1,10)
					if(substr(value,1,10) == Sys.Date()){
						day <- "Aujourd'hui"
					}else if(substr(value,1,10) == Sys.Date() - days(1)){
						day <- "Hier"
					}else if(substr(value,1,10) > Sys.Date() - weeks(1)){
						day <- "Il y a " %>% paste0(difftime(Sys.Date(), substr(value,1,10), units = "days") %>% round() %>% paste0(" Jours"))
					}else if(substr(value,1,10) > Sys.Date() - months(1)){
						day <- "Il y a " %>% paste0(difftime(Sys.Date(), substr(value,1,10), units = "weeks") %>% round() %>% paste0(" Semaines"))
					}else{
						day <- "Il y a " %>% paste0(difftime(Sys.Date(), substr(value,1,10), units = "auto") %>% round() %>% paste0(" Mois"))
					}
					return(day)
				} 
			),
			date_created = colDef(
				style = list(`padding-top` = "25px"),
				name = "Date Création",
				cell = function(value){
					day <- substr(value,1,10)
					if(substr(value,1,10) == Sys.Date()){
						day <- "Aujourd'hui"
					}else if(substr(value,1,10) == Sys.Date() - days(1)){
						day <- "Hier"
					}else if(substr(value,1,10) > Sys.Date() - weeks(1)){
						day <- "Il y a " %>% paste0(difftime(Sys.Date(), substr(value,1,10), units = "days") %>% round() %>% paste0(" Jours"))
					}else if(substr(value,1,10) > Sys.Date() - months(1)){
						day <- "Il y a " %>% paste0(difftime(Sys.Date(), substr(value,1,10), units = "weeks") %>% round() %>% paste0(" Semaines"))
					}else{
						day <- "Il y a " %>% paste0(difftime(Sys.Date(), substr(value,1,10), units = "auto") %>% round() %>% paste0(" Mois"))
					}
					return(day)
				} 
			)
		)
	)
})

output$glossaireArticle <- renderUI({
	if(!is.null(current_article$name)){
		showElement(id="glossairePREV")
		wd <- getwd()
		HTML(markdown::markdownToHTML(knitr::knit(paste0('module/glossaire/',current_article$name,'/index.Rmd'), quiet = TRUE)))
	}
})

output$glossaireContent <- renderUI({
	if(is.null(current_article$name)){
		reactableOutput("glossaireList")
	}
})