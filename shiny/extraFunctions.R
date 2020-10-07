load("data2020.RData")

get.ref.numbers <- function(input.list, year.range = c(2014, 
    2015), which.source = "unhcr", countries = c("1", "2")) {
    # Extract data
    asyl.ls <- input.list$asyl
    total <- input.list$total
    accept <- input.list$accept
    all.eu <- any(countries == "1")
    all.dublin <- any(countries == "2")
    
    # Subset the year for index calculation
    which.year <- year.range[1] - 1
    total.year <- subset(total, year == which.year)
    
    # Get the chosen asyl source
    total.year$asyl <- total.year[, grep(which.source, colnames(total.year))]
    
    # Discard the non-EU states
    if (!all.eu) 
        total.year <- total.year[which(!total.year$country %in% 
            non.eu), ]
    if (!all.dublin) 
        total.year <- total.year[which(!total.year$country %in% 
            non.dublin), ]
    
    # Get the number of Asyl Applicants in the time range (for
    # the specified source!)
    applic.year <- subset(asyl.ls[[which.source]], (country %in% 
        total.year$country) & (year %in% (year.range[1]:year.range[2])))
    all.applic <- sum(aggregate(applic ~ country, applic.year, 
        sum)$applic, na.rm = TRUE)
    
    # Get the number of Accepted Asyl Applicants (only one
    # source)
    accept.year <- subset(accept, (country %in% total.year$country) & 
        (year %in% (year.range[1]:year.range[2])))
    all.accept <- sum(aggregate(accept ~ country, accept.year, 
        sum)$accept, na.rm = TRUE)
    
    # Make Output
    all.numbers <- c(all.applic, all.accept, length(unique(total.year$country)))
    return(all.numbers)
}

get.ref.table <- function(input.list, year.range = c(2014, 2015), 
    which.source = "unhcr", per.capita = FALSE, countries = c("1", 
        "2"), which.idx = "grech", w.pop = 0.4, w.gdp = 0.4, 
    w.asyl = 0.1, w.unemp = 0.1) {
    # Extract data
    asyl.ls <- input.list$asyl
    total <- input.list$total
    accept <- input.list$accept
    all.eu <- any(countries == "1")
    all.dublin <- any(countries == "2")
    
    # Subset the year for index calculation
    which.year <- year.range[1] - 1
    total.year <- subset(total, year == which.year)
    # Get the chosen asyl source
    #total.year$asyl <- total.year[, grep(which.source, colnames(total.year))]
    total.year$asyl <- total.year %>% select(all_of(paste0(which.source,".asyleffect"))) %>% pull()
    # Discard the non-EU states
    if (!all.eu) 
        total.year <- total.year[which(!total.year$country %in% 
            non.eu), ]
    if (!all.dublin) 
        total.year <- total.year[which(!total.year$country %in% 
            non.dublin), ]

    # Get the number of Asyl Applicants in the time range (for
    # the specified source!)
    applic.year <- subset(asyl.ls[[which.source]], (country %in% 
        total.year$country) & (year %in% (year.range[1]:year.range[2])))
    num.applic <- aggregate(applic ~ country, applic.year, sum)
    quota.applic <- round(num.applic$applic/sum(num.applic$applic, 
        na.rm = FALSE), 4)
    all.applic <- sum(num.applic$applic, na.rm = TRUE)
    res.applic <- cbind(num.applic, quota.applic)
    
    # Calculate the chosen index with all the pre-set values
    print(str(total.year))
    res.key <- calc.key(total.year, which = which.idx, weight.pop = w.pop, 
        weight.gdp = w.gdp, weight.unemp = w.unemp, weight.asyl = w.asyl, 
        number = all.applic)
    res.mat <- merge(res.key, res.applic, by = "country")
    
    # Get the number of accepted Asyl Applicants in the time
    # range (only Eurostat)
    accept.year <- subset(accept, (country %in% total.year$country) & 
        (year %in% (year.range[1]:year.range[2])))
    num.accept <- aggregate(accept ~ country, accept.year, sum)
    quota.accept <- round(num.accept$accept/sum(num.accept$accept, 
        na.rm = FALSE), 4)
    all.accept <- sum(num.accept$accept, na.rm = TRUE)
    res.accept <- cbind(num.accept, quota.accept)
    res.mat <- merge(res.mat, res.accept, by = "country")
    res.mat$good <- res.mat$quota.accept >= res.mat$quota.key
    res.output <- res.mat[, c(1, 2, 4, 6, 3, 5, 7)]
    colnames(res.output) <- c("country", "quota", "applications", 
        "accepted", "share.quota", "share.applications", "share.accepted")
    if (per.capita) {
        capita <- aggregate(pop ~ country, subset(total, (country %in% 
            total.year$country) & (year %in% (year.range[1]:year.range[2]))), 
            mean)
        patt.abs <- grep("country|share", colnames(res.output), 
            invert = TRUE)
        res.output[, patt.abs] <- apply(res.output[, patt.abs], 
            2, function(x) (x * 1)/round(capita$pop/1000, 2))
    }
    return(res.output[order(res.output$share.quota, decreasing = TRUE), 
        ])
    
}

get.ref.plot <- function(input.list, year.range = c(2014, 2015), 
    which.source = "first", which.show = "ratio", countries = c("1", 
        "2"), which.idx = "grech", w.pop = 0.4, w.gdp = 0.4, 
    w.asyl = 0.1, w.unemp = 0.1) {
    
    # Extract data
    asyl.ls <- input.list$asyl
    total <- input.list$total
    accept <- input.list$accept
    all.eu <- any(countries == "1")
    all.dublin <- any(countries == "2")
    
    # Subset the year for index calculation
    which.year <- year.range[1] - 1
    total.year <- subset(total, year == which.year)
    
    # Get the (pre-calculated) Asyl-Effect from the chosen source
    total.year$asyl <- total.year[, grep(which.source, colnames(total.year))] %>%
      pull(grep(which.source, colnames(total.year),value = TRUE))
    
    # Discard the non-chosen states
    if (!all.eu) 
        total.year <- total.year[which(!total.year$country %in% 
            non.eu), ]
    if (!all.dublin) 
        total.year <- total.year[which(!total.year$country %in% 
            non.dublin), ]
    
    # Get the number of Asyl Applicants in the time range (for
    # the specified source!)
    applic.year <- subset(asyl.ls[[which.source]], (country %in% 
        total.year$country) & (year %in% (year.range[1]:year.range[2])))
    num.applic <- aggregate(applic ~ country, applic.year, sum)
    quota.applic <- round(num.applic$applic/sum(num.applic$applic, 
        na.rm = FALSE), 4)
    all.applic <- sum(num.applic$applic, na.rm = TRUE)
    res.applic <- cbind(num.applic, quota.applic)
    
    # Calculate the chosen index with all the pre-set values
    res.key <- calc.key(total.year, which = which.idx, weight.pop = w.pop, 
        weight.gdp = w.gdp, weight.unemp = w.unemp, weight.asyl = w.asyl, 
        number = all.applic)

    # Get the number of accepted Asyl Applicants in the time
    # range (only Eurostat)
    accept.year <- subset(accept, (country %in% total.year$country) & 
        (year %in% (year.range[1]:year.range[2])))
    num.accept <- aggregate(accept ~ country, accept.year, sum)
    quota.accept <- round(num.accept$accept/sum(num.accept$accept, 
        na.rm = FALSE), 4)
    all.accept <- sum(num.accept$accept, na.rm = TRUE)
    res.accept <- cbind(num.accept, quota.accept)
    res.mat <- merge(res.key, res.accept, by = "country")
    
    # Merge results with number of asylum applications
    res.mat <- merge(res.mat, res.applic, by = "country")
    # Legend stuff
    leg.labels <- c("Refugees by quota", "Applications in reality", 
        "Accepted Applications in reality")
    leg.cols <- cbPalette[c(6, 5, 2)]
    
    # Make Index variable for countries that perform better than
    # by index
    res.mat$good <- res.mat$quota.accept >= res.mat$quota.key
    # Sort countries by ratio of key (starting with the highest)
    res.order <- order(res.mat$quota.key, decreasing = TRUE)
    res.mat <- res.mat[res.order, ]
    
    if (which.show == "abs") {
        legend_title <- expression(paste(bold("Number of ...")))
        res.mat$quota.key <- res.mat$key
        res.mat$quota.accept <- res.mat$accept
        res.mat$quota.applic <- res.mat$applic
        # gg.ylab <- 'Number of ...'
    } else if (which.show == "per") {
        legend_title <- expression(paste(bold("Number of ... per 1000 inhabitants")))
        res.mat$quota.key <- res.mat$key
        res.mat$quota.accept <- res.mat$accept
        res.mat$quota.applic <- res.mat$applic
        # gg.ylab <- 'Asylum Applications per 1000 inhabitants'
        capita <- aggregate(pop ~ country, subset(total, (country %in% 
            total.year$country) & (year %in% (year.range[1]:year.range[2]))), 
            FUN = mean)
        patt.abs <- grep("quota", colnames(res.mat))
        res.mat[, patt.abs] <- apply(res.mat[, patt.abs], 2, 
            function(x) (x * 1)/round(capita$pop[res.order]/1000, 
                2))
    } else if (which.show == "ratio") {
        # Legend title of plot
        legend_title <- expression(bold(paste("Share of ...")))
        # gg.ylab <- ' Share of ...'
    }
    # legend_title <- ''
    gg.ylab <- ""
    # Final Subset
    res.quota <- res.mat[, grep("quota|country|good", colnames(res.mat))]
    res.quota <- res.quota[, order(colnames(res.quota), decreasing = TRUE)]
    # Melt and Split the data set
    melt.quota <- melt(res.quota, id.vars = c("country", "good"))
    melt.quota <- split(melt.quota, melt.quota$good)
    # Plot it
    xlab.low <- expression(Proportion ~ of ~ Asylum ~ Applications ~ 
        bold(paste("lower than quota")))
    xlab.high <- expression(Proportion ~ of ~ Asylum ~ Applications ~ 
        bold(paste("higher than quota")))
    q1 <- ggplot(melt.quota[[1]], aes(x = country, y = value, 
        fill = variable)) + geom_bar(stat = "identity", position = "dodge", 
        colour = "black") + coord_flip() + scale_fill_manual(legend_title, 
        labels = leg.labels, values = leg.cols, guide = guide_legend(reverse = TRUE)) + 
        scale_x_discrete(limits = rev(unique(melt.quota[[1]]$country))) + 
        labs(y = gg.ylab, x = xlab.low) + scale_y_continuous(labels = comma) + 
        theme(legend.position = "bottom") + guides(fill = guide_legend(title.position = "top", 
        title.hjust = 0.5))
    q2 <- ggplot(melt.quota[[2]], aes(x = country, y = value, 
        fill = variable)) + geom_bar(stat = "identity", position = "dodge", 
        colour = "black") + coord_flip() + scale_fill_manual(legend_title, 
        labels = leg.labels, values = leg.cols) + labs(y = gg.ylab, 
        x = xlab.high) + scale_x_discrete(limits = rev(unique(melt.quota[[2]]$country))) + 
        scale_y_continuous(labels = comma)
    return(list(q1 = q1, q2 = q2))
}

get.index.data <- function(input.list, year.range = 2015, which.source = "unhcr", 
    countries = c("1", "2")) {
    #------------------
    # Main Calculations
    #-----------------
    total <- input.list$total
    accept <- input.list$accept
    all.eu <- any(countries == "1")
    all.dublin <- any(countries == "2")
    # Subset the year for index calculation
    which.year <- year.range
    total.year <- subset(total, year == which.year)
    # Get the chosen asyl source
    total.year$asyl <- total.year[, grep(which.source, colnames(total.year))]
    # Discard the non-EU states
    if (!all.eu) 
        total.year <- total.year[which(!total.year$country %in% 
            non.eu), ]
    if (!all.dublin) 
        total.year <- total.year[which(!total.year$country %in% 
            non.dublin), ]
    # Sort the data
    total.year <- total.year[, c(1, 5, 3, 4, ncol(total.year), 
        6)]
    # Name the data for good viewing
    colnames(total.year) <- c("country", "population", "gdp", 
        "unemp.rate", "asyl.per.million.capita", "gdp.per.capita")
    return(total.year)
}

# Function to calculate the different refugee distribution
# keys that will be plotted
calc.key <- function(x, weight.pop = 0.4, weight.gdp = 0.4, which = "grech", 
    cap = 0.3, weight.unemp = 0.1, weight.asyl = 0.1, number = 1e+06) {
  cat("\n\n")
  print(str(x))
    # Number of refugees to distribute
    if (which == "grech") {
        unemp.effect <- 1 + weight.unemp * (1 - (x$unemp/mean(x$unemp, 
            na.rm = TRUE)))
        asyl.effect <- 1 + weight.asyl * (1 - (x$asyl/mean(x$asyl, 
            na.rm = TRUE)))
        base.effect <- 0.5 * ((x$pop/sum(x$pop, na.rm = TRUE)) + 
            (x$gdp/sum(x$gdp, na.rm = TRUE)))
        quota.key <- (unemp.effect * asyl.effect * base.effect)/sum((unemp.effect * 
            asyl.effect * base.effect), na.rm = TRUE)
        key <- quota.key * number
    } else if (which == "konig") {
        quota.key <- as.matrix(weight.pop * (x$pop/sum(x$pop, 
            na.rm = TRUE)) + weight.gdp * (x$gdp/sum(x$gdp, na.rm = TRUE)))
        key <- quota.key * number
    } else if (which == "eu") {
        # Calculate the effect of GDP and Population
        gdp.effect <- x$gdp/sum(x$gdp, na.rm = TRUE)
        pop.effect <- x$pop/sum(x$pop, na.rm = TRUE)
        base.effect <- gdp.effect + pop.effect
        # Calculate the effect of the past number of Asyl
        # Applications
        pure.asyl <- (x$asyl^(-1))/sum(x$asyl^(-1), na.rm = TRUE)
        pure.asyl[which(is.nan(pure.asyl))] <- 0
        asyl.effect <- pmin(pure.asyl, cap * base.effect)
        # Calculate the effect of Unemployment
        pure.unemp <- (x$unemp^(-1))/sum(x$unemp^(-1), na.rm = TRUE)
        unemp.effect <- pmin(pure.unemp, cap * base.effect)
        alloc.cap <- number * (weight.pop * pop.effect + weight.gdp * 
            gdp.effect + weight.asyl * asyl.effect + weight.unemp * 
            unemp.effect)
        alloc.res <- (number - sum(alloc.cap, na.rm = TRUE)) * 
            (0.5 * base.effect)
        key <- alloc.cap + alloc.res
        quota.key <- key/number
    }
    quota.key <- round(quota.key, 4)
    temp <- cbind.data.frame(x$country, key, quota.key)
    print(temp)
    temp$key <- round(temp$key)
    colnames(temp)[1] <- "country"
    return(temp)
}

# Rolling average from left to calculate the average asylum
# applications per 1mil Population of the preceeding X years
rollAvg <- function(x, width = 5) {
    window <- rev(1:width)
    lenx <- length(x)
    rolled <- rep(NA, lenx)
    min.win <- rep(1, width)
    for (i in 2:lenx) {
        temp.win <- unique(pmax(i - window, min.win))
        rolled[i] <- mean(x[temp.win], na.rm = TRUE)
        if (is.nan(rolled[i])) 
            rolled[i] <- 0
    }
    return(rolled)
}

# Function to calculate the above mentioned asyleffect
calc.asyleffect <- function(asyl, population, name.app = "applic", 
    name.pop = "pop", name.geo = "geo", name.time = "time") {
    merged <- merge(asyl, population, by = c("geo", "time"))
    pure.asyl <- merged[, c(name.app)]/round(merged[, c(name.pop)]/1e+06, 
        2)
    asyleffect <- unlist(by(pure.asyl, merged[, c(name.geo)], 
        rollAvg))
    temp <- cbind(merged[, c(name.time, name.geo)], asyleffect)
    colnames(temp)[3] <- paste0(name.app, ".asyleffect")
    rownames(temp) <- NULL
    return(temp)
}

# Function to read (and correct) csv-data created by Eurostat
read.eurostat <- function(file, name) {
    temp <- read.table(file, header = TRUE, sep = ",", quote = "\"", 
        dec = ".", na.strings = ":")
    temp$Value <- as.numeric(gsub(",", "", temp$Value))
    colnames(temp)[which(colnames(temp) == "Value")] <- name
    if ("GEO" %in% colnames(temp)) 
        temp$GEO <- gsub("^Germany \\(until.*$", "Germany", temp$GEO)
    return(temp)
}

# Function to plot multiple ggplots (not used)
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel ncol: Number of columns of plots nrow:
        # Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
            ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), 
            ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain
            # this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                layout.pos.col = matchidx$col))
        }
    }
}

# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), 
    nrow = 1, position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(position, bottom = arrangeGrob(do.call(arrangeGrob, 
        gl), legend, ncol = 1, heights = grid::unit.c(unit(1, 
        "npc") - lheight, lheight)), right = arrangeGrob(do.call(arrangeGrob, 
        gl), legend, ncol = 2, widths = grid::unit.c(unit(1, 
        "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
    
}

# Extract legend from ggplot (used)
g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

# Colour palette for the colour blind (used)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
    "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
    "#0072B2", "#D55E00", "#CC79A7")

tidy_eurostat <- function(dat, time_format = "date", select_time = NULL,
                          stringsAsFactors = FALSE,
                          keepFlags = FALSE) {
  
  # To avoid warnings
  time <- values <- NULL
  
  # Separate codes to columns
  cnames <- strsplit(colnames(dat)[1], split = "[\\,]")[[1]]
  cnames1 <- cnames[-length(cnames)]  # for columns
  cnames2 <- cnames[length(cnames)]   # for colnames
  
  # Separe variables from first column
  dat <- tidyr::separate_(dat, col = colnames(dat)[1],
                          into = cnames1,
                          sep = ",", convert = FALSE)
  
  # Get variable from column names
  
  cnames2_quo <- as.name(cnames2)
  dat <- tidyr::gather(dat, !!cnames2_quo, values, 
                       -seq_along(cnames1),
                       convert = FALSE)    
  
  # to save memory (and backward compatibility)
  dat <- dplyr::filter(dat, !is.na(values))
  print(head(dat))
  ## separate flags into separate column
  if(keepFlags == TRUE) {
    dat$flags <- as.vector(
      stringi::stri_extract_first_regex(dat$values, 
                                        c("(^0n( [A-Za-z]+)*)|[A-Za-z]+")))
  }
  
  # clean time and values
  dat$time <- gsub("X", "", dat$TIME_PERIOD)
  dat$values <- as.numeric(gsub("[^0-9.-]+", "", as.character(dat$values)))
  
  # variable columns
  var_cols <- names(dat)[!(names(dat) %in% c("time", "values"))]
  
  # reorder to standard order
  dat <- dat[c(var_cols, "time", "values")]
  
  # columns from var_cols are converted into factors
  # avoid convert = FALSE since it converts T into TRUE instead of TOTAL
  if (stringsAsFactors){
    dat[,var_cols] <- lapply(dat[, var_cols, drop = FALSE],
                             function(x) factor(x, levels = unique(x)))
  }
  
  # For multiple time frequency
  freqs <- available_freq(dat$time)
  
  if (!is.null(select_time)){
    if (length(select_time) > 1) stop(
      "Only one frequency should be selected in select_time. Selected: ",
      shQuote(select_time))
    
    # Annual
    if (select_time == "Y"){
      dat <- subset(dat, nchar(time) == 4)
      # Others
    } else {
      dat <- subset(dat, grepl(select_time, time))
    }
    # Test if data
    if (nrow(dat) == 0) stop(
      "No data selected with select_time:", dQuote(select_time), "\n",
      "Available frequencies: ", shQuote(freqs))
  } else {
    
    if (length(freqs) > 1 & time_format != "raw") stop(
      "Data includes several time frequencies. Select frequency with
         select_time or use time_format = \"raw\".
         Available frequencies: ", shQuote(freqs ))
  }
  
  # convert time column
  dat$time <- convert_time_col(dat$time,
                               time_format = time_format)
  
  
  
  dat
  
}

available_freq <- function(x){
  if (is.factor(x)) x <- levels(x)
  x <- gsub("X", "", x)
  freq <- c()
  if (any(nchar(x) == 4)) freq <- c(freq, "Y")
  freq_char <- unique(
    substr(grep("[[:alpha:]]", x, value = TRUE), 5,5))
  freq <- c(freq, freq_char)
  freq
}

convert_time_col <- function(x, time_format){
  
  if (time_format == "raw"){
    y <- x
  } else {
    x <- factor(x)
    if (time_format == "date"){
      y <- eurotime2date(x, last = FALSE)
    } else if (time_format == "date_last"){
      y <- eurotime2date(x, last = TRUE)
    } else if (time_format == "num"){
      y <- eurotime2num(x)
    } else if (time_format == "raw") {
      
    } else {
      stop("An unknown time_format argument: ", time_format,
           " Allowed are date, date_last, num and raw")
    }
  }
  y
}