library(grid)
library(gtable)
library(cowplot)
library(tidyverse)
rm(list = ls())
raw <- readRDS("data/raw_dat.rds")
spill_vol <- readRDS("data/spillover_vols.rds")
spill_rtn <- readRDS("data/spillover_rtns.rds")
static <- readRDS("data/static.rds")
refinitiv<-readRDS("data/refinitiv.rds")
docxtractr::read_docx(path = "spilloverV3.docx")-> spill_doc
spill_doc %>% docxtractr::docx_extract_all_tbls() %>% {.[[29]]}->sample_dat
refinitiv %>% 
  mutate(Our_Study=if_else(RIC %in% sample_dat$Identifier.RIC,"This Study","Rest of A&D industry")) |>
  group_by(Our_Study) |>
  summarise(sum=sum(MktCap,na.rm = T)) -> sums
round(100*sums$sum[2]/sum(sums$sum))

spill_rtn$to50 %>% {unique(.$Stock)}->stock_names
stock_names<-str_remove_all(stock_names,"_") 
raw$prices[-1,] %>% 
  map_df(as.numeric) %>%
  mutate(Date=as.Date(Name,origin = "1899-12-30")) %>%
  select(-Name) %>%
  pivot_longer(!Date,values_to = "price",names_to = "stock") %>%
  filter(Date>=dmy("23-08-2010")) %>%
  mutate(stock_match_name=str_remove_all(stock,"\\'|\\(|\\)|\\.|-"),
         stock_match_name=str_remove_all(stock_match_name," "),
         stock=str_to_title(stock)) %>%
  filter(stock_match_name %in% stock_names) %>%
  arrange(stock,Date) %>%
  group_by(stock) %>%
  mutate(return=log(price/lag(price)),
         volatility=return^2) -> dat

country_of_origin<-tibble(stock=unique(dat$stock),country=c("China","France","China","China","UK","US","France","US","US","US","US","US","Germany","US","US","UK","France","Singapore","US","France","US"))
dat %>% left_join(country_of_origin,by="stock")->dat


shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

# Fig A1
textsize=10
dat %>%
  ggplot(aes(x=Date,y=price,colour=country)) + geom_line() + 
  facet_wrap(~stock,scales = "free_y") +
  guides(colour = guide_legend(title.position = "top",
                               label.position = "bottom",
                               nrow = 1)) +
  theme(text = element_text(size=textsize),
        axis.text.x = element_text(angle = 45)) +
  labs(x="",title="Figure A1: Prices series highlighted by country of incorporation") -> p
shift_legend(p)-> p
png("plots/figA1.png",height = 8, width = 11,units = "in",res=300)
grid.draw(p)
dev.off()

# Fig A2 
dat %>%
  ggplot(aes(x=Date,y=return,colour=country)) + geom_line(linewidth=0.1) + 
  facet_wrap(~stock) +
  guides(colour = guide_legend(title.position = "top",
                               label.position = "bottom",
                               nrow = 1)) +
  theme(text = element_text(size=textsize),
        axis.text.x = element_text(angle = 45)) +
  labs(x="",title="Figure A2: log-return series highlighted by country of incorporation") -> p
shift_legend(p)-> p
png("plots/figA2.png",height = 8.5, width = 11,units = "in",res = 300)
grid.draw(p)
dev.off()


# Fig A3
dat %>%
  ggplot(aes(x=Date,y=volatility,colour=country)) + geom_line(linewidth=0.4) + 
  facet_wrap(~stock) +
  guides(colour = guide_legend(title.position = "top",
                               label.position = "bottom",
                               nrow = 1)) +
  theme(text = element_text(size=textsize),
        axis.text.x = element_text(angle = 45)) +
  labs(x="",title="Figure A3: Volatility series highlighted by country of incorporation")  -> p
shift_legend(p)->p
png("plots/figA3.png",height = 8.5, width = 11,units = "in",res = 300)
grid.draw(p)
dev.off()
