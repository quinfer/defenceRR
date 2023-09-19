# set up
library(tidyverse)
library(flextable)
library(lubridate)
library(stringr)
library(datawizard)
library(igraph)
library(GGally)
library(network)
library(sna)
library(RColorBrewer)
library(docxtractr)
library(corrr)
raw <- readRDS("data/raw_dat.rds")
spill_vol <- readRDS("data/spillover_vols.rds")
spill_rtn <- readRDS("data/spillover_rtns.rds")
static <- readRDS("data/static.rds")
width <-11
height <- 8
dpi <- 300
maxsize<-15
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

country_of_origin<-tibble(stock=unique(dat$stock),country=
                            c("China","France","China","China","UK","US",
                              "France","US","US","US","US","US",
                              "Germany","US","US","UK","France",
                              "Singapore","US","France","US"))
dat %>% left_join(country_of_origin,by="stock")->dat
# fig-rtn50
static$rtn50 %>% 
  filter(!(From %in% c("Net","To Others","Including Own","From Others"))) %>%
  filter(!(To %in% c("Net","To Others","Including Own","From Others"))) %>%
  drop_na()-> links
links$spillover <- as.numeric(links$spillover)
static$rtn50 %>% 
  filter(From =="Including Own") %>%
  rename(name=To,spillover_including_own=spillover) %>%
  select(!From) %>%
  left_join(
    static$rtn50 %>% 
      filter(To =="From Others") %>%
      rename(name=From,spillover_from_others=spillover) %>%
      select(!To),by="name") %>%
  mutate(spillover=as.numeric(spillover_including_own)+as.numeric(spillover_from_others),
         spillover=sqrt(spillover)) %>%
  drop_na()-> nodes
docx<-read_docx("draft.docx")
docx_extract_all_tbls(docx)->all_tbls
all_tbls[[11]] %>% assign_colnames(row=1) -> info
bind_cols(
  nodes %>% arrange(name),
  info %>% arrange(`Company Name`)
) %>%
  mutate(`Company Name`=str_remove_all(`Company Name`," Corp| Co| SE| Inc| PLC| SA| Ltd| AG"))->nodes
nodes$name <- nodes$`Company Name`
#nodes$name[15]<-"Raytheon"
# nodes %>%
#   mutate(size=rank(spillover)) -> nodes
links %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("From"="name")) %>%
  mutate(From=`Company Name`) %>%
  select(!`Company Name`) %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("To"="name")) %>%
  mutate(To=`Company Name`) %>%
  select(!`Company Name`) %>%
  rename(weight=spillover) %>%
  mutate(weight=weight/10)-> links
# links %>% filter
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T) 
# Generate colors based on media type:
#colrs <- c("gray50", "tomato", "gold", "red","brown","green")
# V(net)$color <- colrs[V(net)$Country]
# V(net)$size <- V(net)$spillover*0.01
# E(net)$width <- E(net)$spillover
ggnet2(net,label =F ,size="spillover", edge.size = "weight",
       edge.alpha = 0.2, color = "Country", color.palette ="Set2",max_size = maxsize) +
  guides(color = FALSE, size = FALSE)

ggsave("plots/fig-rtn50.png",width = width,height = height,dpi = dpi)

static$rtn95 %>% 
  filter(!(From %in% c("Net","To Others","Including Own","From Others"))) %>%
  filter(!(To %in% c("Net","To Others","Including Own","From Others"))) %>%
  drop_na()-> links
links$spillover <- as.numeric(links$spillover)
static$rtn95 %>% 
  filter(From =="Including Own") %>%
  rename(name=To,spillover_including_own=spillover) %>%
  select(!From) %>%
  left_join(
    static$rtn95 %>% 
      filter(To =="From Others") %>%
      rename(name=From,spillover_from_others=spillover) %>%
      select(!To),by="name") %>%
  mutate(spillover=as.numeric(spillover_including_own)+as.numeric(spillover_from_others),
         spillover=sqrt(spillover)) %>%
  drop_na()-> nodes
docx<-read_docx("draft.docx")
docx_extract_all(docx)->all_tbls
all_tbls[[11]] %>% assign_colnames(row=1) -> info
bind_cols(
  nodes %>% arrange(name),
  info %>% arrange(`Company Name`)
) %>%
  mutate(`Company Name`=str_remove_all(`Company Name`," Corp| Co| SE| Inc| PLC| SA| Ltd| AG"))->nodes
nodes$name <- nodes$`Company Name`
# nodes %>%
#   mutate(size=rank(spillover)) -> nodes
links %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("From"="name")) %>%
  mutate(From=`Company Name`) %>%
  select(!`Company Name`) %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("To"="name")) %>%
  mutate(To=`Company Name`) %>%
  select(!`Company Name`) %>%
  rename(weight=spillover) %>%
  mutate(weight=weight/10)-> links
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)
ggnet2(net,label =F ,size="spillover", edge.size = "weight",
       edge.alpha = 0.2, color = "Country", color.palette ="Set2",max_size = maxsize) +
  guides(color = FALSE, size = FALSE)

ggsave("plots/fig-rtn95.png",width = width,height = height,dpi=dpi)

static$rtn5 %>% 
  filter(!(From %in% c("Net","To Others","Including Own","From Others"))) %>%
  filter(!(To %in% c("Net","To Others","Including Own","From Others"))) %>%
  drop_na()-> links
links$spillover <- as.numeric(links$spillover)
static$rtn5 %>% 
  filter(From =="Including Own") %>%
  rename(name=To,spillover_including_own=spillover) %>%
  select(!From) %>%
  left_join(
    static$rtn5 %>% 
      filter(To =="From Others") %>%
      rename(name=From,spillover_from_others=spillover) %>%
      select(!To),by="name") %>%
  mutate(spillover=as.numeric(spillover_including_own)+as.numeric(spillover_from_others)) %>%
  drop_na()-> nodes
docx<-read_docx("draft.docx")
docx_extract_all(docx)->all_tbls
all_tbls[[11]] %>% assign_colnames(row=1) -> info
bind_cols(
  nodes %>% arrange(name),
  info %>% arrange(`Company Name`)
) %>%
  mutate(`Company Name`=str_remove_all(`Company Name`," Corp| Co| SE| Inc| PLC| SA| Ltd| AG"))->nodes
nodes$name <- nodes$`Company Name`
# nodes %>%
#   mutate(size=rank(spillover)) -> nodes
links %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("From"="name")) %>%
  mutate(From=`Company Name`) %>%
  select(!`Company Name`) %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("To"="name")) %>%
  mutate(To=`Company Name`) %>%
  select(!`Company Name`) %>%
  rename(weight=spillover) %>%
  mutate(weight=weight/10)-> links
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)
ggnet2(net,label =F ,size="spillover", edge.size = "weight",
       edge.alpha = 0.2, color = "Country", color.palette ="Set2",
       legend.size = 12, legend.position = "right",max_size = maxsize) +
  scale_size_discrete(guide="none")

ggsave("plots/fig-rtn5.png",width = width,height = height , dpi = dpi)

static$vol50 %>% 
  filter(!(From %in% c("Net","To Others","Including Own","From Others"))) %>%
  filter(!(To %in% c("Net","To Others","Including Own","From Others"))) %>%
  drop_na()-> links
links$spillover <- as.numeric(links$spillover)
static$vol50 %>% 
  filter(From =="Including Own") %>%
  rename(name=To,spillover_including_own=spillover) %>%
  select(!From) %>%
  left_join(
    static$vol50 %>% 
      filter(To =="From Others") %>%
      rename(name=From,spillover_from_others=spillover) %>%
      select(!To),by="name") %>%
  mutate(spillover=as.numeric(spillover_including_own)+as.numeric(spillover_from_others),
         spillover=sqrt(spillover)) %>%
  drop_na()-> nodes
docx<-read_docx("draft.docx")
docx_extract_all(docx)->all_tbls
all_tbls[[11]] %>% assign_colnames(row=1) -> info
bind_cols(
  nodes %>% arrange(name),
  info %>% arrange(`Company Name`)
) %>%
  mutate(`Company Name`=str_remove_all(`Company Name`," Corp| Co| SE| Inc| PLC| SA| Ltd| AG"))->nodes
nodes$name <- nodes$`Company Name`
links %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("From"="name")) %>%
  mutate(From=`Company Name`) %>%
  select(!`Company Name`) %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("To"="name")) %>%
  mutate(To=`Company Name`) %>%
  select(!`Company Name`) %>%
  rename(weight=spillover) %>%
  mutate(weight=weight/10)-> links
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)

ggnet2(net,label =F ,size="spillover", edge.size = "weight",
       edge.alpha = 0.2, color = "Country", color.palette ="Set2",max_size = maxsize) +
  guides(color = FALSE, size = FALSE)

ggsave("plots/fig-vol50.png",width = width,height = height,dpi=dpi)

static$vol95 %>% 
  filter(!(From %in% c("Net","To Others","Including Own","From Others"))) %>%
  filter(!(To %in% c("Net","To Others","Including Own","From Others"))) %>%
  drop_na()-> links
links$spillover <- as.numeric(links$spillover)
static$vol95 %>% 
  filter(From =="Including Own") %>%
  rename(name=To,spillover_including_own=spillover) %>%
  select(!From) %>%
  left_join(
    static$vol95 %>% 
      filter(To =="From Others") %>%
      rename(name=From,spillover_from_others=spillover) %>%
      select(!To),by="name") %>%
  mutate(spillover=as.numeric(spillover_including_own)+as.numeric(spillover_from_others)) %>%
  drop_na()-> nodes
docx<-read_docx("draft.docx")
docx_extract_all(docx)->all_tbls
all_tbls[[11]] %>% assign_colnames(row=1) -> info
bind_cols(
  nodes %>% arrange(name),
  info %>% arrange(`Company Name`)
) %>%
  mutate(`Company Name`=str_remove_all(`Company Name`," Corp| Co| SE| Inc| PLC| SA| Ltd| AG"))->nodes
nodes$name <- nodes$`Company Name`
nodes %>%
  mutate(size=rank(spillover)) -> nodes
links %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("From"="name")) %>%
  mutate(From=`Company Name`) %>%
  select(!`Company Name`) %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("To"="name")) %>%
  mutate(To=`Company Name`) %>%
  select(!`Company Name`) %>%
  rename(weight=spillover) %>%
  mutate(weight=weight/10)-> links
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)
ggnet2(net,label =F,size="spillover", edge.size = "weight",
       edge.alpha = 0.2, color = "Country", color.palette ="Set2",max_size = maxsize) +
  guides(color = FALSE, size = FALSE)

ggsave("plots/fig-vol95.png",width = width,height = height,dpi=dpi)

static$vol5 %>% 
  filter(!(From %in% c("Net","To Others","Including Own","From Others"))) %>%
  filter(!(To %in% c("Net","To Others","Including Own","From Others"))) %>%
  drop_na()-> links
links$spillover <- as.numeric(links$spillover)
static$vol5 %>% 
  filter(From =="Including Own") %>%
  rename(name=To,spillover_including_own=spillover) %>%
  select(!From) %>%
  left_join(
    static$vol5 %>% 
      filter(To =="From Others") %>%
      rename(name=From,spillover_from_others=spillover) %>%
      select(!To),by="name") %>%
  mutate(spillover=as.numeric(spillover_including_own)+as.numeric(spillover_from_others)) %>%
  drop_na()-> nodes
docx<-read_docx("draft.docx")
docx_extract_all(docx)->all_tbls
all_tbls[[11]] %>% assign_colnames(row=1) -> info
bind_cols(
  nodes %>% arrange(name),
  info %>% arrange(`Company Name`)
) %>%
  mutate(`Company Name`=str_remove_all(`Company Name`," Corp| Co| SE| Inc| PLC| SA| Ltd| AG"))->nodes
nodes$name <- nodes$`Company Name`
nodes %>%
  mutate(size=rank(spillover)) -> nodes
links %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("From"="name")) %>%
  mutate(From=`Company Name`) %>%
  select(!`Company Name`) %>%
  left_join(nodes %>% select(name,`Company Name`),
            by=c("To"="name")) %>%
  mutate(To=`Company Name`) %>%
  select(!`Company Name`) %>%
  rename(weight=spillover) %>%
  mutate(weight=weight/10)-> links
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)
ggnet2(net,label =F,size="spillover", edge.size = "weight",
       edge.alpha = 0.2, color = "Country", color.palette ="Set2",
       legend.size = 12, legend.position = "right",max_size = maxsize) +
  scale_size_discrete(guide="none")

ggsave("plots/fig-vol5.png",width = width,height = height,dpi = dpi)

spill_rtn$net50 %>%
  ggplot(aes(Date,Spillover,fill=Country)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=Spillover)) +
  facet_wrap(~name,scales="free_y") +
  theme(legend.position = "top",
        text = element_text(size=9)) +
  scale_fill_brewer(type = "qual",palette=2) +
  guides(fill = guide_legend(nrow = 1))

# Define the dimensions and DPI
width <- 11
height <- 8
dpi <- 300

# Save the plot
ggsave("plots/fig-rtnnet50.png", width = width, height = height, dpi = dpi)

spill_rtn$net5 %>%
  ggplot(aes(Date,Spillover,fill=Country)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=Spillover)) +
  facet_wrap(~name,scales="free_y") +
  theme(legend.position = "top",
        text = element_text(size=9)) +
  scale_fill_brewer(type = "qual",palette=2) +
  guides(fill = guide_legend(nrow = 1))

ggsave("plots/fig-rtnnet5.png",width = width,height = height,dpi=dpi)

spill_rtn$not95 %>%
  ggplot(aes(Date,Spillover,fill=Country)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=Spillover)) +
  facet_wrap(~name, scales="free_y") +
  theme(legend.position = "top",
        text = element_text(size=9)) +
  scale_fill_brewer(type = "qual",palette=2) +
  guides(fill = guide_legend(nrow = 1))

ggsave("plots/fig-rtnnet95.png",width = width,height = height,dpi = dpi)

spill_vol$net50 %>%
  ggplot(aes(Date,Spillover,fill=Country)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=Spillover)) +
  facet_wrap(~name,scales = "free_y") +
  theme(legend.position = "top",
        text = element_text(size=9)) +
  scale_fill_brewer(type = "qual",palette=2) +
  guides(fill = guide_legend(nrow = 1))

ggsave("plots/fig-volnet50.png",width = width,height = height,dpi = dpi)

spill_vol$net05 %>%
  ggplot(aes(Date,Spillover,fill=Country)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=Spillover)) +
  facet_wrap(~name,scales = "free_y") +
  theme(legend.position = "top",
        text = element_text(size=9)) +
  scale_fill_brewer(type = "qual",palette=2) +
  guides(fill = guide_legend(nrow = 1))

ggsave("plots/fig-volnet5.png",width = width,height = height,dpi=dpi)

spill_vol$net95 %>%
  ggplot(aes(Date,Spillover,fill=Country)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=Spillover)) +
  facet_wrap(~name,scales = "free_y") +
  theme(legend.position = "top",
        text = element_text(size=9)) +
  scale_fill_brewer(type = "qual",palette=2) +
  guides(fill = guide_legend(nrow = 1))

ggsave("plots/fig-volnet95.png",width = width,height = height, dpi=dpi)

spill_rtn$TCI %>% select(Date,Stock,Spillover) %>% 
  pivot_wider(names_from = "Stock",values_from = "Spillover") %>%
  mutate(RTD=`TCI 95`- `TCI 05`) %>%
  pivot_longer(!Date,names_to = "Stock",values_to = "Spillover")%>%
  mutate(Stock=factor(Stock,levels = c("TCI 50","TCI 95","TCI 05","RTD")))->spill_rtn$TCI
spill_vol$TCI %>% select(Date,Stock,Spillover) %>%  pivot_wider(names_from = "Stock",values_from = "Spillover") %>%
  mutate(RTD=`TCI 95`- `TCI 05`) %>%
  pivot_longer(!Date,names_to = "Stock",values_to = "Spillover") %>%
  mutate(Stock=factor(Stock,levels = c("TCI 50","TCI 95","TCI 05","RTD"))) -> spill_vol$TCI

spill_rtn$TCI %>% rename(Return=Spillover) %>%
  left_join(spill_vol$TCI %>% rename(Volatility=Spillover),
            by=c("Date","Stock")) -> TCI
list("Russia began annexation of Crimea"="February 20,2014",
     "Start of war in Donbas by pro-Russian activists"= "April 7, 2014",
     "October 2014 flash crash"="Oct 15,2014",
     "Brexit referendum"= "June 23, 2016", 
     "Federal Reserve raises interest rates"= "December 14, 2016", 
     "the United Kingdom invokes article 50 of the Lisbon Treaty"="March 29, 2017", 
     "snap election held in the United Kingdom"="June 8, 2017",
     "Dash for cash crisis in bond market peaks"="March 18, 2020",
     "Russia initiated a special military operation in Donbas"="Feb 24,2022")->imp_dates

labels_dat<-tibble(
  label=letters[1:length(imp_dates)],
  xvalues=parse_date_time(imp_dates,"mdy"),
  description=names(imp_dates))

TCI %>% 
  filter(Stock=="TCI 50") %>%
  select(!Stock) %>%
  pivot_longer(!Date,names_to = "Stock",values_to = 'Spillover') %>%
  ggplot(aes(x=Date,y=Spillover)) + geom_line() + 
  geom_vline(xintercept =labels_dat$xvalues,linetype=4,colour="red") +
  facet_wrap(~Stock,nrow = 4,scales = "free_y") +
  theme(legend.position = 'none') +
  xlab(label = "") +
  geom_text(aes(x=xvalues,y=0,label=label),data = labels_dat,nudge_y = -1)

ggsave("plots/fig-TCI50.png",width = width,height = height, dpi=dpi)

TCI %>% 
  filter(Stock=="TCI 05") %>%
  select(!Stock) %>%
  pivot_longer(!Date,names_to = "Stock",values_to = 'Spillover') %>%
  ggplot(aes(x=Date,y=Spillover)) + geom_line() + 
  geom_vline(xintercept =labels_dat$xvalues,linetype=4,colour="red") +
  facet_wrap(~Stock,nrow = 4,scales = "free_y") +
  theme(legend.position = 'none') +
  xlab(label = "") +
  geom_text(aes(x=xvalues,y=0,label=label),data = labels_dat,nudge_y = -1)

ggsave("plots/fig-TCI5.png",width = width,height = height,dpi=dpi)

TCI %>% 
  filter(Stock=="TCI 95") %>%
  select(!Stock) %>%
  pivot_longer(!Date,names_to = "Stock",values_to = 'Spillover') %>%
  ggplot(aes(x=Date,y=Spillover)) + geom_line() + 
  geom_vline(xintercept =labels_dat$xvalues,linetype=4,colour="red") +
  facet_wrap(~Stock,nrow = 4,scales = "free_y") +
  theme(legend.position = 'none') +
  xlab(label = "") +
  geom_text(aes(x=xvalues,y=0,label=label),data = labels_dat,nudge_y = -1)

ggsave("plots/fig-TCI95.png",width = width,height = height,dpi=dpi)

TCI %>% 
  filter(Stock=="RTD") %>%
  select(!Stock) %>%
  pivot_longer(!Date,names_to = "Stock",values_to = 'Spillover') %>%
  ggplot(aes(x=Date,y=Spillover)) + geom_line() + 
  geom_vline(xintercept =labels_dat$xvalues,linetype=4,colour="red") +
  facet_wrap(~Stock,nrow = 4,scales = "free_y") +
  theme(legend.position = 'none') +
  xlab(label = "") +
  geom_text(aes(x=xvalues,y=0,label=label),data = labels_dat,nudge_y = -1)

ggsave("plots/fig-TCIrtd.png",width = width,height = height,dpi=dpi)

