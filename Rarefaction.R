
library(googlesheets4)
library(tidyverse)
library(vegan)
#library(sf)
#library(piggyback)
#library(units)
#library(stringr)
library(doParallel)
#library(purrr)
library(googledrive)
library(readxl)
library(exifr)

######### User settings

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
if (Sys.getenv("USER") == "adam") {gmail = "adamw@buffalo.edu"}

# Define the URL or key of the Google Sheets
fname="data/Rarefaction_FA.xlsx"
sheet_url="https://docs.google.com/spreadsheets/d/13kJ7O4PbH6bM-Y89wGf_tbxs-R1THXlb/edit?usp=sharing&ouid=100268570982256677112&rtpof=true&sd=true"

drive_download(sheet_url, path = fname, overwrite = TRUE)


# Get sheet names
sheets = excel_sheets(fname)

# Read sheets and do a little cleaning for irregularites

data=foreach(i=sheets,.combine=bind_rows) %do% {
  read_xlsx(fname,i) %>% 
#    select(modify_date) |>
    mutate(
      fdate = gsub(":", "-", substring(modify_date, 1, 10)), # get just date part and replace : with -
      fdate2 = paste(fdate, substring(modify_date, 12)),       # Combine the formatted date part with the time part
      datetime = as_datetime(fdate2),       # Convert to datetime and extract the date using as_date
      date = date(datetime),       # extract just date
      time = hms(format(datetime, "%H:%M:%S")),  # get just time part from the datetime
      plot_number=as.character(plot_number),
      photo_type=case_when(
        grepl("plot|S|N|W|E|start|rarefaction|end|stop",fixed=F,genus) ~ "plot",
        T ~ "species"
        ),
      ) |>
    select(-fdate,-fdate2) |>  # drop temporary columns
    select(-exposure_time) #drop exposure time due to irregularity and not needed.
    } 

# explore rarefaction curves

data2 <- data |>
  group_by(plot_number, rarefaction_replicate) |>
  filter(rarefaction_photo==1) |>
  mutate(
    rarefaction_time=datetime-min(datetime),
    rarefaction_min=as.numeric(rarefaction_time)/60,
    count=row_number()-1,
    plot_number_replicate=paste(plot_number,rarefaction_replicate,sep=".")
    ) |>
  select(plot_number,plot_number_replicate,genus, species, datetime,time,rarefaction_time,rarefaction_min,rarefaction_replicate,count)

data2 |>
  filter(!is.na(plot_number)) |>
  ggplot(aes(x=rarefaction_min,y=count,group=plot_number_replicate, color=plot_number))+
  geom_line()+
  labs(y="Unique Species",x="Minutes")

all_bins=expand_grid(plot_number_replicate=unique(data2$plot_number_replicate),time=seq(0,10*60,30))

data_binned <- data |>
  filter(rarefaction_photo==1) |> # keep only rarefaction rows
  select(plot_number,rarefaction_replicate,datetime) |> #simplify the table
  group_by(plot_number, rarefaction_replicate) |>
  mutate(
    time=as.numeric(ceiling_date(datetime, "30 seconds")-ceiling_date(min(datetime), "30 seconds")), #create 30s bins
    plot_number_replicate=paste(plot_number,rarefaction_replicate,sep="."),
    n=1
    ) |>
  right_join(all_bins) |>
  group_by(plot_number_replicate,time) |>
  summarize(n=sum(n,na.rm=T),csum=nrow(n)) |>
  group_by(plot_number_replicate) |>
  arrange(plot_number_replicate,time) |>
  mutate(count=cumsum(n))

data_matrix <- data_binned |>
  select(plot_number_replicate,time,count) |>
  tidyr::spread(time,count)


data_binned |>
  ggplot(aes(x=time,y=count,group=plot_number_replicate))+
  geom_line()+
  labs(y="Unique Species",x="Minutes")


# Fit a species-area curve using the power model
d1=data_binned |> filter(!plot_number_replicate=="198.0.1")

# Step 2: Fit the linear model for each group using nest and purrr
models <- data2 %>%
  filter(!plot_number_replicate%in%c("198.0.1","261.0.1")) %>% 
  group_map(~ broom::tidy(nls(count ~ c * rarefaction_min^z, start = list(c = 0.5, z = 0.5),data = .x)))

# Step 3: Use broom::tidy() to extract model summaries for each group
model_summaries <- models %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)


model <- 

d1$pred=predict(model)
# Summary of the model
summary(model)

ggplot(d1,aes(x=time,y=count))+
  geom_point()+
  geom_line(data=d1,aes(y=pred),col="red")+
  scale_x_continuous(lim=c(0,700))


# Sprex


# richness_estimates <- t(estimateR(as.matrix(data_matrix[,-1]))) |>
#   bind_cols(data.frame(plot=data_matrix[,1])) |>
#   left_join(summarize(data_binned,n_species=max(count)))
# 
# ggplot(richness_estimates,aes(y=S.chao1,x=n_species))+
#   geom_point()+
#   geom_errorbar(aes(ymin=S.chao1-se.chao1,ymax=S.chao1+se.chao1))+
#   geom_abline()
# 


# d1=discovery.curve(c(unlist(data_matrix[2,-1])),f0.func = Swor1,N=10,max.x = 1000,n.pts = 120)
# plot(d1)
# 
# ggplot(d1$rarefact.line,aes(x=x,y=y))+
#   geom_line()+
#   geom_line(data=data_binned,aes(x=time,y=count,group=plot_number_replicate))


# Check for times longer than 15 minutes to identify problem plots
data2 |>
  filter(rarefaction_min>15) |>
  ungroup() |>
  select(plot_number) |>
  distinct ()
 



# Make new filenames for photos

#data2= data |>
#  mutate(
#    newfile=paste0(plot_number,"_",genus,"_",species,"_",rarefaction,".jpeg")
#  )


