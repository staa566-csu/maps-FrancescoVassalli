library(dplyr)
library(tigris)
library(plotly)
library(rjson)
#this data is from www.zillow.com/Data
two_bedroom_data<-read.csv("/Users/francesco/Downloads/Viz/City_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
rent_data<-read.csv("/Users/francesco/Downloads/Viz/City_zori_sm_month.csv")
taxes<-read.csv("/Users/francesco/Downloads/Viz/Taxes.csv")
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
taxes$State = state.abb[match(taxes$State,state.name)]
capRate<-rep(NA,nrow(rent_data))
fips<-rep(NA,nrow(rent_data))

for (i in 1:nrow(two_bedroom_data)) {
    fips_str = lookup_code(two_bedroom_data$State[i],two_bedroom_data$CountyName[i])
    fips_state = substr(fips_str,unlist(gregexpr('\'', fips_str))[1]+1,unlist(gregexpr('\'', fips_str))[2]-1)
    fips_county = substr(fips_str,nchar(fips_str)-4,nchar(fips_str)-2)
    fips_dense = paste(fips_state,fips_county,sep="")
    two_bedroom_data$Metro[i] = substr(fips_dense,1,3)
}
lookup_code("CA","Los Angeles")
for (i in 1:nrow(rent_data)) {
  fips_str = lookup_code(rent_data$State[i],rent_data$CountyName[i])
  fips_state = substr(fips_str,unlist(gregexpr('\'', fips_str))[1]+1,unlist(gregexpr('\'', fips_str))[2]-1)
  fips_county = substr(fips_str,nchar(fips_str)-4,nchar(fips_str)-2)
  fips_dense = paste(fips_state,fips_county,sep="")
  fips[i] = fips_dense
  rent = rent_data[i,ncol(rent_data)]
  if(rent_data$RegionID[i] %in% two_bedroom_data$RegionID){
    own = two_bedroom_data[two_bedroom_data$RegionID==rent_data$RegionID[i],ncol(two_bedroom_data)]
    
  }
  else {
    df.metro = two_bedroom_data[two_bedroom_data$Metro==substr(fips_dense,1,3),]
    if(nrow(df.metro)>=1){
      own = df.metro[1,ncol(df.metro)]
    }
    else{
      own = mean(two_bedroom_data[two_bedroom_data$State==rent_data$State[i],ncol(two_bedroom_data)])
    }
  }
  capRate[i] = rent*12 / own
}


g <- list(
  
  scope = 'usa',
  
  projection = list(type = 'albers usa'),
  
  showlakes = TRUE,
  
  lakecolor = toRGB('white')
  
)

fig <- plot_ly()

fig <- fig %>% add_trace(
  
  type="choropleth",
  
  geojson=counties,
  
  locations=fips,
  
  z=capRate,
  
  colorscale="Viridis",
  
  zmin=0,
  
  zmax=.2,
  
  marker=list(line=list(
    
    width=0)
    
  )
  
)

#fig <- fig %>% colorbar(title = "")

fig <- fig %>% layout(
  
  title = "August 2022 2bd Unadjusted Cap Rates"
  
)


fig <- fig %>% layout(
  
  geo = g
  
)


fig
sum(is.na(capRate))/length(capRate)
length(capRate)

