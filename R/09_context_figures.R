
library(geobr)
library(maptiles)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggforce)
library(edgebundle) # https://github.com/schochastics/edgebundle
library(sfheaders)
library(patchwork)
library(sf)
library(data.table)
library(aopdata)
library(h3jsr)
library(dplyr)

# disable scientific notation
options(scipen = 99)





###### Read spatial context data ------------------

# download Rio state 
state <- read_state(code_state = 33)

# download Rio vector data
muni <- read_municipality(code_muni = 3304557)
muni_bbox <- st_bbox(muni)

    # download tiles and compose raster (SpatRaster)
    base_tile12 <- get_tiles(muni,
                             crop = TRUE,
                             provider = "CartoDB.DarkMatterNoLabels", zoom = 12)

    base_tile10 <- get_tiles(muni,
                           crop = TRUE,
                           provider = "CartoDB.DarkMatterNoLabels", zoom = 10)
    # display map
    # plot_tiles(base_tile12)
    # plot_tiles(base_tile10)

    # convert tile para df
    base_df <- terra::as.data.frame(base_tile10, xy = TRUE)  %>%
      mutate(hex = rgb(red, green, blue, maxColorValue = 255))

    # plot
    ggplot() +
      geom_tile(data = base_df, aes(x, y, fill = hex), color=NA) +
      scale_fill_identity() +
      geom_sf(data=muni , fill=NA, color='gray90', size=.5)


  # # Hex spatial grid of Rio
  # hex_sf <- aopdata::read_landuse(city = 'Rio de Janeiro', geometry = T)
  # head(hex_sf)
  # 



base_map <- ggplot() +
      geom_sf(data=state , fill= 'black') +
      geom_sf(data=muni, color='gray40', fill=NA, size=.5, alpha=.5) +
      coord_sf(xlim = c(muni_bbox[[1]], muni_bbox[[3]]), ylim = c(muni_bbox[[2]], muni_bbox[[4]])) +
      theme_void() +
      theme(panel.background = element_rect(fill='gray10', colour=NA))
   
coords_sf_rio <- coord_sf(xlim = c(muni_bbox[[1]], muni_bbox[[3]]), ylim = c(muni_bbox[[2]], muni_bbox[[4]]))


###### Read Uber data  ------------------

# read data
df_od <- fread('../data/data-raw/rio_origin_destination.csv')
df_pick <- fread('../data/data-raw/rio_pickups.csv')
df_drop <- fread('../data/data-raw/rio_dropoffs.csv')

# filter
df_pick <- df_pick[ date_block_2019 == "mar8_dec20" &
                    weekday_weekend == "weekday" &
                    time_block == "morning_peak"]
  
df_drop <- df_drop[ date_block_2019 == "mar8_dec20" &
                    weekday_weekend == "weekday" &
                    time_block == "morning_peak"]

df_od <- df_od[ date_block_2019 == "mar8_dec20" &
                weekday_weekend == "weekday" &
                time_block == "morning_peak"]


### get spatial H3

# h3 polygons
grid8 <- readRDS( tar_read(grid_res_8) )
grid9 <- readRDS( tar_read(grid_res_9) )
grid <- rbind(grid8, grid9)


# h3 centroids coordinates
h3_od_point <- h3jsr::h3_to_point(h3_address = grid8$id_hex, simple = F)
h3_od_point <- sfheaders::sf_to_df(h3_od_point, fill = T)
setDT(h3_od_point)


## merge
df_pick <- dplyr::left_join(df_pick, grid, by=c('hex_addr'='id_hex'))
df_drop <- dplyr::left_join(df_drop, grid, by=c('hex_addr'='id_hex'))

df_pick <- st_sf(df_pick)
df_drop <- st_sf(df_drop)


# KEEP ONLY DATA POINTS IN RIO

df_pick <- subset(df_pick, hex_addr %in% grid$id_hex)
df_drop <- subset(df_drop, hex_addr %in% grid$id_hex)
df_od   <- subset(df_od, pickup_hex8 %in%  grid8$id_hex)
df_od   <- subset(df_od, dropoff_hex8 %in% grid8$id_hex)





######  Map pop densuty  ------------------

fig_pop_density <- 
      base_map +
      geom_sf(data=subset(grid9, pop_total>0), aes(fill=pop_total), color=NA, alpha=.9) +
      scale_fill_viridis_c(option = 'viridis') +
      coords_sf_rio


ggsave(fig_pop_density, 
       file='../figures/context/fig_pop_density.png', 
       dpi=300,
       width = 20,
       height = 10,
       units = "cm",
       bg= '#FFFFFF')



######  Map pop densuty  ------------------

fig_pop_income <- 
  base_map +
  geom_sf(data=subset(grid9, pop_total>0), aes(fill=as.factor(decil)), color=NA, alpha=.9) +
  scale_fill_viridis_d(option = 'cividis') +
  coords_sf_rio


ggsave(fig_pop_income, 
       file='../figures/context/fig_pop_income.png', 
       dpi=300,
       width = 20,
       height = 10,
       units = "cm",
       bg= '#FFFFFF')




######  Map pick ups  ------------------

fig_pick <- base_map +
            geom_sf(data=df_pick, aes(fill=num_pickups), color=NA,  alpha=.9) +
            scale_fill_viridis_c(option = 'inferno', trans = "log10") +
            coords_sf_rio


ggsave(fig_pick, 
       file='../figures/context/fig_pick.png', 
       dpi=300,
       width = 20,
       height = 10,
       units = "cm",
       bg= '#FFFFFF')

######  Map drop offs  ------------------

fig_drop <- base_map +
            geom_sf(data=df_drop, aes(fill= num_dropoffs), color=NA, alpha=.9) +
            scale_fill_viridis_c(option = 'inferno', trans = "log10") +
            coords_sf_rio

ggsave(fig_drop, 
       file='../figures/context/fig_drop.png', 
       dpi=300,
       width = 20,
       height = 10,
       units = "cm",
       bg= '#FFFFFF')



######  Map origin-destination flows  ------------------

# add income info
setDT(grid8)
df_od[grid8, on=c('pickup_hex8'='id_hex'),
      c('quintil', 'decil') := list(i.quintil, i.decil)]


# add coordinates to od matrix
df_od[h3_od_point, on=c('pickup_hex8'='h3_address'),
      c('pickup_x', 'pickup_y') := list(i.x, i.y)]

df_od[h3_od_point, on=c('dropoff_hex8'='h3_address'),
      c('dropoff_x', 'dropoff_y') := list(i.x, i.y)]





###### build igraph network   ------------------

df_od[, date_block_2019 := NULL]
g <- igraph::graph_from_data_frame(d = df_od, 
                                   vertices = h3_od_point[, .(h3_address,x,y)],
                                   directed = T)

g_poor <- igraph::graph_from_data_frame(d = subset(df_od, decil < 5), 
                                         vertices = h3_od_point[, .(h3_address,x,y)],
                                         directed = T)

g_rich <- igraph::graph_from_data_frame(d = subset(df_od, decil >8), 
                                         vertices = h3_od_point[, .(h3_address,x,y)],
                                         directed = T)




# coordinates of vertices
xy <- cbind(V(g)$x, V(g)$y)


### Edge Bundling

# Edge-Path Bundling
pbundle <- edge_bundle_path(g, xy, 
                            max_distortion = 12,
                            weight_fac =  4,
                            segments = 20)

pbundle_rich <- edge_bundle_path(g_rich , xy, 
                            max_distortion = 12,
                            weight_fac =  4,
                            segments = 20)

pbundle_poor <- edge_bundle_path(g_poor, xy, 
                            max_distortion = 12,
                            weight_fac =  4,
                            segments = 20)

2+2

# fbundle <- edge_bundle_force(g, xy, compatibility_threshold = 0.6)
# sbundle <- edge_bundle_stub(g, xy)
# hbundle <- edge_bundle_hammer(g, xy, bw = 0.7, decay = 0.5)

2+2


setDT(pbundle)[, cat := 'all']
setDT(pbundle_rich)[, cat := 'rich']
setDT(pbundle_poor)[, cat := 'poor']

df_bundle <- rbind(pbundle, pbundle_rich, pbundle_poor)

### Figure -----------------
fig_od_all <- ggplot() +
          geom_sf(data=muni , fill='gray10', color=NA) +
          geom_path(data = pbundle, aes(x, y, group = group),
                    col = "#9d0191", size = 0.009, alpha=.05) +
           geom_path(data = pbundle, aes(x, y, group = group),
                     col = "white", size = 0.00009, alpha=.01) +
          theme_classic() +
          theme(axis.line=element_blank(),
                axis.text=element_blank(),
                axis.ticks=element_blank(),
                axis.title=element_blank())

fig_od_rich <- ggplot() +
                geom_sf(data=muni , fill='gray10', color=NA) +
                geom_path(data = pbundle_rich, aes(x, y, group = group),
                          col = "#9d0191", size = 0.009, alpha=.05) +
                geom_path(data = pbundle_rich, aes(x, y, group = group),
                          col = "white", size = 0.00009, alpha=.01) +
                theme_classic() +
                theme(axis.line=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title=element_blank())

fig_od_poor <- ggplot() +
                geom_sf(data=muni , fill='gray10', color=NA) +
                geom_path(data = pbundle_poor, aes(x, y, group = group),
                          col = "#9d0191", size = 0.009, alpha=.05) +
                geom_path(data = pbundle_poor, aes(x, y, group = group),
                          col = "white", size = 0.00009, alpha=.01) +
                theme_classic() +
                theme(axis.line=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title=element_blank())

ggsave(fig_od_all, 
       file='../figures/context/od_edges_all_05_01.png', 
       dpi=200,
       width = 15,
       height = 13,
       units = "cm",
       bg= '#FFFFFF')

ggsave(fig_od_rich, 
       file='../figures/context/od_edges_rich_05_01.png', 
       dpi=200,
       width = 15,
       height = 13,
       units = "cm",
       bg= '#FFFFFF')

ggsave(fig_od_poor, 
       file='../figures/context/od_edges_poor_05_01.png', 
       dpi=200,
       width = 15,
       height = 13,
       units = "cm",
       bg= '#FFFFFF')





fig_od <- ggplot() +
  geom_tile(data = base_df, aes(x, y, fill = hex), color=NA) +
  scale_fill_identity() +
  geom_sf(data=muni , color='gray50', fill=NA) +
  geom_path(data = df_bundle, aes(x, y, group = group),
            col = "#019d9d", size = 0.009, alpha=.05) + # verde 019d9d ROSA 9d0191
  geom_path(data = df_bundle, aes(x, y, group = group),
            col = "white", size = 0.00009, alpha=.01) +
  facet_wrap(~cat, nrow = 3) +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

ggsave(fig_od, 
       file='../figures/context/od_edges_05_01_tile_verde.png', 
       dpi=200,
       width = 15,
       height = 23,
       units = "cm",
       bg= '#FFFFFF')
