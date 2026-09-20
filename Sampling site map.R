#### Sampling site map for paper

library(ggplot2)
library(sf)
library(ggmapcn)
library(patchwork)
library(grid)

#--------------------------------------------------
# 1. Sampling site coordinates
#--------------------------------------------------

site_lon <- 116 + 42/60   # 116°42′E = 116.7000
site_lat <- 43 + 38/60    # 43°38′N = 43.6333

site_df <- data.frame(
  site = "Sampling site",
  lon  = site_lon,
  lat  = site_lat
)

site_sf <- st_as_sf(
  site_df,
  coords = c("lon", "lat"),
  crs = 4326
)


#--------------------------------------------------
# 2. Map projection
#    Albers Equal-Area projection suitable for China
#--------------------------------------------------

crs_cn <- paste0(
  "+proj=aea ",
  "+lat_1=25 +lat_2=47 ",
  "+lat_0=0 +lon_0=105 ",
  "+datum=WGS84 +units=m +no_defs"
)


#--------------------------------------------------
# 3. Geographic extent of the main map
#--------------------------------------------------

xlim_main <- c(111, 122)
ylim_main <- c(40, 47)


#--------------------------------------------------
# 4. Extended extent for graticules
#
#--------------------------------------------------

xlim_graticule <- c(109, 124)
ylim_graticule <- c(38, 49)


#--------------------------------------------------
# 5. Main map
#--------------------------------------------------

p_main <- ggplot() +
  
  # Provincial administrative map
  geom_mapcn(
    admin_level = "province",
    crs = crs_cn,
    fill = "grey97",
    color = "grey80",
    linewidth = 0.25
  ) +
  
  # National boundary, coastline, and provincial boundaries
  geom_boundary_cn(
    crs = crs_cn,
    coastline_color = "grey45",
    coastline_size = 0.25,
    mainland_color = "black",
    mainland_size = 0.35,
    province_color = "grey70",
    province_size = 0.25
  ) +
  
  # Longitude and latitude graticules
  #
  annotation_graticule(
    xlim = xlim_graticule,
    ylim = ylim_graticule,
    crs = crs_cn,
    lon_step = 2,
    lat_step = 1,
    line_color = "grey85",
    line_width = 0.25,
    line_type = "dashed",
    label_color = NA
  ) +
  
  # Sampling site
  geom_loc(
    data = site_df,
    lon = "lon",
    lat = "lat",
    crs = crs_cn,
    shape = 21,
    size = 3.8,
    stroke = 0.7,
    fill = "#D73027",
    color = "black"
  ) +
  
  # Sampling-site label
  geom_sf_text(
    data = st_transform(site_sf, crs_cn),
    aes(label = "Sampling site"),
    nudge_y = 65000,
    size = 3.8,
    family = "",
    fontface = "bold",
    color = "black"
  ) +
  
  # Scale bar
  annotation_scalebar(
    location = "bl",
    style = "bar",
    width_hint = 0.28,
    line_col = "black",
    text_cex = 0.8
  ) +
  
  # North arrow
  annotation_compass(
    location = "tr",
    which_north = "true",
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm"),
    pad_x  = unit(0.45, "cm"),
    pad_y  = unit(0.45, "cm")
  ) +
  
  # Display extent of the main map
  coord_proj(
    crs = crs_cn,
    xlim = xlim_main,
    ylim = ylim_main,
    expand = FALSE
  ) +
  
  # Axis titles
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°N)"
  ) +
  
  # Plot theme
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      linewidth = 0.8,
      fill = NA
    ),
    
    axis.title = element_text(
      size = 11,
      color = "black"
    ),
    
    axis.text = element_text(
      size = 9.5,
      color = "black"
    ),
    
    plot.margin = margin(6, 6, 6, 6),
    
    legend.position = "none"
  )


#--------------------------------------------------
# 6. China location inset
#--------------------------------------------------

p_inset <- ggplot() +
  
  # Provincial map of China
  geom_mapcn(
    admin_level = "province",
    crs = crs_cn,
    fill = "white",
    color = "grey70",
    linewidth = 0.2
  ) +
  
  # National boundary and coastline
  geom_boundary_cn(
    crs = crs_cn,
    coastline_color = "grey45",
    coastline_size = 0.22,
    mainland_color = "black",
    mainland_size = 0.35,
    province_color = "grey75",
    province_size = 0.18
  ) +
  
  # Sampling site in the inset
  geom_loc(
    data = site_df,
    lon = "lon",
    lat = "lat",
    crs = crs_cn,
    shape = 21,
    size = 2.4,
    stroke = 0.5,
    fill = "#D73027",
    color = "black"
  ) +
  
  # Geographic extent of the inset
  coord_proj(
    crs = crs_cn,
    xlim = c(73, 136),
    ylim = c(-5, 54),
    expand = TRUE
  ) +
  
  # Remove axes, coordinate labels, and grid lines
  theme_void() +
  
  theme(
    panel.border = element_rect(
      color = "black",
      linewidth = 0.7,
      fill = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(2, 2, 2, 2)
  )


#--------------------------------------------------
# 7. Combine the main map and the inset
#--------------------------------------------------

final_plot <- p_main +
  inset_element(
    p_inset,
    left   = 0.02,
    bottom = 0.60,
    right  = 0.33,
    top    = 0.98,
    align_to = "panel"
  )


#--------------------------------------------------
# 8. Display the final figure
#--------------------------------------------------

final_plot


#--------------------------------------------------
# 9. Export as a high-resolution TIFF file
#--------------------------------------------------

ggsave(
  filename = "sampling_site_map_ecology.tiff",
  plot = final_plot,
  width = 180,
  height = 140,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)