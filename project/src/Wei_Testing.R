library(raster)
nat_clay_mdc <- metadata_soils('NAT', 'CLY', req_type = 'desc')
nat_clay_mdc
plot(bne_surface_clay)
devtools::install_github("obrl-soil/slga")
