#--------------------------------------------------------------------------------------------------
# Paletten Zeitarmutsprojekt
#--------------------------------------------------------------------------------------------------

# Sand und dunkles blau(grün) ---------------------------------------------------------------------

c("#cbbd93", "#08435e")


# Dunkler Sand und mittleres blau -----------------------------------------------------------------

c("#b98667", "#67a6cf")


# 8 verschiedene Farben ---------------------------------------------------------------------------
# hellgrau, dunkelgrün, dunkelsand, sand, dunkelbalu, hellblau, hellgrün, dunkelgrün

c("#99989D", "#5e7540", "#d7b772", "#edd892", "#5a78c8", "#8d9af1", "#b3d58e", "#4d4e4d")


# plot of the month -------------------------------------------------------------------------------
c("#6C5B7B","#C06C84","#F67280","#F8B195")

#--------------------------------------------------------------------------------------------------
# Farben österreichische Parteien
#--------------------------------------------------------------------------------------------------

vp <- "#63c3d0"
sp <- "#ff0000"
fp <- "#0066FF"
gr <- "#92d050"
ne <- "#e84188"
kp <- "#a20000"

#--------------------------------------------------------------------------------------------------
# Paletten aus älteren Projekten 
#--------------------------------------------------------------------------------------------------

# weiß bis rot ------------------------------------------------------------------------------------

col0 <- "#cccccc"		# grau
col1 <- "#FFFFFF"		# weiß	
col2 <- "#ffff99"		# hellgelb
col3 <- "#ffff4d"		# gelb
col4 <- "#fab700"		# dunkelgelb
col5 <- "#fa8c00"		# orange
col6 <- "#e05312"		# dunkelorange
col7 <- "#c71e1d"		# rot


# Grün nach rot -----------------------------------------------------------------------------------
# blöd für farbenblinde Personen, also eher nicht verwenden für Sachen, die veröffentlicht werden

col0 <- "#2e7d61"    # dunkelgrün	  #4ebd5d  #2e7d61
col1 <- "#43b68d"    # mittelgrün 	#83cd79
col2 <- "#ABDDA4"    # hellgrün 		#ABDDA4
col3 <- "#FEE08B"    # gelb 		    #FEE08B
col4 <- "#FDAE61"    # hellorange 	#FDAE61	 #F46D43
col5 <- "#D53E4F"    # rot			    #D53E4F
col6 <- "#9E0142"    # dunkelrot		#9E0142

col0 <- "#ABDDA4"
col1 <- "#FEE08B"
col2 <- "#FDAE61"
col3 <- "#F46D43"
col4 <- "#D53E4F"
col5 <- "#9E0142"


# Blau zu Rot mit Spectral ------------------------------------------------------------------------
# das geht auch mit anderen Paletten und mit einer anderen Anzahl von diskreten values dann (hier sinds halt 9)
# auch blöd für Farbenblinde

cols <- c(RColorBrewer::brewer.pal(9, "Spectral")[1:9])
RColorBrewer::display.brewer.all()

# Scientific palettes: ggsci ----------------------------------------------------------------------
# https://nanx.me/ggsci/

# Paletteer gallery -------------------------------------------------------------------------------
# https://pmassicotte.github.io/paletteer_gallery/#sequential

# Wesanderson palettes ----------------------------------------------------------------------------
names(wesanderson::wes_palettes)
wesanderson::wes_palettes

# viridis palettes --------------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# viridis (!)
# magma (!)(white, orange, red, violet, black) - plasma - inferno (similar to magma)
# cividis (!)(yellow, grey, blue)
# mako (alternative to blues) - rocket (reds to black) - turbo (alternative to rainbow)

# Orange - Rot - Lila -----------------------------------------------------------------------------
# Macht sich auch gut mit weißen Linien (statt schwarzen)

col1a <- "#FA8C00"
col1b <- "#E3420C"
col1c <- "#C71E1D"
col1d <- "#97193C"
col1e <- "#5F0E4F"

# mit sechs abstufungen:
col1 <- "#fab700"
col2 <- "#fa8c00"
col3 <- "#e05312"
col4 <- "#c71e1d"
col5 <- "#ad1a55"
col6 <- "#5f0e4f"

# mit gelb zusätzlich:
col1 <- "#fcc24b"
col2 <- "#FA8C00"
col3 <- "#E3420C"
col4 <- "#C71E1D"
col5 <- "#97193C"
col6 <- "#5F0E4F"

# für 7 Abstufungen:
col1 <- "#fce372"
col2 <- "#fab700"
col3 <- "#fa8c00"
col4 <- "#e05312"
col5 <- "#c71e1d"
col6 <- "#ad1a55"
col7 <- "#5f0e4f"


# Dunkelblau bis Hellblau & hellrot bis Dunkelrot -------------------------------------------------

col1 <- "#37375e"
col2 <- "#4679a3"
col3 <- "#aacfdd"
col4 <- "#fe875d"
col5 <- "#d64238"
col6 <- "#a10b0b"


# gelb und schnitzelbraun -------------------------------------------------------------------------

col1 <- "#ffc40c"
col2 <- "#eeaa0d"
col3 <- "#de900f"
col4 <- "#cd7710"
col5 <- "#bd5d12"
col6 <- "#ac4313"
col7 <- "#702b0b"