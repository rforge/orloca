# Carga de los datos de las ciudades
data(andalusia)

# Creacion de un nuevo objeto loca.p
o <- loca.p(x=andalusia$x[1:8], y=andalusia$y[1:8])

# Calculo de los limites del mapa
xmin <- min(andalusia$x)
ymin <- min(andalusia$y)
xmax <- max(andalusia$x)
ymax <- max(andalusia$y)

# Dibujado de las capitales de Andalucia
file = system.file('img', 'andalusian_provinces.png', package='orloca')
img = readPNG(file)
plot(o, img=img, main=gettext('Andalucia'), xleft=xmin, ybottom=ymin, xright=xmax, ytop=ymax)

# Dibujado de las capitales de Andalucia y de las curvas de nivel
contour(o, img=img, main=gettext('Andalusia'), xleft=xmin, ybottom=ymin, xright=xmax, ytop=ymax)

# Busqueda de la localizacion optima
andalusia.loca.p <- loca.p(andalusia$x[1:8], andalusia$y[1:8])
sol <- zsummin(andalusia.loca.p)
# La solucion optima esta 35 Km al norte de Antequera
# Antequera es usualmente considerada como el centro geografico de Andalucia
sol
points(sol[1], sol[2], type='p', col='red')

