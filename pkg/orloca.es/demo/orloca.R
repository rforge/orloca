# Ajuste de parámetros gráficos
require(grDevices)
require(graphics)
opar <- par(ask = dev.interactive(orNone = TRUE))

# Creación de un objeto loca.p
o <- new("loca.p", x = c(-1, 1, 0), y = c(0, 0, 1))

# Imprimiendo un resumen del objeto
print(o)

# Gráfica de los puntos de demanda
plot(o)

# Evaluación de la función en el punto (0, 0.5)
zsum(o, x=0, y=0.5)

# Gráfica de las curvas de nivel de la función objetivo
contour.loca.p(o)

# Gráfica 3D de la función objetivo
persp.loca.p(o)

# Gráfica 3D
persp.loca.p(o, col=cm.colors(10000), border=FALSE, shade=TRUE, theta=50, phi=5, ltheta=135)

# Otra gráfica 3D
persp.loca.p(o, col=cm.colors(10000), border=FALSE, shade=TRUE, theta=50, phi=5, ltheta=135, lphi=90)

# Búsqueda del mínimo
zsummin(o)

# Núevo objeto aleatorio loca.p con 10 puntos de demanda
p <- rloca.p(10)

# Búsqueda del mínimo 
sol <- zsummin(p)

# El mínimo
sol

# Evaluación de la función en el mínimo
zsum(p, sol[1], sol[2])

# Calculo del tiempo de ejecución
system.time(zsummin(p))

# Restauración de los parámetros gráficos
par(opar)
