# Adjusting graphics parameters
require(grDevices)
require(graphics)
opar <- par(ask = dev.interactive(orNone = TRUE))

# Creation of new loca.p object
o <- loca.p(x = c(-1, 1, 0), y = c(0, 0, 1))

# Summaring and printing the object
print(o)

# Plot the demand points
plot(o)

# Evaluation at point (0, .5)
zsum(o, x=0, y=.5)

# Contour plot the objective function
contour.loca.p(o)

# 3D plot of the objective function
persp.loca.p(o)

# 3D nice plot
persp.loca.p(o, col=cm.colors(10000), border=FALSE, shade=TRUE, theta=50, phi=5, ltheta=135)

# Another 3D plot
persp.loca.p(o, col=cm.colors(10000), border=FALSE, shade=TRUE, theta=50, phi=5, ltheta=135, lphi=90)

# Find the minimum
zsummin(o)

# New random loca.p object with 10 demand point
p <- rloca.p(10)

# Find the minimun 
sol <- zsummin(p)

# Show it
sol

# Eval the function at minimun
zsum(p, sol[1], sol[2])

# Timing the algorithm
system.time(zsummin(p))

# Restoring graphics parameters
par(opar)
