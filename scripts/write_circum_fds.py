#!/usr/bin/python

import sys
import math


div_x = int(sys.argv[7])
div_y = int(sys.argv[8])
div_z = int(sys.argv[9])

x_min = float(sys.argv[1])
x_max = float(sys.argv[2])
x_fire_min = 100.0
x_fire_max = 101.0
x_dev = 9.0
y_min = float(sys.argv[3])
y_max = float(sys.argv[4])
y_fire_min = 100.0
y_fire_max = 101.0
y_dev = 150.
z_min = float(sys.argv[5])
z_max = float(sys.argv[6])
chid=sys.argv[10]


dx = (x_max-x_min)/div_x
dy = (y_max-y_min)/div_y
dz = (z_max-z_min)/div_z

print("&HEAD CHID='"+chid+"', TITLE='Geometry with Splines' /")
print("&MESH IJK= "+str(div_x)+", "+str(div_y)+", "+str(div_z)+" XB = "+str(x_min)+", "+str(x_max)+", "+str(y_min)+", "+str(y_max)+
", "+str(z_min)+", "+str(z_max)+" /")
print("&TIME T_END = 0. /")

print("&VENT MB='XMIN', SURF_ID='OPEN' /")
print("&VENT MB='XMAX', SURF_ID='OPEN' /")
print("&VENT MB='YMIN', SURF_ID='OPEN' /")
print("&VENT MB='YMAX', SURF_ID='OPEN' /")
print("&VENT MB='ZMIN', SURF_ID='OPEN' /")
print("&VENT MB='ZMAX', SURF_ID='OPEN' /")

for i in range(11,len(sys.argv)):
    f = open(sys.argv[i], "r")
    data = f.readlines()
    f.close()
    n = [0]*(len(sys.argv)-11)
    coord_x = [[] for x in xrange(len(sys.argv)-11)]
    coord_y = [[] for x in xrange(len(sys.argv)-11)]
    coord_x_min = [0]*(len(sys.argv)-11)
    coord_y_min = [0]*(len(sys.argv)-11)
    coord_x_max = [0]*(len(sys.argv)-11)
    coord_y_max = [0]*(len(sys.argv)-11)
    n[i-11] = 0
    for line in data:
        if not line.strip():
            continue
        n[i-11] += 1
    for line in data:
        if not line.strip():
            continue
        coord_x[i-11].append(line.split(',')[0])
        coord_y[i-11].append(line.split(',')[1])

    coord_x[i-11]=map(float, coord_x[i-11])
    coord_y[i-11]=map(float, coord_y[i-11])

    coord_x_min[i-11] = min(coord_x[i-11])
    coord_y_min[i-11] = min(coord_y[i-11])
    coord_x_max[i-11] = max(coord_x[i-11])
    coord_y_max[i-11] = max(coord_y[i-11])



y = y_min
for i in range(1,div_y+2):
    x = x_min
    for j in range(1,div_x+2):
        for k in range(0,len(sys.argv)-11):
            bool = 0
            if((float(x)<=math.ceil(coord_x_max[k])) and (float(x)>=math.floor(coord_x_min[k]))  and (float(y)<=math.ceil(coord_y_max[k])) and (float(y)>=math.floor(coord_y_min[k]))):
                for l in range(0,n[k]):
                    if((float(x)<coord_x[k][l]) and (float(x+dx)>=coord_x[k][l]) and (float(y)<coord_y[k][l]) and (float(y+dy)>=coord_y[k][l]) and bool == 0):
                        bool = 1
                        print("&OBST XB="+str(x)+","+str(x+dx)+","+str(y)+","+str(y+dy)+","+str(z_min)+","+str(z_max)+", SURF_ID='CONCRETE SURFACE'/")
                        break
        x += dx
    y += dy

    


print("&OBST XB="+str(x_min)+","+str(x_max)+","+str(y_min)+","+str(y_max)+","+str(z_min)+","+str(z_min+dz)+", SURF_ID='FLOOR' /")




print("&VENT XB="+str(x_fire_min)+","+str(x_fire_max)+","+str(y_fire_min)+","+str(y_fire_max)+","+str(z_min+dz)+","+str(z_min+dz)+", SURF_ID='FIRE' /")

print("&DEVC XB="+str(x_dev)+","+str(x_dev)+","+str(y_dev)+","+str(y_dev)+","+str(z_min+dz)+","+str(z_max)+", QUANTITY='LAYER HEIGHT', ID='smoke_layer_height' /")
print("&DEVC XB="+str(x_dev)+","+str(x_dev)+","+str(y_dev)+","+str(y_dev)+","+str(z_min+dz)+","+str(z_max)+", QUANTITY='UPPER TEMPERATURE', ID='smoke_layer_temperature' /")


print("&SURF ID        = 'FIRE'")
print("      HRRPUA   = 2500. /")


print("&SURF ID        = 'CONCRETE SURFACE'")
print("      MATL_ID   = 'CONCRETE'")
print("      RGB       = 128,128,128")
print("      THICKNESS = 0.1 /")

print("&SURF ID        = 'FLOOR'")
print("      MATL_ID   = 'CARPET'")
print("      RGB       = 165,42,42")
print("      THICKNESS = 0.1 /")


print("&MATL ID            = 'CONCRETE'")
print("      SPECIFIC_HEAT = 0.88")
print("      DENSITY       = 2100.")
print("      CONDUCTIVITY  = 1.0 /")

print("&MATL ID            = 'CARPET'")
print("      SPECIFIC_HEAT = 0.88")
print("      DENSITY       = 2100.")
print("      CONDUCTIVITY  = 1.0 /")


print("&TAIL /")



