#!/usr/bin/python

import sys


step_file=sys.stdin
data = step_file.readlines()

n=20
nn=40
cps = [[0]*(n+1) for i in range(nn+1)]
degree = [0]*(n+1)
n_cps = [0]*(n+1)
m_knot = [0]*(n+1)
l = 0
ll = 0
multip = []
u_vect = []
u_vect_f = []
for line in data:
    if not line.strip():
        continue
    i = 0
    if line[0] == '#':
        while line[i] != ';':
            if line[i:i+25] == 'B_SPLINE_CURVE_WITH_KNOTS':
                l = l + 1
                j = i + 25
                m = -1
                while line[j] != ')':
                    if line[j:j+3] == ',(#':
                        ll += 1
                        degree[ll] = int(line[j-1])
                    if line[j] == '#':
                        k = j
                    elif line[j:j+2] == ',#':
                        m = m + 1
                        cps[m][l]=int(line[k+1:j])
                    j = j + 1
                cps[m+1][l]=int(line[k+1:j])
                n_cps[ll] = m+1
                jjj = j+1
                while line[jjj] != ')':
                    if line[jjj] == '(':
                        kkk = jjj
                    jjj += 1
                multip.append(line[kkk+1:jjj])
                jjj += 1
                while line[jjj] != ')':
                    if line[jjj] == '(':
                        kkk = jjj
                    jjj += 1
                u_vect.append(line[kkk+1:jjj])
            i = i + 1

k = 0
for val in multip:
    k += 1
    sp = val.split(',')
    summ = 0
    knot = u_vect[k-1].split(',')
    knot_org = knot[:]
    counter = 0
    for i in range(0,len(sp)):
        summ += int(sp[i])
        if (int(sp[i])>1):
                for j in range(0,int(sp[i])-1):
                    counter += 1
                    knot.insert(counter, knot_org[i])
        else:
                counter += 1
    m_knot[k] = summ-1
    u_vect_f.append(knot)

print(l)

delimiter = ','
counter = 1
for k in range(1,l+1):
    cps_print = []
    for j in range(counter,counter + n_cps[k]+1):
        counter += 1
        cps_print.append(j)
    print(n_cps[k])
    print(degree[k])
    print(m_knot[k])
    print(delimiter.join(u_vect_f[k-1]))
    str_cps = str(cps_print)[1:-1]
    print(str_cps)

print(counter-1)
for k in range(1,l+1):
    for kk in range(0,n_cps[k]+1):
        for line in data:
            if not line.strip():
                continue
            i = 0
            if line[0] == '#':
                while line[i] != '=':
                    i += 1
                if int(line[1:i]) == cps[kk][k]:
                    ii = 0 
                    while line[ii:ii+2] != '))':
                        if line[ii:ii+2] == ',(':
                            jj = ii + 2
                        ii += 1
                    coords = line[jj:ii]
                    print(coords)


