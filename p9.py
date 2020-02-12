import numpy as np
from random import random
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import pandas as pd
import plotnine as p9

maximo = 50
dim = 30
num = dim**2
donde=open ('demo.csv', 'w')

for prob in range(11):
    prob /= 10
    for replicas in range(10):
        valores = [random() < prob for i in range(num)]
        iteraciones = 0
        
        while sum(valores) > 0:
            actual = np.reshape(valores, (dim, dim))
            nuevos = []
            
            
            for pos in range(num):
                fila = (pos - 1) // dim
                columna = (pos - 1) % dim
                vecindad = actual[(fila - 1):(fila + 2), (columna - 1):(columna + 2)]
                nuevos.append(1 * (np.sum(vecindad) - actual[fila, columna] == 3))
            valores = nuevos
            iteraciones += 1
            
            if iteraciones == maximo:
                print("matanza obligatoria")
                break
        p9.ggplot(iteraciones, prob)
        + p9.geom_box()

        print(replicas,prob,iteraciones, file=donde)
        print(replicas, prob, iteraciones)
            
donde.close()

p9.show()
