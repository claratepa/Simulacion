import numpy as np
from random import random
import matplotlib.cm as cm
import matplotlib.pyplot as plt

maximo = 50
dim = 30
num = dim**2
donde=open ('demo.txt', 'w')
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
            print(prob,iteraciones, file=donde)
            donde.close()
        print(prob, replicas, iteraciones)


