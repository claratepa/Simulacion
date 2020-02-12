import numpy as np
from random import random
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import pandas as pd


maximo = 50
dim = 30
num = dim**2
donde = open ('pr2simdata.txt', 'w')
columnas = ['Prob', 'Iter']
dataframe = pd.DataFrame(columns = columnas)

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
        print(replicas, prob, iteraciones, file=donde)
        nuevos = {'Prob': prob, 'Iter': iteraciones}
        dataframe.loc[len(dataframe)] = nuevos

donde.close()
boxplot = dataframe.boxplot(column = 'Iter', by = 'Prob')
import matplotlib.pyplot as plt
plt.savefig('boxplot.png')



        




 
