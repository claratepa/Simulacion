datos = []
with open("feos.txt") as entrada:
    for linea in entrada:
        for campo in linea.split():
            try:
                valor = int(campo)
                datos.append(valor)
            except:
                print(campo)
with open("bonitos.txt", "w") as salida:
    for dato in datos:
        print(dato, file = salida)
