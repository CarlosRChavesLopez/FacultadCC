

def configuracionValida(posicion,cantVacas,distancia):
    ultimaPosicion = posicion[0]

    for i in range(1, len(posicion)):
        if posicion[i]-ultimaPosicion >= distancia:
            cantVacas -= 1
            ultimaPosicion = posicion[i]

    return cantVacas == 0


def vacasEnojadas(barras, vacas, posiciones): 
    if (barras<2 or barras>100000 or vacas>barras):
        return "input incorrecto"
    else:
        posiciones.sort()
        for contador in range( , len(posiciones)):
            if not(configuracionValida(posiciones,vacas,contador)):
                return contador

