casos = int(input())
infoCaso = []

for x in range(0,casos):
    n, c = input().split()
    n = int(n)
    c = int(c)
    ies = []

    for ns in range (0,n) :
        iNuevo = int(input())
        ies.append(iNuevo)
    ies.sort()
    
    infoCaso.append((n,c,ies))


for i in range(0,casos):

    def distanciaPosible(dis):
        vacas = infoCaso[i][1]-1
        pos1=0
        for pos2 in range(1,len(infoCaso[i][2])):

            
            if vacas == 0:

                    return True
            if infoCaso[i][2][pos2]-infoCaso[i][2][pos1]>=dis:
                    pos1=pos2
                    vacas -=1

        return False

    minimo = 1
    maximo = max(infoCaso[i][2]) - min(infoCaso[i][2])

    while minimo <= maximo:
        j = (minimo + maximo) // 2
        if distanciaPosible(j):
            minimo = j + 1
        else:
            maximo = j - 1

    print(j)
    