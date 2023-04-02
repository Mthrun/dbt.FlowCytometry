CombineCls=function(VarCls){
# zusammenfassen der Klassifikationen zu einer Zahl
# inverse funktion: VarCls = Combined2VarCls(Cls)
#
# ACHTUNG bei mehr als 9 unique Klassen sind die Stellen nicht mehr getrennt
#         und die Ruecktransformaiton liefernt nicht mehr die originalen Klassen
#
# INPUT
# VarCls(d,n)                     d cases,  n Classifications
#
# OUTPUT
# Cls(d)                         eine Klassifikation Kombiniert aus VarCls


V=dim(VarCls)
n=V[1]
d=V[2]
#Exponent = (d:-1:1)'-1
Exponent=seq(from=d,by=-1,to=1)-1
Exponent = 10^Exponent

Cls = VarCls%*%Exponent

return(as.vector(Cls))
}
