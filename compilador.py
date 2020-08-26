
# -----------------------------------------------------------------------------
# Compilador
# -----------------------------------------------------------------------------
import ply.lex as lex
import ply.yacc as yacc
import sys

var = 0
n_operacion = 1
tabla_simbolos = {}
tabla_operaciones = {}

# Tokens

palabras_reservadas = {
    'printf' : 'PRINT',
    'if' : 'IF',
    'int' : 'INT',
    'string' : 'STRING',
}

tokens = ['ID','NUMERO','CADENA']

tokens = tokens + list(palabras_reservadas.values())

literals = ['+','-','*','/','(',')','=',';',',','>']

simbolos = literals + list(palabras_reservadas.keys())

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = palabras_reservadas.get(t.value,'ID')    
    return t

def t_NUMERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_CADENA(t): 
    r'"\w.+"'
    t.value = t.value[1:-1] 
    return t


# Ignorar espacio y tabuladores
t_ignore = " \t"


def t_nuevaLinea(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Caracter no valido '%s'" % t.value[0])
    t.lexer.skip(1)

# Analizador lexico
lexer = lex.lex()   

precedence = (
    ('left','+','-'),
    ('left','*','/'),
    ('left','>'),
    ('nonassoc','PRINT'),
    )

# Gramatica

def p_programa(t):
    'programa : listaDeclaracion'

def p_lista_declaracion(t):
    '''listaDeclaracion : listaDeclaracion declaracion
                        | declaracion ''' 

def p_lista_variables(t):
    'listaVariables : listaVariables "," ID'    
    insertar_simbolo(t[3],"variable")                  

def p_lista_variables2(t):
    'listaVariables : ID'
    insertar_simbolo(t[1],"variable")                  

def p_tipo(t):
    '''tipo : INT 
            | STRING'''    

def p_listavarBloque(t):
    '''listaVarBloque : listaVarBloque
                      | tipo listaVariables ";" '''

def p_listaSentencia(t):
    '''listaSentencia : listaSentencia sentencia 
                      | sentencia''' 

def p_bloque(t):        
    'bloque : listaVarBloque listaSentencia '  

def p_declaracion(t):
    '''declaracion : tipo listaVariables ";"
                   | bloque '''                        

def p_sentencia_if_else(t):
    'sentencia : IF "(" expresion ")" sentencia'
    guardar_operaciones("SALTAR",t[3], n_operacion)
    temp = tabla_operaciones[n_operacion-2].copy()
    tabla_operaciones[n_operacion-2] = tabla_operaciones[n_operacion-1].copy()
    tabla_operaciones[n_operacion-1] = temp

def p_sentencia(t):
    'sentencia : expresion'    

def p_sentencia_imprimir(t):
    'sentencia : PRINT "(" expresion ")" ";"'
    guardar_operaciones("IMPRIMIR", t[3])

def p_expresion_operaciones(t):
    '''expresion : expresion '+' expresion
                  | expresion '-' expresion
                  | expresion '*' expresion
                  | expresion '/' expresion
                  | expresion '>' expresion'''
    if t[2] == '+'  : 
        temp = variable_temporal()
        guardar_operaciones("SUMA",temp,t[1],t[3])
        t[0] = temp
    elif t[2] == '-': 
        temp = variable_temporal()
        guardar_operaciones("RESTA",temp,t[1],t[3])
        t[0] = temp
    elif t[2] == '*': 
        temp = variable_temporal()
        guardar_operaciones("MULTIPLICACION",temp,t[1],t[3])
        t[0] = temp
    elif t[2] == '/': 
        temp = variable_temporal()
        guardar_operaciones("DIVISION",temp,t[1],t[3])
        t[0] = temp
    elif t[2] == '>': 
        temp = variable_temporal()
        guardar_operaciones("CONDICION",temp,t[1],t[3])
        t[0] = temp    

def p_expresion_agrupar(t):
    'expresion : "(" expresion ")"'
    t[0] = t[2]

def p_expresion_numero(t):
    'expresion : NUMERO'
    t[0] = t[1]
    insertar_simbolo(t[1], "temporal", "int", t[1])


def p_expresion_cadena(t):
    'expresion : CADENA'
    t[0] = t[1]    
    insertar_simbolo(t[1], "temporal", "string", t[1])

def p_expresion_id(t):
    'expresion : ID'
    t[0] = t[1]

def p_expresion_asignar(t):
    'expresion : ID "=" expresion ";"'
    if vericar_simbolo(t[1]):
        if tabla_simbolos[t[3]]['tipo']=="int":
            guardar_operaciones("MOVER",t[1],t[3],"int")
        else:    
            guardar_operaciones("MOVER",t[1],t[3],"string")

    else:   
        print ('Error: variable '+t[1]+' no declarada')
        sys.exit(0)

def p_error(t):
    print("Error sintactico en '%s'" % t.value)

def insertar_simbolo(simbolo, clase, tipo=0, valor=0):
    tabla_simbolos[simbolo] = {}
    tabla_simbolos[simbolo]['clase'] = clase
    tabla_simbolos[simbolo]['tipo'] = tipo 
    tabla_simbolos[simbolo]['valor'] = valor

def vericar_simbolo(s):
    for simbolo in tabla_simbolos:
        if simbolo==s:
            return 1

    return 0        

def variable_temporal():
    global var
    var_temp = "T"+str(var)
    tabla_simbolos[var_temp] = {}
    tabla_simbolos[var_temp]['valor'] = 0
    tabla_simbolos[var_temp]['tipo'] = "int"
    tabla_simbolos[var_temp]['clase'] = "temporal"
    var = var+1
    return var_temp

def mostrar_tabla():
    for simbolo in tabla_simbolos:
        print (simbolo, tabla_simbolos[simbolo]['valor'], tabla_simbolos[simbolo]['tipo'], tabla_simbolos[simbolo]['clase'])    


def guardar_operaciones(operac, op1, op2=0, op3=0):
    global n_operacion
    tabla_operaciones[n_operacion] = {}
    tabla_operaciones[n_operacion]['oper'] = operac
    tabla_operaciones[n_operacion]['op1'] = op1
    tabla_operaciones[n_operacion]['op2'] = op2
    tabla_operaciones[n_operacion]['op3'] = op3

    n_operacion = n_operacion+1


def generar_codigo():
    op = 1
    while op < n_operacion:
        if tabla_operaciones[op]['oper']=="SUMA":
            print (str(op)+" SUMA "+str(tabla_operaciones[op]['op1'])+" = "+str(tabla_operaciones[op]['op2'])+" + "+str(tabla_operaciones[op]['op3']))

        elif tabla_operaciones[op]['oper']=="RESTA":
            print (str(op)+" RESTA "+str(tabla_operaciones[op]['op1'])+" = "+str(tabla_operaciones[op]['op2'])+" - "+str(tabla_operaciones[op]['op3']))
       
        elif tabla_operaciones[op]['oper']=="MULTIPLICACION":
            print (str(op)+" MULTIPLICACION "+str(tabla_operaciones[op]['op1'])+" = "+str(tabla_operaciones[op]['op2'])+" * "+str(tabla_operaciones[op]['op3']))
            
        elif tabla_operaciones[op]['oper']=="DIVISION":
            print (str(op)+" DIVISION "+str(tabla_operaciones[op]['op1'])+" = "+str(tabla_operaciones[op]['op2'])+" / "+str(tabla_operaciones[op]['op3']))

        elif tabla_operaciones[op]['oper']=="CONDICION":
            if tabla_simbolos[tabla_operaciones[op]['op2']]['valor']>tabla_simbolos[tabla_operaciones[op]['op3']]['valor']:
                tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = 1
            else:        
                tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = 0

            print (str(op)+" MAYOR "+str(tabla_operaciones[op]['op1'])+" = "+str(tabla_operaciones[op]['op2'])+" > "+str(tabla_operaciones[op]['op3']))    

        elif tabla_operaciones[op]['oper']=="IMPRIMIR":
            print (str(op)+" IMPRIMIR "+str(tabla_operaciones[op]['op1']))

        elif tabla_operaciones[op]['oper']=="SALTAR":
            if tabla_simbolos[tabla_operaciones[op]['op1']]['valor']:
                print (str(op)+" SALTAR "+str(tabla_operaciones[op]['op1']) +" "+str(tabla_operaciones[op]['op2']))    
            else:    
                print (str(op)+" SALTAR "+str(tabla_operaciones[op]['op1']) +" "+str(tabla_operaciones[op]['op2']+1))    
                op = op + 1
        else :
            print (str(op)+" MOVER "+str(tabla_operaciones[op]['op1'])+" "+str(tabla_operaciones[op]['op2']))

        op = op+1     

def compilador():

    op = 1
    while op < n_operacion:
        if tabla_operaciones[op]['oper']=="SUMA":
            tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = tabla_simbolos[tabla_operaciones[op]['op2']]['valor']+tabla_simbolos[tabla_operaciones[op]['op3']]['valor']

        elif tabla_operaciones[op]['oper']=="RESTA":
            tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = tabla_simbolos[tabla_operaciones[op]['op2']]['valor']-tabla_simbolos[tabla_operaciones[op]['op3']]['valor']
       
        elif tabla_operaciones[op]['oper']=="MULTIPLICACION":
            tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = tabla_simbolos[tabla_operaciones[op]['op2']]['valor']*tabla_simbolos[tabla_operaciones[op]['op3']]['valor']
            
        elif tabla_operaciones[op]['oper']=="DIVISION":
            tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = tabla_simbolos[tabla_operaciones[op]['op2']]['valor']/tabla_simbolos[tabla_operaciones[op]['op3']]['valor']

        elif tabla_operaciones[op]['oper']=="CONDICION":
            if tabla_simbolos[tabla_operaciones[op]['op2']]['valor']>tabla_simbolos[tabla_operaciones[op]['op3']]['valor']:
                tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = 1
            else:        
                tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = 0

        elif tabla_operaciones[op]['oper']=="IMPRIMIR":
            print (tabla_simbolos[tabla_operaciones[op]['op1']]['valor'])

        elif tabla_operaciones[op]['oper']=="SALTAR":
            if tabla_simbolos[tabla_operaciones[op]['op1']]['valor']==0:
                op = op + 1

        elif tabla_operaciones[op]['oper']=="MOVER":
            tabla_simbolos[tabla_operaciones[op]['op1']]['valor'] = tabla_simbolos[tabla_operaciones[op]['op2']]['valor']
            tabla_simbolos[tabla_operaciones[op]['op1']]['tipo'] = tabla_operaciones[op]['op3']

        op = op+1         

# Analizador sintactico
parser = yacc.yacc()

archivo = open('programa.cpp')
parser.parse(archivo.read())#, debug=True)
        
archivo.close()
print ("\n")
print ("Codigo Objetivo\n")
generar_codigo()
print ("\n")
print ("Ejecucion\n")
compilador()
print ("\n")
print ("Tabla de Simbolos\n")
mostrar_tabla()
print ("\n")
print ("Simbolos usados")
print (simbolos)