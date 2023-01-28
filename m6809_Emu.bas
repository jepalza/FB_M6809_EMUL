' emulador de 6809 y de City Connection, por Joseba Epalza
' jepalza@gmail.com, www.ingepal.es/com, "Sin copirrites"-2011

' modo de pantalla 800x600
Screen 19 ' 800x600

' necesario para el MULTIKEY
' ademas, si usamos compilacion FB, se necesita el "USING FB"
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#EndIf

' necesarios para el "OpenDialog" y selecionar un fichero
#Define WIN_INCLUDEALL
#include once "windows.bi"


' ******************************************************************
' modulo desensamblador, borrar una vez depurado
	Declare Sub M6809_DIS(direccion As integer, longitud As Integer)
	Dim Shared DirIni As Integer=&h1000 ' zona de la RAM a visualizar
	Dim Shared refresco As Integer=0     ' velocidad de actualizacion de los datos (no tocar aqui)
' ******************************************************************

' control de ciclos y tiempos para emulacion real
Dim Shared ciclos_ejecutados As Integer=0 ' almacena los ciclos ejecutados en EXECUTE
Dim Shared tiempo As Double
Dim Shared bucle As integer


		' la variable mas importante, la de la RAM de 64k
		Dim Shared RAM(&h10000) As Integer
		' Borramos la RAM a FF's (solo por precaucion)
		For bucle=0 To &hffff:ram(bucle)=&hff:Next

Dim contador As Integer

' ****************************************************************************************
' ********************   exclusivo par la emulacion de CITY CONNECTION *******************
' ****************************************************************************************

' subrutinas 
Declare sub LeeRom (nombre As String, inirom As Integer)
Declare Sub convcaracteres()
Declare Sub convfondos()
Declare Sub convesprites()
Declare Sub dibujar()
Declare Sub Ponfondo()

' variables temporales para el emulador de VIDEO de un Thomson Mo5
Dim AA As Integer
Dim BB As Integer
Dim CC As Integer
Dim DD As Integer
Dim EE As Integer
Dim FF As Integer
Dim XX As Integer
Dim YY As Integer
Dim SS As String

Dim escala As Integer =1 'factor de pantalla


Dim Shared romt(100000) As integer ' espacio temporal para conversion: aqui metemos las ROMS al inicio
Dim Shared romg(16384 ) As integer ' espacio para las rom de graficos reconvertidos
Dim Shared roms(32768 ) As Integer ' espacio para las rom de monigotes reconvertidos
Dim Shared romf(196608) As Integer ' espacio para las rom de fondos reconvertidos
Dim Shared romp(57344 ) As Integer ' espacio para las rom de datos de fondos

Dim Shared ramp(65536 ) As integer ' espacio final para dibujar la pantalla
Dim Shared ramt(65536 ) As Integer ' espacio temporal para dibujar la pantalla

' ****************************************************************************************
' ***********************                                             ********************
' ****************************************************************************************



' Incluimos en este punto el emulador de 6809
' debe ir aqui, para que reconozca las variables anteriores
#Include "6809_CPU.bas" ' --> este a su vez, incluye TODOS los modulos del 6809


    ' leemos las ROM graficas por grupos dentro de la RAM temporal
    ' y las convertimos a algo mas legible y rapido de tratar
    leerom("C4" ,0    )' CARACTERES GRAFICOS
    convcaracteres

    leerom("C12",0    )' SPRITES
    leerom("C13",8192 )' SPRITES
    convesprites

    leerom("C6" ,0     )' 32k planos 1 y 2
    leerom("C9" ,32768 )' 32k planos 3 y 4
    LeeRom("C7" ,65536 )' 16k planos 1 y 2
    leerom("C8" ,81920 )' 16k planos 3 y 4
    convfondos

    ' las ROM con datos de pantallas, no se convierten, se leen tal cual
    leerom("C2" ,0     )' 32k datos de construccion de pantallas 32k
    leerom("C3" ,32768 )' 16k datos ""      ""      ""     ""    16k
    leerom("C5" ,49152 )' 8k  datos de colores de pantallas
    ' pasamos el contenido leido sobre la ROMP de pantallas
	 For bucle=0 To 57343:romp(bucle)=romt(bucle):Next
    
    
    ' Borramos la RAM a FF's (solo por precaucion)
	 For bucle=0 To &hffff:ram(bucle)=&h00:Next
    ' ahora leemos las ROM principales de CPU dentro de la RAM
    LeeRom ("C10", 16384)
    LeeRom ("C11", 32768)
    ' pasamos el contenido leido sobre la RAM principal CPU 1
	 For bucle=0 To 65535:ram(bucle)=romt(bucle):Next
    

' inicio de la emulacion
m6809_reset()

Open "volcado_de_ram.bin" For Binary Access write As 3
' bucle infinito de ejecuciones: solo sale con "ESC"

' "DIPSWITCH" 1 y 2 antes de comenzar
ram(&h3001)=&hfc ' SW1 bits 0 y 1 = vidas a añadir a las 3 por defecto
ram(&h3002)=&h78 ' SW2 bits 0,1,2 son de las monedas necesarias para una partida

While 1 
  tiempo=Timer()
  
  ' ejecutamos el M6809, una instruccion cada vez y sumamos los ciclos empleados
  ciclos_ejecutados += m6809_execute() 
     
   ' se ejecutan acciones HARDWARE cada 'x' ciclos
   If (ciclos_ejecutados > 30000) Then 
   	ciclos_ejecutados=0
   	
   	' teclas de FIN: OJO QUE SALE SIN PEDIR CONFIRMACION  
      If MultiKey(SC_ESCAPE) Then GoTo final             
   	
   	' Comprobamos interrupciones solo si el estado de CC lo permite
   	m6809_irq()  ' IRQ
   	m6809_firq() ' FIRQ
   	'm6809_NMI() ' NMI no enmascarable
      
		dibujar() ' ponemos la pantalla convertida
		 

   End If

  
' repetimos todo el proceso otra vez
Wend
final:
' aqui solo llegamos al pulsar "ESC", o fin de emulacion
For aa= 0 To 65535
	Put #3,aa+1,Chr(ram(aa))
Next
Close 3 ' guardamos la RAM



' lee un fichero (ROM) dentro de una matriz indicada
Sub LeeRom (nombre As String, inirom As Integer)
	
	Dim linea As String*32
	Dim ini As Integer
   Dim contador As integer

	ini=1
	Open "ROMS\"+nombre For Binary Access read As 1

	While Not Eof(1)
		Get #1,ini,linea
		For contador=1 To Len(linea)
			romt(inirom)=Asc(Mid(linea,contador,1))
			inirom+=1
		Next
		ini+=len(linea)
	Wend

	Close 1
	
End Sub


' rutinas de conversiones graficas para acelerar la representacion en pantalla con pre-calculos
Sub convcaracteres()
  Dim as integer col
  Dim as integer buclef,bucleg,c
  Dim as integer dirinig,pp=0
  Dim as integer dato,bytea,byteb

   for col=0 to 255
      dirinig=(col*8) ' 8 son los bytes de cada carac
      for buclef=0 to 7
        bytea=(romt(dirinig+buclef) and 240)+((romt(dirinig+buclef+2048) and 240) shr 4)
        byteb=((romt(dirinig+buclef) and 15) shl 4)+(romt(dirinig+buclef+2048) and 15)
        for bucleg=7 to 0 step -1
          dato=((bytea and (1 shl bucleg)) shr bucleg)+_
              (((byteb and (1 shl bucleg)) shr bucleg)*2)
           romg(pp)=dato
          pp+=1
        next 
      Next
   Next
   
End Sub

Sub convesprites()
  Dim as integer col
  Dim as integer buclef, bucleg
  Dim as integer dirinig,pp=0
  Dim as integer dato,bytea,byteb,bytec,byted,carac
  Dim as integer x,y,dirinip

   for col=0 to 255
    if (col>127) then x=2048 else x=0
    dirinig=(col*16) ' 16 son los bytes de cada carac
      for buclef=0 to 15
      bytea=( romt(dirinig+buclef+x) and 240)     +((romt(dirinig+buclef+2048+x) and 240) shr 4)
      byteb=((romt(dirinig+buclef+x) and 15) shl 4)+(romt(dirinig+buclef+2048+x) and 15)
      bytec=( romt(dirinig+buclef+8192+x) and 240)+((romt(dirinig+buclef+2048+8192+x) and 240) shr 4)
      byted=((romt(dirinig+buclef+8192+x) and 15) shl 4)+(romt(dirinig+buclef+2048+8192+x) and 15)
       For bucleg=0 to 7
        dato=((bytea and (1 shl bucleg)) shr bucleg)+(((byteb and (1 shl bucleg)) shr bucleg)*2)+_
              (((bytec and (1 shl bucleg)) shr bucleg)*4)+(((byted and (1 shl bucleg)) shr bucleg)*8)
        roms(pp)=dato
        pp+=1
       Next 
      next
   Next

End Sub

Sub convfondos()
  Dim as integer col
  Dim as integer buclef, bucleg
  Dim as integer dirinig, pp=0
  Dim as integer dato,bytea,byteb,bytec,byted
  Dim as integer  y,offset,offset2

  for y=0 to 11 ' bucle para las 12 pantallas
   if (y>7) then offset=16384 else offset=32768
   if (y>7) then offset2=((y-8)*4096)+65536 else offset2=y*4096
   for col=0 to 255
      dirinig=offset2+(col*8) ' 8 son los bytes de cada carac
      for buclef=0 to 7
        bytea=(romt(dirinig+buclef) and 240)+((romt(dirinig+buclef+2048) and 240) shr 4)
        byteb=((romt(dirinig+buclef) and 15) shl 4)+(romt(dirinig+buclef+2048) and 15)
        bytec=(romt(dirinig+buclef+offset) and 240)+((romt(dirinig+buclef+2048+offset) and 240) shr 4)
        byted=((romt(dirinig+buclef+offset) and 15) shl 4)+(romt(dirinig+buclef+2048+offset) and 15)
        For bucleg=7 to 0 step -1
          dato=((bytea and (1 shl bucleg)) shr bucleg)+(((byteb and (1 shl bucleg)) shr bucleg) shl 1)+_
                (((bytec and (1 shl bucleg)) shr bucleg) shl 2)+(((byted and (1 shl bucleg)) shr bucleg) shl 3)
          romf(pp)=dato
          pp+=1
        Next 
      next
   next
  Next

End Sub


Sub dibujar()
  dim as integer rom
  dim as integer pp
  dim as integer col,fil
  dim as integer buclef, bucleg
  dim as integer dirg,dirinig
  dim as integer dato,bytea,byteb,bytec,byted,carac,offset
  dim as integer x,y,dirinip
  dim as integer sprites
  Dim As Integer simetria=0
  Dim As Integer npal, pal, cr, cg, cb ' paleta de colores
  Dim As Integer colorlinea ' colores por filas para los caracteres
  
  Dim ramt2(65536) As Integer ' espacio temporal para dibujar bloques de pantalla
    
    ' recogemos la paleta de colores de la RAM y activamos (solo si cambia) 
    ' son 1280 bytes, que a 2 cada color, hacen 640 colores
    npal=0   
	  For col=&h3800+(512*2) To &h800+(640*2) Step 2
	  	  pal=ram(col)*256+ram(col+1)
	  	  cr=(pal And &hf000 Shr 12)*16
	  	  cg=(pal And &h0f00 Shr 8)*16
	  	  cb=(pal And &h00f0 Shr 4)*16
	  	  palette npal,cr,cg,cb
	  	  npal+=1
	  Next
   
    
    ' rutinas de impresion y manejo de sprites y pantalla
    ponfondo() ' lo primero

    pp=&h1040 ' aqui comienzan los graficos (la pantalla empieza en la &h1000)
    bytec=0
    offset=0
    'For fil=0 To 57360-1
    '	ramt2(fil)=0' borrado de pantalla virtual
    'Next
    for fil=0 to 28
       
        if (fil=3) then         
          x=ram(10)*256+ram(11) ' hace desplazamiento solo entre estas dos filas
          offset=(x mod 8) ' haya los bits que quedan para hacer el desplazamiento suave
          bytec=x shr 3    ' haya los cuadros de 8 bits que hay que desplazar
        End if
         
        if (bytec And 128) then bytec-=128
        for col=0 to 31
        	  ' en caso del BIT7 activo es "monigote"
           if bytec Then 
             sprites=0
             dato=col+bytec
             ' segun el tipo de "monigote", tendremos una longitud u otra que sumar al dato
             if (((col+bytec)>31) And ((col+bytec)<64)) then sprites= 32*32   :dato=col-( 32-bytec)
             if (((col+bytec)>63) And ((col+bytec)<96)) then sprites=(32*32)*2:dato=col-( 64-bytec)
             if (((col+bytec)>95) And ((col+bytec)<128))then sprites=(32*32)*3:dato=col-( 96-bytec)
             if  ((col+bytec)>127)                      then sprites= 0       :dato=col-(128-bytec)
             carac=ram(pp+(fil*32)+dato+sprites)
           Else
           	 ' caso BIT7=0 es caracter (no "monigote")
             carac=ram(pp+(fil*32)+col)
           End If
       
           if carac=0 then goto espacio ' caracteres "0" no se tratan, son transparentes
           'If carac=255 Then GoTo espacio ' de pruebas
           dirinig=(carac*64) ' 8 son los bytes de cada carac
           x=fil*8
		     y=col*8
           dirinip=(x*256)+y
           dirg=dirinip
          
           for buclef=0 to 7           
           	 colorlinea=ram(&h2000+buclef+x) ' cogemos el color de cada linea
             for bucleg=0 to 7              
               dato=romg(dirinig+bucleg+(buclef*8))
               if dato then ramt2(dirg)=dato+colorlinea
               dirg+=1
             Next 
             dirinip+=256
			    dirg=dirinip
           Next  

          espacio:
        Next 
    Next 

    ' traspasa la RAMT a RAMP con el offset dado y conservando el fondo
    for x=0 to 6144-1 'para las tres primeras lineas
      bytea=ramt2(x)
      If bytea Then 
         ramp(x)=bytea
      End If
    Next
      
    for x=6144 to 57360-1 ' para el resto con desplazamiento horizontal
      bytea=ramt2(x+offset)
      If bytea Then 
      	ramp(x)=bytea
      EndIf
    Next


  ' visualizar sprites (32 en total * 4 bytes=128)
  ' 0=altura (vertical)
  ' 1=sprite
  ' 2=informacion
  ' 3=horizontal
  for sprites=&h2800 to (&h2800+(33*4))-1 step 4
   pp=sprites ' graficos

   carac=ram(pp+1)       'monigote
   if carac=0 then goto nosprite

   bytea=225-ram(pp+0)   'x
   byteb=    ram(pp+3)   'y
   dirinip=(bytea*256)+byteb
   dirg=dirinip ' direccion dentro de la RAMP de video virtual a dibujar
   col=ram(pp+2)               'simetria &h10 y paleta &h0F
   dato=(col And &h0f) Shl 4
   simetria=col And &h10

   dirinig=(carac*128) ' direccion de inicio de los datos graficos del monigote

   for buclef=0 to 15
     for bucleg=0 to 7
       bytea=roms(dirinig+bucleg+(buclef*8))
       if bytea then ' si es "0" no se dibuja ese punto (trasparente)
        if simetria=0 then 
		    ramp(dirg+bucleg)=bytea+dato ' espejo (el 4 es para el banco
        else
   		 ramp(dirg+(8-bucleg))=bytea+dato      ' normal    de colores)
        End If       
       End If     
     Next  
     dirinip+=256 
	  dirg=dirinip
   Next
  nosprite:
  Next    

    

    ' volcamos el resultado almacenado en RAMT, en la pantalla real
    ' pantalla emulada de 256*256 (32*32) SOLO se muestran 240x224
    ' el resto queda oculto para que no se vean los movimientos fuera de pantalla
    fil=0:col=324 ' inicio real en pantalla física de 800x600 (fil=ancho, col=alto)
    for x=0 to 57360-1 'para las tres primeras lineas
      bytea=ramp(x)
      If fil>7 And fil<248 Then PSet (fil,col),bytea
      fil+=1:if fil=256 Then fil=0: col+=1
    Next
    
End Sub  


sub ponfondo()

 Dim pp As Integer=0
 Dim As integer col,fil
 Dim As integer buclef, bucleg
 Dim As integer dirg,dirinig
 Dim As integer dato,bytea,byteb,bytec,byted,carac,scroll,offset
 Dim As integer oldscroll=255
 Dim As Integer oldy=0
 Dim As integer x,y,dirinip
 Dim As integer sprites

 ''memset(ramp,0,57344); return;

 ' en caso de que debamos borrar la pantalla
 if (ram(0)=0 Or ram(0)=7) then For col=0 To 57360:ramp(col)=0:Next:exit Sub
 
 y=ram(3)*4096 ' pantalla de fondo a dibujar
 'if y>0 then m6809_IPeriod2=18333 else m6809_IPeriod2=13333
 If y<>oldy Then oldy=y: oldscroll=255
 pp=64+y ' datos de construccion de pantallas

 scroll=0
 for fil=0 To 28
   x=ram(&h3004)*256+ram(&h3005) ' hace scroll solo entre estas dos filas
   offset=(x Mod 16)/2' haya los bits que quedan para hacer el scroll suave
   scroll=x Shr 4 'haya los cuadros de 8 bits que hay que scrollear
   if scroll And 128 Then scroll-=128

   if scroll=oldscroll Then GoTo nodibujar ' si no se ha movido el fondo, no se re-dibuja

   for col=0 To 31
      if scroll Then 
        sprites=0
        dato=col+scroll
        if (((col+scroll)>31)And((col+scroll)<64)) Then sprites= 32*32   :dato=col-(32-scroll)
        if (((col+scroll)>63)And((col+scroll)<96)) Then sprites=(32*32)*2:dato=col-(64-scroll)
        if (((col+scroll)>95)And((col+scroll)<128))Then sprites=(32*32)*3:dato=col-(96-scroll)
        if  ((col+scroll)>127)                     then sprites= 0       :dato=col-(128-scroll)
        carac=romp(pp+(fil*32)+dato+sprites)
       Else
        carac=romp(pp+(fil*32)+col)
       End If
       
      dirg=0 ' direccion de inicio en RAMT: dibuja primero en el temporal para luego
                         ' copiar con desplazamientos hacia 'RAMP'
      x=fil*8
      y=col*8
      dirinip=(x*256)+y
      dirg+=dirinip

      dirinig=(carac*64)+(ram(3)*16384)
      for buclef=0 To 7
        for bucleg=0 To 7
          bytea=romf(dirinig+bucleg+(buclef*8))
          ramt(dirg)=bytea+80
          dirg+=1
        Next
        dirinip+=256
        dirg=dirinip
      Next
   Next col
 Next fil
 
  oldscroll=scroll

  nodibujar:
  
  ' copia con desplazamiento al buffer final
  For col=0 To 57359
  	ramp(col)=ramt(col+offset)
  Next

End Sub
