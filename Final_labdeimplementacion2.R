
#paso las fechas abolutas a dias
#creo las variables nuevas en cada mes
#creo TENDENCIA, MAX, MIN   en funcion de los ultimos 6 meses historia para cada variable

#limpio la memoria
rm( list=ls() )
gc()

library( "data.table" )
library( "Rcpp")


kcarpeta_datasetsOri     <-  "~/cloud/cloud1/datasetsOri/"
kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"
#karchivo_entrada_zip     <-  "paquete_premium.zip" 

kcampos_separador        <-  "\t"
kcampo_id                <-  "numero_de_cliente"
kcampo_foto              <-  "foto_mes"
kclase_nomcampo          <-  "clase_ternaria"


kcampos_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )
ventana_regresion    <- 6


#La salida
karchivo_salida_completo <-  "paquete_premium_exthist_baja+3.txt"
kextension               <-  "exthist"
karchivo_salida_prefijo  <-  paste( "./", kextension, "/",    sep="" )
karchivo_salida_sufijo   <-  paste( "_", kextension, ".txt",    sep="" )

#--------------------SELECIONAR Y CORRER COMPLETO--------------------------

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 4*n );
  

  for(int i = 0; i < n; i++) 
  {
   
    int  libre    = 0 ;
    int  xvalor   = 1 ;
 
    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;


       if( !R_IsNA( a ) ) 
       {
           y[ libre ]= a ;
       x[ libre ]= xvalor ;
          
       libre++ ;
       }
     
       xvalor++ ;
    }

 
    /* Si hay al menos dos valores */
    if( libre > 1 )
    {   
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ; 
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++) 
      { 
        xsum  += x[h] ; 
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;
 
        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ; 
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else  
    { 
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }

  }


  return out;
}')



#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201904 se interpreta como la fecha "20190501 00:00:00"

fdias_entre  = function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#------------------------------------------------------
#guarda el archivo de un mes----

fguardar_foto  = function( pfoto_mes, pdataset )
{
  
  archivo_salida_mes <- paste( karchivo_salida_prefijo,  pfoto_mes,  karchivo_salida_sufijo, sep="" )

  fwrite(  dataset[ get(kcampo_foto) == pfoto_mes, ], file=archivo_salida_mes , sep=kcampos_separador, na="", row.names=FALSE ) 
}
#------------------------------------------------------

#setwd( kcarpeta_datasetsOri )
setwd("~/Dropbox/")
dataset <- fread("paquete_premium.txt")
#dataset <- fread(  cmd=paste( "gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador ) 


#ordeno por  numero_de_cliente y foto_mes
setorder( dataset,  numero_de_cliente, foto_mes )


nrow( dataset )
ncol( dataset )


#----------
#paso los campos fecha a dias relativos

#paso los campos fecha a dias relativos

dataset[  , Master_Fvencimiento    := fdias_entre( get(kcampo_foto), Master_Fvencimiento )  ]
dataset[  , Master_Finiciomora     := fdias_entre( get(kcampo_foto), Master_Finiciomora )   ]
dataset[  , Master_fultimo_cierre  := fdias_entre( get(kcampo_foto), Master_fultimo_cierre )]
dataset[  , Master_fechaalta       := fdias_entre( get(kcampo_foto), Master_fechaalta )     ]
dataset[  , Visa_Fvencimiento      := fdias_entre( get(kcampo_foto), Visa_Fvencimiento )    ]
dataset[  , Visa_Finiciomora       := fdias_entre( get(kcampo_foto), Visa_Finiciomora )     ]
dataset[  , Visa_fultimo_cierre    := fdias_entre( get(kcampo_foto), Visa_fultimo_cierre )  ]
dataset[  , Visa_fechaalta         := fdias_entre( get(kcampo_foto), Visa_fechaalta )       ]


#----------

#se crean los nuevos campos para Master y Visa, teniendo en cuenta los NA's
#Aqui se deben agregar nuevos campois a voluntad

dataset[ , mv_cuenta_estado2       := pmax( Master_cuenta_estado,  Visa_cuenta_estado, na.rm = TRUE) ]
dataset[ , mv_marca_atraso         := pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ]

dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta,
                                            Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_tconsumos            := rowSums( cbind( Master_tconsumos,  Visa_tconsumos) , na.rm=TRUE ) ]
dataset[ , mv_tadelantosefectivo   := rowSums( cbind( Master_tadelantosefectivo,  Visa_tadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]



#----------

last <- nrow( dataset )
kcampo_id_idx  <-  match( kcampo_id, names(dataset) )
#----------
#creo el vector_desde que indica cada ventana
#de esta forma se acelera el procesamiento ya que lo hago una sola vez

vector_ids   <- dataset[[  kcampo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
#----------#agrego al dataset las TENDENCIAS
campos_a_procesar  <- setdiff( names(dataset) ,  kcampos_no_procesar  )  

for(  campo  in  campos_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
   dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
   dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
   dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}

#----------#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , kclase_nomcampo ) , kclase_nomcampo )
setcolorder( dataset, nuevo_orden )
colnames(dataset)

# MODIFICACIONES DEL RIOJANO _FV SOLAMENTE LAS MEJORES 14 PAPA - TIRA NA, PERO TRANQUI

dataset$mcaja_ahorro_Paquete_log_fv<-log1p(dataset$mcaja_ahorro_Paquete)
dataset$mtarjeta_visa_consumo_out_fv <-ifelse(dataset$mtarjeta_visa_consumo > 350000,350000, dataset$mtarjeta_visa_consumo)
dataset$mpasivos_margen_log_fv<-log1p(dataset$mpasivos_margen)
dataset$mcuentas_saldo_out_fv <-(ifelse(dataset$mcuentas_saldo > 50000000,50000000, dataset$mcuentas_saldo))
dataset$mcuenta_corriente_Paquete_out_fv <-ifelse(dataset$mcuenta_corriente_Paquete > 2000001,2000001, dataset$mcuenta_corriente_Paquete)
dataset$mprestamos_personales_log_fv     <-log1p(dataset$mprestamos_personales)
dataset$tplan_sueldo_out_fv <-ifelse(dataset$tplan_sueldo > 41,41, dataset$tplan_sueldo)
dataset$mdescubierto_preacordado_log_fv     <-log1p(dataset$mdescubierto_preacordado)
dataset$tmovimientos_ultimos90dias_log_fv <-log1p(dataset$tmovimientos_ultimos90dias)
dataset$mactivos_margen_log_fv     <-log1p(dataset$mactivos_margen)
dataset$mprestamos_personales_out_fv <-ifelse(dataset$mprestamos_personales > 1110624,1110624, dataset$mprestamos_personales)
dataset$ctarjeta_visa_transacciones_out_fv <-ifelse(dataset$ctarjeta_visa_transacciones > 200,200, dataset$ctarjeta_visa_transacciones)
dataset$marketing_coss_selling_log_fv     <-log1p(dataset$marketing_coss_selling)
dataset$mcomisiones_mantenimiento_log_fv     <-log1p(dataset$mcomisiones_mantenimiento)
dataset$mactivos_margen_log_fv     <-log1p(dataset$mactivos_margen)

dataset$mrentabilidad_out_fv <-ifelse(dataset$mrentabilidad > 90000,90000, dataset$mrentabilidad)
dataset$mrentabilidad_out_fv <-ifelse(dataset$mrentabilidad_out_fv < -6000,-6000, dataset$mrentabilidad_out_fv)
dataset$mrentabilidad_out_fv<-log1p(dataset$mrentabilidad_out_fv)

#INTENTO DE BAJA+3 EL TITANIC NO SE HUNDIO TODAVIA PAPU

# G E N E R A L I Z A N D O :) LOS AÑOS QUE CREO Q VAMOS A NECESITAR, IGUAL SE PUEDE EXTENDER, es cuestion de concatenar
table(dataset$foto_mes)
#m16_1<-subset(dataset,dataset$foto_mes==201601)
#m16_2<-subset(dataset,dataset$foto_mes==201602)
#m16_3<-subset(dataset,dataset$foto_mes==201603)
#m16_4<-subset(dataset,dataset$foto_mes==201604)
#m16_5<-subset(dataset,dataset$foto_mes==201605)
#m16_6<-subset(dataset,dataset$foto_mes==201606)
#m16_7<-subset(dataset,dataset$foto_mes==201607)
m16_8<-subset(dataset,dataset$foto_mes==201608)
m16_9<-subset(dataset,dataset$foto_mes==201609)
m16_10<-subset(dataset,dataset$foto_mes==201610)
m16_11<-subset(dataset,dataset$foto_mes==201611)
m16_12<-subset(dataset,dataset$foto_mes==201612)
m17_1<-subset(dataset,dataset$foto_mes==201701)
m17_2<-subset(dataset,dataset$foto_mes==201702)
m17_3<-subset(dataset,dataset$foto_mes==201703)
m17_4<-subset(dataset,dataset$foto_mes==201704)
m17_5<-subset(dataset,dataset$foto_mes==201705)
m17_6<-subset(dataset,dataset$foto_mes==201706)
m17_7<-subset(dataset,dataset$foto_mes==201707)
m17_8<-subset(dataset,dataset$foto_mes==201708)
m17_9<-subset(dataset,dataset$foto_mes==201709)
m17_10<-subset(dataset,dataset$foto_mes==201710)
m17_11<-subset(dataset,dataset$foto_mes==201711)
m17_12<-subset(dataset,dataset$foto_mes==201712)
m18_1<-subset(dataset,dataset$foto_mes==201801)
m18_2<-subset(dataset,dataset$foto_mes==201802)
m18_3<-subset(dataset,dataset$foto_mes==201803)
m18_4<-subset(dataset,dataset$foto_mes==201804)
m18_5<-subset(dataset,dataset$foto_mes==201805)
m18_6<-subset(dataset,dataset$foto_mes==201806)
m18_7<-subset(dataset,dataset$foto_mes==201807)
m18_8<-subset(dataset,dataset$foto_mes==201808)
m18_9<-subset(dataset,dataset$foto_mes==201809)
m18_10<-subset(dataset,dataset$foto_mes==201810)
m18_11<-subset(dataset,dataset$foto_mes==201811)
m18_12<-subset(dataset,dataset$foto_mes==201812)
m19_1<-subset(dataset,dataset$foto_mes==201901)
m19_2<-subset(dataset,dataset$foto_mes==201902)
m19_3<-subset(dataset,dataset$foto_mes==201903)
m19_4<-subset(dataset,dataset$foto_mes==201904)
m19_5<-subset(dataset,dataset$foto_mes==201905)
m19_6<-subset(dataset,dataset$foto_mes==201906)

#Sacar los b+2 del mismo mes para luego comparar
#m16_1_b2<-m16_1[clase_ternaria == "BAJA+2",numero_de_cliente]
#m16_2_b2<-m16_2[clase_ternaria == "BAJA+2",numero_de_cliente]
#m16_3_b2<-m16_3[clase_ternaria == "BAJA+2",numero_de_cliente]
#m16_4_b2<-m16_4[clase_ternaria == "BAJA+2",numero_de_cliente]
#m16_5_b2<-m16_5[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_6_b2<-m16_6[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_7_b2<-m16_7[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_8_b2<-m16_8[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_9_b2<-m16_9[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_10_b2<-m16_10[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_11_b2<-m16_11[clase_ternaria == "BAJA+2",numero_de_cliente]
m16_12_b2<-m16_12[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_1_b1<-m17_1[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_2_b0<-m17_2[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_3_b1<-m17_3[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_4_b2<-m17_4[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_5_b2<-m17_5[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_6_b2<-m17_6[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_7_b2<-m17_7[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_8_b2<-m17_8[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_9_b2<-m17_9[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_10_b2<-m17_10[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_11_b2<-m17_11[clase_ternaria == "BAJA+2",numero_de_cliente]
m17_12_b2<-m17_12[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_1_b2<-m18_1[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_2_b2<-m18_2[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_3_b2<-m18_3[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_4_b2<-m18_4[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_5_b2<-m18_5[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_6_b2<-m18_6[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_7_b2<-m18_7[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_8_b2<-m18_8[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_9_b2<-m18_9[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_10_b2<-m18_10[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_11_b2<-m18_11[clase_ternaria == "BAJA+2",numero_de_cliente]
m18_12_b2<-m18_12[clase_ternaria == "BAJA+2",numero_de_cliente]
m19_1_b2<-m19_1[clase_ternaria == "BAJA+2",numero_de_cliente]
m19_2_b2<-m19_2[clase_ternaria == "BAJA+2",numero_de_cliente]
m19_3_b2<-m19_3[clase_ternaria == "BAJA+2",numero_de_cliente]
m19_4_b2<-m19_4[clase_ternaria == "BAJA+2",numero_de_cliente]
#m19_5_b2<-m19_5[clase_ternaria == "BAJA+2",numero_de_cliente]
#m19_6_b3<-m19_6[clase_ternaria == "BAJA+2",numero_de_cliente]


# SOY UN MINEROVICH - ESTE CONCATENAR ESTA GUARDADO ENTRENANDO EN "10 MESES CON P_P_PEXTHIT + 15 VARIABLES MIAS".EXCEL
#ACA COMPARO LOS ID DE USUARIO  "BAJA+2" DEL MES 201701 CON 201612, EN EL CASO DE ENCONTRAR COINCIDENCIA, CLAVA UN RE TRUE

m17_1$mi_target <-m17_1$numero_de_cliente %in% m16_12_b2
m17_2$mi_target <-m17_2$numero_de_cliente %in% m17_1_b1
m17_3$mi_target <-m17_3$numero_de_cliente %in% m17_2_b0
m17_4$mi_target <-m17_4$numero_de_cliente %in% m17_3_b1
m17_5$mi_target <-m17_5$numero_de_cliente %in% m17_4_b2
m17_6$mi_target <-m17_6$numero_de_cliente %in% m17_5_b2
m17_7$mi_target <-m17_7$numero_de_cliente %in% m17_6_b2
m17_8$mi_target <-m17_8$numero_de_cliente %in% m17_7_b2
m17_9$mi_target <-m17_9$numero_de_cliente %in% m17_8_b2
m17_10$mi_target <-m17_10$numero_de_cliente %in% m17_9_b2
m17_11$mi_target <-m17_11$numero_de_cliente %in% m17_10_b2
m17_12$mi_target <-m17_12$numero_de_cliente %in% m17_11_b2
m18_1$mi_target <-m18_1$numero_de_cliente %in% m17_12_b2
m18_2$mi_target <-m18_2$numero_de_cliente %in% m18_1_b2
m18_3$mi_target <-m18_3$numero_de_cliente %in% m18_2_b2
m18_4$mi_target <-m18_4$numero_de_cliente %in% m18_3_b2
m18_5$mi_target <-m18_5$numero_de_cliente %in% m18_4_b2
m18_6$mi_target <-m18_6$numero_de_cliente %in% m18_5_b2
m18_7$mi_target <-m18_7$numero_de_cliente %in% m18_6_b2
m18_8$mi_target <-m18_8$numero_de_cliente %in% m18_7_b2
m18_9$mi_target <-m18_9$numero_de_cliente %in% m18_8_b2
m18_10$mi_target <-m18_10$numero_de_cliente %in% m18_9_b2
m18_11$mi_target <-m18_11$numero_de_cliente %in% m18_10_b2
m18_12$mi_target <-m18_12$numero_de_cliente %in% m18_11_b2
m19_1$mi_target <-m19_1$numero_de_cliente %in% m18_12_b2
m19_2$mi_target <-m19_2$numero_de_cliente %in% m19_1_b2
m19_3$mi_target <-m19_3$numero_de_cliente %in% m19_2_b2
m19_4$mi_target <-m19_4$numero_de_cliente %in% m19_3_b2
m19_5$mi_target <-m19_5$numero_de_cliente %in% m19_4_b2
#JUNTO EL DATASET

dataset_nuevo_target	<-	rbind(m17_1,
                              m17_2,
                              m17_3,
                              m17_4,
                              m17_5,
                              m17_6,
                              m17_7,
                              m17_8,
                              m17_9,
                              m17_10,
                              m17_11,
                              m17_12,
                              m18_1,
                              m18_2,
                              m18_3,
                              m18_4,
                              m18_5,
                              m18_6,
                              m18_7,
                              m18_8,
                              m18_9,
                              m18_10,
                              m18_11,
                              m18_12,
                              m19_1,
                              m19_2,
                              m19_3,
                              m19_4,
                              m19_5)


table(dataset_nuevo_target$mi_target)
table(dataset_nuevo_target$clase_ternaria)
table(dataset_nuevo_target$foto_mes)
# ACA PODRIAS GRABAR EL DATASET CON LAS 2 CLASES TERNARIAS!!!

#Borro la otra clase ternaria, es para putos esa- hay que borrarle si o si antes de correr modelos
dataset_nuevo_target$clase_ternaria <- NULL

#HAGO LO FALSE/TRUE A 1 Y 2 CORROBORAR LA PROPORCION DE TRUE CON 1
table(dataset_nuevo_target$mi_target)
dataset_nuevo_target[,mi_target := as.integer(as.logical(dataset_nuevo_target$mi_target))]
table(dataset_nuevo_target$mi_target) #TIENE QUE HABER LA MISMA CANTIDAD DE TRUE QUE DE 1

class(dataset_nuevo_target$mi_target) #TIENE QUE SER INTENGER

#grabo el archivo completo

#setwd( kcarpeta_datasets )

fwrite( dataset_nuevo_target, file="targetnuevo_dpsdefetodoenunomejorado.txt", sep="\t", na="", row.names=FALSE )


#AHORA PODES TIRAR UN LIGTH DIRECTO PWP

#Si queres borrar algo expeto "a" rm(list=setdiff(ls(), "a"))
rm(list=ls())
quit( )



