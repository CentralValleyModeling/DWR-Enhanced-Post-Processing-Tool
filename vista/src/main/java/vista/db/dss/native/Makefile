# Makefile for creating shared library

#Solaris
JAVA_HOME = /site/java/jdk
JAVAH=$(JAVA_HOME)/bin/javah
PLATFORM = solaris
VISTA_CLASS_DIR = ../../../../
HEC_LIB = /delta/models/hec/heclib.a # this lib file allows for long filenames and pathnames
#HEC_LIB=/usr/hec/lib/heclib.a
HEC_DBG_LIB = /usr/hec/lib/heclib-dbg.a
#Windows
#JAVA_HOME = c:\jdk1.1.6\
#PLATFORM = win
#VISTA_CLASS_DIR = d:\delta\models\java\classes
#HEC_LIB = /usr/hec/lib/heclib.a
#HEC_DBG_LIB = /usr/hec/lib/heclib-dbg.a

#F77=f77
F77=/opt/SUNWspro/SC4.2/bin/f77

#CC=CC
CC=/opt/SUNWspro/SC4.2/bin/CC

LINKER=${F77}

SRC_CC=vista_db_dss_DSSDataReader.c vista_db_dss_DSSDataWriter.c
SRC_HH=vista_db_dss_DSSDataReader.h vista_db_dss_DSSDataWriter.h
OBJ_CC=${SRC_CC:%.c=%.o}

INCLUDES = -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/$(PLATFORM)

opt:=CFLAGS= -PIC -fast ${INCLUDES} #-D_REENTRANT
fixed:=CFLAGS= -PIC  ${INCLUDES} -D_REENTRANT
dynamic:=CFLAGS= -PIC ${INCLUDES} -D_REENTRANT 
dbg:=CFLAGS= -PIC -g ${INCLUDES} -D_REENTRANT 


SRC_F77= dssLocal.f 
OBJ_F77=${SRC_F77:%.f=%.o} 

FFLAGS=-e -Nl99 -dalign -PIC -g
opt:=FFLAGS= -e -Nl99 -dalign -fast -PIC -O3 
dbg:=FFLAGS= -e -Nl99 -dalign -PIC -g
LFLAGS = -g -G
opt:=LFLAGS= -G 
dbg:=LFLAGS= -g -G 

LOPTS= -Bdynamic -lC -lF77 -lsunmath -lc -lM77
opt:=LIBALL = ../libdss.so
dbg:=LIBALL = ../libdss_g.so

#library definitions
LIB=lib.a
dbg:=LIBDSS=$(HEC_DBG_LIB)
opt:=LIBDSS=$(HEC_LIB)

opt:=TARGET=opt
dbg:=TARGET=dbg

.KEEP_STATE:

opt dbg: ${LIB}
#	CC -c -g testMain.cc
#	${LINKER} -nolib -o test testMain.o \
#	-L ../source/ -lPTM -lC -lc -lF77 -lsunmath -lm


${LIB}: ${OBJ_CC} ${OBJ_F77}
	${LINKER} ${LFLAGS} -o ${LIBALL} ${OBJ_F77} ${OBJ_CC} \
	${LIBLOC} ${LIBDSS} \
	${LOPTS};

FORCE:	;

patch:	
	rtc_patch_area -o patch.o -size 2000000

rectype: dbg
#	${F77} ${FFLAGS} rectype.f
	${LINKER} ${LFLAGS} -g -o rectype rectype.f ${OBJ_F77} ${OBJ_CC} \
	${LIBLOC} ${LIBDSS} \
	${LOPTS};

dss_retriever: dbg
#	${F77} ${FFLAGS} dss_retriever.f
	${LINKER} ${LFLAGS} -g -o dss_retriever dss_retriever.f ${OBJ_F77} ${OBJ_CC} \
	${LIBLOC} ${LIBDSS} \
	${LOPTS};

test: dbg
	${F77} ${FFLAGS} -c testDSS.f
	${LINKER} ${LFLAGS} -g -o testdss testDSS.f ${OBJ_F77} ${OBJ_CC} \
	${LIBLOC} ${LIBDSS} \
	${LOPTS};

vista_db_dss_DSSDataReader.o: vista_db_dss_DSSDataReader.c
	${CC} ${CFLAGS} -c vista_db_dss_DSSDataReader.c

vista_db_dss_DSSDataReader.c: ../DSSDataReader.java
	${JAVAH} -classpath ${VISTA_CLASS_DIR}:${JAVA_HOME}/lib/classes.zip -jni vista.db.dss.DSSDataReader

vista_db_dss_DSSDataWriter.c: ../DSSDataWriter.java
	${JAVAH} -classpath ${VISTA_CLASS_DIR}:${JAVA_HOME}/lib/classes.zip -jni vista.db.dss.DSSDataWriter
