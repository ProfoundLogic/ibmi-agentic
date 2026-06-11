     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F**********SMDLL1  IF  E           K        DISK
     FCVDOL1    UF   E           K DISK
     FCVOUT     O    E             DISK
     F*
     F*

     D CVIZXO          DS
     D  @@OSYS                 1      1
     D  @@VOL1                 2     11
     D  @@OFLD                12     19
     D  @@OSBD                20     31
     D  @@DCID                32     43
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDT                   7     12  0
     D*
*****D*
     D*
      *-------------------------------------------*
      *    RETCD HAS THE FOLLOWING INFO           *
      *-------------------------------------------*
      *** INPUT    BINARY(4)     BYTES PROVIDED   -   0 =EXCEPTION HANDLING
      *                                            else =Error Info rtnd
      *
      *** OUTPUT   BINARY(4)     BYTES AVALIBLE   -   0 =No error was rtrnd
      *                                            else =Length of data rtnd
      *
      *** OUTPUT   CHAR(7)       EXCEPTION ID           =Message ID
      *
      *** OUTPUT   CHAR(1)       RESERVED               =nothing
      *
      *** OUTPUT   CHAR(*)       EXCEPTION DATA         =error data rtnd
      *
      *-------------------------------------------*
      *
     D RETCD           DS
     D****************************        B   1   40RCLEN
     D***************************         B   5   80RTCD
     D  RTCD                   1      8
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  MSG                   17    272
     D*
     D OPTDIR          C                   CONST('/OPTMLB01')
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     OLKY          KLIST
     C                   KFLD                    @@OSYS            1
     C                   KFLD                    @@VOL1           10
     C                   KFLD                    @@OFLD            8
     C                   KFLD                    @@OSBD           12
     C                   KFLD                    @@DCID           12
     C*
      *
     C     PLOPT         PLIST
     C                   PARM                    #$DCID           12
     C                   PARM                    SPATH            80
     C                   PARM                    TPATH            80
     C                   PARM                    RETCD
     C                   TIME                    DSDTTM
      *
     *** GET DTAARA FOR SET LIMITS ON OL FILE
     C     *DTAARA       DEFINE                  CVIZXO           43
     C     *LOCK         IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *OFF
     C*
     C                   ELSE
     C*** GOTO END OF PROGRAM HERE !
     C                   MOVEL     'NODTAARA'    @@MSG
     C                   MOVE      DSDT          @@DT
     C                   MOVE      DSTIME        @@TIME
     C                   WRITE     PVO01
     ****
     ****
     C                   MOVEL     'E'           CVSTAT
     C                   MOVEL     @@MSG         CVERRD
     C                   UPDATE    PCV01
     C*
     C                   GOTO      TERM
     C***
     C                   ENDIF
     C**
     *** GET GET/LOOP THROUGH THE OL RCDS
     C     OLKY          SETLL     PCV01
     C                   READP     PCV01                                  60
     C     *IN60         DOWEQ     *OFF
     **** *** GOTO END OF PROCESS WHEN WE HIT THE DL RECORDS IN THE CONVERSINO F
     C*********  OLKEY     CABEQ*BLANKS   TERM  *******************
     C*
     C                   MOVE      *BLANKS       #$DCID
     C*** PREPARE PATH VARIABLES
     C                   EXSR      PRPPTH
     C*
     C*
     C*** VALIDATE PATH
     C                   MOVE      *BLANKS       CURPTH
     C                   CAT(P)    OPTDIR:0      CURPTH           80
     C                   CAT(P)    #PATH:0       CURPTH
     C     ##SPTH        IFNE      CURPTH
     C                   EXSR      VDLPTH
     C                   ENDIF
     C** CALL CPY PRGM TO PUT ON IFS
     C*
     C                   MOVE      *BLANKS       RTCD
     C                   CALL      'CVTIZXC1'    PLOPT
     C***
     C     RTCD          IFNE      *BLANKS
     C*** ERROR OCCURED, LOG IT AND CONTINUE?
     C*** CPFA0A0 IS OK, OBJNAME ALREADY EXISTS
     C*
     C                   MOVEL     OLOSYS        @@OSYS
     C                   MOVEL     OLVOL1        @@VOL1
     C                   MOVEL     OLOFLD        @@OFLD
     C                   MOVEL     OLOSBD        @@OSBD
     C                   MOVEL     OLDCID        @@DCID
     ****
     C                   MOVEL     OLKEY         @@KEY
     C                   MOVE      OLSEQ#        @@SEQ#
     C                   MOVEL     OLOFLR        @@OFLR
     C                   MOVEL     OLOID         @@OID
     ****
     C*
     C                   MOVEL     MSG           @@MSG
     C                   MOVEL     RTCD          @@RTCD
     C                   MOVEL     CONDTN        @@MSGI
     C                   MOVE      DSDT          @@DT
     C                   MOVE      DSTIME        @@TIME
     C                   WRITE     PVO01
     ****
     C                   MOVEL     'E'           CVSTAT
     C                   MOVEL     MSG           CVERRD
     C                   UPDATE    PCV01
     C*
     C*
     C                   ENDIF
     C***
     C**
     *** UPDATE THE DTAARA WITH LAST PROCESSED VALUE IN CASE RESTART
     C*
     C                   MOVEL     OLOSYS        @@OSYS
     C                   MOVEL     OLVOL1        @@VOL1
     C                   MOVEL     OLOFLD        @@OFLD
     C                   MOVEL     OLOSBD        @@OSBD
     C                   MOVEL     OLDCID        @@DCID
     ****
     C                   MOVEL     OLKEY         @@KEY
     C                   MOVE      OLSEQ#        @@SEQ#
     C                   MOVEL     OLOFLR        @@OFLR
     C                   MOVEL     OLOID         @@OID
     ****
     C*
     C     *LOCK         OUT       *DTAARA
     C*
     C                   MOVEL     *BLANKS       @@MSG
     C                   MOVEL     *BLANKS       @@RTCD
     C                   MOVEL     *BLANKS       @@MSGI
     C                   MOVE      DSDT          @@DT
     C                   MOVE      DSTIME        @@TIME
     C                   WRITE     PVO01
     ****
     C     CVSTAT        IFNE      'E'
     C                   MOVEL     'C'           CVSTAT
     C                   MOVEL     *BLANKS       CVERRD
     C                   UPDATE    PCV01
     C                   ENDIF
     C                   MOVEL     *BLANKS       CVSTAT
     C                   MOVEL     *BLANKS       CVERRD
     C*
     C***
     C                   READP     PCV01                                  60
     C                   ENDDO                                                      DOW 60 OFF
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C**********************************************************************
     C     PRPPTH        BEGSR
     C*
     C*
     C                   MOVE      *BLANKS       #PATH
     C***  CREATE THE TARGET PATH
     C     '/'           CAT(P)    OLVOL1:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OLOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OLOSBD:0      TPATH
     C                   MOVEL(P)  TPATH         #PATH            80
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OLDCID:0      TPATH
     C                   MOVEL     OLDCID        #$DCID
     C***  CREATE THE SRC PATH
     C*
     C                   MOVEL(P)  TPATH         SPATH
     C*
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
      *
     C*
     C                   ENDSR
     C*
     C***********************************************
     C     VDLPTH        BEGSR
     C**** PATHS TO VALIDATE *****
     C                   MOVE      *BLANKS       XXVAL
     **
     C                   CAT(P)    OPTDIR:0      XXVAL            80
     C                   CAT(P)    '/':0         XXVAL
     C                   CAT(P)    OLVOL1:0      XXVAL
     C*
     C*** VALIDATE EXISTANCE OF VOLUME
     C                   CALL      'SVALIFS'
     C                   PARM                    XXVAL
     C                   PARM      ' '           XXRTC             1
     C*
     C*** IF RTC THEN BAD, MUST CREATE ***
     C     XXRTC         IFNE      *BLANK
     C                   EXSR      CRTDIR
     C                   ENDIF
     C*
     C                   CAT(P)    '/':0         XXVAL
     C                   CAT(P)    OLOFLD:0      XXVAL
     C** VALIDATE EXISTANCE OF FOLDER
     C*
     C                   CALL      'SVALIFS'
     C                   PARM                    XXVAL
     C                   PARM      ' '           XXRTC             1
     C*
     C*** IF RTC THEN BAD, MUST CREATE ***
     C     XXRTC         IFNE      *BLANK
     C                   EXSR      CRTDIR
     C                   ENDIF
     C*
     C*
     C                   CAT(P)    '/':0         XXVAL
     C                   CAT(P)    OLOSBD:0      XXVAL
     C** VALIDATE EXISTANCE OF FOLDER
     C*
     C                   CALL      'SVALIFS'
     C                   PARM                    XXVAL
     C                   PARM      ' '           XXRTC             1
     C*
     C*** IF RTC THEN BAD, MUST CREATE ***
     C     XXRTC         IFNE      *BLANK
     C                   EXSR      CRTDIR
     C                   ENDIF
     C***
     C                   MOVE      *BLANKS       ##SPTH           80
     C                   MOVEL     XXVAL         ##SPTH
     C*
     C*
     C*
     C                   ENDSR
     C*
     C***********************************************
     C     CRTDIR        BEGSR
     C                   CALL      'SMDIFS'
     C                   PARM                    XXVAL
     C                   PARM      ' '           XXRTC
     C*
     C     XXRTC         IFNE      *BLANKS
     C*** ERROR OCCURED ON CREATE DIRECTORY... MUST END HERE!!!
     C                   MOVEL     OLOSYS        @@OSYS
     C                   MOVEL     OLVOL1        @@VOL1
     C                   MOVEL     OLOFLD        @@OFLD
     C                   MOVEL     OLOSBD        @@OSBD
     C                   MOVEL     OLDCID        @@DCID
     C*
     C                   MOVEL     XXRTC         @@RTCD
     C                   MOVEL     'MKDIR'       @@MSGI
     C*
     C                   MOVEL     XXVAL         @@MSG
     C                   MOVE      DSDT          @@DT
     C                   MOVE      DSTIME        @@TIME
     C                   WRITE     PVO01
     ****
     C                   MOVEL     'E'           CVSTAT
     C                   MOVEL     @@MSG         CVERRD
     C                   UPDATE    PCV01
     C*
     C                   GOTO      TERM
     C                   ENDIF
     C**
     C                   ENDSR
