     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H**** USE CPY STREAM FILE THIS TIME
     F*
     H*  REVISIONS:                                                   *
     H****  02 - AOOPTICAL - DATACENTER PROCESSING TO BE ADDED FOR
     F***                    OPTICAL
     H**   10/10/06 -INCLUDE DATACENTER PREFIX PROCESSING
     H*                                                               *
     H*
      * CREATE A DIRECTORY ON OPTICAL *** VIRTUAL PROCESSING *****
     F** (FOR VRT... USE /OPTMLB01 AS THE "FILE SYSTEM NAME"
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**                  01/26/04    THE OPTICAL LIBRARY NAME
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************

     D NPATH           DS
     D  NPFSYS                 1      9
     D****                                    1   5 NPFSYS
     D  NVOLPT                10     80
     D*******                                 6  80 NVOLPT
     D NPTHLN          DS
     D  NPTHL                  1      4B 0
     D*
     D ATRTBL          DS
     D  ATR#A                  1      4B 0
     D ATRLEN          DS
     D  ATRL                   1      4B 0
     D*
     D*** CHANGE 1/26/04 *** GET LIBN FROM SMOCP INSTEAD
     D*** DEL       '/OPTMLB01'           C         OPTDIR
     D*
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

     I*

     C     PCRTSF        PLIST
     C                   PARM                    NPATH            80
     C                   PARM                    NPTHL
     C                   PARM                    ATRTBL
     C                   PARM                    ATRLEN
     C                   PARM                    RETCD
     C*
     C*
     C     PRMSG         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMDESC          256
     C                   PARM                    QMRTN             1
     C*
     C*
     C     PLBRK         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMRTN             1
     C*
     C*
     C     *ENTRY        PLIST                                                  "R"READ OR "W"-
     C                   PARM                    NPTH             63            FRM FLR/DOC   )
     C                   PARM                    MSGDSC          256            ERR MSG DSC
     C                   PARM                    QQRTCD            1            RTCD          )
     C*
     C                   Z-ADD     0             ATR#A
     C                   Z-ADD     4             ATRL
     C*
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*
     C     '/'           CAT(P)    QQLIBN:0      OPTDIR           11
     C*** 01/26/04 ** END ADD *************************************8
     C*
     C*
     C*** BEGIN ADD *** 10/10/06 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 10/10/06 **************************************
     C                   MOVE      *ZERO         RCLEN            15 5
     C******************   MOVE *ZERO     RTCD
     C                   MOVE      *BLANKS       RTCD
     C*
     C                   EXSR      CRTD
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C     CRTD          BEGSR
     C                   MOVEL     OPTDIR        NPFSYS
     C*****************    MOVEL'/QOPT'   NPFSYS
     C                   MOVEL     NPTH          NVOLPT
     C                   Z-ADD     80            NPTHL
     C                   Z-ADD     272           RCLEN
     C****C************    CALL 'QHFCRTDR'PCRTSF
     C**
     ******* ADD **** 10/10/06 *******************************************
     **  IF DATACENTER PROCESSING, WE SHOULD PREFIX THE PATHS
     **
     C     ##PFLR        IFNE      *BLANKS
     **
     C     '/'           CAT(P)    ##PFLR:0      @@WK3            80
     C                   CAT(P)    OPTDIR:0      @@WK3
     C                   CAT(P)    NVOLPT:0      @@WK3
     C                   MOVEL     @@WK3         NPATH
     C                   ENDIF
     **
     ** END  ADD **** 10/10/06 *******************************************
     C                   CALL      'VRTOPTDC'
     C                   PARM                    NPATH            80
     C                   PARM                    RETCD
      *
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      *BLANKS
     C                   MOVE      '1'           ERR               1
     C*** PROCESS ERRORS...RETURN THEM TO THE JOB LOG. ETC.
     C*
      * * * DEFAULT ERROR TO RETURN
     C                   MOVE      '3'           QQRTCD
     C*
      * * * VOLUME FULL(THRESHOLD MET)
     C     CONDTN        IFEQ      'CPF1F61'
     C                   MOVE      '1'           QQRTCD
     C                   END
     C*
      * * * PATH NAME INVALID
     C     CONDTN        IFEQ      'CPF1F48'
     C                   MOVE      '2'           QQRTCD
     C                   END
     C*
     C*** CALL RTV MSG PGM TO SND PGM MSG OF ERROR AND RETRIEVE THE
     C*** DESCRIPTION.
     C                   MOVEL     CONDTN        QMID
     C                   MOVEL     MSG           QMDTA
     C                   MOVEL     *BLANKS       QMDESC
     C                   MOVEL     *BLANKS       QMRTN
     C*
     C                   CALL      'RTVMSGC'     PRMSG
     C     QMRTN         IFEQ      *BLANKS
     C                   MOVE      QMDESC        MSGDSC
     C                   ELSE
     C                   MOVE      *BLANKS       MSGDSC
     C                   END
     C*
     C     CONDTN        IFEQ      'CPF1F71'                                          F SYS
     C                   CALL      'BRKMSGC'     PLBRK
     C*
     C     QMRTN         IFEQ      '*'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C                   END
     C     CPYEND        ENDSR
A    C*
