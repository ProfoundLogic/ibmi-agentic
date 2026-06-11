     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SCCV0R- TAKE PROCESSED RECORDS.  COPY BACK TO 1) OPTICAL 2) IFS
     F*** SHOULD BE ONE FOR ONE... ALL DOCS FROM OPTICAL SHOULD BE ON DASD NOW
     F*** NOW USING ORIGINAL OPTICAL NAMES (DLDCID) PUT ON QOPT AND TO IFS
     F*** ITEMS ON THE IFS SHOULD BE ABLE TO BE COPIED BACK TO OPTICAL DIRECTLY,
     F**   BUT!!! MAY NEED TO COPY TO QDOC FIRST, THEN TO QOPT???
      * * *
      *---- PRE PROCESS PROGRAM TO RUN BEFORE THIS PROGRAM
      * * *   PROGRAM WILL BUILD THE DIRECTORIES FOR THE VOLUME
      * * *
      *---- READ/LOOP BY VOLUME ID FOR BOTH C1 AND C2 FILE
      * * *   A)  IS DOC/OBJ IN QDLS (NOCM ARE ONLY ONES IN IFS)
      * * *          CALL PROGRAM TO PURGE TO OPTICAL
      * * *          CALL PROGRAM TO PURGE TO IFS
      * * *          UPDATE RECORD WITH "P" (PURGED)
      * * *
      * * *   B)  DOC/OBJ NOT IN QDLS
      * * *          CALL PROGRAM TO COPY FROM IFS TO QOPT
      * * *          UPDATE RECORD WITH "P" (PURGED)
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      *
      ***************************************************************
      *  REVISIONS:
     H***************************************************************
     FSXC1L2    UF   E           K DISK
     F*
     FSXC2L2    UF   E           K DISK
     F*
     F*
     F*
     F*
     F*

     D ER1             S             40    DIM(3) CTDATA PERRCD(1)
     D ERM             S             28    DIM(6) CTDATA PERRCD(1)
     D*
      *
     D @@FPTH          DS
     D  @FPRE                  1      9
     D  SVOLPT                10     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D @@TPTH          DS
     D  @TPRE                  1      5
     D  TVOLPT                 6     80
     D @@TPT2          DS
     D  @TPR2                  1      9
     D  TVOLP2                10     80
     D TPTHLN          DS
     D  TPTHL                  1      4B 0
     D*
     D CPYINF          DS
     D  TYPOFC                 1      1
     D  CPYFIL                 2      6
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D                 DS
     D  OCOSBD                 1     12
     D  ##SUBD                 1      8
     D  ##PRI2                 9      9
     D  ##CSBD                10     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * * *  END PROGRAM CONTROL DATA AREA (NON BLANK MEANS END PROGRAM)
      * * *    X#FLG = THIS PROGRAM CONTROL  X#FLG2 = DELETE PROCESS CTL
     D SCCV           UDS
     D  X#FLG                  1      1
     D  X#FLG2                 2      2
     D  X#BLNK                 3    256
     D*
     D                 DS
     D  ERDS                   1     40
     D  ERPGM                  1      8
     D  ERBLK1                 9      9
     D  ERDESC                10     40
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
     C*** DARTS RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    DLKEY
     C                   KFLD                    DLSEQ#
      *
     C     CMK1          KLIST
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLK           KLIST
     C                   KFLD                    DLKEY
     C                   KFLD                    DLSEQ#
     C*
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     OLK           KLIST
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
     C                   KFLD                    OLOFLR
     C                   KFLD                    OLOID
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     MDK           KLIST
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
     C                   KFLD                    OLOID
     C                   KFLD                    OLOFLR
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
      *
     C     PLCV8A        PLIST
     C                   PARM                    @@FPTH           80            FROM  PATH
     C                   PARM                    @@TPTH           80            TO    PATH
     C                   PARM      *BLANKS       RTCD              1            RETURN VALUES
      * @@FPTH (FROM  PATH) FORMAT IS:  /OPTMLB01/OOVL/OFLR/OSUB/OBJN
      * @@FPTH (TO    PATH) FORMAT IS:  /QOPT/OOVL/OFLR/OSUB/OBJN
     C*
      *** PARM LIST FOR CALL TO OPTICAL COPY PROGRAM
      *
     C     PLOPT         PLIST
     C                   PARM                    SPATH            63
     C                   PARM                    TPATH            63
     C                   PARM                    QMSGD           256
     C                   PARM                    QQRTCD            1
      *
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQVOL            10
     C                   PARM                    TOVOL1           10
     C                   PARM                    QQRTN             1
      *
      *
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C* *** SET TO FIRST "C" RCD FOR THE BEGINING VOLUME
     C     QQVOL         SETLL     PC101
     C     QQVOL         READE     PC101                                  60
     C*** CONTINUE PROCESSING AS LONG AS WE ARE WITHIN THE BEGIN/END
     C*** VOLUMES
     C     *IN60         DOWEQ     '0'
     C*
     *** CHECK FOR END OF JOB REQUEST (USE DATA AREA FLAG?)
     ***       CLEAR FLAG, END PGM
     C                   IN        SCCV
     C     X#FLG2        IFNE      *BLANK
     C     *LOCK         IN        SCCV
     C                   MOVE      *BLANK        X#FLG2
     C                   OUT       SCCV
     C                   GOTO      TERM
     C                   ENDIF
     ***
     C* BYPASS THE RECORD IF ALREADY PROCESSED... EITHER IT IS "C" CONVERTED
     C*** OR THE ERROR FLAG IS SET IN ORDER TO USE THE RECORD.
     C*               LOOK FOR "U"???????? OR WE DONT CARE??? NOT ERRORS??
     C     CVPRCS        IFNE      'C'
     C                   GOTO      NXTONE
     C                   ENDIF
     C**** IF THERE WAS NO CM RECORD ON CONVERSION, ONLY UPDATE THE DL
     C***                THIS IS WHERE WE WILL GET DIRECTLY FROM THE
     C*&*********        IFS TO QOPT AS IT ALREADY EXISTS ON THE IFS
     C********
     C     CVERR         IFEQ      ER1(1)
     ** HERE'S WHERE WE GET ITEM FROM IFS TO QOPT
     **
     C*            SCCV9C?   TO=80 FROM = 80  RTN=1
     C*
     C                   MOVEL     '/'           @FPRE
     C                   MOVE      'OPTMLB01'    @FPRE
     C                   MOVEL     '/QOPT'       @TPRE
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    DLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLDCID:0      SVOLPT
     C*
     C***  CREATE THE TO     PATH
     C     '/'           CAT(P)    TOVOL1:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    DLOFLD:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    DLOSBD:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    DLDCID:0      TVOLPT
     C****
     C                   CALL      'SCCV9C'      PLCV8A
     C*** SOME ERROR OCCURED
     C     RTCD          IFNE      *BLANKS
     *** ERRORS UPDATE THE SXC1P RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV9C  '    ERPGM
     C******************   MOVELERM,6     ERDESC
     C******************** MOVELERDS      CVERR
     C                   MOVEL     'P'           CVEFLG
     C                   UPDATE    PC101
     C*
     C                   GOTO      NXTONE
     C                   ENDIF
     C                   GOTO      UPDAT1
     C                   ENDIF                                                    END CVERR=,1
     **** HERES WHWER WE DO THE COPY FROM QDLS TO QOPT
     C*    MUST DO PURGE LIKE SC856C????
     C***
     C***  CREATE THE SRC PATH
     C     '/'           CAT(P)    CVNFLD:0      SPATH
     C                   CAT(P)    '/':0         SPATH
     C     CMIMGF        IFEQ      'Y'
     C                   CAT(P)    CVNDOC:0      SPATH
     C                   ELSE
     C                   CAT(P)    CVNDOC:0      SPATH
     C                   ENDIF
     C*
      *
      *
     C***  CREATE THE TARGET PATH
     C     '/'           CAT(P)    TOVOL1:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    DLOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    DLOSBD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    DLDCID:0      TPATH
     C*
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
      *
     C*** CALL PROGRAM TO COPY THE DLS FILE TO OPTICAL VIA COPY STREAM FILE
     C*** API'S
     C*
     C                   CALL      'CPYTOOPT'    PLOPT
     C*
      ** ** IF UNSUCESSFUL
     C     QQRTCD        IFNE      *BLANK
     C***
     C*** HANDLE OTHER ERRORS AS NEEDED.
     C                   CLEAR                   ERDS
     C                   MOVEL     'CPYTOOPT'    ERPGM
     C                   MOVE      'P'           CVEFLG
     C                   MOVEL     QMSGD         ERDESC
     C                   UPDATE    PC101
     C*
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
      *
     C     UPDAT1        TAG
     C*** UPDATE AS COMPLETED  "P" IN THE CVPRCS FIELD :
     C**
     C******************** MOVEL*BLANKS   CVERR
     C                   MOVEL     *BLANKS       CVEFLG
     C                   MOVEL     'P'           CVPRCS
     C                   UPDATE    PC101
     C**
     C     NXTONE        TAG
     C     QQVOL         READE     PC101                                  60
     C                   ENDDO
     C**********************************************************
     C****** NO PROCESS THE OL/MD RECORDS!!!!
     C*
      *** READ EACH RECORD SELECTED FOR PURGE.
     C* *** SET TO FIRST "C" RCD FOR THE BEGINING VOLUME
     C     QQVOL         SETLL     PC201
     C     QQVOL         READE     PC201                                  60
     C*** CONTINUE PROCESSING AS LONG AS WE ARE WITHIN THE BEGIN/END
     C*** VOLUMES
     C     *IN60         DOWEQ     '0'
     C*
     *** CHECK FOR END OF JOB REQUEST (USE DATA AREA FLAG?)
     ***       CLEAR FLAG, END PGM
     C                   IN        SCCV
     C     X#FLG2        IFNE      *BLANK
     C     *LOCK         IN        SCCV
     C                   MOVE      *BLANK        X#FLG2
     C                   OUT       SCCV
     C                   GOTO      TERM
     C                   ENDIF
     ***
     C* BYPASS THE RECORD IF ALREADY PROCESSED... EITHER IT IS "C" CONVERTED
     C*** OR THE ERROR FLAG IS SET IN ORDER TO USE THE RECORD.
     C*
     C     CVPRCS        IFNE      'C'
     C                   GOTO      NXTON1
     C                   ENDIF
     C**** IF THERE WAS NO MD RECORD ON CONVERSION, ONLY UPDATE THE OL
     C***
     C     CVERR         IFEQ      ER1(2)
     C*** THIS HAD NO CM RECORD
     C*** NOW GO FROM IFS TO OPTICAL DIRECTLY
     C*
     C                   MOVEL     '/'           @FPRE
     C                   MOVE      'OPTMLB01'    @FPRE
     C                   MOVEL     '/QOPT'       @TPRE
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    OLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLDCID:0      SVOLPT
     C*
     C***  CREATE THE TO     PATH
     C     '/'           CAT(P)    TOVOL1:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    OLOFLD:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    OLOSBD:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    OLDCID:0      TVOLPT
     C****
     C                   CALL      'SCCV9C'      PLCV8A
     C*** SOME ERROR OCCURED
     C     RTCD          IFNE      *BLANKS
     *** ERRORS UPDATE THE SXC1P RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV9C  '    ERPGM
     C                   MOVEL     ERM(6)        ERDESC
     C******************** MOVELERDS      CVERR
     C                   MOVEL     'P'           CVEFLG
     C                   UPDATE    PC201
     C*
     C                   GOTO      NXTON1
     C                   ENDIF
     C                   GOTO      UPDAT2
     C                   ENDIF                                                     NO MD ERR END
     C***
     C***   HERE WE DO FROM QDLS TO QOPT
     C***
     C***  CREATE THE SRC PATH
     C     '/'           CAT(P)    CVNFLD:0      SPATH
     C                   CAT(P)    '/':0         SPATH
     C                   CAT(P)    CVNDOC:0      SPATH
     C*
      *
     C***  CREATE THE TARGET PATH
     C     '/'           CAT(P)    TOVOL1:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OLOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OLOSBD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OLDCID:0      TPATH
     C*
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
      *
     C*** CALL PROGRAM TO COPY THE DLS FILE TO OPTICAL VIA COPY STREAM FILE
     C*** API'S
     C*
     C                   CALL      'CPYTOOPT'    PLOPT
     C*
      ** ** IF UNSUCESSFUL
     C     QQRTCD        IFNE      *BLANK
     C***
     C*** HANDLE OTHER ERRORS AS NEEDED.
     C                   CLEAR                   ERDS
     C                   MOVEL     'CPYTOOPT'    ERPGM
     C                   MOVE      'P'           CVEFLG
     C                   MOVEL     QMSGD         ERDESC
     C                   UPDATE    PC201
     C*
     C                   GOTO      NXTON1
     C                   ENDIF
     C*
      *
     C     UPDAT2        TAG
     C*** UPDATE AS COMPLETED  "P" IN THE CVPRCS FIELD :
     C**
     C******************** MOVEL*BLANKS   CVERR
     C                   MOVEL     *BLANKS       CVEFLG
     C                   MOVEL     'P'           CVPRCS
     C                   UPDATE    PC201
     C**
      *
     C     NXTON1        TAG
     C     QQVOL         READE     PC201                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
**
01 - NO CM RCD FOUND
02 - NO MD RCD FOUND
SCCV0R
**
ERR GET NEW NAM FOR DCID/FLR
ERR PRINT DOC
ERR CREATE PDF FROM SPLF
ERR ON CPY QOPT TO QDLS/NCVT
CVT ERR - TRY NO CONVERT
ERR ON CPY IFS TO QOPT/NOCM
