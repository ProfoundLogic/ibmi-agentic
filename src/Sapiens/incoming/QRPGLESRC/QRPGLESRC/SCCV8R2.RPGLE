     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SCCV8R2- DELETE PROCESS1 - UPDATE DARTS WITH NEW INFO
      *         AT BMICO PLATTERS CONVERTED LOCAL
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      *
      ***************************************************************
      *  REVISIONS:
      *                                                               *
      *   10/17/11 C:279 Project: SY195                               *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
     H***************************************************************
     FSXC1L3    UF   E           K DISK
     F*
     FSXC2L3    UF   E           K DISK
     F*
     FSDCMLE    UF   E           K DISK
     F*
     FSMDLL2    UF   E           K DISK
     F*
     FSMOLL1    UF   E           K DISK
     FSMMDL     IF   E           K DISK
     F*
     F*

     D ER1             S             40    DIM(3) CTDATA PERRCD(1)
     D*
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D  ERRTCD                10     10
     D  ERBLK2                11     11
     D  ERDESC                12     40
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
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
     C     MDK1          KLIST
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
     C                   KFLD                    OLOID
     C                   KFLD                    OLOFLR
     C*
      *
      *
      *
     C     SVK1          KLIST
     C                   KFLD                    SVVOL1
     C                   KFLD                    SVKEY
     C                   KFLD                    SVSEQ#
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QBVOL            10
     C                   PARM                    QEVOL            10
     C                   PARM                    QQRTN             1
      *
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
      *
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C* *** SET TO FIRST "C" RCD FOR THE BEGINING VOLUME
     C     QBVOL         SETLL     PC101
     C                   READ      PC101                                  60
     C*** CONTINUE PROCESSING AS LONG AS WE ARE WITHIN THE BEGIN/END
     C*** VOLUMES
     C     *IN60         DOWEQ     '0'
     C     DLVOL1        ANDLE     QEVOL
     C**** SAVE THE KEY TO RE-GET LATER ****
     C                   MOVEL     DLVOL1        SVVOL1           10
     C                   MOVEL     DLKEY         SVKEY            40
     C                   MOVE      DLSEQ#        SVSEQ#            5 0
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
     C     CVPRCS        IFNE      'P'
     C                   GOTO      NXTONE
     C                   ENDIF
     C**** IF THERE WAS NO CM RECORD ON CONVERSION, ONLY UPDATE THE DL
     C***
     C     CVERR         IFEQ      ER1(1)
     C*** THIS HAD NO CM RECORD
     C     NOCM1         TAG
     C     DLK           CHAIN     PDL01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     'I'           DLSTAT
     C                   MOVEL     'SCCV8R2 '    DLPGM
     C                   MOVEL     '@NOCMRCD'    DLUSER
     C                   UPDATE    PDL01
     C                   MOVEL     'U'           CVPRCS
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C                   ELSE
     C                   MOVEL     '2'           CVPRCS
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C                   ENDIF
     C                   ENDIF
     C*
     C**** CM EXISTS FROMORIGINAL CONVERT, PROCESS UPDATE TO THIS FILE
     C*** 1) GET CM RCD (KEY/SEQ# NO SELECT/OMIT)
     C*
     C     CMKY          CHAIN     PCM01                              62
     C*   PRCS = '1' ARE NO HIT ON CM RCD
     C*** IF NO HIT, PUT ERROR CODE IN THE PROCESS FLAG FOR LATER RETRIEVE.
     C*********
     C***  NO HIT MEANS THIS WAS COPIED DIRECTLY TO OPTICAL  - GOTO NOCM1
     C******
     C     *IN62         IFEQ      *ON
     C***
     C                   GOTO      NOCM1
     C****
     C                   ENDIF
     C*
     C****** VALID HIT ON CM, RE-GET THE SXC1P RECORD...
     C     SVK1          CHAIN     PC101                              68
     C     *IN68         IFEQ      *ON
     C****
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
     C**** MOVE CVNDOC AND NFLD INTO STANDARD FIELDS
     C                   MOVEL     CVNDOC        CMDCID
     C                   MOVEL     CVNFLD        CMFOLD
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     *BLANKS       CMIFSP
     C                   MOVEL     *BLANKS       CMUNCN
      *
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMDCID:0      CMUNCN
     C                   MOVEL     ENV           CMIFSP
     C                   ELSE
     C                   MOVEL     *BLANKS       CMUNCN
     C                   MOVEL     *BLANKS       CMIFSP
     C                   ENDIF                                                  CMFOLD IFNE ' '
      *
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C* *** IF NO CONVERT OCCURED FLAG AS "OTHER " OBJECT TYPE
     C     CVERR         IFEQ      ER1(3)
     C     CMIMGF        OREQ      *BLANK
     C                   MOVE      'O'           CMIMGF
     C                   ENDIF
     C***8 THE ABOVE IS ALREADY DONE IN THE SXC1 FILE, SO THE "GET" UPDATES THE
     C***8 FIELDS****   SHOULDN'T NEED THE ABOVE...
     C*
     C*
     C                   MOVEL     'SCCV8R2'     CMPGM
     C***
     C                   UPDATE    PCM01
     C***
     C*** NOW THE DL RECORD**** JUST PGM INFO
     C     DLK           CHAIN     PDL01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     'SCCV8R2 '    DLPGM
     C                   UPDATE    PDL01
     C                   MOVEL     'U'           CVPRCS
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C                   ELSE
     C                   MOVEL     '3'           CVPRCS
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C                   ENDIF
     C***
      *
     C     NXTONE        TAG
     C                   READ      PC101                                  60
     C                   ENDDO
     C**********************************************************
     C****** NO PROCESS THE OL/MD RECORDS!!!!
     C*
      *** READ EACH RECORD SELECTED FOR PURGE.
     C* *** SET TO FIRST "C" RCD FOR THE BEGINING VOLUME
     C     QBVOL         SETLL     PC201
     C                   READ      PC201                                  60
     C*** CONTINUE PROCESSING AS LONG AS WE ARE WITHIN THE BEGIN/END
     C*** VOLUMES
     C     *IN60         DOWEQ     '0'
     C     OLVOL1        ANDLE     QEVOL
     C*** SAV VALS
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
     C     CVPRCS        IFNE      'P'
     C                   GOTO      NXTON1
     C                   ENDIF
     C**** IF THERE WAS NO MD RECORD ON CONVERSION, ONLY UPDATE THE OL
     C***
     C     CVERR         IFEQ      ER1(2)
     C*** THIS HAD NO MD RECORD
     C*** OR IF CHAIN TO MD AND NO HIT....
     C*
     C     NOMD1         TAG
     C*
     C     OLK           CHAIN     POL01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     'I'           OLSTAT
     C                   MOVEL     'SCCV8R2 '    OLPGM
     C                   MOVEL     '@NOMDRCD'    OLUSER
     C                   UPDATE    POL01
     C                   MOVEL     'U'           CVPRCS
     C                   UPDATE    PC201
     C                   GOTO      NXTON1
     C                   ELSE
     C                   MOVEL     '4'           CVPRCS
     C                   UPDATE    PC201
     C                   GOTO      NXTON1
     C                   ENDIF
     C                   ENDIF
     C*
     C     MDK1          CHAIN     PMD01                              67
     C     *IN67         IFEQ      *ON
     C                   GOTO      NOMD1
     C                   ENDIF
     C*
     C**** CM EXISTS FROMORIGINAL CONVERT, PROCESS UPDATE TO THIS FILE
     C*** 1) GET MD RCD (
     C***** NO NEED TO GET MD RECORD??? NO FLAG TO ADJUST IN THERE
     C***8
     C***
     C*** NOW THE OL RECORD****
     C     OLK           CHAIN     POL01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     'SCCV8R2 '    OLPGM
     C                   UPDATE    POL01
     C                   MOVEL     'U'           CVPRCS
     C                   UPDATE    PC201
     C                   GOTO      NXTON1
     C                   ELSE
     C                   MOVEL     '6'           CVPRCS
     C                   UPDATE    PC201
     C                   GOTO      NXTON1
     C                   ENDIF
     C***
      *
     C     NXTON1        TAG
     C                   READ      PC201                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
**
01 - NO CM RCD FOUND
02 - NO MD RCD FOUND
SCCV1C   4 CVT ERR - TRY NO CONVERT
