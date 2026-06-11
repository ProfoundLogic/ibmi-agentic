     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H******************************************************************
     H*  REVISION:                                                    *
     H*                                                               *
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H******************************************************************
     FSDCMP     IF   E             DISK
     FFIXDT1    O    E             DISK
     FFIXDT2    O    E             DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*

     D*
     D MSG             S             40    DIM(26) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*=====================================================================
     C*
     C                   Z-ADD     0             XXCNT             9 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*================================================================
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       XXERR
     C                   MOVE      *BLANKS       XXER1
     C                   MOVEL     *BLANKS       CMERR1
     C                   MOVEL     *BLANKS       CMERR2
     C*
     ***  DOCID/FOLDER NOT BLANK, IMGF = BLANKS (OV TYPE) , NOT DAT ENTRY TYPE
     ***
     C     CMDCID        IFNE      *BLANKS
     C     CMFOLD        ANDNE     *BLANKS
     C     CMIMGF        ANDEQ     *BLANKS
     C     CMFL01        ANDNE     'Y'
     C**
     C*
     **** RUN COPY DOC, TO TMPDTM FOLDER, THEN COPY BACK, THEN DELETE TMPDTM DOC
     ***  CHG AUTHORITIES, OWN, ETC...
     ***
     C**** CHECK EXISTINCE OF THE DLO
     C****
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C                   PARM      *BLANKS       X@RTNC            1
     C*
     C**** IF X@RTNC NE *BLANKS ,DOC NOT EXISTING!!!
     C*
     C     X@RTNC        IFNE      *BLANKS
     C                   MOVEL     MSG(26)       XXERR
     C                   ENDIF
     C*
     C     X@RTNC        IFNE      *BLANKS
     C                   MOVEL     'NOT PRC'     XXER1            30
     C*** WRITE ERROR  FILE
     C                   MOVEL     XXERR         CMERR1
     C                   MOVEL     XXER1         CMERR2
     C                   WRITE     FIXR2
     C***
     C                   GOTO      ERR1
     C                   ENDIF
     C*
     C****
     ***
     C* * * TO TEMP FOLDER 'TMPDTM'
     C                   CALL      'SCPYDOC'
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C                   PARM                    CMDCID
     C                   PARM      'TMPDTM'      TOFOLD           80
     C                   PARM      *BLANKS       QQRTCD            1
     C*
     C*** CHECK FOR RTCD
     C     QQRTCD        IFEQ      '1'
     C                   MOVEL     MSG(1)        XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVEL     MSG(2)        XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVEL     MSG(3)        XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVEL     MSG(4)        XXERR            40
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '5'
     C                   MOVEL     MSG(5)        XXERR
     C                   ENDIF
     C**
     C     QQRTCD        IFNE      *BLANKS
     C                   MOVEL     MSG(22)       XXER1            30
     C*** WRITE ERROR  FILE
     C                   MOVEL     XXERR         CMERR1
     C                   MOVEL     XXER1         CMERR2
     C                   WRITE     FIXR2
     C***
     C                   GOTO      ERR1
     C                   ENDIF
     C*
     C** DELETE ORIGINAL DOC IF NO ERROR!!!
     C*
     C                   CALL      'DLDLOC'
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C                   PARM      *BLANKS       Q1RTCD            2
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*** CHECK FOR RTCD
     C     Q1RTCD        IFEQ      ' 1'
     C                   MOVEL     MSG(6)        XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 2'
     C                   MOVEL     MSG(7)        XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 3'
     C                   MOVEL     MSG(8)        XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 4'
     C                   MOVEL     MSG(9)        XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 5'
     C                   MOVEL     MSG(10)       XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 6'
     C                   MOVEL     MSG(11)       XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 7'
     C                   MOVEL     MSG(12)       XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 8'
     C                   MOVEL     MSG(13)       XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      ' 9'
     C                   MOVEL     MSG(14)       XXERR            40
     C                   ENDIF
     C*
     C     Q1RTCD        IFEQ      '10'
     C                   MOVEL     MSG(15)       XXERR            40
     C                   ENDIF
     C*
     C**
     C     Q1RTCD        IFNE      *BLANKS
     C                   MOVEL     MSG(23)       XXER1            30
     C*** WRITE ERROR  FILE
     C                   MOVEL     XXERR         CMERR1
     C                   MOVEL     XXER1         CMERR2
     C                   WRITE     FIXR2
     C***
     C                   GOTO      ERR1
     C                   ENDIF
     C*
     C*
     **** NOW BACK TO ORIGINAL FOLDER (REPLACE)
     C*
     C                   CALL      'SCPYDOC'
     C                   PARM                    CMDCID
     C                   PARM      'TMPDTM'      FRFOLD           80
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C                   PARM      *BLANKS       QQRTCD            1
     C*
     C*** CHECK FOR RTCD
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVEL     MSG(17)       XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVEL     MSG(18)       XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVEL     MSG(19)       XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVEL     MSG(20)       XXERR            40
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '5'
     C                   MOVEL     MSG(21)       XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFNE      *BLANKS
     C                   MOVEL     MSG(24)       XXER1            30
     C*** WRITE ERROR  FILE
     C                   MOVEL     XXERR         CMERR1
     C                   MOVEL     XXER1         CMERR2
     C                   WRITE     FIXR2
     C***
     C                   GOTO      ERR1
     C                   ENDIF
     C*
     C*
     C***
     C*** NOW CHANGE AUTHORITIES TO DOC
     C                   CALL      'SYCHGDA'
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C                   PARM      *BLANKS       XXDOCD           44
     C                   PARM      '*NO '        XXALWR            4
     C*******************  PARM *BLANKS   QQRTCD  1
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*** NO RTN CODE ANYMORE
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C*** CHECK FOR RTCD
     C     QQRTCD        IFNE      *BLANK
     C                   MOVEL     MSG(16)       XXERR
     C                   ENDIF
     C*
     C     QQRTCD        IFNE      *BLANKS
     C                   MOVEL     MSG(25)       XXER1            30
     C*** WRITE ERROR  FILE
     C                   MOVEL     XXERR         CMERR1
     C                   MOVEL     XXER1         CMERR2
     C                   WRITE     FIXR2
     C***
     C                   GOTO      ERR1
     C                   ENDIF
     C*
     C*
     C**8
     C*** NOW DELETE EXISTING TMPDOC
     C*
     C                   CALL      'DLDLOC'
     C                   PARM                    CMDCID
     C                   PARM      'TMPDTM'      TOFOLD
     C                   PARM      *BLANKS       QQRTCD            1
     C*
     C*** CHECK FOR RTCD
     C***
     **** DONE!
     C     ERR1          TAG
     C*
     C                   MOVEL     CMKEY         XXKEY            20
     C                   MOVEL     CMCOMM        XXCOMM           20
     C                   MOVEL     CMFOLD        X1FOLD           30
     C*** WRITE SUCESS FILE
     C     XXERR         IFEQ      *BLANKS
     C                   MOVEL     XXERR         CMERR1
     C                   MOVEL     XXER1         CMERR2
     C                   WRITE     FIXR1
     C                   ENDIF
     C***
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                            8 'FIXDTM1 '
     O                                           68 'CHG DTM DOCS AUTHORI'
     O                                           86 'TY               '
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                                           28 'REMARK'
     O                                           35 'DIARY'
     O                                           44 'DIARY'
     O          E            HEAD           1
     O                                            9 'DARTS KEY'
     O                                           27 'CODE'
     O                                           35 'FLAG'
     O                                           48 'CMPLT DATE'
     O                                           58 'COMMENT'
     O                                           78 'SEQ#'
     O                                           88 'DCID    '
     O                                          101 'FOLDER   '
     O                                          130 'ERROR    '
     O          E            DET            1
     O                       XXKEY               21
     O                       CMRMCD              28
     O                       CMDIFL              33
     O                       CMDCDT              48 '    /  /  '
     O                       XXCOMM              71
     O                       CMSEQ#        4     78
     O                       CMDCID              92
     O                       X1FOLD             123
     O                       XXERR              166
     O                       XXER1              198
     O          E            TOT            1
     O                                           25 '# of records deleted:'
     O                       XXCNT         4     36
**
SCPYDOC - NOT AUTHORIZED TO DOC OR FLDR
SCPYDOC - FOLDER NOT FOUND
SCPYDOC - DOC NOT FOUND IN FOLDER
SCPYDOC - DOC NAME ALREADY EXISTS
SCPYDOC - UNABLE TO PROCESS RQST FOR DOC
DLDLOC  - RQSTR NOT ENROLLED IN DIST DIR
DLDLOC  - NO DOCS FOUND IN FLR
DLDLOC  - OTHER OWNR SPECIFIED IN OWNPRF
DLDLOC  - DOC/FLR IN USE
DLDLOC  - RQSTR DOESNT HAVE AUTHORITY
DLDLOC  - DLO DOES NOT EXIST
DLDLOC  - DOC IS CHECKED OUT
DLDLOC  - FLR NAME NOT CORRECT
DLDLOC  - FOLD DOESNT EXIST
DLDLOC  - ERROR PROCESSING DLTDLO CMD
SYCHGDA - ERR PROCESSNG CHG IN AUTHORITY
SCPYDOC1- NOT AUTHORIZED TO DOC OR FLDR
SCPYDOC1- FOLDER NOT FOUND
SCPYDOC1- DOC NOT FOUND IN FOLDER
SCPYDOC1- DOC NAME ALREADY EXISTS
SCPYDOC1- UNABLE TO PROCESS RQST FOR DOC
CPY FRM ORIG TO TEMP NOT PRCSD
DLT FRM ORIG FLR NOT PRCSD
CPY FRM TMP TO ORIG NOT PRCSD
CHG AUTHORITY NOT PRCSD
SVALDOC-DOC NOT EXISTING
