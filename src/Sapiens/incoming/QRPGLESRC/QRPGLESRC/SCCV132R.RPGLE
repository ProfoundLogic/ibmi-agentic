     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SCCV1R - CONVERT DOCS TO .PDF FORMAT FOR DARTS
      *
      **** REMEBER, IF CLIENT HAS VDARTS, MORE WORK MUST BE DONE
      ****  IN THESE PGMS, AS OTHER OBJS. ALREADY EXIST IN FOLDERS.
      ****   THE SELECT PROCESS TO BUILD THE WRK FILE MUST KNOW THIS
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      **** SCCV2R (COPIED FROM IS828R)      /***  NOT NEEDED *** REMOVED ***
      *
      ***************************************************************
      *  REVISIONS:
     *                                                                *
     * 11/02/10  C:183 Project: SY195                                 *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to*
     *                 get the root folder name.                      *
     *                                                                *
      *  12/28/07     CI055     180     Create historical PDFs.
      *                                 Cloned from CVT133DEI/SCCV133R
      *
     H***************************************************************
     FSXCVL     UF   E           K DISK
     F*

     D*
     D ERM             S             28    DIM(5) CTDATA PERRCD(1)
     D MSG             S             80    DIM(5) CTDATA PERRCD(1)
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
      ******* BEGIN ADD *********** 12/28/07 ******************
     D DFTFLR          DS
     D  DFTF1                  1      6
     D  DFTF2                  7      9
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3
      ******* END ADD ************* 12/28/07 ******************
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************
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
     C     CVKY          KLIST
     C                   KFLD                    XXKEY            40
      *
     C*
     C*** DARTS RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    XXKEY            40
     C                   KFLD                    XXSEQ#            5 0
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLKY          KLIST
     C                   KFLD                    XXKEY            40
     C                   KFLD                    XXSEQ#            5 0
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *** PARM LIST FOR CALL TO GET NEW FILE NAME/FOLDER
      *
     C     PLCV2         PLIST
     C                   PARM      QQWSID        XXWSID            2
     C                   PARM      '000000'      XXSEQ             6
     C                   PARM      'PDF'         XXSUFX            3
     C                   PARM      '0000'        XXBCHS            4
     C                   PARM      *BLANKS       XXDCID           12
     C                   PARM      *BLANKS       XXBCHI           12
     C                   PARM      *BLANKS       XXFLR             8
     C                   PARM      *BLANKS       XXSFLR           12
     C                   PARM      *BLANKS       XXNPTH           21
     C                   PARM      *BLANK        XXRTCD            1
     C                   PARM      QQPRE         XXPRE            13
      *
      *** PARM LIST FOR CALL TO PRTDOC PROGRAM
      *
     C     PLCV1         PLIST
     C                   PARM      CVDCID        ZZDCID           12
     C                   PARM                    ZZFOLD           80
     C                   PARM      CVKEY         ZZKEY            40
     C                   PARM                    ZZSEQ#            5
     C                   PARM      CVFL05        ZZOPTF            1
     C                   PARM      *BLANK        ZZRTCD            1
     C                   PARM      'QPRINT'      ZZOUTQ           10
     C                   PARM      '001'         ZZCPYS            3
     C                   PARM      *BLANKS       ZZSNAM           10
     C                   PARM      '00000'       ZZSNUM            5
     C                   PARM      *BLANKS       ZZJNAM           10
     C                   PARM      '000000'      ZZJNUM            6
     C                   PARM      *BLANKS       ZZJUSR           10
      *
      *
      *** PARM LIST FOR CALL TO PRTDOC PROGRAM
      *
     C     PLPDF         PLIST
     C                   PARM                    ZZSNAM           10            SPLF NAME
     C                   PARM                    ZZJNUM            6            JOB NUM
     C                   PARM                    ZZJUSR           10            JOB USR
     C                   PARM                    ZZJNAM           10            JOB NAME
     C                   PARM                    @@SNUM            6            SPLF NUM
     C                   PARM                    @@SPTH           80            STORE PATH
     C                   PARM                    @@RTCD            1            RTN CODE
      * @@SPTH (STORE PATH) FORMAT IS:  /DIR/SUBDIR/OBJ.PDF
      *
      ***** BEGIN ADD ************* 12/28/07 **************************
     C     PLCVC         PLIST
     C                   PARM                    ZZSNAM           10            SPLF NAME
     C                   PARM                    ZZJNUM            6            JOB NUM
     C                   PARM                    ZZJUSR           10            JOB USR
     C                   PARM                    ZZJNAM           10            JOB NAME
     C                   PARM                    @@SNUM            6            SPLF NUM
     C                   PARM                    @@RTCD            1            RTN CODE
      ***** END ADD   ************* 12/28/07 **************************
      *
      *
      *** PARM LIST FOR CALL TO MESSAGE PGM
      *
     C     PLCV3         PLIST
     C                   PARM                    QQMSG1           80            MSG TO SND
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2
     C                   PARM                    QQPRE            13
      *** Beg chg ***** 11/02/10 ***********************************************
      * Call the Root determination program
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
     C********   '\QDLS\'  CAT  QQPRE     DLSPRE 19 P
     C     '\'           CAT(P)    ROOT          RPATH            12
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    QQPRE:0       DLSPRE           19
      *** End chg ***** 11/02/10 ***********************************************
      *
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
      ****** BEGIN ADD  ****** 12/28/07 *************************
      * CHECK FOR DATA CENTER
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUF             3
     C                   PARM      *BLANKS       ##PFLR           80
      *
     C     ##PFLR        IFGT      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         ##PFLR
     C                   ENDIF
      ****** END   ADD  ****** 12/28/07 *************************
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C                   READ      PCV01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     COUNT         IFGE      9900
     C                   MOVEL     MSG(1)        QQMSG1
     C*** CAN'T CREATE MORE SPLFS THAN THIS, SO RESTART
     C                   CALL      'SCCV3C'      PLCV3
     C*
     C                   MOVEL     'R '          QQWSID
     C                   GOTO      TERM
     C***
     C                   ENDIF
     C*
     *** CHECK FOR END OF JOB REQUEST (USE DATA AREA FLAG?)
     ***       CLEAR FLAG, END PGM
     C                   IN        SCCV
     C     X#FLG         IFNE      *BLANK
     C     *LOCK         IN        SCCV
     C                   MOVE      *BLANK        X#FLG
     C                   OUT       SCCV
     C                   GOTO      TERM
     C                   ENDIF
     ***
     **
     *** CHECK THE XXSEQ FIELD TO SEE IF IT HAS EXCEEDED THE MAX (999999)
     ***    IF SO, BREAK HERE WITH MESSAGE TO QSYSOPR????
     C                   MOVE      XXSEQ         ##SEQ             6 0
     C     ##SEQ         IFGE      999998
     C*** MUST SHOW ERROR TO SOMEONE
     C*** USE ANOTHER WSID ON NEXT CALL
     C                   MOVEL     MSG(2)        QQMSG1
     C*** CAN'T CREATE MORE SPLFS THAN THIS, SO RESTART
     C                   CALL      'SCCV3C'      PLCV3
     C*
     C     QQWSID        IFEQ      'D1'
     C                   MOVEL     'S '          QQWSID
     C                   ENDIF
     C     QQWSID        IFEQ      'D2'
     C                   MOVEL     'T '          QQWSID
     C                   ENDIF
     C     QQWSID        IFEQ      'D3'
     C                   MOVEL     'U '          QQWSID
     C                   ENDIF
     C     QQWSID        IFEQ      'D4'
     C                   MOVEL     'V '          QQWSID
     C                   ENDIF
     C     QQWSID        IFEQ      'D5'
     C                   MOVEL     'W '          QQWSID
     C                   ENDIF
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*** DETERMINE THE DCID/FOLDER TO TRANSLATE
     C*** 1) FROM SXCVP FILE USE DCID/FOLD
     C*** 2) IF ON OPTICAL, USE OPTICAL VALS
     C*** -----------------------------------------
     C*
     C*** GET FROM OPTICAL IF ON OPTICAL *** CODE IN SCCV1C ...
     C*
     C** CALL PGM TO DO PRTDOC / SPLF AND CREATE .PDF RTN DCID/FLR(SCCV2C )
     C***
     C                   MOVE      CVSEQ#        ZZSEQ#            5
     C*
     C     QQPRE         IFNE      *BLANKS
     C     QQPRE         CAT(P)    CVFOLD:0      ZZFOLD
     C                   ELSE
     C                   MOVE      *BLANKS       ZZFOLD
     C                   MOVEL     CVFOLD        ZZFOLD
     C                   ENDIF
     C*
     C                   CALL      'SCCV1C'      PLCV1
     C*
     C     ZZRTCD        IFNE      *BLANKS
     *** ERRORS UPDATE THE SXCVP RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV1C  '    ERPGM
     C                   MOVE      ZZRTCD        ERRTCD
     C                   MOVEL     ERM(2)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PCV01
     C                   GOTO      NXTONE
     C                   ENDIF
      ******** BEGIN CHANGE ******* 12/28/07 **************************
     C*****                ADD  1         COUNT
     C                   ADD       2             COUNT
      ******** END CHANGE ********* 12/28/07 **************************
     C*
     C                   MOVE      *ALL'0'       @@SNUM
     C                   MOVE      ZZSNUM        @@SNUM            6            SPLF NUM
     **
     **
     C*** BUILD THE @@SPTH FOR CONVERSION HERE ******************
     C***  '\QDLS\' *CAT XXNPTH = @@SPTH
     ***
     *** THE DEFAULT DIRECTORY FOR CONVERSION FILES
      **** BEGIN CHANGE *******  12/28/07 *****************************
     C*********            MOVEL'133DEI  'XXNPTH    P
     C                   MOVE      'POLICY'      DFTF1
     C                   MOVE      'PDF'         DFTF2
     C                   MOVEL(P)  DFTFLR        XXNPTH
      **** END CHANGE   *******  12/28/07 *****************************
     ****
     C                   MOVEL(P)  *BLANKS       @@SPTH
     ****
     ********** BEGIN CHANGE  ************** 12/28/07 ****************
     C*****      QQPRE     IFNE *BLANKS
     C*****      DLSPRE    CAT  XXNPTH:0  @@SPTH    P
     C*****                ELSE
     C*****      '\'       CAT  XXNPTH:0  @@SPTH    P
     C*****                ENDIF
      *
     C                   SELECT
      *
     C     QQPRE         WHENNE    *BLANKS
     C     DLSPRE        CAT(P)    XXNPTH:0      @@SPTH
      *
     C     ##PFLR        WHENGT    *BLANK
     C     ##PFLR        CAT(P)    XXNPTH:0      @@SPTH
      *
     C                   OTHER
     C     '/'           CAT(P)    XXNPTH:0      @@SPTH
     C                   ENDSL
     ********** END   CHANGE  ************** 12/28/07 ****************
     C*
      **** BEGIN CHANGE *******  12/28/07 *****************************
     C****       '\'       CAT  CVKEY:0   @@DC   41 P
     C****       @@DC      CAT  ZZSEQ#:1  @@NM1  50 P
     C                   MOVEL(P)  CVKEY         @DC15            15
     C     '/'           CAT(P)    @DC15:0       @@DC             41
      **** END CHANGE *********  12/28/07 *****************************
      *
      *
      *
      ****** BEGIN ADD  ****** 12/28/07 *************************
      *** Beg chg ***** 11/02/10 ***********************************************
     C*********            MOVEL'/QDLS/'  X1FOLD    P
     C     '/'           CAT(P)    ROOT          RPATH                          Root path
     C     RPATH         CAT(P)    '/':0         RPATH
     C                   MOVEL(P)  RPATH         X1FOLD
      *** End chg ***** 11/02/10 ***********************************************
      * CHECK FOR DC PREFIX
     C     ##PFLR        IFGT      *BLANK
     C     X1FOLD        CAT(P)    ##PFLR:0      X1FOLD
     C                   ENDIF
     C     X1FOLD        CAT(P)    CVFOLD:0      X1FOLD
     C                   MOVEL(P)  CVDCID        X1DCID
      *
      * GET POLICY YEAR
      *
     C                   CALL      'SCCVBC'
     C                   PARM                    X1FOLD           80
     C                   PARM                    X1DCID           12
     C                   PARM      CVKEY         X1KEY            40
     C                   PARM      *ZEROS        X1YEAR            3
     C                   PARM      *BLANKS       @@RTCD
      *
      * CHECK FOR ERRORS
      *
     C     @@RTCD        IFNE      *BLANKS
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCVBC  '    ERPGM
     C                   MOVE      @@RTCD        ERRTCD
     C                   MOVEL     ERM(4)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PCV01
     C                   GOTO      NXTONE
     C                   ENDIF
      *
     C     @@DC          CAT(P)    X1YEAR:0      @@NM1            50
      **** END ADD ************  12/28/07 *****************************
     C                   CAT(P)    '.PDF':0      @@NM1            50
     C*
     C     @@SPTH        CAT(P)    @@NM1:0       @@SPTH
     C*
     C                   MOVE      *BLANK        @@RTCD            1            RTN CODE
      * @@SPTH (STORE PATH) FORMAT IS:  /DIR/SUBDIR/OBJ.PDF
      *
     C                   CALL      'SCCV2C'      PLPDF
     C*
     C     @@RTCD        IFNE      *BLANKS
     *** ERRORS UPDATE THE SXCVP RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV2C  '    ERPGM
     C                   MOVE      @@RTCD        ERRTCD
     C                   MOVEL     ERM(3)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PCV01
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
      ****** BEGIN ADD ***** 12/28/07 *********************************
      * DELETE SPOOLED FILES
      *                    MOVE *BLANKS   @@RTCD
     C                   CALL      'SCCVCC'      PLCVC
     C*
     C     @@RTCD        IFNE      *BLANKS
     *** ERRORS UPDATE THE SXCVP RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCVCC  '    ERPGM
     C                   MOVE      @@RTCD        ERRTCD
     C                   MOVEL     ERM(5)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PCV01
     C                   GOTO      NXTONE
     C                   ENDIF
      ****** END ADD   ***** 12/28/07 *********************************
      *
     C*
     C** UPDATE THE SXCVP FILE HERE INC. INFO FOR OPTICAL IF NECCESARY
     C***
     C**** FIELDS TO UPDATE, OPTICAL FLAG (POSSIBLE), NEWDOC/NEWFLR, STATS
     C**** LMFIELDS???
     C                   MOVEL     CVSEQ#        CVNDOC
     C                   MOVEL     XXNPTH        CVNFLD
     C                   MOVEL     *BLANK        CVFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C                   MOVE      *BLANKS       CVERR
     C                   MOVE      *BLANKS       CVEFLG
     C*
     C                   UPDATE    PCV01
      *
     C     NXTONE        TAG
     C                   READ      PCV01                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C********
**
ERR GET NEW NAM FOR DCID/FLR
ERR PRINT DOC
ERR CREATE PDF FROM SPLF
ERR GET POL YEAR
ERR SPLF DELETION
**
REACHED MAX SPLFS FOR PDF CONVERT (SCCV1X)JOB.  JOB WILL RESTART ITSELF
REACHED MAX ON WSID INDEXING. END JOB. JOB WILL RESTART ITSELF.
