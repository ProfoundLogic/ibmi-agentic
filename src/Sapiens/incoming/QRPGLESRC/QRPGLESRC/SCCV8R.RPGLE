     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SCCV8R - CONVERT DOCS TO .PDF FORMAT FOR DARTS (OPTICAL ONLY)
      *
      **** REMEBER, IF CLIENT HAS VDARTS, MORE WORK MUST BE DONE
      ****  IN THESE PGMS, AS OTHER OBJS. ALREADY EXIST IN FOLDERS.
      ****   THE SELECT PROCESS TO BUILD THE WRK FILE MUST KNOW THIS
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      **** SCCV2R (COPIED FROM IS828R)
      *
      ***************************************************************
      *  REVISIONS:
     *                                                                *
     * 04/15/14  C:300 Project: SY195                                 *
     *                 Update Error messages to provide correct info  *
     *                                                                *
     * 11/02/10  C:183 Project: SY195                                 *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to*
     *                 get the root folder name.                      *
     *                                                                *
     H***************************************************************
     F*
     FSXC1L1    UF   E           K DISK
     F***SXC2L1  UF  E           K        DISK

     D*
     D ERM             S             28    DIM(6) CTDATA PERRCD(1)
     D MSG             S             80    DIM(5) CTDATA PERRCD(1)
     D NR              S             40    DIM(2) CTDATA PERRCD(1)
      *
     D @@FPTH          DS
     D  @FPRE                  1      5
     D  SVOLPT                 6     80
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
     C                   KFLD                    QQVOL
     C                   KFLD                    XXKEY            40
      *
     C*
     C     SVK           KLIST
     C                   KFLD                    QQVOL
     C                   KFLD                    DLKEY
     C                   KFLD                    DLSEQ#
      *
     C*
     C     C1K1          KLIST
     C                   KFLD                    QQVOL
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
     C***** CHANGED HERE TO 60 FROM 21 CVNFLD WAS CROPPED....
     C                   PARM      *BLANKS       XXNPTH           60
     C                   PARM      *BLANK        XXRTCD            1
     C                   PARM      QQPRE         XXPRE            13
      *
      *** PARM LIST FOR CALL TO PRTDOC PROGRAM
      *
     C     PLCV1         PLIST
     C                   PARM      CMDCID        ZZDCID           12
     C                   PARM                    ZZFOLD           80
     C                   PARM      CMKEY         ZZKEY            40
     C                   PARM                    ZZSEQ#            5
     C                   PARM      CMFL05        ZZOPTF            1
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
      *
      *** PARM LIST FOR CALL TO SCCV9R PROGRAM
      *
     C     PLCV8         PLIST
     C                   PARM                    @@FPTH           80            FROM  PATH
     C                   PARM                    @@TPTH           80            TO    PATH
     C                   PARM      *BLANKS       RTCD              1            RETURN VALUES
      * @@FPTH (STORE PATH) FORMAT IS:  /QOPT/OOVL/OFLR/OSUB/OBJN
      * @@FPTH (STORE PATH) FORMAT IS:  /QDLS/FOLDER/DCID
      *
      *** PARM LIST FOR CALL TO SCCV9R PROGRAM
      *
     C     PLCV8A        PLIST
     C                   PARM                    @@FPTH           80            FROM  PATH
     C                   PARM                    @@TPT2           80            TO    PATH
     C                   PARM      *BLANKS       RTCD              1            RETURN VALUES
      * @@FPTH (STORE PATH) FORMAT IS:  /QOPT/OOVL/OFLR/OSUB/OBJN
      * @@FPTH (STORE PATH) FORMAT IS:  /OPTMLB01/OOVL/OFLR/OSUB/OBJN
      *
     C     PLCV8B        PLIST
     C                   PARM                    QQWSID            2
     C                   PARM                    QQPRE            13
     C                   PARM                    QQVOL            10
     C                   PARM                    QQCNT             5
     **
      *** PARM LIST FOR CALL TO SVALIFS PGM
      *
     C     PLVI          PLIST
     C                   PARM                    VOBJ             80            FROM  PATH
     C                   PARM      *BLANKS       VRTN              1            RTN CODE
      *** PARM LIST FOR CALL TO SMDIFS PROGRAM
      *
     C     PLMD          PLIST
     C                   PARM                    VOBJ             80            FROM  PATH
     C                   PARM      *BLANKS       MRTN              1            RTN CODE
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
     C                   PARM                    QQVOL            10
      *** Beg chg ***** 11/02/10 ***********************************************
      * Call the Root determination program
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
     C*******    '\QDLS\'  CAT  QQPRE     DLSPRE 19 P
     C     '\'           CAT(P)    ROOT          RPATH            12            Root path
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    QQPRE:0       DLSPRE           19
      *** End chg ***** 11/02/10 ***********************************************
      *
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C     C1K1          CHAIN     PC101                              60
     C*******************  READ PCV01                    60
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
     C**** CHECK HERE TO SEE IF THIS NEEDS TO BE CONVERTED******************
     C****   IF NOT, THEN COPY FROM OPTICAL TO IT'S ORIGINAL LOCATION (CM) AND
     C****   DOCID (DCID OR IDOC)  - UPDATE THE NEW FIELDS
     C******
     C******  IF SO, THEN RUN CONVERSION ON IT (LIKE BEFORE), AND UPDATE THE NEW
     C***     FIELDS
     C**** ADDED THIS --->>> IF CVERR = NOCM  (NO CM RECORD FOUND, JUST COPY OBJ
     C****   OBJECT TO THE IFS/OPTMLB01... IN ORIGINAL LOCATION
     C****     DONT UPDATE CMFL05 FOR THIS ONE...
     C     CVERR         IFEQ      NR(1)
     C*
     C                   EXSR      NOCM
     C                   GOTO      NXTONE
     C*
     C                   ENDIF
     C*
     C     CMIMGF        CASEQ     ' '           CVTS
     C     CMIMGF        CASNE     ' '           NOCVT
     C                   ENDCS
     C***************** SEPERATE PROCESS WILL ACTUALLY UPDATE SDCMP WITH THE
     C****************   APPROPRIATE INFO.
     C****
     *** -------> PROCESS IS DONE HERE... UPDATE FILES APPROPRIATELY
     **            C1FILE - NEW DCID/FOLDER NO OPTICAL INFO
     C*
     C** UPDATE THE SXCVP FILE HERE INC. INFO FOR OPTICAL IF NECCESARY
     C***
     C**** FIELDS TO UPDATE, OPTICAL FLAG (POSSIBLE), NEWDOC/NEWFLR, STATS
     C**** LMFIELDS???
     C                   MOVEL     XXDCID        CVNDOC
     C                   MOVEL     XXNPTH        CVNFLD
     C*********            MOVEL*BLANK    CMFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C                   MOVE      *BLANKS       CVERR
     C                   MOVE      *BLANKS       CVEFLG
     C*
     C                   UPDATE    PC101
      *
     C     NXTONE        TAG
     C     C1K1          READE     PC101                                  60
     C**************       READ PCV01                    60
     C                   ENDDO
     C*
     **** HERE WE CAN PROCESS THE C2 FILE ***8
     **** SAME AS ABOVE, MOST SHOULD NOT BE OV DOCS, SO THEY WILL BE
     ***8   RESTORED TO THEIR ORIGINAL LOCATION.
     ***
     ***
     C                   MOVE      COUNT         QQCNT
     ***
     ***
     C** ** PROCESS FOR OL AND MD FILES
     C                   CALL      'SCCV8R1'     PLCV8B
     ***
     ***
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     **===================================================================
     C**** SUB TO CONVERT
     C     CVTS          BEGSR
     *** LOAD PARMS FOR CALL TO SCCV2R (WSID IS PASSED IN)
     ***   SEE PARM LIST ABOVE (PARMS SET THERE)
     C*
     C*** CALL PROGRAM TO GET NEW NAME (SCCVR2)
     C*
     C                   CALL      'SCCV2R'      PLCV2
     **
     C*
     C     XXRTCD        IFNE      *BLANK
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV2R  '    ERPGM
     C                   MOVE      XXRTCD        ERRTCD
     C                   MOVEL     ERM(1)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     *** ERRORS UPDATE THE SXCVP RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   ENDIF
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
     C                   MOVE      CMSEQ#        ZZSEQ#            5
     C*
     C     QQPRE         IFNE      *BLANKS
     C     QQPRE         CAT(P)    CMFOLD:0      ZZFOLD
     C                   ELSE
     C                   MOVE      *BLANKS       ZZFOLD
     C                   MOVEL     CMFOLD        ZZFOLD
     C                   ENDIF
     C*
     C                   CALL      'SCCV1C'      PLCV1
     C                   ADD       1             COUNT
     C*
     C     ZZRTCD        IFNE      *BLANKS
     *** ERRORS UPDATE THE SXCVP RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV1C  '    ERPGM
     C                   MOVE      ZZRTCD        ERRTCD
     C**** CHANGE THIS PART... IF NOT ABLE TO CONVERT, TRY NCVT ROUTINE JUST PUT
     C**** DOC WHERE IT BELONGS...
     C***
     C                   MOVEL     ERM(5)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'N'           CVEFLG
     C                   MOVEL     'O'           CMIMGF
     C*
     C                   UPDATE    PC101
     C     SVK           CHAIN     PC101                              60
     C                   EXSR      NOCVT
     C*******
     C                   MOVEL     XXDCID        CVNDOC
     C                   MOVEL     XXNPTH        CVNFLD
     C***************      MOVEL*BLANK    CMFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C***** NOT THIS       MOVE *BLANKS   CVERR
     C****************     MOVE *BLANKS   CVEFLG
     C*
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C*** END OF CHANGES HERE ****
     C                   MOVEL     ERM(2)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
     C                   MOVE      *ALL'0'       @@SNUM
     C                   MOVE      ZZSNUM        @@SNUM            6            SPLF NUM
     C*** BUILD THE @@SPTH FROM THE DOCID/FLR RETURNED ABOVE.
     C***  ** START WITH \QDLS\FLR\SFLR\DCID.PDF
     C***  '\QDLS\' *CAT XXNPTH = @@SPTH
     C     QQPRE         IFNE      *BLANKS
     C     DLSPRE        CAT(P)    XXNPTH:0      @@SPTH
     C                   ELSE
      *** Beg chg ***** 11/02/10 ***********************************************
     C******     '\QDLS\'  CAT  XXNPTH:0  @@SPTH    P
     C     '\'           CAT(P)    ROOT          RPATH                          Root path
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    XXNPTH:0      @@SPTH
      *** End chg ***** 11/02/10 ***********************************************
     C                   ENDIF
     C*
     C     '\'           CAT(P)    XXDCID:0      @@DC             13
     C     @@SPTH        CAT(P)    @@DC:0        @@SPTH
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
     C**** CHANGE THIS PART... IF NOT ABLE TO CONVERT, TRY NCVT ROUTINE JUST PUT
     C**** DOC WHERE IT BELONGS...
     C***
     C                   MOVEL     ERM(5)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'N'           CVEFLG
     C*
     C                   MOVEL     'O'           CMIMGF
     C*
     C                   UPDATE    PC101
     C     SVK           CHAIN     PC101                              60
     C                   EXSR      NOCVT
     C*******
     C                   MOVEL     XXDCID        CVNDOC
     C                   MOVEL     XXNPTH        CVNFLD
     C*******************  MOVEL*BLANK    CMFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C*******NOT THIS      MOVE *BLANKS   CVERR
     C******************   MOVE *BLANKS   CVEFLG
     C*
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C*** END OF CHANGES HERE ****
     C                   MOVEL     ERM(3)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
     C     ECVTS         ENDSR
     **===================================================================
     **===================================================================
     C****   IF NOT, THEN COPY FROM OPTICAL TO IT'S ORIGINAL LOCATION (CM) AND
     C****   DOCID (DCID OR IDOC)  - UPDATE THE NEW FIELDS
     C**** SUB TO NOT CONVERT
     C     NOCVT         BEGSR
     C*
     C** MOVE CM VALS INTO THE "TO" INFORMATION
     C** OPTICAL VALUES INTO THE "FROM " INFORMATION
     C*** CALL PROGARM TO COPY, GET ANY ERRORS AND MAKE SURE COPY OCCURS, ELSE
     C*** FLAG AS ERROR.
     C*
     C                   MOVEL     '/QOPT'       @FPRE
      *** Beg chg ***** 11/02/10 ***********************************************
     C************         MOVEL'/QDLS'   @TPRE
     C     '/'           CAT(P)    ROOT          RPATH                          Root path
     C                   MOVEL     RPATH         @TPRE
      *** End chg ***** 11/02/10 ***********************************************
     C*
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    DLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLDCID:0      SVOLPT
     C*** TO PATH
     C     CMIMGF        IFEQ      'I'
     C     CMIMGF        OREQ      'Y'
     C                   MOVEL     CMIDOC        #$DCID           12
     C                   ELSE
     C                   MOVEL     CMDCID        #$DCID
     C                   ENDIF
     C     '/'           CAT(P)    CMFOLD:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    #$DCID:0      TVOLPT
     C****
     C                   CALL      'SCCV9R'      PLCV8
     C*** SOME ERROR OCCURED
     C     RTCD          IFNE      *BLANKS
     *** ERRORS UPDATE THE SXC1P RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV9R  '    ERPGM
     C                   MOVEL     RTCD          ERRTCD
     C                   MOVEL     ERM(4)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       CVEFLG
    CC*
     C                   ENDIF
     C**** FIELDS TO UPDATE IN THE C1 FILE
     C                   MOVEL     #$DCID        XXDCID
     C                   MOVEL     CMFOLD        XXNPTH
     C*
     C     ENOCVT        ENDSR
     **===================================================================
     **===================================================================
     C****   NO CM RECORD COPY FROM OPTICAL TO IT'S DL NAME/LOCATION
     C     NOCM          BEGSR
     C*
     C** MOVE CM VALS INTO THE "TO" INFORMATION
     C** OPTICAL VALUES INTO THE "FROM " INFORMATION
     C*** CALL PROGARM TO COPY, GET ANY ERRORS AND MAKE SURE COPY OCCURS, ELSE
     C*** FLAG AS ERROR.
     C*
     C                   MOVEL     '/QOPT'       @FPRE
      *** Beg chg ***** 11/02/10 ***********************************************
     C************         MOVEL'/QDLS'   @TPRE
     C     '/'           CAT(P)    ROOT          RPATH                          Root path
     C                   MOVEL     RPATH         @TPRE
      *** End chg ***** 11/02/10 ***********************************************
     C                   MOVEL     '/OPTMLB0'    @TPR2
     C                   MOVE      '1'           @TPR2
     C*
     C*
     C                   MOVE      *BLANKS       VOBJ
     C     @TPR2         CAT(P)    '/':0         VOBJ
     C                   CAT(P)    DLVOL1:0      VOBJ             80
      * *** VALIDATE THE DIRECTORIES WILL EXIST BEFORE COPY
     C                   CALL      'SVALIFS'     PLVI
     C                   ADD       1             COUNT
     C     VRTN          IFNE      *BLANKS
     C*** MAKE DIRECTORY HERE
     C                   CALL      'SMDIFS'      PLMD
     C*
     C     MRTN          IFNE      *BLANKS
     C*??? DO WHAT????
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVE      *BLANKS       VOBJ
     C     @TPR2         CAT(P)    '/':0         VOBJ
     C                   CAT(P)    DLVOL1:0      VOBJ
     C                   CAT(P)    '/':0         VOBJ
     C                   CAT(P)    DLOFLD:0      VOBJ
      * *** VALIDATE THE DIRECTORIES WILL EXIST BEFORE COPY
     C                   CALL      'SVALIFS'     PLVI
     C                   ADD       1             COUNT
     C     VRTN          IFNE      *BLANKS
     C*** MAKE DIRECTORY HERE
     C                   CALL      'SMDIFS'      PLMD
     C*
     C     MRTN          IFNE      *BLANKS
     C*??? DO WHAT????
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   MOVE      *BLANKS       VOBJ
     C     @TPR2         CAT(P)    '/':0         VOBJ
     C                   CAT(P)    DLVOL1:0      VOBJ
     C                   CAT(P)    '/':0         VOBJ
     C                   CAT(P)    DLOFLD:0      VOBJ
     C                   CAT(P)    '/':0         VOBJ
     C                   CAT(P)    DLOSBD:0      VOBJ
      * *** VALIDATE THE DIRECTORIES WILL EXIST BEFORE COPY
     C                   CALL      'SVALIFS'     PLVI
     C                   ADD       1             COUNT
     C     VRTN          IFNE      *BLANKS
     C*** MAKE DIRECTORY HERE
     C                   CALL      'SMDIFS'      PLMD
     C*
     C     MRTN          IFNE      *BLANKS
     C*??? DO WHAT????
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    DLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DLDCID:0      SVOLPT
     C*** TO PATH
     C     CMIMGF        IFEQ      'I'
     C     CMIMGF        OREQ      'Y'
     C                   MOVEL     CMIDOC        #$DCID           12
     C                   ELSE
     C                   MOVEL     CMDCID        #$DCID
     C                   ENDIF
     C*
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    DLVOL1:0      TVOLP2
     C                   CAT(P)    '/':0         TVOLP2
     C                   CAT(P)    DLOFLD:0      TVOLP2
     C                   CAT(P)    '/':0         TVOLP2
     C                   CAT(P)    DLOSBD:0      TVOLP2
     C                   CAT(P)    '/':0         TVOLP2
     C                   CAT(P)    DLDCID:0      TVOLP2
     C****
     C                   CALL      'SCCV9C'      PLCV8A
     C*****************??? CALL 'SCCV9R'  PLCV8A
     C*** SOME ERROR OCCURED
     C     RTCD          IFNE      *BLANKS
     *** ERRORS UPDATE THE SXC1P RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV9C  '    ERPGM
     C                   MOVEL     RTCD          ERRTCD
     C                   MOVEL     ERM(6)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC101
     C                   GOTO      NXTONE
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       CVEFLG
    CC*
     C                   ENDIF
     C**** FIELDS TO UPDATE IN THE C1 FILE
     C                   MOVEL     #$DCID        XXDCID
     C                   MOVEL     CMFOLD        XXNPTH
     C*******
     C                   MOVEL     XXDCID        CVNDOC
     C                   MOVEL     XXNPTH        CVNFLD
     C**** DONT DO THIS ** MOVEL*BLANK    CMFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C****** LEAVE ERR     MOVE *BLANKS   CVERR
     C                   MOVE      *BLANKS       CVEFLG
     C*
     C                   UPDATE    PC101
      *
     C*
     C     ENOCM         ENDSR
     **===================================================================
     C***** beg chg ****** 04/15/14 **********************************
      * Removed QDLS/ from ERM(4) and IFS/ from ERM(6)
     C***** end chg ****** 04/15/14 **********************************
**
ERR GET NEW NAM FOR DCID/FLR
ERR PRINT DOC
ERR CREATE PDF FROM SPLF
ERR ON CPY QOPT TO NCVT                                                                     04/15/14
CVT ERR - TRY NO CONVERT
ERR ON CPY QOPT TO NOCM                                                                     04/15/14
**
REACHED MAX SPLFS FOR PDF CONVERT (SCCV8X)JOB.  JOB WILL RESTART ITSELF
REACHED MAX ON WSID INDEXING. END JOB. JOB WILL RESTART ITSELF.
**
01 - NO CM RCD FOUND
02 - NO MD RCD FOUND
