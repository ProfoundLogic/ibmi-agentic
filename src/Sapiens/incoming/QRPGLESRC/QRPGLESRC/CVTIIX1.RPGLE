     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*===============================================================*
     H** REVISIONS:
     H*
     H*  10/17/11 C:278 Project: SY195
     H*                 Add file system flag parameter with DLO Cammand
     H*
     H*===============================================================*
     F****
     F*  DIRECTORY FOR OUTPUT: /PDS/132/CLDUMP
     F**
     F*
     F*     CLAIM KEY  40A
     F*     RTNCODE     1A
     F*
     F*  READ/LOOP ALL RCDS FOR THE CLAIM KEY  (SDCMP)
     F*      EACH:
     F*           IF DOC ON MAJOR REMARK, CPY THE DOC FROM CURRENT LOCATION
     F*              TO THE IFS DIRECTORY (CO#GROUP/FYRCLAIM#)
     F*              DOCID WILL BE DARTKEY *CAT SEQ# *CAT BLANKS FOR MAJOR
     F*
     F*           IF DOC IS OR ISNOT ON MAJOR REMARK, CHECK THE MORE OBJS
     F*            (SMMDP?) TO SEE IF ANY EXIST, AND IF SO, LOOP FOR EACH:
     F*                CPY THE OBJ FROM THE CURRNT LOC TO THE IFS DIREC (SEE
     F*                ABOVE) DOCID IS DARTKEY *CAT SEQ# *CAT UNIQUE MD VAL
     F*                LOOP FOR ALL
     F*
     FSDCMLA    IF   E           K DISK
     FSMOLL1    IF   E           K DISK
     FSMMDL     IF   E           K DISK
     FCLATCH    O    E             DISK
     F                                     RENAME(PCM01:PATCH)

     D*
     D** MAKE DEFAULT IFS DIRECTORY HERE
     D***
     D                 DS
     D  XO                     1    132
     D                                     DIM(132)
     D @RC             DS
     D  RTCD                   1      8
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  MSG                   17    272
     D*
     D #KY1            DS
     D  #1KEY                  1     40
     D  #1CO#                  1      3
     D  #1FND                  4      6
     D  #1FYR                  7      9
     D  #1CASE                10     16
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C     CMK           KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C*
     C     MDK           KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    CMSEQ#
     C*
     C*
     C     OLK           KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    CMSEQ#
     C                   KFLD                    MDOFLR
     C                   KFLD                    MDOID
     *************************************************************
     *** FOR SCCV2R
     C     PLCV2         PLIST
     C                   PARM      '@1'          XXWSID            2
     C                   PARM      '000000'      XXSEQ             6
     C                   PARM      'PDF'         XXSUFX            3
     C                   PARM      '0000'        XXBCHS            4
     C                   PARM      *BLANKS       XXDCID           12
     C                   PARM      *BLANKS       XXBCHI           12
     C                   PARM      *BLANKS       XXFLR             8
     C                   PARM      *BLANKS       XXSFLR           12
     C                   PARM      *BLANKS       XXNPTH           21
     C                   PARM      *BLANK        XXRTCD            1
     C                   PARM      *BLANK        XXPRE            13
     *************
      *** PARM LIST FOR CALL TO PRTDOC PROGRAM
      *
     C     PLCV1         PLIST
     C                   PARM      X1DCID        ZZDCID           12
     C                   PARM      X1FOLD        ZZFOLD           80
     C                   PARM      CMKEY         ZZKEY            40
     C                   PARM                    ZZSEQ#            5
     C                   PARM      ' '           ZZOPTF            1
     C                   PARM      *BLANK        ZZRTCD            1
     C                   PARM      'QPRINT'      ZZOUTQ           10
     C                   PARM      '001'         ZZCPYS            3
     C                   PARM      *BLANKS       ZZSNAM           10
     C                   PARM      '00000'       ZZSNUM            5
     C                   PARM      *BLANKS       ZZJNAM           10
     C                   PARM      '000000'      ZZJNUM            6
     C                   PARM      *BLANKS       ZZJUSR           10
     C*** Beg Add *** 10/17/11 ****************************************
     C                   PARM                    ZZUNCN          255
     C                   PARM                    ZZFSF             1
     C*** End Add *** 10/17/11 ****************************************
      *
     *************
     **** SCCV2C
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
     *************************************************************
     C*
     C*
     ***** CHANGE TO PROCESS EACH RECORD.  AS WE'VE ALREADY DETERMINED WHICH
     *** ONE'S TO PROCESS.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5
     C                   PARM                    QQPATH          255
     C                   PARM                    QQRTCD            1
     ***
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C                   MOVE      *BLANKS       QQPATH
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      '1'           XXFRST            1
     C                   Z-ADD     *ZEROS        ASQ#
     C                   MOVE      *ZEROS        ASQ
     C*
     C                   TIME                    DSDTTM
     C*
     C**** EXTRACT THE IFS DIRECTORY NAMES TO USE.. BUILD THE DIRECTORY
     C***   FOR EACH CLAIM IF NOT EXIST
     C**** USE THE QQKEY TO GET THE DIRECTORY NAMES
     C**  THE MAIN DIRECTORY WILL BE EXT + CO# + FND
     C**** THE FYR + CLAIM# WILL BE THE SUBDIRECTORY
     C****   EACH "TO" FILE NAME WILL BE QQKEY + SEQ# + EXTENTION
     C*******     FOR CM FILES
     C***8         USE QQKEY + SEQ# + OID + EXT FOR MDS
     C**
     C*
     C     CMK           CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*** PROCESS CM, THEN MD, THEN LOOP
     C*** DO ALL CM, NOT JUST OPTICAL ***
     C*
     C*
     C                   MOVE      CMSEQ#        @@SEQ#            5
     C                   MOVEL     QQKEY         #1KEY
     C**** BUILD THE "TO" DIRECTORY NAME
     C                   EXSR      BLDDIR
     C*
     C     CMDCID        IFNE      *BLANK
     C     CMFL01        ANDEQ     *BLANK
     C*
     C*** IF DCID OR IMGF IS "Y" THEN PUT THE DOCID AND FOLDER INFO TO GET
     C**    COPYIED
     C     CMIMGF        IFEQ      'Y'
     C     CMIMGF        OREQ      'I'
     C                   MOVEL     CMIDOC        XXDCID           12
     C                   MOVEL     CMIDOC        X1DCID           12
     C                   ELSE
     C                   MOVEL     CMDCID        XXDCID
     C                   MOVEL     CMDCID        X1DCID
     C                   ENDIF
     C                   MOVE      CMFOLD        X1FOLD           80
     C                   MOVE      CMFOLD        XXFOLD           80
     C*
     C*
     C***    CM FLAG TELLS IF "ON OPTICAL"
     C     CMFL05        IFEQ      'O'
     C*** IF ON OPTICAL, GET DL RCD, THEN USE THE QOPT AS PREFIX,
     C*** ELSE, USE QDLS AS PREFIX WITH CM VALUES
     C*
     C     CMIMGF        IFEQ      'Y'
     C     CMIMGF        OREQ      'I'
     C                   CALL      'IS977R'
     C                   PARM                    QQKEY
     C                   PARM                    @@SEQ#
     C                   PARM      XXDCID        X1DCID           12
     C                   PARM                    X1FOLD           80
     C                   PARM                    @1RTCD            1
     C                   ELSE
     C                   CALL      'SC871R'
     C                   PARM                    QQKEY
     C                   PARM                    @@SEQ#
     C                   PARM      XXDCID        X1DCID           12
     C                   PARM                    X1FOLD           80
     C                   PARM                    @1RTCD            1
********
     C                   ENDIF
     C*
     C*** IF VALID RETRIEVE
     C     @1RTCD        IFEQ      *BLANKS
     C****
     C                   ELSE                                                     @1RTCD
     C*** NO RETRIEVE FROM OPTICAL ?? IGNORE??
     C*** BYPASS THIS ONE?
     C                   MOVE      'K'           QQRTCD
     C                   GOTO      BPAS1
     C                   ENDIF                                                    @1RTCD
     C*                                                                *
     C                   ENDIF                                                  CMFL05
     C**** FOR ALL OPTICAL AND NON-OPTICAL
     C*** COPY FROM QDLS TO THE "NEW" NAME ON THE IFS
     C*****
     C     XXFRST        IFEQ      '1'
     C                   CALL      'VRTOPTDC'                                    MK DIR  1ST
     C                   PARM      NEWDIR        @@DIR            80
     C                   PARM      *BLANKS       @RC
     C     @RC           IFNE      *BLANKS
     C     CONDTN        ANDNE     'CPFA0A0'
     C*** USE DEFAULT FOLDER
     C                   MOVEL     *BLANKS       @@DIR
     C                   MOVEL     DFTDIR        @@DIR
     C*
     C                   ENDIF                                                    @@RC
     C                   MOVE      ' '           XXFRST                           XXFRST
     C                   ENDIF
     C**** CALL PGM TO COPY TO IFS DIRECTORY
     C*
     C*****         USE X1DCID TO COPY FRM, EXTENTION IS FROM XXDCID
     C**** GET EXTENTION NAME FOR USE ON "TO" *BUILD DOC NAME
     C                   EXSR      GETEXT
     C*                         XXDCID IS THE DOC NAME IN GETEXT...
     ****
     C     CMIMGF        IFEQ      *BLANKS
     C*
     ***
     ***** MUST TRANSLATE TO PDF FOR STORAGE. STORE DIRECT WITH THIS API, THEN U
     ***    UPDATE THE QQRTCD AND QQPATH HERE.
     C*
     *** X1DCID ** X1FOLD IS THE FROM
     C                   MOVE      CMSEQ#        ZZSEQ#
     C*** Beg Add *** 10/17/11 ****************************************
     C                   MOVE      CMUNCN        ZZUNCN
     C                   MOVE      CMIFSP        ZZFSF
     C*** End Add *** 10/17/11 ****************************************
     C                   CALL      'CVTIIXC1'    PLCV1
     C     ZZRTCD        IFNE      *BLANKS
     C                   MOVEL     ZZRTCD        QQRTCD
     C                   MOVEL     *BLANKS       QQPATH
     C                   ELSE
     ****
     C*
     C                   MOVE      *ALL'0'       @@SNUM
     C                   MOVE      ZZSNUM        @@SNUM            6            SPLF NUM
     C                   MOVE      *BLANKS       @@RTCD
     C                   MOVEA     *BLANKS       XO
     C****
     ***** SINCE THIS WILL BE A PDF, NEED TO PUT .PDF EXTENSION ON IT.
     C     '.'           SCAN      @@TOBJ        R                 3 0    90
     C     *IN90         IFEQ      *ON                                            FOUND
     C                   MOVEA     @@TOBJ        XO
     C                   MOVEA     '.PDF'        XO(R)
     C                   MOVEA     XO            @@TOBJ
     C                   ELSE                                                      NOT FND
     C                   CAT(P)    '.PDF':0      @@TOBJ
     C                   ENDIF
     ***
     C     @@DIR         CAT(P)    '\':0         @@SPTH
     C                   CAT(P)    @@TOBJ:0      @@SPTH
    ****
     C                   CALL      'CVTIIXC2'    PLPDF
     **
     C     @@RTCD        IFNE      *BLANKS
     C                   MOVEL     'M'           QQRTCD
     C                   MOVEL     *BLANKS       QQPATH
     C                   ELSE
     C                   MOVEL     'P'           QQRTCD
     C                   MOVEL     @@SPTH        QQPATH
     C                   ENDIF
     ****
     C                   GOTO      BPAS1
     ****
     C                   ENDIF
     C*
     ***
     C*
     C                   ELSE
     C***
     C                   MOVE      *BLANKS       ##FOLD
     C     '\QDLS\'      CAT(P)    'PDS\':0      ##FOLD           80
     C                   CAT(P)    '132\':0      ##FOLD           80
     C                   CAT(P)    X1FOLD:0      ##FOLD           80
     C                   CALL      'CPYOBJC'
     C                   PARM      ##FOLD        #$FDIR           80
     C                   PARM      X1DCID        #$FOBJ          132
     C                   PARM      @@DIR         #$TDIR           80
     C                   PARM      @@TOBJ        #$TOBJ          132
     C                   PARM      *BLANKS       #$RC              2
     C*** IF ERROR OCCURS MUST KEEP IT FOR PASSING BACK TO THE CALLER
     **
     C     #$RC          IFNE      *BLANKS
     C                   MOVEL     'J'           QQRTCD
     C                   MOVEL     *BLANKS       QQPATH
     C                   ELSE
     C                   MOVEL     'P'           QQRTCD
     ***
     C     #$TDIR        CAT(P)    '\':0         OPATH
     C                   CAT(P)    #$TOBJ:0      OPATH           132
     C                   MOVEL     *BLANKS       QQPATH
     C                   MOVEL     OPATH         QQPATH
     C                   ENDIF
     ****
     ***
     C                   ENDIF                                                  CMDCID
     C*
     C     QQRTCD        IFEQ      *BLANKS
     C                   ELSE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BYPASS CM
     C     BPAS1         TAG
     C*** CHECK FOR MORE OBJECTS FOR THE DART RECORD
     C     MDK           CHAIN     PMD01                              61
     C     *IN61         DOWEQ     *OFF
     C*** FOR EACH MD FOUND
     C*
     C                   MOVEL     MDSYSN        X2SYSN
     C                   MOVEL     MDOFLR        X2OFLR
     C                   MOVEL     MDOFLR        SVOFLR           60
     C                   MOVEL     MDOID         X2OID
     C                   MOVEL     MDOID         SVOID            40
     C****                 MOVELMDOPTF    X2OPTF
     C                   MOVEL     *BLANKS       X2OPTF
     C*** CHECK TO SEE IF ON OPTICAL (OL FILE), THEN USE QOPT OR QDLS AS
     C***   THE PREFIX...
     C     OLK           CHAIN     POL01                              62
     C     *IN62         IFEQ      *OFF
     C*** GET MD OBJ FROM OPTICAL
     C**
     C                   CALL      'IS977R1'
     C                   PARM                    QQKEY            40
     C                   PARM      @@SEQ#        X2SEQ#            5
     C                   PARM      OLSYSN        X2SYSN           20
     C                   PARM      OLOFLR        X2OFLR           60                ***
     C                   PARM      OLOID         X2OID            40                ***
     C                   PARM      OLOPTF        X2OPTF            1
     C                   PARM      *BLANK        X2RTCD            1
     C*
     C                   ENDIF
     C*** IF FIRST BUILD DIRECTORY
     C*****
     C     XXFRST        IFEQ      '1'
     C                   MOVEL     *BLANKS       @@DIR
     C                   CALL      'VRTOPTDC'                                    MK DIR  1ST
     C                   PARM      NEWDIR        @@DIR            80
     C                   PARM      *BLANKS       @RC
     C     @RC           IFNE      *BLANKS
     C     CONDTN        ANDNE     'CPFA0A0'
     C*** USE DEFAULT FOLDER
     C                   MOVEL     DFTDIR        @@DIR
     C*
     C                   ENDIF                                                    @@RC
     C                   MOVE      ' '           XXFRST                           XXFRST
     C                   ENDIF
     C*** BUILD NAMES  USE SVOID FOR EXTENTION
     C*** COPY OBJECT FROM/TO
     C**** GET EXTENTION NAME FOR USE ON "TO" *BUILD DOC NAME
     C                   EXSR      MDEXT
     C*
     C                   MOVE      *BLANKS       ##FOLD
     C                   MOVEL     *BLANKS       #$TOBJ
     C                   MOVEL     *BLANKS       #$TDIR
     C                   MOVEL     *BLANKS       #$FDIR
     C                   MOVEL     *BLANKS       #$FOBJ
     C     '\QDLS\'      CAT(P)    'PDS\':0      ##FOLD           80
     C                   CAT(P)    '132\':0      ##FOLD           80
     C                   CAT(P)    X2OFLR:0      ##FOLD           80
     C                   CALL      'CPYOBJC'
     C                   PARM      ##FOLD        #$FDIR           80
     C                   PARM      X2OID         #$FOBJ          132
     C                   PARM      @@DIR         #$TDIR           80
     C                   PARM      @@TOBJ        #$TOBJ          132
     C                   PARM      *BLANKS       #$RC              2
     ******* WRITE CLATCH HERE WITH 'P' AS STATUS!!!
     *** ??? IF ERROR WHAT TO DO ???
     **8 PASS ERROR BACK TO CALLER ?? NO HERE IF ERROR WRITE NEW RECORD WITH ERR
     **
     C     #$TDIR        CAT(P)    '\':0         OPATH           132
     C                   CAT(P)    #$TOBJ:0      OPATH
     **
     C     #$RC          IFNE      *BLANKS
     C                   MOVEL     'X'           CMSTAT
     C                   MOVEL     *BLANKS       CMIUSR
     C                   MOVEL     #$RC          CMIUSR
     C                   MOVEL     *BLANKS       CMUNCN
     C                   ELSE
     C                   MOVEL     'P'           CMSTAT
     C                   MOVEL     *BLANKS       CMIUSR
     C                   MOVEL     OPATH         CMUNCN
     C                   ENDIF
     **
     C                   EXSR      ATCH
     C*
     C     MDK           READE     PMD01                                  61
     C                   ENDDO
     C**
     C***       MD FILE HAS FLAG FOR OPTICAL????
     C*
     C*** PROCESS ALL MD(MORE OBJ) FOR THE KEY/SEQ#
     C*-3
     C*
     C*
     C********   CMK       READEPCM01                    60
     C                   ENDIF                                                       *IN60
     C***
     C                   MOVE      *ON           *INLR
     C***********************************************************************
     C     BLDDIR        BEGSR
     *****
      *
      *
      * * * *   WE NEED TO CREATE OUTPUT DIRECTORY AS :
      *          PDS/132/CLDUMP/CO-FND-FYR-CASE/OBJECTNAME.EXT
      *
     C*** DIRECTORY NAME IS '\' + CO#+FND + '\' + FYR+CASE
     C                   MOVE      *BLANKS       NEWDIR
     C     '\PDS\'       CAT(P)    '132\'        NEWDIR
     C                   CAT(P)    'CLDUMP':0    NEWDIR
     C                   CAT(P)    '\':0         NEWDIR
     C                   CAT(P)    #1CO#:0       NEWDIR           80
     C                   CAT(P)    #1FND:0       NEWDIR
     C                   CAT(P)    #1FYR:0       NEWDIR
     C                   CAT(P)    #1CASE:0      NEWDIR
     C*** BUILD DFTDIR
     C                   MOVE      *BLANKS       DFTDIR
     C     '\PDS\'       CAT(P)    '132\'        DFTDIR
     C                   CAT(P)    'CLDUMP':0    DFTDIR           80
     ***
     C****       '\'       CAT  #1CO#:0   DFTDIR 80 P
     C****                 CAT  #1FND:0   DFTDIR    P
     C***
     C*
     C                   ENDSR
     C*****************************************
     C     GETEXT        BEGSR
     C****  BUILD THE TOOBJECT NAME USING EXTENTION FROM XXDCID
     C*** @@TOBJ 132 IS CREATED
     C*
     C*** IF CMIMGF IS *BLANKS THEN IT'S OV, SO USE .RFT AS EXTENTION
     ***                DO WE CONVERT TO PDF HERE??? THIS IS WHERE TO DO THIS?
     C*
     **** CANT DO THIS... BUT WE WANT PDF ANYWAY... IF DOC IS LESS THAN 8 AND HA
     ***   S AN EXTENSION .DOC, ETC. THIS CAUSES MULTIPLE PERIODS. XXX..RFT
     *** ***** **** ***** **** **** **** **
     C     CMIMGF        IFEQ      *BLANK
     C******************   MOVE '.RFT'    XXDCID
     C                   ENDIF
     C***
     C                   MOVE      *BLANKS       @@TOBJ
     C     QQKEY         CAT(P)    @@SEQ#:0      @@TOBJ          132
     C                   CAT(P)    XXDCID:0      @@TOBJ
     C*
     C                   ENDSR
     C*****************************************
     C     MDEXT         BEGSR
     C****  BUILD THE TOOBJECT NAME USING EXTENTION FROM SVOID
     C*** @@TOBJ 132 IS CREATED     SVOID OFLR
     *** ISSUE HERE IS FOLDER NAME CAN CONTAIN "/" AND WILL NOT PROCESS CORRECT
     ***** PERHAPS PUT A COUNTER IN, AND FOR EACH MD INCREMENT IT, USE IT IN PLA
     ******* PLACE OF THE FOLDER NAME HERE!
     C*
     C     ASQ#          ADD       1             ASQ#              5 0
     C                   MOVE      ASQ#          ASQ               5
     C***
     C                   MOVE      *BLANKS       @@TOBJ
     C     QQKEY         CAT(P)    @@SEQ#:0      @@TOBJ          132
     C                   CAT(P)    ASQ:0         @@TOBJ
     C                   CAT(P)    SVOID:0       @@TOBJ
     C*
     C                   ENDSR
     C*****************************************
     C     ATCH          BEGSR
     C****  WRITE A RECORD HERE FOR THE "ADDTL ATTACHMENT", MARK IT
     C*** SO WE KNOW IT'S NEWLY WRITTEN
     C****    IF ERROR STATUS, USE "X" FOR THE STAT, IF NORMAL USE "P" FOR THE
     C***       STATUS.
     ***
     C*
     C                   MOVEL     'CVTIIX1 '    CMPGM
     C                   MOVE      *BLANKS       CMUSER
     C                   MOVEL     'MDRCD   '    CMUSER
     C*
     C                   MOVE      DSDATE        CMDT
     C                   MOVE      DSTIME        CMTIME
     C*
     ***
     C                   WRITE     PATCH
     C*
     ***
     ***
     ***
     C                   ENDSR
