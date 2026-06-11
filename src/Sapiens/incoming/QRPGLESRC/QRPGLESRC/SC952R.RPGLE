     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *========================================================================
      *========================================================================
    H/TITLE SC952R - PROCESS CLAIMS DARTS EXTRACT
      *========================================================================
      * Description: This program PROCESSES THE DARTS EXTRACT OBJECTS
      *              FOR CLAIMS *************
      *
      *=====================================================================
      * Created on 12/29/2009 by c:02 for Project 135int
      *========================================================================
     * Revisions:
      *
      * 05/02/18 C:062 Ticket: 70614
      *                Use optical info instead of uncn path
      *
      * 03/15/18 C:062 Project 114IWR(Priss 48148)
      *                Remove darts delete, this is being done in SC951R.
      *
      * 08/22/17 C:062 SUPPORT 48943(114)
      *                Main Images with names longer than 12 get cut off
      *
      * 05/15/17  C:62   Project: WC2496
      *                  Changes for DataCenter and IFS processing.
      *
      * 02/18/16  C:53  Project: 114IWR
      *                 Code for Opt to Delete DARTS/attachments
      *
      * 06/26/14 C:062 SUPPORT OP114 85849
      *                Skip if CMDCID=W0070R Or W0070C
     *                                                                   *
     * 04/15/14  C:300 Project: SY195                                    *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
     *                 get the root folder name.                         *
     *
     * 09/06/2012 C:62 SUPPORT IM114 80681
     *                 DELETE FROM OPTTEMP
     *
     *  06/27/11  C:191  PROJECT: 90IGH
     *                    BRING BACK TO BASE - INCREASE PATH VARIABLE SIZE
     *
      *========================================================================
     F****
     F*  DIRECTORY FOR OUTPUT: /DARTS_EXTRACT/XXXXXXXXXXXXXXXXXXX
     F**                                        QQCPTH
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
     FSXDEP     O    E             DISK
     F                                     RENAME(PDE01:PATCH)
     F*** BEG ADD *** 02/18/16 *****************************
     Fsmmdl5    uf   e           k Disk    Rename(pmd01:pmdl5) Prefix(L5)
     Fsmmdl3    uf   e           k Disk    Rename(pmd01:pmdl3) Prefix(L3)
     Fimdolc    if   e           k Disk    Rename(pdo01:pdo0c)
     Fimdold    if   e           k Disk    Rename(pdo01:pdo0d)
     F*** END ADD *** 02/18/16 *****************************
     F*smmpl     if   e           k Disk

     D*
     D** MAKE DEFAULT IFS DIRECTORY HERE
     D***
     D*
     D ROOTFL          C                   CONST('\DARTS_EXTRACT')
     D**
     D                 DS
     D  XO                     1    132
     D                                     DIM(132)
     D @RC             DS
     D  RTCD                   1      8
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  MSG                   17    272
      ***** beg add ****** 04/15/14 **********************************
     D RPGM            C                   CONST('RTVDOCROOT')
      ***** end add ****** 04/15/14 **********************************
     d oldComma        C                   Const('''')
     d newComma        C                   Const('''''')
     D*
     D*** Beg Add *** 02/18/16 **************************************
     D docCntMD        S              5  0
     D docCntDO        S              5  0
     D qqCall          s              1
     D ##cltID         s              3
     D*** End Add *** 02/18/16 **************************************
     D*** Begin Add *** 06/05/18 ************************************
     D mainRmk         s               n   Inz(*Off)
     D isDtmDoc        s               n   Inz(*Off)
     D*** End   Add *** 06/05/18 ************************************
     D @@tobjS         s                   Like(@@tobj)

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
     C*** BEG ADD *** 02/18/16 *****************************
     C*
     C     keySMMDL3     KLIST
     C                   KFLD                    MDoFlr
     C                   KFLD                    MDoID
     C*** BEG ADD *** 02/18/16 *****************************
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
     C*** Begin Chg *** 06/05/18 ****************************************
     C**********         PARM      CMKEY         ZZKEY            40
     C                   PARM                    ZZKEY            40
     C*** End   Chg *** 06/05/18 ****************************************
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
     C*** Begin Add *** 05/15/17 *****************************************
     C                   PARM      CMUNCN        ZZUNCN          255
     C                   PARM      CMIFSP        ZZIFSP            1
     C*** End   Add *** 05/15/17 *****************************************
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
     C*** BEGIN CHG ****** 06/27/11 ************************************
     C**************       PARM           @@SPTH 80        STORE PATH
     C                   PARM                    @@SPTH          255            STORE PATH
     C*** END   CHG ****** 06/27/11 ************************************
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
     C                   PARM                    QQFCYD            7
     C                   PARM                    QQTCYD            7
     C                   PARM                    QQJDAT            7
     C                   PARM                    QQCPTH           22
     C                   PARM                    QQJNBR            6
     C*** Beg Add *** 02/18/16 *************************************
     C                   Parm                    qqDlt             1
     C*
     C* Retrieve Client ID
     C*
     C                   call      'RTVCLTID'
     C                   parm                    ##cltID
     C*** End Add *** 02/18/16 *************************************
     ***
     C***** beg add ****** 04/15/14 **********************************
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
     C***** end add ****** 04/15/14 **********************************
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C                   MOVE      *BLANKS       QQPATH
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      '1'           XXFRST            1
     C                   Z-ADD     *ZEROS        ASQ#
     C                   MOVE      *ZEROS        ASQ
     C*
     C*** Begin Add *** 05/15/17 *****************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUF             3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** End   Add *** 05/15/17 *****************************************
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
     C*** Begin Add *** 06/26/14 *****************************************
     C     CMDCID        ANDNE     'W0070R'
     C     CMDCID        ANDNE     'W0070C'
     C*** End   Add *** 06/26/14 *****************************************
     C********** CMFL01    ANDEQ*BLANK                     **CAN HAVE THIS
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
     C*** Beg Add *** 02/18/16 **************************************
     C                   Move      CMuncN        ##uncN
     C*** End Add *** 02/18/16 **************************************
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
     C*** Beg Add *** 02/18/16 **************************************
     C                   If        qqDlt = 'Y'
     C                   Call      'SC954C'
     C                   Parm      X1dcid        ##obj            12
     C                   Parm      X1fold        ##fDir           80
     C                   Parm      '  '          ##rtcd            2
     C                   Parm                    ##uncN          255
     C                   Parm      ' '           ##fsf             1
     C                   Else
     C*** End Add *** 02/18/16 **************************************
     C*
     *** X1DCID ** X1FOLD IS THE FROM
     C                   MOVE      CMSEQ#        ZZSEQ#
     C*** Begin Chg *** 06/05/18 ****************************************
     C                   MOVEL     CMKEY         ZZKEY
     C                   Eval      mainRmk = *On
     C                   EXSR      CVTTOPDF
     C**********         CALL      'SC951C1'     PLCV1
     C*****ZZRTCD        IFNE      *BLANKS
     C**********         MOVEL     ZZRTCD        QQRTCD
     C**********         MOVEL     *BLANKS       QQPATH
     ****
     C*****'**ERROR '    CAT       ZZRTCD:0      QQPATH
     ***
     C**********         ELSE
     ****
     C*
     C**********         MOVE      *ALL'0'       @@SNUM
     C**********         MOVE      ZZSNUM        @@SNUM            6            SPLF NUM
     C**********         MOVE      *BLANKS       @@RTCD
     C**********         MOVEA     *BLANKS       XO
     C****
     ***** SINCE THIS WILL BE A PDF, NEED TO PUT .PDF EXTENSION ON IT.
     C*****'.'**         SCAN      @@TOBJ        R                 3 0    90
     C******IN90         IFEQ      *ON                                            FOUND
     C**********         MOVEA     @@TOBJ        XO
     C**********         MOVEA     '.PDF'        XO(R)
     C**********         MOVEA     XO            @@TOBJ
     C**********         ELSE                                                      NOT FND
     C**********         CAT(P)    '.PDF':0      @@TOBJ
     C**********         ENDIF
     ***
     C*****@@DIR         CAT(P)    '\':0         @@SPTH
     C**********         CAT(P)    @@TOBJ:0      @@SPTH
    ****
     C**********         CALL      'SC951C2'     PLPDF
     **
     C*****@@RTCD        IFNE      *BLANKS
     C**********         MOVEL     'M'           QQRTCD
     C**********         MOVEL     *BLANKS       QQPATH
     C**********         ELSE
     C**********         MOVEL     'P'           QQRTCD
     C**********         MOVEL     @@SPTH        QQPATH
     C**********         ENDIF
     ****
     C**********         GOTO      BPAS1
     ****
     C**********         ENDIF
     C*** End   Chg *** 06/05/18 ****************************************
     C*** Beg Add *** 02/18/16 **************************************
     C                   EndIf
     C*** End Add *** 02/18/16 **************************************
     C*
     ***
     C*
     C                   ELSE
     C***
     C                   MOVE      *BLANKS       ##FOLD
     ***
     C                   MOVEL     X1FOLD        TST4              4
     ***********************
     C********** TST4      IFNE 'PDS\'
     C**         TST4      ANDNE'PDS/'
     C**         '\QDLS\'  CAT  'PDS\':0  ##FOLD 80 P
     C**                   CAT  '135\':0  ##FOLD 80 P
     C**                   ELSE
     C***** beg chg ****** 04/15/14 **********************************
     C*****              CAT(P)    '\QDLS\':0    ##FOLD
     C*** Begin Add *** 08/22/17 ***********************************
     C     CMUNCN        IFNE      ' '
     C*** Begin Add *** 05/02/18 *************************************
     C     CMFL05        ANDNE     'O'
     C*** End   Add *** 05/02/18 *************************************
     C                   EXSR      EXTDOC
     C                   MOVEL     BIGDOC        #$FOBJ
     C                   MOVEL(P)  BIGFLD        ##FOLD
     C                   ELSE
     C*** End   Add *** 08/22/17 ***********************************
      /Free
       //*** Begin Add *** 05/15/17 *****************************************
       If ##pFlr = ' ';
       //*** End   Add *** 05/15/17 *****************************************
          ##FOLD = '\' + %trim(ROOT) + '\';
       //*** Begin Add *** 05/15/17 *****************************************
       Else;
          ##FOLD = '\' + %Trim(ROOT) + '\' + %Trim(##pFlr) + '\';
       EndIf;
       //*** End   Add *** 05/15/17 *****************************************
      /End-free
     C***** end chg ****** 04/15/14 **********************************
     C***********          ENDIF
     ***
     C                   CAT(P)    X1FOLD:0      ##FOLD           80
     C*** BEGIN ADD *** 08/22/17 ***********************************
     C                   MOVEL     X1DCID        #$FOBJ
     C                   ENDIF
     C*** END   ADD *** 08/22/17 ***********************************
     C*** Beg Add *** 02/18/16 **************************************
     C                   If        qqDlt = 'Y'
     C*
     C*** Check for more than 1 SMMDP record that refernces this document
     C                   Eval      docCntMD = 0
     C     CMK           Chain     pmd01
     C                   If        %Found(SMMDL)
     C                   If        MDuncp <> ' '
     C     MDuncp        Chain     pmdl5
     C                   If        %Found(SMMDL5)
     C                   DoU       %EOF(SMMDL5)
     C                   Eval      docCntMD = docCntMD + 1
     C     MDuncp        ReadE     pmdl5
     C                   EndDo
     C                   EndIf
     C                   Else
     C     keySMMDL3     Chain     pmdl3
     C                   If        %Found(SMMDL3)
     C                   DoU       %EOF(SMMDL3)
     C                   Eval      docCntMD = docCntMD + 1
     C     keySMMDL3     ReadE     pmdl3
     C                   EndDo
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C*
     C*** Check for more than 1 IMDOP1 record that refernces this document
     C                   Eval      docCntDO = 0
     C     CMK           Chain     pdo0C
     C                   If        %Found(IMDOLC)
     C     DOdcid        Chain     pdo0d
     C                   If        %Found(IMDOLD)
     C                   DoU       %EOF(IMDOLD)
     C                   Eval      docCntDO = docCntDO + 1
     C     DOdcid        ReadE     pdo0d
     C                   EndDo
     C                   EndIf
     C                   EndIf
     C*
     C                   If        docCntMD <= 1 And docCntDO <= 1
     C                   Call      'DELOBJC'
     C                   Parm      ##FOLD        #$FDIR
     C                   Parm      X1DCID        #$FOBJ
     C                   Parm      *BLANKS       #$RC
     C                   EndIf
     C*
     C                   Else
     C*** End Add *** 02/18/16 **************************************
     C                   CALL      'CPYOBJC'
     C                   PARM      ##FOLD        #$FDIR           80
     C*** BEGIN CHG *** 08/22/17 ***********************************
     C**********         PARM      X1DCID        #$FOBJ          132
     C                   PARM                    #$FOBJ          132
     C*** END   CHG *** 08/22/17 ***********************************
     C                   PARM      @@DIR         #$TDIR           80
     C                   PARM      @@TOBJ        #$TOBJ          132
     C                   PARM      *BLANKS       #$RC              2
     C*** BEGIN ADD *** 09/06/12 ***********************************************
     C*
     C* REMOVE ITEM FROM OPTTEMP
     C*
     C                   If        ##cltID = '114'
     C     'OPTTEMP'     SCAN      #$FDIR                                 90
     C                   If        *in90 = *On
     C                   CALL      'DELOBJC'
     C                   PARM      ##FOLD        #$FDIR
     C                   PARM      X1DCID        #$FOBJ
     C                   PARM      *BLANKS       #$RC
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 09/06/12 ***********************************************
     C*** Beg Add *** 02/18/16 **************************************
     C                   EndIf
     C*** End Add *** 02/18/16 **************************************
     C*** IF ERROR OCCURS MUST KEEP IT FOR PASSING BACK TO THE CALLER
     **
     C     #$RC          IFNE      *BLANKS
     ***
     C                   MOVEL     *BLANKS       QQPATH
     ***
     C     #$RC          IFEQ      '99'                                           OBJECT ALREADY EXI
     C     #$TDIR        CAT(P)    '\':0         OPATH
     C                   CAT(P)    #$TOBJ:0      OPATH           132
     C                   MOVEL     *BLANKS       QQPATH
     C                   MOVEL     OPATH         QQPATH
     C                   MOVEL     '99      '    CMIUSR
     C                   ELSE
     C                   MOVEL     *BLANKS       QQPATH
     C     '**ERROR '    CAT(P)    #$RC:0        QQPATH
     C                   ENDIF
**********
     C                   MOVEL     'J'           QQRTCD
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
     C*** Begin Del *** 03/15/18 ************************************
     C*** Beg Add *** 02/18/16 **************************************
     C**********         If        qqDlt = 'Y'
     C**********         Call      'SC954R'
     C**********         Parm                    qqKey
     C**********         Parm                    xxSeq#
     C**********         Parm      ' '           qqCall
     C**********         EndIf
     C*** End Add *** 02/18/16 **************************************
     C*** End   Del *** 03/15/18 ************************************
     C                   ENDIF
     C*
     C*** BYPASS CM
     C     BPAS1         TAG
     C*** CHECK FOR MORE OBJECTS FOR THE DART RECORD
     C     MDK           CHAIN     PMD01                              61
     C     *IN61         DOWEQ     *OFF
      /free
     c**Chain (MDkey:MDseq#:MDuncP) pmp01;
     c**If %Found(smmpl);
     c*/end-free
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
     C*** Begin Add *** 06/05/18 **********************************
     C*
     C* Is this a DTM Document?
     C                   ExSr      ChkDtmDoc
     C                   If        isDtmDoc = *On
     C                   MoveL     x2Oid         x1dcId
     C                   MoveL     x2Oflr        x1fold
     C                   MOVE      MDseq#        ZZSEQ#
     C                   MOVEL     MDkey         ZZKEY
     C                   Eval      mainRmk = *Off
     C                   EXsR      MdExt
     C                   ExSr      CvtToPDF
     C                   Else
     C*** End   Add *** 06/05/18 **********************************
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
     C********   '\QDLS\'  CAT  'PDS\':0  ##FOLD 80 P
     C*********            CAT  '135\':0  ##FOLD 80 P
     C*******************  CAT  X2OFLR:0  ##FOLD 80 P
     C***** beg chg ****** 04/15/14 **********************************
     C*****       '\QDLS\'      CAT(P)    X2OFLR:0      ##FOLD           80
      /Free
       //*** Begin Chg *** 05/15/17 **********************************
       //**********##FOLD = '\' + %trim(ROOT) + '\' + %trim(X2OFLR);
       If ##pFlr = ' ';
          If MDuncF = 'Q';
             ##FOLD = '\QDLS\' + %trim(X2OFLR);
          Else;
             ##FOLD = '\' + %trim(ROOT) + '\' + %trim(X2OFLR);
          EndIf;
       Else;
          If MDuncF = 'Q';
             ##FOLD = '\QDLS\' +  %Trim(##pFlr) + '\' + %trim(X2OFLR);
          Else;
             ##FOLD = '\' + %trim(ROOT) + '\' + %Trim(##pFlr) +
                      '\' + %trim(X2OFLR);
          EndIf;
       EndIf;
       //*** End   Chg *** 05/15/17 **********************************
      /End-free
     C***** end chg ****** 04/15/14 **********************************
     C*** Beg Add *** 02/18/16 **************************************
     C                   If        qqDlt = 'Y'
     C*
     C*** Check for more than 1 SMMDP record that refernces this document
     C                   Eval      docCntMD = 0
     C                   If        MDuncp <> ' '
     C     MDuncp        Chain     pmdl5
     C                   If        %Found(SMMDL5)
     C                   DoU       %EOF(SMMDL5)
     C                   Eval      docCntMD = docCntMD + 1
     C     MDuncp        ReadE     pmdl5
     C                   EndDo
     C                   EndIf
     C                   Else
     C     keySMMDL3     Chain     pmdl3
     C                   If        %Found(SMMDL3)
     C                   DoU       %EOF(SMMDL3)
     C                   Eval      docCntMD = docCntMD + 1
     C     keySMMDL3     ReadE     pmdl3
     C                   EndDo
     C                   EndIf
     C                   EndIf
     C*
     C*** Check for more than 1 IMDOP1 record that refernces this document
     C                   Eval      docCntDO = 0
     C     CMK           Chain     pdo0C
     C                   If        %Found(IMDOLC)
     C     DOdcid        Chain     pdo0d
     C                   If        %Found(IMDOLD)
     C                   DoU       %EOF(IMDOLD)
     C                   Eval      docCntDO = docCntDO + 1
     C     DOdcid        ReadE     pdo0d
     C                   EndDo
     C                   EndIf
     C                   EndIf
     C*
     C                   If        docCntMD <= 1 And docCntDO <= 1
     C                   Call      'DELOBJC'
     C                   Parm      ##fold        #$fDir
     C                   Parm      X2oid         #$fObj
     C                   Parm      *Blanks       #$rc
     C                   EndIf
     C*
     C                   Else
     C*** End Add *** 02/18/16 **************************************
      /free
       @@tobjS = @@tobj;
       x2Oid   = '"' + %Trim(x2Oid) + '"';
       @@tobj  = '"' + %Trim(@@tobj) + '"';
      /end-free
     C                   CALL      'CPYOBJC'
     C                   PARM      ##FOLD        #$FDIR           80
     C                   PARM      X2OID         #$FOBJ          132
     C                   PARM      @@DIR         #$TDIR           80
     C                   PARM      @@TOBJ        #$TOBJ          132
     C                   PARM      *BLANKS       #$RC              2
     C*** BEGIN ADD *** 09/06/12 ***********************************************
     C*
     C* REMOVE ITEM FROM OPTTEMP
     C*
     C                   If        ##cltID = '114'
     C     'OPTTEMP'     SCAN      #$FDIR                                 90
     C                   If        *in90 = *On
     C                   CALL      'DELOBJC'
     C                   PARM      ##FOLD        #$FDIR
     C                   PARM      X2OID         #$FOBJ
     C                   PARM      *BLANKS       #$RC
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 09/06/12 ***********************************************
     C*** Beg Add *** 02/18/16 **************************************
     C                   EndIf
     C*** End  dd *** 02/18/16 **************************************
     C*** Begin Add *** 06/05/18 **********************************
     C                   EndIf
     C*** End   Add *** 06/05/18 **********************************
     ******* WRITE CLATCH HERE WITH 'P' AS STATUS!!!
     *** ??? IF ERROR WHAT TO DO ???
     **8 PASS ERROR BACK TO CALLER ?? NO HERE IF ERROR WRITE NEW RECORD WITH ERR
     **
     C*** Begin Add *** 05/15/17 *******************************************
     C*                  If        MDuncP <> ' '           x2OidS
     C*                  Movel     MDuncP        oPath     @@tobjS
     C*                  Else
     C*** End   Add *** 05/15/17 *******************************************
     C     #$TDIR        CAT(P)    '\':0         OPATH           132
     C                   CAT(P)    @@tobjS:0     OPATH
     C*** Begin Add *** 05/15/17 *******************************************
     C*                  EndIf
     C*** End   Add *** 05/15/17 *******************************************
     **
     C     #$RC          IFNE      *BLANKS
     C                   MOVEL     'X'           CMSTAT
     C                   MOVEL     *BLANKS       CMIUSR
     C                   MOVEL     #$RC          CMIUSR
     C                   MOVEL     *BLANKS       CMUNCN
     C     #$RC          IFEQ      '99'
     C                   MOVEL     OPATH         CMUNCN
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'P'           CMSTAT
     C                   MOVEL     *BLANKS       CMIUSR
     C                   MOVEL     OPATH         CMUNCN
     C                   ENDIF
     **
     C                   EXSR      ATCH
     C*                  EndIf
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
      *          XXXXXXXXPLDUMP/CO-FND-FYR-CASE/OBJECTNAME.EXT
      *
     C*** DIRECTORY NAME IS '\' + CO#+FND + '\' + FYR+CASE
     C                   MOVE      *BLANKS       NEWDIR
     C***********'\PDS\'   CAT  '135':0   NEWDIR    P
     C*** Begin Add *** 05/15/17 ****************************
     C     ##pflr        IfNE      ' '
     C     '/'           CAT(p)    ##pflr:0      NEWDIR
     C                   EndIf
     C*** End   Add *** 05/15/17 ****************************
     C                   CAT(P)    ROOTFL:0      NEWDIR
     C                   CAT(P)    QQCPTH:0      NEWDIR
     C                   CAT(P)    '\':0         NEWDIR
     C                   CAT(P)    #1CO#:0       NEWDIR           80
     C                   CAT(P)    #1FND:0       NEWDIR
     C                   CAT(P)    #1FYR:0       NEWDIR
     C                   CAT(P)    #1CASE:0      NEWDIR
     C*** BUILD DFTDIR
     C*** Begin Add *** 05/15/17 ****************************
     C     ##PFLR        IFNE      *BLANKS
     C                   EVAL      DftDir = '/' + %Trim(##pflr) + %Trim(rootfl)
     C                             + %Trim(qqcpth) + %Trim(qqpath)
     C                   ELSE
     C*** End   Add *** 05/15/17 ****************************
     C                   MOVE      *BLANKS       DFTDIR
     C********** '\PDS\'   CAT  '135':0   DFTDIR    P
     C                   CAT(P)    ROOTFL:0      DFTDIR           80
     C                   CAT(P)    QQCPTH:0      DFTDIR
     C                   CAT(P)    QQPATH:0      DFTDIR           80
     C*** Begin Add *** 05/15/17 ****************************
     C                   endif
     C*** End   Add *** 05/15/17 ****************************
     ***
     C****       '\'       CAT  #1CO#:0   DFTDIR 80 P
     C****                 CAT  #1FND:0   DFTDIR    P
     C***
     C*
     C                   ENDSR
     C*** Begin Add *** 06/05/18 ***********************************
     C*=============================================================
     C* CHKDTMDOC - Check to see if object is a DTM document
     C*=============================================================
     C     CHKDTMDOC     BEGSR
     C*
     C                   Eval      isDtmDoc = *Off
     C                   If        %Subst(x2Oid:13:28) = ' ' And
     C                             %Subst(x2Oid:10:3) >= '000' And
     C                             %Subst(x2Oid:10:3) <= '999' And
     C                             MDoDsc = 'CLAIM DENIAL LETTER'
     C                   Eval      isDtmDoc = *On
     C                   EndIf
     C*
     C                   ENDSR
     C*=============================================================
     C* CVTTOPDF - Convert DTM Documents to PDF
     C*=============================================================
     C     CVTTOPDF      BEGSR
     C*
     C                   CALL      'SC951C1'     PLCV1
     C     ZZRTCD        IFNE      *BLANKS
     C                   MOVEL     ZZRTCD        QQRTCD
     C                   MOVEL     *BLANKS       QQPATH
     ****
     C     '**ERROR '    CAT       ZZRTCD:0      QQPATH
     ***
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
     C                   CALL      'SC951C2'     PLPDF
     **
     C     @@RTCD        IFNE      *BLANKS
     C                   MOVEL     'M'           QQRTCD
     C                   MOVEL     *BLANKS       QQPATH
     C                   ELSE
     C                   MOVEL     'P'           QQRTCD
     C                   MOVEL     @@SPTH        QQPATH
     C                   ENDIF
     ****
     C                   If        mainRmk = *On
     C                   GOTO      BPAS1
     C                   EndIf
     ****
     C                   ENDIF
     C*
     C                   ENDSR
     C*** End   Add *** 06/05/18 ***********************************
     C*** Begin Add *** 08/22/17 ***********************************
     C*=============================================================
     C* EXTDOC - Extract Document from UNC Path
     C*=============================================================
     C     EXTDOC        BEGSR
     C*
     C                   Z-ADD     1             PS                3 0
     C                   Z-ADD     1             LT                3 0
     C     *IN91         DOUEQ     *OFF
     C     '/'           SCAN      CMUNCN:PS     P                 3 0    91
     C     *IN91         IFEQ      *OFF
     C                   SUBST     CMUNCN:PS     BIGDOC          132
     C     LT            SUBST     CMUNCN:1      BIGFLD           80
     C                   LEAVE
     C                   ENDIF
     C     P             ADD       1             PS
     C                   Z-ADD     P             LT
     C                   Z-ADD     0             P
     C                   ENDDO
     C*
     C                   ENDSR
     C*** End   Add *** 08/22/17 ***********************************
     C*****************************************
     C     GETEXT        BEGSR
     C****  BUILD THE TOOBJECT NAME USING EXTENTION FROM XXDCID
     C*** @@TOBJ 135 IS CREATED
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
     C*** Begin Add *** 08/22/17 ***********************************
     C     CMUNCN        IFNE      ' '
     C                   EXSR      EXTDOC
     C                   CAT(P)    BIGDOC:0      @@TOBJ
     C                   ELSE
     C*** End   Add *** 08/22/17 ***********************************
     C                   CAT(P)    XXDCID:0      @@TOBJ
     C*** Begin Add *** 08/22/17 ***********************************
     C                   ENDIF
     C*** End   Add *** 08/22/17 ***********************************
     C*
     C                   ENDSR
     C*****************************************
     C     MDEXT         BEGSR
     C****  BUILD THE TOOBJECT NAME USING EXTENTION FROM SVOID
     C*** @@TOBJ 135 IS CREATED     SVOID OFLR
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
     C                   MOVEL     'SC952R  '    CMPGM
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
