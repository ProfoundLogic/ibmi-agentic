     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*  Program: F0461RTX                                            *
     H*  Description: Work With EOMBs Header Info update/display      *
     H*               TEXAS VERSION/HOSPITAL BILLS                    *
     H*                                                               *
     H*  Created:  C:117  12/11/2000                                  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*
     H*  08/11/15 C:079 Project: SY360
     H*                 Load SCICDIND
     H*
     H*   4/16/15 C:212 Project: MC395
     H*                 ICD10 TX 837 EDI Changes. DSPF also changed.
     H*
     H*  04/24/14 C:280 Support: 85416
     H*                 change field size on screen rlic recompile
     H*
     H*  02/16/09 C:212 SUPPORT:67199                                 *
     H*                 DRG CODE AND DISCHARGE DATE SHOULD NOT BE     *
     H*                 MANDATORY FOR OUTPATIENT BILL                 *
     H*
     H*  10/30/08 C:198 PROJECT:TX048
     H*                 CHANGED SCFLIC TO SCRLIC
     H*
     H*  07/29/08 C:191 PROJECT:TX045 PRISS#16
     H*                 ADD DISCOUNT DATE FIELD
     H*
     H*  07/22/08 C:191 PROJECT:TX045 PRISS#14
     H*                 ADD DRG# FIELD
     H*
     H*  04/30/08 C:191 PROJECT:TX045
     H*                 ADD NEW FIELD
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  09/18/03 C:079 SUPPORT: MC025 38833
     H*                 CORRECT ERROR CHECK ON SCTAPE
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*                                                               *
     H*  07/18/02 C:79 PROJECT TX017                                  *
     H*                POPULATE EUDCN WITH SCTSS                      *
     H*                                                               *
     H*  02/08/02 C:79 PROJECT TX017                                  *
     H*                UPDATE FAEAP RECORD                            *
     H*                                                               *
     H*  02/08/02 C:62 PROJECT TX017                                  *
     H*                ALLOW 'R' TO BE VALID VALUE FOR EDI FLAG.      *
     H*                                                               *
     H*  10/16/01 C:62 PROJECT:MC025 29089                            *
     H*                ADD F11=EOMB/NOTES                             *
     H*                                                               *
     H*****************************************************************
     FF0461FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*  Physical file definition (for maintaining record)
     FFAEUP     UF A E             DISK
     F                                     RENAME(PEU01:DBRCDF)
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*** BEG CHG *** 02/08/02 ****************************************
     F*********FAEAL1  IF  E           K        DISK
     FFAEAL1    UF   E           K DISK
     F*** END CHG *** 02/08/02 ****************************************
     F*
     FFMCDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F*  GENERIC TABLE FILE
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*  PROCEDURE CODE MASTER FILE
     FFMPRL     IF   E           K DISK
     F*  RESONABLE AND CUSTOMARY FILE
     FFMRCP     IF   E             DISK
     FFMRCL1    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     F*** BEG ADD *** 04/07/08 **************************************
     FFTDGP     IF   E             DISK
     FFTDGL     IF   E           K DISK
     F                                     RENAME(PDG01:PDG02)
     F*** END ADD *** 04/07/08 **************************************
     F*** Beg Add *** 04/16/15 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 04/16/15 ***************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) -
     D                                     Header Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*   Data structure of FL License # to make sure first two
     D*     characters of license # are alpha
     D                 DS
     D  SCLIC#                 1      9
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      9
     D*** BEG ADD ****** 04/30/08 ************************************
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D FL1const        C                   CONST('Referring provider s-
     D                                     tate lic#')
     D FL2const        C                   CONST('Drg code')
     D FL3const        C                   CONST('Discharge date')
     D                 DS
     D  XXNOT1                 1     70
     D  XXNT1                  1     38
     D  XXNT2                 39     70
     D                 DS
     D  XXNOT2                 1     70
     D  XXNT21                 1     16
     D  XXNT22                17     70

     I*** END ADD ****** 04/30/08 ************************************
     D*** Beg Add *** 04/16/15 ***************************************
     D RtvICD10Code    pr                  extpgm('F2554R')
     D incall                         1
     D inKC                           1
     D inKL                           1
     D outICD10                       7
     D inLOST                         1
     D*
     D incall          s              1
     D inKC            s              1
     D inKL            s              1
     D outICD10        s              7
     D inLOST          s              1

     D kkicd10         s                   like(iccode)
     D kkind           s                   like(icind)
     D hldia1          s              7    inz
     D hldia7          s              7    inz
     D*** End Add *** 04/16/15 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR FAEUP
     C                   PARM                    @@RRN2            9 0          FOR FAEOP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     APIPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EOSUB#        #2SUB#            5 0
     C                   PARM      EOFND         #2FND             3 0
     C                   PARM      EOFYR         #2FYR             3 0
     C                   PARM      EOCASE        #2CASE            7 0
     C                   PARM      EOEOB#        #2EOB#            9 0
     C                   PARM      EORCN         #2RCN             3 0
     C                   PARM                    #2DSEQ            5 0
     C                   PARM      'H'           #2CALL            1
     C                   PARM                    #2OVAL           70
     C                   PARM                    #2NVAL           70
     C*** END ADD ****** 04/30/08 ************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*   KEY LIST FOR PAYMENT PLAN UPDATE
     C*
     C     PTKEY         KLIST
     C                   KFLD                    EUCO#
     C                   KFLD                    EUSUB#
     C                   KFLD                    EUFND
     C                   KFLD                    EUFYR
     C                   KFLD                    EUCASE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C*
     C*  KEY LIST FOR UPDATING EAEDI
     C*
     C     EAKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C* KEY LIST FOR PROCEDURE CODE MASTER FILE
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*
     C* KEY LIST FOR REASONABLE AND CUSTOMARY FILE
     C*
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C*** BEG ADD *** 07/22/08 ****************************************
     C     DGKY          KLIST
     C                   KFLD                    KKDRG#
     C                   KFLD                    KKEFDT
     C*** END ADD *** 07/22/08 ****************************************
     C*
     C*** Beg Add *** 04/16/15 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kkind
     C*** End Add *** 04/16/15 ***************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CACO#         QQCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    CASS#         KKSS#
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    EUDIA1        XXDIA
     C     *LIKE         DEFINE    EUDIA1        XXDIA1
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    DIICD9        ##ICD9
     C     *LIKE         DEFINE    DILOST        ##LOST
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    SCPCM         HLPCM
     C     *LIKE         DEFINE    SCPDSC        HLDESC
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOBLST        KKBLST
     C*** Beg Del *** 04/16/15 ***************************************
     C**** *LIKE         DEFINE    EUDIA1        HLDIA1
     C*** End Del *** 04/16/15 ***************************************
     C     *LIKE         DEFINE    EUDIA1        HLDIA2
     C     *LIKE         DEFINE    EUDIA1        HLDIA3
     C     *LIKE         DEFINE    EUDIA1        HLDIA4
     C     *LIKE         DEFINE    DILOST        HLLOS1
     C     *LIKE         DEFINE    DILOST        HLLOS2
     C     *LIKE         DEFINE    DILOST        HLLOS3
     C     *LIKE         DEFINE    DILOST        HLLOS4
     C     *LIKE         DEFINE    EUDCN         HLTSS
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*** BEG ADD *** 07/22/08 **************************************
     C     *LIKE         DEFINE    DGDRG#        KKDRG#
     C*** END ADD *** 07/22/08 **************************************
     C*** BEG ADD ****** 04/30/08 *************************************
     C     *LIKE         DEFINE    EUFLIC        XOFLIC
     C*** END ADD ****** 04/30/08 *************************************
     C*** BEG ADD ****** 07/22/08 *************************************
     C     *LIKE         DEFINE    EUDRG#        XODRG#
     C*** END ADD ****** 07/22/08 *************************************
     C*** BEGIN ADD *** 07/29/08 **************************************
     C     *LIKE         DEFINE    EUDCDT        HLDCDT
     C     *LIKE         DEFINE    SCDCDT        XODCDT
     C*** END   ADD *** 07/29/08 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Make sure 1st Diag Code is populated & make sure that it is
     C*    a valid code
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   GOTO      CK1END
     C                   ELSE
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 04/16/15 ************************************
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        7190
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   Else
     C/free
         kkICD10 = SCDIA1;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in71 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
B01  C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDIF
     C*
     C*** BEG ADD *** 07/29/08 ****************************************
     C* VALIDATE DISCHARGE DATE
     C*
     C*** BEG ADD *** 02/16/09 **************************************
     C     EUICN         IFEQ      'IR'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C*** END ADD *** 02/16/09 **************************************
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        8190
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEG ADD *** 02/16/09 **************************************
     C                   ENDIF
     C*** END ADD *** 02/16/09 **************************************
     C*
     C     SCDCDT        IFNE      *ZEROS
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCDCDT
     C                   ENDIF
     C                   ENDIF                                                  SCDCDT NE *ZEROS
     C*
     C* DISCHARGE DATE CANNOT BE LESS THAN ADMISSION DATE
     C*
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDCDT
     C*
     C     HLDCDT        IFNE      *ZEROS
     C     HLDCDT        ANDLT     EUADDT
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C*** END ADD *** 07/29/08 ****************************************
     C* Procedure Code Method is required
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        906746
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* Validate Procedure Code Method
     C*
     C     SCPCM         IFNE      '0'
     C     SCPCM         ANDNE     '4'
     C     SCPCM         ANDNE     '5'
     C     SCPCM         ANDNE     '9'
     C                   SETON                                        906846
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM NE '0'
     C*
     C* If Proc Code Method =0 Principal Procedure Code MUST be BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC1        ANDNE     *BLANKS
     C                   SETON                                        906439
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF Proc Code Method =0 Secondary Procedure Code MUST be BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC2        ANDNE     *BLANKS
     C                   SETON                                        906238
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* Validate procedure codes base on procedure code methods entered
     C* if Proc Code Method NE 0 THEN Princ Procedure Code Required
     C     SCPCM         IFNE      '0'
     C*
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906939
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=4
     C*
     C     SCPCM         IFEQ      '5'
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906939
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=5
     C*
     C     SCPCM         IFEQ      '9'
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 04/16/15 ************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLS1         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906939
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   Else
     C/free
         kkICD10 = SCPRC1;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in39 = *on;
           *in69 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
B01  C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDIF                                                  SCPCM =9
     C*
     C* IF Proc Code Method NE 0 THEN Secondary Code is optional
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906338
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906338
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 04/16/15 ************************************
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLS2         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906338
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   Else
     C/free
         kkICD10 = SCPRC2;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in38 = *on;
           *in63 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
B01  C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C                   ENDIF                                                  SCPCM NE 0
     C*
     C*  Make sure that Type of Service Status is "F" or "I"
     C*
     C     SCTSS         IFEQ      *BLANKS
     C                   SETON                                        907237
     C                   ELSE
     C     SCTSS         IFEQ      'F'
     C     SCTSS         OREQ      'I'
     C                   SETOFF                                       72
     C     SCTSS         IFEQ      'F'
     C                   MOVEL     'FINAL'       SCTSSD
     C                   ENDIF
     C     SCTSS         IFEQ      'I'
     C                   MOVEL     'INTRIM'      SCTSSD
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        907237
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Make sure that Data Subm flag is either "Y" or "N" & not
     C*    blank
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ELSE
     C*** BEG DEL *** 09/18/03 ****************************************
     C**********
     C********** SCTAPE    IFEQ 'Y'
     C********** SCTAPE    OREQ 'N'
     C****BEGIN*ADD *** 02/08/02 **************************************
     C********** SCTAPE    OREQ 'R'
     C********** EOSYS     ANDNE'H'
     C****END***ADD *** 02/08/02 **************************************
     C**********           SETOF                     76
     C**********           ELSE
     C**********           SETON                     7690
     C**********           GOTO CK1END
     C**********           ENDIF
     C*** END DEL *** 09/18/03 ****************************************
     C*** BEG ADD *** 09/18/03 ****************************************
     C     SCTAPE        IFNE      'Y'
     C     SCTAPE        ANDNE     'N'
     C     SCTAPE        ANDNE     'R'
     C                   SETON                                        7690
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD *** 09/18/03 ****************************************
     C                   ENDIF
     C*** BEG ADD ****** 04/30/08 ************************************
     C*** BEG CHG ****** 10/30/08 ************************************
     C********** SCFLIC    IFEQ *BLANKS
     C     SCRLIC        IFEQ      *BLANKS
     C*** END CHG ****** 10/30/08 ************************************
     C                   SETON                                        7790
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD *** 07/22/08 ****************************************
     C* VALIDATE DRG#
     C*
     C*** BEG CHG *** 02/16/09 ****************************************
     C*******    *IN48     IFEQ *ON
     C*******    SCDRG#    ANDEQ0
     C*******              SETON                     9079
     C     EUICN         IFEQ      'IR'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C     SCDRG#        IFEQ      *ZEROS
     C     *IN48         IFEQ      *ON
     C                   SETON                                        9079
     C                   ENDIF
     C*** END CHG *** 02/16/09 ****************************************
     C                   ELSE
     C     SCDRG#        IFNE      0
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99=*ON
     C                   ENDIF                                                  SCDRG#<>0
     C                   ENDIF                                                  SCDRG#=0
     C*** BEG ADD *** 02/16/09 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/16/09 ****************************************
     C*** END ADD *** 07/22/08 ****************************************
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C                   MOVEL     SCDIA1        HLDIA1
     C                   MOVEL     SCLOS1        HLLOS1
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C/Free
          HLDIA7 = SCDIA1;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa1 = HLDIA7;
             *in90 = *on;
             *in36 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in47 = *on;
          Endif;

          If scDia1 <> *blanks;
            kkICD10 = scDIa1;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS1 = 'D';
              SCDSC1 = ICDSC1;
              SCDS1  = ICFCODE;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 04/16/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA1
     C                   MOVEL     XXLOST        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C*** BEG CHG ****** 04/30/08 ************************************
     C***********          SETON                     9046
     C                   SETON                                        9047
     C*** END CHG ****** 04/30/08 ************************************
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIA1
     C                   MOVEL     HLLOS1        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C                   SETON                                        9036
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C* PROCEDURE CODE METHOD SEARCH
     C*
     C     HFLD          WHENEQ    'SCPCM'
     C                   MOVEL     SCPCM         HLPCM
     C                   MOVEL     SCPDSC        HLDESC
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'PCMT'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLPCM         SCPCM
     C                   MOVEL     HLDESC        SCPDSC
     C                   SETON                                        9046
     C*
     C                   ELSE
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCPCM
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   SETON                                        9039      pos cursor
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C                   ENDIF                                                  ##KL='1'
     C*
     C* PROCEDURE CODE SEARCHES
     C*
     C     HFLD          WHENEQ    'SCPRC1'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        905039
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9039
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC1        HLPRC1
     C                   MOVEL     SCP1DS        HLP1DS
     C                   MOVE      SCLS1         HLLOS1
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   MOVE      HLLOS1        SCLS1
     C                   SETON                                        9039
     C                   ELSE
     C     ##RRN         CHAIN     PRC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCLS1
     C                   MOVEL(P)  RCCPT4        SCPRC1
     C                   MOVEL(P)  RCDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9038
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '5'
     C*
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC1        HLPRC1
     C                   MOVEL     SCP1DS        HLP1DS
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****                MOVE EOAST     ##ST    2
     C                   MOVE      EDITST        ##ST              2
     C*** END CHG *** 04/23/07 ****************************************
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC1
     C                   MOVEL(P)  ##DESC        SCP1DS
     C                   MOVE      *BLANKS       SCLS1
     C                   SETON                                        9038
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC1        HLPRC1            7
     C                   MOVEL     SCLS1         HLLOS1
     C                   MOVEL     SCP1DS        HLP1DS           40
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C/Free
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPrc1 = HLPRC1;
             *in90 = *on;
             *in39 = *on;
          Else;
             scPrc1 = outICD10;
             *in90 = *on;
             *in38 = *on;
          Endif;

          If scPrc1 <> *blanks;
            kkICD10 = scPrc1;
            kkind = 'O';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLS1 = 'O';
              SCP1DS = ICDSC1;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 04/16/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL     HLLOS1        SCLS1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC1
     C                   MOVE      ##LOST        SCLS1
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLS1         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9038
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC2'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        905338
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9038
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC2        HLPRC2
     C                   MOVEL     SCP2DS        HLP2DS
     C                   MOVE      SCLS2         HLLOS2
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   MOVE      HLLOS2        SCLS2
     C                   SETON                                        9038
     C                   ELSE
     C     ##RRN         CHAIN     PRC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCLS2
     C                   MOVEL(P)  RCCPT4        SCPRC2
     C                   MOVEL(P)  RCDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9037
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '5'
     C*
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC2        HLPRC2
     C                   MOVEL     SCP2DS        HLP2DS
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****                MOVE EOAST     ##ST    2
     C                   MOVE      EDITST        ##ST              2
     C*** END CHG *** 04/23/07 ****************************************
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9038
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC2
     C                   MOVEL(P)  ##DESC        SCP2DS
     C                   MOVE      *BLANKS       SCLS2
     C                   SETON                                        9037
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC2        HLPRC2            7
     C                   MOVE      SCLS2         HLLOS2
     C                   MOVEL     SCP2DS        HLP2DS           40
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C/Free
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPrc2 = HLPRC2;
             *in90 = *on;
             *in38 = *on;
          Else;
             scPrc2 = outICD10;
             *in90 = *on;
             *in37 = *on;
          Endif;

          If scPrc2 <> *blanks;
            kkICD10 = scPrc2;
            kkind = 'O';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLS2 = 'O';
              SCP2DS = ICDSC1;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 04/16/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL     HLLOS2        SCLS2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9038
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC2
     C                   MOVE      ##LOST        SCLS2
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLS2         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9037
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCTAPE'
     C                   SETON                                        9074
     C                   SETOFF                                       7366
     C*** BEG ADD ****** 04/30/08 ************************************
     C*** BEG CHG ****** 10/30/08 ************************************
     C********** HFLD      WHEQ 'SCFLIC'
     C     HFLD          WHENEQ    'SCRLIC'
     C*** END CHG ****** 10/30/08 ************************************
     C                   SETON                                        9078
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 07/29/08 ************************************
     C     HFLD          WHENEQ    'SCDCDT'
     C                   SETON                                        9084
     C*** END ADD ****** 07/29/08 ************************************
     C*
     C     HFLD          WHENEQ    'SCRCVD'
     C                   SETON                                        9034
     C*
     C     HFLD          WHENEQ    'SCTSS'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCTSS         HLTSS
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'SVTZ'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLTSS         SCTSS
     C                   SETON                                        9037
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTSS
     C                   MOVEL     GTDESC        SCTSSD
     C                   SETON                                        9031
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*** BEG ADD ****** 07/22/08 ************************************
     C*   SCDRG# SEARCH
     C     HFLD          WHENEQ    'SCDRG#'
     C*
     C                   CALL      'F2354R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      0             KKDRG#
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PDG01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDRG#        SCDRG#
     C                   MOVEL     DGDESC        SCDGDS
     C                   SETON                                        36
     C                   ENDIF                                                  *IN99=*OFF
     C                   ELSE
     C                   SETON                                        49
     C                   ENDIF                                                  #RRN<>0
     C*** END ADD ****** 07/22/08 ************************************
     C*
     C                   ENDSL
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 10/16/01 **************************************
     C*================================================================
     C* KKDO - F11=EOMB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C*
     C                   CALL      'F0355R'
     C                   PARM      *BLANKS       ##CALL
     C                   PARM      *BLANKS       ##KC
     C                   PARM      *BLANKS       ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 10/16/01 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EUCO#         KKCO#
     C                   MOVEL     EUSUB#        KKSUB#
     C                   MOVEL     EUFND         KKFND
     C                   MOVEL     EUFYR         KKFYR
     C                   MOVEL     EUCASE        KKCASE
     C*
     C                   MOVEL(P)  EUSUB#        XXSUB#            5
     C                   MOVEL(P)  EUFND         XXFND             3
     C                   MOVEL(P)  EUFYR         XXFYR             3
     C                   MOVEL(P)  EUCASE        XXCASE            7
     C*
     C     CAKY          CHAIN     PCA01                              60
     C                   MOVEL     CASS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C                   MOVE      EUEDI         SCTAPE
     C                   MOVEL     EUDCN         SCTSS
     C     SCTSS         IFEQ      'F'
     C                   MOVEL     'FINAL'       SCTSSD
     C                   ENDIF
     C     SCTSS         IFEQ      'I'
     C                   MOVEL     'INTRIM'      SCTSSD
     C                   ENDIF
     C*
     C     EUDIA1        IFNE      *BLANKS
     C                   MOVEL     EUDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     EUDIA1        SCDIA1
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCDS1
     C                   MOVE      *BLANKS       SCDIA1
     C                   ENDIF
     C*
     C*  Get Proc Code Method, Princ Proc, & Other Proc
     C*
     C                   MOVEL     EUPCM         SCPCM
     C                   MOVE      'PCMT'        KKTBID
     C                   MOVEL(P)  SCPCM         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUPRC1        SCPRC1
     C                   MOVE      EUPRC1        SCLS1
     C     SCPCM         IFEQ      '4'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLS1         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC2        SCPRC2
     C                   MOVE      EUPRC2        SCLS2
     C     SCPCM         IFEQ      '4'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLS2         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*** BEG ADD ****** 04/30/08 ************************************
     C*** BEG CHG ****** 10/30/08 ************************************
     C*************        MOVELEUFLIC    SCFLIC
     C*************        MOVELEUFLIC    XOFLIC
     C                   MOVEL     EURLIC        SCRLIC
     C                   MOVEL     EURLIC        XOFLIC
     C*** END CHG ****** 10/30/08 ************************************
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD *** 04/07/08 ****************************************
     C                   MOVEL     EUDGDS        SCDGDS
     C*
     C                   SETOFF                                       48
     C     EUICN         IFEQ      'IR'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C                   SETON                                        48
     C                   ENDIF                                                  EUICN
     C*
     C                   Z-ADD     EUDRG#        SCDRG#
     C                   Z-ADD     EUDRG#        XODRG#
     C     EUDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDESC        SCDGDS
     C                   ENDIF
     C*** END ADD *** 04/07/08 ****************************************
     C*** BEG ADD *** 07/29/08 ****************************************
     C                   Z-ADD     EUDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDCDT
     C                   MOVEL     SRDTO         XODCDT
     C*** END ADD *** 07/29/08 ****************************************
     C*
     C*** BEG ADD ****** 04/16/15 ************************************
     C/Free
       //*** Beg Add *** 08/11/15 ***********************************
         scICDInd = EUicdInd;
       //*** End Add *** 08/11/15 ***********************************
         If SCICDIND = 'Y';

           // Diag Codes

           SCDIA1 = EUICD1001;
           kkICD10 = SCDIA1;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSC1 = ICDSC1;
             SCDS1 = ICFCODE;
             SCLOS1 = 'D';
           EndIf;

           If SCPCM = '9';

              SCPRC1  = EUPRC1001;
              kkICD10 = SCPRC1;

              Chain (kkICD10:'O') FMICD10L1;
              If %Found(FMICD10L1);
                SCP1DS = ICDSC1;
              EndIf;

              SCPRC2  = EUPRC1002;
              kkICD10 = SCPRC2;

              Chain (kkICD10:'O') FMICD10L1;
              If %Found(FMICD10L1);
                SCP2DS = ICDSC1;
              EndIf;

           EndIf;

 E01     EndIf;
      /End-Free
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCDIA1        EUDIA1
     C                   MOVEL     SCPCM         EUPCM
     C                   MOVEL     SCPRC1        EUPRC1
     C                   MOVE      SCLS1         EUPRC1
     C                   MOVEL     SCPRC2        EUPRC2
     C                   MOVE      SCLS2         EUPRC2
     C*** BEG ADD ****** 04/16/15 ************************************
     C/Free
       //*** Beg Add *** 08/11/15 ***********************************
         EUicdInd = scICDInd;
       //*** End Add *** 08/11/15 ***********************************
         If SCICDIND = 'Y';

           EUICD1001 = SCDIA1;
           EUPRC1001 = SCPRC1;
           EUPRC1002 = SCPRC2;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 04/16/15 ************************************
     C*** BEG ADD *** 07/18/02 ****************************************
     C                   MOVEL     SCTSS         EUDCN
     C*** END ADD *** 07/18/02 ****************************************
     C*** BEG ADD ****** 04/30/08 ************************************
     C*** BEG CHG ****** 10/30/08 ************************************
     C************         MOVELSCFLIC    EUFLIC
     C************         MOVELSCFLIC    EORLIC
     C                   MOVEL     SCRLIC        EURLIC
     C                   MOVEL     SCRLIC        EORLIC
     C*** END CHG ****** 10/30/08 ************************************
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD *** 07/22/08 **************************************
     C                   MOVEL(P)  SCDRG#        EUDRG#
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDESC        EUDGDS
     C                   ENDIF
     C*** END ADD *** 07/22/08 **************************************
     C*** BEG ADD *** 07/29/08 ****************************************
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDCDT
     C*** END ADD *** 07/29/08 ****************************************
     C*
     C                   MOVEL     SCTAPE        EUEDI
     C                   MOVEL     SCTAPE        EOTAPE
     C     EAKEY         CHAIN     PEA01                              60
     C*** BEG ADD *** 02/08/02 ****************************************
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     SCTAPE        EAEDI
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      DSTIME        EATIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   UPDATE    PEA01
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*** END ADD *** 02/08/02 ****************************************
     C*
     C                   MOVE      Q1USER        EUUSER
     C                   MOVE      Q1PGM         EUPGM
     C                   MOVE      DSTIME        EUTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C*
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   MOVE      DSTIME        EOTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      DSTIME        EATIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO
     C*** BEGIN ADD *** 10/16/01 **************************************
     C     *INKK         CASEQ     *ON           KKDO
     C*** END   ADD *** 10/16/01 **************************************
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C*** BEG ADD *** 04/23/07 ****************************************
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'C'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C*** END ADD *** 04/23/07 ****************************************
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN2        CHAIN     PEO01                              60
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   UPDATE    PEO01
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEUP
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
     C                   ENDSR
     C*** BEG ADD ****** 04/30/08 ************************************
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF EITHER OF THE FIELDS IS CHANGED THEN CALL API
     C     EUFLIC        IFNE      XOFLIC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL1const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOFLIC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EUFLIC
     C*** BEG ADD ****** 07/22/08 ************************************
     C* DRUG CODE IS CHANGED
     C     EUDRG#        IFNE      XODRG#
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL2const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XODRG#        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EUDRG#
     C*** END ADD ****** 07/22/08 ************************************
     C*** BEG ADD ****** 07/29/08 ************************************
     C* DISCHARGE DATE IS CHANGED
     C     EUDCDT        IFNE      XODCDT
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL3const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XODCDT        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EUDCDT
     C*** END ADD ****** 07/29/08 ************************************
     C                   ENDSR
     C*** END ADD ****** 04/30/08 ************************************
