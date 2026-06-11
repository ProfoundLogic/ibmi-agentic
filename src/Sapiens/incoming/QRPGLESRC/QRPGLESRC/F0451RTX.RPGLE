     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  Program: F0451RTX                                            *
     H*  Description: Work With EOMBs Header Info update/display      *
     H*               TEXAS VERSION/PHYSICIAN BILLS                   *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*
     H*  08/11/15 C:079 Project: SY360
     H*                 Load SCICDIND
     H*                 Add error messages for ICD10
     H*
     H*  04/16/15 C:212 Project: MC395
     H*                 ICD10 TX 837 EDI Changes. DSPF also changed.
     H*
     H*  01/10/12 C:251 PROJECT:25MSO
     H*                 Added Referring provider license # processing.
     H*
     H*  10/30/08 C:198 PROJECT:TX048
     H*                 Added Rendering provider license # processing.
     H*
     H*  04/30/08 C:191 PROJECT:TX045
     H*                 Add new fields
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  01/05/03 C:062 SUPPORT: MC025 40498                          *
     H*                 ONLY ALLOW DIAGNOSIS TYPE ICD9 CODES          *
     H*                                                               *
     H*  03/12/03 C:079 SUPPORT: MC044 36319                          *
     H*                 CHECK THE STATUS OF DIAG CODES                *
     H*                                                               *
     H*  02/08/02 C:62 PROJECT TX017                                  *
     H*                ALLOW 'R' TO BE VALID VALUE FOR EDI FLAG.      *
     H*                                                               *
     H*****************************************************************
     FF0451FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEHP     UF A E             DISK
     F                                     RENAME(PEH01:DBRCDF)
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     FFAEAL1    IF   E           K DISK
     F*
     FFMCDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFAESL4    UF   E           K DISK
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
     D OPTNM           C                   CONST('Physician (HCFA) -
     D                                     Header Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D CHGNUM          C                   CONST('SA020R')
     D*** BEG ADD ****** 04/30/08 ************************************
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D NT2             C                   CONST('Billing provider sta-
     D                                     te lic#')
     D NT3             C                   CONST('Rendering provider N-
     D                                     PI# ')
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
     D hldia7          s              7    inz
     D hldia           s              7    inz
     D*** End Add *** 04/16/15 ***************************************
     D*** Beg Add *** 08/11/15 ********************************
     D isLeave         s              1a
     D*** End Add *** 08/11/15 ********************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR FAEHP
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
     C*  Change number parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
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
     C                   KFLD                    EHCO#
     C                   KFLD                    EHSUB#
     C                   KFLD                    EHFND
     C                   KFLD                    EHFYR
     C                   KFLD                    EHCASE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C*** BEGIN ADD *** 01/05/04 **************************************
     C                   KFLD                    XXLOST
     C*** END   ADD *** 01/05/04 **************************************
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
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRCN
     C*
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
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
     C     *LIKE         DEFINE    EHDIA1        XXDIA
     C     *LIKE         DEFINE    EHDIA1        XXDIA1
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C     *LIKE         DEFINE    ESRCN         KKRCN
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C*** Beg Del *** 04/16/15 ***************************************
     C***  *LIKE         DEFINE    EHDIA1        HLDIA
     C*** End Del *** 04/16/15 ***************************************
     C     *LIKE         DEFINE    DILOST        HLLOS
     C     *LIKE         DEFINE    DIDSC1        HLDSC
     C     *LIKE         DEFINE    DIDS1         HLDS
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*** BEG ADD ****** 04/30/08 *************************************
     C     *LIKE         DEFINE    EHFLIC        XOFLIC
     C     *LIKE         DEFINE    EHRNPI        XORNPI
     C*** END ADD ****** 04/30/08 *************************************
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
     C*    a valid code; if 2nd, 3rd, & 4th Diag Codes are filled,
     C*    make sure they are valid.
     C*
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   GOTO      CK1END
     C                   ELSE
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND <> 'Y'
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
     C                   EndIf
     C*** END ADD ****** 04/16/15 ************************************
     C                   ENDIF
     C*
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   IF        SCICDIND <> 'Y'
     C*** END ADD ****** 04/16/15 ************************************
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        7790
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DIDS1         SCDS2
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        7890
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DIDS1         SCDS3
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        7990
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DIDS1         SCDS4
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     SCFLIC        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/30/08 ************************************
     C     SCRNLC        IFEQ      *BLANKS
     C                   SETON                                        6990
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD ****** 10/30/08 ************************************
     C*  Make sure that Data Subm flag is either "Y" or "N" & not
     C*    blank
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ELSE
     C     SCTAPE        IFEQ      'Y'
     C     SCTAPE        OREQ      'N'
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     SCTAPE        OREQ      'R'
     C*** END   ADD *** 02/08/02 **************************************
     C                   SETOFF                                       76
     C                   ELSE
     C                   SETON                                        7690
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
      /Free
           //*** Beg Add *** 08/11/15 ********************************
         isLeave = ' ';
         Dow isLeave <> 'Y';
           //*** End Add *** 08/11/15 ********************************
         If SCICDIND = 'Y';

           //  Diag Codes

           //*** Beg Add *** 08/11/15 ********************************
         If scDia1 <> *blanks;
           //*** End Add *** 08/11/15 ********************************

           kkICD10 = SCDIA1;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCLOS1 = 'D';
                SCDSC1 = ICDSC1;
                SCDS1  = ICFCODE;
           //*** Beg Add *** 08/11/15 ********************************
             Else;
                Eval *in80 = *on;
                Eval *in90 = *on;
                isLeave = 'Y';
                Leave;
           //*** End Add *** 08/11/15 ********************************
             EndIf;
           //*** Beg Add *** 08/11/15 ********************************
         Endif;

         If scDia2 <> *blanks;
           //*** End Add *** 08/11/15 ********************************

           kkICD10 = SCDIA2;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCLOS2 = 'D';
                SCDSC2 = ICDSC1;
                SCDS2  = ICFCODE;
           //*** Beg Add *** 08/11/15 ********************************
             Else;
                Eval *in81 = *on;
                Eval *in90 = *on;
                isLeave = 'Y';
                Leave;
           //*** End Add *** 08/11/15 ********************************
             EndIf;
           //*** Beg Add *** 08/11/15 ********************************
         Endif;

         If scDia3 <> *blanks;
           //*** End Add *** 08/11/15 ********************************

           kkICD10 = SCDIA3;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCLOS3 = 'D';
                SCDSC3 = ICDSC1;
                SCDS3  = ICFCODE;
           //*** Beg Add *** 08/11/15 ********************************
             Else;
                Eval *in82 = *on;
                Eval *in90 = *on;
                isLeave = 'Y';
                Leave;
           //*** End Add *** 08/11/15 ********************************
             EndIf;
           //*** Beg Add *** 08/11/15 ********************************
         Endif;

         If scDia4 <> *blanks;
           //*** End Add *** 08/11/15 ********************************

           kkICD10 = SCDIA4;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCLOS4 = 'D';
                SCDSC4 = ICDSC1;
                SCDS4  = ICFCODE;
           //*** Beg Add *** 08/11/15 ********************************
             Else;
                Eval *in83 = *on;
                Eval *in90 = *on;
                isLeave = 'Y';
                Leave;
           //*** End Add *** 08/11/15 ********************************
             EndIf;
           //*** Beg Add *** 08/11/15 ********************************
         Endif;
           //*** End Add *** 08/11/15 ********************************
E01      EndIf;
           //*** Beg Add *** 08/11/15 ********************************
         Leave;
         Enddo;
           //*** End Add *** 08/11/15 ********************************

      /End-Free
     C*** END ADD ****** 04/16/15 ************************************
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

     C                   MOVE      *BLANKS       XXICD9
     C*** BEGIN DEL *** 01/05/04 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C*** END   DEL *** 01/05/04 **************************************
     C                   MOVEL     SCDIA1        HLDIA
     C                   MOVEL     SCLOS1        HLLOS
     C                   MOVEL     SCDSC1        HLDSC
     C                   MOVEL     SCDS1         HLDS
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #1 SEARCH
      /Free
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
             *in35 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in36 = *on;
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
      /End-Free
     C                   ELSE
     C*** END ADD ****** 04/16/15 ************************************
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA1
     C                   MOVEL     XXLOST        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C                   SETON                                        36
     C                   ELSE
     C                   SETON                                        35
     C                   MOVEL     HLDIA         SCDIA1
     C                   MOVEL     HLLOS         SCLOS1
     C                   MOVEL     HLDSC         SCDSC1
     C                   MOVEL     HLDS          SCDS1
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVE      *BLANKS       XXICD9
     C*** BEGIN DEL *** 01/05/04 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C*** END   DEL *** 01/05/04 **************************************
     C                   MOVEL     SCDIA2        HLDIA
     C                   MOVEL     SCLOS2        HLLOS
     C                   MOVEL     SCDSC2        HLDSC
     C                   MOVEL     SCDS2         HLDS
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
      /Free
          HLDIA7 = SCDIA2;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa2 = HLDIA7;
             *in90 = *on;
             *in36 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in37 = *on;
          Endif;

          If scDia2 <> *blanks;
            kkICD10 = scDIa2;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS1 = 'D';
              SCDSC1 = ICDSC1;
              SCDS1  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 04/16/15 ************************************
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA2
     C                   MOVEL     XXLOST        SCLOS2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC2
     C  N60              MOVEL     DIDS1         SCDS2
     C                   SETON                                        37
     C                   ELSE
     C                   SETON                                        36
     C                   MOVEL     HLDIA         SCDIA2
     C                   MOVEL     HLLOS         SCLOS2
     C                   MOVEL     HLDSC         SCDSC2
     C                   MOVEL     HLDS          SCDS2
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVE      *BLANKS       XXICD9
     C*** BEGIN DEL *** 01/05/04 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C*** END   DEL *** 01/05/04 **************************************
     C                   MOVEL     SCDIA3        HLDIA
     C                   MOVEL     SCLOS3        HLLOS
     C                   MOVEL     SCDSC3        HLDSC
     C                   MOVEL     SCDS3         HLDS
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #3 SEARCH
     C*
      /Free
          HLDIA7 = SCDIA3;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa3 = HLDIA7;
             *in90 = *on;
             *in37 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in38 = *on;
          Endif;

          If scDia3 <> *blanks;
            kkICD10 = scDIa3;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS1 = 'D';
              SCDSC1 = ICDSC1;
              SCDS1  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 04/16/15 ************************************
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA3
     C                   MOVEL     XXLOST        SCLOS3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C  N60              MOVEL     DIDS1         SCDS3
     C                   SETON                                        38
     C                   ELSE
     C                   SETON                                        37
     C                   MOVEL     HLDIA         SCDIA3
     C                   MOVEL     HLLOS         SCLOS3
     C                   MOVEL     HLDSC         SCDSC3
     C                   MOVEL     HLDS          SCDS3
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVE      *BLANKS       XXICD9
     C*** BEGIN DEL *** 01/05/04 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C*** END   DEL *** 01/05/04 **************************************
     C                   MOVEL     SCDIA4        HLDIA
     C                   MOVEL     SCLOS4        HLLOS
     C                   MOVEL     SCDSC4        HLDSC
     C                   MOVEL     SCDS4         HLDS
     C*** BEG ADD ****** 04/16/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #4 SEARCH
     C*
      /Free
          HLDIA7 = SCDIA4;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa4 = HLDIA7;
             *in90 = *on;
             *in38 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in39 = *on;
          Endif;

          If scDia4 <> *blanks;
            kkICD10 = scDIa4;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS1 = 'D';
              SCDSC1 = ICDSC1;
              SCDS1  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 04/16/15 ************************************
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA4
     C                   MOVEL     XXLOST        SCLOS4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC4
     C  N60              MOVEL     DIDS1         SCDS4
     C                   SETON                                        39
     C                   ELSE
     C                   SETON                                        38
     C                   MOVEL     HLDIA         SCDIA4
     C                   MOVEL     HLLOS         SCLOS4
     C                   MOVEL     HLDSC         SCDSC4
     C                   MOVEL     HLDS          SCDS4
     C                   ENDIF
     C*** BEG ADD ****** 04/16/15 ************************************
B01  C                   ENDIF
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCTAPE'
     C                   SETON                                        9068
     C                   SETOFF                                       6967
     C*** BEG ADD ****** 04/30/08 ************************************
     C     HFLD          WHENEQ    'SCFLIC'
     C                   SETON                                        9073
     C     HFLD          WHENEQ    'SCRNPI'
     C                   SETON                                        9074
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/30/08 ************************************
     C     HFLD          WHENEQ    'SCRNLC'
     C                   SETON                                        9067
     C*** END ADD ****** 10/30/08 ************************************
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EHCO#         KKCO#
     C                   MOVEL     EHSUB#        KKSUB#
     C                   MOVEL     EHFND         KKFND
     C                   MOVEL     EHFYR         KKFYR
     C                   MOVEL     EHCASE        KKCASE
     C*
     C                   MOVEL(P)  EHSUB#        XXSUB#            5
     C                   MOVEL(P)  EHFND         XXFND             3
     C                   MOVEL(P)  EHFYR         XXFYR             3
     C                   MOVEL(P)  EHCASE        XXCASE            7
     C*
     C     CAKY          CHAIN     PCA01                              60
     C                   MOVEL     CASS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVEL     EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C                   MOVE      EHEDI         SCTAPE
     C*
     C     EHDIA1        IFNE      *BLANKS
     C                   MOVEL     EHDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     EHDIA1        SCDIA1
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCDS1
     C                   MOVE      *BLANKS       SCDIA1
     C                   ENDIF
     C*
     C     EHDIA2        IFNE      *BLANKS
     C                   MOVEL     EHDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDS1         SCDS2
     C                   MOVEL     EHDIA2        SCDIA2
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCLOS2
     C                   MOVE      *BLANKS       SCDS2
     C                   MOVE      *BLANKS       SCDIA2
     C                   ENDIF
     C*
     C     EHDIA3        IFNE      *BLANKS
     C                   MOVEL     EHDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDS1         SCDS3
     C                   MOVEL     EHDIA3        SCDIA3
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC3
     C                   MOVE      *BLANKS       SCLOS3
     C                   MOVE      *BLANKS       SCDS3
     C                   MOVE      *BLANKS       SCDIA3
     C                   ENDIF
     C*
     C     EHDIA4        IFNE      *BLANKS
     C                   MOVEL     EHDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDS1         SCDS4
     C                   MOVEL     EHDIA4        SCDIA4
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC4
     C                   MOVE      *BLANKS       SCLOS4
     C                   MOVE      *BLANKS       SCDS4
     C                   MOVE      *BLANKS       SCDIA4
     C                   ENDIF
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   MOVEL     EHFLIC        SCFLIC
     C                   MOVEL     EHRNPI        SCRNPI
     C* STORE ORIGINAL VALUES
     C                   MOVEL     EHFLIC        XOFLIC
     C                   MOVEL     EHRNPI        XORNPI
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/30/08 ************************************
     C                   MOVEL     EHRNLC        SCRNLC
     C*** END ADD ****** 10/30/08 ************************************
     C*** BEG ADD *** 01/10/12 ***************************************
     C                   MOVEL     EHRFLC        SCRFLC
     C*** END ADD *** 01/10/12 ***************************************
     C*** BEG ADD ****** 04/16/15 ************************************
      /Free
       //*** Beg Add *** 08/11/15 ***********************************
         scICDInd = EHicdInd;
       //*** End Add *** 08/11/15 ***********************************
         If SCICDIND = 'Y';

           SCDIA1 = EHICD1001;
           kkICD10 = SCDIA1;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSC1 = ICDSC1;
             SCDS1 = ICFCODE;
             SCLOS1 = 'D';
           EndIf;

           SCDIA2 = EHICD1002;
           kkICD10 = SCDIA2;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCDSC2 = ICDSC1;
                SCDS2 = ICFCODE;
                SCLOS2 = 'D';
             EndIf;

           SCDIA3 = EHICD1003;
           kkICD10 = SCDIA3;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCDSC3 = ICDSC1;
                SCDS3 = ICFCODE;
                SCLOS3 = 'D';
             EndIf;

           SCDIA4 = EHICD1004;
           kkICD10 = SCDIA4;

           Chain (kkICD10:'D') FMICD10L1;
             If %Found(FMICD10L1);
                SCDSC4 = ICDSC1;
                SCDS4 = ICFCODE;
                SCLOS4 = 'D';
             EndIf;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCDIA1        EHDIA1
     C                   MOVEL     SCDIA2        EHDIA2
     C                   MOVEL     SCDIA3        EHDIA3
     C                   MOVEL     SCDIA4        EHDIA4

     C*** BEG ADD ****** 04/16/15 ************************************
      /Free
       //*** Beg Add *** 08/11/15 ***********************************
          EHicdInd = scICDInd;
       //*** End Add *** 08/11/15 ***********************************
        If  SCICDIND = 'Y';

          EHICD1001 = SCDIA1;
          EHICD1002 = SCDIA2;
          EHICD1003 = SCDIA3;
          EHICD1004 = SCDIA4;

E01     EndIf;
      /End-Free
     C*** END ADD ****** 04/16/15 ************************************
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   MOVEL     SCFLIC        EHFLIC
     C                   MOVEL     SCRNPI        EHRNPI
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/30/08 ************************************
     C                   MOVEL     SCRNLC        EHRNLC
     C*** END ADD ****** 10/30/08 ************************************
     C*** BEG ADD *** 01/10/12 ***************************************
     C                   MOVEL     SCRFLC        EHRFLC
     C*** END ADD *** 01/10/12 ***************************************
     C                   MOVEL     SCTAPE        EHEDI
     C                   MOVEL     SCTAPE        EOTAPE
     C     EAKEY         CHAIN     PEA01                              60
     C                   MOVEL     SCTAPE        EAEDI
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
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
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C*
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
     C*** BEGIN ADD *** 01/05/04 **************************************
     C                   MOVEL(P)  'D'           XXLOST
     C*** END   ADD *** 01/05/04 **************************************
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
     C     ESKY          SETGT     PES01
     C     ESKY1         READPE    PES01                                  59
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   UPDATE    PEO01
     C  N59              UPDATE    PES01
     C*** BEG ADD ****** 04/30/08 ************************************
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
     C                   FEOD      FAEHP
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
     C                   ENDSR
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF EITHER OF THE FIELDS IS CHANGED THEN CALL API
     C     EHFLIC        IFNE      XOFLIC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  NT2           XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOFLIC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EHFLIC
     C*
     C     EHRNPI        IFNE      XORNPI
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  NT3           XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XORNPI        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EHRNPI
     C                   ENDSR
     C*** END ADD ****** 04/30/08 ************************************
