     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr***dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*  Program: F0451RFL                                            *
     H*  Description: Work With EOMBs Header Info update/display      *
     H*               FLORIDA VERSION/PHYSICIAN BILLS                 *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  09/01/16  C:212 FL091 Phase 2                                *
     H*                  Renamed Dia1/2/3/4 to A/B/C/D                *
     H*                  Added Dia E-L and related changes            *
     H*
     H*   08/13/15 C:212 Project FL092                                *
     H*                  FL ICD10 Medical EDI changes.DSPF changed too*
     H*                                                              *
     H*   05/27/15 C:306 SUPPORT 34971                                *
     H*                  Removed the F2 button from F0451FFL          *
     H*                  screen action panel. (Recompile only).       *
     H*                                                              *
     H*   05/09/11 C:257 SUPPORT 76516
     H*                  Remove all edits on License # except that
     H*                  license cannot be blank.
     H*                         
     H*   10/04/08 C:180 SUPPORT 65159
     H*                  Fourth position of FL Phy lic can be blank
     H*
     H*  09/10/07 C:180   SUPPORT: 58610                              *
     H*                   Allow editing of zip code.
     H*                         
     H*  08/28/07  C:180  SUPP 59201
     H*                   Validate SCLIC# based on following formats:
     H*
     H*                   AANNNNNNNNNNN
     H*                   (OR)
     H*                   AAANNNNNNNNNN
     H*                   (OR)
     H*                   AAAANNNNNNNNN
     H*
     H*                   Left justify and space fill to end of field
     H*                   Key alpha prefix and numeric digits of
     H*                   license number
     H*                   Must be valid value
     H*                   DO NOT zero pad numeric portion
     H*
     H*  04/23/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*                         
     H* 03/14/07  C:182  Project: FL067
     H*                  1. Changed Fld Heading for 'Payment plan'
     H*                     to be 'Payment code'.
     H*                         
     H*
     H*  02/23/06 C:163 SUPPORT:MC010 51595
     H*                 FL ALLOW FUTURE DATE OF SERVICE
     H*
     H*  11/08/05 C:163 PROJECT: 48MWK
     H*                 FL EDI CORRECTION LICENSE #
     H*
     H*  04/15/04 C:079 PROJECT: FL046
     H*                 1. USE GENERIC TABLE FOR SCTAPE
     H*                 2. CHANGE EDIT ON PAYMENT PLAN
     H*
     H*  10/09/03 C:062 PROJECT: FL045
     H*                 CORRECT THE ASSIGNMENT OF LICENSE#
     H*
     H*  04/28/03 C:079 PROJECT: FL043
     H*                 EXPAND LIC# FIELD TO 13A
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*                                                               *
     H*  11/10/02  C:079  PROJECT:FS248                               *
     H*                   CHANGE CURSOR POS INDICATORS                *
     H*                                                               *
     H*  12/09/01  C:079  PROJECT:FL040                               *
     H*                   ALLOW 4TH POS OF LIC# TO BE ALPHANUM        *
     H*                                                               *
     H*  09/18/01  C:044  SUPPORT:MC044 28917                         *
     H*                   CHANGE EDITING ON SCLIC# TO ALLOW ALPHANUM  *
     H*                   IN POSITION 3                               *
     H*                                                               *
     H*  09/20/00  C:117  PROJECT:FL031                               *
     H*                   CHANGEING THE EDITTING FOR SCFLIC;          *
     H*                   ADDING SEARCH TO SAME FIELD.                *
     H*                                                               *
     H*  04/11/00  C:117  PROJECT:FS211                               *
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA    *
     H*                   SUBMISSION                                  *
     H*                                                               *
     H*  04/02/00 C:117 Project: MCMNT                                *
     H*                 FIXED SCREEN HEADINGS                         *
     H*                                                               *
     H*  10/21/99 C:117 Project: FLMNT                                *
     H*                 Sup Seq: 20175                                *
     H*                 Added error checking for last 7 digits of FL  *
     H*                 Lic#; they must all be numeric.               *
     H*                                                               *
     H*  09/27/99  C:117 PROJ: MC152                                  *
     H*                  Making corrections to the editing of FL      *
     H*                  Payment Plan field                           *
     H*                                                               *
     H*****************************************************************
     FF0451FFL  CF   E             WORKSTN
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
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEG ADD *** 04/11/00 ****************************************
     FFAESL4    UF   E           K DISK
     F*** END ADD *** 04/11/00 ****************************************
     F*** BEG ADD *** 09/20/00 ****************************************
     F*** BEG DEL *** 05/09/11 ****************************************
     F*******FMPLLFL IF  E           K        DISK
     F*** END DEL *** 05/09/11 ****************************************
     FSTSTL1    IF   E           K DISK
     F*** END ADD *** 09/20/00 ****************************************
     F*** Beg Add *** 08/13/15 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/13/15 ***************************************
      /COPY SCOPYMT01D
     D****¢¢¢ BEGIN DELETE 08/28/07 ***********************************
     D****¢¢¢             LT     26  26  1               ALPHABET
     D****¢¢¢ END   DELETE 08/28/07 ***********************************
     D*** BEG ADD *** 08/28/07 ****************************************
     D*** BEG DEL *** 05/09/11 ****************************************
     D******              @A     26  26  1
     D******              @N     10  10  1
     D*** END DEL *** 05/09/11 ****************************************
     D LL              S              1    DIM(11)
     D*** END ADD *** 08/28/07 ****************************************
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
     D*** BEG DEL *** 12/09/01 ****************************************
     D************************************
     D****Data*structure*of*FL*License*#*to make sure first two
     D******characters*of*license*#*are*alpha
     D************DS**********************
     D************************************    1   9 SCLIC#
     D************************************    1   1 SCLIC1
     D************************************    2   2 SCLIC2
     D****BEG*ADD*****10/21/99*****************************************
     D****BEG*CHG*****09/18/01*****************************************
     D*************************************** 3   9 SCLIC3
     D************************************    3   3 SCLIC3
     D************************************    4   9 SCLIC4
     D*** END CHG *** 09/18/01 ****************************************
     D*** END ADD *** 10/21/99 ****************************************
     D*** END DEL *** 12/09/01 ****************************************
     D*** BEG ADD *** 12/09/01 ****************************************
     D*
     D*   Data structure of FL License # to make sure first two
     D*     characters of license # are alpha
     D                 DS
     D  SCLIC#                 1     13
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D*** BEG DEL *** 04/28/03 ****************************************
     D*************************************** 5   9 SCLIC5
     D*** END DEL *** 04/28/03 ****************************************
     D*** BEG ADD *** 04/28/03 ****************************************
     D*** BEG DEL *** 08/28/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*****¢¢¢¢                               5  11 SCLIC5
     D*****¢¢¢¢                               4  10 SCLIC6
     D*****¢¢¢¢                               3   9 SCLIC7
     D*** END DEL *** 08/28/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*** END ADD *** 04/28/03 ****************************************
     D*** END ADD *** 12/09/01 ****************************************
     D*** BEG ADD *** 08/28/07 ****************************************
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*** END ADD *** 08/28/07 ****************************************
     D******** BEGIN ADD **********  09/10/07 *************************
     D*   Data structure
     D                 DS
     D  EOFLIC                 1      9
     D  EOFLI5                 1      5

     I******** END ADD ************  09/10/07 *************************
     D*** Beg Add *** 08/13/15 ***************************************
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
     D*** End Add *** 08/13/15 ***************************************

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
     C*  KEY LIST FOR ERRCK FOR FL PAYMENT PLAN
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** BEG ADD *** 04/11/00 ****************************************
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
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 09/20/00 ****************************************
     C*
     C* FL Provider Lic#
     C*
     C     PLKY          KLIST
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** KFLD           KKLIC#  9
     C                   KFLD                    KKLIC#           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C* System-wide State Control File
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*** END ADD *** 09/20/00 ****************************************
     C*** Beg Add *** 08/13/15 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kkind
     C*** End Add *** 08/13/15 ***************************************
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
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** BEG ADD *** 04/11/00 ****************************************
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C     *LIKE         DEFINE    ESRCN         KKRCN
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
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
     C***** Beg Add *** 09/01/16 ****************************************
     C** initialize indicators
     C                   eval      erDiaE = *off
     C                   eval      erDiaF = *off
     C                   eval      erDiaG = *off
     C                   eval      erDiaH = *off
     C                   eval      erDiaI = *off
     C                   eval      erDiaJ = *off
     C                   eval      erDiaK = *off
     C                   eval      erDiaL = *off
     C                   eval      MsDiaE = *blanks
     C                   eval      MsDiaF = *blanks
     C                   eval      MsDiaG = *blanks
     C                   eval      MsDiaH = *blanks
     C                   eval      MsDiaI = *blanks
     C                   eval      MsDiaJ = *blanks
     C                   eval      MsDiaK = *blanks
     C                   eval      MsDiaL = *blanks
     C***** End Add *** 09/01/16 ****************************************
     C*
     C*  Make sure 1st Diag Code is populated & make sure that it is
     C*    a valid code; if 2nd, 3rd, & 4th Diag Codes are filled,
     C*    make sure they are valid.
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   GOTO      CK1END
     C                   ELSE
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
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
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   IF        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
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
     C*
     C***** Beg Add *** 09/01/16 ****************************************
     C*
     C     SCDIAE        IFNE      *BLANKS
     C                   MOVEL     SCDIAE        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaE = *on
     C                   Eval      MsDiaE = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSE
     C                   MOVEL     DIDSC1        SCDSCE
     C                   MOVEL     DIDS1         SCDSE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAF        IFNE      *BLANKS
     C                   MOVEL     SCDIAF        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaF = *on
     C                   Eval      MsDiaF = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSF
     C                   MOVEL     DIDSC1        SCDSCF
     C                   MOVEL     DIDS1         SCDSF
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAG        IFNE      *BLANKS
     C                   MOVEL     SCDIAG        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaG = *on
     C                   Eval      MsDiaG = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSG
     C                   MOVEL     DIDSC1        SCDSCG
     C                   MOVEL     DIDS1         SCDSG
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAH        IFNE      *BLANKS
     C                   MOVEL     SCDIAH        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaH = *on
     C                   Eval      MsDiaH = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSH
     C                   MOVEL     DIDSC1        SCDSCH
     C                   MOVEL     DIDS1         SCDSH
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAI        IFNE      *BLANKS
     C                   MOVEL     SCDIAI        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaI = *on
     C                   Eval      MsDiaI = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSI
     C                   MOVEL     DIDSC1        SCDSCI
     C                   MOVEL     DIDS1         SCDSI
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAJ        IFNE      *BLANKS
     C                   MOVEL     SCDIAJ        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaJ = *on
     C                   Eval      MsDiaJ = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSJ
     C                   MOVEL     DIDSC1        SCDSCJ
     C                   MOVEL     DIDS1         SCDSJ
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAK        IFNE      *BLANKS
     C                   MOVEL     SCDIAK        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaK = *on
     C                   Eval      MsDiaK = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSK
     C                   MOVEL     DIDSC1        SCDSCK
     C                   MOVEL     DIDS1         SCDSK
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIAL        IFNE      *BLANKS
     C                   MOVEL     SCDIAL        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                          90
     C                   Eval      erDiaL = *on
     C                   Eval      MsDiaL = 'MCR0742'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOSL
     C                   MOVEL     DIDSC1        SCDSCL
     C                   MOVEL     DIDS1         SCDSL
     C                   ENDIF
     C                   ENDIF
     C***** End Add *** 09/01/16 ****************************************
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C*  Make sure Liscense# isn't blank
     C*
     C     SCFLIC        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ENDIF
     C****** BEG DEL ******* 05/09/11 ********************************
     C*
     C*** BEG CHG *** 09/20/00 ****************************************
     C*  Check to see if the Provider DB installed
     C*
     C******               MOVEL'FL'      KKST
     C******     STKY      CHAINPST01                99
     C******     *IN99     IFEQ *OFF
     C******     STBS01    ANDEQ'Y'
     C******     SCTAPE    ANDEQ'Y'
     C*
     C* edit against the Prov Lic# database
     C*
     C******               MOVELSCFLIC    KKLIC#
     C******     PLKY      CHAINPPL01                99
     C******     *IN99     IFEQ *ON
     C******               SETON                     9064
     C******               ENDIF                           *IN99
     C*
     C******               ELSE                            STBS01=Y
     C****** END DEL ******* 05/09/11 ********************************
     C*
     C     SCFLIC        IFNE      *BLANKS
     C                   MOVEL     SCFLIC        SCLIC#
     C****** BEG ADD ******* 05/09/11 ********************************
     C                   ENDIF                                                  SCFLIC
     C****** END ADD ******* 05/09/11 ********************************
      ******¢¢¢¢ BEGIN DELETE ******* 08/28/07 ************************
     C***¢¢¢               Z-ADD1         X       20
     C***¢¢¢     SCLIC1    LOKUPLT,X                     60
     C***¢¢¢     *IN60     IFEQ '0'
     C***¢¢¢               MOVE '1'       *IN66
     C***¢¢¢               SETON                     90
     C***¢¢¢               GOTO CK1END
     C***¢¢¢               END
     C***¢¢¢
     C***¢¢¢               Z-ADD1         X
     C***¢¢¢     SCLIC2    LOKUPLT,X                     60
     C***¢¢¢     *IN60     IFEQ '0'
     C***¢¢¢               MOVE '1'       *IN66
     C***¢¢¢               SETON                     90
     C***¢¢¢               GOTO CK1END
     C***¢¢¢               END
      ****¢¢¢
      ******¢¢¢¢ END   DELETE ******* 08/28/07 ************************
      * MAYBE INSERT NEW CODE HERE
     C****** BEG DEL ******* 05/09/11 ********************************
     C****** BEG ADD ******* 08/28/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C******               MOVE *ZEROS    LCNT    50
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C******     SCLIC1    LOKUP@A                       60
     C******     *IN60     IFEQ *OFF
     C******               SETON                     9066
     C******               GOTO CK1END
     C******               ENDIF
     C*
     C******               ADD  1         LCNT
      *
     C******     SCLIC2    LOKUP@A                       60
     C******     *IN60     IFEQ *OFF
     C******               SETON                     9066
     C******               GOTO CK1END
     C******               ENDIF
     C*
     C******               ADD  1         LCNT    50
      *
     C* 3RD Position can be alpha or numeric
      *
     C******     SCLIC3    LOKUP@A                       60
     C******     *IN60     IFEQ *ON
     C******               ADD  1         LCNT    50
     C******               ELSE
     C******     SCLIC3    LOKUP@N                       40
     C******     *IN40     IFEQ *OFF
     C******               SETON                     9064
     C******               GOTO CK1END
     C******               ENDIF
     C******               ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C*******    SCLIC4    IFGT *BLANKS
     C***** END ADD *******  10/04/08 *********************************
      *
     C******     SCLIC4    LOKUP@A                       60
     C******     *IN60     IFEQ *ON
      *
     C******     LCNT      IFEQ 3
     C******               ADD  1         LCNT    50
     C******               ELSE
     C******               SETON                     9064
     C******               GOTO CK1END
     C******               ENDIF
      *
     C******               ELSE
     C******     SCLIC4    LOKUP@N                       40
     C******     *IN40     IFEQ *OFF
     C******               SETON                     9064
     C******               GOTO CK1END
     C******               ENDIF
     C******               ENDIF
     C***** BEGIN ADD *****  10/04/08 *********************************
     C******               ENDIF
     C***** END ADD *******  10/04/08 *********************************
      *
     C******               CLEARLL
      *
     C******               SELEC
     C******     LCNT      WHEQ 2
     C******               MOVEASCLICA    LL
     C******     LCNT      WHEQ 3
     C******               MOVEASCLICB    LL
     C******     LCNT      WHEQ 4
     C******               MOVEASCLICC    LL
     C******               ENDSL
      ******
     C******     13        SUB  LCNT      MAX     30
     C******               Z-ADDMAX       IX      30
      * Find 1st non-blank
     C******               DO   MAX
     C******     LL,IX     IFNE *BLANKS
     C******               LEAVE
     C******               ENDIF
     C******               SUB  1         IX
     C******               ENDDO
      * Check numeric part
      *
      *
     C******     LL,1      IFGT *ZERO
      *
     C******               DO   IX        IY      30
     C******     LL,IY     LOKUP@N                       60
     C******     *IN60     IFEQ *OFF
     C******               SETON                     9064
     C******               LEAVE
     C******               ENDIF
     C******               ENDDO
      *
     C******               ELSE
     C******               SETON                     9064
     C******               ENDIF
      *
     C******     *IN90     IFEQ *ON
     C******               GOTO CK1END
     C******               ENDIF
     C****** END ADD ******* 08/28/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C******               END                             STBS01=Y
     C***** END DEL *******  05/09/11 *********************************
     C*** END CHG *** 09/20/00 ****************************************
     C*** BEG ADD *** 09/18/01 ****************************************
     C* 3rd position can be alpha or numeric
     C******¢¢¢¢¢  BEG DELETE 08/28/07  *******************************
     C******¢¢¢¢           TESTN          SCLIC3     60
     C******¢¢¢¢
     C*****¢¢¢¢  *IN60     IFEQ '0'
     C*****¢¢¢¢
     C****¢¢¢    SCLIC3    IFLT 'A'
     C****¢¢¢    SCLIC3    ORGT 'Z'
     C****¢¢¢
     C****¢¢¢              SETON                     9087
     C****¢¢¢              GOTO CK1END
     C****¢¢¢
     C****¢¢¢              ENDIF                           SCLIC2 IFLT
     C****¢¢¢
     C*****¢¢¢             ENDIF                           *IN60 = 0
     C*** END ADD *** 09/18/01 ****************************************
     C*** BEG ADD *** 09/18/01 ****************************************
     C* 4TH position can be alpha or numeric
     C****¢¢¢¢             TESTN          SCLIC4     60
     C****¢¢¢¢
     C****¢¢¢¢   *IN60     IFEQ '0'
     C****¢¢¢
     C****¢¢¢    SCLIC4    IFLT 'A'
     C****¢¢¢    SCLIC4    ORGT 'Z'
     C****¢¢¢
     C****¢¢¢              SETON                     9086
     C****¢¢¢              GOTO CK1END
     C****¢¢¢
     C****¢¢¢              ENDIF                           SCLIC2 IFLT
     C****¢¢¢
     C****¢¢¢              ENDIF                           *IN60 = 0
     C*** END ADD *** 09/18/01 ****************************************
     C*
     C*** BEG ADD *** 10/21/99 ****************************************
     C*** BEG DEL *** 12/09/01 ****************************************
     C**  The last 7 digits MUST BE numeric
     C*
     C*** BEG DEL *** 12/09/01 ****************************************
     C*** BEG CHG *** 09/18/01 ****************************************
     C******************** TESTN          SCLIC3     60
     C******************** TESTN          SCLIC4     60
     C*** END CHG *** 09/18/01 ****************************************
     C*** END DEL *** 12/09/01 ****************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C****¢¢¢              SELEC
     C****¢¢¢    SCLIC3    WHGE 'A'
     C****¢¢¢    SCLIC3    ANDLE'Z'
     C****¢¢¢    SCLIC4    ANDGE'A'
     C****¢¢¢    SCLIC4    ANDLE'Z'
     C*** END ADD *** 04/28/03 ****************************************
     C*** BEG ADD *** 12/09/01 ****************************************
     C****¢¢¢              TESTN          SCLIC5     60
     C*** END ADD *** 12/09/01 ****************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C****¢¢¢    SCLIC3    WHGE 'A'
     C****¢¢¢    SCLIC3    ANDLE'Z'
     C****¢¢¢    SCLIC4    ANDGE'0'
     C****¢¢¢    SCLIC4    ANDLE'9'
     C****¢¢¢              TESTN          SCLIC6     60
     C****¢¢¢
     C****¢¢¢    SCLIC3    WHGE '0'
     C****¢¢¢    SCLIC3    ANDLE'9'
     C****¢¢¢    SCLIC4    ANDGE'0'
     C****¢¢¢    SCLIC4    ANDLE'9'
     C****¢¢¢              TESTN          SCLIC7     60
     C****¢¢¢
     C****¢¢¢    SCLIC3    WHGE '0'
     C****¢¢¢    SCLIC3    ANDLE'9'
     C****¢¢¢    SCLIC4    ANDGE'A'
     C****¢¢¢    SCLIC4    ANDLE'Z'
     C****¢¢¢              TESTN          SCLIC5     60
     C****¢¢¢
     C****¢¢¢              OTHER
     C****¢¢¢              TESTN          SCLIC5     60
     C****¢¢¢
     C****¢¢¢              ENDSL
     C*** END ADD *** 04/28/03 ****************************************
     C****¢¢¢
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              MOVE '1'       *IN65
     C****¢¢¢              SETON                     90
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C*** END ADD *** 10/21/99 ****************************************
     C****¢¢¢ END DELETE ****************** 08/28/07 ******************
     C*
     C******               END                             LIC#<>*BLANK
     C*
     C*  Make sure FL Payment Plan isn't blank & that it is valid
     C*   (It may be blank per FL Tape Subm chgs)
     C*** BEG DEL *** 09/27/99 ****************************************
     C*********  SCPMNT    IFEQ *BLANKS
     C*********            SETON                     7390
     C*********            ELSE
     C*********            SETOF                     73
     C*********            ENDIF
     C*** END DEL *** 09/27/99 ****************************************
     C*** BEG DEL *** 04/15/04 ****************************************
     C********** SCPMNT    IFNE *BLANKS
     C**********           MOVEL'FLPP'    KKTBID    P
     C**********           MOVELSCPMNT    KKKEY     P
     C********** GTKY      CHAINPGT01                99
     C********** *IN99     IFEQ *ON
     C**********           SETON                     9074
     C**********           GOTO CK1END
     C**********           ENDIF                           *IN99 IFEQ *ON
     C**********           ENDIF                                       ON
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* VALIDATE PAYMENT PLAN
     C*
     C     SCPMNT        IFEQ      *BLANKS
     C     SCTAPE        ANDNE     'Y'
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCTAPE        IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPMNT        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*  Make sure that Data Subm flag is either "Y" or "N" & not
     C*    blank
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ELSE
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7690
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG DEL *** 04/15/04 ****************************************
     C**********
     C********** SCTAPE    IFEQ 'Y'
     C********** SCTAPE    OREQ 'N'
     C**********           SETOF                     76
     C**********           ELSE
     C**********           SETON                     7690
     C**********           GOTO CK1END
     C**********           ENDIF
     C**********           ENDIF
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C*  Carr rcvd date added to screen; required
     C*
     C     SCRCVD        IFEQ      *ZEROS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCRDT EQ ZEROS
     C*
     C*  Date must be valid
     C                   Z-ADD     SCRCVD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCRCVD
     C                   ENDIF                                                  SRERR EQ 1
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRCVD
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLACDT
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C*  Date cannot be less than DOI
     C     HLRCVD        IFNE      *ZEROS
     C     HLRCVD        ANDLT     HLACDT
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLCRDT
     C*
     C*  Date cannot be greater than system date
     C     HLRCVD        IFNE      *ZEROS
     C     HLRCVD        ANDGT     HLSYDT
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLCRDT NE *ZEROS
     C*
     C*  Date cannot be less than To Service Date
     C     HLRCVD        IFLT      EOTODT
     C*** BEG ADD *** 02/23/06 ****************************************
     C     ##SDTS        ANDNE     'Y'
     C*** END ADD *** 02/23/06 ****************************************
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLRCVD<TODT EROS
     C*
     C*  Date cannot be less than Date Paid
     C     HLRCVD        IFGT      EOWCCK
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLRCVD<WCCK EROS
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
         If SCICDIND = 'Y';

           //  Diag Codes

           If SCDIA1 <> *blanks;
              kkICD10 = SCDIA1;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS1 = 'D';
                   SCDSC1 = ICDSC1;
                   SCDS1  = ICFCODE;
                Else;
                   *in71 = *on;
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIA2 <> *blanks;
              kkICD10 = SCDIA2;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS2 = 'D';
                   SCDSC2 = ICDSC1;
                   SCDS2  = ICFCODE;
                Else;
                   *in77 = *on;
                   *in90 = *on;
                EndIf;
           EndIf;

           If SCDIA3 <> *blanks;
              kkICD10 = SCDIA3;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS3 = 'D';
                   SCDSC3 = ICDSC1;
                   SCDS3  = ICFCODE;
                Else;
                   *in78 = *on;
                   *in90 = *on;
                EndIf;
           EndIf;

           If SCDIA4 <> *blanks;
              kkICD10 = SCDIA4;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS4 = 'D';
                   SCDSC4 = ICDSC1;
                   SCDS4  = ICFCODE;
                Else;
                   *in79 = *on;
                   *in90 = *on;
                EndIf;
           EndIf;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
     C*** Beg Add ****** 09/01/16 ************************************
      /Free

           If SCDIAE <> *blanks;
              kkICD10 = SCDIAE;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSE = 'D';
                   SCDSCE = ICDSC1;
                   SCDSE  = ICFCODE;
                Else;
                   erDiaE = *on;
                   MsDiaE = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAF <> *blanks;
              kkICD10 = SCDIAF;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSF = 'D';
                   SCDSCF = ICDSC1;
                   SCDSF  = ICFCODE;
                Else;
                   erDiaF = *on;
                   MsDiaF = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAG <> *blanks;
              kkICD10 = SCDIAG;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSG = 'D';
                   SCDSCG = ICDSC1;
                   SCDSG  = ICFCODE;
                Else;
                   erDiaG = *on;
                   MsDiaG = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAH <> *blanks;
              kkICD10 = SCDIAH;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSH = 'D';
                   SCDSCH = ICDSC1;
                   SCDSH  = ICFCODE;
                Else;
                   erDiaH = *on;
                   MsDiaH = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAI <> *blanks;
              kkICD10 = SCDIAI;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSI = 'D';
                   SCDSCI = ICDSC1;
                   SCDSI  = ICFCODE;
                Else;
                   erDiaI = *on;
                   MsDiaI = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAJ <> *blanks;
              kkICD10 = SCDIAJ;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSJ = 'D';
                   SCDSCJ = ICDSC1;
                   SCDSJ  = ICFCODE;
                Else;
                   erDiaJ = *on;
                   MsDiaJ = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAK <> *blanks;
              kkICD10 = SCDIAK;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSK = 'D';
                   SCDSCK = ICDSC1;
                   SCDSK  = ICFCODE;
                Else;
                   erDiaK = *on;
                   MsDiaK = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

           If SCDIAL <> *blanks;
              kkICD10 = SCDIAL;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOSL = 'D';
                   SCDSCL = ICDSC1;
                   SCDSL  = ICFCODE;
                Else;
                   erDiaL = *on;
                   MsDiaL = 'MCR0742';
                   *in90 = *on;
                EndIf;
           Endif;

      /End-Free
     C*** End Add ****** 09/01/16 ************************************
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
     C***** Beg Add *** 09/01/16 ****************************************
     C** initialize indicators
     C                   eval      indFoDiaE = *off
     C                   eval      indFoDiaF = *off
     C                   eval      indFoDiaG = *off
     C                   eval      indFoDiaH = *off
     C                   eval      indFoDiaI = *off
     C                   eval      indFoDiaJ = *off
     C                   eval      indFoDiaK = *off
     C                   eval      indFoDiaL = *off
     C***** End Add *** 09/01/16 ****************************************
     C*
     C*** BEG ADD *** 11/10/02 ****************************************
     C*
     C* SET OFF CURSOR POSITIONING INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       3738
     C*** END ADD *** 11/10/02 ****************************************
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C*** BEG ADD ****** 08/13/15 ************************************
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
             *in31 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in32 = *on;
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
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C*** BEG CHG *** 11/10/02 ****************************************
     C********** *INKL     IFEQ *OFF
     C**********           MOVELXXICD9    SCDIA1
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      SCDIA1 = XXICD9
     C*****              MOVE      XXICD9        SCDIA1
     C*** END CHG ****** 08/13/15 ************************************
     C*** END CHG *** 11/10/02 ****************************************
     C                   MOVEL     XXLOST        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C*** BEG ADD *** 11/10/02 ****************************************
     C                   SETON                                        32
     C                   ELSE
     C                   SETON                                        31
     C*** END ADD *** 11/10/02 ****************************************
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** BEG ADD ****** 08/13/15 ************************************
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
             *in32 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in33 = *on;
          Endif;

          If scDia2 <> *blanks;
            kkICD10 = scDIa2;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS2 = 'D';
              SCDSC2 = ICDSC1;
              SCDS2  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C*** BEG CHG *** 11/10/02 ****************************************
     C********** *INKL     IFEQ *OFF
     C**********           MOVELXXICD9    SCDIA2
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      SCDIA2 = XXICD9
     C*****              MOVE      XXICD9        SCDIA2
     C*** END CHG ****** 08/13/15 ************************************
     C*** END CHG *** 11/10/02 ****************************************
     C                   MOVEL     XXLOST        SCLOS2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC2
     C  N60              MOVEL     DIDS1         SCDS2
     C*** BEG ADD *** 11/10/02 ****************************************
     C                   SETON                                        33
     C                   ELSE
     C                   SETON                                        32
     C*** END ADD *** 11/10/02 ****************************************
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C*** BEG ADD ****** 08/13/15 ************************************
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
             *in33 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in34 = *on;
          Endif;

          If scDia3 <> *blanks;
            kkICD10 = scDIa3;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS3 = 'D';
              SCDSC3 = ICDSC1;
              SCDS3  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C*** BEG CHG *** 11/10/02 ****************************************
     C********** *INKL     IFEQ *OFF
     C**********           MOVELXXICD9    SCDIA3
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      SCDIA3 = XXICD9
     C*****              MOVE      XXICD9        SCDIA3
     C*** END CHG ****** 08/13/15 ************************************
     C*** END CHG *** 11/10/02 ****************************************
     C                   MOVEL     XXLOST        SCLOS3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C  N60              MOVEL     DIDS1         SCDS3
     C*** BEG ADD *** 11/10/02 ****************************************
     C                   SETON                                        34
     C                   ELSE
     C                   SETON                                        33
     C*** END ADD *** 11/10/02 ****************************************
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C*** BEG ADD ****** 08/13/15 ************************************
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
             *in34 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in35 = *on;
          Endif;

          If scDia4 <> *blanks;
            kkICD10 = scDIa4;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS4 = 'D';
              SCDSC4 = ICDSC1;
              SCDS4  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C*** BEG CHG *** 11/10/02 ****************************************
     C********** *INKL     IFEQ *OFF
     C**********           MOVELXXICD9    SCDIA4
     C     XXICD9        IFNE      *BLANKS
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      SCDIA4 = XXICD9
     C*****              MOVE      XXICD9        SCDIA4
     C*** END CHG ****** 08/13/15 ************************************
     C*** END CHG *** 11/10/02 ****************************************
     C                   MOVEL     XXLOST        SCLOS4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC4
     C  N60              MOVEL     DIDS1         SCDS4
     C*** BEG ADD *** 11/10/02 ****************************************
     C                   SETON                                        35
     C                   ELSE
     C                   SETON                                        34
     C*** END ADD *** 11/10/02 ****************************************
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
B01  C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C***** Beg Add *** 09/01/16 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIAE'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #E SEARCH
     C*
      /Free
          HLDIA7 = SCDIAE;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaE = HLDIA7;
             *in90 = *on;
             indFoDiaE = *on;
          Else;
             scDiaE = outICD10;
             *in90 = *on;
             indFoDiaF = *on;
          Endif;

          If scDiaE <> *blanks;
            kkICD10 = scDiaE;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSE = 'D';
              SCDSCE = ICDSC1;
              SCDSE  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAE = XXICD9
     C                   MOVEL     XXLOST        SCLOSE
     C                   MOVEL     SCDIAE        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCE
     C  N60              MOVEL     DIDS1         SCDSE
     C                   Eval      indFoDiaF = *on
     C                   ELSE
     C                   Eval      indFoDiaE = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAF'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #F SEARCH
     C*
      /Free
          HLDIA7 = SCDIAF;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaF = HLDIA7;
             *in90 = *on;
             indFoDiaF = *on;
          Else;
             scDiaF = outICD10;
             *in90 = *on;
             indFoDiaG = *on;
          Endif;

          If scDiaF <> *blanks;
            kkICD10 = scDiaF;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSF = 'D';
              SCDSCF = ICDSC1;
              SCDSF  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAF = XXICD9
     C                   MOVEL     XXLOST        SCLOSF
     C                   MOVEL     SCDIAF        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCF
     C  N60              MOVEL     DIDS1         SCDSF
     C                   Eval      indFoDiaG = *on
     C                   ELSE
     C                   Eval      indFoDiaF = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAG'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #G SEARCH
     C*
      /Free
          HLDIA7 = SCDIAG;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaG = HLDIA7;
             *in90 = *on;
             indFoDiaG = *on;
          Else;
             scDiaG = outICD10;
             *in90 = *on;
             indFoDiaH = *on;
          Endif;

          If scDiaG <> *blanks;
            kkICD10 = scDiaG;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSG = 'D';
              SCDSCG = ICDSC1;
              SCDSG  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAG = XXICD9
     C                   MOVEL     XXLOST        SCLOSG
     C                   MOVEL     SCDIAG        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCG
     C  N60              MOVEL     DIDS1         SCDSG
     C                   Eval      indFoDiaH = *on
     C                   ELSE
     C                   Eval      indFoDiaG = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAH'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #H SEARCH
     C*
      /Free
          HLDIA7 = SCDIAH;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaH = HLDIA7;
             *in90 = *on;
             indFoDiaH = *on;
          Else;
             scDiaH = outICD10;
             *in90 = *on;
             indFoDiaI = *on;
          Endif;

          If scDiaH <> *blanks;
            kkICD10 = scDiaH;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSH = 'D';
              SCDSCH = ICDSC1;
              SCDSH  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAH = XXICD9
     C                   MOVEL     XXLOST        SCLOSH
     C                   MOVEL     SCDIAH        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCH
     C  N60              MOVEL     DIDS1         SCDSH
     C                   Eval      indFoDiaI = *on
     C                   ELSE
     C                   Eval      indFoDiaH = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAI'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #I SEARCH
     C*
      /Free
          HLDIA7 = SCDIAI;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaI = HLDIA7;
             *in90 = *on;
             indFoDiaI = *on;
          Else;
             scDiaI = outICD10;
             *in90 = *on;
             indFoDiaJ = *on;
          Endif;

          If scDiaI <> *blanks;
            kkICD10 = scDiaI;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSI = 'D';
              SCDSCI = ICDSC1;
              SCDSI  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAI = XXICD9
     C                   MOVEL     XXLOST        SCLOSI
     C                   MOVEL     SCDIAI        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCI
     C  N60              MOVEL     DIDS1         SCDSI
     C                   Eval      indFoDiaJ = *on
     C                   ELSE
     C                   Eval      indFoDiaI = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAJ'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #J SEARCH
     C*
      /Free
          HLDIA7 = SCDIAJ;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaJ = HLDIA7;
             *in90 = *on;
             indFoDiaJ = *on;
          Else;
             scDiaJ = outICD10;
             *in90 = *on;
             indFoDiaK = *on;
          Endif;

          If scDiaJ <> *blanks;
            kkICD10 = scDiaJ;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSJ = 'D';
              SCDSCJ = ICDSC1;
              SCDSJ  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAJ = XXICD9
     C                   MOVEL     XXLOST        SCLOSJ
     C                   MOVEL     SCDIAJ        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCJ
     C  N60              MOVEL     DIDS1         SCDSJ
     C                   Eval      indFoDiaK = *on
     C                   ELSE
     C                   Eval      indFoDiaJ = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAK'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #K SEARCH
     C*
      /Free
          HLDIA7 = SCDIAK;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaK = HLDIA7;
             *in90 = *on;
             indFoDiaK = *on;
          Else;
             scDiaK = outICD10;
             *in90 = *on;
             indFoDiaL = *on;
          Endif;

          If scDiaK <> *blanks;
            kkICD10 = scDiaK;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSK = 'D';
              SCDSCK = ICDSC1;
              SCDSK  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAK = XXICD9
     C                   MOVEL     XXLOST        SCLOSK
     C                   MOVEL     SCDIAK        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCK
     C  N60              MOVEL     DIDS1         SCDSK
     C                   Eval      indFoDiaL = *on
     C                   ELSE
     C                   Eval      indFoDiaK = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIAL'
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #L SEARCH
     C*
      /Free
          HLDIA7 = SCDIAL;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDiaL = HLDIA7;
             *in90 = *on;
             indFoDiaL = *on;
          Else;
             scDiaL = outICD10;
             *in90 = *on;
             *in35 = *on;
          Endif;

          If scDiaL <> *blanks;
            kkICD10 = scDiaL;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOSL = 'D';
              SCDSCL = ICDSC1;
              SCDSL  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM      'D'           XXLOST
     C*
     C                   SETON                                        90
     C     XXICD9        IFNE      *BLANKS
     C                   EVAL      SCDIAL = XXICD9
     C                   MOVEL     XXLOST        SCLOSL
     C                   MOVEL     SCDIAL        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSCL
     C  N60              MOVEL     DIDS1         SCDSL
     C                   Eval      *in35 = *on
     C                   ELSE
     C                   Eval      indFoDiaL = *on
     C                   ENDIF
B01  C                   ENDIF
     C*
     C***** End Add *** 09/01/16 ************************************
     C*
     C     HFLD          WHENEQ    'SCPMNT'
     C*
     C     SCTAPE        IFEQ      'Y'
     C                   MOVEL     'FLPP'        ##TBID
     C                   ELSE
     C                   MOVEL     'FLPA'        ##TBID
     C                   ENDIF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** PARM 'FLPP'    ##TBID
     C                   PARM                    ##TBID
     C*** END CHG *** 04/15/04 ****************************************
     C*
     C*** BEG DEL *** 11/10/02 ****************************************
     C********** ##KC      IFEQ '1'
     C********** ##KL      OREQ '1'
     C********** ##RRN     OREQ *ZEROS
     C*** END DEL *** 11/10/02 ****************************************
     C*** BEG ADD *** 11/10/02 ****************************************
     C     ##RRN         IFEQ      *ZEROS
     C*** END ADD *** 11/10/02 ****************************************
     C                   SETON                                        9036
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPMNT
     C                   SETON                                        9037
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*** BEG CHG *** 09/20/00 ****************************************
     C     HFLD          WHENEQ    'SCFLIC'
     C***********          SETON                     9069
     C***********          SETOF                     6867
     C                   MOVEL     'FL'          KKST
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCLIC#    HLLIC#  9
     C                   MOVEL(P)  SCLIC#        HLLIC#           13
     C*** END CHG *** 04/28/03 ****************************************
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** PARM           ##LIC#  9
     C                   PARM                    ##LIC#           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   SETON                                        9038
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELHLLIC#    SCLIC#  9
     C                   MOVEL(P)  HLLIC#        SCLIC#           13
     C*** END CHG *** 04/28/03 ****************************************
     C                   ELSE
     C     ##LIC#        IFEQ      *BLANKS
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELHLLIC#    SCLIC#
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C*** END CHG *** 04/28/03 ****************************************
     C                   SETON                                        9038
     C                   ELSE
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVEL##LIC#    SCFLIC
     C                   MOVEL(P)  ##LIC#        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   SETON                                        9036
     C                   ENDIF                                                  ##LIC#=*BLAN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*** BEG CHG *** 11/10/02 ****************************************
     C******************** SETON                     9038
     C                   SETON                                        9069
     C*** END CHG *** 11/10/02 ****************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*** END CHG *** 09/20/00 ****************************************
     C     HFLD          WHENEQ    'SCTAPE'
     C*** BEG DEL *** 04/15/04 ****************************************
     C*******************  SETON                     9068
     C*******************  SETOF                     6967
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   SETON                                        37
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'FLES'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTAPE
     C                   MOVEL(P)  GTDESC        SCDSDS
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*** BEG ADD *** 11/10/02 ****************************************
     C     HFLD          WHENEQ    'SCRCVD'
     C                   SETON                                        9039
     C*** END ADD *** 11/10/02 ****************************************
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
     C*** BEG ADD *** 04/11/00 ****************************************
     C                   Z-ADD     CAACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     EORCN         IFEQ      0
     C                   Z-ADD     EOCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
     C                   ELSE
     C                   MOVEL     'R'           KKBLST
     C     ESKY          SETGT     PES01
     C     ESKY1         READPE    PES01                                  59
     C     *IN59         IFEQ      *OFF
     C                   Z-ADD     ESRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Populate the Carr paid dt
     C*
     C                   Z-ADD     EOWCCK        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPAID
     C*** END ADD *** 04/11/00 ****************************************
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C                   MOVE      EHEDI         SCTAPE
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVE EHLIC#    SCFLIC
     C                   MOVEL(P)  EHLIC#        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EHFLIC        IFNE      *BLANKS
     C*** BEGIN CHG *** 10/09/03 **************************************
     C**********           MOVELEHLIC#    SCFLIC    P
     C                   MOVEL(P)  EHFLIC        SCFLIC
     C*** END   CHG *** 10/09/03 **************************************
     C                   ENDIF
     C*** END ADD *** 04/28/03 ****************************************
     C                   MOVE      EHPPLN        SCPMNT
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
     C*
     C***** Beg Add *** 09/01/16 **************************************
     C*
     C     EHICDE        IFNE      *BLANKS
     C                   MOVEL     EHICDE        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCE
     C                   MOVEL     DILOST        SCLOSE
     C                   MOVEL     DIDS1         SCDSE
     C                   MOVEL     EHICDE        SCDIAE
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCE
     C                   MOVE      *BLANKS       SCLOSE
     C                   MOVE      *BLANKS       SCDSE
     C                   MOVE      *BLANKS       SCDIAE
     C                   ENDIF
     C*
     C     EHICDF        IFNE      *BLANKS
     C                   MOVEL     EHICDF        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCF
     C                   MOVEL     DILOST        SCLOSF
     C                   MOVEL     DIDS1         SCDSF
     C                   MOVEL     EHICDF        SCDIAF
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCF
     C                   MOVE      *BLANKS       SCLOSF
     C                   MOVE      *BLANKS       SCDSF
     C                   MOVE      *BLANKS       SCDIAF
     C                   ENDIF
     C*
     C     EHICDG        IFNE      *BLANKS
     C                   MOVEL     EHICDG        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCG
     C                   MOVEL     DILOST        SCLOSG
     C                   MOVEL     DIDS1         SCDSG
     C                   MOVEL     EHICDG        SCDIAG
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCG
     C                   MOVE      *BLANKS       SCLOSG
     C                   MOVE      *BLANKS       SCDSG
     C                   MOVE      *BLANKS       SCDIAG
     C                   ENDIF
     C*
     C     EHICDH        IFNE      *BLANKS
     C                   MOVEL     EHICDH        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCH
     C                   MOVEL     DILOST        SCLOSH
     C                   MOVEL     DIDS1         SCDSH
     C                   MOVEL     EHICDH        SCDIAH
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCH
     C                   MOVE      *BLANKS       SCLOSH
     C                   MOVE      *BLANKS       SCDSH
     C                   MOVE      *BLANKS       SCDIAH
     C                   ENDIF
     C*
     C     EHICDI        IFNE      *BLANKS
     C                   MOVEL     EHICDI        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCI
     C                   MOVEL     DILOST        SCLOSI
     C                   MOVEL     DIDS1         SCDSI
     C                   MOVEL     EHICDI        SCDIAI
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCI
     C                   MOVE      *BLANKS       SCLOSI
     C                   MOVE      *BLANKS       SCDSI
     C                   MOVE      *BLANKS       SCDIAI
     C                   ENDIF
     C*
     C     EHICDJ        IFNE      *BLANKS
     C                   MOVEL     EHICDJ        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCJ
     C                   MOVEL     DILOST        SCLOSJ
     C                   MOVEL     DIDS1         SCDSJ
     C                   MOVEL     EHICDJ        SCDIAJ
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCJ
     C                   MOVE      *BLANKS       SCLOSJ
     C                   MOVE      *BLANKS       SCDSJ
     C                   MOVE      *BLANKS       SCDIAJ
     C                   ENDIF
     C*
     C     EHICDK        IFNE      *BLANKS
     C                   MOVEL     EHICDK        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCK
     C                   MOVEL     DILOST        SCLOSK
     C                   MOVEL     DIDS1         SCDSK
     C                   MOVEL     EHICDK        SCDIAK
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCK
     C                   MOVE      *BLANKS       SCLOSK
     C                   MOVE      *BLANKS       SCDSK
     C                   MOVE      *BLANKS       SCDIAK
     C                   ENDIF
     C*
     C     EHICDL        IFNE      *BLANKS
     C                   MOVEL     EHICDL        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSCL
     C                   MOVEL     DILOST        SCLOSL
     C                   MOVEL     DIDS1         SCDSL
     C                   MOVEL     EHICDL        SCDIAL
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSCL
     C                   MOVE      *BLANKS       SCLOSL
     C                   MOVE      *BLANKS       SCDSL
     C                   MOVE      *BLANKS       SCDIAL
     C                   ENDIF
     C***** End Add *** 09/01/16 **************************************
     C*
     C******** BEGIN ADD **********  09/10/07 *************************
     C                   MOVEL     EOFLIC        SCSVZP
     C******** END ADD ************  09/10/07 *************************
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
         scICDind = EHicdInd;
         If SCICDIND = 'Y';

           If EHICD1001 <> *blanks;
              SCDIA1 = EHICD1001;
              kkICD10 = SCDIA1;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSC1 = ICDSC1;
                SCDS1 = ICFCODE;
                SCLOS1 = 'D';
              EndIf;
           EndIf;

           If EHICD1002 <> *blanks;
              SCDIA2 = EHICD1002;
              kkICD10 = SCDIA2;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC2 = ICDSC1;
                   SCDS2 = ICFCODE;
                   SCLOS2 = 'D';
                EndIf;
           EndIf;

           If EHICD1003 <> *blanks;
              SCDIA3 = EHICD1003;
              kkICD10 = SCDIA3;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC3 = ICDSC1;
                   SCDS3 = ICFCODE;
                   SCLOS3 = 'D';
                EndIf;
           EndIf;

           If EHICD1004 <> *blanks;
              SCDIA4 = EHICD1004;
              kkICD10 = SCDIA4;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC4 = ICDSC1;
                   SCDS4 = ICFCODE;
                   SCLOS4 = 'D';
                EndIf;
           EndIf;

E01      EndIf;
      /End-Free

       //***** Beg Add *** 09/01/16 ************************************
      /Free

           If EHICDE <> *blanks;
              SCDIAE = EHICDE;
              kkICD10 = SCDIAE;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCE = ICDSC1;
                SCDSE = ICFCODE;
                SCLOSE = 'D';
              EndIf;
           EndIf;

           If EHICDF <> *blanks;
              SCDIAF = EHICDF;
              kkICD10 = SCDIAF;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCF = ICDSC1;
                SCDSF = ICFCODE;
                SCLOSF = 'D';
              EndIf;
           EndIf;

           If EHICDG <> *blanks;
              SCDIAG = EHICDG;
              kkICD10 = SCDIAG;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCG = ICDSC1;
                SCDSG = ICFCODE;
                SCLOSG = 'D';
              EndIf;
           EndIf;

           If EHICDH <> *blanks;
              SCDIAH = EHICDH;
              kkICD10 = SCDIAH;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCH = ICDSC1;
                SCDSH = ICFCODE;
                SCLOSH = 'D';
              EndIf;
           EndIf;

           If EHICDI <> *blanks;
              SCDIAI = EHICDI;
              kkICD10 = SCDIAI;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCI = ICDSC1;
                SCDSI = ICFCODE;
                SCLOSI = 'D';
              EndIf;
           EndIf;

           If EHICDJ <> *blanks;
              SCDIAJ = EHICDJ;
              kkICD10 = SCDIAJ;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCJ = ICDSC1;
                SCDSJ = ICFCODE;
                SCLOSJ = 'D';
              EndIf;
           EndIf;

           If EHICDK <> *blanks;
              SCDIAK = EHICDK;
              kkICD10 = SCDIAK;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCK = ICDSC1;
                SCDSK = ICFCODE;
                SCLOSK = 'D';
              EndIf;
           EndIf;

           If EHICDL <> *blanks;
              SCDIAL = EHICDL;
              kkICD10 = SCDIAL;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSCL = ICDSC1;
                SCDSL = ICFCODE;
                SCLOSL = 'D';
              EndIf;
           EndIf;

      /End-Free
       //***** End Add *** 09/01/16 ************************************

     C*** END ADD ****** 08/13/15 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*** BEG CHG *** 11/10/02 ****************************************
     C******************** MOVELSCDIA1    EHDIA1
     C******************** MOVELSCDIA2    EHDIA2
     C******************** MOVELSCDIA3    EHDIA3
     C******************** MOVELSCDIA4    EHDIA4
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      EHDIA1 = SCDIA1
     C                   EVAL      EHDIA2 = SCDIA2
     C                   EVAL      EHDIA3 = SCDIA3
     C                   EVAL      EHDIA4 = SCDIA4
     C*****              MOVE      SCDIA1        EHDIA1
     C*****              MOVE      SCDIA2        EHDIA2
     C*****              MOVE      SCDIA3        EHDIA3
     C*****              MOVE      SCDIA4        EHDIA4
     C*** END CHG ****** 08/13/15 ************************************
     C***** Beg Add *** 09/01/16 *************************************
     C                   EVAL      EHICDE = SCDIAE
     C                   EVAL      EHICDF = SCDIAF
     C                   EVAL      EHICDG = SCDIAG
     C                   EVAL      EHICDH = SCDIAH
     C                   EVAL      EHICDI = SCDIAI
     C                   EVAL      EHICDJ = SCDIAJ
     C                   EVAL      EHICDK = SCDIAK
     C                   EVAL      EHICDL = SCDIAL
     C***** End Add *** 09/01/16 *************************************
     C*** END CHG *** 11/10/02 ****************************************
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
        EHicdInd = scICDind;
        If  SCICDIND = 'Y';

          EHICD1001 = SCDIA1;
          EHICD1002 = SCDIA2;
          EHICD1003 = SCDIA3;
          EHICD1004 = SCDIA4;

       //***** Beg Add *** 09/01/16 *************************************
          EHICDE = SCDIAE;
          EHICDF = SCDIAF;
          EHICDG = SCDIAG;
          EHICDH = SCDIAH;
          EHICDI = SCDIAI;
          EHICDJ = SCDIAJ;
          EHICDK = SCDIAK;
          EHICDL = SCDIAL;
       //***** End Add *** 09/01/16 *************************************

E01     EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCFLIC    EHLIC#
     C                   MOVEL(P)  SCFLIC        EHFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   MOVEL     SCTAPE        EHEDI
     C                   MOVEL     SCTAPE        EOTAPE
     C     EAKEY         CHAIN     PEA01                              60
     C                   MOVEL     SCTAPE        EAEDI
     C                   MOVEL     SCPMNT        EHPPLN
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C*  Update FAEOP if RCN = 0; else update FAESP  carr rcvd dt
     C*
     C     EORCN         IFEQ      0
     C                   MOVE      SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C                   ELSE
     C                   MOVE      SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRRDT
     C                   Z-ADD     EORCN         ESRCN
     C                   ENDIF
     C*** END ADD *** 04/11/00 ****************************************
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
     C************ BEGIN ADD ********* 09/10/07 *********************
     C                   MOVE      SCSVZP        EOFLI5
      ************ END ADD *********** 09/10/07 *********************
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
     C*** BEG ADD *** 04/11/00 ****************************************
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C*** END ADD *** 04/11/00 ****************************************
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
     C*** BEG ADD *** 02/23/06 ****************************************
     C                   CALL      'SA009R'
 1   C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
 2   C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
 7   C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
 8   C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
 9   C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
10   C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
11   C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C*
     C*** END ADD *** 02/23/06 ****************************************
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
     C*******    EAKEY     CHAINPEA01                60
     C     @@RRN2        CHAIN     PEO01                              60
     C*** BEG ADD *** 04/11/00 ****************************************
     C     ESKY          SETGT     PES01
     C     ESKY1         READPE    PES01                                  59
     C*** END ADD *** 04/11/00 ****************************************
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   UPDATE    PEO01
     C*** BEG ADD *** 04/11/00 ****************************************
     C  N59              UPDATE    PES01
     C*** END ADD *** 04/11/00 ****************************************
     C*******              UPDATPEA01
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
     C                   ENDSR
     C*** BEG DEL *** 05/09/11 ****************************************
*****C*TABLE @A  - ALPHABET
*****C*****ABCDEFGHIJKLMNOPQRSTUVWXYZ
*****C*
     C***0123456789
     C*** END DEL *** 05/09/11 ****************************************
