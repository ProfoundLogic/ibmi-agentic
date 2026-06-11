     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*  Program: F0461RFL                                            *
     H*  Description: Work With EOMBs Header Info update/display      *
     H*               FLORIDA VERSION/HOSPITAL BILLS                  *
     H*                                                               *
     H*  Created:  C:117  01/15/2000                                  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*
     H*   12/02/15 C:212 Fix some missing/misplaced code while retrofit
     H*
     H*   12/02/15 C:212 Support:38640-Retrofit Support 37918 changes *
     H*   11/03/15 C:307 Support 37918                                *
     H*                  FL ICD10 Medical EDI changes.                *
     H*                  FL Hospital EDI correction screen 2          *
     H*                                                               *
     H*   08/13/15 C:212 Project FL092                                *
     H*                  FL ICD10 Medical EDI changes.DSPF changed too*
     H*
     H*   05/27/15 C:306 SUPPORT 34971                                *
     H*                  Removed the F2 button from F0461FFL          *
     H*                  screen action panel. (Recompile only).       *
     H*
     H*  05/26/15  C:212  Support 34974
     H*                   DSPF changed, program recompiled :
     H*                   Removed SCR03/SCDSDS that is used in SCR01 but was
     H*                   redefined in SCR03 and causing overlapping
     H*
     H*  03/11/10  C:227  PROJECT FL083
     H*                   Added field EOBNPI
     H*
     H*  10/04/08  C:180  SUPPORT 65159
     H*                   Fourth position of FL Phy lic can be blank
     H*
     H*  08/08/08  C:079  SY156
     H*                   PRISS.  Prevent going to scr03 after search
     H*                   on license number
     H*
     H*  09/25/07  C:180  SUPP 59201
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
     H*  09/10/07 C:180   SUPPORT: 58610                              *
     H*                   Allow editing of zip code.
     H*
     H*  07/28/07 C:079   Project: FL067                              *
     H*                   Do not edit against the lic database        *
     H*
     H*  05/09/07 C:182   Project: FL067                              *
     H*                   1.Added New Field on scr02                  *
     H*                         'Operating Physicians Lic#:'          *
     H*                                                               *
     H*  04/23/07 C:188 PROJECT:FL061                                 *
     H*                 Set EDITST to adj or policy state based       *
     H*                 on value of EOCPST                            *
     H*                                                               *
     H* 03/14/07 C:182   Project: FL067                              *
     H*                  1.Add New Screen SCR02 with fields          *
     H*                        'Other proc (3)'                      *
     H*                        'Other proc (4)'                      *
     H*                        'Other proc (5)'                      *
     H*                        'Other proc (6)'                      *
     H*                        'Other diag (5)'                      *
     H*                        'Other diag (6)'                      *
     H*                        'Other diag (6)'                      *
     H*                        'Other diag (8)'                      *
     H*                        'Ext. cause (1)'                      *
     H*                        'Ext. cause (2)'                      *
     H*                        'Ext. cause (3)'                      *
     H*                        'Sched/Unsched '                      *
     H*                                                               *
     H*
     H*  02/27/06 C:163 SUPPORT: MC010 49588
     H*                 PRINCIPAL PROCEDURE CODE NO LONGER REQUIRED
     H*
     H*  02/23/06 C:163 SUPPORT:MC010 51595
     H*                 FL ALLOW FUTURE DATE OF SERVICE
     H*
     H*  02/01/06 C:079 SUPPORT: 51252
     H*                 ALLOW UPDATE OF STATEMENT COVERS FIELDS
     H*                 COMPILE ONLY
     H*
     H*  11/08/05 C:163 PROJECT: 48MWK
     H*                 FL EDI CORRECTION LICENSE #
     H*
     H*  04/15/04 C:079 PROJECT: FL046
     H*                 USE GENERIC TABLE FOR SCTAPE
     H*
     H*  08/19/03 C:062 PROJECT: FL045
     H*                 -ONLY ALLOW OPER TYPE FOR PROCEDURE CODES 9
     H*                 -ONLY SEARCH PROCEDURE CODE FILE.
     H*                 -ADD EDITS FOR SCADDT, SCSCPF AND SCSCPT
     H*
     H*  04/28/03 C:079 PROJECT: FL043
     H*                 EXPAND LIC# FIELD TO 13A
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  12/09/01  C:079  PROJECT:FL040
     H*                   ALLOW 4TH POS OF LIC# TO BE ALPHANUM
     H*
     H*  09/18/01  C:079  SUPPORT:MC044 28917
     H*                   ALLOW THE 3RD POSITION OF LIC# TO BE ALPAH
     H*                   OR NUMERIC
     H*
     H*  09/20/00  C:117  PROJECT:FL031
     H*                   CHANGEING THE EDITTING FOR SCLIC#;
     H*                   ADDING SEARCH TO SAME FIELD.
     H*
     H*  04/11/00  C:117  PROJECT:FS211
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA
     H*                   SUBMISSION
     H*
     H*  04/02/00 C:117 Project: MCMNT                                *
     H*                 FIXED SCREEN HEADING (I* SPEC)
     H*
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
     FF0461FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEUP     UF A E             DISK
     F                                     RENAME(PEU01:DBRCDF)
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     FFAEAL1    IF   E           K DISK
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
     F*** BEGIN DEL *** 08/19/03 **************************************
     F**********FMRCP   IF  E                    DISK
     F*** END   DEL *** 08/19/03 **************************************
     FFMRCL1    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     F*** BEG ADD *** 04/11/00 ****************************************
     FFAESL3    UF   E           K DISK
     F*** END ADD *** 04/11/00 ****************************************
     F*** BEGIN ADD *** 10/10/03 **************************************
     FFAESL5    UF   E           K DISK
     F                                     RENAME(PES01:PES02)
     FFAEUL     UF   E           K DISK
     F                                     INFDS(DBFDSA)
     F*** END   ADD *** 10/10/03 **************************************
     F*** BEG ADD *** 09/20/00 ****************************************
     F* FL Physician DB File
     F*** BEG DEL *** 07/28/07 ****************************************
     F* FL Physician DB File
     F****** FMPLLFL IF  E           K        DISK
     F*** END DEL *** 07/28/07 ****************************************
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*** END ADD *** 09/20/00 ****************************************
     F*** Beg Add *** 08/13/15 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/13/15 ***************************************
      /COPY SCOPYMT01D
     D*** BEG DELETE 09/25/07 *****************************************
     D****¢¢¢             LT     26  26  1               ALPHABET
     D*** END DELETE 09/25/07 *****************************************
     D*** BEG ADD *** 09/25/07 ****************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D*** END ADD *** 09/25/07 ****************************************
     D*
     D***PY SCOPYMT01I
     D*** BEGIN ADD *** 10/10/03 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*** END   ADD *** 10/10/03 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) -
     D                                     Header Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(2)
     D*** BEG DEL *** 12/09/01 ****************************************
     D**************************************
     D****Data*structure*of*FL*License*#*to*make sure first two
     D******characters*of*license*#*are*alpha
     D************DS************************
     D**************************************  1   9 SCLIC#
     D**************************************  1   1 SCLIC1
     D**************************************  2   2 SCLIC2
     D****BEG*ADD*****10/21/99*****************************************
     D****BEG*CHG*****09/18/01*****************************************
     D*************************************** 3   9 SCLIC3
     D**************************************  3   3 SCLIC3
     D**************************************  4   9 SCLIC4
     D*** END CHG *** 09/18/01 ****************************************
     D*** END ADD *** 10/21/99 ****************************************
     D*** END DEL *** 12/09/01 ****************************************
     D*** BEG ADD *** 12/09/01 ****************************************
     D*
     D*   Data structure of FL License # to make sure first two
     D*     characters of license # are alpha
     D                 DS
     D*** BEG CHG *** 04/28/03 ****************************************
     D*************************************** 1   9 SCLIC#
     D  SCLIC#                 1     13
     D*** END CHG *** 04/28/03 ****************************************
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D*** BEG END *** 04/28/03 ****************************************
     D*************************************** 5   9 SCLIC5
     D*** END END *** 04/28/03 ****************************************
     D*** BEG ADD *** 04/28/03 ****************************************
     D*** BEG DEL *** 09/25/07 ****************************************
     D**********                              5  11 SCLIC5
     D**********                              4  10 SCLIC6
     D**********                              3   9 SCLIC7
     D*** END DEL *** 09/25/07 ****************************************
     D*** END ADD *** 04/28/03 ****************************************
     D*** BEG ADD *** 09/25/07 ****************************************
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*** END ADD *** 09/25/07 ****************************************
     D*** END DEL *** 12/09/01 ****************************************
     D******** BEGIN ADD **********  09/10/07 *************************
     D*   Data structure
     D                 DS
     D  EOFLIC                 1      9
     D  EOFLI5                 1      5
     D******** END ADD ************  09/10/07 *************************
     D*** BEG ADD *** 09/25/07 ****************************************
     D*
     D*
     D*OPERATING PHYSICIANS LICENSE NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCOLIC                 1     13
     D  SCOLC1                 1      1
     D  SCOLC2                 2      2
     D  SCOLC3                 3      3
     D  SCOLC4                 4      4
     D  SCOLCA                 3     13
     D  SCOLCB                 4     13
     D  SCOLCC                 5     13

     I*** END ADD *** 09/25/07 ****************************************
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
     D hldia_7         s              7    inz
     D*** End Add *** 08/13/15 ***************************************

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
     C*** BEGIN ADD *** 08/19/03 **************************************
     C                   KFLD                    KKLOST
     C*** END   ADD *** 08/19/03 **************************************
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
     C                   KFLD                    KKRCN
     C                   KFLD                    KKBLST
     C*** BEGIN ADD *** 10/10/03 **************************************
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRCN
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** END   ADD *** 10/10/03 **************************************
     C*
     C*** BEG ADD *** 09/20/00 ****************************************
     C*
     C* FL Provider Lic#
     C*
     C     PLKY          KLIST
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** KFLD           KKFLIC  9
     C                   KFLD                    KKFLIC           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY
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
     C*** BEG ADD *** 04/11/00 ****************************************
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C     *LIKE         DEFINE    ESRCN         KKRCN
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCRDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLRRDT
     C     *LIKE         DEFINE    EOCRDT        HLRSDT
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 09/20/00 ****************************************
     C     *LIKE         DEFINE    STKEY         ##KEY
     C     *LIKE         DEFINE    EUDIA1        HLDIA1
     C     *LIKE         DEFINE    EUDIA1        HLDIA2
     C     *LIKE         DEFINE    EUDIA1        HLDIA3
     C     *LIKE         DEFINE    EUDIA1        HLDIA4
    C*** BEG ADD ***** 03/14/07 **************************************
     C     *LIKE         DEFINE    EUDIA1        HLDIA5
     C     *LIKE         DEFINE    EUDIA1        HLDIA6
     C     *LIKE         DEFINE    EUDIA1        HLDIA7
     C     *LIKE         DEFINE    EUDIA1        HLDIA8
     C     *LIKE         DEFINE    DILOST        HLLOS5
     C     *LIKE         DEFINE    DILOST        HLLOS6
     C     *LIKE         DEFINE    DILOST        HLLOS7
     C     *LIKE         DEFINE    DILOST        HLLOS8
    C*** END ADD ***** 03/14/07 **************************************
     C     *LIKE         DEFINE    DILOST        HLLOS1
     C     *LIKE         DEFINE    DILOST        HLLOS2
     C     *LIKE         DEFINE    DILOST        HLLOS3
     C     *LIKE         DEFINE    DILOST        HLLOS4
     C*** END ADD *** 09/20/00 ****************************************
     C*** BEGIN ADD *** 10/10/03 **************************************
     C     *LIKE         DEFINE    EUADDT        HLADDT
     C     *LIKE         DEFINE    EUSCPF        HLSCPF
     C     *LIKE         DEFINE    EUSCPT        HLSCPT
     C*** END   ADD *** 10/10/03 **************************************
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEGIN ADD *** 10/10/03 **************************************
     C*================================================================
     C* CARERR - Date check subrountine
     C*================================================================
     C     CARERR        BEGSR
     C*
     C     CARTAG        TAG
     C*
     C     EORCN         IFEQ      0
     C                   SETON                                        19
     C                   SETOFF                                       20
     C                   Z-ADD     EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCSDT
     C                   EXFMT     SCR04
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C* DATE CARR RCVD CANNOT BE BLANK
     C*
     C     SCRCVD        IFEQ      *ZEROS
     C                   SETON                                        9010
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* DATE RECIEVED MUST BE VALID
     C*
     C     SCRCVD        IFNE      *ZEROS
     C                   Z-ADD     SCRCVD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCRCVD
     C                   Z-ADD     SCRCVD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDLT     EOACDT
     C                   SETON                                        9022
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDLT     EOTODT
     C*** BEG ADD *** 02/23/06 ****************************************
     C     ##SDTS        ANDNE     'Y'
     C*** END ADD *** 02/23/06 ****************************************
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDGT     HLSYDT
     C                   SETON                                        9025
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF ADJ STATE IS KY, SENT DT CANNOT BE BLANK
     C*
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****      EOAST     IFEQ 'KY'
     C     EDITST        IFEQ      'KY'
     C*** END CHG *** 04/23/07 ****************************************
     C     SCCSDT        ANDEQ     *ZEROS
     C                   SETON                                        9008
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRCVD        IFNE      *ZEROS
     C     SCCSDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C*
     C     HLCSDT        IFGT      HLCRDT
     C                   SETON                                        9026
     C                   GOTO      CARTAG
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCRCVD NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9027
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDLT     EOACDT
     C                   SETON                                        9013
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDLT     EOTODT
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDGT     HLSYDT
     C                   SETON                                        9011
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C                   MOVE      SCRCVD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C                   MOVE      EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCSDT
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE                                                   *INKL=OFF
     C*
     C                   SETON                                        90
     C                   GOTO      CAREND
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ELSE                                                   RCN=0
     C*
     C                   SETON                                        20
     C                   SETOFF                                       19
     C                   Z-ADD     SCRCVD        SCRRDT
     C                   Z-ADD     ESRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRSDT
     C*
     C                   EXFMT     SCR04
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CARR RCVD DT CANNOT BE BLANK
     C*
     C     SCRRDT        IFEQ      *ZEROS
     C                   SETON                                        9010
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* DATE RECIEVED MUST BE VALID
     C*
     C     SCRRDT        IFNE      *ZEROS
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCRRDT
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOACDT
     C                   SETON                                        9022
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOTODT
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDGT     HLSYDT
     C                   SETON                                        9025
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF ADJ STATE IS KY, SENT DT CANNOT BE BLANK
     C*
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****      EOAST     IFEQ 'KY'
     C     EDITST        IFEQ      'KY'
     C*** END CHG *** 04/23/07 ****************************************
     C     SCCSDT        ANDEQ     *ZEROS
     C                   SETON                                        9008
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRRDT        IFNE      *ZEROS
     C     SCRSDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C*
     C     HLRSDT        IFGT      HLRRDT
     C                   SETON                                        9026
     C                   GOTO      CARTAG
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCRCVD NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9027
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOACDT
     C                   SETON                                        9013
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOTODT
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDGT     HLSYDT
     C                   SETON                                        9011
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     ESKY1         SETGT     PES02
     C     ESKY2         READPE    PES02                                  60
     C                   MOVE      SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRRDT
     C                   MOVE      SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRSDT
     C                   Z-ADD     SCRRDT        SCRCVD
     C*
     C                   UPDATE    PES02
     C*
     C                   ELSE                                                   *INKL=OFF
     C*
     C                   SETON                                        90
     C                   GOTO      CAREND
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  RCN=0
     C*
     C     CAREND        ENDSR
     C*** END   ADD *** 10/10/03 **************************************
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
     C*** BEGIN ADD *** 10/10/03 **************************************
     C*================================================================
     C*  EDIERR - EDI ERROR SR FOR FUTURE DATES
     C*================================================================
     C     EDIERR        BEGSR
     C*
     C     EDITAG        TAG
     C*
     C                   EXFMT     SCR03
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCTAPE        IFNE      *BLANKS
     C     SCTAPE        ANDNE     'Y'
     C     SCTAPE        ANDNE     'N'
     C                   SETON                                        9018
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EUKY          CHAIN     PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCTAPE        EUEDI
     C                   UPDATE    PEU01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     EDIEND        ENDSR
     C*** END   ADD *** 10/10/03 **************************************
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
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
     C                   Else
     C/free
         kkICD10 = SCDIA1;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in71 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
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
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA2;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in77 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
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
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA3;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in78 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
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
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA4;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in79 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*
     C* Procedure Code Method is required
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* Validate Procedure Code Method
     C*
     C     SCPCM         IFNE      '0'
     C     SCPCM         ANDNE     '4'
     C     SCPCM         ANDNE     '5'
     C     SCPCM         ANDNE     '9'
     C                   SETON                                        9068
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM NE '0'
     C*
     C* If Proc Code Method =0 Principal Procedure Code MUST be BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC1        ANDNE     *BLANKS
     C                   SETON                                        9064
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF Proc Code Method =0 Secondary Procedure Code MUST be BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC2        ANDNE     *BLANKS
     C                   SETON                                        9062
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* Validate procedure codes base on procedure code methods entered
     C* if Proc Code Method NE 0 THEN Princ Procedure Code Required
     C     SCPCM         IFNE      '0'
     C*
     C     SCPCM         IFEQ      '4'
     C*** BEGIN ADD *** 02/27/06 **************************************
     C     SCPRC1        ANDNE     *BLANKS
     C*** END   ADD *** 02/27/06 **************************************
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9069
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=4
     C*
     C     SCPCM         IFEQ      '5'
     C*** BEGIN ADD *** 02/27/06 **************************************
     C     SCPRC1        ANDNE     *BLANKS
     C*** END   ADD *** 02/27/06 **************************************
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9069
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=5
     C*
     C*** BEGIN ADD *** 08/19/03 **************************************
     C                   MOVEL     ' '           SCLS1
     C*** END   ADD *** 08/19/03 **************************************
     C     SCPCM         IFEQ      '9'
     C*** BEGIN ADD *** 02/27/06 **************************************
     C     SCPRC1        ANDNE     *BLANKS
     C*** END   ADD *** 02/27/06 **************************************
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
     C*** BEGIN ADD *** 08/19/03 **************************************
     C                   MOVEL     'O'           SCLS1
     C*** END   ADD *** 08/19/03 **************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS1    KKLOST
     C                   MOVE      SCLS1         KKLOST
     C*** END   CHG *** 08/19/03 **************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9069
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCPRC1;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in69 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
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
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C*** BEGIN ADD *** 08/19/03 **************************************
     C                   MOVEL     ' '           SCLS2
     C*** END   ADD *** 08/19/03 **************************************
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 08/13/15 ************************************
     C*** BEGIN ADD *** 08/19/03 **************************************
     C                   MOVEL     'O'           SCLS2
     C*** END   ADD *** 08/19/03 **************************************
     C                   MOVEL(P)  SCPRC2        KKICD9
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS2    KKLOST
     C                   MOVE      SCLS2         KKLOST
     C*** END   CHG *** 08/19/03 **************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCPRC2;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in63 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C                   ENDIF                                                  SCPCM NE 0
     C*
     C*  Make sure Liscense# isn't blank
     C*
     C     SCFLIC        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*** BEG DEL *** 07/28/07 ****************************************
     C*  Check to see if the Provider DB installed
     C*******
     C*******              MOVEL*BLANKS   ##KEY
     C*******              MOVEL'FL'      ##KEY
     C*******    STKY      CHAINPST01                99
     C*******    *IN99     IFEQ *OFF
     C*******    STBS01    ANDEQ'Y'
     C*******
     C**edit*against the Prov Lic# database
     C*******
     C*******              MOVELSCFLIC    KKFLIC
     C*******    PLKY      CHAINPPL01                99
     C*******    *IN99     IFEQ *ON
     C*******              SETON                     9010
     C*******              ENDIF                           *IN99
     C*******
     C*******              ELSE                            STBS01=Y
     C*** END DEL *** 07/28/07 ****************************************
     C     SCFLIC        IFNE      *BLANKS
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCFLIC    SCLIC#
     C                   MOVEL(P)  SCFLIC        SCLIC#
     C*** END CHG *** 04/28/03 ****************************************
      ******¢¢¢¢ BEGIN DELETE ******* 09/25/07 ************************
     C****¢¢¢              Z-ADD1         X       20
     C****¢¢¢    SCLIC1    LOKUPLT,X                     60
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              MOVE '1'       *IN66
     C****¢¢¢              SETON                     90
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C****¢¢¢
     C****¢¢¢              Z-ADD1         X
     C****¢¢¢    SCLIC2    LOKUPLT,X                     60
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              MOVE '1'       *IN66
     C****¢¢¢              SETON                     90
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C*** BEG ADD *** 09/18/01 ****************************************
     C* 3rd position can be alpha or numeric
     C****¢¢¢              TESTN          SCLIC3     60
     C****¢¢¢
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢
     C****¢¢¢    SCLIC3    IFLT 'A'
     C****¢¢¢    SCLIC3    ORGT 'Z'
     C****¢¢¢
     C****¢¢¢              SETON                     9087
     C****¢¢¢              GOTO CK1END
     C****¢¢¢
     C****¢¢¢              ENDIF                           SCLIC3 IFLT
     C****¢¢¢
     C****¢¢¢              ENDIF                           *IN60 = 0
     C*** END ADD *** 09/18/01 ****************************************
     C*** BEG ADD *** 12/09/01 ****************************************
     C* 4TH position can be alpha or numeric
     C****¢¢¢              TESTN          SCLIC4     60
     C****¢¢¢
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢
     C****¢¢¢    SCLIC4    IFLT 'A'
     C****¢¢¢    SCLIC4    ORGT 'Z'
     C****¢¢¢
     C****¢¢¢              SETON                     9086
     C****¢¢¢              GOTO CK1END
     C****¢¢¢
     C****¢¢¢              ENDIF                           SCLIC4 IFLT
     C****¢¢¢
     C****¢¢¢              ENDIF                           *IN60 = 0
     C*** END ADD *** 12/09/01 ****************************************
     C*
     C*** BEG ADD *** 10/21/99 ****************************************
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
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              SETON                     6590
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C*** END ADD *** 10/21/99 ****************************************
     C****** BEG ADD ******* 08/28/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9066
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9066
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCLIC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9010
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C     SCLIC4        IFGT      *BLANKS
     C***** END ADD *******  10/04/08 *********************************
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9010
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9010
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C***** BEGIN ADD *****  10/04/08 *********************************
     C                   ENDIF
     C***** END ADD *******  10/04/08 *********************************
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCLICA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCLICB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCLICC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9065
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9065
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C****** END ADD ******* 08/28/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C*** END ADD *** 04/28/03 ****************************************
     C     *IN60         IFEQ      '0'
     C                   SETON                                        6590
     C                   GOTO      CK1END
     C                   END
     C*** END ADD *** 10/21/99 ****************************************
     C*
     C                   END                                                    LIC#<>*BLANK
     C*** BEG ADD *** 09/20/00 ****************************************
     C*** BEG DEL *** 07/28/07 ****************************************
     C*******************  END
     C*** END DEL *** 07/28/07 ****************************************
     C*** END ADD *** 09/20/00 ****************************************
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
     C     GTKY          CHAIN     PGT02                              99
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
     C*
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C*  Carr rcvd date added to screen; required
     C*
     C     SCRCVD        IFEQ      *ZEROS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRCVD EQ ZEROS
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
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEGIN ADD *** 10/10/03 **************************************
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLADDT
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSCPF
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSCPT
     C*
     C* VALIDATE ADMISSION DATE IF ENTERED
     C*
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETON                                        85
     C                   SETON                                        88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCADDT
     C                   ENDIF
     C*
     C* VALIDATE THE FROM STATEMENT COVERS DATE
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90
     C                   SETOFF                                       84
     C                   SETON                                        85
     C                   SETON                                        88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPF
     C                   ENDIF
     C*
     C* VALIDATE THE STATEMENT COVERS TO DATE
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETOFF                                       85
     C                   SETON                                        88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPT
     C                   ENDIF
     C*
     C* STATEMENT COVERS TO AND FROM DATES CANNOT BE LESS THAN DOI
     C*
     C     HLSCPF        IFNE      *ZEROS
     C     HLSCPF        ANDLT     EOACDT
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETON                                        85
     C                   SETOFF                                       88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C     HLSCPT        IFNE      *ZEROS
     C     HLSCPT        ANDLT     EOACDT
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETON                                        85
     C                   SETON                                        88
     C                   SETOFF                                       89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPT NE *ZEROS
     C*
     C* STATEMENT COVERS TO AND FROM DATES CANNOT BE GREATER THAN SYSTEM DATE
     C* UNLESS SYSTEM FLAG ALLOWS                                      M DATE
     C*
     C     ##SDTS        IFNE      'Y'
     C*
     C     HLSCPF        IFNE      *ZEROS
     C     HLSCPF        ANDGT     HLSYDT
     C                   SETON                                        90
     C                   SETOFF                                       84
     C                   SETOFF                                       85
     C                   SETON                                        88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C     HLSCPT        IFNE      *ZEROS
     C     HLSCPT        ANDGT     HLSYDT
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETOFF                                       85
     C                   SETOFF                                       88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN SYSTEM DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSYDT
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETON                                        85
     C                   SETOFF                                       88
     C                   SETOFF                                       89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C*  Give an error window if the date is a future date and the
     C*  data submission flag is 'Y'.
     C*
     C                   ELSE                                                   XXFSDF='Y'
     C     HLSCPT        IFGT      HLSYDT
     C     SCTAPE        ANDEQ     'Y'
     C                   EXSR      EDIERR
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##SDTS IFNE
     C*
     C* STATEMENT COVERS TO DATE MUST BE < STATEMENT COVERS TO DATE
     C*
     C     HLSCPF        IFGT      HLSCPT
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETOFF                                       85
     C                   SETON                                        88
     C                   SETOFF                                       89
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* DATE CARRIER RECEIVED CANNOT BE < DATE STATEMENT COVERS TO
     C*
     C* This is all conditioned on weather or not the EDI flag is on
     C*
     C     SCTAPE        IFEQ      'Y'
     C     HLSCPT        ANDGT     HLRCVD
     C                   EXSR      CARERR
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF                                                  EUEDI=Y
     C*
     C* ADMISSION DATE CANNOT BE LESS THAN ACCIDENT DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDLT     EOACDT
     C                   SETON                                        90
     C                   SETOFF                                       84
     C                   SETON                                        85
     C                   SETOFF                                       88
     C                   SETON                                        89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN STATEMENT COVERS FROM DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSCPF
     C                   SETON                                        90
     C                   SETON                                        84
     C                   SETOFF                                       85
     C                   SETOFF                                       88
     C                   SETOFF                                       89
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*** END   ADD *** 10/10/03 **************************************
     C*** BEG  ADD *** 03/11/10 **************************************
     C                   SETOFF                                       59
     C     EOREVI        IFGE      'E'
     C                   SETON                                        59
     C                   ENDIF
     C*** END  ADD *** 03/11/10 **************************************
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
                EndIf;
           EndIf;

           If SCDIA2 <> *blanks;
              kkICD10 = SCDIA2;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS2 = 'D';
                   SCDSC2 = ICDSC1;
                   SCDS2  = ICFCODE;
                EndIf;
           EndIf;

           If SCDIA3 <> *blanks;
              kkICD10 = SCDIA3;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS3 = 'D';
                   SCDSC3 = ICDSC1;
                   SCDS3  = ICFCODE;
                EndIf;
           EndIf;

           If SCDIA4 <> *blanks;
              kkICD10 = SCDIA4;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS4 = 'D';
                   SCDSC4 = ICDSC1;
                   SCDS4  = ICFCODE;
                EndIf;
           EndIf;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
    C*** BEG ADD ***** 03/14/07 **************************************
     C     SCDIA5        IFNE      *BLANKS
     C                   SETOFF                                       54
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     SCDIA5        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        5490
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS5
     C                   MOVEL     DIDSC1        SCDSC5
     C                   MOVEL     DIDS1         SCDS5
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA5;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in54 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA6        IFNE      *BLANKS
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   SETOFF                                       55
     C                   MOVEL     SCDIA6        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        5590
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS6
     C                   MOVEL     DIDSC1        SCDSC6
     C                   MOVEL     DIDS1         SCDS6
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA6;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in55 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA7        IFNE      *BLANKS
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   SETOFF                                       56
     C                   MOVEL     SCDIA7        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        5690
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS7
     C                   MOVEL     DIDSC1        SCDSC7
     C                   MOVEL     DIDS1         SCDS7
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA7;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in56 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*
     C     SCDIA8        IFNE      *BLANKS
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   SETOFF                                       57
     C                   MOVEL     SCDIA8        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        5790
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS8
     C                   MOVEL     DIDSC1        SCDSC8
     C                   MOVEL     DIDS1         SCDS8
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCDIA8;
         Chain (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in57 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*
     C* IF Proc Code Method =0 Secondary Procedure Code MUST be BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC3        ANDNE     *BLANKS
     C                   SETON                                        9095
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPRC3=*BLANKS
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC4        ANDNE     *BLANKS
     C                   SETON                                        9098
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPRC4=*BLANKS
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC5        ANDNE     *BLANKS
     C                   SETON                                        9029
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPRC5=*BLANKS
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC6        ANDNE     *BLANKS
     C                   SETON                                        9035
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPRC6=*BLANKS
     C*
     C* IF Proc Code Method NE 0 THEN Secondary Code is optional
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9093
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9093
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLS3
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     'O'           SCLS3
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLS3         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9093
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCPRC3;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in93 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C*                    ENDIF                           SCPCM NE 0
     C*
     C* IF Proc Code Method NE 0 THEN Secondary Code is optional
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9096
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9096
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLS4
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     'O'           SCLS4
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLS4         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9096
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCPRC4;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in96 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C*                    ENDIF                           SCPCM NE 0
     C*
     C* IF Proc Code Method NE 0 THEN Secondary Code is optional
     C     SCPRC5        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC5        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9014
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC5 NE *BLANKS
     C*
     C     SCPRC5        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC5        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9014
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC5 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLS5
     C     SCPRC5        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     'O'           SCLS5
     C                   MOVEL(P)  SCPRC5        KKICD9
     C                   MOVE      SCLS5         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9014
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCPRC5;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in14 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPRC5 NE *BLANKS
     C*
     C*                    ENDIF                           SCPCM NE 0
     C*
     C* IF Proc Code Method NE 0 THEN Secondary Code is optional
     C     SCPRC6        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC6        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9030
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC6 NE *BLANKS
     C*
     C     SCPRC6        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC6        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9030
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC6 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLS6
     C     SCPRC6        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     'O'           SCLS6
     C                   MOVEL(P)  SCPRC6        KKICD9
     C                   MOVE      SCLS6         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9030
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCPRC6;
         Chain(n) (kkICD10:'O') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in30 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPRC6 NE *BLANKS
     C*
     C     SCCAU1        IFNE      *BLANKS
     C*
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     SCCAU1        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9028
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCCAU1;
         Chain(n) (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in28 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCCAU1 NE *BLANKS
     C*
     C     SCCAU2        IFNE      *BLANKS
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C                   MOVEL     SCCAU2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9050
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCCAU2;
         Chain(n) (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in50 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCCAU2 NE *BLANKS
     C*
     C     SCCAU3        IFNE      *BLANKS
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND <> 'Y'
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVEL     SCCAU3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        6190
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DILOST        SCC1DS
     C                   MOVEL     DIDSC1        SCCDS3
     C                   MOVEL     DIDS1         SCCD1
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   Else
     C/free
         kkICD10 = SCCAU3;
         Chain(n) (kkICD10:'D') FMICD10L1;
         If Not %Found(FMICD10L1);
           *in61 = *on;
           *in90 = *on;
           leavesr;
         EndIf;
      /end-free
     C                   EndIf
     C*** END ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*                                                                LANKS
     C     SCSCHD        IFNE      *BLANK
     C                   MOVEL(P)  SCSCHD        KKKEY
     C                   MOVEL(P)  'FLSC'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C   99              SETON                                        9058
     C   99              GOTO      CK2END
     C                   ENDIF                                                  SCSCHD<>BLANK
     C****** BEG ADD ******* 09/25/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C     SCOLIC        IFNE      *BLANKS
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCOLC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9065
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCOLC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9065
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCOLC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCOLC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9065
     C                   GOTO      CK2END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C     SCOLC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9065
     C                   GOTO      CK2END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCOLC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9065
     C                   GOTO      CK2END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCOLCA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCOLCB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCOLCC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9066
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9066
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK2END
     C                   ENDIF
     C****** END ADD ******* 09/25/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   ENDIF                                                  SCSCHD<>BLANK
     C*
    C*** END ADD ***** 03/14/07 **************************************
     C*
     C*** BEG  ADD *** 03/11/10 **************************************
     C                   SETOFF                                       59
     C     EOREVI        IFGE      'E'
     C                   SETON                                        59
     C                   ENDIF
     C*** END  ADD *** 03/11/10 **************************************
     C*
     C*** BEG ADD ****** 11/03/15 ************************************
      /Free
         If SCICDIND = 'Y';

           //  Diag Codes

           If SCDIA5 <> *blanks;
              kkICD10 = SCDIA5;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS5 = 'D';
                   SCDSC5 = ICDSC1;
                   SCDS5  = ICFCODE;
                EndIf;
           EndIf;

           If SCDIA6 <> *blanks;
              kkICD10 = SCDIA6;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS6 = 'D';
                   SCDSC6 = ICDSC1;
                   SCDS6  = ICFCODE;
                EndIf;
           EndIf;

           If SCDIA7 <> *blanks;
              kkICD10 = SCDIA7;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS7 = 'D';
                   SCDSC7 = ICDSC1;
                   SCDS7  = ICFCODE;
                EndIf;
           EndIf;

           If SCDIA8 <> *blanks;
              kkICD10 = SCDIA8;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCLOS8 = 'D';
                   SCDSC8 = ICDSC1;
                   SCDS8  = ICFCODE;
                EndIf;
           EndIf;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 11/03/15 ************************************
     C     CK2END        ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C     CK3END        ENDSR
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
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #1 SEARCH
      /Free
          HLDIA_7 = SCDIA1;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa1 = HLDIA_7;
             *in90 = *on;
             *in36 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in37 = *on;
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
     C*** BEGIN CHG *** 10/10/03 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVE      'D'           XXLOST
     C*** END   CHG *** 10/10/03 **************************************
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
     C                   SETON                                        9037
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIA1
     C                   MOVEL     HLLOS1        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C                   SETON                                        9036
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVEL     SCDIA2        HLDIA2
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
      /Free
          HLDIA_7 = SCDIA2;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa2 = HLDIA_7;
             *in90 = *on;
             *in37 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in38 = *on;
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
     C*** BEGIN CHG *** 10/10/03 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVE      'D'           XXLOST
     C*** END   CHG *** 10/10/03 **************************************
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
     C                   SETON                                        9038
     C                   ELSE
     C                   MOVEL     HLDIA2        SCDIA2
     C                   MOVEL     HLLOS2        SCLOS2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC2
     C  N60              MOVEL     DIDS1         SCDS2
     C                   SETON                                        9037
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVEL     SCDIA3        HLDIA3
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #3 SEARCH
     C*
      /Free
          HLDIA_7 = SCDIA3;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa3 = HLDIA_7;
             *in90 = *on;
             *in38 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in35 = *on;
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
     C*** BEGIN CHG *** 10/10/03 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVE      'D'           XXLOST
     C*** END   CHG *** 10/10/03 **************************************
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
     C                   SETON                                        9035
     C                   ELSE
     C                   MOVEL     HLDIA3        SCDIA3
     C                   MOVEL     HLLOS3        SCLOS3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C  N60              MOVEL     DIDS1         SCDS3
     C                   SETON                                        9038
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVEL     SCDIA4        HLDIA4
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** DIAGNOSIS CODE #4 SEARCH
     C*
      /Free
          HLDIA_7 = SCDIA4;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa4 = HLDIA_7;
             *in90 = *on;
             *in35 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in34 = *on;
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
     C*** BEGIN CHG *** 10/10/03 **************************************
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVE      'D'           XXLOST
     C*** END   CHG *** 10/10/03 **************************************
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
     C                   SETON                                        9044
     C                   ELSE
    C*** BEG CHG ***** 03/14/07 **************************************
     C************         MOVELHLDIA3    SCDIA3
     C                   MOVEL     HLDIA4        SCDIA4
     C************         MOVELHLLOS3    SCLOS3
     C                   MOVEL     HLLOS4        SCLOS4
     C************         MOVELSCDIA3    XXDIA
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C************  N60                MOVELDIDSC1    SCDSC3
     C  N60              MOVEL     DIDSC1        SCDSC4
     C************  N60                MOVELDIDS1     SCDS3
     C  N60              MOVEL     DIDS1         SCDS4
    C*** END CHG ***** 03/14/07 **************************************
     C                   SETON                                        9035
     C                   ENDIF
     C*** BEG ADD ****** 08/13/15 ************************************
B01  C                   ENDIF
     C*** END ADD ****** 08/13/15 ************************************
     C*
    C*** BEG ADD ***** 03/14/07 **************************************
     C*
     C     HFLD          WHENEQ    'SCDIA5'
     C                   MOVEL     SCDIA5        HLDIA5
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #5 SEARCH
      /Free
          HLDIA_7 = SCDIA5;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa5 = HLDIA_7;
             *in90 = *on;
             *in44 = *on;
          Else;
             scDIa5 = outICD10;
             *in90 = *on;
             *in42 = *on;
          Endif;

          If scDia5 <> *blanks;
            kkICD10 = scDIa5;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS5 = 'D';
              SCDSC5 = ICDSC1;
              SCDS5  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCDIA5
     C                   MOVEL     XXLOST        SCLOS5
     C                   MOVEL     SCDIA5        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC5
     C  N60              MOVEL     DIDS1         SCDS5
     C                   SETON                                        9042
     C                   ELSE
     C                   MOVEL     HLDIA5        SCDIA5
     C                   MOVEL     HLLOS5        SCLOS5
     C                   MOVEL     SCDIA5        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC5
     C  N60              MOVEL     DIDS1         SCDS5
     C                   SETON                                        9044
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA6'
     C                   MOVEL     SCDIA6        HLDIA6
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #6 SEARCH
      /Free
          HLDIA_7 = SCDIA6;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa6 = HLDIA_7;
             *in90 = *on;
             *in42 = *on;
          Else;
             scDIa6 = outICD10;
             *in90 = *on;
             *in45 = *on;
          Endif;

          If scDia6 <> *blanks;
            kkICD10 = scDIa6;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS6 = 'D';
              SCDSC6 = ICDSC1;
              SCDS6  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCDIA6
     C                   MOVEL     XXLOST        SCLOS6
     C                   MOVEL     SCDIA6        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC6
     C  N60              MOVEL     DIDS1         SCDS6
     C                   SETON                                        9045
     C                   ELSE
     C                   MOVEL     HLDIA6        SCDIA6
     C                   MOVEL     HLLOS6        SCLOS6
     C                   MOVEL     SCDIA6        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC6
     C  N60              MOVEL     DIDS1         SCDS6
     C                   SETON                                        9042
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA7'
     C                   MOVEL     SCDIA7        HLDIA7
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #7 SEARCH
      /Free
          HLDIA_7 = SCDIA7;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa7 = HLDIA_7;
             *in90 = *on;
             *in45 = *on;
          Else;
             scDIa7 = outICD10;
             *in90 = *on;
             *in41 = *on;
          Endif;

          If scDia7 <> *blanks;
            kkICD10 = scDIa7;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS7 = 'D';
              SCDSC7 = ICDSC1;
              SCDS7  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCDIA7
     C                   MOVEL     XXLOST        SCLOS7
     C                   MOVEL     SCDIA7        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC7
     C  N60              MOVEL     DIDS1         SCDS7
     C                   SETON                                        9041
     C                   ELSE
     C                   MOVEL     HLDIA7        SCDIA7
     C                   MOVEL     HLLOS7        SCLOS7
     C                   MOVEL     SCDIA7        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC7
     C  N60              MOVEL     DIDS1         SCDS7
     C                   SETON                                        9045
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCDIA8'
     C                   MOVEL     SCDIA8        HLDIA8
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   IF        SCICDIND = 'Y'
     C
     C** DIAGNOSIS CODE #8 SEARCH
      /Free
          HLDIA_7 = SCDIA8;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa8 = HLDIA_7;
             *in90 = *on;
             *in41 = *on;
          Else;
             scDIa8 = outICD10;
             *in90 = *on;
             *in51 = *on;
          Endif;

          If scDia8 <> *blanks;
            kkICD10 = scDIa8;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLOS8 = 'D';
              SCDSC8 = ICDSC1;
              SCDS8  = ICFCODE;
            Endif;
          Endif;
      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCDIA8
     C                   MOVEL     XXLOST        SCLOS8
     C                   MOVEL     SCDIA8        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC8
     C  N60              MOVEL     DIDS1         SCDS8
     C                   SETON                                        9051
     C                   ELSE
     C                   MOVEL     HLDIA8        SCDIA8
     C                   MOVEL     HLLOS8        SCLOS8
     C                   MOVEL     SCDIA8        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC8
     C  N60              MOVEL     DIDS1         SCDS8
     C                   SETON                                        9041
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCPRC3'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9094
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9051
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC3        HLPRC3
     C                   MOVEL     SCP3DS        HLP3DS
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
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   SETON                                        9051
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC3
     C                   MOVEL(P)  ##DESC        SCP3DS
     C                   MOVE      *BLANKS       SCLS3
     C                   SETON                                        9052      POS CUR SCPRC4
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC3        HLPRC3            7
     C                   MOVE      SCLS3         HLLOS3
     C                   MOVEL     SCP3DS        HLP3DS           40
     C*** BEG ADD ****** 11/03/15 ************************************
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
             scPrc3 = HLPRC3;
             *in90 = *on;
             *in51 = *on;
          Else;
             scPrc3 = outICD10;
             *in90 = *on;
             *in52 = *on;
          Endif;

          If scPrc3 <> *blanks;
            kkICD10 = scPrc3;
            kkind = 'O';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLS3 = 'O';
              SCP3DS = ICDSC1;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL     HLLOS3        SCLS3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   SETON                                        9051
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC3
     C                   MOVE      ##LOST        SCLS3
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVEL     SCLS3         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9052
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCPRC4'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9097
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9052
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC4        HLPRC4
     C                   MOVEL     SCP4DS        HLP4DS
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
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   SETON                                        9052
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC4
     C                   MOVEL(P)  ##DESC        SCP4DS
     C                   MOVE      *BLANKS       SCLS3
     C                   SETON                                        9091      POS CUR SCPRC4
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC4        HLPRC4            7
     C                   MOVE      SCLS4         HLLOS4
     C                   MOVEL     SCP4DS        HLP4DS           40
     C*** BEG ADD ****** 11/03/15 ************************************
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
             scPrc4 = HLPRC4;
             *in90 = *on;
             *in52 = *on;
          Else;
             scPrc4 = outICD10;
             *in90 = *on;
             *in91 = *on;
          Endif;

          If scPrc4 <> *blanks;
            kkICD10 = scPrc4;
            kkind = 'O';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLS4 = 'O';
              SCP4DS = ICDSC1;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL     HLLOS4        SCLS4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   SETON                                        9052
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC4
     C                   MOVE      ##LOST        SCLS4
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVEL     SCLS4         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9091
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCPRC5'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9015
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9091
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC5        HLPRC5
     C                   MOVEL     SCP5DS        HLP5DS
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
     C                   MOVEL(P)  HLPRC5        SCPRC5
     C                   MOVEL(P)  HLP5DS        SCP5DS
     C                   SETON                                        9091
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC5
     C                   MOVEL(P)  ##DESC        SCP5DS
     C                   MOVE      *BLANKS       SCLS5
     C                   SETON                                        9092      POS CUR SCPRC4
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC5        HLPRC5            7
     C                   MOVE      SCLS5         HLLOS5
     C                   MOVEL     SCP5DS        HLP5DS           40
     C*** BEG ADD ****** 11/03/15 ************************************
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
             scPrc5 = HLPRC5;
             *in90 = *on;
             *in91 = *on;
          Else;
             scPrc5 = outICD10;
             *in90 = *on;
             *in92 = *on;
          Endif;

          If scPrc5 <> *blanks;
            kkICD10 = scPrc5;
            kkind = 'O';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLS5 = 'O';
              SCP5DS = ICDSC1;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC5        SCPRC5
     C                   MOVEL     HLLOS5        SCLS5
     C                   MOVEL(P)  HLP5DS        SCP5DS
     C                   SETON                                        9091
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC5
     C                   MOVE      ##LOST        SCLS5
     C                   MOVEL(P)  SCPRC5        KKICD9
     C                   MOVEL     SCLS5         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9092
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C     HFLD          WHENEQ    'SCPRC6'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9034
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9092
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC6        HLPRC6
     C                   MOVEL     SCP6DS        HLP6DS
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
     C                   MOVEL(P)  HLPRC6        SCPRC6
     C                   MOVEL(P)  HLP6DS        SCP6DS
     C                   SETON                                        9092
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC6
     C                   MOVEL(P)  ##DESC        SCP6DS
     C                   MOVE      *BLANKS       SCLS6
     C                   SETON                                        9043      POS CUR SCPRC4
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC6        HLPRC6            7
     C                   MOVE      SCLS6         HLLOS6
     C                   MOVEL     SCP6DS        HLP6DS           40
     C*** BEG ADD ****** 11/03/15 ************************************
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
             scPrc6 = HLPRC6;
             *in90 = *on;
             *in92 = *on;
          Else;
             scPrc6 = outICD10;
             *in90 = *on;
             *in43 = *on;
          Endif;

          If scPrc6 <> *blanks;
            kkICD10 = scPrc6;
            kkind = 'O';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
              SCLS6 = 'O';
              SCP6DS = ICDSC1;
            Endif;
          Endif;
      /End-free
     C                   Else
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC6        SCPRC6
     C                   MOVEL     HLLOS6        SCLS6
     C                   MOVEL(P)  HLP6DS        SCP6DS
     C                   SETON                                        9092
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC6
     C                   MOVE      ##LOST        SCLS6
     C                   MOVEL(P)  SCPRC6        KKICD9
     C                   MOVEL     SCLS6         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9043
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C*
     C     HFLD          WHENEQ    'SCCAU1'
     C                   MOVEL     SCCAU1        HLDIA5
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** EXTERNAL CAUSE #1 SEARCH
     C*
      /Free
          HLDIA_7 = SCCAU1;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scCAU1 = HLDIA_7;
             *in90 = *on;
             *in46 = *on;
          Else;
             scCAU1 = outICD10;
             *in90 = *on;
             *in43 = *on;
          Endif;

          If scCAU1 <> *blanks;
            kkICD10 = scCAU1;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
          //*** Beg Chg *** 12/02/15 ***************************************
              SCC1DS = 'D';
              //SCLOS1 = 'D';
          //*** End Chg *** 12/02/15 ***************************************
              SCCDS1 = ICDSC1;
              SCCD1  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCCAU1
     C                   MOVEL     XXLOST        SCC1DS
     C                   MOVEL     SCCAU1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCCDS1
     C  N60              MOVEL     DIDS1         SCCD1
     C                   SETON                                        9046
     C                   ELSE
     C                   MOVEL     HLDIA5        SCCAU1
     C                   MOVEL     HLLOS5        SCC1DS
     C                   MOVEL     SCCAU1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCCDS1
     C  N60              MOVEL     DIDS1         SCCD1
     C                   SETON                                        9043
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C*
     C     HFLD          WHENEQ    'SCCAU2'
     C                   MOVEL     SCCAU2        HLDIA5
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** EXTERNAL CAUSE #2 SEARCH
     C*
      /Free
          // **** Beg Chg *** 12/02/15 *************************************
          //HLDIA_7 = SCCAU1;

          HLDIA_7 = SCCAU2;
          // **** End Chg *** 12/02/15 *************************************
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scCAU2 = HLDIA_7;
             *in90 = *on;
             *in63 = *on;
          Else;
             scCAU2 = outICD10;
             *in90 = *on;
             *in46 = *on;
          Endif;

          If scCAU2 <> *blanks;
          // **** Beg Chg *** 12/02/15 *************************************
            //kkICD10 = scCAU1;

            kkICD10 = scCAU2;
          // **** End Chg *** 12/02/15 *************************************
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
          //*** Beg Chg *** 12/02/15 ***************************************
              SCC2DS = 'D';
              //SCLOS2 = 'D';
          //*** End Chg *** 12/02/15 ***************************************
              SCCDS2 = ICDSC1;
              SCCD2  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCCAU2
     C                   MOVEL     XXLOST        SCC2DS
     C                   MOVEL     SCCAU2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCCDS2
     C  N60              MOVEL     DIDS1         SCCD2
     C                   SETON                                        9063
     C                   ELSE
     C                   MOVEL     HLDIA5        SCCAU2
     C                   MOVEL     HLLOS5        SCC2DS
     C                   MOVEL     SCCAU2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCCDS2
     C  N60              MOVEL     DIDS1         SCCD2
     C                   SETON                                        9046
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C*
     C     HFLD          WHENEQ    'SCCAU3'
     C                   MOVEL     SCCAU3        HLDIA5
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   If        SCICDIND = 'Y'
     C*
     C** EXTERNAL CAUSE #3 SEARCH
     C*
      /Free
          HLDIA_7 = SCCAU3;
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scCAU3 = HLDIA_7;
             *in90 = *on;
             *in48 = *on;
          Else;
             scCAU3 = outICD10;
             *in90 = *on;
             *in63 = *on;
          Endif;

          If scCAU3 <> *blanks;
            kkICD10 = scCAU3;
            kkind = 'D';
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
          //*** Beg Chg *** 12/02/15 ***************************************
              SCC3DS = 'D';
              //SCLOS3 = 'D';
          //*** End Chg *** 12/02/15 ***************************************
              SCCDS3 = ICDSC1;
              SCCD3  = ICFCODE;
            Endif;
          Endif;

      /End-Free
     C                   ELSE
     C*** END ADD ****** 11/03/15 ************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      'D'           XXLOST
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
     C                   MOVEL     XXICD9        SCCAU3
     C                   MOVEL     XXLOST        SCC3DS
     C                   MOVEL     SCCAU3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCCDS3
     C  N60              MOVEL     DIDS1         SCCD3
     C                   SETON                                        9048
     C                   ELSE
     C                   MOVEL     HLDIA5        SCCAU3
     C                   MOVEL     HLLOS5        SCC3DS
     C                   MOVEL     SCCAU3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCCDS3
     C  N60              MOVEL     DIDS1         SCCD2
     C                   SETON                                        9063
     C                   ENDIF
     C*** BEG ADD ****** 11/03/15 ************************************
     C                   ENDIF
     C*** END ADD ****** 11/03/15 ************************************
     C*
     C*SCHEDULE/UNSCHEDULE INDICATOR
     C*
     C     HFLD          WHENEQ    'SCSCHD'
    C*
     C                   MOVEL(P)  'FLSC'        KKTBID
    C*
     C*          GTKY      CHAINPGT02                99
    C*
     C*          *IN99     IFEQ *OFF
    C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
    C*
     C     ##RRN         IFNE      *ZEROS
    C*
     C     ##RRN         CHAIN     PGT01                              99
    C*
     C     *IN99         IFEQ      *OFF
    C*
     C                   MOVEL     GTKEY         SCSCHD
     C                   MOVEL     GTDESC        SCSHDS
    C*
    C*** BEG CHG ***** 05/09/07 **************************************
     C*****************    SETON                     9048  POS CURSOR
     C*** BEG CHG *** 03/11/10 ****************************************
     C***********          SETON                     9049  POS CURSOR
     C                   SETON                                        9032      POS CURSOR
     C*** END CHG *** 03/11/10 ****************************************
     C*
     C                   ELSE                                                   *IN99<>0
     C                   SETON                                        9048      POS CURSOR
    C*** END CHG ***** 05/09/07 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
    C*
     C                   ELSE                                                   ##RRN<>0
    C*
     C                   SETON                                        9048      POS CURSOR
    C*
     C                   ENDIF                                                  ##RRN<>0
    C*
     C*                    ENDIF                           99 = OFF
     C*
    C*** END ADD ***** 03/14/07 **************************************
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
     C                   SETON                                        9050
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C********** SCPCM     OREQ '5'
     C                   SETON                                        9039
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C*** BEGIN DEL *** 08/19/03 **************************************
     C********** SCPCM     IFEQ '5'
     C**********           MOVELSCPRC1    HLPRC1
     C**********           MOVELSCP1DS    HLP1DS
     C**********           MOVE SCLOS1    HLLOS1
     C**********           Z-ADD0         ##RRN
     C**********           CALL 'F2310R'
     C**********           PARM '2'       ##CALL
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##RRN
     C*
     C********** ##KL      IFEQ '1'
     C********** ##RRN     OREQ *ZEROS
     C**********           MOVELHLPRC1    SCPRC1    P
     C**********           MOVELHLP1DS    SCP1DS    P
     C**********           MOVE HLLOS1    SCLOS1
     C**********           SETON                     9039
     C**********           ELSE
     C********** ##RRN     CHAINPRC01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVE *BLANKS   SCLOS1
     C**********           MOVELRCCPT4    SCPRC1    P
     C**********           MOVELRCDSC1    SCP1DS    P
     C**********           ENDIF                           *IN99 = *OFF
     C**********           SETON                     9040
     C**********           ENDIF                           ##KL = '1'
     C**********           ENDIF                           SCPCM = '5'
     C*** END   DEL *** 08/19/03 **************************************
     C*
     C     SCPCM         IFEQ      '4'
     C*** BEGIN ADD *** 08/19/03 **************************************
     C     SCPCM         OREQ      '5'
     C*** END   ADD *** 08/19/03 **************************************
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
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE *BLANKS   SCLOS1
     C                   MOVE      *BLANKS       SCLS1
     C*** END   CHG *** 08/19/03 **************************************
     C                   SETON                                        9040
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC1        HLPRC1            7
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVELSCLOS1    HLLOS1
     C                   MOVEL     SCLS1         HLLOS1
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVEL     SCP1DS        HLP1DS           40
     C*** BEG ADD ****** 08/13/15 ************************************
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
             *in40 = *on;
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
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C*** BEGIN CHG *** 10/10/03 **************************************
     C**********           PARM ' '       ##LOST
     C                   PARM      'O'           ##LOST
     C*** END   CHG *** 10/10/03 **************************************
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVELHLLOS1    SCLOS1
     C                   MOVEL     HLLOS1        SCLS1
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC1
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE ##LOST    SCLOS1
     C                   MOVE      ##LOST        SCLS1
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS1    KKLOST
     C                   MOVE      SCLS1         KKLOST
     C*** END   CHG *** 08/19/03 **************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9040
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   EndIf                                                  ##ICD9 = *BLANKS
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC2'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9053
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C********** SCPCM     OREQ '5'
     C                   SETON                                        9040
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C*** BEGIN DEL *** 08/19/03 **************************************
     C********** SCPCM     IFEQ '5'
     C**********           MOVELSCPRC2    HLPRC2
     C**********           MOVELSCP2DS    HLP2DS
     C**********           MOVE SCLOS2    HLLOS2
     C**********           Z-ADD0         ##RRN
     C**********           CALL 'F2310R'
     C**********           PARM '2'       ##CALL
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##RRN
     C**********
     C********** ##KL      IFEQ '1'
     C********** ##RRN     OREQ *ZEROS
     C**********           MOVELHLPRC2    SCPRC2    P
     C**********           MOVELHLP2DS    SCP2DS    P
     C**********           MOVE HLLOS2    SCLOS2
     C**********           SETON                     9040
     C**********           ELSE
     C********** ##RRN     CHAINPRC01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVE *BLANKS   SCLOS2
     C**********           MOVELRCCPT4    SCPRC2    P
     C**********           MOVELRCDSC1    SCP2DS    P
     C**********           ENDIF                           *IN99 = *OFF
     C**********           SETON                     9028
     C**********           ENDIF                           ##KL = '1'
     C**********           ENDIF                           SCPCM = '5'
     C*** END   DEL *** 08/19/03 **************************************
     C*
     C     SCPCM         IFEQ      '4'
     C*** BEGIN ADD *** 08/19/03 **************************************
     C     SCPCM         OREQ      '5'
     C*** END   ADD *** 08/19/03 **************************************
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
     C                   SETON                                        9040
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC2
     C                   MOVEL(P)  ##DESC        SCP2DS
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE *BLANKS   SCLOS2
     C                   MOVE      *BLANKS       SCLS2
     C*** END   CHG *** 08/19/03 **************************************
     C                   SETON                                        9028
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC2        HLPRC2            7
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS2    HLLOS2
     C                   MOVE      SCLS2         HLLOS2
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVEL     SCP2DS        HLP2DS           40
     C*** BEG ADD ****** 08/13/15 ************************************
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
             *in40 = *on;
          Else;
             scPrc2 = outICD10;
             *in90 = *on;
             *in28 = *on;
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
     C*** END ADD ****** 08/13/15 ************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C*** BEGIN CHG *** 10/10/03 **************************************
     C**********           PARM ' '       ##LOST
     C                   PARM      'O'           ##LOST
     C*** END   CHG *** 10/10/03 **************************************
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVELHLLOS2    SCLOS2
     C                   MOVEL     HLLOS2        SCLS2
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9040
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC2
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE ##LOST    SCLOS2
     C                   MOVE      ##LOST        SCLS2
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVEL(P)  SCPRC2        KKICD9
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS2    KKLOST
     C                   MOVEL     SCLS2         KKLOST
     C*** END   CHG *** 08/19/03 **************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9028
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** BEG ADD ****** 08/13/15 ************************************
     C                   EndIf
     C*** END ADD ****** 08/13/15 ************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*** BEG DEL *** 09/20/00 ****************************************
     C*******    HFLD      WHEQ 'SCFLIC'
     C*******              SETON                     9073
     C*******              SETOF                     7466
     C*** END DEL *** 09/20/00 ****************************************
     C*** BEG ADD *** 09/20/00 ****************************************
     C     HFLD          WHENEQ    'SCFLIC'
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCFLIC    HLFLIC  9
     C                   MOVEL(P)  SCFLIC        HLFLIC           13
     C*** END CHG *** 04/28/03 ****************************************
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** PARM           ##FLIC  9
     C                   PARM                    ##FLIC           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   SETON                                        9028
     C                   SETOFF                                       38
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELHLFLIC    SCFLIC  9
     C                   MOVEL(P)  HLFLIC        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   ELSE
     C     ##FLIC        IFEQ      *BLANKS
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELHLFLIC    SCFLIC
     C                   MOVEL(P)  HLFLIC        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   SETON                                        9028
     C                   ELSE
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVEL##FLIC    SCFLIC
     C                   MOVEL(P)  ##FLIC        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C                   SETON                                        9031
     C                   SETOFF                                       28
     C                   ENDIF                                                  ##FLIC=*BLAN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C                   SETON                                        9028
     C                   ENDIF                                                  *IN60=*OFF
     C*** END ADD *** 09/20/00 ****************************************
     C*
     C     HFLD          WHENEQ    'SCTAPE'
     C*** BEG DEL *** 04/15/04 ****************************************
     C*******************  SETON                     9074
     C*******************  SETOF                     7366
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
    C*** BEG DEL *** 03/14/07 ****************************************
     C***************      SETON                     3190
    C*** END DEL *** 03/14/07 ****************************************
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'FLES'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTAPE
     C                   MOVEL(P)  GTDESC        SCDSDS
    C*** BEG ADD *** 03/14/07 ****************************************
     C                   SETON                                        9033
    C*** END ADD *** 03/14/07 ****************************************
     C                   ENDIF
    C*** BEG ADD *** 03/14/07 ****************************************
     C                   ELSE
     C                   SETON                                        9031
    C*** END ADD *** 03/14/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C     HFLD          WHENEQ    'SCRCVD'
     C                   SETON                                        9034
     C*
     C                   ENDSL
     C*
     C*** BEG ADD *** 08/08/08 ****************************************
     C                   SETON                                        90
     C*** END ADD *** 08/08/08 ****************************************
     C*
     C                   ENDSR
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
     C*** BEGIN CHG *** 10/10/03 **************************************
     C********** ESKY      REDPEPES01                    60
     C     ESKY          READPE(N) PES01                                  60
     C*** END   CHG *** 10/10/03 **************************************
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ESRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 04/11/00 ****************************************
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C******** BEGIN ADD **********  09/10/07 *************************
     C                   MOVEL     EOFLIC        SCSVZP
     C******** END ADD ************  09/10/07 *************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL(P)  EOBNPI        SCBNPI
     C                   MOVEL(P)  EUFALC        SCFALC
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C                   MOVE      EUEDI         SCTAPE
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVE EULIC#    SCFLIC
     C                   MOVEL(P)  EULIC#        SCFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EUFLIC        SCFLIC
     C                   ENDIF
     C*** END ADD *** 04/28/03 ****************************************
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
     C     EUDIA2        IFNE      *BLANKS
     C                   MOVEL     EUDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDS1         SCDS2
     C                   MOVEL     EUDIA2        SCDIA2
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCLOS2
     C                   MOVE      *BLANKS       SCDS2
     C                   MOVE      *BLANKS       SCDIA2
     C                   ENDIF
     C*
     C     EUDIA3        IFNE      *BLANKS
     C                   MOVEL     EUDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDS1         SCDS3
     C                   MOVEL     EUDIA3        SCDIA3
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC3
     C                   MOVE      *BLANKS       SCLOS3
     C                   MOVE      *BLANKS       SCDS3
     C                   MOVE      *BLANKS       SCDIA3
     C                   ENDIF
     C*
     C     EUDIA4        IFNE      *BLANKS
     C                   MOVEL     EUDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDS1         SCDS4
     C                   MOVEL     EUDIA4        SCDIA4
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC4
     C                   MOVE      *BLANKS       SCLOS4
     C                   MOVE      *BLANKS       SCDS4
     C                   MOVE      *BLANKS       SCDIA4
     C                   ENDIF
     C*
    C*** BEG ADD ***** 03/14/07 **************************************
     C*
     C     EUDIA5        IFNE      *BLANKS
     C                   MOVEL     EUDIA5        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC5
     C                   MOVEL     DILOST        SCLOS5
     C                   MOVEL     DIDS1         SCDS5
     C                   MOVEL     EUDIA5        SCDIA5
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC5
     C                   MOVE      *BLANKS       SCLOS5
     C                   MOVE      *BLANKS       SCDS5
     C                   MOVE      *BLANKS       SCDIA5
     C                   ENDIF
     C*
     C     EUDIA6        IFNE      *BLANKS
     C                   MOVEL     EUDIA6        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC6
     C                   MOVEL     DILOST        SCLOS6
     C                   MOVEL     DIDS1         SCDS6
     C                   MOVEL     EUDIA6        SCDIA6
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC6
     C                   MOVE      *BLANKS       SCLOS6
     C                   MOVE      *BLANKS       SCDS6
     C                   MOVE      *BLANKS       SCDIA6
     C                   ENDIF
     C*
     C     EUDIA7        IFNE      *BLANKS
     C                   MOVEL     EUDIA7        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC7
     C                   MOVEL     DILOST        SCLOS7
     C                   MOVEL     DIDS1         SCDS7
     C                   MOVEL     EUDIA7        SCDIA7
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC7
     C                   MOVE      *BLANKS       SCLOS7
     C                   MOVE      *BLANKS       SCDS7
     C                   MOVE      *BLANKS       SCDIA7
     C                   ENDIF
     C*
     C     EUDIA8        IFNE      *BLANKS
     C                   MOVEL     EUDIA8        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC8
     C                   MOVEL     DILOST        SCLOS8
     C                   MOVEL     DIDS1         SCDS8
     C                   MOVEL     EUDIA8        SCDIA8
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC8
     C                   MOVE      *BLANKS       SCLOS8
     C                   MOVE      *BLANKS       SCDS8
     C                   MOVE      *BLANKS       SCDIA8
     C                   ENDIF
     C     EUCAU1        IFNE      *BLANKS
     C                   MOVEL     EUCAU1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCCDS1
     C                   MOVEL     DILOST        SCC1DS
     C                   MOVEL     DIDS1         SCCD1
     C                   MOVEL     EUCAU1        SCCAU1
     C                   ELSE
     C                   MOVE      *BLANKS       SCCDS1
     C                   MOVE      *BLANKS       SCC1DS
     C                   MOVE      *BLANKS       SCCD1
     C                   MOVE      *BLANKS       SCCAU1
     C                   ENDIF
     C*
     C     EUCAU2        IFNE      *BLANKS
     C                   MOVEL     EUCAU2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCCDS2
     C                   MOVEL     DILOST        SCC2DS
     C                   MOVEL     DIDS1         SCCD2
     C                   MOVEL     EUCAU2        SCCAU2
     C                   ELSE
     C                   MOVE      *BLANKS       SCCDS2
     C                   MOVE      *BLANKS       SCC2DS
     C                   MOVE      *BLANKS       SCCD2
     C                   MOVE      *BLANKS       SCCAU2
     C                   ENDIF
     C*
     C     EUCAU3        IFNE      *BLANKS
     C                   MOVEL     EUCAU3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCCDS3
     C                   MOVEL     DILOST        SCC3DS
     C                   MOVEL     DIDS1         SCCD3
     C                   MOVEL     EUCAU3        SCCAU3
     C                   ELSE
     C                   MOVE      *BLANKS       SCCDS3
     C                   MOVE      *BLANKS       SCC3DS
     C                   MOVE      *BLANKS       SCCD3
     C                   MOVE      *BLANKS       SCCAU3
     C                   ENDIF
     C*
     C     EUSCHD        IFNE      *BLANKS
     C                   MOVEL(P)  EUSCHD        KKKEY
     C                   MOVEL(P)  'FLSC'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     GTKEY         SCSCHD
     C                   MOVEL     GTDESC        SCSHDS
     C                   ENDIF                                                  *IN99 = '0'
     C                   ENDIF                                                  EUSCHD <>  ' '
     C*
    C*** BEG ADD ***** 05/09/07 **************************************
     C*
     C                   MOVEL     EUOLIC        SCOLIC
     C*
    C*** END ADD ***** 05/09/07 **************************************
    C*** END ADD ***** 03/14/07 **************************************
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
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE EUPRC1    SCLOS1
     C                   MOVE      EUPRC1        SCLS1
     C*** END   CHG *** 08/19/03 **************************************
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
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS1    KKLOST
     C                   MOVE      SCLS1         KKLOST
     C*** END   CHG *** 08/19/03 **************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC2        SCPRC2
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE EUPRC2    SCLOS2
     C                   MOVE      EUPRC2        SCLS2
     C*** END   CHG *** 08/19/03 **************************************
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
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVE SCLOS2    KKLOST
     C                   MOVE      SCLS2         KKLOST
     C*** END   CHG *** 08/19/03 **************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC3        SCPRC3
     C                   MOVE      EUPRC3        SCLS3
     C     SCPCM         IFEQ      '4'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLS3         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C                   MOVEL     EUPRC4        SCPRC4
     C                   MOVE      EUPRC4        SCLS4
     C     SCPCM         IFEQ      '4'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLS4         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C                   MOVEL     EUPRC5        SCPRC5
     C                   MOVE      EUPRC5        SCLS5
     C     SCPCM         IFEQ      '4'
     C     SCPRC5        ANDNE     *BLANKS
     C                   MOVEL     SCPRC5        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC5        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC5        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC5        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC5        KKICD9
     C                   MOVE      SCLS5         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C                   MOVEL     EUPRC6        SCPRC6
     C                   MOVE      EUPRC6        SCLS6
     C     SCPCM         IFEQ      '4'
     C     SCPRC6        ANDNE     *BLANKS
     C                   MOVEL     SCPRC6        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC6        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC6        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC6        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC6        KKICD9
     C                   MOVE      SCLS6         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*** BEGIN ADD *** 10/10/03 **************************************
     C                   SETOFF                                       16
     C     EUADDT        IFEQ      *ZEROS
     C                   SETON                                        16
     C                   ELSE
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADDT
     C                   ENDIF
     C*
     C                   SETOFF                                       17
     C     EUSCPF        IFEQ      *ZEROS
     C                   SETON                                        17
     C                   ELSE
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCPF
     C                   ENDIF
     C*
     C                   SETOFF                                       18
     C     EUSCPT        IFEQ      *ZEROS
     C                   SETON                                        18
     C                   ELSE
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCPT
     C                   ENDIF
     C*** END   ADD *** 10/10/03 **************************************
     C*** BEG  ADD *** 03/11/10 **************************************
     C                   SETOFF                                       59
     C     EOREVI        IFGE      'E'
     C                   SETON                                        59
     C                   ENDIF
     C*** END  ADD *** 03/11/10 **************************************
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
         scICDind = EUicdInd;
         If SCICDIND = 'Y';

           // Diag Codes

           If EUICD1001 <> *blanks;
              SCDIA1 = EUICD1001;
              kkICD10 = SCDIA1;

              Chain (kkICD10:'D') FMICD10L1;
              If %Found(FMICD10L1);
                SCDSC1 = ICDSC1;
                SCDS1 = ICFCODE;
                SCLOS1 = 'D';
              EndIf;
           EndIf;

           If EUICD1002 <> *blanks;
              SCDIA2 = EUICD1002;
              kkICD10 = SCDIA2;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC2 = ICDSC1;
                   SCDS2 = ICFCODE;
                   SCLOS2 = 'D';
                EndIf;
           EndIf;

           If EUICD1003 <> *blanks;
              SCDIA3 = EUICD1003;
              kkICD10 = SCDIA3;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC3 = ICDSC1;
                   SCDS3 = ICFCODE;
                   SCLOS3 = 'D';
                EndIf;
           EndIf;

           If EUICD1004 <> *blanks;
              SCDIA4 = EUICD1004;
              kkICD10 = SCDIA4;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC4 = ICDSC1;
                   SCDS4 = ICFCODE;
                   SCLOS4 = 'D';
                EndIf;
           EndIf;

         //*** Beg Add *** 12/02/15 **************************************
           If EUICD1005 <> *blanks;
              SCDIA5 = EUICD1005;
              kkICD10 = SCDIA5;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC5 = ICDSC1;
                   SCDS5 = ICFCODE;
                   SCLOS5 = 'D';
                EndIf;
           EndIf;
           If EUICD1006 <> *blanks;
              SCDIA6 = EUICD1006;
              kkICD10 = SCDIA6;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC6 = ICDSC1;
                   SCDS6 = ICFCODE;
                   SCLOS6 = 'D';
                EndIf;
           EndIf;
           If EUICD1007 <> *blanks;
              SCDIA7 = EUICD1007;
              kkICD10 = SCDIA7;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC7 = ICDSC1;
                   SCDS7 = ICFCODE;
                   SCLOS7 = 'D';
                EndIf;
           EndIf;
           If EUICD1008 <> *blanks;
              SCDIA8 = EUICD1008;
              kkICD10 = SCDIA8;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCDSC8 = ICDSC1;
                   SCDS8 = ICFCODE;
                   SCLOS8 = 'D';
                EndIf;
           EndIf;

           // External Cause Codes

           If EUCAU1001 <> *blanks;
              SCCAU1 = EUCAU1001;
              kkICD10 = SCCAU1;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCCDS1 = ICDSC1;
                   SCCD1 = ICFCODE;
                   SCC1DS = 'D';
                EndIf;
           EndIf;

           If EUCAU1002 <> *blanks;
              SCCAU2 = EUCAU1002;
              kkICD10 = SCCAU2;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCCDS2 = ICDSC1;
                   SCCD2 = ICFCODE;
                   SCC2DS = 'D';
                EndIf;
           EndIf;

           If EUCAU1003 <> *blanks;
              SCCAU3 = EUCAU1003;
              kkICD10 = SCCAU3;

              Chain (kkICD10:'D') FMICD10L1;
                If %Found(FMICD10L1);
                   SCCDS3 = ICDSC1;
                   SCCD3 = ICFCODE;
                   SCC3DS = 'D';
                EndIf;
           EndIf;

         //*** End Add *** 12/02/15 **************************************

           If SCPCM = '9';

              If EUPRC1001 <> *blanks;
                 SCPRC1  = EUPRC1001;
                 kkICD10 = SCPRC1;

                 Chain (kkICD10:'O') FMICD10L1;
                 If %Found(FMICD10L1);
         //*** Beg Add *** 12/02/15 **************************************
                   SCLS1 = 'O';
         //*** End Add *** 12/02/15 **************************************
                   SCP1DS = ICDSC1;
                 EndIf;
              EndIf;

              If EUPRC1002 <> *blanks;
                 SCPRC2  = EUPRC1002;
                 kkICD10 = SCPRC2;

                 Chain (kkICD10:'O') FMICD10L1;
                 If %Found(FMICD10L1);
         //*** Beg Add *** 12/02/15 **************************************
                   SCLS2 = 'O';
         //*** End Add *** 12/02/15 **************************************
                   SCP2DS = ICDSC1;
                 EndIf;
              EndIf;

           //* Beg Add ****** 11/03/15 ***********************************  *

         //*** Beg Dlt *** 12/02/15 **************************************
         //*|  If EUICD1005 <> *blanks;
         //*|     SCDIA5 = EUICD1005;
         //*|     kkICD10 = SCDIA5;
         //*|
         //*|     Chain (kkICD10:'D') FMICD10L1;
         //*|       If %Found(FMICD10L1);
         //*|          SCDSC5 = ICDSC1;
         //*|          SCDS5 = ICFCODE;
         //*|          SCLOS5 = 'D';
         //*|       EndIf;
         //*|  EndIf;
         //*|  If EUICD1006 <> *blanks;
         //*|     SCDIA6 = EUICD1006;
         //*|     kkICD10 = SCDIA6;
         //*|
         //*|     Chain (kkICD10:'D') FMICD10L1;
         //*|       If %Found(FMICD10L1);
         //*|          SCDSC6 = ICDSC1;
         //*|          SCDS6 = ICFCODE;
         //*|          SCLOS6 = 'D';
         //*|       EndIf;
         //*|  EndIf;
         //*|  If EUICD1007 <> *blanks;
         //*|     SCDIA7 = EUICD1007;
         //*|     kkICD10 = SCDIA7;
         //*|
         //*|     Chain (kkICD10:'D') FMICD10L1;
         //*|       If %Found(FMICD10L1);
         //*|          SCDSC7 = ICDSC1;
         //*|          SCDS7 = ICFCODE;
         //*|          SCLOS7 = 'D';
         //*|       EndIf;
         //*|  EndIf;
         //*|  If EUICD1008 <> *blanks;
         //*|     SCDIA8 = EUICD1008;
         //*|     kkICD10 = SCDIA8;
         //*|
         //*|     Chain (kkICD10:'D') FMICD10L1;
         //*|       If %Found(FMICD10L1);
         //*|          SCDSC8 = ICDSC1;
         //*|          SCDS8 = ICFCODE;
         //*|          SCLOS8 = 'D';
         //*|       EndIf;
         //*|  EndIf;
         //*** End Dlt *** 12/02/15 **************************************

             //*** Beg Chg *** 12/02/15 ************************************
              //If EUPRC1002 <> *blanks;

              If EUPRC1003 <> *blanks;
             //*** End Chg *** 12/02/15 ************************************
                 SCPRC3  = EUPRC1003;
                 kkICD10 = SCPRC3;

                 Chain (kkICD10:'O') FMICD10L1;
                 If %Found(FMICD10L1);
         //*** Beg Add *** 12/02/15 **************************************
                   SCLS3 = 'O';
         //*** End Add *** 12/02/15 **************************************
                   SCP3DS = ICDSC1;
                 EndIf;
              EndIf;
              If EUPRC1004 <> *blanks;
                 SCPRC4  = EUPRC1004;
                 kkICD10 = SCPRC4;

                 Chain (kkICD10:'O') FMICD10L1;
                 If %Found(FMICD10L1);
         //*** Beg Add *** 12/02/15 **************************************
                   SCLS4 = 'O';
         //*** End Add *** 12/02/15 **************************************
                   SCP4DS = ICDSC1;
                 EndIf;
              EndIf;
              If EUPRC1005 <> *blanks;
                 SCPRC5  = EUPRC1005;
                 kkICD10 = SCPRC5;

                 Chain (kkICD10:'O') FMICD10L1;
                 If %Found(FMICD10L1);
         //*** Beg Add *** 12/02/15 **************************************
                   SCLS5 = 'O';
         //*** End Add *** 12/02/15 **************************************
                   SCP5DS = ICDSC1;
                 EndIf;
              EndIf;
              If EUPRC1006 <> *blanks;
                 SCPRC6  = EUPRC1006;
                 kkICD10 = SCPRC6;

                 Chain (kkICD10:'O') FMICD10L1;
                 If %Found(FMICD10L1);
         //*** Beg Add *** 12/02/15 **************************************
                   SCLS6 = 'O';
         //*** End Add *** 12/02/15 **************************************
                   SCP6DS = ICDSC1;
                 EndIf;
              EndIf;
         //* end add ****** 11/03/15 ************************************

           EndIf;

E01      EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      EUDIA1 = SCDIA1
     C                   EVAL      EUDIA2 = SCDIA2
     C                   EVAL      EUDIA3 = SCDIA3
     C                   EVAL      EUDIA4 = SCDIA4
     C*** BEG CHG ****** 11/03/15 ************************************
     C                   EVAL      EUDIA5 = SCDIA5
     C                   EVAL      EUDIA6 = SCDIA6
     C                   EVAL      EUDIA7 = SCDIA7
     C                   EVAL      EUDIA8 = SCDIA8
     C*** END CHG ****** 11/03/15 ************************************
     C*** BEG CHG ****** 08/13/15 ************************************
     C****               MOVEL     SCDIA1        EUDIA1
     C****               MOVEL     SCDIA2        EUDIA2
     C****               MOVEL     SCDIA3        EUDIA3
     C****               MOVEL     SCDIA4        EUDIA4
     C*** END CHG ****** 08/13/15 ************************************
     C*** BEG ADD ****** 08/13/15 ************************************
      /Free
        EUicdInd = scICDind;
        If  SCICDIND = 'Y';

          EUICD1001 = SCDIA1;
          EUICD1002 = SCDIA2;
          EUICD1003 = SCDIA3;
          EUICD1004 = SCDIA4;
         //* Beg Add ****** 11/03/15 ************************************
          EUICD1005 = SCDIA5;
          EUICD1006 = SCDIA6;
          EUICD1007 = SCDIA7;
          EUICD1008 = SCDIA8;
         //* End Add ****** 11/03/15 ********************************* ***
          EUPRC1001 = SCPRC1;
          EUPRC1002 = SCPRC2;
         //**Beg Add ****** 11/03/15 ************************************
          EUPRC1003 = SCPRC3;
          EUPRC1004 = SCPRC4;
          EUPRC1005 = SCPRC5;
         //**Beg Add ****** 12/02/15 ************************************
          EUPRC1006 = SCPRC6;
          EUCAU1001 = SCCAU1;
          EUCAU1002 = SCCAU2;
          EUCAU1003 = SCCAU3;
         //**End Add ****** 12/02/15 ************************************
         //**End Add ****** 11/03/15 ************************************

E01     EndIf;
      /End-Free
     C*** END ADD ****** 08/13/15 ************************************
    C*** BEG ADD ***** 03/14/07 **************************************
     C***** BEG DLT *** 12/02/15 **************************************
     C*** BEG CHG ****** 11/03/15 ************************************
     C*****              Eval      SCDIA5 = EUDIA5
     C*****              Eval      SCDIA6 = EUDIA6
     C*****              Eval      SCDIA7 = EUDIA7
     C*****              Eval      SCDIA8 = EUDIA8
     C*** End CHG ****** 11/03/15 ************************************
     C***** END DLT *** 12/02/15 **************************************
     C***** BEG DLT *** 12/02/15 **************************************
     C***** These were changed to EVAL above

     C*****              MOVEL     SCDIA5        EUDIA5
     C*****              MOVEL     SCDIA6        EUDIA6
     C*****              MOVEL     SCDIA7        EUDIA7
     C*****              MOVEL     SCDIA8        EUDIA8
     C***** END DLT *** 12/02/15 **************************************
     C                   MOVEL     SCPCM         EUPCM
     C*** BEG CHG ****** 08/13/15 ************************************
     C                   EVAL      EUPRC1 = SCPRC1
     C                   EVAL      EUPRC2 = SCPRC2
     c*** BEG ADD ****** 11/03/15 ************************************
     C                   EVAL      EUPRC3 = SCPRC3
     C                   EVAL      EUPRC4 = SCPRC4
     C                   EVAL      EUPRC5 = SCPRC5
     c*** END ADD ****** 11/03/15 ************************************
     c*** BEG ADD ****** 12/02/15 ************************************
     C                   EVAL      EUPRC6 = SCPRC6
     c*** END ADD ****** 12/02/15 ****************c*******************
     C****               MOVEL     SCPRC1        EUPRC1
     C****               MOVEL     SCPRC2        EUPRC2
     C*** END CHG ****** 08/13/15 ************************************
     C*** BEG DLT ****** 12/02/15 ************************************
     C***** These were changed to EVAL above

     C*****              MOVEL     SCPRC3        EUPRC3
     C*****              MOVEL     SCPRC4        EUPRC4
     C*****              MOVEL     SCPRC5        EUPRC5
     C*****              MOVEL     SCPRC6        EUPRC6
     C*** END DLT ****** 12/02/15 ************************************
     C*** BEG CHG ****** 12/02/15 ************************************
     C                   EVAL      EUCAU1 = SCCAU1
     C                   EVAL      EUCAU2 = SCCAU2
     C                   EVAL      EUCAU3 = SCCAU3
     C*
     C*****              MOVEL     SCCAU1        EUCAU1
     C*****              MOVEL     SCCAU2        EUCAU2
     C*****              MOVEL     SCCAU3        EUCAU3
     C*** END CHG ****** 12/02/15 ************************************
     C                   MOVEL     SCSCHD        EUSCHD
    C*** END ADD ***** 03/14/07 **************************************
    C*** BEG ADD ***** 05/09/07 **************************************
     C                   MOVEL     SCOLIC        EUOLIC
    C*** END ADD ***** 05/09/07 **************************************
     C*
     C*** BEG ADD *** 04/28/03 ****************************************
     C******************** MOVELSCFLIC    EULIC#
     C                   MOVEL     SCFLIC        EUFLIC
     C*** END ADD *** 04/28/03 ****************************************
     C                   MOVEL     SCTAPE        EUEDI
     C                   MOVEL     SCTAPE        EOTAPE
     C************ BEGIN ADD ********* 09/10/07 *********************
     C                   MOVE      SCSVZP        EOFLI5
      ************ END ADD *********** 09/10/07 *********************
     C     EAKEY         CHAIN     PEA01                              60
     C                   MOVEL     SCTAPE        EAEDI
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
     C                   ENDIF
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEGIN ADD *** 10/10/03 **************************************
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUADDT
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUSCPF
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUSCPT
     C*** END   ADD *** 10/10/03 **************************************
     C*
     C                   MOVE      Q1USER        EUUSER
     C                   MOVE      Q1PGM         EUPGM
     C                   MOVE      DSTIME        EUTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C*
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL(P)  SCBNPI        EOBNPI
     C                   MOVEL(P)  SCFALC        EUFALC
     C*** END ADD *** 03/11/10 ****************************************
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
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   SETOFF                                       31
     C*** END ADD *** 04/15/04 ****************************************
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
     C*** BEGIN ADD *** 10/10/03 **************************************
     C*
     C* Call API for additional system control information
     C*
     C                   CALL      'SA009R'
 1   C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
 2   C                   PARM      ' '           ##EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM      ' '           ##ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM      ' '           ##ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM      ' '           ##PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM      ' '           ##SDTS            1            ALLOW FUTURE SERV DT
 7   C                   PARM      ' '           ##MCIV            1            MED CASES INIT VIEW
 8   C                   PARM      ' '           ##PBIV            1            PROV BILLS ININT VIE
 9   C                   PARM      ' '           ##AINV            1            AUTO INVOICE FLAG
10   C                   PARM      ' '           ##WHPG            1            Work History Usage
11   C                   PARM      ' '           ##PRIV            1            PROVIDER INIT VIEW
12   C                   PARM      ' '           ##TDUP            1            ADJUDICATION DUP CRI
13   C                   PARM      ' '           ##NWSL            1            ADJUDICATION DUP CRI
     C*** END   ADD *** 10/10/03 **************************************
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
     C     ESKY          CHAIN     PES01                              59
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
     C                   FEOD      FAEUP
     C                   ENDSR
**  TABLE @A  - ALPHABET
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
