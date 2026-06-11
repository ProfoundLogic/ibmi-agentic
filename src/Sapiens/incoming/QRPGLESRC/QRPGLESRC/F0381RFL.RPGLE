     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0381R                                              *
     H*  Description: Dental Header Info update/display               *
     H*  Created: 06/21/99  C:117 Project: MC152                      *
     H*****************************************************************
     H*  REVISIONS
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  06/27/19 C:079 Project: 152MAP                               *
     H*                 Relax edits if Pbills lite is installed       *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  06/03/15 C:079 PROJECT: MC329 Phase III                      *
     H*                 Validate specific indicator for ICD10 codes   *
     H*
     H*  09/08/14 C:280 PROJECT: 147MDC                               *
     H*                 Add new field dispecific and validate         *
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Include rendering information for CSI.        *
     H*                                                               *
     H*  11/19/12 C:287 Project: MC367
     H*                 Disable function code F11 for GS access to Bill Entry
     H*                                                               *
     H*   04/05/11 C:257 Support 71931                                *
     H*                  Assigning RRN to @@RRN after WRITEDBRCDF to  *
     H*                  resolve duplicate records in file FAEHP.     *
     H*
     H*   07/12/10 C:079 Support: 73533                               *
     H*                  Do not validate license prefix in this pgm   *
     H*
     H*   03/11/10 C:227 PROJECT: FL083                               *
     H*              220 Added Service date in parameter of API.      *
     H*                                                               *
     H*   10/04/08 C:180 SUPPORT 65159
     H*                  Fourth position of FL Phy lic can be blank
     H*
     H*   03/20/08 C:198                                              *
     H*                  Project: FS297                               *
     H*                  MDS to reject selected prefixes for specialt-*
     H*                  -ies not associate with WC - Scope change    *
     H*                                                               *
     H* 12/10/07 C:198 Project: FL064                                 *
     H*                Expanded license# field to 13 characters.      *
     H*                                                               *
     H*  08/29/07  C:180  SUPP 59201
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
     H*  04/20/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*                         
     H* 03/21/07  C:182  Project: FS297
     H*                  Set EDI flag = N if license# is invali
     H*                         
     H* 03/14/07  C:182  Project: FL067
     H*                  1. Changed Fld Heading for 'Payment plan'
     H*                     to be 'Payment code'.
     H*                         
     H* 12/22/06 C:182 PROJECT: FS294
     H*                ADDED SEARCH OPTION TO FIELD SCEDI
     H*
     H*  11/08/05 C:163 PROJECT: 48MWK
     H*                 FL EDI CORRECTION LICENSE #
     H*
     H*  03/25/05 C:079 PROJECT: FL050
     H*                 1. CHANGE EDIT ON LICENSE NUMBER
     H*
     H*  04/15/04 C:079 PROJECT: FL046
     H*                 1. CHANGE EDIT ON ELIGIBLE FOR DATA SUBMISSION
     H*                 2. ADD PAYMENT PLAN
     H*
     H*  04/28/03 C:079 PROJECT: FL043
     H*                 EXPAND LIC# FIELD TO 13A
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  12/16/02 C:079 PROJECT: FL027
     H*                 ADD PLACE OF TREATMENT, LIC#, MASTER LIC#
     H*                 FOR FLORIDA DATA SUBMISSION
     H*
     H*  11/18/02 C:079 PROJECT: FSMNT
     H*                 CORRECT CURSOR POSITIONING FOR DIAG CODE FLDS
     H*
     H*  10/01/01 C:079 SUPPORT: MC114 28975
     H*                 REQUIRE SERVICE ZIP CODE IF AST = PA. DEFAULT
     H*                 TO ZEROS
     H*
     H*  07/06/01 C:079 PROJECT: FSMNT #196
     H*                 ADD EDIT TO SPECIALTY CODE
     H*
     H*  05/30/01 C:062 PROJECT: FS229
     H*                 POPULATE EOMB# SCREEN FIELD
     H*
     H*  05/25/01 C:062 PROJECT: FS228
     H*                 ADD SERVICE ZIP CODE
     H*
     H*  02/14/01 C:117 PROJECT: FS220
     H*                 ADD DIAGNOSIS CODES TO SCREEN
     H*
     H*  10/13/00 C:062 Support MC238
     H*                 ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  06/19/00 C:117 PROJECT FS211
     H*                 Additional Edits to date fields
     H*                 Need to update EDEDI flag.
     H*
     H*  02/21/00 C:117 PROJ: FS208
     H*                 ADDED LOGIC TO PROTECT SPECIALTY IF ON A
     H*                 RECON; ALSO SAVES SPECIALTY.
     H*
     H*  11/12/99 C:79  MC152
     H*                 Delete/Change efficiency isssue.
     H*                                                               *
     H*  09/23/99 C:56  Project: MC212                                *
     H*                 Add Provider Specialty search                 *
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR - SCR01                                                 *
     H*  09-    INQUIRY MODE
     H*  25-    PROTECT ELIGIBLE FOR DATA SUBMISSION
     H*  28-    CURSOR POSITIONING
     H*  29-    RECON (ALL SCREENS)
     H*  30-    ELIGIBLE FOR DATA SUBMISSION (ALL SCREENS)
     H*  31-    INVALID SEARCH FIELD
     H*  32-39  CURSOR POSITIONING
     H*  40-    ADD MODE
     H*  41-    CURSOR POSITIONING
     H*  42-    LICENSE NUMBER DATABASE INSTALLED
     H*  43-    CURSOR POSITIONING
     H*  47-    NETWORK
     H*         INVALID SEARCH FIELD
     H*  68-    DISPLAY EOB/NOTES
     H*  71-77  ERROR
     H*  78-81  ERROR
     H*  82-    INVALID SEARCH FIELD
     H*  83-    ERROR
     H*  84-88  ERROR
     H*  90-    COPYBOOK ERROR
     H*  98-99  GENERAL CHAIN
     H**********************************************************************
     H* INDICATORS - SCR01                                             *****
     H*
     H*** Beg Add *** 08/03/11 ****************************************
     H*
     H*  INDICATORS - SCR02
     H*
     H*  09-    INQUIRY MODE
     H*** End Add *** 08/03/11 ****************************************
     H**********************************************************************
     H*
     FF0381FFL  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFAEDP     UF A E             DISK
     F                                     RENAME(PED01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F*
     F*  Bill Entry Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  Provider Master File
     FFMPDL     IF   E           K DISK
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFAEEL     IF   E           K DISK
     FFAEDL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F*** BEG ADD *** 02/14/01 ****************************************
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*** END ADD *** 02/14/01 ****************************************
     F*** BEGIN ADD *** 05/25/01 **************************************
     F*  Geographic Zip Area File
     FFMGZL     IF   E           K DISK
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*** END   ADD *** 05/25/01 **************************************
     F*** BEG ADD *** 12/16/02 ****************************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END ADD *** 12/16/02 ****************************************
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
      /COPY SCOPYMT01D
     d*** Beg Add *** 06/27/19 ***************************************
      /copy sprmodule
     d isPbillsLite    S               n
     d*** End Add *** 06/27/19 ***************************************
     D*** BEG ADD *** 08/29/07 ****************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D*** END ADD *** 08/29/07 ****************************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*** BEG ADD *** 04/28/03 ****************************************
     D                 DS
     D  SCLIC#                 1     13
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D*** BEG CHG *** 03/25/05 ****************************************
     D***********************************     5  11 SCLIC5
     D*** BEG DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*****¢¢¢¢¢                              5   9 SCLIC5
     D*** END DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*** END CHG *** 03/25/05 ****************************************
     D*** BEG CHG *** 03/25/05 ****************************************
     D***********************************     4  10 SCLIC6
     D*** BEG DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*****¢¢¢¢¢                              4   6 SCLIC6
     D*** END DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*** END CHG *** 03/25/05 ****************************************
     D*** BEG DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*****¢¢¢¢¢                              3   9 SCLIC7
     D*** END DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*** END ADD *** 04/28/03 ****************************************
     D*** BEG ADD *** 08/29/07 ****************************************
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*** END ADD *** 08/29/07 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Dental Header Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     I*** Beg Del *** 04/17/13 ****************************************
     D******* #OFSCR          C                   CONST(1)
     I*** End Del *** 04/17/13 ****************************************
     D*** BEG ADD *** 03/20/08 ****************************************
     D ZZLIC#          C                   CONST('ZZ99999999999')

     I*** END ADD *** 03/20/08 ****************************************
     D***Beg Add *** 05/23/13 ***************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtcd                         1
     D*
     D xxicd10         s              1
     D @@rtcd          s              1
     D***End Add *** 05/23/13 ***************************************
     D*** Beg Add *** 08/05/13 ***************************************
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
     D*** End Add *** 08/05/13 ***************************************
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
     D*** End Add *** 07/18/25 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEDP
     C                   PARM                    @@RRN2            9 0          for FAEOP
     C                   PARM                    @@NWCD            5
     C                   PARM                    @@CHRG            9            TOTAL CHARGE
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
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY
     C*
     C** Key List for Specialty Code File
     C*
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C** Key List for Bill Entry Control FIle
     C*
     C     BEKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C** Key List for Provider Master File
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* KEY LIST FOR Header EOB/Notes File
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
     C     EDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEG ADD *** 02/14/01 ****************************************
     C*
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*** END ADD *** 02/14/01 ****************************************
     C*** BEGIN ADD *** 05/25/01 **************************************
     C*
     C* Key List for Geographic Zip Area File
     C*
     C     GZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C*
     C*  Key List for Provider Auxillary File
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*** END   ADD *** 05/25/01 **************************************
     C*
     C*** BEG ADD *** 12/16/02 ****************************************
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKID
     C                   KFLD                    KKKEY
     C*** END ADD *** 12/16/02 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kklost
     C*** End Add *** 08/05/13 ***************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        ##SUB#
     C     *LIKE         DEFINE    STKEY         ##KEY
     C     *LIKE         DEFINE    EDCO#         KKCO#
     C     *LIKE         DEFINE    EDSUB#        KKSUB#
     C     *LIKE         DEFINE    EDFND         KKFND
     C     *LIKE         DEFINE    EDFYR         KKFYR
     C     *LIKE         DEFINE    EDCASE        KKCASE
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    DIICD9        ##ICD9
     C     *LIKE         DEFINE    DILOST        ##LOST
     C*** BEG ADD *** 12/16/02 ****************************************
     C     *LIKE         DEFINE    GTTBID        KKID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 12/16/02 ****************************************
     C*** BEG ADD ***** 12/22/06 ****************************************
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C*** END ADD ***** 12/22/06 ****************************************
     C*** BEG ADD *** 04/20/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/20/07 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C     *LIKE         DEFINE    EOFRDT        @@SDTE
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C                   ENDSR
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
    C*** Beg Add *** 06/27/19 ****************************************
     C                   if        not isPbillsLite
    C*** End Add *** 06/27/19 ****************************************
       //*** Beg add ***** 07/18/25 ***********************************
       If isModEnabled('NCCI Med Data Call');
          If scsvzp = *zeros and EOrcn = 0;
             *in81 = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*
     C* VALIDATE PLACE OF TREATMENT
     C*
     C                   MOVEL(P)  SCPOTX        KKKEY
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** MOVEL'FLPX'    KKID      P
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPX'        KKID
     C                   ELSE
     C                   MOVEL(P)  'FLET'        KKID
     C                   ENDIF
     C*** END CHG *** 04/15/04 ****************************************
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEG ADD *** 07/06/01 ****************************************
     C*
     C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
     C*
     C     SCSPCD        IFNE      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 07/06/01 ****************************************
     C*** BEGIN ADD *** 05/25/01 **************************************
     C*
     C** SERVICE ZIP CODE CANNOT BE ZERO
     C*
     C*** BEG ADD *** 10/01/01 ****************************************
     C*** BEG CHG *** 04/20/07 ****************************************
     C*****      EOAST     IFEQ 'PA'
     C     EDITST        IFEQ      'PA'
     C*** END CHG *** 04/20/07 ****************************************
     C     *IN29         ANDEQ     *OFF
     C*** END ADD *** 10/01/01 ****************************************
     C*
     C     SCSVZP        IFEQ      *ZEROS
     C                   SETON                                        8190
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END   ADD *** 05/25/01 **************************************
     C*
     C*** BEG ADD *** 10/01/01 ****************************************
     C                   ENDIF                                                  SCAST=PA
     C*** END ADD *** 10/01/01 ****************************************
     C*
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN PGMSTR SUBROUINTE)
     C*
     C   30SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C*** BEG DEL *** 04/15/04 ****************************************
     C**********
     C********** SCEDI     IFNE *BLANKS
     C********** SCEDI     ANDNE'Y'
     C********** SCEDI     ANDNE'N'
     C**********           SETON                     9079
     C**********           GOTO CK1END
     C**********           ENDIF                           SCTAPEIFNE BLANKS
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     *IN25         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'FLES'        KKID
     C                   MOVEL(P)  SCEDI         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 02/14/01 ****************************************
     C** VALIDATE 1ST DIAGNOSIS CODE
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa1;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in73 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in91 = *off;
           if scdia1 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in91 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in91 = *off;

           if scdia1 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in91 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*
     C*** End Add *** 08/05/13 ****************************************
     C*
     C                   ENDIF
     C*
     C** VALIDATE DIAGNOSIS CODE #2 IF ENTERED
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa2;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in74 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in92 = *off;
           if scdia2 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in92 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in92 = *off;

           if scdia2 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in92 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** VALIDATE DIAGNOSIS CODE #3 IF ENTERED
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** ENd Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa3;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in75 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in93 = *off;
           if scdia3 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in93 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in93 = *off;

           if scdia3 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in93 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** VALIDATE DIAGNOSIS CODE #4 IF ENTERED
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa4;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in76 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in94 = *off;
           if scdia4 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in94 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 03/12/03 ****************************************
     C     DISTAT        OREQ      'I'
     C*** END ADD *** 03/12/03 ****************************************
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in94 = *off;

           if scdia4 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in94 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*** END ADD *** 02/14/01 ****************************************
     C*** BEG ADD *** 12/16/02 ****************************************
     C*
     C* LICENSE NUMBER IS REQUIRED IF EDI FLAG IS Y
     C*
     C*** BEG CHG *** 04/15/04 ****************************************
     C********** SCEDI     IFEQ 'Y'
     C     SCEDI         IFNE      'N'
     C     SCEDI         ANDNE     ' '
     C*** END CHG *** 04/15/04 ****************************************
     C     SCLIC#        ANDEQ     *BLANKS
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C*** BEG ADD *** 03/20/08 ****************************************
     C                   ELSE
     C     SCLIC1        IFEQ      'Z'
     C     SCLIC2        ANDEQ     'Z'
     C     SCLIC#        ANDNE     ZZLIC#
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD *** 03/20/08 ****************************************
     C                   ENDIF
     C*** END ADD *** 12/16/02 ****************************************
     C*
     C*** BEG ADD *** 04/28/03 ****************************************
     C     SCLIC#        IFNE      *BLANKS
     C*** BEG CHG *** 03/25/05 ****************************************
     C********** SCEDI     ANDEQ'Y'
     C     SCEDI         ANDNE     'N'
     C     SCEDI         ANDNE     ' '
     C*** END CHG *** 03/25/05 ****************************************
     C*
      *** BEG DELETE ***** 08/29/07 *¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C***¢       SCLIC1    IFLT 'A'
     C***¢       SCLIC1    ORGT 'Z'
     C***¢                 SETON                     9085
     C***¢                 GOTO CK1END
     C***¢                 ENDIF                           SCLIC1 < 'A'
     C***¢
     C***¢       SCLIC2    IFLT 'A'
     C***¢       SCLIC2    ORGT 'Z'
     C***¢                 SETON                     9085
     C***¢                 GOTO CK1END
     C***¢                 ENDIF
     C*
     C* 3rd position can be alpha or numeric
     C***¢                 TESTN          SCLIC3     60
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢
     C***¢       SCLIC3    IFLT 'A'
     C***¢       SCLIC3    ORGT 'Z'
     C***¢
     C***¢                 SETON                     9086
     C***¢                 GOTO CK1END
     C***¢
     C***¢                 ENDIF                           SCLIC2 IFLT
     C***¢
     C***¢                 ENDIF                           *IN60 = 0
     C***¢
     C* 4rd position can be alpha or numeric
     C***¢
     C***¢                 TESTN          SCLIC4     60
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢
     C***¢       SCLIC4    IFLT 'A'
     C***¢       SCLIC4    ORGT 'Z'
     C***¢
     C***¢                 SETON                     9087
     C***¢                 GOTO CK1END
     C***¢
     C***¢                 ENDIF                           SCLIC2 IFLT
     C***¢
     C***¢                 ENDIF                           *IN60 = 0
     C***¢
     C***¢The last 7 digits MUST BE numeric
     C***¢
     C*** BEG ADD *** 04/28/03 ****************************************
     C***¢                 SELEC
     C***¢       SCLIC3    WHGE 'A'
     C***¢       SCLIC3    ANDLE'Z'
     C***¢       SCLIC4    ANDGE'A'
     C***¢       SCLIC4    ANDLE'Z'
     C*** END ADD *** 04/28/03 ****************************************
     C***¢
     C***¢                 TESTN          SCLIC5     60
     C*** BEG ADD *** 03/25/05 ****************************************
     C***¢       *IN60     IFEQ '0'
     C***¢                 SETON                     9065
     C***¢                 GOTO CK1END
     C***¢                 END
     C*** END ADD *** 03/25/05 ****************************************
     C***¢
     C*** BEG ADD *** 04/28/03 ****************************************
     C***¢       SCLIC3    WHGE 'A'
     C***¢       SCLIC3    ANDLE'Z'
     C***¢       SCLIC4    ANDGE'0'
     C***¢       SCLIC4    ANDLE'9'
     C***¢                 TESTN          SCLIC6     60
     C*** BEG ADD *** 03/25/05 ****************************************
     C***¢       *IN60     IFEQ '0'
     C***¢                 SETON                     9064
     C***¢                 GOTO CK1END
     C***¢                 END
     C*** END ADD *** 03/25/05 ****************************************
     C***¢
     C***¢       SCLIC3    WHGE '0'
     C***¢       SCLIC3    ANDLE'9'
     C***¢       SCLIC4    ANDGE'0'
     C***¢       SCLIC4    ANDLE'9'
     C***¢                 TESTN          SCLIC7     60
     C***¢
     C***¢       SCLIC3    WHGE '0'
     C***¢       SCLIC3    ANDLE'9'
     C***¢       SCLIC4    ANDGE'A'
     C***¢       SCLIC4    ANDLE'Z'
     C***¢                 TESTN          SCLIC5     60
     C***¢
     C***¢                 OTHER
     C***¢                 TESTN          SCLIC5     60
     C***¢
     C***¢                 ENDSL
     C*
     C***¢       *IN60     IFEQ '0'
     C***¢                 SETON                     9088
     C***¢                 GOTO CK1END
     C***¢                 END
     C*
     C****** END DELETE ******* 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C****** BEG ADD ******* 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9085
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
     C                   MOVE      *IN40         SV40              1
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C     SCLIC4        IFGT      *BLANKS
     C***** END ADD *******  10/04/08 *********************************
      *
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
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
     C                   SETON                                        9086
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9086
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C****** END ADD ******* 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*
     C*** END ADD *** 04/28/03 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* PAYMENT PLAN IS REQUIRED
     C*
     C     SCPPLN        IFEQ      *BLANKS
     C                   SETON                                        7790
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE PAYMENT PLAN
     C*
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** MOVEL'FLPP'    KKID      P
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKID
     C                   ENDIF
     C*** END CHG *** 04/15/04 ****************************************
     C                   MOVEL(P)  SCPPLN        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   SETON                                        8490
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 03/11/10 **************************************
     C*
     C*  VALIDATE THE REVISION INDICATOR
     C*
     C     SCREVI        IFEQ      *BLANK
     C                   SETON                                        9052
     C                   GOTO      CK1END
     C                   ELSE
     C*
     C                   MOVEL(P)  'REVI'        KKID
     C                   MOVEL(P)  SCREVI        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        9053
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C                   ENDIF                                                  REVI
     C*** END   ADD *** 03/11/10 **************************************
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scsnpi));
          If invalidPos <> 0 or scsnpi = *blanks;
             mssnpi = 'MC50007';
             ersnpi = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
    C*** Beg Add *** 06/27/19 ****************************************
     C                   Endif
    C*** End Add *** 06/27/19 ****************************************
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
     C*** BEG ADD *** 09/23/99 ****************************************
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*** BEGIN DEL *** 10/13/00 **************************************
     C**********           SETOF                     3836
     C*** END   DEL *** 10/13/00 **************************************
     C*** BEG ADD *** 02/14/21 ****************************************
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373941
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   SETOFF                                       43
     C                   SETOFF                                       91
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   CLEAR                   HLDIA
     C*
     C*
     C*** BEG ADD *** 02/14/21 ****************************************
     C                   SELECT
     C*** BEG ADD *** 02/14/01 ****************************************
     C*** BEG ADD *** 12/16/02 ****************************************
     C*
     C** PLACE OF TREATMENT SEARCH
     C*
     C     HFLD          WHENEQ    'SCPOTX'
     C*
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPX'        ##ID
     C                   ELSE
     C                   MOVEL(P)  'FLET'        ##ID
     C                   ENDIF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** PARM 'FLPX'    ##ID    4
     C                   PARM                    ##ID              4
     C*** END CHG *** 04/15/04 ****************************************
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPOTX
     C                   MOVEL(P)  GTDESC        SCPXDS
     C                   SETON                                        34
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        39
     C*
     C                   ENDIF
     C*** END ADD *** 12/16/02 ****************************************
     C*
     C** DIAGNOSIS CODE #1 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        HLDIA             5
     C                   MOVEL(P)  SCDIA1        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa1 = HLDIA;
             *in90 = *on;
             *in34 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in28 = *on;
          Endif;

          If scDia1 <> *blanks;
             kkICD10 = scDIa1;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   eval      *in91 = *off
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA1
     C                   MOVEL(P)  HLDIA         SCDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA1
     C                   MOVEL(P)  ##ICD9        SCDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9028      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        HLDIA             5
     C                   MOVEL(P)  SCDIA2        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa2 = HLDIA;
             *in90 = *on;
             *in28 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in32 = *on;
          Endif;

          If scDia2 <> *blanks;
             kkICD10 = scDIa2;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD2DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA2
     C                   MOVEL(P)  HLDIA         SCDIA2
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9028      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA2
     C                   MOVEL(P)  ##ICD9        SCDIA2
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9032      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** DIAGNOSIS CODE #3 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        HLDIA             5
     C                   MOVEL(P)  SCDIA3        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa3 = HLDIA;
             *in90 = *on;
             *in32 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in33 = *on;
          Endif;

          If scDia3 <> *blanks;
             kkICD10 = scDIa3;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD3DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA3
     C                   MOVEL(P)  HLDIA         SCDIA3
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA3
     C                   MOVEL(P)  ##ICD9        SCDIA3
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** DIAGNOSIS CODE #4 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        HLDIA             5
     C                   MOVEL(P)  SCDIA4        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa4 = HLDIA;
             *in90 = *on;
             *in33 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in38 = *on;
          Endif;

          If scDia4 <> *blanks;
             kkICD10 = scDIa4;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD4DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA4
     C                   MOVEL(P)  HLDIA         SCDIA4
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9033      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA4
     C                   MOVEL(P)  ##ICD9        SCDIA4
     C*** End Chg *** 05/23/13 ****************************************
     C*** BEG CHG *** 11/18/02 ****************************************
     C******************** SETON                     9035  pos cursor
     C                   SETON                                        9038      pos cursor
     C*** END CHG *** 11/18/02 ****************************************
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*** END ADD *** 02/14/01 ****************************************
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     SCSPCD        HLSPCD            3
     C                   CLEAR                   ##KEY1
     C                   CALL      'F2335R'
     C                   PARM      '2'           ##CALL
     C                   PARM                    ##KEY1           10
     C                   PARM      ' '           ##RTCD
     C*
     C     ##RTCD        IFNE      '*'
     C                   MOVE      HLSPCD        SCSPCD
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY1        SCSPCD
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C*** BEGIN CHG *** 05/25/01 **************************************
     C**********           SETON                     9038  pos cursor
     C                   SETON                                        9035      pos cursor
     C*** END   CHG *** 05/25/01 **************************************
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/16/02 ****************************************
     C     HFLD          WHENEQ    'SCLIC#'
     C*
     C     *IN42         IFEQ      *ON
     C*
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C*** BEG CHG *** 11/08/05 ****************************************
     C******************** PARM *BLANKS   ##LIC#  9
     C                   PARM      *BLANKS       ##LIC#           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C     ##LIC#        IFNE      *BLANKS
     C                   MOVEL(P)  ##LIC#        SCLIC#
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   SETON                                        43
     C                   ELSE
     C                   SETON                                        41
     C*** END ADD *** 04/15/04 ****************************************
     C                   ENDIF
     C*
     C*** BEG DEL *** 04/15/04 ****************************************
     C******************** SETON                     41
     C*** END DEL *** 04/15/04 ****************************************
     C*
     C                   ELSE
     C*
     C                   SETON                                        80
     C*
     C                   ENDIF                                                  *IN42 = *ON
     C*** END ADD *** 12/16/02 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     HFLD          WHENEQ    'SCPPLN'
     C*
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        ##ID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        ##ID
     C                   ENDIF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** PARM 'FLPP'    ##ID    4
     C                   PARM                    ##ID              4
     C*** END CHG *** 04/15/04 ****************************************
     C*
     C                   SETON                                        43
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPPLN
     C                   MOVEL(P)  GTDESC        SCPPDS
    C*** BEG ADD ***** 03/11/10 **************************************
     C                   SETOFF                                       43
     C                   SETON                                        54
    C*** END ADD ***** 03/11/10 **************************************
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
    C*** BEG ADD ***** 12/22/06 **************************************
     C     HFLD          WHENEQ    'SCEDI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'FLES'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCEDI
     C*
     C                   SETON                                        9041
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        9038
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
    C*** END ADD ***** 12/22/06 **************************************
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*** BEGIN ADD *** 10/13/00 **************************************
    C*** BEG ADD ***** 03/11/10 **************************************
    C*
     C     HFLD          WHENEQ    'SCREVI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'REVI'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCREVI
     C                   MOVEL(P)  GTDESC        SCREVD
     C*
     C                   SETON                                        9054
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        9054
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
    C*** END ADD ***** 03/11/10 **************************************
     C                   OTHER
     C*
     C                   SELECT
    C*** BEG DEL ***** 12/22/06 **************************************
     C********   HFLD      WHEQ 'SCEDI'
     C********             SETON                     31
    C*** END DEL ***** 12/22/06 **************************************
     C*** BEGIN ADD *** 05/25/01 **************************************
     C     HFLD          WHENEQ    'SCSVZP'
     C                   SETON                                        82
     C*** END   ADD *** 05/25/01 **************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCSNPI'
     C                   EVAL      MSSNPI = 'MCR0676'
     C                   EVAL      ERSNPI = *ON
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*** END   ADD *** 10/13/00 **************************************
     C                   ENDSL
     C*
     C*** BEG ADD *** 12/16/02 ****************************************
     C                   SETON                                        90
     C*** END ADD *** 12/16/02 ****************************************
     C*
     C                   ENDSR
     C*** END ADD *** 09/23/99 ****************************************
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0
     C                   CALL      'F0355R'
     C                   PARM      ' '           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C                   CLEAR                   SCDIA1
     C                   CLEAR                   SCDIA2
     C                   CLEAR                   SCDIA3
     C                   CLEAR                   SCDIA4
     C                   CLEAR                   SCD1DS
     C                   CLEAR                   SCD2DS
     C                   CLEAR                   SCD3DS
     C                   CLEAR                   SCD4DS
     C*** BEG ADD *** 10/01/01 ****************************************
     C                   CLEAR                   SCSVZP
     C*** END ADD *** 10/01/01 ****************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C                   CLEAR                   SCZIP
     C                   CLEAR                   SCSNPI
     C*** Beg Add *** 07/18/25 ***************************************
     C*** BEG ADD *** 12/16/02 ****************************************
     C                   CLEAR                   SCPOTX
     C                   CLEAR                   SCPXDS
     C                   CLEAR                   SCLIC#
     C                   CLEAR                   SCFLIC
     C*** END ADD *** 12/16/02 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   CLEAR                   SCPPLN
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   CLEAR                   SCREVI
     C*** END ADD *** 03/11/10 ****************************************
     C*** Beg Add *** 04/17/13 ****************************************
     C                   CLEAR                   SCRFEI
     C                   CLEAR                   SCRLSN
     C                   CLEAR                   SCRFSN
     C                   CLEAR                   SCRAD1
     C                   CLEAR                   SCRAD2
     C                   CLEAR                   SCRCTY
     C                   CLEAR                   SCRST
     C                   CLEAR                   SCRZIP
     C*** End Add *** 04/17/13 ****************************************
     C*
     C* If in ADD mode and @@RRN is not zero, then user has used F12
     C* from the detail program.  This require a chain to the
     C* Header Info file in order to access the Header information.
     C*
     C     @@MODE        IFEQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C** GET PROVIDER SPECIALTY CODE (SERVICING PROVIDER)
     C*
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C*** BEG CHG *** 02/21/00 **(PUT CONDITION AROUND SCSPCD)*********
     C*  IF THERE IS A SPECIALTY CODE IN THE FAEOP FILE, USE IT;
     C*    OTHERWISE, USE THE ON IN FMPDP
     C     EOSPCD        IFNE      *BLANKS
     C                   MOVEL     EOSPCD        SCSPCD
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C                   ENDIF
     C*** END CHG *** 02/21/00 **(PUT CONDITION AROUND SCSPCD)*********
     C*** BEGIN ADD *** 05/25/01 **************************************
     C*
     C* Load service zip code
     C*
     C*** BEG ADD *** 10/01/01 ****************************************
     C*** BEG CHG *** 04/20/07 ****************************************
     C*****      EOAST     IFNE 'PA'
     C     EDITST        IFNE      'PA'
     C*** END CHG *** 04/20/07 ****************************************
     C*** END ADD *** 10/01/01 ****************************************
     C                   MOVE      *BLANKS       SCSVZP
     C*
     C     EOFLIC        IFNE      *BLANKS
     C                   MOVEL     EOFLIC        SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      EOFLIC        SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C                   ELSE
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C  N99              MOVE      P1ZIP         SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDIF                                                  *IN99=OFF
     C*
     C*** BEG ADD *** 10/01/01 ****************************************
     C                   ELSE
     C                   MOVEL     EOFLIC        SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      EOFLIC        SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C*
     C                   ENDIF                                                  SCAST<>PA
     C*** END ADD *** 10/01/01 ****************************************
     C*** END   ADD *** 05/25/01 **************************************
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA
     C*
     C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
     C*
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EEKY          CHAIN     PEE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     EDTNPI        SCSNPI
     C*** End Add *** 07/18/25 ***************************************
     C*** BEG ADD *** 02/14/01 ****************************************
     C                   MOVEL     'D'           KKLOST
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EDDIA1        SCDIA1
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  EDDIA1        SCDIA1
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1001;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD1DS
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EDDIA2        SCDIA2
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  EDDIA2        SCDIA2
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1002;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD2ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD2DS
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EDDIA3        SCDIA3
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  EDDIA3        SCDIA3
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1003;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD3ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD3DS
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EDDIA4        SCDIA4
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  EDDIA4        SCDIA4
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1004;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD4ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD4DS
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*** END ADD *** 02/14/01 ****************************************
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C*
     C*** BEG ADD *** 06/19/00 ****************************************
     C     EDEDI         IFNE      *BLANKS
     C                   MOVEL     EDEDI         SCEDI
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  'FLES'        KKID
     C                   MOVEL(P)  SCEDI         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 06/19/00 ****************************************
     C*** BEG ADD *** 12/16/02 ****************************************
     C*
     C* LOAD PLACE OF TREATMENT AND DESCRIPTION
     C*
     C                   MOVE(P)   EDPOTX        SCPOTX
     C                   MOVEL(P)  SCPOTX        KKKEY
     C*** BEG CHG *** 04/15/04 ****************************************
     C******************** MOVEL'FLPX'    KKID      P
     C                   MOVEL(P)  'FLET'        KKID
     C*** END CHG *** 04/15/04 ****************************************
     C*
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCPXDS
     C*
     C* LOAD LICENSE NUMBER FIELDS
     C*
     C                   MOVEL(P)  EDLIC#        SCLIC#
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EDFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EDFLIC        SCLIC#
     C                   ENDIF
     C*** END ADD *** 04/28/03 ****************************************
     C*
     C     PDKY          CHAIN     PPD01                              99
     A*** Begin Chg *** 12/10/07 **************************************
     C**N99**********      MOVELPDFLIC    SCFLIC    P
     C  N99              MOVEL(P)  PDLIC#        SCFLIC
     A*** End   Chg *** 12/10/07 **************************************
     C*** END ADD *** 12/16/02 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL     EOREVI        SCREVI
     C*** END ADD *** 03/11/10 ****************************************
     C     @@MODE        IFEQ      'A'
     C                   MOVEL(P)  BEPPLN        SCPPLN
     C*
     C                   ELSE
     C                   MOVEL(P)  EDPPLN        SCPPLN
     C*
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C     @@MODE        IFEQ      'A'
     C     SCREVI        ANDEQ     *BLANKS
     C                   MOVEL     BEREVI        SCREVI
     C                   ENDIF
     C*** END ADD *** 03/11/10 ****************************************
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      EDRFEI        SCRFEI
     C                   MOVE      EDRLSN        SCRLSN
     C                   MOVE      EDRFSN        SCRFSN
     C                   MOVE      EDRAD1        SCRAD1
     C                   MOVE      EDRAD2        SCRAD2
     C                   MOVE      EDRCTY        SCRCTY
     C                   MOVE      EDRST         SCRST
     C                   MOVE      EDRZIP        SCRZIP
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          // Beg Chg *** 08/05/13 ****************************************
          //scICdind = 'N';
          //If xxicd10 = *on;
          If xxicd10 = *on and EDicdind ='Y';
          // End Chg *** 08/05/13 ****************************************
             scDIa1  = EDicd1001;
             scDIa2  = EDicd1002;
             scDIa3  = EDicd1003;
             scDIa4  = EDicd1004;
             scICdind= EDicdind ;
          // Beg Add *** 08/05/13 ****************************************
          Elseif EDicdind ='N';
             scICdind = EDicdind;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*** BEG ADD *** 03/20/08 ****************************************
     C                   MOVE      *OFF          LICCHG            1
     C     SCLIC#        IFNE      EDFLIC
     C                   MOVE      *ON           LICCHG
     C                   ENDIF
     C*** END ADD *** 03/20/08 ****************************************
     C*
     C                   Z-ADD     EOCO#         EDCO#
     C                   Z-ADD     EOSUB#        EDSUB#
     C                   Z-ADD     EOFND         EDFND
     C                   Z-ADD     EOFYR         EDFYR
     C                   Z-ADD     EOCASE        EDCASE
     C                   MOVE      EOSS#         EDSS#
     C                   Z-ADD     EOACDT        EDACDT
     C*
     C*** BEG ADD *** 02/21/00 ****************************************
     C                   MOVE      SCSPCD        EOSPCD
     C*** END ADD *** 02/21/00 ****************************************
     C*** BEG ADD ***** 03/20/08 **************************************
     C     SCLIC1        CAT       SCLIC2        XXLIC2            2
     C     XXLIC2        CAT       SCLIC3        XXLIC3            3
     C*** END ADD ***** 03/20/08 **************************************
     C*** END ADD ***** 03/21/07 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C***********
     C***********          CALL 'FA746R'
     C***********          PARM           SCAST
     C***********          PARM           SCLIC#
     C***********          PARM ' '       @@RTCD  1
     C*** END DEL ***** 07/12/10 **************************************
     C*** BEG ADD ***** 03/20/08 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C***********          PARM           @@BTYP
     C*** END DEL ***** 07/12/10 **************************************
     C*** END ADD ***** 03/20/08 **************************************
     C*** BEG ADD ***** 03/11/10 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C***********          PARM           @@SDTE
     C*** END DEL ***** 07/12/10 **************************************
     C*** END ADD ***** 03/11/10 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C*********
     C*********  @@RTCD    IFEQ 'Y'
     C*** END DEL ***** 07/12/10 **************************************
     C*** BEG ADD ***** 03/20/08 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C*********
     C*********  XXLIC2    OREQ 'PU'
     C*********  XXLIC2    OREQ 'PS'
     C*********  XXLIC2    OREQ 'NP'
     C*********  XXLIC3    OREQ 'DME'
     C*** END DEL ***** 07/12/10 **************************************
     C*** END ADD ***** 03/20/08 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C*********
     C*********            MOVE 'N'       SCEDI
     C*********            ELSE
     C*********  LICCHG    IFEQ *ON
     C*********            MOVE 'E'       SCEDI
     C*********            ENDIF
     C*********            ENDIF                           @@RTCD = 'Y'
     C*** END DEL ***** 07/12/10 **************************************
     C*** END ADD ***** 03/21/07 **************************************
     C*** BEG ADD *** 06/19/00 ****************************************
     C                   MOVEL     SCEDI         EDEDI
     C*** END ADD *** 06/19/00 ****************************************
     C*** BEG ADD *** 02/14/01 ****************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        EDDIA1
     C********           MOVE(P)   SCDIA2        EDDIA2
     C********           MOVE(P)   SCDIA3        EDDIA3
     C********           MOVE(P)   SCDIA4        EDDIA4
     C                   MOVEL(P)  SCDIA1        EDDIA1
     C                   MOVEL(P)  SCDIA2        EDDIA2
     C                   MOVEL(P)  SCDIA3        EDDIA3
     C                   MOVEL(P)  SCDIA4        EDDIA4
     C*** End Chg *** 05/23/13 ****************************************
     C*** END ADD *** 02/14/01 ****************************************
     C*** BEG ADD *** 10/01/01 ****************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVEL     SCSNPI        EDTNPI
     C*** END ADD *** 10/01/01 ****************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C*** BEG ADD *** 12/16/02 ****************************************
     C                   MOVE(P)   SCPOTX        EDPOTX
     C*** BEG CHG *** 04/28/03 ****************************************
     C******************** MOVELSCLIC#    EDLIC#    P
     C                   MOVEL(P)  SCLIC#        EDFLIC
     C*** END CHG *** 04/28/03 ****************************************
     C*** END ADD *** 12/16/02 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  SCPPLN        EDPPLN
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      SCREVI        EOREVI
     C*** END ADD *** 03/11/10 ****************************************
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCRFEI        EDRFEI
     C                   MOVE      SCRLSN        EDRLSN
     C                   MOVE      SCRFSN        EDRFSN
     C                   MOVE      SCRAD1        EDRAD1
     C                   MOVE      SCRAD2        EDRAD2
     C                   MOVE      SCRCTY        EDRCTY
     C                   MOVE      SCRST         EDRST
     C                   MOVE      SCRZIP        EDRZIP
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          // Beg Add *** 08/05/13 ****************************************
          EDicdind  = scICdind;
          // End Add *** 08/05/13 ****************************************
          If xxICD10 = '1' and scICdInd ='Y';
             EDicd1001 = scDIa1;
             EDicd1002 = scDIa2;
             EDicd1003 = scDIa3;
             EDicd1004 = scDIa4;
             // If ICD10 is Active then no need of ICD9 fields
             EDdia1 = *Blanks;
             EDdia2 = *Blanks;
             EDdia3 = *Blanks;
             EDdia4 = *Blanks;
          Else;
             EDdia1 = scDIa1;
             EDdia2 = scDIa2;
             EDdia3 = scDIa3;
             EDdia4 = scDIa4;

             // If ICD09 is Active then no need of ICD10 fields

             EDicd1001 = *Blanks;
             EDicd1002 = *Blanks;
             EDicd1003 = *Blanks;
             EDicd1004 = *Blanks;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
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
       //*** Beg add ***** 07/18/25 ************************************
       mssnpi = *blanks;
       ersnpi = *off;
       //*** End add ***** 07/18/25 ************************************
     C*** BEGIN ADD *** 10/13/00 **************************************
     C                   SETOFF                                       383631
     C*** END   ADD *** 10/13/00 **************************************
     C*** Beg Add *** 06/03/15 ****************************************
     C                   SETOFF                                       919293
     C                   SETOFF                                       94
     C*** End Add *** 06/03/15 ****************************************
     C*** BEG ADD *** 09/23/99 ****************************************
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C*** END ADD *** 09/23/99 ****************************************
     C                   ENDCS
     C*** Beg Add *** MC367 *** 11/19/12 ******************************
      /Free
      /If Defined(WEB)
         If *INKK;
           ExSr KKDO;  //F11=EOB/Notes
         EndIf;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/19/12 ******************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*  Get relative record number for header record and put in
     C*  parm @@RRN.  This will allow the ability to access header
     C*  data if user F12=Cancel from detail screen on an add.
     C*
     C     @@MODE        IFEQ      'A'
     C     EDKY          CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* The following call to the WRITER subroutine is necessary
     C* because if program bombs out in add mode before the user
     C* takes action to write the header record, 'change' mode
     C* will be looking to update a record that does not exist.
     C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
     C* call the write subroutine to add the header record.
     C*       (WILL BE EXECUTED IF USER HITS ENTER)
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *OFF
     C     @@RRN         ANDEQ     *ZEROS
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C*
     C*   Call Cancel/Pend program if user hits F3=EXIT
     C*   If in Add mode, enable user to cancel bill (##CANC = Y)
     C*   Otherwise do not allow cancel capability (##CANC = N)
     C*
     C     *INKC         IFEQ      *ON
     C     @@MODE        ANDNE     'D'
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'Y'           ##CANC                         able to cancel
     C                   ELSE
     C                   MOVE      ' '           ##CANC                         unable to cancel
     C                   ENDIF
     C*
     C* If in add mode and F3=Exit is hit, call writer subroutine
     C* to write the header record
     C*
     C     @@MODE        IFEQ      'A'
     C     *INKC         ANDEQ     *ON
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* If in 'change' mode and F3=Exit is hit, call update subroutine
     C* to update the header record with the changed info.
     C* This will work on both SCR01 and SCR02.
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *ON
     C     @@RRN         ANDNE     *ZEROS
     C                   EXSR      UPDATR
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C* The following call to the WRITER subroutine is necessary
     C* because if program bombs out in add mode before the user
     C* takes action to write the header record, 'change' mode
     C* will be looking to update a record that does not exist.
     C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
     C* call the write subroutine to add the header record.
     C*       (WILL BE EXECUTED IF USER HITS F3=EXIT)
     C*
     C     @@MODE        IFEQ      'C'
     C     @@RRN         ANDEQ     *ZEROS
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0392R'
     C     *INKL         PARM      ' '           ##KL
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##CANC            1
     C                   PARM                    @@CHRG
     C                   PARM      ' '           ##RTCD            1
     C**********           MOVE '1'       @@KC
     C*
     C                   ENDIF                                                  *INKC = *ON
     C*
     C*** BEG DEL *** 11/12/99 ****************************************
     C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
     C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
     C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
     C**********
     C********** EOBLST    IFEQ 'U'
     C********** *INKL     ANDNE*ON
     C********** ##RTCD    ANDNE'P'
     C**********           Z-ADD@@RRN2    ##RRN
     C**********           CALL 'FA713R'
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##RRN
     C**********           ENDIF                           EOBLST = 'U'
     C*** END DEL *** 11/12/99 ****************************************
     C*
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
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
     C*** BEG ADD *** 04/20/07 ****************************************
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'Y'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C*** END ADD *** 04/20/07 ****************************************
     C*
     C*** BEGIN ADD *** 05/30/01 **************************************
     C                   Z-ADD     EOEOB#        SCEOB#
     C*** END   ADD *** 05/30/01 **************************************
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*** BEG ADD *** 03/20/08 **************************************
     C                   MOVE      EOBTYP        @@BTYP            1
     C*** END ADD *** 03/20/08 **************************************
     C*** BEG ADD *** 03/11/10 **************************************
     C                   MOVE      EOFRDT        @@SDTE
     C*** END ADD *** 03/11/10 **************************************
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
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
     C* NETWORK IS INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        47
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*
     C** Get 'Reductions' headings
     C*
      /Free
       clear scHMRA_dot;
       clear scHOT1_dot;
      /If Not Defined(WEB)
       scHMRA_dot = '. . . . . .';
       scHOT1_dot = '. . . . . .';
      /Endif
      /End-free
     C                   Z-ADD     EOSUB#        ##SUB#
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMRA
     C                   MOVEL     CXHNET        SCHNET
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C** DETERMINE IF ELIGIBLE FOR DATA SUBMISSION SHOULD BE DISPLAYED
     C** AND REQUIRED AND SET INDICATOR IF LIC# DATABASE IS INSTALLED
     C*
     C*** BEG CHG *** 04/20/07 ****************************************
     C*****                MOVELEOAST     ##KEY
     C                   MOVEL     EDITST        ##KEY
     C*** END CHG *** 04/20/07 ****************************************
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C*** BEG CHG *** 04/20/07 ****************************************
     C*****                MOVE EOAST     KKAST
     C                   MOVE      EDITST        KKAST
     C*** END CHG *** 04/20/07 ****************************************
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVEL     EOSTYP        KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C     SCEDI         IFEQ      *BLANKS
     C  N98              MOVE      BEEDI         SCEDI
     C                   ENDIF
     C*
     C* SEE IF LIC# DATABASE IS INSTALLED
     C*
     C     STBS01        IFEQ      'Y'
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       30
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C** SET VARIOUS FIELDS
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  EONTWK
     C*
     C* SET INITIAL CURSOR POSITON TO PLACE OF TREATMENT
     C*** BEG CHG *** 02/14/00 ****************************************
     C***********          SETON                     37
     C*** BEG CHG *** 12/16/02 ****************************************
     C***********          SETON                     34
     C                   SETON                                        39
     C*** END CHG *** 12/16/02 ****************************************
     C*** END CHG *** 02/14/00 ****************************************
     C*
     C*** BEG ADD *** 02/21/00 ****************************************
     C*
     C* PROTECT SPECIALTY IF EORCN>0
     C*
     C     EORCN         IFGT      *ZERO
     C                   SETON                                        29
     C                   ELSE
     C*
     C                   SETOFF                                       29
     C                   ENDIF                                                  EORCN>0
     C*** END ADD *** 02/21/00 ****************************************
     C*** BEG ADD *** 10/01/01 ****************************************
     C*
     C*** BEG DEL *** 04/15/04 ****************************************
     C*********
     C* IF PA BILL AND NOT A RECON THEN POSITION CURSOR TO SERVICE ZIP
     C* AND DISPLAY IN HIGH INTENSITY.
     C*********
     C*********  EOAST     IFEQ 'PA'
     C*********  *IN29     ANDEQ*OFF
     C*********
     C*********            SETON                     3884
     C*********            SETON                     3884
     C*********
     C*********            ENDIF
     C*** END DEL *** 04/15/04 ****************************************
     C*** END ADD *** 10/01/01 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* PROTECT ELIG FOR DATA SUBMISSION IF SENT TO THE STATE ALREADY
     C* IT CAN BE CHANGE IN THE EDI CORRECTION SCREENS
     C*
     C     EOTAPE        IFNE      ' '
     C     EOTAPE        ANDNE     'Y'
     C     EOTAPE        ANDNE     'N'
     C     EOTAPE        ANDNE     'E'
     C                   SETON                                        25
     C                   ELSE
     C                   SETOFF                                       25
     C                   ENDIF
     C*
     C                   MOVEL(P)  'FLES'        KKID
     C                   MOVEL(P)  SCEDI         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDNE     'I'
     C                   MOVEL(P)  GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      2             #OFSCR            1 0
     C                   ELSE
     C                   MOVE      1             #OFSCR
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

         // Call System API to check wheather the ICD10 is active or not

         @@rtcd = *blanks;
         callp SA015R(@@rtcd);

         Select;
         // ICD10 is inactive
         When @@rtcd = '1';
         xxicd10 = '0';
         dspICdind = '1';

         // ICD10 is active
         When @@rtcd = '2';
         xxicd10 = '1';
         dspICdind = '0';
         // Beg Add *** 08/05/13 ****************************************
         // Beg Add *** 10/01/15 ****************************************
         If @@mode = 'A';
         // End Add *** 10/01/15 ****************************************
           scICdInd ='Y';
         // Beg Add *** 10/01/15 ****************************************
         Endif;
         // End Add *** 10/01/15 ****************************************
         // End Add *** 08/05/13 ****************************************
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;

      /End-Free
     C*** End Add *** 05/23/13 ****************************************
       //*** Beg Add *** 06/27/19 *************************************
      /Free
         isPbillsLite = isModEnabled('Pbills Lite');
      /End-Free
       //*** End Add *** 06/27/19 *************************************
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EDUSER
     C                   MOVEL     Q1PGM         EDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EDDT
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C** UPDATE EOMB TRANSACTION FILE
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEGIN ADD *** 05/25/01 **************************************
     C*
     C     SCSVZP        IFEQ      *ZEROS
     C                   MOVEL     '     '       EOFLIC
     C*** BEG ADD *** 10/01/01 ****************************************
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C  N99              MOVE      P1ZIP         SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C*** BEG CHG *** 10/01/01 ****************************************
     C******************** ELSE
     C                   ENDIF
     C*** END CHG *** 10/01/01 ****************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL(P)  EOFLIC        KKZIPA            3
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*** BEG DEL *** 10/01/01 ****************************************
     C******************** ENDIF
     C*** END DEL *** 10/01/01 ****************************************
     C*
     C*** END   ADD *** 05/25/01 **************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      SCREVI        EOREVI
     C*** END ADD *** 03/11/10 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* If in ADD mode and @@RRN (FAEDP) is not zeros, then the user
     C* has used F12 from detail screen.  Must delete previous written
     C* Header record and write the new one.  This prevents having two
     C* Header records for the bill.
     C*
     C     @@RRN         IFNE      *ZEROS
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    DBRCDF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@RRN NE *ZEROS
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   Z-ADD     EOEOB#        EDEOB#
     C                   MOVE      'A'           EDSTAT
     C                   MOVEL     Q1USER        EDUSER
     C                   MOVEL     Q1USER        EDIUSR
     C                   MOVEL     Q1PGM         EDPGM
     C                   MOVEL     Q1PGM         EDIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EDTIME
     C                   Z-ADD     DSTIME        EDITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EDDT
     C                   MOVEL     SRDTO         EDIDT
     C*
     C                   WRITE     DBRCDF
     C*** BEG ADD *** 04/05/11 ****************************************
     C     EDKY          CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD *** 04/05/11 ****************************************
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 10/01/01 ****************************************
     C     SCSVZP        IFEQ      *ZEROS
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C  N99              MOVE      P1ZIP         SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDIF
     C*** END ADD *** 10/01/01 ****************************************
     C*** BEGIN ADD *** 05/25/01 **************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL(P)  SCSVZP        KKZIPA            3
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*** END   ADD *** 05/25/01 **************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      SCREVI        EOREVI
     C*** END ADD *** 03/11/10 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEDP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
