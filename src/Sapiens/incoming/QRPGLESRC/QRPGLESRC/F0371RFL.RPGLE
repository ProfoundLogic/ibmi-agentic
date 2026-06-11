     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0371RFL
     H*  Description: Florida Pharmacy Header Info update/display
     H*  Copied from F0371R: 04/15/04  C:79 Project: FL046
     H*****************************************************************
     H*  REVISIONS
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  06/27/19 C:079 Project: 152MAP                               *
     H*                 Relax edits if Pbills lite is installed       *
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disable function code F11 for GS access to Bill Entry
     H*                                                               *
     H*   04/05/11 C:257 Support 71931                                *
     H*                  Assigning RRN to @@RRN after WRITEDBRCDF to  *
     H*                  resolve duplicate records in file FAEHP.     *
     H*
     H*   10/12/10 C:257 Support: 74311                               *
     H*                  Allow entry of ZZ99999999999 for the         *
     H*                  'Pharmacists state license number.' when the *
     H*                  vlaue of "Payment type:" = S.                *
     H*
     H*   10/06/10 C:251 Support: 74192                               *
     H*                  Relaxed edit to allow 'PS' prefix when type  *
     H*                  = 'S'
     H*
     H*   07/12/10 C:079 Support: 73533                               *
     H*                  Do not validate license prefix in this pgm   *
     H*
     H*   06/22/10 C:227 Project: FL083 Priss 33                      *
     H*                  Relaxed edit for license#                    *
     H*                                                               *
     H*   04/26/10 C:079 PROJECT: FL083                               *
     H*                  Removed license edit NU, PU, DME             *
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
     H*  12/24/07  C:198  SUPP 61321
     H*                   SCPLIC made mandatory when processed on or
     H*                   after 01012008, validated SCPLIC to be in
     H*                   DMENNNNNNNNNN format when payment type =
     H*                   'S'. It is displayed in high intensity
     H*                   when required.
     H*
     H*  08/29/07  C:180  SUPP 59201
     H*                   Validate SCPLIC based on following formats:
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
     H*  04/20/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H* 03/21/07  C:182  Project: FS297
     H*                  Set EDI flag = N if license# is invali
     H*
     H* 03/14/07  C:182  Project: FL067
     H*                  1. Added Pharmacists State License Number
     H*                  2. Changed Fld Heading for 'Payment plan'
     H*                     to be 'Payment code'.
     H*                         
     H* 12/22/06  C:182  PROJECT: FS294
     H*                  ADDED SEARCH OPTION TO FIELD SCEDI
     H*
     H*  04/12/06  C:163  PROJECT: MC099 52302
     H*                   Pharmacy Bills editing on protected fields
     H*
     H*  03/14/06  C:163  PROJECT: MC010 51553
     H*                   PHARMACY NUMBER NO LONGER REQUIRED
     H*
     H*  01/10/06  C:163  PROJECT: MC270
     H*                   RX NEW FIELD - "MEDICAL SUPPLIES"
     H*
     H*  10/31/05  C:163  PROJECT: FL056
     H*                   FL EDI SCREEN CHANGES
     F*
     H*  04/15/04 C:079 PROJECT: FL046
     H*                 1. CHANGE EDIT ON ELIGIBLE FOR DATA SUBMISSION
     H*                 2. ADD DESCRIPTION FIELD
     H*                 2. ADD STATEMENT DATE
     H*
     H*  02/21/02 C:062 PROJECT: MC250
     H*                 REQUIRE PHARMACY NUMBER IF FLORIDA BILL.
     H*
     H*  07/06/01 C:079 PROJECT: FSMNT #196
     H*                 ADD EDIT TO SPECIALTY CODE
     H*
     H*  05/30/01 C:062 PROJECT: FS229
     H*                 POPULATE EOMB# SCREEN FIELD
     H*
     H*  05/24/01 C:062 Support MCMNT #190
     H*                 COMPILE ONLY
     H*
     H*  10/13/00 C:062 Support MC238
     H*                 ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  02/21/00  C:117  PROJ: FS208
     H*                   ADDED LOGIC TO PROTECT SPECIALTY IF ON A
     H*                   RECON; ALSO SAVES SPECIALTY.
     H*
     H*  11/12/99 C:79  MC152
     H*                 Delete/Change efficiency isssue.
     H*
     H*  09/10/99 C:56  Project: KY010
     H*                 Write SCEDI to EOTAPE
     H*
     H*****************************************************************
     H* INDICATORS - SCR01
     H*  09-    INQUIRY MODE
     H*  16-    REQUIRE PHARMACY NUMBER
     H*  29-    RECON (ALL SCREENS)
     H*  30-    ELIGIBLE FOR DATA SUBMISSION (ALL SCREENS)
     H*  36-37  CURSOR POSITIONING
     H*  47-    NETWORK
     H*  51-52  INVALID SEARCH FIELD
     H*  68-    DISPLAY EOB/NOTES
     H*  69-    DISPLAY NETWORK
     H*  78-84  ERROR
     H*  89     ERROR
     H*  90-    COPYBOOK ERROR
     H*  98-99  GENERAL CHAIN
     H*****************************************************************
     H*
     FF0371FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEPP     UF A E             DISK
     F                                     RENAME(PEP01:DBRCDF)
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
     FFAEPL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F*** BEG ADD *** 04/15/04 ****************************************
     FFTGTL     IF   E           K DISK
     F*** END ADD *** 04/15/04 ****************************************
     F*** BEG ADD *** 10/31/05 ****************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END ADD *** 10/31/05 ****************************************
     F*** Beg Add *** 07/18/25 ***************************************
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*** End Add *** 07/18/25 ***************************************
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
    D*** BEG ADD *** 03/14/07 ****************************************
     D*
     D*OPERATING PHYSICIANS LICENSE NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCPLIC                 1     13
     D  SCPLC1                 1      1
     D  SCPLC2                 2      2
     D  SCPLC3                 3      3
     D  SCPLC4                 4      4
     D*** BEG DEL *** 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****¢¢¢¢
     D*****¢¢¢¢¢                              5   9 SCPLC5
     D*****¢¢¢¢¢                              4   6 SCPLC6
     D*****¢¢¢¢¢                              3   9 SCPLC7
     D*** END DEL *** 08/29/07 ****************************************
     D  @SCPL8                 1      8
     D  @SCPL5                 5     13
     D  @SCPL2                 1      2
     D  @SCPL3                 1      3
    D*** END ADD *** 03/14/07 ****************************************
     D*** BEG ADD *** 08/29/07 ****************************************
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*** END ADD *** 08/29/07 ****************************************
    D*** BEG ADD *** 12/24/07 ****************************************
     D  SCLICD                 9     13
    D*** END ADD *** 12/24/07 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pharmacy Header Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
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
     C                   PARM                    @@RRN             9 0          for FAEPP
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
     C** Key List for Fee Schedule Control File
     C*
     C********** SBKY      KLIST
     C**********           KFLD           KKKEY
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
     C     EPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END ADD *** 04/15/04 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        ##SUB#
     C     *LIKE         DEFINE    STKEY         ##KEY
     C*** BEG ADD *** 04/15/04 ****************************************
     C     *LIKE         DEFINE    EOCRDT        HLSTDT
     C*** END ADD *** 04/15/04 ****************************************
     C     *LIKE         DEFINE    EPCO#         KKCO#
     C     *LIKE         DEFINE    EPSUB#        KKSUB#
     C     *LIKE         DEFINE    EPFND         KKFND
     C     *LIKE         DEFINE    EPFYR         KKFYR
     C     *LIKE         DEFINE    EPCASE        KKCASE
     C********** *LIKE     DEFN FBKEY     KKKEY
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*** BEG ADD *** 04/15/04 ****************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 10/31/05 ****************************************
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C*** END ADD *** 10/31/05 ****************************************
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
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* STATEMENT DATE IS REQUIRED
     C*
    C*** BEG DEL **** 04/02/07 ***************************************
     C****************           SCEDI     IFNE 'Y'
     C****************           SCEDI     ANDNE'N'
     C****************           SCSTDT    ANDEQ*ZEROS
     C****************     SETON                     8090
     C****************     GOTO CK1END
     C****************     ENDIF
    C*** END DEL **** 04/02/07 ***************************************
     C*
     C* VALIDATE STATEMENT DATE
     C*
    C*** BEG DEL ***** 03/14/07 **************************************
     C********   SCSTDT    IFNE *ZEROS
     C********
     C********             Z-ADDSCSTDT    SRDTI
     C********             CALL 'SRCVTDAT'DATCHK
     C********   SRERR     IFEQ *ON
     C********             SETON                     8190
     C********             GOTO CK1END
     C********             ELSE
     C********             MOVELSRDTO     SCSTDT
     C********             ENDIF
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     HLSTDT
     C********
     C* MUST BE >= TO ACCIDENT DATE
     C********
     C********   HLSTDT    IFLT EOACDT
     C********             SETON                     8290
     C********             GOTO CK1END
     C********             ENDIF
     C********
     C* MUST BE <= DATE CARRIER RECEIVED
     C********
     C********   HLSTDT    IFGT EOCRDT
     C********             SETON                     8490
     C********             GOTO CK1END
     C********             ENDIF
     C********
     C********             ENDIF
    C*** END DEL ***** 03/14/07 **************************************
     C*** END ADD *** 04/15/04 ****************************************
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

       //*** Beg add ***** 07/18/25 ***********************************
       If isModEnabled('NCCI Med Data Call');
          If scsvzp = *zeros and EOrcn = 0;
             ersvzp = *on;
             mssvzp = 'MCR2472';
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*** BEGIN DEL *** 03/14/06 **************************************
     C*** BEGIN ADD *** 02/21/02 **************************************
     C*
     C* PHARMACY NUMBER IS REQUIRED IF FLORIDA BILL
     C*
     C*** BEGIN ADD *** 01/10/06 **************************************
     C********** SCRXPT    IFEQ 'D'
     C********** SCRXPT    OREQ 'B'
     C*** END   ADD *** 01/10/06 **************************************
     C********** SCPHAR    IFEQ *BLANKS
     C********** EOAST     ANDEQ'FL'
     C**********           SETON                     8990
     C**********           ENDIF
     C*** BEGIN ADD *** 01/10/06 **************************************
     C**********           ENDIF                           SCRXPT='D' OR 'B'
     C*** END   ADD *** 01/10/06 **************************************
     C*** END   ADD *** 02/21/02 **************************************
     C*** END   DEL *** 03/14/06 **************************************
     C*** BEGIN ADD *** 04/12/06 **************************************
     C     *IN29         IFEQ      *OFF
     C*** END   ADD *** 04/12/06 **************************************
     C*** BEGIN ADD *** 01/10/06 **************************************
     C*
     C*  VALIDATE PAYMENT TYPE
     C*
     C*** BEGIN CHG *** 04/12/06 **************************************
     C** 30 **** SCRXPT    IFEQ *BLANKS
     C     SCRXPT        IFEQ      *BLANKS
     C*** END   CHG *** 04/12/06 **************************************
     C                   SETON                                        9094
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRXPT=' '
     C*
     C*** BEGIN CHG *** 04/12/06 **************************************
     C** 30 **** SCRXPT    IFNE *BLANKS
     C     SCRXPT        IFNE      *BLANKS
     C*** END   CHG *** 04/12/06 **************************************
     C                   MOVEL(P)  'FLPH'        KKTBID
     C*
     C                   MOVEL(P)  SCRXPT        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  SCRXPT<>' '
     C*** END   ADD *** 01/10/06 **************************************
    C*
    C*** BEG ADD ***** 03/14/07 **************************************
    C*
    C* VALIDATE PHARMACISTS STATE LICENSE NUMBER.
    C*
    C*** BEG DEL ***** 03/14/07 **************************************
     C*****      SCPLIC    IFEQ *BLANKS
     C*****      SCRXPT    ANDNE'S'
     C*****                SETON                     9042
     C*****                GOTO CK1END
     C*****
     C*****                ELSE
    C*** END DEL ***** 03/14/07 **************************************
     C*
    C*** BEG ADD ***** 12/24/07 **************************************
     C** DETERMINE IF LICENSE NUMBER SHOULD BE REQUIRED
     C*
     C     *IN29         IFEQ      *OFF
     C*
     C                   SETOFF                                       86
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     EOCRDT        IFGE      1080101
     C                   SETON                                        85
     C                   ELSE
     C     EOCRDT        IFGE      XXDT
     C                   SETON                                        85
     C                   ENDIF                                                  EOCRDT>1080101
     C                   ENDIF                                                  EOCRDT>XXDT
     C*
     C     *IN85         IFEQ      *ON
     C     SCPLIC        IFEQ      *BLANKS
     C                   SETON                                        9042
     C                   GOTO      CK1END
     C                   ELSE                                                   SCPLIC = ' '
     C     SCRXPT        IFEQ      'S'
     C*** BEG ADD *** 10/06/10 *************************************************
     C     @SCPL2        ANDNE     'PS'
     C*** END ADD *** 10/06/10 *************************************************
     C*** BEG ADD *** 06/22/10 *************************************************
     C     SCREVI        IFEQ      'E'
     C     @SCPL3        IFNE      'DME'
     C*** BEG ADD *** 10/12/10 *************************************************
     C     @SCPL2        IFNE      'ZZ'
     C                   SETON                                        9044
     C                   GOTO      CK1END
     C                   ENDIF                                                  @SCPL2
     C                   ENDIF                                                  @SCPL3
     C*** END ADD *** 10/12/10 *************************************************
     C     @SCPL3        IFEQ      'DME'
     C* Find 1st non-blank
     C     ' '           CHECKR    SCLICB        CNT              13 0
     C     CNT           IFGT      0
     C     CNT           SUBST     SCPLIC:4      @LIC#            10
     C                   MOVEA     @LIC#         LL
     C                   Z-ADD     1             IZ                2 0
     C                   DO        CNT           IZ
     C     LL(IZ)        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDDO
     C                   ELSE                                                   CNT>0
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C                   ENDIF                                                  CNT>0
     C                   ENDIF                                                  @SCPL3 = 'DME'
     C*** BEG ADD *** 10/12/10 *************************************************
     C     @SCPL2        IFEQ      'ZZ'
     C* Find 1st non-blank
     C     ' '           CHECKR    SCLICB        CNT              13 0
     C     CNT           IFGT      0
     C     CNT           SUBST     SCPLIC:3      @LIC#            10
     C                   MOVEA     @LIC#         LL
     C                   Z-ADD     1             IZ                2 0
     C                   DO        CNT           IZ
     C     LL(IZ)        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDDO
     C                   ELSE                                                   CNT>0
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C                   ENDIF                                                  CNT>0
     C                   ENDIF                                                  @SCPL2 = 'ZZ'
     C*** END ADD *** 10/12/10 *************************************************
     C                   ELSE                                                   SCREVI
     C*** END ADD *** 06/22/10 *************************************************
     C     @SCPL8        IFNE      'DME99999'
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  @SCPL8
     C     @SCPL8        IFEQ      'DME99999'
     C     SCLICD        ANDNE     '99999'
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  @SCPL8 &SCLICD
     C*** BEG ADD *** 06/22/10 *************************************************
     C                   ENDIF                                                  SCREVI
     C*** END ADD *** 06/22/10 *************************************************
     C                   ENDIF                                                  SCRXPT = 'S'
     C                   ENDIF                                                  SCPLIC = 0
     C                   ENDIF                                                  *IN85 = *ON
     C*
     C                   ENDIF                                                  *IN29 = *OFF
     C     SCRXPT        IFNE      'S'
    C*** END ADD ***** 12/24/07 **************************************
     C     SCPLIC        IFNE      *BLANKS
     C*
     C*** BEG DEL ***** 03/20/08 **************************************
     C***********          SELEC
     C*
     C***********@SCPL8    WHEQ 'DME99999'
     C***********@SCPL5    ANDEQ'99999'
     C***********          GOTO CK1END
     C***********
     C*********** SCPL2    WHEQ 'NP'
     C*********** SCPL2    OREQ 'PS'
     C*********** SCPL2    OREQ 'PU'
     C***********          GOTO CK1END
     C***********
     C*********** SCPL3    WHEQ 'DME'
     C***********          GOTO CK1END
     C***********
     C***********          OTHER
     C*** END DEL ***** 03/20/08 **************************************
      *** BEG DELETE ***** 08/29/07 *¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C***¢       SCPLC1    IFLT 'A'
     C***¢       SCPLC1    ORGT 'Z'
     C***¢                 SETON                     9044
     C***¢                 GOTO CK1END
     C***¢                 ENDIF                           SCOLC1 < 'A'
     C***¢
     C***¢       SCPLC2    IFLT 'A'
     C***¢       SCPLC2    ORGT 'Z'
     C***¢                 SETON                     9044
     C***¢                 GOTO CK1END
     C***¢                 ENDIF
     C***¢
     C* 3rd position can be alpha or numeric
     C***¢
     C***¢                 TESTN          SCPLC3     60
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢
     C***¢       SCPLC3    IFLT 'A'
     C***¢       SCPLC3    ORGT 'Z'
     C***¢
     C***¢                 SETON                     9049
     C***¢                 GOTO CK1END
     C***¢
     C***¢                 ENDIF                           SCOLC3 IFLT
     C***¢
     C***¢                 ENDIF                           *IN60 = 0
     C***¢
     C* 4TH position can be alpha or numeric
     C***¢
     C***¢                 TESTN          SCPLC4     60
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢
     C***¢       SCPLC4    IFLT 'A'
     C***¢       SCPLC4    ORGT 'Z'
     C***¢
     C***¢                 SETON                     9050
     C***¢                 GOTO CK1END
     C***¢
     C***¢                 ENDIF                           SCOLC4 IFLT
     C***¢
     C***¢                 ENDIF                           *IN60 = 0
     C***¢
     C***¢The last 7 digits MUST BE numeric
     C***¢
     C***¢                 SELEC
     C***¢       SCPLC3    WHGE 'A'
     C***¢       SCPLC3    ANDLE'Z'
     C***¢       SCPLC4    ANDGE'A'
     C***¢       SCPLC4    ANDLE'Z'
     C***¢
     C***¢                 TESTN          SCPLC5     60
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢                 SETON                     9048
     C***¢                 GOTO CK1END
     C***¢                 END
     C***¢
     C***¢       SCPLC3    WHGE 'A'
     C***¢       SCPLC3    ANDLE'Z'
     C***¢       SCPLC4    ANDGE'0'
     C***¢       SCPLC4    ANDLE'9'
     C***¢                 TESTN          SCPLC6     60
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢                 SETON                     9046
     C***¢                 GOTO CK1END
     C***¢                 END
     C***¢
     C***¢       SCPLC3    WHGE '0'
     C***¢       SCPLC3    ANDLE'9'
     C***¢       SCPLC4    ANDGE'0'
     C***¢       SCPLC4    ANDLE'9'
     C***¢                 TESTN          SCPLC7     60
     C***¢
     C***¢       SCPLC3    WHGE '0'
     C***¢       SCPLC3    ANDLE'9'
     C***¢       SCPLC4    ANDGE'A'
     C***¢       SCPLC4    ANDLE'Z'
     C***¢                 TESTN          SCPLC5     60
     C***¢
     C***¢                 OTHER
     C***¢                 TESTN          SCPLC5     60
     C***¢
     C***¢                 ENDSL
     C***¢
     C***¢       *IN60     IFEQ '0'
     C***¢                 SETON                     9045
     C***¢                 GOTO CK1END
     C***¢                 END
      *** END DELETE ***** 08/29/07 *¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*
     C****** BEG ADD ******* 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCPLC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9044
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCPLC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9044
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCPLC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCPLC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9044
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C     SCPLC4        IFGT      *BLANKS
     C***** END ADD *******  10/04/08 *********************************
      *
     C     SCPLC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9044
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40
     C     SCPLC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9044
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
     C                   SETON                                        9045
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9045
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C****** END ADD ******* 08/29/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C*
     C*** BEG DEL ***** 03/20/08 **************************************
     C************         ENDSL
     C*** END DEL ***** 03/20/08 **************************************
     C*
     C                   ENDIF                                                  SCPLIC <> *BLANKS
     C*
    C*** END ADD ***** 03/14/07 **************************************
    C*** BEG ADD ***** 12/24/07 **************************************
     C                   ENDIF                                                  SCRXPT <> 'S'
    C*** END ADD ***** 12/24/07 **************************************
     C*
    C*** BEG DEL ***** 03/14/07 **************************************
     C*****                ENDIF                           SCPLIC =' '
    C*** END DEL ***** 03/14/07 **************************************
    C*
    C*** END ADD ***** 03/14/07 **************************************
    C*
     C*** BEGIN ADD *** 10/31/05 **************************************
     C*
     C*  VALIDATE PAYMENT PLAN
     C*
     C*** BEGIN ADD *** 01/10/06 **************************************
     C     SCRXPT        IFEQ      'D'
     C     SCRXPT        OREQ      'B'
     C*** END   ADD *** 01/10/06 **************************************
     C*** BEGIN CHG *** 04/12/06 **************************************
     C** 30 **** SCPPLN    IFEQ *BLANKS
     C     SCPPLN        IFEQ      *BLANKS
     C*** END   CHG *** 04/12/06 **************************************
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPPLN IFEQ BLANK
     C*** BEGIN ADD *** 01/10/06 **************************************
     C                   ENDIF                                                  SCRXPT='D' OR 'B'
     C*** END   ADD *** 01/10/06 **************************************
     C*
     C*** BEGIN CHG *** 04/12/06 **************************************
     C** 30 **** SCPPLN    IFNE *BLANKS
     C     SCPPLN        IFNE      *BLANKS
     C*** END   CHG *** 04/12/06 **************************************
     C*
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9093
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*** END   ADD *** 10/31/05 **************************************
     C*** BEGIN ADD *** 04/12/06 **************************************
     C                   ENDIF                                                  *IN29=*OFF
     C*** END   ADD *** 04/12/06 **************************************
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN PGMSTR SUBROUINTE)
     C*
     C   30SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
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
     C                   MOVEL(P)  'FLES'        KKTBID
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
     C*** BEG ADD *** 03/11/10 **************************************
     C*
     C*  VALIDATE THE REVISION INDICATOR
     C*
     C     SCREVI        IFEQ      *BLANK
     C                   SETON                                        9052
     C                   GOTO      CK1END
     C                   ELSE
     C*
     C                   MOVEL(P)  'REVI'        KKTBID
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
    C*** Beg Add *** 06/27/19 ****************************************
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scdnpi));
          If invalidPos <> 0 or scdnpi = *blanks;
             msdnpi = 'MC50007';
             erdnpi = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
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
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C*** BEGIN CHG *** 10/31/05 **************************************
     C******************** SETOF                     37
     C                   SETOFF                                       3738
     C*** END   CHG *** 10/31/05 **************************************
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   SELECT
     C*
     C*
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     SCSPCD        HLSPCD            3
     C                   CLEAR                   ##KEY1
     C                   CALL      'F2335R'
     C                   PARM      '2'           ##CALL            1
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
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCSVZP'
     C                   EVAL       *IN90  =*on
     C                   EVAL       ersvzp =*on
     C                   EVAL       mssvzp ='MCR0676'
     C     HFLD          WHENEQ    'SCDNPI'
     C                   EVAL       *IN90  =*on
     C                   EVAL       erdnpi =*on
     C                   EVAL       msdnpi ='MCR0676'
     C*** End Add *** 07/18/25 ***************************************
     C*** BEGIN ADD *** 01/10/06 **************************************
     C     HFLD          WHENEQ    'SCRXPT'
     C*
     C                   MOVEL(P)  'FLPH'        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9040
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCRXPT
     C                   MOVEL(P)  GTDESC        SCRXPD
    C*
    C*** BEG ADD ***** 03/14/07 **************************************
    C*
     C*****************    SETON                     9038
     C                   SETON                                        9041
    C*
    C*** END ADD ***** 03/14/07 **************************************
    C*
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*
     C*** END   ADD *** 01/10/06 **************************************
    C*
    C*** BEG ADD ***** 03/14/07 **************************************
    C*
     C     HFLD          WHENEQ    'SCPLIC'
     C*
     C                   SETON                                        9043
    C*
    C*** END ADD ***** 03/14/07 **************************************
    C*
     C*** BEGIN ADD *** 10/31/05 **************************************
     C     HFLD          WHENEQ    'SCPPLN'
     C*
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        ##TBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        ##TBID
     C                   ENDIF                                                  SCEDI IFEQ 'Y'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9038
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPPLN
     C                   MOVEL(P)  GTDESC        SCPDSC
    C*** BEG CHG ***** 03/11/10 **************************************
     C***********          SETON                     9039
     C                   SETON                                        9055
    C*** END CHG ***** 03/11/10 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
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
    C*** BEG CHG ***** 03/11/10 **************************************
     C************         SETOF                     39
     C                   SETON                                        54
    C*** END CHG ***** 03/11/10 **************************************
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
    C*** BEG CHG ***** 03/11/10 **************************************
     C***********          SETOF                     39
     C                   SETON                                        55
    C*** END CHG ***** 03/11/10 **************************************
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
    C*** END ADD ***** 12/22/06 **************************************
     C*** END   ADD *** 10/31/05 **************************************
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
     C*** BEGIN ADD *** 10/13/00 **************************************
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPHAR'
     C                   SETON                                        51
    C*** BEG DEL ***** 12/22/06 **************************************
     C*********  HFLD      WHEQ 'SCEDI'
     C*********            SETON                     52
    C*** END DEL ***** 12/22/06 **************************************
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*** END   ADD *** 10/13/00 **************************************
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0
     C                   CALL      'F0355R'
     C                   PARM      ' '           ##CALL
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
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   CLEAR                   SCREVI
     C*** END ADD *** 03/11/10 ****************************************
     C*** BEG ADD *** 05/25/04 ****************************************
    C*** BEG DEL *** 03/14/07 ****************************************
     C************         Z-ADDEOCSDT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDYY
     C************         MOVELSRDTO     SCSTDT
    C*** END DEL *** 03/14/07 ****************************************
     C*** END ADD *** 05/25/04 ****************************************
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
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA
     C*** Beg Add *** 07/18/25 ***************************************
     C     EOFLIC        IFNE      *BLANKS
     C                   MOVEL     EOFLIC        SCSVZP
     C                   MOVE      EOFLIC        SCZIP
     C                   ELSE
     C     PDKY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C  N99              MOVE      P1ZIP         SCZIP
     C                   ENDIF
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*****      @@MODE    IFNE 'A'
     C*
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
     C*
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     EPDNPI        SCDNPI
     C*** End Add *** 07/18/25 ***************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL     EOREVI        SCREVI
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     BEREVI        SCREVI
     C                   ENDIF                                                  @@MODE=A
     C*** END ADD *** 03/11/10 ****************************************
     C*** BEG ADD *** 01/10/06 ****************************************
     C     @@MODE        IFEQ      'A'
     C     @@RRN         ANDEQ     *ZEROS
     C                   MOVE      'D'           EPRXPT
     C                   ENDIF                                                  @@MODE=A
     C*
     C     EPRXPT        IFNE      *BLANKS
     C                   MOVE      EPRXPT        SCRXPT
     C*
     C                   MOVEL(P)  'FLPH'        KKTBID
     C*
     C                   MOVEL(P)  SCRXPT        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRXPD
     C                   ELSE
     C                   CLEAR                   SCRXPD
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  EPRXPT IFNE BLANK
     C*** END ADD *** 01/10/06 ****************************************
     C*** BEG ADD *** 10/31/05 ****************************************
     C     EPPPLN        IFNE      *BLANKS
     C                   MOVE      EPPPLN        SCPPLN
     C*
     C     EPEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF                                                  EPEDI IFEQ 'Y'
     C*
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  EPPPLN IFNE BLANK
     C*** END ADD *** 10/31/05 ****************************************
    C*
    C*** BEG ADD ***** 03/14/07 **************************************
    C*
     C     EPPLIC        IFNE      *BLANKS
     C*
     C                   MOVE      EPPLIC        SCPLIC
     C*
     C                   ENDIF                                                  EPPLIC IFNE BLANK
    C*
    C*** END ADD ***** 03/14/07 **************************************
    C*
     C     EPEDI         IFNE      *BLANKS
     C                   MOVE      EPEDI         SCEDI
     C*** BEG ADD *** 04/15/04 ****************************************
     C*** BEG CHG *** 04/20/07 ****************************************
     C*****      EOAST     IFEQ 'FL'
     C     EDITST        IFEQ      'FL'
     C*** END CHG *** 04/20/07 ****************************************
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCEDI         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C                   ENDIF                                                  EPEDI NE *BLANKS
     C                   MOVEL     EPPHAR        SCPHAR
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C*
     C*****                ENDIF                           @@MODE NE 'A'
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*** BEG ADD *** 03/20/08 ****************************************
     C                   MOVE      *OFF          LICCHG            1
     C     SCPLIC        IFNE      EPPLIC
     C                   MOVE      *ON           LICCHG
     C                   ENDIF
     C*** END ADD *** 03/20/08 ****************************************
     C*
     C                   Z-ADD     EOCO#         EPCO#
     C                   Z-ADD     EOSUB#        EPSUB#
     C                   Z-ADD     EOFND         EPFND
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   MOVE      EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
     C*** Beg Add *** 07/18/25 ***************************************
     C     SCSVZP        IFEQ      *ZEROS
     C                   MOVEL     '     '       EOFLIC
     C     PDKY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C  N99              MOVE      P1ZIP         SCZIP
     C                   ENDIF
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C                   MOVEL     SCDNPI        EPDNPI
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*** BEG ADD *** 10/31/05 ****************************************
     C                   MOVE      SCPPLN        EPPPLN
     C*** BEG ADD ***** 03/20/08 **************************************
     C*
     C     SCPLC1        CAT       SCPLC2        XXLIC2            2
     C     XXLIC2        CAT       SCPLC3        XXLIC3            3
     C*
     C                   MOVEL(P)  SCPLIC        SCLIC#           13
     C                   MOVE      EOBTYP        @@BTYP            1
     C*** BEG ADD *** 03/11/10 **************************************
     C                   MOVE      EOFRDT        @@SDTE
     C*** END ADD *** 03/11/10 **************************************
     C*
     C*** END ADD ***** 03/20/08 **************************************
     C*** BEG ADD ***** 03/21/07 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C**********           CALL 'FA746R'
     C**********           PARM           SCAST
     C**********           PARM           SCPLIC
     C**********           PARM ' '       @@RTCD  1
     C*** END DEL ***** 07/12/10 **************************************
     C*** BEG ADD ***** 03/20/08 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C**********           PARM           @@BTYP  1
     C*** END DEL ***** 07/12/10 **************************************
     C*** END ADD ***** 03/20/08 **************************************
     C*** BEG ADD ***** 03/11/10 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C*********            PARM           @@SDTE
     C*** END DEL ***** 07/12/10 **************************************
     C*** END ADD ***** 03/11/10 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C*********
     C*********  @@RTCD    IFEQ 'Y'
     C*** END DEL ***** 07/12/10 **************************************
     C*** BEG ADD ***** 03/20/08 **************************************
     C*** BEG DEL ***** 04/26/10 **************************************
     C**********
     C********** XXLIC3    ANDNE'DME'
     C********** @@RTCD    OREQ 'N'
     C********** XXLIC2    ANDNE'PU'
     C********** XXLIC2    ANDNE'PS'
     C********** XXLIC2    ANDNE'NP'
     C*** END DEL ***** 04/26/10 **************************************
     C*** END ADD ***** 03/20/08 **************************************
     C*** BEG DEL ***** 07/12/10 **************************************
     C*********
     C*********            MOVE 'N'       SCEDI
     C*********            ELSE
     C*********  LICCHG    IFEQ *ON
     C*********            MOVE 'E'       SCEDI
     C*********            ENDIF
     C*********            ENDIF                           XXLIC2 & XXLIC3
     C*** END DEL ***** 04/26/10 **************************************
     C*** END ADD ***** 03/21/07 **************************************
     C*** END ADD *** 10/31/05 ****************************************
     C                   MOVE      SCEDI         EPEDI
     C                   MOVE      SCPHAR        EPPHAR
     C*** BEG ADD *** 01/10/06 ****************************************
     C                   MOVE      SCRXPT        EPRXPT
     C*** END ADD *** 01/10/06 ****************************************
    C*** BEG ADD *** 03/14/07 ****************************************
     C                   MOVE      SCPLIC        EPPLIC
    C*** END ADD *** 03/14/07 ****************************************
     C*** BEG ADD *** 02/21/00 ****************************************
     C                   MOVE      SCSPCD        EOSPCD
     C*** END ADD *** 02/21/00 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      SCREVI        EOREVI
     C*** END ADD *** 03/11/10 ****************************************
     C*** BEG ADD ***** 04/26/10 **************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD ***** 04/26/10 **************************************
     C*
     C*****                Z-ADDSCCHRG    EOCHRG
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
       //*** Beg add ***** 07/18/25 ************************************
       ersvzp = *off;
       mssvzp = *blanks;
       erdnpi = *off;
       //*** End add ***** 07/18/25 ************************************
     C*** BEGIN ADD *** 10/31/05 **************************************
     C                   SETOFF                                       9293
     C*** END   ADD *** 10/31/05 **************************************
     C*** BEGIN ADD *** 01/10/06 **************************************
     C                   SETOFF                                       9495
     C*** END   ADD *** 01/10/06 **************************************
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
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
     C     EPKY          CHAIN     PEP01                              99
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
     C*********
     C*********  EOBLST    IFEQ 'U'
     C*********  *INKL     ANDNE*ON
     C*********  ##RTCD    ANDNE'P'
     C*********            Z-ADD@@RRN2    ##RRN
     C*********            CALL 'FA713R'
     C*********            PARM ' '       ##KC
     C*********            PARM ' '       ##KL
     C*********            PARM           ##RRN
     C*********            ENDIF                           EOBLST = 'U'
     C*** END DEL *** 11/12/99 ****************************************
     C*
     C*** BEGIN ADD *** 10/31/05 **************************************
     C                   SETOFF                                       9293
     C*** END   ADD *** 10/31/05 **************************************
     C*** BEGIN ADD *** 01/10/06 **************************************
     C                   SETOFF                                       9495
     C*** END   ADD *** 01/10/06 **************************************
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
     C** AND REQUIRED
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
     C                   MOVEL     EOTAPE        SCEDI
     C*
     C     BEKY          CHAIN     PBE01                              98
     C     SCEDI         IFEQ      *BLANKS
     C  N98              MOVE      BEEDI         SCEDI
     C                   ENDIF
     C*** BEG ADD *** 10/31/05 ****************************************
     C  N98              MOVE      BEPPLN        SCPPLN
     C*** END ADD *** 10/31/05 ****************************************
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
    C*** BEG DEL ***** 03/14/07 **************************************
     C*
     C* SET INITIAL CURSOR POSITON TO STATEMENT DATE
     C*
     C************         SETON                     37
    C*** END DEL ***** 03/14/07 **************************************
    C*** BEG ADD ***** 03/14/07 **************************************
     C*
     C* SET INITIAL CURSOR POSITON TO PHARMACY NUMBER
     C*
     C                   SETON                                        37
    C*** END ADD ***** 03/14/07 **************************************
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
    C*** BEG ADD ***** 12/24/07 **************************************
     C** DETERMINE IF LICENSE NUMBER SHOULD BE REQUIRED
     C*
     C     *IN29         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     EOCRDT        IFGE      1080101
     C                   SETON                                        85
     C                   ELSE
     C     EOCRDT        IFGE      XXDT
     C                   SETON                                        85
     C                   ENDIF                                                  EOCRDT>XXDT
     C                   ENDIF                                                  EOCRDT>1080101
     C                   ENDIF                                                  *IN29 = *OFF
    C*** END ADD ***** 12/24/07 **************************************
     C*** BEGIN DEL *** 03/14/06 **************************************
     C*** BEGIN ADD *** 02/21/02 **************************************
     C*
     C* Make pharmacy number required if Florida Bill
     C*
     C**********           SETOF                     16
     C********** EOAST     IFEQ 'FL'
     C**********           SETON                     16
     C**********           ENDIF
     C*
     C*** END   ADD *** 02/21/02 **************************************
     C*** END   DEL *** 03/14/06 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* PROTECT ELIG FOR DATA SUBMISSION IF SENT TO THE STATE ALREADY
     C* IT CAN BE CHANGE IN THE EDI CORRECTION SCREENS
     C*
     C*** BEG CHG *** 04/20/07 ****************************************
     C*****      EOAST     IFEQ 'FL'
     C     EDITST        IFEQ      'FL'
     C*** END CHG *** 04/20/07 ****************************************
     C     EOTAPE        ANDNE     ' '
     C     EOTAPE        ANDNE     'Y'
     C     EOTAPE        ANDNE     'N'
     C     EOTAPE        ANDNE     'E'
     C                   SETON                                        25
     C                   ELSE
     C                   SETOFF                                       25
     C                   ENDIF
     C*
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCEDI         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDNE     'I'
     C                   MOVEL(P)  GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEGIN ADD *** 10/31/05 **************************************
     C*
     C     SCPPLN        IFNE      *BLANKS
     C*
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF                                                  SCEDI IFEQ 'Y'
     C*
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  SCPPLN IFNE BLANK
     C*** END   ADD *** 10/31/05 **************************************
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
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C** UPDATE EOMB TRANSACTION FILE
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*** BEG ADD *** 04/15/04 ****************************************
    C*** BEG DEL *** 03/14/07 ****************************************
     C************         Z-ADDSCSTDT    SRDTI
     C************         CALL 'SRCVTDAT'DTCYMD
     C************         MOVELSRDTO     EOCSDT
    C*** END DEL *** 03/14/07 ****************************************
     C*** END ADD *** 04/15/04 ****************************************
     C*** END ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      SCREVI        EOREVI
     C*** END ADD *** 03/11/10 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* If in ADD mode and @@RRN (FAEPP) is not zeros, then the user
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
     C                   Z-ADD     EOEOB#        EPEOB#
     C                   MOVE      'A'           EPSTAT
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1PGM         EPPGM
     C                   MOVEL     Q1PGM         EPIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C                   Z-ADD     DSTIME        EPITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     SRDTO         EPIDT
     C*
     C                   WRITE     DBRCDF
     C*** BEG ADD *** 04/05/11 ****************************************
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD *** 04/05/11 ****************************************
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C******               Z-ADDSCCHRG    EOCHRG
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   MOVE      SCEDI         EOTAPE
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
    C*** BEG DEL *** 03/14/07 ****************************************
     C*********            Z-ADDSCSTDT    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EOCSDT
    C*** END DEL *** 03/14/07 ****************************************
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      SCREVI        EOREVI
     C*** END ADD *** 03/11/10 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEPP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
