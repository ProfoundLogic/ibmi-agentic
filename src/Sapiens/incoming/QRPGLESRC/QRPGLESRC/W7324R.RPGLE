     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7324R - Deductible Invoice Reprint Prompt
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*  Provides the ability to reproduce the deductible invoices    *
     H*  and detail report as of a specific loss date.  The posting   *
     H*  date is a required filed and will be edited to verify that   *
     H*  the deductible were posted on that date.  If no record is    *
     H*  found for the entered posting date, an error message is      *
     H*  displayed.  The deductible invoice flag is a required field  *
     H*  (Y/N).  If the deductible invoice flag is 'Y' then the print *
     H*  date is required.  The deductible detail flag is a required  *
     H*  filed (Y/N).                                                 *
     H*                                                               *
     H* CALLED FROM: Accounting Processing menu                       *
     H*                                                               *
     H* CALLS TO:  W7324C - Generate Deductible Billing Report and    *
     H*                     Invoices                                  *
     H* CREATED:                                                      *
     H*                                                               *
     H* 08/26/99 C:112 PROJECT WC921                                  *
     H*                Created a Screen Prompt to allow users to      *
     H*                reprint deductible invoices and detail report  *
     H*                as of a specific loss date                     *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/20/25 C:333 Project: WC2494                                *
     H*                Deductible Invoices and Detail Report in Scriptura
     H*                DSPF: Checked(Y) Deductible invoice/detail Report
     H*                 Checkbox when Scriptura is flagged.           *
     H*                - indicator disChkBox added for scdcin/scddrp. *
     H*                                                               *
     H* 01/13/25 C:344 Project: WC2989                                *
     H*                Added capability to pass-thru/ call the newer  *
     H*                Deductible Billing Reprint prompt program.     *
     H*                Corrected error indicator for the 'Excluded    *
     H*                policies with no activity is not selected' in  *
     H*                the display file.
     H*                                                               *
     H* 12/03/15 C:136 Support: 34655                                 *
     H*                Add indicator to control display of deductible *
     H*                type text box. It should display only when the *
     H*                large deductible module is enabled             *
     H*                                                               *
     H* 11/24/11  C:282 Project: WC2003                               *
     H*                 ADD PROMPT FOR PRINT OF SSN.                  *
     H*                                                               *
     H* 02/01/10  C:33  Project: 25WRB                                *
     H*                -Change to use group select processing         *
     H*                                                               *
     H* 01/11/10  C:33  Project: WC1838                               *
     H*                 Process Large & Small deductibles             *
     H*                                                               *
     H* 07/03/07 C:132 PROJECT 71WHI                                  *
     H*              - Add parm &MODE for 09/27/06 code for 71WHI     *
     H*              - Will load parm with "D" for "demand" mode      *
     H*                                                               *
     H* 09/29/06 C:132 PROJECT 93WGL                                  *
     H*                Add parm for excluding policies w/no activity. *
     H*                                                               *
     H* 04/13/06 C:58  PROJECT WC1321                                 *
     H*                Add workfile identification caption.           *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71 - 80  Used for error messaging                            *
     H*  85 - 88  Used for positioning cursor                         *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     H*
     FW7324F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMFNP     UF   E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     F* Table file
     FWTABL     IF   E           K DISK
     F* Deductible Detail File
     FWDDDL2    IF   E           K DISK
     F***  Begin Add  ***  02/01/10  **************************************
     FWXGSL     IF   E           K DISK
     F                                     RENAME(PGS01:PGS00)
     F***  End   Add  ***  02/01/10  **************************************
     F***  Begin Add  ***  01/11/10  **************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SSY01)
     F***  End   Add  ***  01/11/10  **************************************
      *** Beg Add ***** 01/20/25 **************************************
      * Admin/Darts Control File
     FWMEXL     IF   E           K DISK    RENAME(PEX01:PEX00)
      *** End Add ***** 01/20/25 **************************************
      *** Beg Add ***** 01/13/25 **************************************
     d DedBlRep        pr                  Extpgm('WTDEDBLREP')
      *** End Add ***** 01/13/25 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Deductible Invoice R-
     D                                     eprint')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D #OFSCR          C                   CONST(1)
      *** Beg Add ***** 01/20/25 *****************************
     D/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     D processType     c                   const('Deductible Invoice')
     D xxProcType      s            100
     D xxSubmit        s              1n
     D xxMass          s              1
     D
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     D xxRMCD                         5    overlay(svlda:252)
     D xxCopy                         1    overlay(svlda:257)
     D xxPrint                        1    overlay(svlda:258)
     D xxPrinter                     10    overlay(svlda:259)
     D xxTray                        50    overlay(svlda:269)
     D xx#CopiesA                     3    overlay(svlda:319)
     D xxEmail                        1    overlay(svlda:322)
     D xxToEmail                    200    overlay(svlda:323)
     D xxSubject                    150    overlay(svlda:523)
     D xxBody                       350    overlay(svlda:673)
      *** End Add ***** 01/20/25 *****************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @#MODE            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPODT
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    DDPODT        XXPODT
     C     *LIKE         DEFINE    DDFND         XXFND
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    @#MODE        @@MODE
     C*
     C/COPY SCOPYMT01C
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C*====================================================================
     C* Convert dates from 6-8, if needed
     C*====================================================================
     C     CVTDTS        BEGSR
     C*
     C* Posting date
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPODT
     C                   ENDIF
     C*
     C* Print date
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPRDT
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  01/11/10  **************************************
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
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 90 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
     C*
     C*  Posting date entered must be valid
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C* Deductible Billing Type
     C     LRGDED        IFEQ      *ON
     C*
     C     SCDDTP        IFNE      'B'
     C     SCDDTP        ANDNE     'L'
     C     SCDDTP        ANDNE     'S'
     C                   SETON                                        30  90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C     *LIKE         DEFINE    SCDDTP        ##DDTP
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C* Check to ensure there are deductibles posted for the entered
     C* Posr Date (SCPODT)
     C*
     C                   MOVE      ##CO#         XXCO#
     C                   MOVE      ##FND         XXFND
     C***  Begin Add  ***  02/01/10  **************************************
     C     *LIKE         DEFINE    *IN60         SVIN60
     C     XXCO#         SETLL     PGS00
     C     SVIN60        DOUEQ     *ON
     C     XXCO#         READE     PGS00                                  60
     C                   MOVE      *IN60         SVIN60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     GSFND         XXFND
     C***  End   Add  ***  02/01/10  **************************************
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXPODT
     C     DDKY          CHAIN     PDD01                              60
     C***  Begin Add  ***  02/01/10  **************************************
     C     *IN60         IFEQ      *OFF
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C***  End   Add  ***  02/01/10  **************************************
     C     *IN60         IFEQ      '1'
     C                   SETON                                        9072
     C                   LEAVE
     C                   ENDIF
     C*
     C* Deductible invoice entry (SCDCIN) must be 'Y' or 'N'
     C*
     C     SCDCIN        IFNE      'Y'
     C     SCDCIN        ANDNE     'N'
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
     C*
     C* The Print date is required if Deductible invoice entry is Yes
     C* (SCDCIN='Y')
     C*
     C     SCDCIN        IFEQ      'Y'
     C     SCPRDT        IFEQ      *ZERO
     C                   SETON                                        9075
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9074
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Remarks, if entered must be valid
     C*
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM1        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C***  Begin Chg  ***  01/11/10  **************************************
     C**N60***** BCSTAT    IFEQ 'I'
     C     *IN60         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C***  End   Chg  ***  01/11/10  **************************************
     C                   SETON                                        9076
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C***  Begin Chg  ***  01/11/10  **************************************
     C**N60***** BCSTAT    IFEQ 'I'
     C     *IN60         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C***  End   Chg  ***  01/11/10  **************************************
     C                   SETON                                        9077
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C***  Begin Chg  ***  01/11/10  **************************************
     C**N60***** BCSTAT    IFEQ 'I'
     C     *IN60         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C***  End   Chg  ***  01/11/10  **************************************
     C                   SETON                                        9078
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCCOM4        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM4        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C***  Begin Chg  ***  01/11/10  **************************************
     C**N60***** BCSTAT    IFEQ 'I'
     C     *IN60         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C***  End   Chg  ***  01/11/10  **************************************
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C* Deductible detail report (SCDDRP) must be 'Y' or 'N'
     C*
     C     SCDDRP        IFNE      'Y'
     C     SCDDRP        ANDNE     'N'
     C                   SETON                                        9080
     C                   LEAVE
     C                   ENDIF
     C*
      ***** BEG ADD ***** 09/29/06 ************************************
     C     SCEXCL        IFNE      *BLANKS
     C     SCEXCL        ANDNE     'Y'
     C                   SETON                                        9081
     C                   LEAVE
     C                   ENDIF                                                  SCEXCL = BLANK
      ***** END ADD ***** 09/29/06 ************************************
      ***** BEG ADD ***** 11/24/11 ************************************
     C     *IN44         IFEQ      *OFF
      *
     C     SCDDRP        IFEQ      'Y'
     C     SCY4N         ANDEQ     ' '
     C                   SETON                                        9082
     C                   LEAVE
     C                   ENDIF                                                  SCDDRP    IFEQ 'Y'
     C     SCDDRP        IFEQ      'N'
     C     SCY4N         ANDNE     ' '
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF                                                  SCDDRP    IFEQ 'N'
      *
     C* IF USER SET "PRINT SSN" TO "Y" THEN DISPLAY INFO WINDOW.
     C     SCY4N         IFNE      #1Y4N
     C     SCY4N         ANDEQ     'Y'
     C                   MOVEL     *OFF          *INKL
     C                   EXFMT     WDW01
      *
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *OFF          *INKL
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF                                                  *INKL     IFEQ *ON
      *
     C                   ENDIF                                                  SCY4N     IFNE #1Y4N
      *
     C                   ENDIF                                                  *IN44     IFEQ *OFF
      ***** END ADD ***** 11/24/11 ************************************
     C*
     C                   ENDDO
     C*
     C                   ENDSR
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
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   EXSR      CVTDTS
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCOM1'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   SELECT
     C     SCKEY         WHENEQ    *BLANKS
     C                   SETON                                        85
     C                   OTHER
     C***  End   Add  ***  01/11/10  **************************************
     C                   MOVEL     SCKEY         SCCOM1
     C                   SETON                                        86
     C***  Begin Add  ***  01/11/10  **************************************
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C     HFLD          WHENEQ    'SCCOM2'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   SELECT
     C     SCKEY         WHENEQ    *BLANKS
     C                   SETON                                        86
     C                   OTHER
     C***  End   Add  ***  01/11/10  **************************************
     C                   MOVEL     SCKEY         SCCOM2
     C                   SETON                                        87
     C***  Begin Add  ***  01/11/10  **************************************
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C     HFLD          WHENEQ    'SCCOM3'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   SELECT
     C     SCKEY         WHENEQ    *BLANKS
     C                   SETON                                        87
     C                   OTHER
     C***  End   Add  ***  01/11/10  **************************************
     C                   MOVEL     SCKEY         SCCOM3
     C                   SETON                                        88
     C***  Begin Add  ***  01/11/10  **************************************
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C     HFLD          WHENEQ    'SCCOM4'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   SELECT
     C     SCKEY         WHENEQ    *BLANKS
     C                   SETON                                        88
     C                   OTHER
     C***  End   Add  ***  01/11/10  **************************************
     C                   MOVEL     SCKEY         SCCOM4
     C***  Begin Add  ***  01/11/10  **************************************
     C                   SETON                                        89
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
     C*================================================================
     C*  KMDO   - Process F13; user parameters
     C*================================================================
     C*
     C     KMDO          BEGSR
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   EXSR      CVTDTS
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C                   CALL      USPAPI                               99
     C*
     C                   EXSR      GETCOF
     C*
     C                   EXSR      PGMSTR
     C*
     C                   ENDSR                                                  KMDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      *ZEROS        SCPODT
     C                   MOVE      *BLANK        SCDCIN
     C                   MOVE      *ZEROS        SCPRDT
     C                   MOVE      *BLANK        SCCOM1
     C                   MOVE      *BLANK        SCCOM2
     C                   MOVE      *BLANK        SCCOM3
     C                   MOVE      *BLANK        SCCOM4
     C                   MOVE      *BLANK        SCDDRP
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     *** Beg Add ***** 01/20/25 *****************************************
        xxSubmit = *on;
        If isUseScript;
           xxMass = 'Y';
           xxProcType = processType;

           In *Lock *dtaara;
            xxDart = 'N';
            xxRMCD = ' ';

            Dow not %eof(wxgsl);
               If gsIeFl = 'I';
                  Chain (GSCo#:GSFnd) pex00;
                  Select;
                  When %found and EXDDRM <> ' ';
                     xxDart = 'Y';
                     xxRMCD = EXDDRM;
                     Leave;
                  Other;
                     xxDart = 'N';
                     xxRMCD = ' ';
                  Endsl;
               Endif;
               Read pgs00;
            Enddo;
           Out *dtaara;

           xxSubmit = OutputWindow(xxProcType:xxMass);

           If xxSubmit = *off;
              *in90 = *on;
              Leavesr;
           Endif;
        Endif;
     *** End Add ***** 01/20/25 *****************************************
     C*
     C* call W7324C submit Deductible invoice print program (W7325C)
     C* and the Deductible detail report program (W7926C)
     C*
     C                   MOVE      ##CO#         ##CMP#
     C                   MOVE      ##FND         ##GRP
     C*
     C                   MOVEL     SCPODT        ##PODT
      *
     C                   MOVEL     SCPRDT        ##PRDT
     C*
     C* Populate the ##LSDT PARM with the last day of the previous
     C* month.  Do this by calling SRPAST8
     C*
     C                   Z-ADD     DDIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         X8CADT            8
     C                   MOVE      X8CADT        SRCADT            8 0
     C                   SUBST     X8CADT:3      X                 2
     C                   MOVE      X             XXDAY             2 0
     C                   Z-ADD     XXDAY         SRPAST            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    SRCADT
     C                   PARM                    SRPAST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
      *
     C                   MOVEL(P)  SRCADT        ##LSDT
     C*
     C*
     C                   MOVE      SCCOM1        ##COM1
     C                   MOVE      SCCOM2        ##COM2
     C                   MOVE      SCCOM3        ##COM3
     C                   MOVE      SCCOM4        ##COM4
     C                   MOVEL     ##OUTQ        ##QUE
      ***** BEG ADD ***** 09/29/06 ************************************
     C                   MOVE      SCEXCL        ##EXCL            1
      ***** END ADD ***** 09/29/06 ************************************
      *
      ***** BEG ADD ***** 07/03/07 ************************************
     C                   MOVE      'D'           ##MODE            1
      ***** END ADD ***** 07/03/07 ************************************
      *
     C                   CALL      'W7324C'
     C                   PARM                    ##CMP#            3
     C                   PARM                    ##GRP             3
     C                   PARM                    ##LSDT            8
     C                   PARM                    ##PODT            8
     C                   PARM                    ##PRDT            8
     C                   PARM                    ##COM1            2
     C                   PARM                    ##COM2            2
     C                   PARM                    ##COM3            2
     C                   PARM                    ##COM4            2
     C                   PARM                    ##QUE            10
     C                   PARM                    SCDCIN
     C                   PARM                    SCDDRP
      ***** BEG ADD ***** 07/03/07 ************************************
     C                   PARM                    ##MODE
      ***** END ADD ***** 07/03/07 ************************************
      ***** BEG ADD ***** 09/29/06 ************************************
     C                   PARM                    ##EXCL
      ***** END ADD ***** 09/29/06 ************************************
     C***  Begin Add  ***  01/11/10  **************************************
     C                   PARM      SCDDTP        ##DDTP
     C***  End   Add  ***  01/11/10  **************************************
     C***  Begin Add  ***  11/24/11  **************************************
     C                   PARM      SCY4N         ##Y4N             1
     C***  End   Add  ***  11/24/11  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   EXSR      MVSCDB
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
       //*** Beg Add ***** 01/20/25 ***************************************
       isUseScript = UseScriptura(processType);
       If isUseScript;
          scDCIN = 'Y';
          scDDRP = 'Y';
          disChkBox = *on;
       Else;
          scDCIN = 'N';
          scDDRP = 'N';
          disChkBox = *off;
       Endif;
       //*** End Add ***** 01/20/25 ***************************************
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCNAME
     C*
     C                   CLEAR                   @@MODE
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  @#MODE        @@MODE
     C                   ENDIF                                                  Q1PRMS>=2
      *** Beg Add ***** 01/13/25 **************************************
       If @@Mode <> 'O';
          DedBlRep();
          *InLr = *On;
          Return;
       Endif;
      *** End Add ***** 01/13/25 **************************************
     C*
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C*
     C*** BEGIN ADD *** 04/13/06 ************************************
     C*  GET client suffix; set up workfile caption.
     C                   CALL      'SA030R'
     C                   PARM                    SUFF              3
     C*
     C                   MOVEL     'PDSRPT'      SCLIB
     C     SCLIB         CAT       SUFF:0        SCLIB
     C     SCLIB         CAT       ',':0         SCLIB
     C*** END ADD ***** 04/13/06 ************************************
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   CLEAR                   WCFL43
     C     'WC'          CHAIN     SWC01                              60
     C*
     C* Check for Large Deductible Module
     C                   SELECT
     C     WCFL43        WHENEQ    'Y'
     C                   MOVE      X'20'         AT1                            Normal
     C                   MOVE      X'26'         AT2                            Underscore, HI
     C                   MOVE      *ON           LRGDED            1
      *** beg add ***** 12/03/15 *****************************************
     c                   eval      isLrgDed = *on
      *** end add ***** 12/03/15 *****************************************
     C                   OTHER
     C                   MOVE      X'27'         AT1                            Non-display
     C                   MOVE      X'A7'         AT2                            Protect,Non-display
     C                   MOVE      *OFF          LRGDED
      *** beg add ***** 12/03/15 *****************************************
     c                   eval      isLrgDed = *off
      *** end add ***** 12/03/15 *****************************************
     C                   MOVE      'S'           SCDDTP
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C***  Begin Add  ***  11/24/11  **************************************
     C*
     C                   MOVE      ##CO#         #1CO#             3
     C                   CALL      'WA097R'
     C                   PARM                    #1CO#
     C                   PARM      'W7324R'      #1RPID           10
     C     SCY4N         PARM      *BLANKS       #1Y4N             1
     C     *IN44         PARM      *BLANKS       #1CHG             1
      *
     C***  End   Add  ***  11/24/11  **************************************
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
     C                   EXSR      MVSCDB
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   EXSR      MVSCDB
     C                   ENDSR
