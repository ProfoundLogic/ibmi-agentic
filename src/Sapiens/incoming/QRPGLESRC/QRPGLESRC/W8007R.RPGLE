     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
      *
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W8007R - PROMPT FOR GENERATE DEDUCTIBLE BILLING REPORT & INVOICES
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM PROMPTS THE USER FOR THE BILLING FUND YEAR,    *
     H*   A DATE TO CALCULATE LOSSES, THE A/R POSTING DATE, THE       *
     H*   INVOICE DATE, THE PRINT DATE, AND REMARKS FOR THE INVOICES. *
     H* CALLED FROM:                                                  *
     H*   NONE                                                        *
     H* CALLS TO:                                                     *
     H*   W8007C                                                      *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 02/28/95               PROJECT: WC311                *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/20/25 C:300 Project: WC2494                                *
     H*                Add processing to display output control window*
     H*                for Deductible Invoices when Scriptura is requested*
     H*                DSPF: Show Output Control Window and Hide Darts*
     H*                and Darts remark code labels/fields Conditionally
     H*                                                               *
     H* 01/13/25 C:344 Project: WC2989                                *
     H*                Added capability to pass-thru/ call the newer  *
     H*                Deductible Billing Reprint prompt program.     *
     H*                                                               *
     H* 12/03/15 C:136 Support: 34655                                 *
     H*                Add indicator to control display of deductible *
     H*                type text box. It should display only when the *
     H*                large deductible module is enabled             *
     H*                                                               *
     H* 10/26/12 C:136 Support: WA114 80990                           *
     H*                Further correction for edit issues related     *
     H*                to DARTS field                                 *
     H*                                                               *
     H* 08/02/11 C:061 Project WC2003                                 *
     H*                Add prompt for print of SSN.                   *
     H*                                                               *
     H* 06/28/11  C:189 Seq#: WA025/76502                             *
     H*                 Corrected the edit issues related to          *
     H*                 the DARTS field in multi-grp selection        *
     H*                 case.                                         *
     H*                                                               *
     H* 06/02/11  C:189 Support  76502                                *
     H*                 25WRB window edits not working in V11 due to  *
     H*                 incorrectly defined variable                  *
     H*                                                               *
     H* 09/14/10  C:33  Seq#: WA093/73898                             *
     H*                -When running in month-end, default LSDT to    *
     H*                  prior month-end date and do not allow for    *
     H*                  entry.                                       *
     H*                                                               *
     H* 06/25/10 C:189 Project: WC1878                                *
     H*                 Add Single Employer Processing to the         *
     H*                 Deductible Invoice Deletion. The program      *
     H*                 has been coverted to RPGLE, so that existing  *
     H*                 procedures can be used.                       *
     H*                                                               *
     H* 04/12/10  C:33  Project: 25WRB   Priss #16                    *
     H*                -Default DARTS resolution to 'Y'.              *
     H*                -Require DARTS code if DARTS flag is 'Y' and   *
     H*                  not all groups have a default value.         *
     H*                                                               *
     H* 02/01/10  C:33  Project: 25WRB                                *
     H*                -Change to use group select processing         *
     H*                                                               *
     H* 01/11/10  C:33  Project: WC1838                               *
     H*                 Process Large & Small deductibles             *
     H*                                                               *
     H* 01/08/09  C:189 Project 71WJU                                 *
     H*                 Deductible Invoices to resolve to Darts       *
     H*                                                               *
     H*                                                               *
     H* 11/28/07 C:190  Project 135WKC-Priss# 7.                      *
     H*                 1. The new option of "exclude policies        *
     H*                    with no activity (Y/ ):" should de-        *
     H*                    fault to No.                               *
     H*                 2. Since hierarchy is region then group ,     *
     H*                    place the region range option above group. *
     H*                                                               *
     H* 09/24/07  C:190 Project 135WKC                                *
     H*                 Add new screen field "Region range.           *
     H*                 This will only be displayed if the            *
     H*                 CLTID = '135'.                                *
     H*                                                               *
     H* 09/29/06 C:132 PROJECT 93WGL                                  *
     H*                Add parm for excluding policies w/no activity. *
     H*                                                               *
     H* 04/13/06 C:58  PROJECT WC1321                                 *
     H*                Add workfile identification caption.           *
     H*                                                               *
     H* 09/26/05 C:61  PROJECT 135WED WC1336                          *
     H*                Add prompt selection by group range.           *
     H*                                                               *
     H* 12/02/99 C:15  Support: WC048/20874                           *
     H*                Fixed edit on posting date.                    *
     H*                                                               *
     H* 12/01/99 C:118 PROJECT: WC577                                 *
     H*                Made V3 changes to display file.               *
     H*                                                               *
     H* 09/22/95 C:24 PROJECT: CM005                                  *
     H*               REMOVE BILLING FUND YEAR FROM THE SCREEN.  THE  *
     H*               BILLING FUND YEAR WILL BE CALCULATED FOR EACH   *
     H*               EMPLOYER IN THE BILLING PROGRAM (W8020R).       *
     H*                                                               *
     H*  05/29/96 C:33 PROJECT WC420                                  *
     H*                IF THIS REPORT IS CALLED ON DEMAND IT SHOULD   *
     H*                BE ROUTED TO THE USER PARM OUTQ INSTEAD OF THE *
     H*                OVERRIDE OUTQ.                                 *
     H*****************************************************************
     FW8007F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      ***** BEG ADD ******* 01/08/09 **********************************
     FWMEXL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
      ***** END ADD ******* 01/08/09 **********************************
      ***** BEG ADD ******* 09/24/07 **********************************
     F                                     INFDS(DBFDS)
      ***** END ADD ******* 09/24/07 **********************************
     FWTABL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      *** BEG ADD ***** 06/25/10 **************************************
     fwmemp     if   e             disk    rename(pem01 : pem0p)
     fwmfnp     if   e             disk    rename(pfn01 : pfn0p)
      *** END ADD ***** 06/25/10 **************************************
     D*
     D***  Begin Chg  ***  01/11/10  **************************************
     D OPTNM           C                   CONST('Deductible Invoices')
     D***  End   Chg  ***  01/11/10  **************************************
      ***** BEG ADD ******* 09/24/07 **********************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
      ***** END ADD ******* 09/24/07 **********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D***  Begin Add  ***  09/14/10  **************************************
     D   DSDD                  9     10  0
     D***  End   Add  ***  09/14/10  **************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXDAT1                 1      7  0
     D*** BEGIN CHG *** 12/02/99 ************************************
     D**************************************  2   50XXYRM1
     D  XXYRM1                 1      5  0
     D*** END CHG ***** 12/02/99 ************************************
     D  XXDA1                  6      7  0
     D                 DS
     D  XXDAT2                 1      7  0
     D*** BEGIN CHG *** 12/02/99 ************************************
     D**************************************  2   50XXYRM2
     D  XXYRM2                 1      5  0
     D*** END CHG ***** 12/02/99 ************************************
     D  XXDA2                  6      7  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ******* 09/26/05 **********************************
      *** BEG ADD ***** 06/25/10 **************************************
     d singleMode      s               n   inz(*off)
     d rrnGrp          s              9  0
     d rrnEmp          s              9  0
     d error           s              1
     d inkc            s              1
     d inkl            s              1
     d found           s               n   inz(*off)
     d ##co@           s              3  0
     d/copy wprgroup
     d/copy wpremplr
      *** END ADD ***** 06/25/10 **************************************
      *** Beg Add ***** 01/13/25 **************************************
     d DedInvPgm       pr                  Extpgm('WTDEDINV')
      *** End Add ***** 01/13/25 **************************************
     d*** Beg Add ***** 01/20/25 *******************************************
     d/copy sPrLetter
     D/copy sPrXML
     d*** Beg Add ***** 01/13/25 *******************************************
     d/copy wprgenpop
     d*** End Add ***** 01/13/25 *******************************************
     D isUseScript     S              1n   inz
     d processType     s            100    inz('Deductible Invoice')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d*** Beg Add ***** 01/13/25 *******************************************
     d strMsg          s            250a
     d*** End Add ***** 01/13/25 *******************************************
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d*** End Add ***** 01/20/25 *******************************************
      *
      *
      *
      *=============================================================
      *  KLIST
      *=============================================================
      *
      *  Group
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      ***** END ADD ******* 09/26/05 **********************************
      *
      ***** BEG ADD ******* 09/24/07 **********************************
     C     FNKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
      ***** END ADD ******* 09/24/07 **********************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST                                                          SSSS
     C                   KFLD                    Q1USER
     C*
      ***** BEG ADD ******* 01/08/09 **********************************
     C     EXKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
      ***** END ADD ******* 01/08/09 **********************************
      *** beg add ***** 10/26/12 **************************************
     C     EXKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    scfnd
      *** end add ***** 10/26/12 **************************************
      ***** BEG DEL ******* 09/26/05 **********************************
     C*********
     C***KEY*LIST FOR THE FUND MASTER FILE
     C*********
     C*********  FNKEY     KLIST
     C*********            KFLD           UPCO#
     C*********            KFLD           UPFND
      ***** END DEL ******* 09/26/05 **********************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
      *
      *
      *
      *===========================================================
      *  *LIKE DEFN
      *===========================================================
     C     *LIKE         DEFINE    TTKEY         XXKEY
      *
      *
      *===========================================================
      *  PLIST
      *===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
      *
      *
      *===========================================================
      *  MAIN
      *===========================================================
      *** BEG ADD ***** 01/13/25 **************************************
       If qqCall = 'S';
          strMsg = 'This menu option has been replaced.  Please use the +
          Deductible Invoices menu option which will allow you to +
          select a single group then a single employer.';

          PopOneBtn(strMsg:'Outdated Menu Option':'Return');
          *InLr = *On;
          Return;
       Endif;
       DedInvPgm();
       *InLr = *On;
       Return;
      *** END ADD ***** 01/13/25 **************************************
      *** BEG ADD ***** 06/25/10 **************************************
     c                   IF        Q1PRMS  > 1
     c                   IF        QQCALL = 'S'
     c                   EVAL      singleMode = *on
     c                   EVAL      *in65  = *on
     c                   ENDIF
     c                   ENDIF
      *** END ADD ***** 06/25/10 **************************************
      *** BEG ADD **** 09/24/07 **********************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '135'
     C                   MOVE      *ON           *IN38
     C                   ELSE
     C                   MOVE      *OFF          *IN38
     C                   ENDIF
      *** END ADD **** 09/24/07 **********************************************
     C*
     C***  Begin Add  ***  09/14/10  **************************************
     C                   CALL      'SRCHKLIB'                           99
     C                   PARM      'PDSMTH'      ##LIB            10
     C                   PARM      '*USR'        ##LIBL            4
     C                   PARM      '1'           ##AppSfx          1
     C     MTHEND        PARM      '0'           ##FLAG            1
     C*
     C     *LIKE         define    *IN99         MTHEND
     C***  End   Add  ***  09/14/10  **************************************
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL06        IFNE      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C***  Begin Add  ***  01/11/10  **************************************
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
     C                   MOVE      X'27'         AT2                            Non-display
     C                   MOVE      *OFF          LRGDED
      *** beg add ***** 12/03/15 *****************************************
     c                   eval      isLrgDed = *off
      *** end add ***** 12/03/15 *****************************************
     C                   MOVE      'S'           SCDDTP
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLSDT
     C***  Begin Add  ***  09/14/10  **************************************
     C                   select
     C                   when      not mthEnd
     C                   MOVE      X'26'         AT3                             Underscore, HI
     C                   other
     C                   MOVE      X'A2'         AT3                             Protect,HI
     C                   TIME                    DSDTTM
     C                   CALL      'SRPAST8'
     C     SCLSDT        PARM      DSDATE        ##DATE            8 0
     C                   PARM      DSDD          ##NUM             5 0
     C                   PARM                    ##ERR             1
     C                   ENDSL
     C***  End   Add  ***  09/14/10  **************************************
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRDT
     C*
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANKS       SCCOM4
      ***** BEG ADD ******* 09/24/07 **********************************
     C                   Z-ADD     *ZEROS        XXRGN#            3 0
      ***** END ADD ******* 09/24/07 **********************************
      ***** BEG ADD ***** 09/29/06 ************************************
     C     CLTID         IFEQ      '093'
     C     CLTID         OREQ      '071'
     C                   MOVE      'Y'           SCEXCL
     C                   ENDIF                                                  SCEXCL = BLANK
      ***** END ADD ***** 09/29/06 ************************************
     C*
     C***  Begin Add  ***  04/12/10  **************************************
     C                   MOVE      'Y'           SCDFLG
     C*
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W7944R'      ##PGM            10
     C                   PARM      *OFF          ##FLAG            1
     C*
     C***  beg   chg  ***  06/02/11  **************************************
     C**********         MOVE      ##FLAG        MULTGR            2
     C                   MOVE      ##FLAG        MULTGR            1
     C***  end   chg  ***  06/02/11  **************************************
     C***  End   Add  ***  04/12/10  **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
      ******* beg add ****** 08/02/11 *********************************

     c                   move      upco#         #1co#

     c                   call      'WA097R'
     c                   parm                    #1co#             3
     c                   parm      'W8007R'      #1rpid           10
     c     scy4n         parm      *blank        #1y4n             1
     c     *in44         parm      *blank        #1chg             1

      ******* end add ****** 08/02/11 *********************************
     C******BEG  ADD **** 01/08/09 ***********************************
     C     EXKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      EXDDRM        SCDDRM
     C                   END
     C******END  ADD ***** 01/08/09 ***********************************
     C*
     C***  Begin Add  ***  04/12/10  **************************************
     C     MULTGR        IFEQ      *ON
     C***  BEG   DEL  ***  06/28/11  **************************************
     C***********        CLEAR                   SCDDRM
     C***********        CLEAR                   SVDDRM
     C***  END   DEL  ***  06/28/11  **************************************
     C                   ENDIF
     C***  End   Add  ***  04/12/10  **************************************
      ***** BEG DEL ******* 09/26/05 **********************************
     C*********  FNKEY     CHAINPFN01                60
     C******BEGIN ADD **** 12/01/99 ***********************************
     C*********  *IN60     IFEQ *OFF
     C******END   ADD **** 12/01/99 ***********************************
     C*********            MOVE UPFND     SCFND
     C*********            MOVE FNNAME    SCNAME
     C******BEGIN ADD **** 12/01/99 ***********************************
     C*********            ELSE
     C*********            MOVEL*BLANKS   SCFND
     C*********            MOVEL*BLANKS   SCNAME
     C*********            ENDIF                           *IN60=OFF
     C******END   ADD **** 12/01/99 ***********************************
      ***** END DEL ******* 09/26/05 **********************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C***  Begin Chg  ***  01/11/10  **************************************
     C**********           MOVE QQPRDS    SCPRDS
     C                   SELECT
     C     Q1PRMS        WHENGE    1
     C                   MOVEL     QQPRDS        ##DATA
     C                   OTHER
     C                   MOVEL     OPTNM         ##DATA
     C                   ENDSL
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  End   Chg  ***  01/11/10  **************************************
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
      *** Beg Add ***** 01/20/25 *****************************************
      * Set Visibility of Darts and Darts remarks fields
      /Free
        isUseScript = UseScriptura(processType);
        isRsvDarts = *On;

        If isUseScript;
           isRsvDarts = *Off;
        Endif;
      /End-Free
      *** End Add ***** 01/20/25 *****************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  COMMAND 10 - TABLE FILE SEARCH - COMMENT 1
     C*
     C     *INKJ         IFEQ      '1'
     C*
      *** BEG ADD ***** 06/25/10 **************************************
     c                   select
     c                   when      hfld = 'SCFND'
     c                   exsr      GroupSearch
     c                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     c                   exsr      EmplrSearch
     c                   endsl
      *** END ADD ***** 06/25/10 **************************************
     C***  Begin Add  ***  02/01/10  **************************************
     C* Don't process group searches
     C     HFLD          IFEQ      'SCFFND'
     C     HFLD          OREQ      'SCTFND'
     C                   CLEAR                   HFLD
     C                   ENDIF
     C***  End   Add  ***  02/01/10  **************************************
     C*
      ***** BEG ADD ******* 11/28/07 **********************************
     C*  F10 - SEARCH - REGION CODE
     C*
     C     FNKY1         CHAIN     PFN01                              99
     C     HFLD          IFEQ      'SCFRGN'
     C     HFLD          OREQ      'SCTRGN'
     C*
     C                   Z-ADD     *ZEROS        XXRGN#
     C*
     C                   CALL      'W0520R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    XXRGN#
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     XXRGN#        ANDNE     *ZEROS
     C*
     C     HFLD          IFEQ      'SCFRGN'
     C                   Z-ADD     XXRGN#        SCFRGN
     C                   MOVE      *ON           *IN35                          SCTRGN POSITION
     C                   ENDIF                                                  HFLD=SCFRGN
     C*
     C     HFLD          IFEQ      'SCTRGN'
     C                   Z-ADD     XXRGN#        SCTRGN
     C                   MOVE      *ON           *IN71                          SCFAGT POSITION
     C                   ENDIF                                                  HFLD=SCTRGN
     C*
     C***  End   Chg  ***  10/11/06  **************************************
     C                   ELSE
     C*
     C     HFLD          COMP      'SCFRGN'                               36    SCFRGN POSITION
     C     HFLD          COMP      'SCTRGN'                               37    SCTRGN POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCFRGN
      *
      ***** END ADD ******* 11/28/07 **********************************
      ***** BEG ADD ******* 09/26/05 **********************************
      *
      *  FROM Group
      *
     C     HFLD          IFEQ      'SCFFND'
     C                   MOVE      UPCO#         ##CO#
     C*
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
      *
     C     ##FND         IFEQ      *BLANK
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      *ON           *IN74
     C                   MOVE      ##FND         SCFFND
     C                   ENDIF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFFND
      *
      *  TO Group
      *
     C     HFLD          IFEQ      'SCTFND'
     C                   MOVE      UPCO#         ##CO#
     C*
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
      *
     C     ##FND         IFEQ      *BLANK
     C                   MOVE      *ON           *IN74
     C                   ELSE
      ***** BEG CHG ******* 11/28/07 **********************************
      ***** BEG CHG ******* 09/24/07 **********************************
     C*************        MOVE *ON       *IN76
     C*************        MOVE *ON       *IN32
     C                   MOVE      *ON           *IN76
      ***** END CHG ******* 09/24/07 **********************************
      ***** END CHG ******* 11/28/07 **********************************
     C                   MOVE      ##FND         SCTFND
     C                   ENDIF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTFND
      *
      ***** END ADD ******* 09/26/05 **********************************
      ***** BEG DLT ******* 09/24/07 **********************************
      ***** BEG ADD ******* 09/24/07 **********************************
     C***F10*- SEARCH - REGION CODE
     C*******
     C*******    FNKY1     CHAINPFN01                99
     C*******    HFLD      IFEQ 'SCFRGN'
     C*******    HFLD      OREQ 'SCTRGN'
     C*******
     C*******              Z-ADD*ZEROS    XXRGN#
     C*******
     C*******              CALL 'W0520R'
     C*******    *INKC     PARM           @@KC    1
     C*******    *INKL     PARM           @@KL    1
     C*******              PARM '2'       @@CALL  1
     C*******              PARM           @@RRN   90
     C*******              PARM DBRRN     @@RRN2  90
     C*******              PARM           XXRGN#
     C*******
     C*******    *INKC     CABEQ*ON       TERM
     C*******
     C*******    *INKL     IFEQ *OFF
     C*******    XXRGN#    ANDNE*ZEROS
     C*******
     C*******    HFLD      IFEQ 'SCFRGN'
     C*******              Z-ADDXXRGN#    SCFRGN
     C*******              MOVE *ON       *IN35            SCTRGN POSITION
     C*******              ENDIF                           HFLD=SCFRGN
     C*******
     C*******    HFLD      IFEQ 'SCTRGN'
     C*******              Z-ADDXXRGN#    SCTRGN
     C*******              MOVE *ON       *IN76            SCFAGT POSITION
     C*******              ENDIF                           HFLD=SCTRGN
     C*******
     C*******              ELSE
     C*******
     C*******    HFLD      COMP 'SCFRGN'                 36SCFRGN POSITION
     C*******    HFLD      COMP 'SCTRGN'                 37SCTRGN POSITION
     C*******
     C*******              ENDIF                           *INKL=OFF
     C*******
     C*******              MOVEL'SCR01'   Q2RECF    P
     C*******              GOTO STRT
     C*******              ENDIF                           HFLD=SCFRGN
      *******
      ***** END ADD ******* 09/24/07 **********************************
      ***** END DLT ******* 11/28/07 **********************************
      *
      *  Comments
     C*
     C     SCCOM1        IFEQ      '? '
     C     SCCOM1        OREQ      ' ?'
     C     HFLD          OREQ      'SCCOM1'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C                   MOVEL     SCKEY         SCCOM1
     C     *INKC         CABEQ     *ON           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  COMMAND 10 = TABLE FILE SEARCH - COMMENT 2
     C*
     C     SCCOM2        IFEQ      '? '
     C     SCCOM2        OREQ      ' ?'
     C     HFLD          OREQ      'SCCOM2'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    QQKC
     C     *INKL         PARM                    QQKL
     C                   PARM                    SCKEY
     C*
     C                   MOVEL     SCKEY         SCCOM2
     C     *INKC         CABEQ     *ON           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  COMMAND 10 = TABLE FILE SEARCH - COMMENT 3
     C*
     C     SCCOM3        IFEQ      '? '
     C     SCCOM3        OREQ      ' ?'
     C     HFLD          OREQ      'SCCOM3'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    QQKC
     C     *INKL         PARM                    QQKL
     C                   PARM                    SCKEY
     C*
     C                   MOVEL     SCKEY         SCCOM3
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  COMMAND 10 = TABLE FILE SEARCH - COMMENT 4
     C*
     C     SCCOM4        IFEQ      '? '
     C     SCCOM4        OREQ      ' ?'
     C     HFLD          OREQ      'SCCOM4'
     C*
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    QQKC
     C     *INKL         PARM                    QQKL
     C                   PARM                    SCKEY
     C*
     C                   MOVEL     SCKEY         SCCOM4
     C*
     C     *INKC         CABEQ     *ON           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
      ***** BEG ADD ******* 01/08/09 **********************************
     C     HFLD          IFEQ      'SCDDRM'
     C                   MOVE      *BLANKS       QQRMCD            5
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVEL     QQRMCD        SCDDRM
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      '1'           *IN67
     C                   GOTO      STRT
      ***** END ADD ******* 01/08/09 **********************************
     C                   END
     C*
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      ***** BEG ADD ******* C9/26/05 **********************************
      *
      *** BEG ADD ***** 06/25/10 **************************************
     c                   if        singleMode
     c                   eval      found = validGrp(scfnd)
     c                   eval      *in81 = not(found)
     c                   eval      *in90 = *in81
      *
     c                   if        *in90 = *on
     C                   Goto      STRT
     c                   endif
      *
     c                   if        not(*in90)
      *
     c                   if        scemp# = 0
     c                   seton                                        8990
     C                   Goto      STRT
     C                   else
      *
     c                   eval      found=validEmp(scemp#:0:rrnEmp:scfnd)
     c                   eval      *in82 = not(found)
     c                   eval      *in90 = *in82
      *
     c                   if        *in90 = *on
     C                   Goto      STRT
     c                   endif
      *
     c                   endif
     c                   endif
      *
     c                   if        scdiv <> *blank and not(*in90)
     c                   move(p)   scdiv         xxdiv             5 0
     c                   eval      found = validEmp(scemp#:xxdiv:rrnEmp:scfnd)
     c                   eval      *in83 = not(found)
     c                   eval      *in82 = not(found)
     c                   eval      *in90 = *in83
      *
     c                   if        *in90 = *on
     C                   Goto      STRT
     c                   endif
      *
     c                   endif
     c                   endif
      *
      *** END ADD ***** 06/25/10 **************************************
      * Group range must be valid
      *
      ***** BEG ADD ******* 09/24/07 **********************************
     C     SCFFND        IFNE      *ZEROS
     C     SCTFND        ANDNE     *ZEROS
     C     SCFRGN        ANDNE     *ZEROS
     C     SCFRGN        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCFFND        IFNE      *ZEROS
     C     SCTFND        ANDNE     *ZEROS
      ***** END ADD ******* 09/24/07 **********************************
     C                   MOVE      SCFFND        XXFND
     C     FNKY          CHAIN     PFN01                              72
     C   72              GOTO      STRT
      *
     C                   MOVE      SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              75
     C   75              GOTO      STRT
      ***** BEG ADD ******* 09/24/07 **********************************
     C                   ENDIF
      ***** END ADD ******* 09/24/07 **********************************
      *
     C     SCTFND        IFLT      SCFFND
     C                   MOVE      *ON           *IN73
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 09/26/05 **********************************
      ***** BEG ADD ******* 09/24/07 **********************************
      *
      * Region range must be valid
     C*
     C     SCTRGN        IFLT      SCFRGN
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
      ***** END ADD ******* 09/24/07 **********************************
     C*  LOSSES AS OF DATE MUST BE VALID
     C*
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLSDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  A/R POSTING DATE ENTERED MUST BE VALID
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE POSTING DATE CANNOT BE LESS THAN THE MONTH OF THE CURRENT
     C*  DATE
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C     XXYRM1        IFLT      XXYRM2
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  PRINT DATE MUST BE VALID
     C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  BILLING COMMENTS IF ENTERED MUST BE VALID
     C*
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM1        XXKEY
     C     TBKEY         CHAIN     PBC01                              25
     C  N25BCSTAT        COMP      'I'                                    25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              26
     C  N26BCSTAT        COMP      'I'                                    26
     C     *IN26         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              27
     C  N27BCSTAT        COMP      'I'                                    27
     C     *IN27         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM4        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM4        XXKEY
     C     TBKEY         CHAIN     PBC01                              28
     C  N28BCSTAT        COMP      'I'                                    28
     C     *IN28         CABEQ     '1'           STRT
     C                   END
     C*
      ***** BEG ADD ***** 09/29/06 ************************************
     C     SCEXCL        IFNE      *BLANKS
     C     SCEXCL        ANDNE     'Y'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEXCL = BLANK
      ***** END ADD ***** 09/29/06 ************************************
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C* Deductible Billing Type
     C     LRGDED        IFEQ      *ON
     C*
     C     SCDDTP        IFNE      'B'
     C     SCDDTP        ANDNE     'L'
     C     SCDDTP        ANDNE     'S'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C     *LIKE         DEFINE    SCDDTP        QQDDTP
     C***  End   Add  ***  01/11/10  **************************************
     C*
      ***** BEG ADD ***** 01/08/09 ************************************
     C     SCDFLG        IFNE      *BLANKS
     C     SCDFLG        ANDNE     'Y'
     C                   SETON                                        68  90
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEXCL = BLANK
     C*
     C***  Begin Add  ***  04/12/10  **************************************
     C*** beg chg *** 10/26/12 ************************************************
     C*****MULTGR        IFEQ      *ON
     C***********
     C**DARTS*remark code will be required if all groups do not have a
     C***default*remark code set-up.
     C*****SCDFLG        IFEQ      'Y'
     C*****BEG***ADD  ***  06/28/11  **************************************
     C*****SCDDRM        ANDEQ     *BLANKS
     C*****END***ADD  ***  06/28/11  **************************************
     C***********        CALL      'W7944R1'                            99
     C***********        PARM      '1'           ##CALL            1
     C***********        PARM      'EXDDRM'      ##TYPE           10
     C***********        PARM      '0'           ##ERR             1
     C*****##ERR*        IFEQ      *ON
     C*****SCDDRM        ANDEQ     *BLANKS
     C***********        SETOFF                                       70
     C***********        WRITE     SCR01
     C***********        SETON                                        70  90
     C***********        GOTO      STRT
     C*****BEG***ADD  ***  06/28/11  **************************************
     C***********        ELSE
     C***********        SETON                                        677090
     C***********        GOTO      STRT
     C*****END***ADD  ***  06/28/11  **************************************
     C***********        ENDIF
     C***********        ENDIF
     C***********
     C*****BEG***DEL  ***  06/28/11  **************************************
     C**Notify*user the DARTS remark code is only used for groups w/o a
     C***default*remark code set-up.
     C*****************     SCDDRM        IFNE      *BLANKS
     C*****************     SVDDRM        ANDEQ     *BLANKS
     C*******************     *LIKE         DEFINE    SCDDRM        SVDDRM
     C*****************  MOVEL     SCDDRM        SVDDRM
     C*************      SETON                                        71  90
     C*************      GOTO      STRT
     C*************      ENDIF
     C*****END***DEL  ***  06/28/11  **************************************
     C***********
     C***********        ENDIF
     C*****End***Add  ***  04/12/10  **************************************
     C***********
     C***DARTS*REMARK FIELD MUST BE VALID
     C*****SCDDRM        IFNE      *BLANKS
     C*****SCDDRM        CHAIN     PCT01                              60
     C***********
     C******IN60*        IFEQ      *ON
     C***********        SETON                                        69  90
     C***********        GOTO      STRT
     C***********        ENDIF
     C***********        ENDIF
      ******END*ADD ***** 01/08/09 ************************************
     C*****BEG***ADD  ***  06/28/11  **************************************
     C*****SCDDRM        IFEQ      *BLANKS
     C*****EXDDRM        ANDEQ     *BLANKS
     C*****MULTGR        ANDEQ     *OFF
     C***********        SETON                                        70  90
     C***********        GOTO      STRT
     C***********        ENDIF
     C***  END   ADD  ***  06/28/11  **************************************
      *
      * Don't edit DARTS code unless resolving bills to DARTS.
      *
     c                   if        scdflg = 'Y'
     c                   if        multgr = *on and scddrm = *blanks
     c                   call      'W7944R1'                            99
     c                   parm      '1'           ##call            1
     c                   parm      'EXDDRM'      ##type           10
     c                   parm      '0'           ##err             1
     c                   if        ##err = *on
     c                   eval      *in70 = *on
     c                   eval      *in90 = *on
     c                   goto      strt
     c                   endif                                                  ##err = *on
     c                   endif                                                  multgr = *on
     c                   if        multgr = *off
     c                   clear                   EXDDRM
     c     exky1         chain     pex01
     c                   if        scddrm = *blanks and
     c                             EXDDRM = *blanks
     c                   eval      *in70 = *on
     c                   eval      *in90 = *on
     c                   goto      strt
     c                   endif                                                  scddrm = *blanks
     c                   endif                                                  multgr = *off
     c                   if        scddrm <> *blanks
     c     scddrm        chain     pct01
     c                   if        %found and ctstat = 'A'
     c                   else
     c                   eval      *in69 = *on
     c                   eval      *in90 = *on
     c                   goto      strt
     c                   endif                                                  %found
     c                   endif                                                  scddrm <> *blanks
     c                   endif                                                  scdflg
     C*** end chg *** 10/26/12 ************************************************
      ******* beg add ****** 08/02/11 *********************************
      *
      * If user set "Print SSN" to "Y" then display info window.
      *
     c                   if        (scy4n <> #1y4n) and
     c                             (scy4n = 'Y') and
     c                             (*in44 = *off)
     c                   eval      *inkl = *off
     c                   exfmt     wdw01

     c                   if        *inkl = *on
     c                   eval      *inkl = *off
     C                   movel(p)  'SCR01'       q2recf
     c                   goto      strt
     c                   endif

     c                   endif
      *
      ******* end add ****** 08/02/11 *********************************
      *** Beg Add ***** 01/20/25 *****************************************
      /Free
        xxSubmit = *on;
        If isUseScript;
           xxProcType = processType;
           If singleMode;
              xxMass = ' ';
           Else;
              xxMass = 'Y';
           Endif;

           In *Lock *dtaara;
           Clear svLDA;
           xxDart = 'N';
           xxRMCD = ' ';
           If scDdrm <> ' ';
              xxDart = 'Y';
              xxRMCD = scDdrm;
           Endif;
           Out *dtaara;
           If rrnEmp <> 0;
              xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':rrnEmp);
           Else;
              xxSubmit = OutputWindow(xxProcType:xxMass);
           Endif;
           If xxSubmit = *off;
        //    Return *Off;
     C                   GOTO      STRT
           Else;
              In *Lock *dtaara;
              Out *dtaara;
              scDflg = xxDart;
              scDdrm = xxRMCD;
        //    Return *On;
           Endif;
        Else;
        // Return *On;
        Endif;
      /End-Free
      *** End Add ***** 01/20/25 *****************************************
      *
     C*  CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
      ***** BEG DEL ******* 09/26/05 **********************************
     C*********            MOVE UPFND     QQFND   3
      ***** END DEL ******* 09/26/05 **********************************
      ***** BEG ADD ******* 09/26/05 **********************************
     C                   MOVE      SCFFND        QQFND             3
     C                   MOVE      SCTFND        QQTFND            3
      ***** END ADD ******* 09/26/05 **********************************
     C                   MOVE      SCLSDT        QQLSDT            8
     C                   MOVE      SCPODT        QQPODT            8
     C                   MOVE      SCPRDT        QQPRDT            8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*
      ***** BEG ADD ***** 09/29/06 ************************************
     C                   MOVE      SCEXCL        QQEXCL            1
      ***** END ADD ***** 09/29/06 ************************************
      *
      ***** BEG ADD ***** 09/24/07 ************************************
     C                   MOVE      SCFRGN        QQFRGN            3
     C                   MOVE      SCTRGN        QQTRGN            3
     C*
     C     SCTRGN        IFNE      *ZEROS
     C                   MOVE      '000'         QQFND
     C                   MOVE      '999'         QQTFND
     C                   ENDIF
      ***** END ADD ***** 09/24/07 ************************************
     C*
      *** BEG ADD ***** 06/25/10 **************************************
     c                   MOVE      SCEMP#        QQEMP#            9
     c                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCFND         QQTFND
     C                   MOVE      SCFND         QQFND
     C*
      *** END ADD ***** 06/25/10 **************************************
     C                   CALL      'W8007C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQLSDT
     C                   PARM                    QQPODT
     C                   PARM                    QQPRDT
     C                   PARM                    SCCOM1
     C                   PARM                    SCCOM2
     C                   PARM                    SCCOM3
     C                   PARM                    SCCOM4
     C                   PARM                    QQOUTQ
      ***** BEG ADD ******* 09/26/05 **********************************
     C                   PARM                    QQTFND
      ***** END ADD ******* 09/26/05 **********************************
     C*
      ***** BEG ADD ***** 09/29/06 ************************************
     C                   PARM                    QQEXCL
      ***** END ADD ***** 09/29/06 ************************************
      ***** BEG ADD ***** 09/24/07 ************************************
     C                   PARM                    QQFRGN
     C                   PARM                    QQTRGN
      ***** END ADD ***** 09/24/07 ************************************
      ***** BEG ADD ***** 01/08/09 ************************************
     C                   PARM                    SCDFLG
     C                   PARM                    SCDDRM
      ***** END ADD ***** 01/08/09 ************************************
     C***  Begin Add  ***  01/11/10  **************************************
     C                   PARM      SCDDTP        QQDDTP
     C***  End   Add  ***  01/11/10  **************************************
      *** BEG ADD ***** 06/25/10 **************************************
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
      *** END ADD ***** 06/25/10 **************************************
      ******* beg add ****** 08/02/11 *********************************
     C                   PARM                    scy4n
      ******* end add ****** 08/02/11 *********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      ***BEG ADD ***** 06/25/10 **************************************
    c*====================================================================
    c* Search groups
    c*====================================================================
     c     GroupSearch   begsr
     c                   eval      rrnGrp = GrpSrch(inkc:inkl:scFnd:##Co@)
     c                   if        rrnGrp <> *zero
     c     rrnGrp        chain     WMFNP
     c                   eval      scFnd  = FNFND
     c                   eval      scName = FNNAME
     c                   seton                                        86
     c                   endif                                                  WMFNP
     c                   if        rrnGrp = *zero
     c                   seton                                        85
     c                   endif                                                  WMFNP
     c                   endsr
    c*====================================================================
    c* Search employers
    c*====================================================================
     c     EmplrSearch   begsr
     c                   if        scFnd = *zero
     c                   exsr      GroupSearch
     c                   endif
     c                   clear                   rrnEmp
     c                   if        scFnd <> *zero
     C                   MOVE      UPCO#         ##CO@
     c                   eval      rrnEmp = EmpSrch(inkc:inkl:error:
     c                                              scFnd:##Co@)
     c                   if        rrnEmp <> *zero
     c     rrnEmp        chain     WMEMP
     c                   eval      scEmp# = EMEMP#
     c                   eval      scDiv  = %editc(EMDIV:'3')
     c                   seton                                        76
     c                   endif                                                  rrnEMP
     c                   endif                                                  scfnd <> 0
     c                   if        rrnEmp = *zero
     c                   seton                                        86
     c                   endif                                                  rrnEMP
     c                   endsr
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
      ***END ADD ***** 06/25/10 **************************************
