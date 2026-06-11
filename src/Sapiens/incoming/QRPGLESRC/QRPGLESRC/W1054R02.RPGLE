     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
      /define CONVERT
      /TITLE W1054R02 - ENDORSEMENT FORM MAINTENANCE
      *****************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
      *                OR ADD RECORDS IN THE ENDORSEMENT TABLE FILE.  *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    MENU                                                       *
      *                                                               *
      *  CALLS TO:                                                    *
      *    ENPGM3 - Additional information program                    *
      *                                                               *
      *  CREATED:           PROGRAMMER:       PROJECT:                *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 05/10/24 C:079 Ticket: 115133-133                             *
      *                Update WVEMST                                  *
      *                                                               *
      * 04/30/24 C:300 Project: WC2433, QA Testing Issues             *
      *               -Error for LDEEDT - "The Year Portion of a Date *
      *               -or Timestamp value is not in a correct range"  *
      *                                                               *
      * 02/29/24 C:300 Project: WC2433                                *
      *               -Chanegs for Endorsement Forms Additional Info link
      *                                                               *
      * 02/02/21 C:136 Project: WC2757                                *
      *               -Call TX waiver endorsement program             *
      *                in mode '3' with A/C/D in mode2.               *
      *               -Ensure policy is eligible for waiver form      *
      *                before allowing it to be added.                *
      *               -Fix bug for populating LDEEDT.                 *
      *                                                               *
      * 02/14/20 C:136 Support: 80310                                 *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *               -Move form name to different position in LDA    *
      *                to not conflict with Scriptura processing.     *
      *               -Don't update WMMUP except for Eff Date in      *
      *                update mode. The division field was being      *
      *                overwritten with the state rpt division.       *
      *               -Call ENPGM3 with the correct employer info     *
      *                from WMMUP not WDELP so that additional info   *
      *                displays correctly.                            *
      *               -Write MUDIV with the processing division, not  *
      *                the state reporting division.                  *
      *               -Remove division number from edits for duplicate*
      *                forms in ERRCK1 and ERRCKC.                    *
      *                                                               *
      * 10/24/18 C:291 Project: WC2549                                *
      *                Load Endorsement Form to LDA.                  *
      *                                                               *
      * 08/20/18 C:291 Project: 133WQP                                *
      *                Only allow Waiver Endorsements to  be added in *
      *                Quote mode.                                    *
      *                                                               *
      * 11/10/16 C:136 Project: SY371                                 *
      *                Add extended description                       *
      *                                                               *
      * 03/07/16 C:136 PhaseWare 39815                                *
      *                Write the form record before additional        *
      *                details are written per GW. The client will    *
      *                have to manually delete the form record        *
      *                if they cancel out of the detail pgm without   *
      *                writing a detail record.                       *
      *                                                               *
      * 01/11/15 C:302 Support 37601 & 38820                          *
      *                Do not copy waivers unless they are for the    *
      *                form we are copying.                           *
      *                                                               *
      * 08/29/14 C:180 Support WA133 86633                            *
      *                Record lock conflict on file WMMUP             *
      *                                                               *
      * 06/05/08 C:197 PROJECT WC1530 PRISS#6                         *
      *                ENDORSEMENTS - GENERIC END APPROVED/EXCLUDED   *
      *                STATES                                         *
      *                                                               *
      * 07/17/07  C:184 PROJECT WC1529                                *
      *                 Only attach endorsement if endorsement for is *
      *                 authorized for client.                        *
      *                                                               *
      * 04/17/07 C:61  SUPPORT WA099 44141                            *
      *                Change DLTPRE routine to delete "Addtional     *
      *                Information" data associated with              *
      *                "Manual Attach" policy page endorsements at    *
      *                time endorsement is deleted(removed) from.     *
      *                policy.                                        *
      *                                                               *
      * 03/28/07 C:184 PROJECT WC1518 AND WC1530
      *                Endorsement "Approved by State" Edit
      *                                                               *
      * 02/21/07 C:61  PROJECT 139WGW                                 *
      *                Flag as manual endorsement WMMUP/MUMNAU in     *
      *                ADD mode.                                      *
      *                                                               *
      * 05/10/05 C:61  SUPPORT WA135 46984                            *
      *              - Only Waiver records for specific endorsement   *
      *                should be deleted.                             *
      *              - Change endorsment number in Waiver File to     *
      *                zero for renewal year.                         *
      *              - Only delete Waiver records if waiver           *
      *                endorsement is being deleted.                  *
      *                                                               *
      * 03/25/05 C:61  SUPPORT WA033 46131                            *
      *              - Make "Endorsement Number" a display only       *
      *                field.                                         *
      *              - Pass "Endorsement #" in local data area.       *
      *              - Do not call "other info" program for mid-term  *
      *                endorsements. (end# <> 0).                     *
      *                                                               *
      * 06/01/00 C:90  PROJECT: CM027 SEQ#: 22859                     *
      *                Add copy waiver function.                      *
      *                                                               *
      * 08/17/99 C:113 PROJECT: WC904                                 *
      *                Changed parms for call to W1055R to match new  *
      *                parms.                                         *
      *                                                               *
      * 04/29/99 C:113 PROJECT: SUPPORT                               *
      *                Program will now call the add'l info program   *
      *                and check for an F3 or F12 before writing to   *
      *                WMMUP.  If the user hits F3 or F12 in the      *
      *                additional info program then we don't want to  *
      *                attach that endorsement.                       *
      *                                                               *
      * 04/07/99 C:113 PROJECT: SUPPORT 17554                         *
      *                Corrected display of description so that when  *
      *                an endorsement is already present, it will not *
      *                get the description of that endorsement.       *
      *                                                               *
      * 02/17/99 C:113 PROJECT: 48WPB                                 *
      *                Added code to attach endorsement to the State  *
      *                Reporting Division.                            *
      *                                                               *
      * 10/16/98 C:114 PROJECT CM027                                  *
      *                LOAD ENDORSEMENT DESCRIPTION FROM THE TABLE    *
      *                IF IT IS NOT PRESENT.                          *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.      *
      * ------------------------------------------------------------- *
      *****************************************************************
      *                                                               *
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      *  40 - ADD MODE                                                *
      * N40 - UPDATE MODE                                             *
      *  59 - FIRST PASS IN ADD MODE                                  *
      *  90 - GENERAL ERROR INDICATOR                                 *
      *                                                               *
      *  SCR01:                                                       *
      *  ERROR:                                                       *
      *  71 - (WCR0503) FORM FIELD CANNOT BE BLANK                    *
      *  72 - (WCR0505) FORM DOES NOT EXIST                           *
      *  73 - (WC30005) FORM USED PREVIOUSLY                          *
      *  74 - (WC30000) SCREEN UPDATED                                *
      *  75 - (WCR0500) ENDORSEMENT DATE INVALID                      *
      *  76 - (WCR0930) ENDORSEMENT DATE NOT IN POLICY RANGE          *
      *  77 - (WCR0547) ENDORSEMENT # CANNOT BE NEGATIVE              *
      *  79 - (WCR3209) ENDORSEMENT FORMS FOR THIS STATE NOT AVAILABLE*
      *                 CONTACT FISERV.                               *
      *                                                               *
      *****************************************************************
     FW1054F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*  Physical file definition (for maintaining record)
     FSMCOL     IF   E           K DISK
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDB)
     fwmmula    if   e           k disk
     FWMMUP     UF A E             DISK
     F                                     RENAME(PMU01:DBRCDF)
     FWTENL     IF   E           K DISK
      *
      * Waiver
     FWMWVL1    UF A E           K DISK
      *
     FWXFML11   UF A E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      /COPY SCOPYMT01D
     D*
     D FNFDB           DS
     D  FNRRN                397    400B 0
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26  0
      *** beg add ***** 02/02/21 ******************
     d  ldWvrMode             32     32
      *** end add ***** 02/02/21 ******************
     d  ldStSpcfFlg           40     40
     d  ldForm                41     55
     D*
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Endorsement-
     D                                      Forms')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D #OFSCR          C                   CONST(1)
     D WVFRM1          C                   CONST('WC000313       ')
     D WVFRM2          C                   CONST('LUB0020199     ')

     D*================================================================
     D* Stand Alone Fields
     D*================================================================
     D##Env            S              1a
     D@@Env            S              1a
     DflgEnv           S              1a
      *** beg add ***** 02/29/24 **************************
     d @@RRN           s              9  0
     d @@RRN2          s              9  0
      *** end add ***** 02/29/24 **************************
      *** beg add ***** 02/02/21 **************************
     d chrAddPgm       s             10
     d chrPrtPgm       s             10
     d chrState        s              2    Inz('TX')
     d chrWvBlkPrjDs   s            500
     d chrWvrForm      s             15
      /Copy wPrEndorse
      /Copy wPrEmplr
      *** end add ***** 02/02/21 **************************
      //*** Beg Add 05/10/24 *****************************************
       dcl-s intStRptDiv Like(WVdiv);
      //*** End Add 05/10/24 *****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
      *** beg chg ***** 02/29/24 *************
     C*****              PARM                    @@RRN             9 0
     C*****              PARM                    @@RRN2            9 0
     C                   PARM                    xxRRN             9
     C                   PARM                    xxRRN2            9
      *** end chg ***** 02/29/24 *************
     C                   PARM                    @@ENV
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C     SCAGT#        PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD
     C                   PARM                    ##PNAM           60
     C*
     C*  Compress the employer header information parameter list
     C*
     C     CMPRPL        PLIST
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Employer Attached Endorsement Form file
     C*
     C     MUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##FYR
     C                   KFLD                    ##FORM
     C                   KFLD                    ##SEQ#
     C     MUKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##FYR
     C                   KFLD                    ##FORM
     C*
     C*  Endorsement Form file
     C*
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FORM
     C*
     C*  Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C     EMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C*
     C*  Company Master file
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C*  Fund Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  Waiver Information File
     C*
     C     WVKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C*
     C     WVKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C                   KFLD                    ##WAV#
     C*
     C     WVKY2         KLIST
     C                   KFLD                    MUCO#
     C                   KFLD                    MUFND
     C                   KFLD                    MUEMP#
     C                   KFLD                    MUDIV
     C                   KFLD                    MUFYR
     C                   KFLD                    MUEND#
     C*
     C*  Assesable Forms Data Fill File
     C*
     C     FMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C                   KFLD                    ##FORM
     C*
     C*  Employer Detail File
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    SCTFYR
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MUCO#         ##CO#
     C     *LIKE         DEFINE    MUFND         ##FND
     C     *LIKE         DEFINE    MUEMP#        ##EMP#
     C     *LIKE         DEFINE    MUDIV         ##DIV
     C     *LIKE         DEFINE    MUFYR         ##FYR
     C     *LIKE         DEFINE    MUFORM        ##FORM
     C     *LIKE         DEFINE    MUEND#        ##END#
     C     *LIKE         DEFINE    MUSEQ#        ##SEQ#
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C     *LIKE         DEFINE    MUFORM        @@FORM
     C     *LIKE         DEFINE    MUFORM        XXFORM
     C     *LIKE         DEFINE    MUEEDT        XXEEDT
     C     *LIKE         DEFINE    WVWAV#        ##WAV#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING INCLUDE/EXCLUDE STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     MUEXFL        IFEQ      'I'
     C                   MOVE      'E'           MUEXFL
     C                   ELSE
     C                   MOVE      'I'           MUEXFL
     C                   ENDIF                                                  MUEXFL
     C*
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C*
     C                   MOVEL     DSTIME        MUTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
      **** BEGIN ADD **** 06/01/00 ************************************
     C*================================================================
     C* CPYWAV - Subroutine for copying waivers processing
     C*================================================================
     C     CPYWAV        BEGSR

     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON

     C                   MOVE      *OFF          *IN90

     C                   EXFMT     SCRCPY
     C                   EXSR      ERRCKC

     C                   ENDDO

     C     *INKL         IFEQ      *OFF
      *  Determine next avail Waiver # for copy TO policy year.
     C                   MOVEL     SCTFYR        ##FYR
     C     WVKY          SETGT     PWV01
     C     WVKY          READPE    PWV01                                  60

     C     *IN60         IFEQ      *OFF
     C     WVWAV#        ADD       1             NXTWAV            5 0
     C                   ELSE
     C                   Z-ADD     1             NXTWAV
     C                   ENDIF

     C                   MOVEL     SCFYR         ##FYR
     C                   MOVE      *LOVAL        ##WAV#
     C     WVKY1         SETLL     PWV01
     C     WVKY          READE     PWV01                                  60

     C     *IN60         DOWEQ     *OFF
     C     WVENM         IfEq      ##FORM
     C                   MOVEL     SCTFYR        WVFYR
     C                   Z-ADD     0             WVEND#
     C                   Z-ADD     NXTWAV        WVWAV#
     C                   ADD       1             NXTWAV

     C                   MOVEL     Q1USER        WVUSER
     C                   MOVEL     Q1USER        WVIUSR
     C                   MOVEL     Q1PGM         WVPGM
     C                   MOVEL     Q1PGM         WVIPGM

     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WVDT
     C                   MOVEL     SRDTO         WVIDT

     C                   MOVEL     DSTIME        WVTIME

     C     FMKY          CHAIN     PFM01                              60
     C     *IN60         IFEQ      *OFF

     C                   MOVEL     SCTFYR        F3FYR

     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1USER        F3IUSR
     C                   MOVEL     Q1PGM         F3PGM
     C                   MOVEL     Q1PGM         F3IPGM

     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F3DT
     C                   MOVEL     SRDTO         F3IDT

     C                   MOVEL     DSTIME        F3TIME

     C                   WRITE     PFM01
     C                   ENDIF                                                  *IN60

       //*** Beg Add *** 05/10/24 ******************************************
       intStRptDiv = GetStRptDv(WVco#:WVfnd:WVemp#);
       WVemst = GetRatingState(WVco#:WVfnd:WVemp#:intstRptDiv:
         WVfyr);
       //*** End Add *** 05/10/24 ******************************************
     C                   WRITE     PWV01
     C                   Endif

     C     WVKY          READE     PWV01                                  60
     C                   ENDDO

     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUEEDT

     C                   MOVEL     SCTFYR        MUFYR
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1USER        MUIUSR

     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     Q1PGM         MUIPGM

     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C                   MOVEL     SRDTO         MUIDT

     C                   MOVEL     DSTIME        MUTIME

     C                   WRITE     DBRCDF

     C                   ENDIF                                                  *INKL

     C                   RETURN

     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
      * Delete "Additional Information" data associated with
      * policy page endorsement
     C     MUEND#        IFEQ      0
     C                   CALL      'W1054R04'
     C                   PARM      MUCO#         #1CO#             3 0
     C                   PARM      MUFND         #1FND             3 0
     C                   PARM      MUEMP#        #1EMP#            9 0
     C                   PARM      MUFYR         #1FYR             3 0
     C                   PARM      MUFORM        #1FORM           15
     C                   ENDIF                                                  MUFORM = 0
      *
     C                   ENDSR                                                  DLTPRE
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
     C*
     C*  Form field cannot be blank
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCFORM        ANDEQ     *BLANK
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCFORM
     C*Valid Endorsement State
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C*
     C                   CALL      'SRVAES'
     C                   PARM      SCFORM        #@FORM           15
     C                   PARM      *BLANK        #@RTN             1
     C*
     C     #@RTN         IFEQ      'N'
     C                   SETON                                        79  90
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*  Verify that the form is in the table file
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C*
     C                   MOVEL     SCFORM        ##FORM
     C     ENKY          CHAIN     PEN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        72  90
     C                   Else
     C                   If        flgEnv = 'Q' and
     C                             EnType <> 'Waiver'
     C                   SETON                                        72  90
     C                   Endif
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF                                                  *IN90 = *OFF

      //*** beg add ***** 02/02/21 ***********************
       Select;
       When ENtype <> 'Waiver';
       When *In90 = *On;
       When *In40 = *Off;
       Other;
          chrWvrForm = ENform;
          If GetWaiverInfo(ELco#:ELfnd:chrState:ELpfdt:chrWvrForm:
           chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:ELfyr:ELemp#) = *Off;
             errNoWvrDs = chrWvBlkPrjDs;
             *In80 = *On;
             *In90 = *On;
          Endif;
       Endsl;
      //*** end add ***** 02/02/21 ***********************

     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C*
     C                   CALL      'SRCKES'
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    ELFYR
     C                   PARM                    SCFORM
     C                   PARM      ' '           #ATTCH            1
     C*
     C     #ATTCH        IFEQ      'N'
     C     XXOVR         ANDEQ     'N'
     C     #ATTCH        OREQ      'N'
     C     XXFORM        ANDNE     SCFORM
     C                   EXFMT     WDWRNW
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      '0'           *INKL
     C                   ELSE
     C                   MOVE      'Y'           XXOVR
     C                   MOVE      SCFORM        XXFORM
     C                   ENDIF                                                  *INKL=*ON
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*  Check to see if the form has already been attached
     C*
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN90         ANDEQ     *OFF                                         NO ERRORS SO FAR
     C                   MOVEL     SCFORM        ##FORM
     C                   MOVEL     SCEND#        ##END#
     C     MUKY          SETLL     PMU01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF
     C*
     C*  Endorsement date must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        75  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCEEDT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Endorsement date must fall within the policy date range
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEEDT
     C*
     C     XXEEDT        IFLT      ELPFDT
     C     XXEEDT        ORGT      ELPTDT
     C                   SETON                                        76  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C*
     C*  Check to be sure the screen has not been updated since entry
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90MUDT          IFGT      SAVEDT
     C     MUDT          OREQ      SAVEDT
     C     MUTIME        ANDGT     SAVETM
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C*  Redisplay screen to show endorsement description, if necessary
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN59         ANDEQ     *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C     *IN90         OREQ      *OFF
     C     SCDESC        ANDEQ     *BLANK
     C                   MOVE      *ON           *IN59
     C                   MOVE      *ON           *IN90
     C                   MOVEL     'I'           SCEXFL
      /Free
       If ENdsc2 <> *blanks;
          scDesc = Endsc2;
       Else;
          scDesc = Endesc;
       Endif;
      /End-free
     C                   MOVEL     ENST          SCST
     C                   MOVEL     ENMNAU        SCMNAU
     C                   MOVEL     ENPRNT        SCPRNT
     C                   ENDIF                                                  90='0',59='0',40='1'
     C*
     C*  We have passed all edits, go to the next screen
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
      **** BEGIN ADD **** 06/01/00 ************************************
     C*================================================================
     C* ERRCKC - Error checking for copy window (SCRCPY)
     C*================================================================
     C     ERRCKC        BEGSR
     C*
     C*  Policy year and endorsement date must be entered
     C*
     C     SCTFYR        IFEQ      *ZEROS
     C                   SETON                                        23  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C  N90SCENDT        IFEQ      *ZEROS
     C                   SETON                                        24  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  Verify that the policy year exists
     C*
     C     ELKY          CHAIN     PEL02                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        20  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  Verify that the endorsement date is valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        25  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCENDT
     C                   ENDIF                                                  SRERR = *ON
      *
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Endorsement date must be between employer effective dates
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   SETON                                        26  90
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT            7 0
     C*
     C     XXENDT        IFGE      ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C                   SETOFF                                       26  90
     C                   ENDIF                                                  XXENDT GE ELPFDT
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Verify that the form does not already exist for FYR entered
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   MOVEL     MUFORM        ##FORM
     C                   MOVEL     SCTFYR        ##FYR
     C  N90MUKY1         CHAIN     PMU01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   SETON                                        22  90
     C                   ENDIF
     C*
     C                   ENDSR
      **** END   ADD **** 06/01/00 ************************************
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10 (SEARCHES)
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C*  Endorsement form search
     C*
     C     HFLD          IFEQ      'SCFORM'
     C*
     C                   CALL      'W1055R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      FNRRN         ##RRN             9 0
     C                   PARM      *BLANK        XXFORM
     C                   PARM                    ##ENV
     *
     C*
     C     XXFORM        IFNE      *BLANKS
     C                   MOVEL(P)  XXFORM        SCFORM
     C                   ENDIF                                                  @@FORM<>*BLANKS
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* OTHINF - Call program for additional info entry if required.
     C*================================================================
     C     OTHINF        BEGSR
     C*
     C*  If there is an additional information program, call it now
     C*
     C     ENPGM3        IFNE      *BLANKS
      /Free
       q#Co# = %EditC(MUco#:'X');
       q#Fnd = %EditC(MUFnd:'X');
       q#Emp# = %EditC(MUEmp#:'X');
       q#Div = %EditC(MUdiv:'X');
       q#Fyr = %EditC(MUfyr:'X');
      /End-Free
      *
      *  LOAD ENDORSEMENT NUMBER TO LOCAL DATA AREA POSITION 1 THRU 5
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVE      SCEND#        LDEND#
     C                   MOVE      *ZERO         LDMYRR
     C                   MOVE      ELFYR         LDFYR
     C                   MOVE      SCFORM        LDFORM
      *** beg chg ***** 02/02/21 ***************************
      *** Wrong date going into dtaara...
     C***                MOVE      SCENDT        LDEEDT
       ldEEDt = scEEDt;
       Select;
       When ENtype <> 'Waiver';
       When @@mode = 'A';
          ldWvrMode = 'A';
       When @@mode = *Blank;
          ldWvrMode = 'C';
       When @@mode = '1' Or @@mode = 'D';
          ldWvrMode = '1';
       Endsl;
      *** end chg ***** 02/02/21 ***************************
     C                   OUT       *DTAARA
     C*
     C*
     C     @@MODE        IFEQ      '1'
     C     @@MODE        OREQ      'D'
     C     SCEND#        ORNE      *ZERO
     C                   MOVE      '1'           Q#MOD2
     C                   ELSE
     C                   MOVE      *BLANK        Q#MOD2
     C                   ENDIF

     C                   CALL      ENPGM3
     C                   PARM                    Q#CO#             3
     C                   PARM                    Q#FND             3
     C                   PARM                    Q#EMP#            9
     C                   PARM                    Q#DIV             5
     C                   PARM                    Q#FYR             3
     C                   PARM      '3'           Q#MODE            1
     C                   PARM      @@RRN2        Q#RRN             9 0
     C                   PARM                    Q#MOD2            1
     C                   ENDIF                                                  ENPGM3 <> ' '
     C*
     C                   ENDSR                                                  OTHINF
      *
      *
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEEDT
     C*
     C                   MOVEL     MUFORM        SCFORM
     C                   MOVEL     MUEND#        SCEND#
      /Free
       If MUdsc2 <> *blanks;
          scDesc = MUdsc2;
       Else;
          scDesc = MUdesc;
       Endif;
      /End-free
     C                   MOVEL     MUENST        SCST
     C                   MOVEL     MUEXFL        SCEXFL
     C                   MOVEL     MUMNAU        SCMNAU
     C                   MOVEL     MUPRNT        SCPRNT
     C                   ELSE
     C                   MOVEL     *BLANK        SCFORM
     C                   MOVE      *ZERO         SCEEDT
     C                   MOVEL     *ZERO         SCEND#
     C                   MOVEL     *BLANK        SCDESC
     C                   MOVEL     *BLANK        SCST
     C                   MOVEL     *BLANK        SCEXFL
     C                   MOVEL     *BLANK        SCMNAU
     C                   MOVEL     *BLANK        SCPRNT
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUEEDT
      /Free
       If Not(*In40);
          LeaveSr;
       Endif;
      /End-Free
     C                   Z-ADD     ##CO#         MUCO#
     C                   Z-ADD     ##FND         MUFND
     C                   Z-ADD     ##EMP#        MUEMP#
     C                   Z-ADD     ##DIV         MUDIV
     C                   Z-ADD     ##FYR         MUFYR
     C                   MOVE      SCEND#        MUEND#
     C                   MOVE      SCFORM        MUFORM
     C                   MOVEL     SCST          MUENST
      /Free
       MUdesc = ENdesc;
       MUdsc2 = ENdsc2;
      /End-free
     C*
     C                   MOVEL     SCEXFL        MUEXFL
     C                   MOVEL     SCMNAU        MUMNAU
     C   40              MOVEL     'M'           MUMNAU
     C                   MOVEL     SCPRNT        MUPRNT
     C   40              MOVEL     'A'           MUSTAT
     C   40              MOVEL     *ZEROS        MUSEQ#
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
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      *  DISPLAY DETAIL RECORDS
      *
     C     @@MODE        IFEQ      'D'
     C                   MOVEL     SCFORM        ##FORM
     C     ENKY          CHAIN     PEN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   EXSR      OTHINF
     C                   ENDIF                                                  ENKY
      *
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      /free
         ##Env = ' ';
         FlgEnv = ' ';
         If %Parms > 5;
            If @@ENV <> *Blanks;
               ##Env = @@ENV;
               FlgEnv = @@ENV;
            Endif;
         Endif;
      //*** beg add ***** 02/29/24 ***********************
       @@rrn = %Dec(xxRRN:9:0);
       @@rrn2 = %Dec(xxRRN2:9:0);
      //*** end add ***** 02/29/24 ***********************
      /End-free
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  Load screen headings
     C*
     C*  Get Employer Detail record
     C*
     C     @@RRN2        CHAIN     PEL01                              60
     C*
      //*** beg add ***** 02/29/24 ***********************
      /free
       If @@mode = 'F' or @@mode = 'R';    // F - Change mode, R - Add (create) mode
          If @@mode = 'F';
             @@mode = 'C';
          Endif;
          If @@mode = 'R';
             @@mode = 'A';
          Endif;

          Chain(n) @@RRN DBRCDF;
          If %found(wmmup);
             scform = MUform;
             //*** beg chg ***** 04/30/24 ***********************
             //*** sceedt = MUeedt;
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEEDT
             //*** end chg ***** 04/30/24 ***********************
             scend# = MUend#;
             Chain (MUco#:MUfnd:MUform) Pen01;
             If %found(wtenl);
                Exsr othInf;
             Endif;
          Endif;
          Return;
       Endif;
      /End-free
      //*** end add ***** 02/29/24 ***********************
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELFND         ##FND
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELFYR         ##FYR
     C                   MOVE      ELFYR         SCFYR
     C                   MOVE      ELEMP#        ##EMP#
     C                   MOVE      ELDIV         ##DIV
     C*
     C                   MOVEL     *BLANK        ##CONM
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVEL     FNNAME        ##FNAM
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   MOVEL     ##FNAM        SCNAME
     C*
     C*  Get Employer Master record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  Compress EMP#/DIV/NAME
     C*
     C                   CALL      CMPRES        CMPRPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C                   MOVE      *OFF          *IN58                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN58                          CANCELLED
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
     C*
     C*  Get Agent/Producer information
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C     @@MODE        CASEQ     'P'           CPYWAV
     C                   ENDCS
     C*
     C*  Do not display info fields if this is the first time in ADD
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVE      *OFF          *IN59
     C                   ELSE
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     C* Flag for overriding endorsement exclude validation
     C                   MOVE      'N'           XXOVR             1
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
      /Free
          exsr OTHINF;

       // Check Q#MOD2 for an 'F' indicating that an F3 or an F12 was
       // pressed in the additional info program.

          If Q#MOD2 <> 'F';
      /End-Free
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C                   MOVEL     DSTIME        MUTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF
      /Free
          Endif;
      /End-Free
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     Q1PGM         MUIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C                   MOVEL     SRDTO         MUIDT
     C                   MOVEL     DSTIME        MUTIME
      /free
       Write dbrcdf;
       Exsr othInf;
      /end-free
     C                   FEOD      WMMUP
     C                   ENDSR
      **

