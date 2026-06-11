     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0165R - WORK WITH CANCELLATIONS/REINSTATEMENTS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL CX FOR *
     H*                A SELECTED POLICY YEAR.  EACH CX CAN BE        *
     H*                ACCESSED FOR UPDATE, DISPLAY OR SELECTION      *
     H*                DEPENDING ON THE CALLING PROGRAM.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH EMPLOYER POLICY YEARS                   *
     H*  CALLS TO:                                                    *
     H*   -CANCELLATIONS                                              *
     H*      W0165R02 - CANCELLATION INFORMATION (OPT 2/4/5)          *
     H*      W0165C   - PRINT CANCELLATION (OPT 6)                    *
     H*      W0045R02 - DISPLAY PREM FIN CO DETAILS (F7)
     H*   -REINSTATEMENTS                                             *
     H*      W0167R   - REINSTATEMENT INFORMATION (OPT 2/4/5)         *
     H*      W0167C   - PRINT REINSTATEMENT (OPT 6)                   *
     H*      W0045R02 - DISPLAY PREM FIN CO DETAILS (F7)              *
     H*                                                               *
     H*  CREATED: 06/12/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
      * REVISIONS:                                                    *
      *
      * 07/25/05 C:61  PROJECT 133WBV
      *                Display Cancellation Count.
      *
      * 08/03/04 C:61  PROJECT WC966
      *                Do not allow revised CX if CX date < policy
      *                eff date.  Do not allow revise on renewal year
      *                policy.
     H*                                                               *
     H* 07/21/03 C:61  SUPPORT WA126 38052                            *
     H*                Change so "Adress Label (R/L)" no longer       *
     H*                required input for reprint.                    *
     H*                                                               *
     H* 12/09/02 C:61  SUPPORT WA101 35024                            *
     H*                Use CX issue date to determine which CX reason *
     H*                code description to display.                   *
     H*                                                               *
     H* 11/27/01 C:61  SUPPORT WA107 29814  107BQ                     *
     H*                Only use active WTCRP records.                 *
     H*                                                               *
     H* 05/04/01 C:61  SUPPORT WA113 27362                            *
     H*                Do not allow opt 2=Change to enter             *
     H*                reinstatement if CX issue date is in the       *
     H*                future.                                        *
     H*                                                               *
     H*  12/07/99 C:118 PROJECT: WC956                                *
     H*                 Group not loading correctly.                  *
     H*                                                               *
     H*  12/01/99 C:118 PROJECT: WC956                                *
     H*                 Disable Opt 2=Change if called from work with *
     H*                 reinstatements and policy year is audited.    *
     H*                                                               *
     H*  09/29/99 C:118 PROJECT: WC939                                *
     H*                 Added 'Premium Finance Co.' in RI in display  *
     H*                 if a premium finance co. has been assigned to *
     H*                 the policy.  Added F7=Finance info which calls*
     H*                 W0045R02 WW Premium Finance Co. maintenance   *
     H*                 to display finance company details. Added     *
     H*                 WMPFP.                                        *
     H*
     H* 08/10/99  C:87  PROJECT: CM046                                *
     H*                 Load "Deletd Division" to cancellation reason *
     H*                 description if XHENFL='Y'.  Add an edit if    *
     H*                 CXENFL='Y': only allow opt5 if in 'WW Canx'   *
     H*                 or not allow any options if in 'WW Reinstmt'. *
     H*                                                               *
     H* 07/29/99  C:87  PROJECT: WC757                                *
     H*                 Remove option 4=Delete.  Add code to not allow*
     H*                 option 2=Change if a subsequent cancellation  *
     H*                 has been processed (i.e. higher request #).   *
     H*                                                               *
     H* 02/24/99  C:87   PROJECT: SUPPORT TASB #NONE                  *
     H*                  Change to use the current date as opposed to *
     H*                  the policy eff date when reading the canx    *
     H*                  reason code table file(WTCRP).               *
     H*                                                               *
     H* 11/10/98  C:87  PROJECT: WC852                                *
     H*                 Change to use WTCRL instead of WTABL to get   *
     H*                 the cancellation reason code record.          *
     H*                                                               *
     H* 10/01/98  C:87  PROJECT: WC762                                *
     H*                 CHANGE DATES TO OUTPUT AS 6 DIGITS.  ADD      *
     H*                 CONFIRMATION FLAG(CXCFFL) AND CANCELLED IN    *
     H*                 ERROR FLAG(CXERR) TO SCREEN.                  *
     H*                                                               *
     H* 04/03/98  C:78  PROJECT: WC825                                *
     H*                 ALLOW ACCESS TO DARTS. F14 ADDED.             *
     H*                                                               *
     H* 12/29/97  C:35  PROJECT: WC735                                *
     H*                 DO NOT ALLOW OPTION 2 (CHANGE) ON A CX ONCE   *
     H*                 IT HAS BEEN REINSTATED.                       *
     H*                                                               *
     H* 12/24/97  C:35  PROJECT: WC735                                *
     H*                 ADD ADDRESS LABEL TYPE PARAMETER TO THE       *
     H*                 REPRINT CALL.                                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  45 - Display Prem Fin Co code and name in RI
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0165F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWDCXP     IF   E             DISK
     F                                     RENAME(PCX01:PFRCDF)
     FWDCXL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCX01:DBRCDF)
     FWDCXL2    IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F***** BEGIN ADD **** 09/29/99 ***********************************
     F                                     INFDS(DBFDSG)
     F***** END   ADD **** 09/29/99 ***********************************
     FWMUPL     IF   E           K DISK
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTABL   IF  E           K        DISK
     FWTCRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F***  END   CHG  ***  11/10/98  **********************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)
     F***** BEGIN ADD **** 09/29/99 ***********************************
     F* Logical for Premium Finance Co.
     F*
     FWMPFL     IF   E           K DISK    INFDS(DBFDSF)
     F*
     F***** END   ADD **** 09/29/99 ***********************************
      /COPY SCOPYWW01D
     D*
     D OF1             S              1    DIM(35)
     D HD              S             79    DIM(8) CTDATA PERRCD(1)              HEADERS
      ****** BEGIN ADD ****** 04/03/98 ********************************
     D******************* OF      1  30  1   OFD    35   Options, Function
     D***** BEGIN CHG **** 09/29/99 ***********************************
     D OF              S              1    DIM(31) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(31) ALT(OF)
     D**********          OF      1  32  1   OFD    35   Options, Function
     D***** END   CHG **** 09/29/99 ***********************************
      ****** END   ADD ****** 04/03/98 ********************************
      ***** BEG ADD ***** 08/03/04 ************************************
      ***** END ADD ***** 08/03/04 ************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D***** BEGIN ADD **** 09/29/99 ***********************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D***** END   ADD **** 09/29/99 ***********************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
      ****** BEGIN ADD ***** 04/03/98 *********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      ****** END   ADD ***** 04/03/98 *********************************
     D*
     D OPTNMC          C                   CONST('Cancellations')
     D OPTNMR          C                   CONST('Reinstatements')
     D DELETD          C                   CONST('*Deleted')
     D***  BEGIN ADD  ***  08/10/99  **********************************
     D DELDIV          C                   CONST('Deleted Division')
     D***  END   ADD  ***  08/10/99  **********************************
     D ACDPGC          C                   CONST('W0165R02')
     D ACDPGR          C                   CONST('W0167R')
     D REPRTC          C                   CONST('W7165C')
     D REPRTR          C                   CONST('W7167C')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D SF1PAG          C                   CONST(8)
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(1)
     D*
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      ***** BEG ADD ***** 08/03/04 ************************************
      *
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
      *
      *
      *
      *

     C     *LIKE         DEFINE    EMCAD1        XXCXDT
      ***** END ADD ***** 08/03/04 ************************************
     C     *LIKE         DEFINE    CXREQ#        KKREQ#
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C     *LIKE         DEFINE    CRKEY         XXKEY
     C     *LIKE         DEFINE    EMNAME        U1PDSC
     C     *LIKE         DEFINE    EMNAME        U2PDSC
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    UPLVL         SVLVL
      ******** BEGIN ADD **** 04/03/98 ********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ******** END   ADD **** 04/03/98 ********************************
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    ELRTCD        KKRTCD
     C***  END   ADD  ***  11/10/98  **********************************
     C***** BEGIN ADD **** 09/29/99 **********************************
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C***** END   ADD **** 09/29/99 **********************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*  CANCELLATION RECORD
     C*
     C     CXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXREQ#
     C*
     C     CXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*  EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C*  TABLE FILE
     C**********
     C********** TBKY      KLIST
     C**********           KFLD           ELCO#
     C**********           KFLD           XXKEY
     C*
     C*  Rate set code file
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKRTCD
     C*
     C*  Cancellation reason code file
     C     CRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C***  END   CHG  ***  11/10/98  **********************************
     C*
     C*  SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    KKREQ#
     C*
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  PREMIUM FINANCE CO. KEY
     C*
     C     PFKY          KLIST
     C                   KFLD                    UPCO#
     C***** BEGIN CHG **** 12/07/99 ***********************************
     C**********           KFLD           UPFND
     C                   KFLD                    SCFND
     C***** END   CHG **** 12/07/99 ***********************************
     C                   KFLD                    SCFCOD
     C*
     C     PFKY2         KLIST
     C                   KFLD                    UPCO#
     C***** BEGIN CHG **** 12/07/99 ***********************************
     C**********           KFLD           UPFND
     C                   KFLD                    SCFND
     C***** END   CHG **** 12/07/99 ***********************************
     C                   KFLD                    SCFCOD
     C                   KFLD                    XXPFDT
     C***** END   ADD **** 09/29/99 ***********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@TYPE            1
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C***  BEGIN CHG  ***  10/01/98  **********************************
     C*******************  PARM *BLANKS   SRTFMT  8
     C                   PARM      '*MDY'        SRTFMT            8
     C***  END   CHG  ***  10/01/98  **********************************
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C     SCENFL        WHENEQ    'Y'
     C     @@TYPE        ANDEQ     'C'
     C     SCSEL         ANDNE     '5'
     C                   SETON                                        81  90
     C*
     C     SCENFL        WHENEQ    'Y'
     C     @@TYPE        ANDEQ     'R'
     C     SCSEL         ANDNE     *BLANK
     C                   SETON                                        81  90
     C***  END   ADD  ***  08/10/99  **********************************
      **** BEGIN ADD **** 12/29/97 *********************************************
      *
      * CHANGE IS NOT ALLOWED ON A CANCELLATION ONCE IT HAS BEEN REINSTATED
      *
     C     SCSEL         WHENEQ    '2'
     C     @@TYPE        ANDEQ     'C'
     C     SCREDT        ANDNE     0
      *
     C                   SETON                                        79  90
      *
      **** END   ADD **** 12/29/97 *********************************************
     C***  BEGIN ADD  ***  07/29/99  **********************************
     C     SCSEL         WHENEQ    '2'
     C*  Find the last request #
     C                   MOVE      *HIVAL        XXREQ#
     C     CXKY          SETGT     PCX02
     C     CXKY1         READPE    PCX02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXREQ#        ANDGT     SCREQ#
     C                   SETON                                        80  90
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   ADD  ***  07/29/99  **********************************
      ***** BEG ADD ***** 08/03/04 ************************************
      *
      *  Do not allow revision if auto CX of renewal year
      *
     C     XXCXDT        IFNE      *ZERO
     C     @@TYPE        ANDEQ     'C'
     C     XXCXDT        ANDLT     ELPFDT
     C                   SETON                                        90  72
     C                   ENDIF
      *
      *  Do not allow reinstatement of renewal year processed as
      *  automatic flat cancellation.
      *
     C     XXCXDT        IFNE      *ZERO
     C     @@TYPE        ANDEQ     'R'
     C     XXCXDT        ANDLT     ELPFDT
     C                   SETON                                        90  73
     C                   ENDIF
      ***** END ADD ***** 08/03/04 ************************************
     C*
     C***  BEGIN DEL  ***  07/29/99  **********************************
     C********** SCSEL     WHEQ '4'
     C**********
     C***DELETION (OPT 4) ALLOWED ONLY WHEN
     C******1. THE ENTERED REQUEST# IS THE LAST REQ# FOR EMPLOYER AND
     C******2. THERE IS NO REINSTATEMENT DATE AND
     C******3. USER SECURITY LEVEL IS GREATER THAN OR EQUAL TO THE
     C*********LEVEL ALLOWED IN THE APPLICATION SYSTEM FILE.
     C**********
     C***FIND LAST REQUEST#
     C**********
     C**********           MOVE *HIVAL    XXREQ#
     C********** CXKY      SETGTPCX02
     C********** CXKY1     REDPEPCX02                    60
     C**********
     C**N60*****           SELEC
     C********** SCREQ#    WHNE CXREQ#
     C**********           SETON                     75  90
     C********** SCREDT    WHNE *ZERO
     C**********           SETON                     76  90
     C********** SVLVL     WHLT WCSL04
     C**********           SETON                     77  90
     C**********           ENDSL
     C***  END   DEL  ***  07/29/99  **********************************
     C*
     C**
     C     SCSEL         WHENEQ    '6'
     C*
     C     @@TYPE        IFEQ      'R'
     C     SCREDT        ANDEQ     *ZERO
     C                   SETON                                        78  90
     C                   ENDIF                                                  @@TYPE=R/SCREDT=*ZER
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C*
      ***** BEG ADD ******* 05/04/01 **********************************
      *
      * Reinstatement not allowed if CX issue date in the future.
      *
     C     SCSEL         IFEQ      '2'
     C     @@TYPE        ANDEQ     'R'
     C     SYSDT         ANDLT     S1CDAT
     C                   SETON                                        90  71
     C                   ENDIF
      *
      ***** END ADD ******* 05/04/01 **********************************
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C                   MOVE      *HIVAL        KKREQ#
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKREQ#
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C                   MOVE      SCREQ#        KKREQ#
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CXREQ#        SCREQ#
     C*
     C                   MOVEL(P)  *BLANKS       SCDESC
     C                   MOVEL     CXRSN         XXKEY
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** TBKY ***  CHAINPCR01                60
     C**** N60 **********  MOVELCRDESC    SCDESC    P
     C*
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C***  BEGIN CHG  ***  02/24/99  **********************************
     C******************** Z-ADDELPFDT    KKEFDT
      ***** BEG CHG ******* 12/09/02 **********************************
     C*********            Z-ADDSYSDT     KKEFDT
     C                   Z-ADD     CXCDAT        KKEFDT
      ***** END CHG ******* 12/09/02 **********************************
     C***  END   CHG  ***  02/24/99  **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 11/27/01 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 11/27/01 **********************************
     C  N60              MOVEL(P)  CRDESC        SCDESC
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   CHG  ***  11/10/98  **********************************
     C***  BEGIN ADD  ***  08/10/99  **********************************
     C                   MOVE      CXENFL        SCENFL
     C     CXENFL        IFEQ      'Y'
     C                   MOVEL(P)  DELDIV        SCDESC
     C                   ENDIF
     C***  END   ADD  ***  08/10/99  **********************************
      ***** BEG ADD ******* 05/04/01 **********************************
     C                   Z-ADD     CXCDAT        S1CDAT
      ***** END ADD ******* 05/04/01 **********************************
     C*
     C                   Z-ADD     CXCDAT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C                   Z-ADD     CXCXDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCCXDT
     C*
     C                   Z-ADD     CXRDAT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCRDAT
     C*
     C                   Z-ADD     CXREDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCREDT
     C*
     C***  BEGIN ADD  ***  10/01/98  **********************************
     C                   MOVE      CXCFFL        SCCFFL
     C                   MOVE      CXERR         SCERR
     C***  END   ADD  ***  10/01/98  **********************************
     C***** beg add ****** 07/25/05 ***********************************
     C                   Z-ADD     CXCNC#        SCCNC#
     C***** end add ****** 07/25/05 ***********************************
     C                   ENDSR                                                  MVDBSC
      ***  BEGIN ADD  ***  04/03/98  *********************************
      *
      *
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C***  END   ADD  ***  04/03/98  *********************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*===============================================================
     C* KGDO - SUBROUTINE FOR F7
     C*===============================================================
     C*
     C*  F7=FINANCE INFO
     C*
     C     KGDO          BEGSR
     C*
     C                   CALL      'W0045R02'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      DBRRNF        @@RRN             9 0
     C                   PARM      DBRRNG        @@RRN2            9 0
     C*
     C                   ENDSR                                                  KGDO
     C*
     C***** END   ADD **** 09/29/99 ***********************************
     C*
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6
     C*===============================================================
     C*
     C* OPTION 6=RE-PRINT CANCELLATION/REINSTATMENT
     C*
     C     OPT6          BEGSR
     C*
     C                   MOVE      *OFF          *IN27
     C                   MOVE      *ON           *IN90
     C                   MOVEL     'N'           SCCERT
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C*  VALIDATE THE CERTIFICATE FLAG
     C*
     C  NKLSCCERT        IFNE      'Y'
     C     SCCERT        ANDNE     'N'
     C                   MOVE      *ON           *IN27
      **** BEGIN ADD **** 12/24/97 *********************************************
     C                   MOVE      *ON           *IN90
      **** END   ADD **** 12/24/97 *********************************************
     C                   ELSE
     C                   MOVE      *OFF          *IN90
     C                   ENDIF                                                  SCCERT IFNE 'Y'
      **** BEGIN ADD **** 12/24/97 *********************************************
      *
      * ADDRESS LABEL TYPE MUST BE 'R' OR 'L'
      *
     C  NKL
     CANN90SCTYPE        IFNE      'R'
     C     SCTYPE        ANDNE     'L'
      ***** BEG ADD ******* 07/21/03 **********************************
     C     SCTYPE        ANDNE     *BLANK
      ***** END ADD ******* 07/21/03 **********************************
      *
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN90
      *
     C                   ELSE
      *
     C                   MOVE      *OFF          *IN90
      *
     C                   ENDIF                                                  SCTYPE IFNE 'R'
      **** END   ADD **** 12/24/97 *********************************************
     C*
     C   KL              MOVE      *OFF          *IN90                          *IN90=*OFF
     C*
     C                   ENDDO                                                  *IN90=*OFF
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  SUBMIT CANCELLATION/REINSTATEMENT PRINT PROGRAM
     C*
     C                   MOVE      ##CO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCREQ#        QQREQ#            5
     C                   MOVEL(P)  UPOUTQ        QQOUTQ           10
      **** BEGIN ADD **** 12/24/97 *********************************************
     C                   MOVE      SCTYPE        QQTYPE            1
      **** END   ADD **** 12/24/97 *********************************************
     C*
     C                   CALL      REPRT                                99
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQREQ#
     C                   PARM                    SCCERT
     C                   PARM      'Y'           QQREPR            1
     C                   PARM                    QQOUTQ
      **** BEGIN ADD **** 12/24/97 *********************************************
     C                   PARM                    QQTYPE
      **** END   ADD **** 12/24/97 *********************************************
     C*
     C                   CALL      'WA007R'                             99
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDSR                                                  OPT6
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
      ******* BEGIN ADD ****** 04/03/98 *******************************
     C     *INKN         CASEQ     *ON           KNDO
      ******* END   ADD ****** 04/03/98 *******************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C     *INKG         CASEQ     *ON           KGDO
     C***** END   ADD **** 09/29/99 ***********************************
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C                   MOVE      ##CO#         ##CO#
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C  N09              MOVE      ' '           ##CALL            1
     C   09              MOVE      '1'           ##CALL
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL(P)  OPTNMC        OPTNM            40
     C                   MOVEL(P)  ACDPGC        ACDPGM           10
     C                   MOVEL(P)  REPRTC        REPRT            10
     C*
     C***** BEGIN CHG **** 09/29/99 ***********************************
     C**********           MOVEAOFD,26    OF1
     C**********           MOVEA'YNN'     OF1,28
     C**********           MOVEAOF1       OFD,26
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C***** END CHG ****** 09/29/99 ***********************************
     C*
     C     Q1PRMS        IFGT      5
     C*
     C     @@TYPE        IFEQ      'R'
     C*
     C                   MOVEL(P)  OPTNMR        OPTNM
     C                   MOVEL(P)  ACDPGR        ACDPGM
     C                   MOVEL(P)  REPRTR        REPRT
     C*
     C*  DO NOT ALLOW USE OF F6=ADD IF USING REINSTATEMENTS
     C*
     C***** BEGIN CHG **** 09/29/99 ***********************************
     C*
     C**********           MOVEAOFD,26    OF1
     C**********           MOVEA'NNN'     OF1,28
     C**********           MOVEAOF1       OFD,26
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C***** END   CHG **** 09/29/99 ***********************************
     C*
     C                   ENDIF                                                  @@TYPE=R
     C*
     C                   ENDIF                                                  Q1PRMS>5
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    09    SEARCH
     C*
     C*  GET SYSTEM TABLE VALUE
     C*
     C                   Z-ADD     *ZERO         WCSL04
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C                   Z-ADD     *ZEROS        SVLVL
     C                   MOVEL     *BLANKS       UPOUTQ
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C  N60              Z-ADD     UPLVL         SVLVL
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN2        CHAIN     PEL01                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C* IF A PREMIUM FINANCE CO. HAS BEEN ASSIGNED TO THE POLICY, LOAD
     C*
     C                   MOVEL     ELPFIN        SCFCOD
     C                   MOVEL     ELPFDT        XXPFDT
     C***** BEGIN ADD **** 12/07/99 ***********************************
     C                   MOVEL     ELFND         SCFND
     C***** END   ADD **** 12/07/99 ***********************************
     C                   MOVE      *OFF          *IN45
     C*
     C     SCFCOD        IFNE      *BLANKS
     C                   MOVE      *ON           *IN45
     C*
     C*  DISPLAY 'F7=FINANCE INFO'
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C     PFKY2         SETGT     PPF01
     C*
     C     PFKY          READPE    PPF01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PFNAME        SCPFNM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ELSE
     C*
     C*  REMOVE 'F7=FINANCE INFO' FROM SCREEN
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF                                                  *SCFCOD<>BLK
     C*
     C***** END ADD ****** 09/29/99 ***********************************
     C***** BEGIN ADD **** 12/01/99 ***********************************
     C*
     C*  IF CALLED FROM REINSTATEMENTS AND POLICY YEAR IS AUDITED
     C*     DISABLE OPT 2=CHANGE.
     C*
     C     Q1PRMS        IFGT      5
     C*
     C     @@TYPE        IFEQ      'R'
     C     ELFAF         ANDEQ     'A'
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '02'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ELSE
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '02'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF                                                  @@TYPE=R
     C*                                                    ELFAF=A
     C                   ENDIF                                                  Q1PRMS>5
     C*
     C***** END   ADD **** 12/01/99 ***********************************
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
      ***** BEG ADD ***** 08/03/04 ************************************
      *
      *  Get Employer Cancellation date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     EF(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     CD(X)         XXCXDT
      ***** END ADD ***** 08/03/04 ************************************
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C  N09FNEXDT        IFNE      *ZEROS
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   Z-ADD     SCFYR         ##FYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDESC
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE     FNNAME        U1PDSC
     C     LO:UP         XLATE     SSPDSC        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF                                                  SCNRES=0
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Headers - HD
Opt Req #  Cancel Reason    Issued  Effective  Cnt  Err  Cx   Issued  Effective
Opt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=Reprint                YYY
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=Finance info          YYN
211F11=Change View          NNN
212F12=Cancel               YYY
214F14=Darts                YYN
3
