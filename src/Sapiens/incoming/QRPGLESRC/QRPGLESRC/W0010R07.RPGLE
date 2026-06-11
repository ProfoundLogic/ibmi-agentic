     Hcopyright('(c) 2024 Sapiens, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H bnddir('STBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0010R07 - GROUP AUXILIARY INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE GROUP AUXILIARY FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH GROUPS                                  *
     H*    W0010R02 - GROUP INFORMATION                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/16/96  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/16/24 C:340 PROJECT: 153WAD                                *
     H*                Add 2 New Field.                               *
     H*                1. Renewal Reminder Notice Flag                *
     H*                2. Renewal Reminder CX Reason Code             *
     H*                                                               *
     H* 09/18/23 C:300 Project: WC2196 Support: 113869 (SUNZ)         *
     H*                Getting error 'Job queue GRP614 in QGPL not    *
     H*                found' when saving Group Auxiliary Information *
     H*                with value in 'Group Billing Job Queue Name'   *
     H*                To Fix --                                      *
     H*                Do not call W0010C02 at data center clients    *
     H*                                                               *
     H* 10/19/21  C:65  Project: Phw#98606 (SCS) - WC1866             *
     H*                 Update to check for job queue object in same  *
     H*                 library as the subsystem exists in.           *
     H*                                                               *
     H* 04/28/20 C:079 Support: 146/88598                             *
     H*                Remove edit on IDC NCCI Group Carrier Code     *
     H*                                                               *
     H* 12/23/19 C:180 Project: 133IRJ                                *
     H*                Add Include in Darts Extract                   *
     H*                                                               *
     H* 10/02/19 C:062 Project: 114WPM                                *
     H*                Add 1099 Override FEIN                         *
     H*                                                               *
     H* 08/19/19 C:079 Project: WC2604                                *
     H*                Add IDC field for Carrier Group Code           *
     H*                                                               *
     H* 12/26/17 C:267 PROJECT C150WCI                                *
     H*                Add Business Segment ID#                       *
     H*                                                               *
     H* 10/02/17 C: 53 PROJECT 114WHY                                 *
     H*                Add Freeform Description Field                 *
     H*                                                               *
     H* 02/16/17 C:136 Support: 45752                                 *
     H*                Don't invoke edits on output logo file name    *
     H*                if not populated.                              *
     H*                                                               *
     H* 12/06/16 C:65  Project: SY371                                 *
     H*                Add path field for logo.                       *
     H*                                                               *
     H* 09/15/16 C:65  Project: SY371                                 *
     H*                Add output logo file name to screen 1 for      *
     H*                Scriptura or other output purposes.            *
     H*                                                               *
     H* 07/25/16 C:136 Project: 114WYQ                                *
     H*                Add Disable SMS Text check box                 *
     H*                                                               *
     H* 10/15/15 C:65  Project: WC2386                                *
     H*                Change prompt text for logo/signature check    *
     H*                boxes for policy page to reflect header/footer *
     H*                info is to be disabled.                        *
     H*                                                               *
     H* 01/20/15 C:295 PROJECT: 129WTI                                *
     H*                Add Assessments to Auto Write-Off              *
     H*                                                               *
     H* 11/22/13 C:279 PROJECT: 129WSB                                *
     H*                Added code for processing for fields added on  *
     H*                Group Auxiliary Screen03.                      *
     H*                                                               *
     H* 08/26/11 C:136 PROJECT: WC1918                                *
     H*                DO NOT DISPLAY POLICY PAGE 3C INFO FIELDS IF   *
     H*                WC1918 IS ACTIVE IN THE CLIENT ENV             *
     H*                                                               *
     H* 10/25/10 C:255 PROJECT: WC1866                                *
     H*                Added code for processing for fields added on  *
     H*                Group Auxiliary Screen03.                      *
     H*                                                               *
     H* 08/17/10 C:235 PROJECT: SY190                                 *
     H*                From SCR03 field "*currently not used*" is     *
     H*                Removed.                                       *
     H*                                                               *
     H* 04/23/10 C:61  PROJECT WC1880                                 *
     H*              - Add field SCR02 for entry of page segment used *
     H*                for signature on Certificate of Insurance.     *
     H*              - Add search on SCCEOV field used for entry of   *
     H*                overlay used to print Certificate of Insurance.*
     H*                                                               *
     H* 12/02/09 C:136 Project: 25WPZ                                 *
     H*                Add field for fiscal year start month          *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ADD XML FILE TYPE TO CHECK TYPE                *
     H*                                                               *
     H*  06/25/09 C:220 Project: 114WTR                               *
     H*                 Interface with Zurich                         *
     H*                                                               *
     H*  05/20/09 C:33  Project: WC1770                               *
     H*                 Medicare Secondary Payer                      *
     H*                                                               *
      *  12/28/05 C:167 Project: WC1364
      *                 Add field for NAIC code
      *
     H*  09/31/05 C:62 Project: WA135 49163                           *
     H*                ADD CERT OVERLAY FIELD PROCESSING.             *
     H*                                                               *
     H*  06/30/05 C:65 Project: 135WCV                                *
     H*                ADD ENTRIES FOR POLICY PAGE DOCUMENT GRAPHICS. *
     H*                                                               *
     H*  03/05/05 C:132 Project: WA099  SEQ#: 45931                   *
     H*                Do not change to ADD mode if no record exists  *
     H*                for the group.                                  *
     H*                                                               *
     H*  11/13/03 C:65 Project: WC1167                                *
     H*                ADDED ENTRY FOR BILLING LOGO PAGE SEGMENT.     *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX information on second screen.  *
     H*                                                               *
     H*  02/18/00 C:15 Project: WC976                                 *
     H*                Added entry for 'Group Serviced by TPA' flag.  *
     H*                                                               *
     H*  01/13/99 C:90 PROJECT: WC858                                 *
     H*                ADD ENTRY OF 2ND LINE OF ITEM 3.C.             *
     H*                                                               *
     H*  07/06/98 C:33 PROJECT: 21WAI                                 *
     H*                DO NOT VALIDATE FIELDS THAT ARE NOT DISPLAYED. *
     H*                                                               *
     H*  05/04/98 C:78 PROJECT: WC577                                 *
     H*                IRS CODE CAN BE ALPHA-NUMERIC. ERROR MESSAGE   *
     H*                CHANGE.                                        *
     H*                                                               *
     H*  03/23/98 C:85 PROJECT: WC773                                 *
     H*                DETERMINE IF CLIENT IS ADMIN-ONLY (IND. 94),   *
     H*                OR CLAIMS-ONLY (IND. 95).                      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H*  65 - WCPOLS INSTALLED                                        *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  20 - (WCR0502) Federal Tax Id must be > zero                 *
     H*  21 - (WCR0547) IRS Transmittal Code cannot be < zero         *
     H*  22 - (WCR0547) Internal Audit # cannot be < zero             *
     H*  23 - (WCR0547) Service CO/TPA Code cannot be < zero          *
     H*  24 - (WCR0547) NCCI Carrier # cannot be < zero               *
     H*  25 - (WCR0501) WCPOLS Electronic Reporting must be 'Y' or 'N'*
     H*  26 - (WCR0501) Process Claim Checks must be 'Y' or 'N'       *
     H*  27 - (WCR0503) Claim Bank Account Code cannot be blanks      *
     H*  28 - (WCR0736) Claim Bank Account Code invalid for Group     *
     H*  29 - (WCR0501) Process Agent Comm Checks must be 'Y' or 'N'  *
     H*  30 - (WCR0503) Agent Bank Account Code cannot be blanks      *
     H*  31 - (WCR0736) Agent Bank Account Code invalid for group     *
     H*  32 - (WCR0547) Min Amt for Commission Check cannot be < zero *
     H*                                                               *
     H* SCR02:                                                        *
     H*  30 - (WCR0826) Group Logo Folder must exist                  *
     H*  31 - (SCM0517) User does not have authourity to Group Logo Flr*
     H*  32 - (SCM0517) Group Logo Document must exist                *
     H*  36 - (WCR0502) 2ND Deviation must be > zero                  *
     H*                                                               *
     H* SCR03:                                                        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0010F07  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDF2P     UF A E             DISK
     F                                     RENAME(PF201:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDF2L     IF   E           K DISK
     F                                     INFDS(DBFDS2)
      *
     F***** BEG ADD **** 02/13/03 **********************************
     FSTSTL3    IF   E           K DISK
     F***** END ADD **** 02/13/03 **********************************
      *_____________________________________________________________________
      *  Bank Info file       key=CO#/APPL/CKTP/GRP/EFDT omit BISTAT='I'
     F***WMBIL5  IF  E           K        DISK
      *_____________________________________________________________________
     FWMFNP     IF   E             DISK
     FSSYLF     IF   E           K DISK
     F***** BEG ADD **** 06/25/09 **********************************
     F*  TPA Contact Master Information
     FWMTCP     IF   E             DISK
     F*  TPA Contact Master Information (TPCO#/TPTPAC)
     FWMTCL     IF   E           K DISK
     F                                     RENAME(PTC01:PTC02)
     F***** END ADD **** 06/25/09 **********************************
      ***** BEG ADD ******* 04/23/10 **********************************
      *
      * Generic Table File
     FWTGTL     IF   E           K DISK
      ***** END ADD ******* 04/23/10 **********************************
      *************** Bed Add *********** 11/22/13 ********************
     FWTABL     IF   E           K DISK
     F                                     ignore(PSY01)
     F                                     ignore(PPC01)
     F                                     ignore(PRC01)
     F                                     ignore(PBC01)
     F                                     ignore(PCS01)
      *************** End Add *********** 11/22/13 ********************
      ***  Beg Add  ***  01/16/24  *** 153WAD ***********************
     FWTCRL1    IF   E           K DISK
      ***  End Add  ***  01/16/24  *** 153WAD ***********************
      ** BEG ADD ***** 07/27/09 ********************************************
      /COPY SCOPYMT01D
     D CHD             S             37    DIM(3) CTDATA PERRCD(1)              CHECK TYPE HDR
      ** END ADD ***** 07/27/09 ********************************************
      **************** Beg Add ************ 11/22/13 ***********************
     D##Call           s              1
     D##Kc#            s              1
     D##Kl#            s              1
     D##Key            s             10
      **************** End Add ************ 11/22/13 ***********************
      **************** Beg Add ************ 01/20/15 ***********************
     D##Ctyp           s                   like (TTctyp)
     DisError          s                   like (*in90)
      **************** End Add ************ 01/20/15 ***********************
    D*** begin add ***** 12/06/16 ********************************************
     d strSlash        s              1
     d strSlash2       s              1
    D*** end add ******* 12/06/16 ********************************************
      ***  Begin Add ***** 09/18/23 *****************************
     d suffix          s              3    inz
      ***  End   Add ***** 09/18/23 *****************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Group Auxiliary Inf-
     D                                     ormation')
      *** BEG CHG ***** 12/02/09 *************************************
     D***********   2                     C         #OFSCR
     D #OFSCR          C                   CONST(3)
      *** END CHG ***** 12/02/09 *************************************
     D*** BEG ADD ***** 10/25/10 *************************************
     D                 DS
     D  XXSBS                  1     10
     D*** END ADD ***** 10/25/10 *************************************
     D COMAPI          C                   CONST('WA001R')
     D*
     D XXDVHD          C                   CONST('Deviation')
     D XXDVH2          C                   CONST('from (M,S,N):')
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D*
     D                 DS
     D  F2LZIP                 1      9  0
     D  F2LZP1                 1      5  0
     D  F2LZP2                 6      9  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)

      **************** Beg Add ************ 11/22/13 *****************
     DcallW2161R       pr                  extpgm('W2161R')
     D##Call                               like(##Call)
     D##Kc#                                like(##Kc#)
     D##Kl#                                like(##Kl#)
     D##Key                                like(##Key)
      **************** End Add ************ 11/22/13 *****************
      ***  Begin Add ***** 09/18/23 *****************************
      // Retrieve Client Suffix
     d RtvCltSuffix    pr                  ExtPgm('SA030R')
     d suffix                         3
      ***  End   Add ***** 09/18/23 *****************************
      *** beg add ***** 07/25/16 ******************
      /copy SprModule
      *** end add ***** 07/25/16 ******************
     C*
      ***** BEG ADD ******* 04/23/10 **********************************
      *
      *
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
      ***** END ADD ******* 04/23/10 **********************************
     C***** BEG ADD ******* 10/25/10 **********************************
     C     *DTAARA       DEFINE    JOBQSBS       XXSBS
     C***** END ADD ******* 10/25/10 **********************************
     C********** *LIKE     DEFN BICKTP    XXACCT
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
      *
      *
      *================================================================
      *  PLIST
      *================================================================
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      ***** BEG ADD ******* 04/23/10 **********************************
      *
      *
      *================================================================
      *  KLIST
      *================================================================
      *
      *  GENERIC TABLE
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      ***** END ADD ******* 04/23/10 **********************************
     C*
     C*  GROUP AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    SCFND
     C*
     C***** BEG ADD **** 06/25/09 **********************************
     C*
     C*  TPA CONTACT MASTER INFORMATION
     C*
     C     KTC01         KLIST
     C                   KFLD                    FNCO#             3 0
     C                   KFLD                    XXTPAC           10
     C***** END ADD **** 06/25/09 **********************************
     C*
      *************** Beg Add ************ 11/22/13 ****************
     C     KeyWTABL      KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY            10
      *************** End Add ************ 11/22/13 ****************
     C***BANK ACCOUNT MASTER FILE
     C*****
     C*****      BIKY      KLIST
     C*****                KFLD           FNCO#
     C*****                KFLD           XXAPPL  2
     C*****                KFLD           XXACCT
     C*****                KFLD           SCFND
     C*
      ***** BEG ADD **** 02/13/03 **********************************
     C*  SYSTEM TABLE FILE - STATE CODES
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      ***** END ADD **** 02/13/03 **********************************
      *
     C*  KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  FEDERAL TAX ID MUST BE GREATER THAN ZERO
     C*
     C  N90SCFEIN        IFLE      *ZERO
     C                   SETON                                        9020
     C                   ENDIF                                                  SCFEIN<=*ZERO
     C****** BEGIN CHG ******* 05/04/98 *******************************
     C*
     C***IRS TRANSMITTAL CODE CANNOT BE LESS THAN 0
     C*  IRS TRANSMITTAL CODE CANNOT BE BLANK
     C*
     C**N90      SCTCC     IFLT *ZERO
     C  N90SCTCC         IFEQ      *BLANKS
     C                   SETON                                        9021
     C                   ENDIF                                                  SCTCC<*ZERO
     C****** END   DEL ******* 05/04/98 *******************************
     C*
     C*  INTERNAL AUDIT# CANNOT BE LESS THAN 0
     C*
     C  N90SCIAUD        IFLT      *ZERO
     C                   SETON                                        9022
     C                   ENDIF                                                  SCIAUD<*ZERO
     C*
     C*  SERVICE CO/TPA CODE CANNOT BE LESS THAN 0
     C*
     C  N90SCSCTC        IFLT      *ZERO
     C                   SETON                                        9023
     C                   ENDIF                                                  SCSCTC<*ZERO
     C*
     C*** BEGIN ADD *** 02/18/00 **************************************
     C*  SERVICE CO/TPA CODE CANNOT BE LESS THAN 0
     C*
     C  N90SCTPA         IFNE      *BLANKS
     C     SCTPA         ANDNE     'Y'
     C     SCTPA         ANDNE     'N'
     C                   SETON                                        9035
     C                   ENDIF
     C*** END ADD ***** 02/18/00 **************************************
     C*
     C*  NCCI CARRIER # CANNOT BE LESS THAN 0
     C*
     C  N90SCCAR#        IFLE      *ZERO
     C     SCWCPL        ANDEQ     'Y'
     C***  BEGIN ADD  ***  07/06/98  *********************************
     C     *IN95         ANDEQ     *OFF                                         NOT CLAIM ONLY
     C***  END   ADD  ***  07/06/98  *********************************
     C                   SETON                                        9024
     C                   ENDIF                                                  SCCAR#<*ZERO
     C*** Beg Add *** 08/19/19 ***************************************
      /Free
       //*** Beg Del *** 04/28/20 ******************************************
       //**********
       //**********      erNCCIgpCd = *off;
       //**********      If isModEnabled('IDC') and scNcarGrp = 0;
       //**********         *in90 = *on;
       //**********         erNCCIgpCd = *on;
       //**********         mgNCCIgpCd = 'This field is required for NCCI +
       //**********           Indemnity Data Call';
       //**********       Endif;
       //*** End Del *** 04/28/20 ******************************************
      /End-Free
     C*** End Add *** 08/19/19 ***************************************
     C*
     C*  WCPOLS ELECTRONIC REPORTING MUST BE 'Y' OR 'N'
     C*
     C     *IN65         IFEQ      *ON
     C*
     C  N90SCWCPL        IFNE      'Y'
     C     SCWCPL        ANDNE     'N'
     C***  BEGIN ADD  ***  07/06/98  *********************************
     C     *IN95         ANDEQ     *OFF                                         NOT CLAIM ONLY
     C***  END   ADD  ***  07/06/98  *********************************
     C                   SETON                                        9025
     C                   ENDIF                                                  SCWCPL<>Y/N
     C*
     C                   ENDIF                                                  *IN65
     C*
     C*  PROCESS CLAIM CHECKS MUST BE 'Y' OR 'N'
     C*
     C  N90SCCLCK        IFNE      'Y'
     C     SCCLCK        ANDNE     'N'
     C***  BEGIN ADD  ***  07/06/98  *********************************
     C     *IN94         ANDEQ     *OFF                                         NOT ADMIN ONLY
     C***  END   ADD  ***  07/06/98  *********************************
     C                   SETON                                        9026
     C                   ENDIF                                                  SCCLCK<>Y/N
      *
      *  Type of Payment 'C' or 'V' (If SCCLCK is 'Y')
      *  CSI is allowed type X for XML as well
      *
      *** BEG ADD ***** 07/27/09 *************************************
     C  N90              SELECT
     C     CLTID         WHENEQ    '114'
     C     SCCLCK        IFEQ      'Y'
     C     SCCTYP        ORNE      *BLANK
     C     SCCTYP        IFNE      'C'
     C     SCCTYP        ANDNE     'V'
     C     SCCTYP        ANDNE     'X'
     C     *IN94         ANDEQ     *OFF                                         NOT ADMIN ONLY
     C                   SETON                                        9071
     C                   ENDIF                                                  SCCTYP<>C/V/X
     C                   ENDIF                                                  SCCLCK/SCCTYP
     C                   OTHER
      *** END ADD ***** 07/27/09 *************************************
     C  N90SCCLCK        IFEQ      'Y'
     C     SCCTYP        ORNE      *BLANK
     C     SCCTYP        IFNE      'C'
     C     SCCTYP        ANDNE     'V'
     C***  BEGIN ADD  ***  07/06/98  *********************************
     C     *IN94         ANDEQ     *OFF                                         NOT ADMIN ONLY
     C***  END   ADD  ***  07/06/98  *********************************
     C                   SETON                                        9034
     C                   ENDIF                                                  SCCTYP<>C/V
     C                   ENDIF                                                  SCCLCK/SCCTYP
      *** BEG ADD ***** 07/27/09 *************************************
     C                   ENDSL
      *** END ADD ***** 07/27/09 *************************************
     C*
     C**N90*     SCCLCK    IFEQ 'Y'
     C******
     C***CLAIM BANK ACCOUNT CODE CAN NOT BE BLANK
     C******
     C**N90*     SCACCT    IFEQ *BLANKS
     C******               SETON                     9027
     C******               ENDIF                           SCACCT=*BLANKS
     C******
     C***CLAIM BANK ACCOUNT CODE MUST BE VALID AND ACTIVE FOR THE GROUP
     C***(LOGICAL OMITS INACTIVE STATUS)
     C******
     C******               MOVELSCACCT    XXACCT
     C******               MOVE 'WC'      XXAPPL
      ******
     C******     BIKY      CHAINPBI01                60
     C******
     C**N90*     *IN60     IFEQ *ON
     C******               SETON                     9028
     C******               ENDIF                           *IN60=*ON
     C******
     C******               ENDIF                           SCCLCK=Y
     C*
     C*  PROCESS AGENT COMMISSION CHECKS MUST BE 'Y' OR 'N'
     C*
     C  N90SCAGCK        IFNE      'Y'
     C     SCAGCK        ANDNE     'N'
     C***  BEGIN ADD  ***  07/06/98  *********************************
     C     *IN95         ANDEQ     *OFF                                         NOT CLAIM ONLY
     C***  END   ADD  ***  07/06/98  *********************************
     C                   SETON                                        9029
     C                   ENDIF                                                  SCAGCK<>Y/N
     C*
     C**N90*     SCAGCK    IFEQ 'Y'
     C******
     C***AGENT BANK ACCOUNT CODE CAN NOT BE BLANK
     C******
     C**N90*     SCAACC    IFEQ *BLANKS
     C******               SETON                     9030
     C******               ENDIF                           SCAACC=*BLANKS
     C******
     C***AGENT BANK ACCOUNT CODE MUST BE VALID AND ACTIVE FOR THE GROUP
     C***(LOGICAL OMITS INACTIVE STATUS)
     C******
     C******               MOVELSCAACC    XXACCT
     C******               MOVE 'AG'      XXAPPL
     C******
     C******     BIKY      CHAINPBI01                60
     C**N90*     *IN60     IFEQ *ON
     C******               SETON                     9030
     C******               ENDIF                           *IN60=*ON
     C******               ENDIF                           SCAGCK=Y
     C*
     C*  MINIMUM AMT. FOR COMMISSION CHECK CAN NOT BE LESS THAN 0
     C*
     C  N90SCAGCK        IFEQ      'Y'
     C  N90SCMACC        IFLT      *ZERO
     C***  BEGIN ADD  ***  07/06/98  *********************************
     C     *IN95         ANDEQ     *OFF                                         NOT CLAIM ONLY
     C***  END   ADD  ***  07/06/98  *********************************
     C                   SETON                                        9032
     C                   ENDIF                                                  SCMACC<*ZERO
     C*
     C                   ENDIF                                                  SCAGCK=Y
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F2DT          IFGT      SAVEDT
     C     F2DT          OREQ      SAVEDT
     C     F2TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C***  BEGIN ADD  ***  06/25/09  *********************************
     C*
     C* Validate against TPA Contact Master Information
     C*
     C                   MOVE      SCTPAC        XXTPAC
     C*
     C     KTC01         CHAIN     PTC02                              60
     C     *IN60         IFEQ      *ON
     C     SCTPAC        ANDNE     *BLANKS
     C                   SETON                                          4590
     C                   ENDIF
     C*
     C***  END   ADD  ***  06/25/09  *********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
      *
      * If Lock Box info is entered, edit all lock box fields- valid data.
      ***** BEG ADD ***** 02/13/03 ***************************************
     C     SCLAD1        IFGT      *BLANKS
     C     SCLAD2        ORGT      *BLANKS
     C     SCLCTY        ORGT      *BLANKS
     C     SCLZP1        ORGT      *ZEROS
     C*
     C*  THE ADDRESS LINE CANNOT BE BLANK
     C*
     C  N90SCLAD1        IFEQ      *BLANK
     C     SCLAD2        ANDGT     *BLANK
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCMAD1=*BLANK
     C  N90SCLAD1        IFEQ      *BLANK
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCMAD1=*BLANK
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C  N90SCLCTY        IFEQ      *BLANK
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCMCTY=*BLANK
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCLST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        74  90
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C  N90SCLZP1        IFLE      *ZERO
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCMZP1<=*ZERO
     C*
     C                   ENDIF
      ***** END ADD ***** 02/13/03 ***************************************
      *
     C*** BEGIN ADD ***  11/13/03  ************************************
     C*
     C*  BILLING LOGO MUST BE A VALID PAGE SEGMENT OBJECT IF ENTERED.
     C*
     C  N90SCBLOG        IFNE      *BLANKS
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCBLOG        SRNAME           10
     C                   PARM      '*PAGSEG'     SROTYP            7
     C                   PARM                    SRERR             1
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        37  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCBLOG IFNE
     C*** END ADD *****  11/13/03  ************************************
      ***** BEG ADD ******* 04/23/10 **********************************
      *
      *  CERT OF INS - OVERLAY - MUST EXIST ON SYSTEM
      *
     C  N90SCCEOV        IFNE      *BLANKS
      *
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCCEOV        #2NAME           10
     C                   PARM      '*OVL   '     #2OTYP            7
     C                   PARM      *BLANK        #2ERR             1
      *
     C     #2ERR         IFEQ      '1'
     C                   SETON                                        78  90
     C                   ENDIF
      *
     C                   ENDIF                                                  SCBLOG IFNE
      *
      *  CERT OF INS - OVERLAY - MUST EXIST IN SYSTEM TABLE FILE 'COVL'
      *                          IF ENTERED
      *
     C  N90SCCEOV        IFNE      *BLANKS
     C                   MOVEL     'COVL'        KKTBID
     C                   MOVEL(P)  SCCEOV        KKKEY
     C     GTKY          CHAIN     PGT01                              79
     C   79              SETON                                        90
     C                   ENDIF                                                  SCCEOV
      *
      *  CERT OF INS - PAGSEG MUST EXIST ON SYSTEM
      *
     C  N90SCSPSG        IFNE      *BLANKS
      *
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCSPSG        #2NAME           10
     C                   PARM      '*PAGSEG'     #2OTYP            7
     C                   PARM      *BLANK        #2ERR             1
      *
     C     #2ERR         IFEQ      '1'
     C                   SETON                                        80  90
     C                   ENDIF
      *
     C                   ENDIF                                                  SCSPSG
      *
      ***** END ADD ******* 04/23/10 **********************************
     C*
     C*  FOLDER MUST BE VALID (IF ENTERED)
     C*
     C  N90SCFOLD        IFNE      *BLANKS
     C*
     C                   MOVE      ' '           QQRTCD            1
     C*
     C                   CALL      'SVALFLR'
     C                   PARM                    SCFOLD
     C                   PARM                    QQRTCD
     C*
     C*    FOLDER MUST EXIST
     C*
     C  N90QQRTCD        IFEQ      '1'
     C                   SETON                                        9030
     C                   ENDIF                                                  QQRTCD=1
     C*
     C*    USER MUST HAVE AUTHORITY TO FOLDER
     C*
     C  N90QQRTCD        IFEQ      '2'
     C                   SETON                                        9031
     C                   ENDIF                                                  QQRTCD=2
     C*
     C                   ENDIF                                                  SCFOLD<>*BLANKS
     C*
     C*  DOCUMENT MUST BE VALID (IF ENTERED)
     C*
     C                   MOVEL     SCDCID        QQDCID
     C                   MOVEL     SCFOLD        QQFOLD
     C*
     C  N90SCDCID        IFNE      *BLANKS
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD
     C*
     C  N90QQRTCD        IFEQ      '1'
     C                   SETON                                        9032
     C                   ENDIF                                                  QQRTCD=1
     C*
     C                   ENDIF                                                  SCFCID<>*BLANKS
     C*** BEGIN ADD ***  06/30/05  ************************************
     C*
     C* DOCUMENT HAS LOGO FLAG MUST BE VALID.
     C*
     C  N90SCDOCL        IFNE      'Y'
     C     SCDOCL        ANDNE     *BLANK
     C                   SETON                                        9038
     C                   ENDIF                                                  SCDOCL IFNE
     C*
     C* DOCUMENT HAS SIGNATURE FLAG MUST BE VALID.
     C*
     C  N90SCDOCS        IFNE      'Y'
     C     SCDOCS        ANDNE     *BLANK
     C                   SETON                                        9039
     C                   ENDIF                                                  SCDOCS IFNE
     C*
     C*** END ADD *****  06/30/05  ************************************
       //*** begin add *** 09/15/16 ************************
      /free
       If scSLogo <> *blanks;
          Clear erSLogo;
          Clear msSLogo;

          If %scan('.':scSLogo) = 0;
             msSLogo = 'Logo file name must have a file name extension.';
             erSlogo = *on;
             *in90 = *on;
          Endif;
       //*** begin add *** 12/06/16 ************************
          Clear erSLogoPth;
          Clear msSLogoPth;

          // Image path must be entered.
          If scSLogoPth = ' ' and scSLogo <> ' ';
             msSLogoPth = 'Logo image file path must be entered.';
             erSLogoPth = *on;
             *in90 = *on;
          Else;

          // Path must begin and end with a '/' character.
             strSlash  = %subst(scSLogoPth:1:1);
             strSlash2 = %subst(scSLogoPth:%len(%trim(scSLogoPth)):1);

             If strSlash <> '/' or strSlash2 <> '/';
                msSLogoPth = 'Logo image file path must begin and end ' +
                 'with a ''/''.';
                erSLogoPth = *on;
                *in90 = *on;
             Endif;

          Endif;
       Endif;
       //*** end add ***** 12/06/16 ************************
      /end-free
       //*** end add ***** 09/15/16 ************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F2DT          IFGT      SAVEDT
     C     F2DT          OREQ      SAVEDT
     C     F2TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
      /free
       //********** Beg Add ********** 11/22/13 **********************

       //*** Beg Add *** 01/16/24 *** 153WAD **********************
       If *In90 = *Off and scRNAC = 'Y';
          If scRcxRsn = ' ';
             msRCXRSN = 'Enter Cancellation Reason Code.';
             *in82 = *On;
             *in90 = *On;
          Else;
             Chain (##CO#:scRcxRsn) WTCRL1;
              If Not %Found;
                 msRCXRSN = 'No cancellation reason codes exist ' +
                            'for this value.';
                 *in83 = *On;
                 *in90 = *On;
              EndIf;
          Endif;
       Endif;
       //*** End Add *** 01/16/24 *** 153WAD **********************

       // Threshold values must be in ascending order, if entered.
       If *in90 = *off;
          If scThRl > scThRh;
             *in75 = *on;
             *in90 = *on;
          Endif;

          // If A/R deposit adjustment code entered,
          // A/R adjustment code must be:
          // 'I' - Deposit Payment Type
          // 'D' - Deposit Category Type
          // Active status

          If scDbCd <> *blanks;
             kkKey = scDbCd;
             Chain KeyWTABL PTT01;
             If not %found or TTstat <> 'A' or TTtype <> 'I' or TTctyp <> 'D';
                *in76 = *on;
                *in90 = *on;
             Endif;
          Endif;

          // If A/R premium adjustment code entered,
          // A/R adjustment code must be:
          // 'P' - Premium Payment Type
          // 'P' - Premium Category Type
          // Active status

          If scPbCd <> *blanks;
             kkKey = scPbCd;
             Chain KeyWTABL PTT01;
             If not %found or TTstat <> 'A' or TTtype <> 'P' or TTctyp <> 'P';
                *in79 = *on;
                *in90 = *on;
             Endif;
          Endif;

          // If A/R fee adjustment code entered,
          // A/R adjustment code must be:
          // 'Q' - Fee Payment Type
          // 'F' - Fee Category Type
          // Active status

          If scFbCd <> *blanks;
             kkKey = scFbCd;
             Chain KeyWTABL PTT01;
             If not %found or TTstat <> 'A' or TTtype <> 'Q' or TTctyp <> 'F';
                *in81 = *on;
                *in90 = *on;
             Endif;
          Endif;
          //********** Beg Add ********** 01/20/15 **********************
          // If A/R assessment write-off code entered,
          // A/R adjustment code must be:
          // 'F','Q' - Assessment Payment Type
          // '1','2','3','4','5','6','7' - Assessment Category Type
          // Active status
          // Validation for adjustment code
             Exsr AssCode;
          //********** End Add ********** 01/20/15 **********************
       Endif;
       //********** End Add ********** 11/22/13 **********************
       //*** Begin Add *** 12/23/19 *********************************
       If scDxTr = *blank;
          scDxTr = 'N';
       Endif;
       //*** End   Add *** 12/23/19 *********************************
      /End-free
      *** BEG ADD ***** 12/02/09 *************************************
     C  N90SCFYSM        IFLT      0
     C     SCFYSM        ORGT      12
     C                   SETON                                        9071
     C                   ENDIF
      *** END ADD ***** 12/02/09 *************************************
     C***** BEG ADD ******* 10/25/10 **********************************
     C*Check for Valid JobQ entered at Screen.
     C  N90SCJOBQ        IFNE      *BLANKS
     C*CALL PROGRAM TO CHECK THE VALIDITY OF NAME ENTERED AT SCREEN
     C                   CALL      'SRCKOBJN'
     C                   PARM                    SCJOBQ
     C                   PARM                    @@FLG             1
     C*IF INVALID
     C     @@FLG         IFEQ      '1'
     C                   SETON                                        9054
     C                   ENDIF
     C                   ENDIF
     C*Subsystem/JobQ are Mutually Exclusive
     C  N90SCJOBQ        IFNE      *BLANKS
     C     SCSBS         ANDEQ     *BLANKS
     C                   SETON                                        9053
     C                   ENDIF
     C*Subsystem/JobQ are Mutually Exclusive
     C  N90SCJOBQ        IFEQ      *BLANKS
     C     SCSBS         ANDNE     *BLANKS
     C                   SETON                                        9053
     C                   ENDIF
     C*Check existence of Subsystem entered at Screen.
     C  N90SCSBS         IFNE      *BLANKS
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCSBS         OBJNAM           10
     C                   PARM      '*SBSD'       OBJTYP            7
     C                   PARM                    ERR               1
     C* ERR=1(NOT FOUND)
     C     ERR           IFEQ      '1'
     C                   SETON                                        9050
     C                   ELSE
     C                   SETOFF                                         50
     C                   ENDIF
     C                   ENDIF                                                  SCSBS *NE ' '
     C*** begin add ***  10/19/21  ************************************
     C                   If        ##lib = ' ' and scsbs <> ' '
     C                   Eval      ##obj = scsbs
     C                   Call      'RTVOBJLIB'
     C                   Parm                    ##Obj            10
     C                   Parm      '*SBSD'       ##Type            7
     C                   Parm                    ##Lib            10
     C                   Endif
     C*** end add *****  10/19/21  ************************************
     C* Jobq/Subsystem entered first time at screen.
     C  N90F2JOBQ        IFEQ      *BLANKS
     C     SCJOBQ        ANDNE     *BLANKS
     C* Call program to check whether JobQ already assigned to any subsystem
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCJOBQ        SRNAME
     C                   PARM      '*JOBQ'       SROTYP
     C                   PARM                    SRERR
     C*If Entered JobQ already exist.
     C     SRERR         IFEQ      '0'
     C* Call program to check whether JobQ already assigned to any subsystem
     C                   MOVE      SCJOBQ        @JOBQ
     C                   CALL      'RTVJOBQA'
     C                   PARM                    @JOBQ            10
     C*** begin chg ***  10/19/21  ************************************
     C******             PARM      'QGPL'        @JQLIB           10
     C                   PARM      ##Lib         @JQLIB           10
     C*** end chg *****  10/19/21  ************************************
     C                   PARM                    @JQSTS           10
     C                   PARM                    @SBNAM           10
     C                   PARM                    @JBCNT           10 0
     C*If JobQ already assigned (returned subsystem different from screen)
     C     @SBNAM        IFNE      SCSBS
     C                   MOVE      SCJOBQ        JOB
     C                   MOVE      @SBNAM        SBNM
     C                   SETON                                        9055
     C                   ENDIF                                                  END @SBNAM CHK
     C                   ENDIF                                                  END SRERR=0
     C                   ENDIF                                                  SCJOBQ/F2JOBQ
     C* If JobQ/Subsystem is changed at screen.
     C  N90F2JOBQ        IFNE      *BLANKS
     C     SCJOBQ        ANDNE     F2JOBQ
     C*
     C     F2SBS         ORNE      *BLANKS
     C     SCSBS         ANDNE     F2SBS
     C* Check whether Old JobQ already exist
     C                   CALL      'SRCHKOBJ'
     C                   PARM      F2JOBQ        SRNAME
     C                   PARM      '*JOBQ'       SROTYP
     C                   PARM                    SRERR
     C*If Old JobQ already exist.
     C     SRERR         IFEQ      '0'
     C                   MOVE      F2JOBQ        @JOBQ
     C* Call program to check whether old JobQ is in use or not
     C                   CALL      'RTVJOBQA'
     C                   PARM                    @JOBQ            10
     C*** begin chg ***  10/19/21  ************************************
     C******             PARM      'QGPL'        @JQLIB           10
     C                   PARM      ##Lib         @JQLIB           10
     C*** end chg *****  10/19/21  ************************************
     C                   PARM                    @JQSTS           10
     C                   PARM                    @SBNAM           10
     C                   PARM                    @JBCNT           10 0
     C*If JobQ/Subsystem changed and Old JobQ is in use (has any Job).
     C     @JBCNT        IFNE      *ZEROS
     C                   MOVE      F2JOBQ        JOB
     C                   MOVE      F2SBS         SBNM
     C*JobQ changed.
     C     SCJOBQ        IFNE      F2JOBQ
     C                   SETON                                        9056
     C                   ELSE
     C*Else Subsystem changed.
     C                   SETON                                        9057
     C                   ENDIF                                                  SCJOBQ<>F2JOBQ
     C                   ENDIF                                                  @JOBCNT
     C                   ENDIF                                                  SRERR = 0
     C* If JobQ changed and Old JobQ not in use
     C     SCJOBQ        IFNE      F2JOBQ
     C* Check whether New JobQ already exist
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCJOBQ        SRNAME
     C                   PARM      '*JOBQ'       SROTYP
     C                   PARM                    SRERR
     C*If Changed JobQ already exist.
     C     SRERR         IFEQ      '0'
     C                   MOVE      SCJOBQ        @JOBQ
     C* Call program whether it is already assigned to any subsystem
     C                   CALL      'RTVJOBQA'
     C                   PARM                    @JOBQ            10
     C*** begin chg ***  10/19/21  ************************************
     C******             PARM      'QGPL'        @JQLIB           10
     C                   PARM      ##Lib         @JQLIB           10
     C*** end chg *****  10/19/21  ************************************
     C                   PARM                    @JQSTS           10
     C                   PARM                    @SBNAM           10
     C                   PARM                    @JBCNT           10 0
     C*If JobQ already assigned (returned subsystem different from screen)
     C     @SBNAM        IFNE      SCSBS
     C                   MOVE      SCJOBQ        JOB
     C                   MOVE      @SBNAM        SBNM
     C                   SETON                                        9055
     C                   ENDIF                                                  END @SBNAM CHK
     C                   ENDIF                                                  END SRERR = 0
     C                   ENDIF                                                  SCJOBQ<>F2JOBQ
     C*
     C                   ENDIF                                                  JQ/SUBSYS CHG
     C* CALL PROGRAM TO CREATE JOBQ
      ***  Begin Add ***** 09/18/23 *****************************
      /free
       // Do not create JobQ at Data Center Clients
       If suffix = '   ';
      /end-free
      ***  End   Add ***** 09/18/23 *****************************
     C                   MOVE      ##CO#         #@CO#             3
     C                   MOVE      ##FND         #@FND             3
     C  N90              CALL      'W0010C02'
     C                   PARM                    #@CO#
     C                   PARM                    #@FND
     C                   PARM                    F2JOBQ
     C                   PARM                    SCJOBQ
     C                   PARM                    F2SBS
     C                   PARM                    SCSBS
     C                   PARM                    #@MSG           132
     C                   PARM                    #@RTN             1
     C*
     C     #@RTN         IFNE      ' '
     C                   MOVEL(P)  #@MSG         SCMS
     C                   SETON                                        9052
     C                   ENDIF                                                  #@RTN <> ' '
      ***  Begin Add ***** 09/18/23 *****************************
      /free
       Endif;
      /end-free
      ***  End   Add ***** 09/18/23 *****************************
     C***** END ADD ******* 10/25/10 **********************************
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90F2DT          IFGT      SAVEDT
     C     F2DT          OREQ      SAVEDT
     C     F2TIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
      *
      ****************************************************************
      *  KJDO - Process F10 "Table Search" requests.                 *
      ****************************************************************
     C     KJDO          BEGSR
      *
     C                   SELECT
      ***** BEG ADD ******* 04/23/10 **********************************
      *
      * CERT OF INS OVERLAY
      *
     C     HFLD          WHENEQ    'SCCEOV'
     C                   CALL      'W2493R'
     C                   PARM                    #1PRDS           40
     C                   PARM      *BLANK        #1CALL            1
     C                   PARM      'COVL'        #1ID              4
     C                   PARM      *BLANKS       #1KEY            10
     C                   PARM      *BLANK        #1RTC             1
      *
     C     #1KEY         IFEQ      *BLANKS
     C                   SETON                                        76
     C                   ELSE
     C                   SETON                                        77
     C                   MOVEL(P)  #1KEY         SCCEOV
     C                   ENDIF                                                  #1KEY
      *
      ***** END ADD ******* 04/23/10 **********************************
     C*** BEG ADD *****  06/25/09  ************************************
     C     HFLD          WHENEQ    'SCTPAC'
     C                   MOVE      *OFF          *IN45
     C                   CALL      'W0009R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CO#
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *ZEROS        @#RRN             9 0
     C*
     C     @#RRN         CHAIN     PTC01                              60
     C  N60              MOVE      TCTPAC        SCTPAC
     C  N60              MOVE      SCTPAC        F2TPAC
     C  N60              SETON                                          42
     C   60              SETON                                          43
     C*
     C*                    EXSR UPDATR
     C*
     C*** END ADD *****  06/25/09  ************************************
      *_______________________________________________________________
     C*****      HFLD      WHEQ 'SCACCT'
      *****
     C*****                MOVE SCFND     QQFND
     C*****                CALL 'W2423R'
     C*****                PARM '2'       ##MODE  1
     C*****      *INKC     PARM *INKC     ##KC    1
     C*****      *INKL     PARM *INKL     ##KL    1
     C*****                PARM           ##CO#
     C*****                PARM           QQFND   3
     C*****                PARM *BLANKS   ##ACCT 20
     C*****                PARM *ZERO     ##EFDT  8
     C*****                PARM 'WC'      ##APPL  2
     C*****                PARM *BLANKS   ##CKTP 10
      *****
     C*****      ##CKTP    IFNE *BLANK
     C*****                MOVE ##CKTP    SCACCT
     C*****                MOVE *ON       *IN72
     C*****                ELSE
     C*****                MOVE *ON       *IN71
     C*****                ENDIF
      *****
      *****___________________________________________________________
     C*****      HFLD      WHEQ 'SCAACC'
      *****
     C*****                MOVE SCFND     QQFND
     C*****                CALL 'W2423R'
     C*****                PARM '2'       ##MODE
     C*****      *INKC     PARM *INKC     ##KC
     C*****      *INKL     PARM *INKL     ##KL
     C*****                PARM           ##CO#
     C*****                PARM           QQFND
     C*****                PARM *BLANKS   ##ACCT
     C*****                PARM *ZERO     ##EFDT
     C*****                PARM 'AG'      ##APPL
     C*****                PARM *BLANKS   ##CKTP
      *****
     C*****      ##CKTP    IFNE *BLANK
     C*****                MOVE ##CKTP    SCAACC
     C*****                MOVE *ON       *IN74
     C*****                ELSE
     C*****                MOVE *ON       *IN73
     C*****                ENDIF
      *
      /Free
       //************* Beg Add ************ 11/22/13 *****************

       When Hfld = 'SCDBCD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             *in83 = *on;
             *in90 = *on;
          Else;
             scDbCd = ##Key;
             *in84 = *on;
             *in90 = *on;
          Endif;

       When Hfld = 'SCPBCD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             *in84 = *on;
             *in90 = *on;
          Else;
             scPbCd = ##Key;
             *in85 = *on;
             *in90 = *on;
          Endif;

       When Hfld = 'SCFBCD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             *in85 = *on;
             *in90 = *on;
          Else;
             scFbCd = ##Key;
             *in90 = *on;
          Endif;

       //************* Beg Add ************ 01/20/15 *****************

       When Hfld = 'SC1ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus1 = *on;
             *in90 = *on;
          Else;
             sc1ACD = ##Key;
             *in90 = *on;
          Endif;

       When Hfld = 'SC2ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus2 = *on;
             *in90 = *on;
          Else;
             sc2ACD = ##Key;
             *in90 = *on;
          Endif;

       When Hfld = 'SC3ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus3 = *on;
             *in90 = *on;
          Else;
             sc3ACD = ##Key;
             *in90 = *on;
          Endif;

       When Hfld = 'SC4ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus4 = *on;
             *in90 = *on;
          Else;
             sc4ACD = ##Key;
             *in90 = *on;
          Endif;

       When Hfld = 'SC5ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus5 = *on;
             *in90 = *on;
          Else;
             sc5ACD = ##Key;
             *in90 = *on;
          Endif;

       When Hfld = 'SC6ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus6 = *on;
             *in90 = *on;
          Else;
             sc6ACD = ##Key;
             *in90 = *on;
          Endif;

       When Hfld = 'SC7ACD';
          ##Call = '2';
          ##Key = *blanks;
          callW2161R(##Call:##Kc#:##Kl#:##Key);
          If ##Key = *blanks;
             setfocus7 = *on;
             *in90 = *on;
          Else;
             //** Begin Add *** 10/02/19 *****************************
             setfocus8 = *on;
             //** End   Add *** 10/02/19 *****************************
             sc7ACD = ##Key;
             *in90 = *on;
          Endif;
       //************* End Add ************ 01/20/15 *****************
       //************* End Add ************ 11/22/13 *****************
      /End-free
      *_______________________________________________________________
     C                   OTHER                                                  FIELD NO SEARCH
     C                   MOVE      *ON           *IN49
      *
     C                   ENDSL
      *
     C                   ENDSR
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0010R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     F2FEIN        SCFEIN
     C                   MOVEL     F2TCC         SCTCC
     C                   MOVEL     F2IAUD        SCIAUD
     C                   MOVEL     F2SCTC        SCSCTC
     C                   MOVEL     F2AREA        SCAREA
     C                   MOVEL     F2SPH#        SCSPH1
     C                   MOVE      F2SPH#        SCSPH2
     C                   MOVEL     F2CAR#        SCCAR#
      ************** Beg Del ************** 11/22/13 ******************
     C**************     MOVEL     F2WCPL        SCWCPL
     C**************     MOVEL     F2CLCK        SCCLCK
     C*****                MOVELF2ACCT    SCACCT
     C**************     MOVEL     F2AGCK        SCAGCK
      ************** End Del ************** 11/22/13 ******************
      /Free
       //*********** Beg Add ************** 11/22/13 ******************
       If F2wcpl = ' ';
          scWcPl = 'N';
       Else;
          scWcPl = F2wcpl;
       Endif;
       If F2clck = ' ';
          scClck = 'N';
       Else;
          scClck = F2clck;
       Endif;
       If F2agck = ' ';
          scAgCk = 'N';
       Else;
          scAgCk = F2agck;
       Endif;
       //*********** End Add ************** 11/22/13 ******************
      /End-free
     C*****                MOVELF2AACC    SCAACC
     C                   MOVEL     F2MACC        SCMACC
     C**** BEGIN DLT **** 08/17/10 ************************************
     C**************       MOVELF2UNM1    SCUNM1
     C**************       MOVELF2UNM2    SCUNM2
     C**************       MOVELF2UNM3    SCUNM3
     C**************       MOVELF2UNM4    SCUNM4
     C**************       MOVELF2UNM5    SCUNM5
     C**************       MOVELF2UNM6    SCUNM6
     C**************       MOVELF2UNM7    SCUNM7
     C**** END   DLT **** 08/17/10 ************************************
     C                   MOVEL     F2PP3C        SCPP3C
     C**** BEGIN ADD **** 01/13/99 ************************************
     C                   MOVEL     F2PPC3        SCPPC3
     C**** END   ADD **** 01/13/99 ************************************
     C**** BEGIN ADD **** 11/13/03 ************************************
     C                   MOVEL     F2BLOG        SCBLOG
     C**** END   ADD **** 11/13/03 ************************************
     C                   MOVEL     F2FOLD        SCFOLD
     C                   MOVEL     F2DCID        SCDCID
     C*** BEGIN ADD ***  06/30/05  ************************************
     C                   MOVE      F2DOCL        SCDOCL
     C                   MOVE      F2DOCS        SCDOCS
     C*** END ADD *****  06/30/05  ************************************
     C                   MOVEL     F2CTYP        SCCTYP
     C*** BEGIN ADD *** 02/18/00 **************************************
     C                   MOVE      F2TPA         SCTPA
     C*** END ADD ** ** 02/18/00 **************************************
     C*
      ***** BEG ADD ***** 02/13/03 ***************************************
     C                   MOVEL     F2LAD1        SCLAD1
     C                   MOVEL     F2LAD2        SCLAD2
     C                   MOVEL     F2LCTY        SCLCTY
     C                   MOVEL     F2LST         SCLST
     C                   MOVEL     F2LZP1        SCLZP1
     C                   MOVEL     F2LZP2        SCLZP2
      ***** END ADD ***** 02/13/03 ***************************************
      ************** Beg Add ********** 11/22/13 *************************
      /Free
       scThRl = F2thrl;
       scThRh = F2thrh;
       scDbCd = F2dbcd;
       scPbCd = F2pbcd;
       scFbCd = F2fbcd;
      /End-free
      ************** End Add ********** 11/22/13 *************************
      ************** Beg Add ********** 01/20/15 *************************
      /Free
       sc1ACD = F21acd;
       sc2ACD = F22acd;
       sc3ACD = F23acd;
       sc4ACD = F24acd;
       sc5ACD = F25acd;
       sc6ACD = F26acd;
       sc7ACD = F27acd;
       //** Begin Add *** 10/02/19 *****************************
       sc1099Fein = F21099Fein;
       //** Begin Add *** 10/02/19 *****************************
       //*** begin add *** 09/15/16 ************************
       scSLogo = F2SLogo;
       //*** end add ***** 09/15/16 ************************
       //*** begin add *** 12/06/16 ************************
       scSLogoPth = F2SPath;
       //*** end add ***** 12/06/16 ************************
      /End-free
      ************** End Add ********** 01/20/15 *************************
     C*** BEGIN ADD *** 09/31/05 **************************************
     C                   MOVEL     F2CEOV        SCCEOV
     C*** END   ADD *** 09/31/05 **************************************
      ***** BEG ADD ******* 04/23/10 **********************************
     C                   MOVEL(P)  F2SPSG        SCSPSG
      ***** END ADD ******* 04/23/10 **********************************
      *** Begin add *** 12/28/05 **************************************
     C                   MOVEL     F2NAIC        SCNAIC
      *** End   add *** 12/28/05 **************************************
     C***  Begin Add  ***  05/20/09  **************************************
     C                   Z-ADD     F2RRE#        SCRRE#
     C***  End   Add  ***  05/20/09  **************************************
      *** Beg   add *** 06/25/09 **************************************
     C                   MOVEL     F2TPAC        SCTPAC
      *** End   add *** 06/25/09 **************************************
      *** BEG ADD ***** 12/02/09 **************************************
     C                   Z-ADD     F2FYSM        SCFYSM
      *** BEG ADD ***** 12/02/09 **************************************
     C*** BEG ADD ***** 10/25/10 **************************************
     C     WCFL51        IFEQ      'Y'
     C                   MOVEL     F2JOBQ        SCJOBQ
     C                   MOVEL     F2SBS         SCSBS
     C*
     C     *LOCK         IN        XXSBS
     C                   MOVEL     XXSBS         SCSBSD
     C                   UNLOCK    XXSBS
     C                   ENDIF
     C*** END ADD ***** 10/25/10 **************************************
     C*** begin add ***  10/19/21  ************************************
     C                   Eval      ##obj = xxsbs
     C                   Eval      ##lib = ' '
     C                   If        xxsbs <> ' '
     C                   Call      'RTVOBJLIB'
     C                   Parm                    ##Obj            10
     C                   Parm      '*SBSD'       ##Type            7
     C                   Parm                    ##Lib            10
     C                   Endif
     C*** end add *****  10/19/21  ************************************

      *** beg add ***** 07/25/16 **************************************
      /free
       scDisSms = F2DisSms;
      /end-free
      *** end add ***** 07/25/16 **************************************
      *** beg add *** 10/02/17 **************************************
      /free
       scFreFTxt = F2FREF;
      /end-free
      *** end add *** 10/02/17 **************************************
      *** beg add *** 12/26/17 **************************************
      /free
       scBSI# = F2BSI#;
      /end-free
      *** end add *** 12/26/17 **************************************
      *** beg add *** 08/19/19 **************************************
      /free
       scNCarGrp = F2nCarGrp;
       //*** Begin Add *** 12/23/19 *********************************
       scDXtr = F2DxTr;
       //*** End   Add *** 12/23/19 *********************************
      /end-free
      *** end add *** 08/19/19 **************************************
       //*** Beg Add *** 01/16/24 *** 153WAD **********************
       scRNAC  = F2RNote;
       scRcxRsn = F2RcxRsn;
       //*** End Add *** 01/16/24 *** 153WAD **********************

     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     FNCO#         F2CO#
     C                   Z-ADD     SCFND         F2FND
     C                   Z-ADD     SCFEIN        F2FEIN
     C                   MOVEL     SCTCC         F2TCC
     C                   MOVEL     SCIAUD        F2IAUD
     C                   MOVEL     SCSCTC        F2SCTC
     C                   MOVEL     SCAREA        F2AREA
     C                   MOVEL     SCSPH1        F2SPH#
     C                   MOVE      SCSPH2        F2SPH#
     C                   MOVEL     SCCAR#        F2CAR#
     C*
     C   65              MOVEL     SCWCPL        F2WCPL
     C  N65              MOVEL     'N'           F2WCPL
     C*
     C                   MOVEL     SCCLCK        F2CLCK
     C*****                MOVELSCACCT    F2ACCT    P
     C                   MOVEL     SCAGCK        F2AGCK
     C*****                MOVELSCAACC    F2AACC    P
     C                   MOVEL     SCMACC        F2MACC
     C**** BEGIN DLT **** 08/17/10 ************************************
     C**************       MOVELSCUNM1    F2UNM1
     C**************       MOVELSCUNM2    F2UNM2
     C**************       MOVELSCUNM3    F2UNM3
     C**************       MOVELSCUNM4    F2UNM4
     C**************       MOVELSCUNM5    F2UNM5
     C**************       MOVELSCUNM6    F2UNM6
     C**************       MOVELSCUNM7    F2UNM7
     C**** END   DLT **** 08/17/10 ************************************
     C                   MOVEL     SCPP3C        F2PP3C
     C**** BEGIN ADD **** 01/13/99 ************************************
     C                   MOVEL     SCPPC3        F2PPC3
     C**** END   ADD **** 01/13/99 ************************************
     C                   MOVEL     SCFOLD        F2FOLD
     C**** BEGIN ADD **** 11/13/03 ************************************
     C                   MOVEL     SCBLOG        F2BLOG
     C**** END   ADD **** 11/13/03 ************************************
     C                   MOVEL     SCDCID        F2DCID
     C                   MOVEL     SCCTYP        F2CTYP
     C*** BEGIN ADD ***  06/30/05  ************************************
     C                   MOVE      SCDOCL        F2DOCL
     C                   MOVE      SCDOCS        F2DOCS
     C*** END ADD *****  06/30/05  ************************************
     C*** BEGIN ADD *** 02/18/00 **************************************
     C                   MOVEL     SCTPA         F2TPA
     C*** END ADD ** ** 02/18/00 **************************************
     C*
      ***** BEG ADD ***** 02/13/03 ***************************************
     C                   MOVEL     SCLAD1        F2LAD1
     C                   MOVEL     SCLAD2        F2LAD2
     C                   MOVEL     SCLCTY        F2LCTY
     C                   MOVEL     SCLST         F2LST
     C                   MOVEL     SCLZP1        F2LZP1
     C                   MOVEL     SCLZP2        F2LZP2
      ***** END ADD ***** 02/13/03 ***************************************
      ************** Beg Add ********** 11/22/13 *************************
      /Free
       F2thrl = scThRl;
       F2thrh = scThRh;
       F2dbcd = scDbCd;
       F2pbcd = scPbCd;
       F2fbcd = scFbCd;
      /End-free
      ************** End Add ********** 11/22/13 *************************
      ************** Beg Add ********** 01/20/15 *************************
      /Free
       F21acd = sc1ACD;
       F22acd = sc2ACD;
       F23acd = sc3ACD;
       F24acd = sc4ACD;
       F25acd = sc5ACD;
       F26acd = sc6ACD;
       F27acd = sc7ACD;
       //** Begin Add *** 10/02/19 *****************************
       F21099Fein = sc1099Fein;
       //** Begin Add *** 10/02/19 *****************************
      /End-free
      ************** End Add ********** 01/20/15 *************************
     C*** BEGIN ADD *** 09/31/05 **************************************
      ***** beg del ******* 04/23/10 **********************************
     C*********            MOVELSCCEOV    F2CEOV
      ***** end del ******* 04/23/10 **********************************
     C*** END   ADD *** 09/31/05 **************************************
      ***** BEG ADD ******* 04/23/10 **********************************
     C                   MOVEL(P)  SCCEOV        F2CEOV
     C                   MOVEL(P)  SCSPSG        F2SPSG
      ***** END ADD ******* 04/23/10 **********************************
      *** Begin add *** 12/28/05 **************************************
     C                   MOVEL     SCNAIC        F2NAIC
      *** End   add *** 12/28/05 **************************************
     C***  Begin Add  ***  05/20/09  **************************************
     C                   Z-ADD     SCRRE#        F2RRE#
     C***  End   Add  ***  05/20/09  **************************************
      *** Begin add *** 06/25/09 **************************************
     C                   MOVEL     SCTPAC        F2TPAC
      *** End   add *** 06/25/09 **************************************
      *** BEG ADD ***** 12/02/09 **************************************
     C                   Z-ADD     SCFYSM        F2FYSM
      *** BEG ADD ***** 12/02/09 **************************************
      *
     C*** BEG ADD ***** 10/25/10 **************************************
     C     WCFL51        IFEQ      'Y'
     C                   MOVEL     SCJOBQ        F2JOBQ
     C                   MOVEL     SCSBS         F2SBS
     C                   ENDIF
     C*** END ADD ***** 10/25/10 **************************************
      *** beg add ***** 07/25/16 ***************************
      /free
       F2DisSms = scDisSms;
       //*** begin add *** 09/15/16 ************************
       F2SLogo = scSLogo;
       //*** end add ***** 09/15/16 ************************
       //*** begin add *** 12/06/16 ************************
       F2SPath = scSLogoPth;
       //*** end add ***** 12/06/16 ************************
      /end-free
      *** end add ***** 07/25/16 ***************************
      *** beg add *** 10/02/17 **************************************
      /free
       F2FREF = scFreFTxt;
      /end-free
      *** end add *** 10/02/17 **************************************
      *** beg add *** 12/26/17 **************************************
      /free
       F2BSI# = scBSI#;
      /end-free
      *** beg add *** 08/19/19 **************************************
      /free
       F2nCarGrp = scNCarGrp;
       //*** Begin Add *** 12/23/19 *********************************
       F2DxTr =  scDXtr;
       //*** End   Add *** 12/23/19 *********************************
       //*** Beg Add *** 01/16/24 *** 153WAD **********************
       F2RNote  = scRNAC;
       F2RcxRsn = scRcxRsn;
       //*** End Add *** 01/16/24 *** 153WAD **********************
      /end-free
      *** end add *** 08/19/19 **************************************
      *** end add *** 12/26/17 **************************************
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR

     *** beg add ***** 07/25/16 ***********************************************
      /free
       isRelayMod = isModEnabled('RelaySMS');
      /end-free
     *** end add ***** 07/25/16 ***********************************************

      *** Beg Add ***** 09/18/23 *****************************
      /free
       // Retrieve Client Suffix
       RtvCltSuffix(suffix);
      /end-free
      *** End Add ***** 09/18/23 *****************************

     C*** BEG ADD ***** 07/27/09 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C                   MOVE      CHD(2)        SCCHDR
     C                   MOVEL     CHD(3)        SCCHDD
     *** beg add *** 10/02/17 *******************************************  ****
      /free
       isClt114 = *on;
      /end-free
     *** end add *** 10/02/17 *******************************************  ****
     C                   OTHER
     C                   MOVE      CHD(1)        SCCHDR
     C                   ENDSL
     C*** END ADD ***** 07/27/09 **************************************
       //*** Beg Add *** 01/16/24 *** 153WAD **********************
       If CltID = '153';
          VisClt153 = *On;
       EndIf;
       //*** End Add *** 01/16/24 *** 153WAD **********************
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVE      *ON           DSPTYP
     C*
     C     ##RRN         CHAIN     PFN01                              60
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
      ***** BEG ADD ***** 03/05/05 **************************************
     C                   CLEAR                   PF201
      ***** END ADD ***** 03/05/05 **************************************
     C*
     C     F2KY          CHAIN     PF201                              60
     C  N60              Z-ADD     DBRRN2        @@RRN             9 0
      ***** BEG CHG ***** 03/05/05 **************************************
     C***60***********     MOVEL'A'       @@MODE
     C   60
     CANN09              MOVEL     'A'           @@MODE
     C   60              Z-ADD     *ZEROS        F2LZIP
      ***** END CHG ***** 03/05/05 **************************************
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL03        COMP      'Y'                                    65    WCPOLS
     C*
     C*** BEGIN ADD *** 03/23/98 **************************************
     C*
     C*  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
     C*
     C*  Access System Control file 'WC' record
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     WCFL08        IFNE      *BLANK
     C*
     C     WCFL08        IFEQ      'A'
     C                   MOVE      *ON           *IN94                          ADMIN ONLY
     C                   ENDIF                                                  WCFL08=A
     C*
     C     WCFL08        IFEQ      'C'
     C                   MOVE      *ON           *IN95                          CLAIMS ONLY
     C                   ENDIF                                                  WCFL08=C
     C*
     C                   ENDIF                                                  WCFL08<>*BLANK
     C***** BEG ADD ******* 10/25/10 **********************************
     C*SYSTEM CONTROL FLAG
     C     WCFL51        IFEQ      'Y'
     C                   MOVE      *ON           *IN51                          JOBQ/SUBSYSTEM
     C                   ENDIF                                                  WCFL51=Y
     C***** END ADD ******* 10/25/10 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** END   ADD *** 03/23/98 **************************************
     C*** BEG ADD ***** 08/26/11 **************************************
      *
      * HIDE 3C FIELDS IF CLAIMS ONLY OR IF CLIENT IS
      * INACTIVE OR INELIGIBLE FOR WC1918
      *
     C                   CALL      'SRCHKOBJ'
     C                   PARM      'W0010R65'    OBJNAM           10
     C                   PARM      '*PGM'        OBJTYP            7
     C                   PARM                    ERR               1
      *
     C     ERR           IFEQ      *ON
     C                   MOVE      *OFF          WC1918            1
     C                   ELSE
     C                   MOVE      *ON           WC1918
     C                   ENDIF
      *
     C                   SELECT
     C     *IN95         WHENEQ    *ON
     C     CLTID         OREQ      '048'
     C     CLTID         OREQ      '065'
     C     CLTID         OREQ      '081'
     C     CLTID         OREQ      '091'
     C     CLTID         OREQ      '093'
     C     CLTID         OREQ      '129'
     C     WC1918        OREQ      *OFF
     C                   OTHER
     C                   MOVE      X'A7'         AT01F
     C                   MOVE      X'A7'         AT02F
     C                   MOVE      X'A7'         AT03F
     C                   ENDSL
      *
     C*** END ADD ***** 08/26/11 **************************************
       //*** begin add *** 09/15/16 ************************
      /free
       Clear isScript;

       isScript = isModEnabled('Scriptura-Documents')
        or isModEnabled('Scriptura-Various')
        or isModEnabled('3rd Party XML');

       tipSLogo = 'The output logo file name must have a valid image ' +
        'file name and extension.';
       //*** begin add *** 12/06/16 ************************
       tipSLogoPt = 'The output logo path must contain a valid file path ' +
        'and begin/end with a ''/'' character.';
       //*** end add ***** 12/06/16 ************************

      /end-free
       //*** end add ***** 09/15/16 ************************
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F2USER
     C                   MOVEL     Q1PGM         F2PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2DT
     C                   MOVEL     DSTIME        F2TIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F2IUSR
     C                   MOVEL     Q1USER        F2USER
     C                   MOVEL     Q1PGM         F2IPGM
     C                   MOVEL     Q1PGM         F2PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2IDT
     C                   MOVEL     SRDTO         F2DT
     C                   MOVEL     DSTIME        F2ITIM
     C                   MOVEL     DSTIME        F2TIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDF2P
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
      /Free
       //********** Beg Add ********** 01/20/15 **********************
       //=============================================================
       // AssCode  - Validate the Assessment Code
       //=============================================================
         BegSr AssCode;
          isError = *off;

          If sc1ACd <> *blanks;
             kkKey = sc1ACd;
             ##Ctyp = '1';
             Exsr Validate;
             If isError = *on;
                erasscode1 = *on;
             Endif;
          Endif;

          If sc2ACd <> *blanks;
             kkKey = sc2ACd;
             ##Ctyp = '2';
             Exsr Validate;
             If isError = *on;
                erasscode2 = *on;
             Endif;
          Endif;

          If sc3ACd <> *blanks;
             kkKey = sc3ACd;
             ##Ctyp = '3';
             Exsr Validate;
             If isError = *on;
                 erasscode3 = *on;
             Endif;
          Endif;

          If sc4ACd <> *blanks;
             kkKey = sc4ACd;
             ##Ctyp = '4';
             Exsr Validate;
             If isError = *on;
                erasscode4 = *on;
             Endif;
          Endif;

          If sc5ACd <> *blanks;
             kkKey = sc5ACd;
             ##Ctyp = '5';
             Exsr Validate;
             If isError = *on;
                erasscode5 = *on;
             Endif;
          Endif;

          If sc6ACd <> *blanks;
             kkKey = sc6ACd;
             ##Ctyp = '6';
             Exsr Validate;
             If isError = *on;
                erasscode6 = *on;
             Endif;
          Endif;

          If sc7ACd <> *blanks;
             kkKey = sc7ACd;
             ##Ctyp = '7';
             Exsr Validate;
             If isError = *on;
                erasscode7 = *on;
             Endif;
          Endif;
         endsr;
       //=============================================================
       // Validate - Validate the Assessment Code
       //=============================================================
          Begsr Validate;
             Chain KeyWTABL PTT01;
             If not %found or TTstat <> 'A' or TTtype <> 'Q'
              or TTctyp <> ##ctyp;
                isError = *on;
                *in90 = *on;
             Endif;
          Endsr;
       //=============================================================
       //********** End Add ********** 01/20/15 **********************
      /End-Free
**
Type of payment . . . . . . . .(C/V):
Type of payment . . . . . . .(C/V/X):
(C=Check, V=Voucher, X=XML file)
