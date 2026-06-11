     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8070R4 - A49 LOG API
     H*****************************************************************
     H* DESCRIPTION:  THIS PGM WILL CREATE A LOG RECORD BASED ON THE  *
     H*               ENTRY PARMS.  CAN BE USED TO CREATE BOTH 148    *
     H*               AND A49 LOG RECORDS.  THIS PGM DOES NOT CREATE  *
     H*               ANY ASSOCIATED DIARIES.                         *
     H*                                                               *
     H* CALLED FROM:  W0080R, W0080R1                                 *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   15        6/12/97        WC468                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H* 02/14/22 C:079  Project: WC2797                               *
     H*                 Add IAIABC 3.1.4 field(s) LGbCrC              *
     H*                                                               *
     H* 07/14/21 C:079 Project: WC2816                                *
     H*                Add USL&H logic                                *
     H*                                                               *
     H* 10/27/20 C:053 SUPPORT 91729                                  *
     H*                Invalid MTC date for QT transaction.           *
     H*                                                               *
     h* 07/02/20 C:180 Project WC2738                                 *
     h*                Add condition for denials based on             *
     h*                EDI control file settings.                     *
     H*                                                               *
     H* 03/14/19 C:212 Project WC2658                                 *
     H*                Add processing related to addition of new eff. *
     H*                dates for override                             *
     H*                                                               *
     H* 07/08/15 C:180 SUPPORT 35571 (RMLA)                           *
     H*                Check for valid transaction set.               *
     H*                                                               *
     H* 07/03/14 C:180 SUPPPORT WC146 86012, 85708                    *
     H*                Suppress A49, denials for NC                   *
     H*                                                               *
     H* 03/21/14 C:279 Support: WC025 85122                           *
     H*                Modified to update the MTC override date       *
     H*                correctly.                                     *
     H*                                                               *
     H* 11/28/13 C:279 Support: WC025 84188                           *
     H*                Modified to capture the MTC information for    *
     H*                corrected filing.                              *
     H*                                                               *
     H* 09/11/13  C:190  Project: WC2227                              *
     H*                  Add "MTC Date Override"                      *
     H*                                                               *
     H* 12/29/11  C:062  SUPPORT WC114 78755                          *
     H*                  Default the Jurisdiction State if blank.     *
     H*                                                               *
     H* 02/09/11  C:180  SUPPORT 75809                                *
     H*                  Fix invalid data written to DARTS.           *
     H*                                                               *
     H* 06/21/10 C:220 Project: 139WLM                                *
     H*                 -Correct the code under tag '09/14/09'        *
     H*                  for proper GTGTP call.                       *
     H*                                                               *
     H* 05/10/10 C:247 SUPPORT 72364                                  *
     H*                EDI - screen : Option 18 is not working        *
     H*                                                               *
     H* 04/05/10 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                  PROJECT RE-JAD , RE-DESIGNED                 *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 09/14/09 C:234 SUPPORT 69429,69319,68934,68822                *
     H*                Disallow entry of invalid MTC codes. Comment   *
     H*                referencing of 'SMTC' or 'FMTC' in the E1/E2   *
     H*                processing.                                    *
     H*                                                               *
     H* 05/26/09 C:180 SUPPORT 68608
     H*                Fix error when creating a PY for a claim that
     H*                was created prior to the EDI eff date.
     H*
     H* 05/15/09 C:180 SUPPORT 68384
     H*                DO NOT CREATE AN A49-04 WHEN A 148-04 already
     H*                exists.
     H*
     H* 04/16/09 C:180 SUPPORT 68069
     H*                When a recursive call occurs, exit to prevent
     H*                the creation of an A49 without the required
     H*                148.
     H*
     H* 04/02/09 C:180 SUPPORT 66757/66744
     H*                Do not create a A49 when accident date is
     H*                prior to the EDI START DATE in the EDI CTL file.
     H*
     H* 02/15/08 C:191 Project: WC1584 PRISS# 38                      *
     H*                EDI Release 3: Create A49/04 for denied FL     *
     H*                               claims                          *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                EDI Release 3: Load LGMTCD with LLISCM         *
     H*                                                               *
     H* 03/15/05 C:15  Project: 114WJE                                *
     H*                Added code to check the EDI flag at the group  *
     H*                level to determine if EDI should be processed. *
     H*                                                               *
     H* 02/17/03 C:53  Support: WC082/35923                           *
     H*                Initialize LGBLFL so data from a previous      *
     H*                chain.                                         *
     H*                                                               *
     H* 02/12/03 C:15  Project: WC468                                 *
     H*                Load the system date into LGMTCD if LGMTCD = 0.*
     H*                                                               *
     H* 03/22/02 C:53  Support: WC025/30451-31966                     *
     H*                Per Rick Honeycutt, load the pmt transaction   *
     H*                date into the MTC date.                        *
     H*                                                               *
     H* 09/22/00 C:15  Project: WC468                                 *
     H*                Added code to load a unique seq# into LGDSQH.  *
     H*                                                               *
     H*  04/26/00 C:61  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     FWMLGL4    IF A E           K DISK
     FWMCML1    IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FWMFNL     IF   E           K DISK
     FWMLLL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d numDate7        s              7  0
     d* EDI Control
     d GetEdiCtrlInfo  pr                  ExtPgm('WA076R')
     d   XXCO#                        3  0
     d   XXJST                        2
     d   XXTSET                       3
     d   CMFND                        3  0
     d   @RTNCD                       1
     d   @EDIF                        3
     d   IXELT                        1
     d   IXELTD                       3  0
     d   IXFROI                       2
     d   IXSDUE                       3  0
     d   IXRLS                        1
     d   IXCSDT                       7  0
     d   IXEADC                       5
     d   IXHCPR                       5
     d   IXFNRC                       5
     d   IXHCED                       1
     d   IXCRER                       5
     d   IXCEIR                       5
     d   IXCECR                       5
     d   IXCTAR                       5
     d   IXCTRR                       5
     d   IXmo                         1
     d   IXmltd                       3  0
     d   IXmsdt                       7  0
     d   IXrsdt                       7  0
     d   IXedny                       1
     d   IXdnydlw                     1
     d*** Beg Add *** 07/14/21 *************************************************
     d   IXusLh                       1
     d*** End Add *** 07/14/21 *************************************************
     d*=========================================================================
     d* Standalone top
     d*=========================================================================
     d indClmDtChk     s               n   inz(*off)
     d*** Beg Add *** 07/14/21 *************************************************
     d   svUsLh        s                   Like(IXusLh)
     d*** End Add *** 07/14/21 *************************************************
     d*=========================================================================
     d* Standalone bottom
     d*=========================================================================

     C**************************************************************************
     C*  First, determine if an active log record already exists with
     C*    the same MTC code.  If so, do not create another one.
     C*
     C                   EXSR      SRDUP
     C*
     C*  If an A49 is being requested, verify processing date.         sts.
     C                   EXSR      PRCDAT
      /Free
         Exsr EdiCtrl;
      /End-Free
     C                   IF        @RTNCD <> 'Y'
     C                   GOTO      TERM
     C                   ENDIF
      *
     C****  Create the Log reocrd.
     C****  Determine if a DARTS remark should be created.  If so,
     C****    create one.
      /Free

          Select;

          When XXJST = 'NC' and XXTSET = 'A49';
               QQRTCD = '4';

          When XXJST = 'NC' and XXMTC = '04';
               QQRTCD = '4';

          When XXMTC = '04' and
               GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw) = *Off;
               QQRTCD = '4';
          Other;
               // If an A49 is being requested, verify that a 148 log record exist
               If XXTSET = 'A49';
                  EXSR SR148;
               Endif;

               // Create log record
               EXSR SRLOG;

               // Determine if a DARTS remark should be created.  If so, create one.
               EXSR SRDRT1;
               If XXDART = 'Y';
                  EXSR SRDRT2;
               Endif;
          Endsl;


      /End-Free
     C*
     C*  End the program.
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  *INZSR - Initialization Routine.
     C*****************************************************************
     C     *INZSR        BEGSR
     C*
     C*  Entry Parameter List
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQTSET            3
     C                   PARM                    QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQCPGM           10
     c                   PARM                    QQOVRDATE         7
     c                   Parm                    qqMtCc            2
     c                   Parm                    qqMtC2            8
     c*** Beg *** 02/14/22 ****************************************************
     c                   Parm                    qqBcRc            1
     c*** End *** 02/14/22 ****************************************************
     C*
     C*  Key list for the Log Record file
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
     C                   KFLD                    XXMTC
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
     C*
     C*  Key list for the Claim Master
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  Key list for the EDI Claim Control file
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C*
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  WMFNL - Group Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     LLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C     GIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJST
     C                   PARM                    XXTSET
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C                   PARM      *BLANKS       IXCRER            5
     C                   PARM      *BLANKS       IXCEIR            5
     C                   PARM      *BLANKS       IXCECR            5
     C                   PARM      *BLANKS       IXCTAR            5
     C                   PARM      *BLANKS       IXCTRR            5
     C                   PARM      *BLANKS       IXmo              1
     C                   PARM      *ZEROS        IXmltd            3 0
     C                   PARM      *ZEROS        IXmsdt            7 0
     C                   PARM      *ZEROS        IXrsdt            7 0
     C                   PARM      *BLANKS       IXedny            1
     C                   PARM      *BLANKS       IXdnydlw          1
     C*** Beg Add *** 07/14/21 ************************************************
     C                   Parm      *Blanks       IXusLh            1
     C*** End Add *** 07/14/21 ************************************************
      *
     C*
     C*   Define Work Fields
     C*
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C     *LIKE         DEFINE    LGMTC         XXMTC
     C     *LIKE         DEFINE    LGJST         XXJST
     C     *LIKE         DEFINE    CMUSER        XXUSER
     C     *LIKE         DEFINE    LGDUE         XXDATE
     C     *LIKE         DEFINE    IXMSDT        SVMSDT
     C     *LIKE         DEFINE    IXRSDT        SVRSDT
     C                   MOVE      *BLANKS       XXFROI            1
     C                   MOVE      *BLANKS       XXDART            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQMTC         XXMTC
     C                   MOVE      QQJST         XXJST
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      /free
       If %parms > 9;
          numDate7 = %int(qqOvrDate);
       Endif;
      /end-free
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     FNEDI         CABEQ     'Y'           TERM
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C     *LIKE         DEFINE    IXCSDT        SVCSDT
     C                   MOVE      *ZEROS        SVCSDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C                   ENDSR
     C**************************************************************************
     C*  SRDUP - This subroutine checks for an active duplicate
     C*    Log record.  If a dup exists, the pgm will end.
     C**************************************************************************
       //*** Beg Add *** 07/14/21 *********************************************
       //=======================================================================
       // Subroutine ProgramEnd
       //=======================================================================
       Begsr ProgramEnd;

       *inlr = *on;
       Return;

       Endsr;
       //*** End Add *** 07/14/21 *********************************************
     C     SRDUP         BEGSR
     C*
     C                   MOVE      QQTSET        XXTSET
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     LGSTS         IFEQ      'EI'                                         ACTIVE LOG RECORD
     C     LGSTS         OREQ      'EC'
     C                   MOVE      '2'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C*  SR148 - This subroutine checks for a 148 submission.  If none
     C*    exists, a 148 will be submitted if all criteria is meet.
     C**************************************************************************
     C     SR148         BEGSR
     C*
     C*  Check for a 148 log record.
     C*
     C                   MOVE      'N'           XXFROI
     C                   MOVE      '148'         XXTSET
     C     LGKY1         CHAIN     PLG01                              60
     C  N60              MOVE      'Y'           XXFROI
     C*  If the claim entry date is less than the EDI start date, seton
     C*    XXFROI to indicate that the FROI has been sent.
     C*
     C     *IN60         IFEQ      *ON
     C     XXCLT#        ANDNE     '114'

     C/free
         // initialize a common indicator to compare claim date with start date
         indClmDtChk = *off;

         Select;

          // check for medical date override
          when CMltfl = 'M' and svmsdt > *zeros;
             if CMidt < svmsdt;
                indClmDtChk = *on;
             endif;

          // check for record only date override
          when CMltfl = 'R' and svrsdt > *zeros;
             if CMidt < svrsdt;
                indClmDtChk = *on;
             endif;

          // check for other scenario
          other;
             if CMidt < svcsdt;
                indClmDtChk = *on;
             endif;

         EndSl;

       //*** Beg Add *** 07/14/21 *********************************************
       If svUsLh = 'Y' and CMusLh = 'Y';
          Exsr ProgramEnd;
       Endif;
       //*** End Add *** 07/14/21 *********************************************

      /end-free
     C                   If        indClmDtChk = *on
     C                   MOVE      'Y'           XXFROI
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *ON
     C     XXCLT#        ANDEQ     '114'
     C     CMACDT        ANDLT     SVCSDT
     C                   MOVE      'Y'           XXFROI
     C                   ENDIF
      *
     C*  If creating a 148 rec will cause a recursive call condition,
     c*  exit pgm.
      *
     C     XXFROI        IFEQ      'N'
     C     Q1PRMS        ANDEQ     9
     C     QQCPGM        ANDEQ     'W0070R6'
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  If no 148 log record exists, call the 148 log API.  This will
     C*    create a 148 log record if certain criteria are meet.
     C*
     C     XXFROI        IFEQ      'N'
     C                   MOVE      'B'           QQACTF
     C                   MOVE      *BLANKS       QQMGID
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W0070R6'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACTF            1            ACTION FLAG
     C                   PARM                    QQMGID            7            MSG ID
     C                   PARM                    QQRTCD            1
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     QQTSET        ANDEQ     'A49'
     C     QQMTC         ANDEQ     '04'
     C     LGKY1         READE     PLG01                                  60
     C     *IN60         DOWEQ     *OFF
     C     LGMTC         IFEQ      '04'
     C                   GOTO      TERM
     C                   ENDIF
     C     LGKY1         READE     PLG01                                  60
     C                   ENDDO
     C                   ENDIF
     C                   ENDSR
     C*****************************************************************
     C*  SRLOG - This subroutine creates the A49 log record.
     C*****************************************************************
     C     SRLOG         BEGSR
     C*
     C*  Get the Jurisdiction state from the Claim Master
     C*
     C     XXJST         IFEQ      *BLANKS
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVEL     CMJRST        XXJST
     C                   ENDIF
     C*
     C*
     C***        Select if 1st comp pmt on claim.
     C*
     C                   MOVE      *OFF          XXSEL             1
     C     PAKY          CHAIN     PPA01                              12
     C     *IN12         DOWEQ     *OFF
     C*
     C*  Ignore pmts made by other TPAs.
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              99
     C   99              MOVE      *BLANKS       B2PYCD
     C*
     C     PARETP        IFEQ      'C'
     C     B2PYCD        ANDNE     *BLANKS
     C     XXMTC         ANDNE     'QT'
     C                   MOVE      *ON           XXSEL
     C                   LEAVE
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  12
     C                   ENDDO
     C*
     C*
     C*  Load the Maint Type Code
     C*
     C                   CLEAR                   PLG01
     C                   MOVE      XXCO#         LGCO#
     C                   MOVE      XXFND         LGFND
     C                   MOVE      XXFYR         LGFYR
     C                   MOVE      XXCASE        LGCASE
     C                   MOVEL     XXJST         LGJST
     C                   MOVEL     QQTSET        LGTSET
     C                   MOVEL     QQMTC         LGMTC
     C                   Z-ADD     XXDATE        LGDUE
     C                   MOVE      'EC'          LGSTS
     C                   MOVE      *BLANK        LGSTAT
     C                   MOVE      *ZEROS        LGEXDT
     C     XXSEL         IFEQ      *ON
     C                   MOVE      PATRDT        LGMTCD
     C* If LGJST is FL, LGMTC is PY:
     C*  Retrieve LLISCM from WMLLP, if non-zero
     C*   load to LGMTCD
     C     LGJST         IFEQ      'FL'
     C     LGMTC         ANDEQ     'PY'
     C     LLKY          CHAIN     PLL01                              99
     C     *IN99         IFEQ      *OFF
     C     LLISCM        ANDNE     0
     C                   MOVE      LLISCM        LGMTCD
     C                   ENDIF                                                  *IN99=*OFF
     C                   ENDIF                                                  LGJST='FL'
     C                   ELSE
      /free
      /End-free
     C                   Z-ADD     XXDATE        LGMTCD
     C                   ENDIF

      /free
       If numDate7 > 0 ;
          LGmtcd = numDate7;
       Endif;
      /End-free
     C                   MOVE      *ZEROS        LGSENT
     C                   MOVE      *ZEROS        LGARCV
     C                   MOVE      *ZEROS        LGAENT
     C                   MOVE      *ZEROS        LGDSQA
     C                   MOVE      *ZEROS        LGDSQB
     C                   MOVE      *ZEROS        LGDSQC
     C                   MOVE      *ZEROS        LGDSQD
     C                   MOVE      *ZEROS        LGDSQE
     C                   MOVE      *ZEROS        LGDSQF
     C                   MOVE      *ZEROS        LGDSQG
     C                   MOVE      *ZEROS        LGDSQH
     C                   MOVE      *ZEROS        LGDSQI
     C                   MOVE      *ZEROS        LGDSQJ
     C                   MOVE      *ZEROS        LGDSQK
     C                   MOVE      *ZEROS        LGDSQL
     C                   MOVE      *ZEROS        LGDSQM
     C                   MOVE      *BLANKS       LGHCPY
     C                   MOVE      *ZEROS        LGSCHP
     C                   MOVE      *ZEROS        LGPRSQ
     C                   MOVE      *BLANK        LGBLFL                         ADD 02/17/03
     C                   MOVEL     Q1USER        LGIUSR
     C                   Z-ADD     XXDATE        LGIDT
     C                   Z-ADD     DSTIME        LGITM
     C                   MOVEL     Q1PGM         LGIPGM
     C                   MOVE      Q1USER        LGUSER
     C                   Z-ADD     XXDATE        LGDT
     C                   Z-ADD     DSTIME        LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C*  Call api to retrieve the next available log seq#.
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   MOVE      *ZEROS        ##SEQ#
     C                   CALL      'WA045R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##SEQ#            5
     C                   PARM                    ##RTCD            1
     C*
     C                   MOVE      ##SEQ#        LGDSQH
     C                   EXSR      SRMTC
       //*** Beg Add *** 02/14/22 *********************************************
       If Q1prms > 12;
          LGbCrC = qqBcRc;
       Endif;
       //*** End Add *** 02/14/22 *********************************************
      /Free
       Exsr srMtCc;
      /End-free
     C     XXVMTC        IFEQ      'Y'
     C*
     C                   EXSR      DARTS
     C                   WRITE     PLG01
     C*
     C                   ENDIF
     C                   ENDSR
     C**************************************************************************
     C*  SRDRT1 - This subroutine determines if a DARTS remark should
     C*    be created for the processed transaction.
     C**************************************************************************
     C     SRDRT1        BEGSR
     C*
     C*  Retrieve the EDI Claim Control Maintenance Record.
     C*
     C                   MOVEL(P)  QQTSET        XXTSET
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IXFNRC
     C                   ENDIF
     C*
     C*  If processing a 'Final Payment', create a DARTS remark.
     C*
     C                   MOVE      'N'           XXDART
     C     XXMTC         IFEQ      'FN'
     C     IXFNRC        ANDNE     *BLANKS
     C     XXMTC         OREQ      'S1'
     C     IXFNRC        ANDNE     *BLANKS
     C                   MOVE      'Y'           XXDART
     C                   MOVEL(P)  IXFNRC        XXRMCD            5
     C                   MOVEL(P)  Q1USER        XXUSER
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C*  SRDRT2 - This subroutine writes the DARTS record.
     C**************************************************************************
     C     SRDRT2        BEGSR
     C*
     C                   MOVEL(P)  QQCO#         XCKEY            40
     C     XCKEY         CAT       QQFND:0       XCKEY
     C     XCKEY         CAT       QQFYR:0       XCKEY
     C     XCKEY         CAT       QQCASE:0      XCKEY
      *
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
     C                   MOVE      *BLANKS       Q#DCID           12
     C                   MOVE      *BLANKS       Q#FOLD           80
     C                   MOVE      *BLANKS       Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVEL(P)  XXRMCD        Q#RMCD            5
     C                   MOVE(P)   DSDATE        Q#CDAT            8
      *
     C     Q#RMCD        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  XXMTC         Q#COMM
     C                   CAT       CTDESC:2      Q#COMM
     C                   MOVEL(P)  CTDEPG        Q#DCID
     C                   MOVE      CTFL01        Q#DIFL
      *
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  XCKEY         Q#FOLD
     C                   MOVE      'Y'           Q#FL01
     C                   ENDIF
      *
     C     CTDPRG        IFGT      *ZEROS
     C                   MOVE      'Y'           Q#FL02
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL(P)  XCKEY         Q#KEY            40
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
     C                   ENDSR
      *
     C**************************************************************************
     C*---------------------------------------------------------------**********
     C*  PRCDAT - Check processing date
     C*---------------------------------------------------------------**********
     C     PRCDAT        BEGSR
      *
     C     XXCLT#        IFEQ      '114'
      *
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     XXJST         IFEQ      *BLANKS
     C     *IN60         ANDEQ     *OFF
     C                   MOVEL     CMJRST        XXJST
     C                   ENDIF
     C                   MOVE      *BLANKS       XXTSET
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
      *
     C     CMACDT        IFLT      IXCSDT
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
      *
     C     XXCLT#        IFNE      '114'
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     XXJST         IFEQ      *BLANKS
     C     *IN60         ANDEQ     *OFF
     C                   MOVEL     CMJRST        XXJST
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXTSET
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVE      IXCSDT        SVCSDT
     C/free
          svMSDT = IXmsdt;
          svRSDT = IXrsdt;
       //*** Beg Add *** 07/14/21 **********************************************
          svUsLh = IXusLh;
       //*** End Add *** 07/14/21 **********************************************
      /end-free
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      QQTSET        XXTSET
     C                   ENDSR
      /free
       Begsr srMtCc;
       If %parms > 10;
          If qqMtCc <> ' ';
             LGmtCc = qqMtCc;
          Endif;
          If qqMtC2 <> ' ';
      /end-free
     C                   Move      qqmtc2        srDti
     C                   CALL      'SRCVTDAT'    dtCYMD
     C                   movel     srDto         LGmtC2
      /free
          Endif;
       Endif;
       Endsr;
      /end-free
     C*****************************************************************
     C*  SRMTC - To check for a valid MTC for the reporting state.
     C*
     C*****************************************************************
     C     SRMTC         BEGSR
     C*
     C                   MOVE      'Y'           XXVMTC            1
     C*   - First, determine if a state table exists
     C                   MOVEL(P)  '  E2'        XXTBID
     C                   MOVEL     LGJST         XXTBID
     C     GIKY          CHAIN     PGI01                              90
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  LGMTC         XXTKEY
     C     GTKY          CHAIN     PGT01                              90
     C*
     C     *IN90         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   MOVE      'N'           XXVMTC
     C                   MOVE      '4'           QQRTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C     DARTS         BEGSR
     C*
     C     LGTSET        IFEQ      '148'
     C     LGTSET        OREQ      'A49'
     C*
     C                   MOVE      LGJST         XXJST
     C                   MOVE      LGTSET        XXTSET
     C*
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C*
     C                   SELECT
     C     LGSTS         WHENEQ    'RE'
     C     IXCRER        ANDNE     *BLANKS
     C                   MOVE      IXCRER        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EI'
     C     IXCEIR        ANDNE     *BLANKS
     C                   MOVE      IXCEIR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE      IXCECR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE      IXCTAR        Q#RMCD
     C*
     C     LGSTS         WHENEQ    'TR'
     C     IXCTRR        ANDNE     *BLANKS
     C                   MOVE      IXCTRR        Q#RMCD
     C*
     C                   OTHER
     C                   GOTO      END
     C                   ENDSL
     C*
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C*
     C     Q#RMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  CTDESC        Q#COMM
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C*
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   UPUSR         Q#USER           10
     C                   ENDIF
     C     Q#USER        IFEQ      *BLANKS
     C                   GOTO      END
     C                   ENDIF
     C     LGSTS         IFEQ      'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C     LGSTS         IFEQ      'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   ENDIF
     C*
     C     CTDPRG        IFGT      0
     C                   MOVEL(P)  'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   ENDIF
     C*
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL     *BLANKS       Q#COMM
     C     LGMTC         CAT       '-':0         Q#COMM
     C                   CAT       CTDESC:0      Q#COMM
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   ENDIF
     C                   ENDIF
     C     END           ENDSR
      /Free
       //********************************************************************
       // Subroutine EdiCtrl - Get EDI control settings
       //********************************************************************
       Begsr EdiCtrl;
        @RTNCD = *BLANKS;
        @EDIF  = *BLANKS;
        IXELT  = *BLANKS;
        IXELTD = *ZEROS;
        IXFROI = *BLANKS;
        IXSDUE = *ZEROS;
        IXRLS  = *BLANKS;
        IXCSDT = *ZEROS;
        IXEADC = *BLANKS;
        IXHCPR = *BLANKS;
        IXFNRC = *BLANKS;
        IXHCED = *BLANKS;
        IXCRER = *BLANKS;
        IXCEIR = *BLANKS;
        IXCECR = *BLANKS;
        IXCTAR = *BLANKS;
        IXCTRR = *BLANKS;
        IXmo   = *blanks;
        IXmltd = *zeros;
        IXmsdt = *zeros;
        IXrsdt = *zeros;
        IXedny = *blanks;
        IXdnydlw = *blanks;
        //*** Beg Add *** 07/14/21 ****************************************************************
        IXusLh   = *blanks;
        //*** End Add *** 07/14/21 ****************************************************************
        //*** Beg Chg *** 07/14/21 ****************************************************************
        //***GetEdiCtrlInfo(XXCO#:XXJST:XXTSET:CMFND:@RTNCD:@EDIF:IXELT:IXELTD:
        //***               IXFROI:IXSDUE:IXRLS:IXCSDT:IXEADC:IXHCPR:IXFNRC:IXHCED:
        //***               IXCRER:IXCEIR:IXCECR:IXCTAR:IXCTRR:
        //***               IXMO:IXMLTD:IXMSDT:IXRSDT:IXedny:IXdnydlw);
        GetEdiCtrlInfo(XXCO#:XXJST:XXTSET:CMFND:@RTNCD:@EDIF:IXELT:IXELTD:
                       IXFROI:IXSDUE:IXRLS:IXCSDT:IXEADC:IXHCPR:IXFNRC:IXHCED:
                       IXCRER:IXCEIR:IXCECR:IXCTAR:IXCTRR:
                       IXMO:IXMLTD:IXMSDT:IXRSDT:IXedny:IXdnydlw:IXusLh);
        //*** End Chg *** 07/14/21 ****************************************************************

       Endsr;
      /End-Free
         DCL-PROC GetDenyFlag;
         DCL-PI *N IND;
            @IXedny char(1) Const;
            @cmdny char(1) Const;
            @IxdnyDlw char(1) Const;
            @cmdtlw packed(7:0) Const;
         END-PI;

         DCL-S isDeny ind ;

         isDeny = *Off;

         If @IXedny = 'Y' and @CMdny = 'Y';
            If @IXdnyDlw = 'Y';
               If @CMdtlw > *Zero;
                  isDeny = *On;
               Endif;
            Else;
                  isDeny = *On;
            Endif;
         Endif;

         Return isDeny;
       END-PROC GetDenyFlag;
