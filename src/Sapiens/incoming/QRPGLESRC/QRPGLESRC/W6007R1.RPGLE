     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W6007R1 - PROCESS AUTO CHECK REQUESTS
     H*****************************************************************
     H* PROGRAM DESCRIPTIION:                                         *
     H*   Process Auto Check Requests for Daily Batch and             *
     H*   Group Batch.                                                *
     H*                                                               *
     H* THIS PROGRAM:                                                 *
     H*   Reads all Groups for the Company number.                    *
     H*   Browses the Request for Payment for each Group.             *
     H*   If the Request Date is less than or equal to the            *
     H*   entered Process Date,                                       *
     H*     then Group Aux is accessed to obtain the Bank Acct,       *
     H*     the Check Summary file is accessed for Last Chk#,         *
     H*     the Check Request is accessed.  If the record is          *
     H*     not found, the fields are assigned and the record         *
     H*     is written.  If the record is found, all fields           *
     H*     except Updated, Printed, Wasted, & Restart Chk#           *
     H*     are assigned and the record updated.                      *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W6007C1                                                     *
     H* CALLS  TO:                                                    *
     H*   none                                                        *
     H*                                                               *
     H* CREATED 05/28/04  C:53  PROJECT: WC1198                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/31/21 C:079 Support: 152/95226                             *
     H*                Remove flgSkipChk for client 152               *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                Add Starting EFT and E-Pay# and E-Pay Vendor   *
     H*                                                               *
     H* 04/30/19 C:062 Project: 152WAB                                *
     H*                Apply Optacomp Mods.                           *
     H*
     H* 11/06/18 C:053 Project: 090WIM
     H*                Client does not want to create check schedule records
     H*
     H* 07/31/15 C:136 Project: WC2337                                *
     H*               -Add processing for EFT payments                *
     H*               -Add error report. If routing is not set up,    *
     H*                the report will go to the user profile and     *
     H*                job name that the auto batch job was submitted *
     H*                under.                                         *
     H*                                                               *
     H* 02/28/14 C:136 Support: RED3966                               *
     H*                Temp fix to TASB was removed so that this      *
     H*                program would work with RED3966 change in      *
     H*                WA023R when V14R3 or later is installed.       *
     H*                                                               *
     H* 07/22/13 C:136 Project: 147WBD Priss 5                        *
     H*                Client request to not auto create              *
     H*                check schedule records                         *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                1) Change keylist RPKY1                        *
     H*                2) Use GetBankAcc instead of WA023R            *
     H*                                                               *
     H* 04/19/10 C:235 PROJECT: 25WRG 25WOO                           *
     H*               Added code to create Check Schedule records for *
     H*               EFT accounts or any other bank codes if there   *
     H*               is more than one per group.                     *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 05/01/09 C:220 PROJECT: WC1766                                *
     H*                Create Holiday Blackout Date entry screen      *
     H*                for mailing Claims checks                      *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                Default mailing date to next working date after*
     H*                processing date                               *
     H*                                                               *
     H* 09/01/06 C:15  Support: WC135/54458                           *
     H*                Added code to prevent "test" groups from being *
     H*                processed.  This will prevent checks from      *
     H*                automatically printing on a test group.        *
     H*                                                               *
     H* 01/25/05 C:62 Project: 45566                                  *
     H*               -MAKE SURE THE RESTART FLAG IS SET TO 'Y' IF    *
     H*                THERE IS A TAG VALUE                           *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*    35 -  *ON for Group Batch, *OFF for Daily Batch            *
     H*    40 -  Check Request file hit or no hit                     *
     H*    99 -  General File Access                                  *
     H*
     H*  @@RTCD VALUES
     H*     0 - Invalid co#
     H*     1 - Invalid batch date
     H*     2 - Invalid group check from date
     H*     3 - Invalid group check to date
     H*     4 - group check from date is > than the to date
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWACPL     UF A E           K DISK    USROPN
     FWAC3L     UF A E           K DISK    USROPN
     F                                     RENAME(PCP01:PCP02)
     FWARPL12   IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWSCKL     IF A E           K DISK
     FWMCXL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWZDBP     UF A E             DISK
     FWZD3P     UF A E             DISK
     fsmorl     if   e           k disk    rename(por01:por00)
     fqsysprt   o    f  102        printer oflind(*inof) usropn

      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D/copy sprCompany
     D/copy sprString
     D/copy sprRtvBrnd
     D/copy WPRBANK
     D ##acct          s             20
     D ##call          s              1
     D ##hdcy          s              1
     D ##chkq          s             10
     D ##ftyp          s             10
     D ##rtcd          s              1
     D ##efdt          s              7
     D ##cpyq          s             10
     D ##cktp          s             10
     D ##coid          s             20
     D ##appl          s              2
     D ##co#           s              3
     D ##fnd           s              3
     D ##eftc          s              1
     D ##srdt          s              8
     d clientID        s              3    inz('000')
     d flgSkipChk      s               n
     d #brnd1          s             80    inz
     d #brnd2          s             80    inz
     d companyName     s             60
     d errorMsg        s             80
     d isFirstErr      s               n
     d routeCoName     s             60
     d sysDate         s               d   datfmt(*usa)
     d sysTime         s               t   timfmt(*hms:)
     d reportTitle     s             42
     d WrtChkSched     pr              n
     d  msg                          80
     d WriteError      pr
     d  msg                          80    const
     d WrtRouting      pr
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE CHECK MAINTENANCE FILE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RPCKTP
     C                   KFLD                    RPEFTC
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBDAT
     C                   KFLD                    XXFND
     C                   KFLD                    ##ACCT
     C                   KFLD                    ##CKTP
     C                   KFLD                    ##EFTC
     C*
     C     CPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE CHECK SUMMARY FILE
     C*
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ##ACCT
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@BDAT            8            Batch dt
     C                   PARM                    @@MBCH            1            Monthly
     C                   PARM                    @@YBCH            1            Yearly
     C                   PARM                    @@FRDT            8            From dt
     C                   PARM                    @@TODT            8            To dt
     C                   PARM                    @@RTCD            1            Rtrn err msg
     C*
     C*  Date conversion parm lists
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     PLPAST        PLIST
     C                   PARM                    XXBDA8
     C                   PARM      1             ##DAYS            5 0
     C                   PARM                    ##ERR             1
     C*
     C*  Work field definitions
     C*
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    CPPRDT        XXBDAT
     C     *LIKE         DEFINE    CPFRDT        XXFRDT
     C     *LIKE         DEFINE    CPTODT        XXTODT
     C     *LIKE         DEFINE    WCKEY         XSKEY
     C*
      /free
       GetClientID(clientID);
       Select;
       //*** Beg Chg *** 03/31/21 ******************************************
       //***When clientID = '114' Or clientID = '152';
       When clientID = '114';
       //*** End Chg *** 03/31/21 ******************************************
          flgSkipChk = *on;
       When clientID = '090';
          flgSkipChk = *on;
       Other;
          flgSkipChk = *off;
       Endsl;
      /end-free
     C                   MOVE      *BLANK        @@RTCD
     C                   MOVE      *BLANK        XXPMT             1
     C*
     C                   MOVEL     @@CO#         XXCO#
     C                   MOVEL     @@BDAT        XXBDA8            8 0
     C                   MOVEL     @@FRDT        XXFRD8            8 0
     C                   MOVEL     @@TODT        XXTOD8            8 0
     C* Retrieve EDI setup flag
     C                   MOVEL(P)  'WC'          XSKEY
     C                   SETOFF                                       66
     C     XSKEY         CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL09        ANDEQ     'Y'
     C                   SETON                                        66
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Open the appropriate Check Processing file based on the check
     C*   type flag passed into the pgm.
     C*    - DATES > 0,  OPEN WACPL  (NORMAL CHECKS)
     C*    - DATES < 0,  OPEN WAC3L  (GROUP CHECKS)
     C*
     C     XXFRD8        IFGT      *ZEROS
     C     XXTOD8        ANDGT     *ZEROS
     C                   OPEN      WAC3L
     C                   MOVE      '1'           *IN35
     C                   ELSE
     C                   OPEN      WACPL
     C                   MOVE      '0'           *IN35
     C                   END
      /free
       RtvBrnd('@':#brnd1:#brnd2);
       If *in35;
          reportTitle = 'Group Auto Check Schedule Exception Report';
       Else;
          reportTitle = 'Daily Auto Check Schedule Exception Report';
       Endif;
       companyName = GetCompNam(xxCo#);
       routeCoName = companyName;
       companyName = CenterFld(companyName);
      /end-free
     C     *hms          move      q1time        sysTime
     C     *usa          move      *date         sysDate
     C*
     C*  Get/validate the company #
     C*
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      '0'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C*  If batch date is 0 get system date and subtract 1 day.
     C*
     C     XXBDA8        IFLE      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBDA8
     C*
     C                   CALL      'SRPAST8'     PLPAST
     C                   ENDIF
     C*
     C*  Validate the Batch date
     C*
     C                   Z-ADD     XXBDA8        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   Z-ADD     XXBDA8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBDAT
     C*
     C*  IF GROUP CHECK OPTION HAS BEEN SELECTED, THEN VALIDATE THE
     C*   DATE RANGE FOR THE CHECKS.
     C*
     C     *IN35         IFEQ      '1'
     C                   Z-ADD     XXFRD8        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '2'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   Z-ADD     XXFRD8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     XXTOD8        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '3'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   Z-ADD     XXTOD8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C*  DATES MUST BE IN ACENDING ORDER
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '4'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   END                                                    *IN35
     C*****
     C*  All date edits have been passed.
     C****
     C*
     C*  Create an entry in the Check Processing file for all groups
     C*    which have a request pmt eligible for processing.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     c     flgSkipChk    andeq     *off
     C                   Z-ADD     FNFND         XXFND
     C*
     C*  Don't process a group if the expiration date is greater than
     C*    the system date.
     C*
     C     FNEXDT        IFNE      *ZERO                                        Group Expiration DT
     C     FNEXDT        ANDLT     XXBDAT
     C     FNTEST        OREQ      'Y'
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*  Only process a group if there is a Group Aux record and the
     C*    'Process Check' flag is 'Y'.
     C*
     C     RPKY          CHAIN     PF201                              99
     C     *IN99         IFEQ      *ON
     C     F2CLCK        ORNE      'Y'                                          Process Claims
     C                   GOTO      NEXT
     C                   ENDIF

      /free
       Setll (xxCo#:xxFnd) prp01;
       Reade (xxCo#:xxFnd) prp01;
       Dow not(%eof);

       // Skip group checks if processing daily or
       //  daily checks if processing group checks
          If *in35 and RPpgtp = *blank or
           not(*in35) and RPpgtp <> *blank;
             Reade (xxCo#:xxFnd) prp01;
             Iter;
          Endif;

       // Write the check schedule else write to error report
          If WrtChkSched(errorMsg) = *off;
             WriteError(errorMsg);
          Endif;

       // Get the next record with a different bank code and EFT flag
       Setgt (xxCo#:xxFnd:RPcktp:RPeftc) prp01;
       Reade (xxCo#:xxFnd) prp01;
       Enddo;
      /end-free

     C     NEXT          TAG
     C     FNKY          READE     PFN01                                  99
     C                   ENDDO
     C*
     C*** Setup batch records.
     C*   WZDBP FOR DIALY
     C*
     C                   Z-ADD     XXBDA8        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXBDA2            6 0
     C*
     C     *IN35         IFEQ      '0'
     C*
     C     1             CHAIN     PDB01                              89
     C*
     C                   MOVEL     XXBDA8        DBSDT                          8-MDCY
     C                   MOVEL     XXBDA2        DBTDT                          6-MDY
     C*
     C                   MOVE      DSTIME        DBSTM
     C                   MOVE      DSTIME        DBTTM
     C                   MOVE      @@MBCH        DBMNTH
     C                   MOVE      @@YBCH        DBYEAR
     C                   MOVE      Q1USER        DBUSR
     C                   MOVE      Q1JOB         DBJOB
     C                   MOVE      @@CO#         DBCO#
     C     DBTAG         IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           DBRTRT
     C                   ENDIF
     C*
     C* Leaving DBRTRT & DBTAG as is in case this function is run
     C* agin with a restart pending.
     C*
     C  N89              UPDATE    PDB01
     C   89              WRITE     PDB01
     C*
     C                   MOVE      'D'           @@RTCD
     C*
     C*   WZD3P FOR GROUP
     C                   ELSE
     C*
     C     1             CHAIN     PD301                              89
     C*
     C                   MOVEL     XXBDA8        D3SDT                          8-MDCY
     C                   MOVEL     XXBDA2        D3TDT                          6-MDY
     C*
     C                   MOVE      DSTIME        D3STM
     C                   MOVE      DSTIME        D3TTM
     C                   MOVE      @@MBCH        D3MNTH
     C                   MOVE      @@YBCH        D3YEAR
     C                   MOVE      Q1USER        D3USR
     C                   MOVE      Q1JOB         D3JOB
     C                   MOVE      @@CO#         D3CO#
     C     D3TAG         IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           D3RTRT
     C                   ENDIF
     C*
     C* Leaving D3RTRT & D3TAG as is in case this function is run
     C* agin with a restart pending.
     C*
     C  N89              UPDATE    PD301
     C   89              WRITE     PD301
     C*
     C                   MOVE      'G'           @@RTCD
     C*
     C                   ENDIF
     C*
     C**********************************************************
     C*  END THE PROGRAM
     C*
     C     EOJ           TAG

      /free
       Select;
       When @@rtcd = '0';
          WriteError('Company number ' + @@co# + ' is invalid.');
       When @@rtcd = '1';
          WriteError('Batch date ' + @@bdat + ' is invalid.');
       When @@rtcd = '2';
          WriteError('Group check from date ' + @@frdt + ' is invalid.');
       When @@rtcd = '3';
          WriteError('Group check to date ' + @@todt + ' is invalid.');
       When @@rtcd = '4';
          WriteError('Group check dates ' + @@frdt + ' and ' + @@todt +
           ' must be in ascending order');
       Endsl;

       If %open(qsysprt);
          WrtRouting();
          Close qsysprt;
       Endif;
      /end-free

     C                   MOVE      '1'           *INLR
     C                   RETURN
      *=========================================================
     oqsysprt   e            head           1 06
     o                       sysTime             +6
     o                       companyName         80
     o                       sysDate             96
     o          e            head           2
     o                       Q1pgm               +6
     o                       reportTitle         72
     o                       page          z     96
     o          e            detail         1
     o                       errorMsg            86
     o          e            route          1 06
     o                                           20 'Company  . . :'
     o                       routeCoName         90
     o          e            route          1 59
     o                       #brnd1              +6
     o          e            route          1
     o                       #brnd2              +6
      *=========================================================
      * Procedures
      *=========================================================
     pWrtChkSched      b
     dWrtChkSched      pi              n
     d msg                           80
      /free
       ##co#  = %editc(RPco#:'X');
       ##fnd  = %editc(RPfnd:'X');
       ##srdt = %editc(xxbda8:'X');
       ##eftc = RPeftc;
       ##cktp = RPcktp;
       ##appl = 'WC';
       ##hdcy = *Blanks;
       ##chkq = *Blanks;
       ##ftyp = *Blanks;
       ##rtcd = *Blanks;
       ##efdt = *Zeros ;
       ##cpyq = *Zeros ;
       ##coid = *Blanks;
       ##acct = GetBankAcc(##co#:##fnd:##appl:##srdt:##call:##hdcy:
        ##chkq:##ftyp:##efdt:##cpyq:##cktp:##eftc:##coid:##rtcd);
       Select;
       When ##rtcd <> '1';
          Exsr WriteSchedule;
          Return *on;
       When ##eftc = *blank;
          msg = 'MICR checks cannot be written for group '
           + %char(RPfnd);
          Return *off;
       When ##eftc = 'E';
          msg = 'EFT transactions cannot be written for '
           + %char(RPfnd);
          Return *off;
       //*** Beg Add *** 07/20/20 ******************************************
       When ##eftc = 'V';
          msg = 'E-Pay transactions cannot be written for '
           + %char(RPfnd);
          Return *off;
       //*** End Add *** 07/20/20 ******************************************
       Endsl;

       // WriteSchedule subroutine

       Begsr WriteSchedule;
       //Group Checks
       If *in35 = *on;
          Chain cpky pcp02;
       //Daily Checks
       Else;
          Chain cpky pcp01;
       Endif;
       CPco#  = RPco#;
       CPfnd  = RPfnd;
       CPacct = ##acct;
       CPcktp = ##cktp;
       CPeftc = ##eftc;
       CPprdt = xxBDat;
       If not(%found);
          CPrstr = *zero;
          CPwast = *blank;
          CPsta0 = *blank;
          CPsta1 = *blank;
       Endif;
       //*** Beg Chg *** 07/20/20 ******************************************
       //*** If ##eftc = 'E';
       //***Else;
       //***   Chain ckky pck01;
       //***   CPchk# = CKlchk;
       //***Endif;
       Select;
          When ##eftc = 'E';
             CPchk# = CKlChk;
             CPePayVn = *blanks;
             CPEPay# = 0;
          When ##eftc = 'V';
             Chain(N) ckky pck01;
             If %Found (WSCKL);
                CPchk# = 0;
                CPEpay# = CKePay#;
                CPePayVn = CKePayVn;
             Endif;
          Other;
             Chain(N) ckky pck01;
             If %Found (WSCKL);
                CPchk# = CKlChk;
                CPEPay# = 0;
                CPePayVn = *blanks;
             Endif;
       Endsl;
       //*** End Chg *** 07/20/20 ******************************************
       CPcmdt = *zeros;
       // *In66 means EDI processing is enabled and
       //   CPcmdt (Mailing Date) needs to be written
       If *in66;
          Exsr WrtMailDate;
       Endif;
       // *in35 = *on (Group Chks are processing)
       If *in35 = *on;
          CPfrdt = xxFrDt;
          CPtodt = xxToDt;
       Else;
          Cpfrdt = *zeros;
          CPtodt = *zeros;
       Endif;
       Exsr WrtMaintInfo;
       Select;
       When *in35 and %found(WAC3L);
          Update pcp02;
       When *in35 and not(%found(WAC3L));
          Write pcp02;
       When not(*in35) and %found(WACPL);
          Update pcp01;
       When not(*in35) and not(%found(WACPL));
          Write pcp01;
       Endsl;
       Endsr;
      /end-free

      * WrtMaintInfo subroutine

     c     WrtMaintInfo  BEGSR
     c                   MOVE      Q1USER        CPUSER
     c                   MOVE      Q1PGM         CPPGM
     c                   TIME                    DSDTTM
     c                   Z-ADD     DSDATE        SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         CPDT
     c                   Z-ADD     DSTIME        CPTIME
     c                   ENDSR

      * WrtMailDate subroutine

     c     WrtMailDate   BEGSR
     c                   Z-ADD     *ZEROS        XXEDIM            1 0
     c     CXKY          CHAIN     PCX01                              60
     c     *IN60         IFEQ      *OFF
     c                   Z-ADD     CXEDIM        XXEDIM
     c                   ENDIF                                                  *IN60=*OFF
     c                   Z-ADD     XXEDIM        SRDAYS            5 0
     c                   Z-ADD     CPPRDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTMDY1
     c                   MOVEL     SRDTO         SRDATE            8 0
     c                   MOVE      ##CO#         @XCO#
     c                   MOVE      SRDATE        @XDTI
     c                   MOVE      SRDAYS        @XSRD
     c                   MOVE      '2'           @XMOD
     c                   MOVE      *BLANKS       @XDTO
     c                   MOVE      *BLANK        @XRTCD
     c                   CALL      'WA072R'
     c                   PARM                    @XCO#             3
     c                   PARM                    @XDTI             8
     c                   PARM                    @XSRD             5
     c                   PARM                    @XMOD             1
     c                   PARM                    @XDTO             8
     c                   PARM                    @XRTCD            1
     c                   MOVEL     @XDTO         SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         CPCMDT
     c                   MOVEL     @XDTO         SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         CPCMDT
     c                   ENDSR

     pWrtChkSched      e
      *=========================================================
     pWriteError       b
     dWriteError       pi
     d msg                           80    const
      /free
       If not %open(qsysprt);
          Open qsysprt;
       Endif;
       If *inof = *on or isFirstErr = *off;
          Except head;
          *inof = *off;
          isFirstErr = *on;
       Endif;
       errorMsg = msg;
       Except detail;
       Return;
      /end-free
     pWriteError       e
      *=========================================================
     pWrtRouting       b
      /free
       Chain (Q1pgm:'0') por00;
       If not(%found);
          Return;
       Else;
          Except Route;
       Endif;
      /end-free
     pWrtRouting       e

