     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('STBNDDIR')
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0340R1 - EOMB WORKFILE BUILD                            *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:02                                              *
     H* CREATED:    07/31/98  PROJECT: 48MBS                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  03/11/25 C:342 PROJECT: 152MCE                               *
     H*                 S1 EOB Export File.                           *
     H*                                                               *
     H*  09/22/23 C:062 PROJECT: 146MKP                               *
     H*                 Add other 2 fee processing.                   *
     H*                                                               *
     H*  09/05/20 C:062 PROJECT: WC2749                               *
     H*                 Add V-pay processing for zero eobs.           *
     H*                                                               *
     H*  03/21/19 C:180 PROJECT: 146MIZ                               *
     H*                 Issue separate check for % of fee schedule    *
     H*                 savings.                                      *
     H*                                                               *
     H*  02/12/18 C:180 PROJECT: 114MJC                               *
     H*                 Add 835 export                                *
     H*                                                               *
     H*  08/26/08 C:62  PROJECT: 25MOS                                *
     H*                 DON'T CALC EAOCA1 IF NOT ZERO                 *
     H*                                                               *
     H*  02/07/07 C:62  PROJECT: 144MAP                               *
     H*                 POPULATE EAUSER WITH EOUSER AND EARMDS WITH   *
     H*                 CMCLAD.                                       *
     H*                                                               *
     H*  12/09/04 C:62  PROJECT: NY007                                *
     H*                 POPULATE EAOCA1                               *
     H*                                                               *
     H*  01/21/02 C:62  PROJECT: AR033                                *
     H*                 UPDATE EANHRF WITH 'H'.                       *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  07/27/99  C:62  PROJECT: MC201                               *
     H*                  ADD UPDATE OF EAAPPF IN FAEAP.               *
     H*                                                               *
     H*  04/22/99  C:94  PROJECT: MC154                               *
     H*                  BASE CALL TO W0081R ON AUTO CHECK ISSUANCE   *
     H*                  FLAG                                         *
     H*                                                               *
     H*  02/16/99  C:94  PROJECT: 25MKL                               *
     H*                  DEL UPDATE OF EAEPFL AND EAAPPF              *
     H*                                                               *
     H*  11/10/98  C:94  PROJECT: 87MBV                               *
     H*                  ADDED ALL UPDATES THAT WERE DONE IN          *
     H*                  F6020R AND F7071R1 (BASE VERSION)            *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H* 61 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     F*** EOMB File selected on EOSTAT *NE 'P'
     FFAEAL1    UF   E           K DISK
     F*
     FFYEAP     O    E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFYEA835P  O    E           K DISK
     F                                     RENAME(PEA01:PEA835)
     FFAEOL4    UF   E           K DISK
     F*** Beg Add *** 03/11/25 **************************************
     FFAEOLG    IF   E           K DISK    RENAME(PEO01:PEO02)
     FFYEAL7    IF   E           K DISK    RENAME(PEA01:PEA07)
     F*** End Add *** 03/11/25 **************************************
     F*
     FFMCML1    IF   E           K DISK
     F*
     FFMF1L     IF   E           K DISK
     F*
     F***** FROM F6020R *****
     F*
     FFDCALL    UF   E           K DISK
     F*
     FFMFNL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFACLML8   IF   E           K DISK
     d changeWCMCdemo  pr                  ExtPgm('FA002R')
     D*** Begin Add *** 09/05/20 **************************************
     D* V-Pay OutFile Process
     d VPayOutFile     pr                  ExtPgm('FAVPAYOUT')
     D   inCompany#                   3  0 Const
     D   inSubscriber                 5  0 Const
     D   inGroup#                     3  0 Const
     D   inYear                       3  0 Const
     D   inCase                       7  0 Const
     D   inEob#                       9  0 Const
     D   inRecon                      3  0 Const
     D   inMode                       1    Const
     D*** End   Add *** 09/05/20 **************************************
     D*** Beg   Add *** 03/11/25 **************************************
     d GenerateEOB     PR                  ExtPgm('F0346C')
     d  faeopRRN                      9
     d  adjState                      2
     d  docPath                     255

     dGetAdjState      pr                  EXTPGM('FA787R')
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inCase                         7  0
     d OutAdjState                    2
     dGetFaeopRrn      pr                  EXTPGM('FA788R')
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inCase                         7  0
     d inEob#                         9  0
     d OutFaeopRrn                    9

     D*** End   Add *** 03/11/25 **************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C***** Beg Add ** 03/11/25*******************************************
     d/copy sPrEnv
     d ExpFolder       ds           256    dtaara(F0340RPTH)
     d strdir                  1    256
     d claimantName    s             46    inz
     d newdate         s              6    inz
     d faeopRRN        s              9
     d adjstate        s              2
     d docPath         s            255
     d dirLnk          s            255
     d Environ         s              1
     d sysDate         s               d
     C***** End Add ** 03/11/25*******************************************
     C*
     C*  CLAIM MASTER FILE
     C*
     C*          CMKY      KLIST
     C*                    KFLD           EOCO#
     C*                    KFLD           EOSUB#
     C*                    KFLD           EOFND
     C*                    KFLD           EOFYR
     C*                    KFLD           EOCASE
     C*
     C*
     C     CMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FS BASE CONTROL FILE KEY LIST
     C*
     C*
     C*  EOB FILE KEY LIST
     C*
     C*          EOKY      KLIST
     C*                    KFLD           XXCO#
     C*
     C*  EOB FILE KEY LIST
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*  FUND AUXILIARY FILE KEY LIST
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C                   KFLD                    EAEOB#
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
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQWCPR            1
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EASUB#        XXSUB#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    F1OUTA        XXOUTA
     C     *LIKE         DEFINE    EANAMT        @@NAMT
     C*** Begin Chg *** 09/22/23 *******************************************
     C******LIKE         DEFINE    EANAMT        @@MRA
     C     *LIKE         DEFINE    EAOCA2        @@OCA2
     C*** End   Chg *** 09/22/23 *******************************************
     C     *LIKE         DEFINE    EAUSER        HDUSER
     C     *LIKE         DEFINE    EARMDS        HDRMDS
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANK        ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANK        ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANK        ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANK        ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANK        ##SDTS            1            ALLOW FUTURE SERV DT
     C*
     C*
     C* Retrieve the client id
     C*
     C                   SETOFF                                       16
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C     XXCLT#        IFEQ      '144'
     C                   SETON                                        16
     C                   ENDIF
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C* CHAIN OUT TO SYSTEM CONTROL FILE
     C*
     C*
     C*
     C*
     C     ##PAY0        IFEQ      'Y'
     C                   MOVE      ##PAY0        QQWCPR
     C                   ELSE
     C                   MOVE      *BLANK        QQWCPR
     C                   ENDIF                                                  XXWCPR IFEQ
     C*
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXSUB#
     C*
     C*
     C* SET UP FOR READ OF EOB FILE
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY          READE     PEA01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C*
     C     EASUB#        IFNE      XXSUB#
     C     EAFND         ORNE      XXFND
     C*
     C                   MOVE      EASUB#        XXSUB#
     C                   MOVE      EAFND         XXFND
     C*
     C     F1KY          CHAIN     PF101                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVE      F1OUTA        XXOUTA
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ENDIF                                                  EOSUB# IFNE
     C*
     C     XXOUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C*
     C                   ELSE
     C*
     C     EAPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EAPAY IFLE 0
     C*
     C                   ENDIF                                                  XXOUTA IFEQ
     C*
     C*
     C                   MOVE      Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C                   Z-ADD     EAEOB#        CACHK#
     C*
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C*
     C     CAKEY         READE     PCA01                                  50
     C                   END
     C     *IN16         IFEQ      *ON
     C                   MOVEL(P)  EAUSER        HDUSER
     C                   MOVEL(P)  EARMDS        HDRMDS
     C     EOKY          CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EOUSER        EAUSER
     C                   ELSE
     C                   CLEAR                   EAUSER
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMCLAD        EARMDS
     C                   ELSE
     C                   CLEAR                   EARMDS
     C                   ENDIF
     C                   ENDIF
     C*** Begin Add *** 09/05/20 **************************************
      /Free
       // Call V-Pay Process for Zero Eobs
       If EApLnk = '2' And EAeftC = 'V';
          VPayOutFile(EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#:EArcn:'B');
       EndIf;
      /End-Free
     C*** End   Add *** 09/05/20 **************************************
      *
     C                   WRITE     PEA02
      *
     C                   MOVE      'H'           EABHRF
     C                   MOVE      'H'           EANHRF
     C*
     C                   MOVE      EASUB#        ##SUB#
     C                   MOVE      EAFND         ##GRP#
     C                   MOVE      EAEOB#        ##EOB#
     C                   MOVE      EAFYR         ##FYR
     C                   MOVE      EACASE        ##CASE
     C*
     C*
     C     ##ACIS        IFEQ      'Y'
     C     EAAPPF        ANDEQ     *BLANKS
     C*
     C                   CALL      'W0081R'
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##GRP#            3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##EOB#            9
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      *ZEROS        @@NAMT
     C*** Begin Chg *** 09/22/23 *******************************************
     C**********         PARM      *ZEROS        @@MRA
     C                   PARM      *ZEROS        @@OCA2
     C*** End   Chg *** 09/22/23 *******************************************
     C*
     C                   ENDIF
     C                   MOVE      'Y'           EAAPPF
     C     @@NAMT        IFNE      *ZEROS
     C*
     C                   Z-ADD     @@NAMT        EANAMT
     C*
     C                   ENDIF
      /Free
       //*** Begin Chg *** 09/22/23 ****************************************
       //*** If @@MRA > *Zeros;
       //*EAoca2 = @@MRA;
       If @@OCA2 > *Zeros;
          EAoca2 = @@OCA2;
       //*** End   Chg *** 09/22/23 ****************************************
       Endif;
      /End-Free
     C     EAOCA1        IFEQ      *ZEROS
     C     EAINPE        ADD       EAOUTE        EAOCA1
     C                   ADD       EAASCE        EAOCA1
     C                   ADD       EACLNE        EAOCA1
     C                   ENDIF
     C     *IN16         IFEQ      *ON
     C                   MOVEL(P)  HDUSER        EAUSER
     C                   MOVEL(P)  HDRMDS        EARMDS
     C                   ENDIF
      /Free
       //Determine 835 export processing type/qualifier

       EA835Q = *blanks;

       //Check if 837 Claim
       Chain (EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#) FACLML8;

       If %found(FACLML8);
          // Check in Fund file
          Chain (EAco#:EAsub#:EAfnd) FMFNL;
          If %found(FMFNL);
             If FNoeob = '835';
                EA835Q = 'G';
             Endif;
          Endif;

          // Check in Vendor File
          Chain (EAfein:EAmod) FMPDL;
          If %found(FMPDL);
             If PD835P = 'Y';
                Select;
                When EA835Q = 'G';
                   EA835Q = 'B';
                When EA835Q = *blanks;
                   EA835Q = 'V';
                Endsl;
             Endif;
          Endif;

          //Write to Work File
          If EA835Q <> *blanks;
             Write PEA835;
          Endif;
       Endif;
      /End-Free
     C*
     C                   UPDATE    PEA01
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     'P'           EOSTAT
     C*
     C                   MOVE      Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
      *
      *
     C     EAKY          READE     PEA01                                  50
      *
     C                   ENDDO                                                  *IN50 DOWEQ
      *
     C***** Beg Add ** 03/11/25*******************************************
          If xxClt# = '152';
             Environ = CheckEnv();
             In *lock ExpFolder;
             If strDir = *blanks;
                If Environ = 'Q';
                   Dirlnk  = '/pds/952/Completed EOBs';
                Else;
                   Dirlnk  = '/pds/152/Completed EOBs';
                Endif;
             Else;
                Dirlnk = strDir;
             Endif;

             Setll *loval pea07;
             Read fyeal7;
             Dow not %eof(fyeal7);
                Chain (EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#) peo02;
                If %found(Faeolg);
                   AdjState = *blanks;
                   faeopRRN = *blanks;
                   docPath  = *blanks;
                   Callp GetAdjstate(EOco#:EOfnd:EOfYr:EOcase:AdjState);
                   Callp GetFaeopRrn(EOco#:EOfnd:EOfYr:EOcase:EOeob#:faeopRRN);
                   docPath  = %trim(Dirlnk) + '/'
                            + %trim(EAsinv) + '_' + %EditC(EAfnd:'X')
                            + %EditC(EAfyr:'X')  + %EditC(EAcase:'X');
                   Chain cmky pcm01;
                   If %found(fmcml1);
                      claimantName = %trim(CMlsn) + ' ' + %trim(CMfsn);
                   Else;
                      claimantName = *blanks;
                   Endif;

                   sysDate = %date();
                   newdate  = %editc(%dec(sysDate:*mdy):'X');

                   docPath = %trim(docPath) + '_' + %trim(claimantName) +
                             '_' + newdate + '.pdf';
                   // Create the EOB PDF
                   GenerateEOB(faeopRRN:Adjstate:docPath);
                Endif;
                Read fyeal7;
             Enddo;

          Endif;
     C***** End Add ** 03/11/25*******************************************
     C     TERM          TAG
      *
     C                   SETON                                        LR
