     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE EDI Research w/ROES
    H*********************************************************************
    H*  CALLED FROM: W8079C1                                             *
    H*                                                                   *
    H*  CALLS TO:
    H*                                                                   *
    H*  CREATED: 12/21/07      C:184     PROJECT: 135WKI                 *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* 09/07/19  C:062 Project: 152WAU                                   *
     H*                 Change date conversions                           *
    H*                                                                   *
     H* 01/28/10  C:249 Project: SY215                                    *
     H*                 Retrieve brand Name                               *
     H*                                                                   *
    H* 04/14/08 C:15  Project: 90WFR                                     *
    H*               -Added code to update existing log records.         *
    H*               -Use the claim transfer file to retrieve a valid    *
    H*                claim key, if the key from the backfill file is    *
    H*                not valid.                                         *
    H*********************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWXLGP     IF   E             DISK    INFDS(DBFDS)
     FWMCML1    UF   E           K DISK
     F*** Begin Add *** 11/17/19 ******************************************
     FWMCML17   IF   E           K DISK    Rename(pcm01:pcm17)
     F*** End   Add *** 11/17/19 ******************************************
     F*** Begin Add *** 11/17/19 ******************************************
     FWMCMLG    IF   E           K DISK    Rename(pcm01:pcm0G)
     F*** End   Add *** 11/17/19 ******************************************
     FSMUPL     IF   E           K DISK
     FWMLGP     O    E             DISK
    F*** Begin add *** 04/14/08 ******************************************
     FWACTL2    UF   E           K DISK
     FWMLGL     UF   E           K DISK    rename(plg01:plg02)
    F*** End add ***** 04/14/08 ******************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN66)

     D upper           C                   Const('ABCDEFGHIJKLMNOPQRST+
     D                                     UVWXYZ')
     D lower           C                   Const('abcdefghijklmnopqrst+
     D                                     uvwxyz')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D  ERRARR         S             40    DIM(6) CTDATA  PERRCD(1)
     D*
     D  DBFDS          DS
     D  DBRRN                397    400B 0
     D*
     D  XXCO#          S              3  0
     D  XXFND          S              3  0
     D  XXFYR          S              3  0
     D  XXCASE         S              7  0
     D  XXTITL         S                   LIKE(ORTITL)
     D  XXERDS         S             40    INZ
     D  ERRIND         S              1N   INZ('0')
     D  HDRPRT         S              1N   INZ('0')
     D  FSTHDR         S              1N   INZ('0')
    d*** Begin add *** 04/14/08 ******************************************
     D  svtrdt         S              7  0
     D  update         S              1
    d*** End add ***** 04/14/08 ******************************************
     D*
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     D*
     D*** Date/Time Data Structure
     D*
     D                 DS            12
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM                    SRFFMT            8
     C                   PARM                    SRTFMT            8
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
     C     DTCYMD2       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* KEYLIST COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*                                                                   SRINIT
     C* KEYLIST OUTPUT ROUTING FILE                                       SRINIT
     C*                                                                   SRINIT
     C     ORKY          KLIST                                              INIT
     C                   KFLD                    Q1PGM                      INIT
     C                   KFLD                    XXFILE            1        INIT
     C*
     C* KEYLIST CLAIM FILE
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C                   TIME                    DSDTTM
     C*** MAIN PROCESSING STARTS *******************************************
      /free
       Chain (Q1user) pup01;
       If %Found(smupl);
          xxCo# = UPco#;
       Else;
          xxCo# = 777;
       EndIf;
      /end-free
     C*
     C                   READ      WXLGP
     C                   DOW       NOT %EOF(WXLGP)
     C*
     C                   EXSR      SRVALID
     C*
     C                   IF        ERRIND = '0'
    C*** Begin add *** 04/14/08 ******************************************
     C                   eval      update = 'N'
     C                   exsr      srupd
     C                   IF        update = 'N'
    C*** End add ***** 04/14/08 ******************************************
     C                   EXSR      SRMVXM
     C                   WRITE     PLG01
    C*** Begin add *** 04/14/08 ******************************************
     C                   ENDIF
    C*** End add ***** 04/14/08 ******************************************
     C                   ENDIF
     C*
     C                   READ      WXLGP
     C                   ENDDO
     C*
     C*
     C                   EVAL      *INLR = *ON
     C*** MAIN PROCESSING ENDS *********************************************
     C**********************************************************************
     C* VALIDATION SUBROUTINE
     C**********************************************************************
     C     SRVALID       BEGSR
     C*
     C                   EVAL      ERRIND = '0'
      /free
       LXacLc = %Xlate(lower:upper:LXacLc);
       Chain (xxCo#:LXacLc) pcm17;
       If %Found(wmcml17);
          xxCo#  = CMco#;
          xxFnd  = CMfnd;
          xxFyr  = CMfyr;
          xxCase = CMcase;
       Else;
          If %Subst(LXaclc:4:1) = '-' And %Subst(LXaclc:8:1) = '-' And
             %Subst(LXaclc:16:1) = ' ';
     C* CHECK FOR CLAIM RECORD
     C*
     C     3             SUBST     LXACLC:1      X1FND             3
     C                   MOVEL     X1FND         XXFND
     C     3             SUBST     LXACLC:5      X1FYR             3
     C                   MOVEL     X1FYR         XXFYR
     C     7             SUBST     LXACLC:9      X1CASE            7
     C                   MOVEL     X1CASE        XXCASE
      /free
          Else;
             Chain (xxCo#:LXacl#) pcm0g;
             If %Found(wmcmlg);
                xxFnd  = CMfnd;
                xxFyr  = CMfyr;
                xxCase = CMcase;
             Else;
                xxFnd  = 0;
                xxFyr  = 0;
                xxCase = 0;
             EndIf;
          EndIf;
       EndIf;
      /end-free
     C*
     C*** RETRIEVE COMPANY RECORD
     C     COKY          CHAIN     PCO01
     C*
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End Add ***** 01/28/10 **************************************
     C     CMKY          CHAIN     PCM01
    C*** Begin add *** 04/14/08 ******************************************
     C                   IF        NOT(%FOUND)
     C*
     C                   setoff                                       99
     C                   eval      svtrdt = 0
     C                   dow       *in99 = *off
     C     CMKY          CHAIN     PCT01                              99
     C                   if        *in99 = *off and cttrdt > svtrdt
     C                             and ctxfyr <> ctfyr
     C                   eval      svtrdt = cttrdt
     C                   eval      xxfyr  = ctfyr
     C                   else
     C     CMKY          reade     PCT01                                  99
     C   99              leave
     C                   endif
     C     CMKY          reade     PCT01                                  99
     C                   enddo
     C*
     C                   Endif
     C*
     C     CMKY          CHAIN     PCM01
    C*** End add ***** 04/14/08 ******************************************
     C                   IF        NOT(%FOUND)
     C                   EVAL      XXERDS = ERRARR(1)
     C                   EXSR      SRWRTERR
     C                   ELSE
     C* Add state claim# to claim file.
     C                   IF        CMACL# = *blanks
     C                   EVAL      CMACL# = LXACL#
     C                   update    PCM01
     C                   ENDIF
     C*
     C                   ENDIF
     C* CHECK FOR LXACLC
     C                   IF        LXACLC = *BLANKS
     C                   EVAL      XXERDS = ERRARR(2)
     C                   EXSR      SRWRTERR
     C                   ENDIF
     C*
     C* CHECK FOR LXTSET
     C                   IF        LXTSET = *BLANKS
     C                   EVAL      XXERDS = ERRARR(3)
     C                   EXSR      SRWRTERR
     C                   ENDIF
     C*
     C* CHECK FOR LXMTC
     C                   IF        LXMTC = *BLANKS
     C                   EVAL      XXERDS = ERRARR(4)
     C                   EXSR      SRWRTERR
     C                   ENDIF
     C*
     C* CHECK FOR LXJST
     C                   IF        LXJST = *BLANKS
     C                   EVAL      XXERDS = ERRARR(5)
     C                   EXSR      SRWRTERR
     C                   ENDIF
     C*
     C* CHECK FOR LXSTS
     C                   IF        LXSTS = *BLANKS
     C                   EVAL      XXERDS = ERRARR(6)
     C                   EXSR      SRWRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C*********************************************************************
    C*** Begin add *** 04/14/08 ******************************************
     C*********************************************************************
     C* This subroutine determines if the log record already exists, and
     C* if so, updates that log record.
     C*********************************************************************
     C     srupd         BEGSR
     C*
     C                   eval      svtrdt = 0
     C     CMKY          CHAIN     PLG02                              99
     C                   dow       *in99 = *off
     C                   if        lgpgm <> 'W8079R2'
     C*
     C*  If a match is found, update the log record.
     C*
     C                   if        lgjst = lxjst and lgtset = lxtset
     C                             and lgmtc = lxmtc and lgsts <> 'TA'
     C                             and lgsts <> 'TE' and lgsts <> 'TR'
     C*
     C                   EVAL      LGSTS = LXSTS
     C*
     C                   IF        LXSENT > *ZERO
     C                   Z-ADD     LXSENT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGSENT
     C                   ENDIF
     C*
     C                   IF        LXARCV > *ZERO
     C                   Z-ADD     LXARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGARCV
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   EVAL      LGTIME = DSTIME
     C                   EVAL      LGUSER = Q1USER
     C                   EVAL      LGPGM  = 'W8079R2'
     C*
     C                   update    plg02
     C                   eval      update = 'Y'
     C                   leave
     C                   endif
     C*
     C                   endif
     C     cmky          reade     plg02                                  99
     C                   enddo
     C*
     C                   ENDSR
    C*** End add ***** 04/14/08 ******************************************
     C*********************************************************************
     C* SUBROUTINE TO CHECK OVERFLOW
     C*********************************************************************
     C     SRWRTERR      BEGSR
     C*
     C                   IF        HDRPRT = '0'
     C                   EXSR      SRPDST
     C                   EXCEPT    RPHDR1
     C                   EVAL      FSTHDR = '1'
     C                   EVAL      HDRPRT = '1'
     C                   ENDIF
     C*
     C                   IF        *IN66 = *ON AND FSTHDR = '0'
     C                   EXCEPT    RPHDR1
     C                   EVAL      *IN66 = *OFF
     C                   ELSE
     C                   EVAL      FSTHDR = '0'
     C                   ENDIF
     C*
     C                   EXCEPT    RPDTL1
     C*
     C                   EVAL      ERRIND = '1'
     C*
     C                   ENDSR
     C*********************************************************************
     C**********************************************************************
     C* PRINT DISTRIBUTION AND HEADER PAGES
     C**********************************************************************
     C     SRPDST        BEGSR
     C*
     C                   EVAL      XXFILE = '0'
     C*
     C     ORKY          CHAIN     POR01
     C                   IF        %FOUND
     C                   EVAL      XXTITL = ORTITL
     C                   ENDIF
     C*
     C                   EXCEPT    RPDST1
     C*
     C                   ENDSR
     C**********************************************************************
     C* MOVE FIELD VALUES FROM WXLGP TO WMLGP
     C**********************************************************************
     C     SRMVXM        BEGSR
     C                   EVAL      LGCO# = XXCO#
     C                   EVAL      LGFND = XXFND
     C                   EVAL      LGFYR = XXFYR
     C                   EVAL      LGCASE = XXCASE
     C                   EVAL      LGTSET = LXTSET
     C                   EVAL      LGMTC = LXMTC
     C                   EVAL      LGJST = LXJST
     C                   EVAL      LGSTS = LXSTS
     C*
     C                   Z-ADD     LXMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C                   MOVEL     SRDTO         LGMTCD
     C*
     C                   Z-ADD     LXDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C                   MOVEL     SRDTO         LGDUE
     C                   IF        LGDUE = *ZERO
     C                   IF        LGMTCD <> *ZERO
     C                   Z-ADD     LGMTCD        LGDUE
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     LXSENT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C                   MOVEL     SRDTO         LGSENT
     C                   IF        LGSENT = *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGSENT
     C                   ENDIF
     C*
     C                   Z-ADD     LXARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C                   MOVEL     SRDTO         LGARCV
     C                   IF        LGARCV = *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGARCV
     C                   ENDIF
     C*
     C*** Begin Chg *** 09/07/19 ******************************
     C**********         Z-ADD     DSDATE        SRDTI
     C**********         CALL      'SRCVTDAT'    DTCYMD
     C**********         MOVEL     SRDTO         LGEXDT
     C                   EVAL      LGEXDT = LGMTCD
     C*** End   Chg *** 09/07/19 ******************************
     C*
     C                   MOVEL     SRDTO         LGIDT
     C                   MOVEL     SRDTO         LGDT
     C                   EVAL      LGITM = DSTIME
     C                   EVAL      LGTIME = DSTIME
     C                   EVAL      LGIUSR = Q1USER
     C                   EVAL      LGUSER = Q1USER
     C                   EVAL      LGIPGM  = 'W8079R2'
     C                   EVAL      LGPGM  = 'W8079R2'
     C                   ENDSR
     C*****************************************************************
     O
     OQSYSPRT   E            RPDST1         1
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDST1         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDST1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST1         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST1         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDST1         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST1         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST1         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDST1         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDST1         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST1         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDST1         2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            RPDST1         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDST1         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     o*** Beg Chg *****  01/28/10  ************************************
     O***       E            RPDST1           60
     O***                                        23 '(C) COPYRIGHT FISERV IN'
     O***                                        46 'SURANCE SOLUTIONS. INC,'
     O***                                        51 '.2007'
     O          e            rpdst1         1 59
     O                       #brnd1
     O          e            rpdst1         1
     O                       #brnd2
     o*** END CHG *****  01/28/10  ************************************
     O          E            RPHDR1         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              89
     O                       DSDATE        Y    128
     O          E            RPHDR1         2
     O                       Q1PGM               10
     O                       XXTITL              99
     O                                          123 'PAGE:'
     O                       PAGE1         Z    128
     O          E            RPHDR1         2
     O                                            9 'COMPANY #'
     O                                           14 'FUND'
     O                                           19 'YEAR'
     O                                           24 'CASE'
     O                                           45 'REASON OF ERROR'
     O          E            RPDTL1         1
     O                       XXCO#                3
     O                       XXFND               13
     O                       XXFYR               18
     O                       XXCASE              27
     O                       XXERDS              70
**CTDATA ERRARR
Claim not found.
Claim Admin is blank.
Transaction Set ID is blank.
Maintainence Type code is blank.
Jurisdiction state is blank.
Entry status is blank.
