     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7091R2 - CLAIM LABELS REPRINT - PRINT PGM
     H****************************************************************
     H*  PROGRAMMER:  C:79     PROJECT# WC742                        *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*         W7091C2 - CLAIM LABELS REPRINT - OVERRIDE            *
     H*  CALLS TO:                                                   *
     H*         SRFRLS - BUILD A COMBINED NAME FROM FIRST AND LAST   *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                              *
     H*  09/15/99 C:112 Support #: 19591                             *
     H*               Modify date to print as an 8 character from 6  *
     H*               Modify print of Fiscal Year on label from 2    *
     H*               characters to 3 as part of the case #.         *
     H*                                                              *
     H*  08/31/99 C:112 Support # 19458                              *
     H*               Labels are not printing out when they need to. *
     H*               If a record only claim (CMLTFL = 'R') is       *
     H*               changed later to Medical or Indemnity          *
     H*               (CMLTFL = 'M' or 'I') then a label is required *
     H*               but is not printing. Modified code to pick up  *
     H*               this condition and print labels required.      *
     H*                                                              *
     H*  08/12/99 C:112 Project: WC861                               *
     H*               If field CXRLBL = 'Y' then print labels for    *
     H*               'Record Only' claims (CMLTFL='R') along with   *
     H*               Medical (CMLTFL='M') and Indemnity (CMLTFL='I')*
     H*               claims                                         *
     H*                                                              *
     H*  12/22/98  C:90  SEQ#: WC081/14777                           *
     H*                  MOVED PRINT INFO TO LEFT TO FIT LABEL       *
     H*                                                              *
     H*  06/05/98  C:33  SEQ#: WC082/12005                           *
     H*                  CHAIN TO EMPLR USING DIVISION               *
     H*                                                              *
     H*  02/25/98  C:58  SUPPORT: WC091  SEQ # 10195                 *
     H*           Corrected line spacing.                            *
     H****************************************************************
     H*  INDICATOR USAGE:                                            *
     H*                                                              *
     H*   50  -  GENERAL CHAINING                                    *
     H*   60  -  GENERAL CHAINING                                    *
     H*                                                              *
     H****************************************************************
      *** BEGIN DELETE *** 08/12/99 **********************************
     F*******WMCML3  IF  E           K        DISK
      *** END CHANGE ***** 08/12/99 **********************************
      *** BEGIN ADD ****** 08/31/99 **********************************
     FWMCML3    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
      *** END ADD ******** 08/31/99 **********************************
      *** BEGIN ADD ****** 08/12/99 **********************************
     FWMCML10   IF   E           K DISK
      *** END ADD ******** 08/12/99 **********************************
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *** BEGIN ADD *** 08/12/99 **************************************
      * Claim DARTS Control File
     FWMCXL     IF   E           K DISK
      *** END ADD ***** 08/12/99 **************************************
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************
      *
      *   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
      *

     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *   KEY LIST FOR THE CLAIM FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXENTD
     C                   KFLD                    XXFND
      *
      *
      *
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     CMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *** BEGIN ADD *** 08/31/99 **************************************
      *
      * Change variable to reflect a new logical (WMCML10) is being
      * used where the CMIDT is a key field instead of CMENTD
      *
     C     CMKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXIDT
     C                   KFLD                    XXFND
      *** END ADD ******** 08/31/99 ***********************************
      *** BEGIN ADD *** 08/12/99 **************************************
     C*
     C*  KEY LIST FOR THE CLAIMS/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** END ADD ***** 08/12/99 **************************************
      *
      *   KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C***  BEGIN ADD  ***  06/05/98  *********************************
     C                   KFLD                    XXDIV
     C***  END   ADD  ***  06/05/98  *********************************
      *
     C     *LIKE         DEFINE    CMENTD        X1FRDT
     C     *LIKE         DEFINE    CMENTD        X1TODT
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C***  BEGIN ADD  ***  06/05/98  *********************************
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C***  END   ADD  ***  06/05/98  *********************************
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMENTD        XXENTD
     C     *LIKE         DEFINE    CMIDT         XXIDT
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C***  BEGIN ADD  ***  08/31/99  *********************************
     C     *LIKE         DEFINE    CMIDT         XXFDAT
     C***  END   ADD  ***  08/31/99  *********************************
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND1            3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFND2            3
     C                   PARM                    QQFRYR            3
     C                   PARM                    QQTOYR            3
     C                   PARM                    QQFCAS            7
     C                   PARM                    QQTCAS            7
     C                   PARM                    QQSTAT            1
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQMODE            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND1        XXFND1            3 0
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C                   MOVE      QQTODT        XXTODT            8 0
     C                   MOVE      QQFND2        XXFND2            3 0
     C                   MOVE      QQFRYR        XXFRYR            3 0
     C                   MOVE      QQTOYR        XXTOYR            3 0
     C                   MOVE      QQFCAS        XXFCAS            7 0
     C                   MOVE      QQTCAS        XXTCAS            7 0
     C                   MOVE      QQSTAT        XXSTAT            1
      *
      *  CONVERT DATES TO CYMD FORMAT
      *
     C     XXTODT        IFNE      *ZEROS
      *
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FRDT
      *
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TODT
      *
     C                   ENDIF                                                  XXTODT NE 0
      *
     C                   MOVE      X1FRDT        XXENTD
      *** BEGIN ADD ****** 08/12/99 ***********************************
     C                   MOVE      X1FRDT        XXIDT
      *** END ADD ******** 08/12/99 ***********************************
      *================================================================
      * PROCESS ALL FUNDS
      *
     C     XXFND1        IFEQ      *ZERO
     C     X1TODT        ANDNE     *ZERO
      *
      *  READ FUND MASTER
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      FNFND         XXFND
      *** BEGIN ADD *** 08/12/99 **************************************
      *
      * Read file after getting new Group and initializing XXFND in
      * variable.  This read is used to get value of CXRLBL.  If
      * CXRLBL='Y' then 'Record Only' claim labels will also be
      * printed with Medical (CMLTFL='M') and Indemnity (CMLTFL='I')
      * claim labels
      *
     C     CXKY          CHAIN     PCX01                              60
      *** END ADD ***** 08/12/99 **************************************
      *
      *  READ THE CLAIM RECORDS
      *
      *** BEGIN DELETE *** 08/31/99 ***********************************
     C********** CMKY1     SETLLPCM01
     C********** CMKY      READEPCM01                    50
      *** END DELETE ***** 08/31/99 ***********************************
      *** BEGIN ADD ****** 08/31/99 ***********************************
     C     CXRLBL        IFEQ      'Y'
     C     CMKY4         SETLL     PCM01
     C     CMKY          READE     PCM01                                  50
     C                   MOVE      CMIDT         XXFDAT
     C                   ELSE
     C     CMKY1         SETLL     PCM03
     C     CMKY          READE     PCM03                                  50
     C                   MOVE      CMENTD        XXFDAT
     C                   ENDIF
      *** END ADD ******** 08/31/99 ***********************************
      *
     C     *IN50         DOWEQ     *OFF
      *** BEGIN CHANGE *** 08/12/99 ***********************************
     C********** CMENTD    ANDLEX1TODT
      *** BEGIN DLT *** 08/31/99 **************************************
     C********** CMIDT     ANDLEX1TODT
      *** END DLT ***** 08/31/99 **************************************
      *** END CHANGE ***** 08/12/99 ***********************************
      *** BEGIN ADD *** 08/31/99 **************************************
     C     XXFDAT        ANDLE     X1TODT
      *** END ADD ***** 08/31/99 **************************************
      *** BEGIN ADD *** 08/12/99 **************************************
      *
      * Check CXRLBL flag to see if 'Record Only' claim labels will be
      * printed for the specific claim along with Medical (CMLTFL='M')
      * and Indemnity (CMLTFL='I') claims
      *
      *** BEGIN DLT *** 08/31/99 **************************************
     C********** CXRLBL    IFEQ 'Y'
     C********** CMLTFL    OREQ 'M'
     C********** CMLTFL    OREQ 'I'
      *** END DLT ***** 08/31/99 **************************************
      *** END ADD ***** 08/12/99 **************************************
      *
      *  PRINT THE CLAIM LABEL
      *
     C     CMFND         IFEQ      XXFND
     C                   EXSR      SRLABL
     C                   ENDIF                                                  XXFND1=0
      *
      *** BEGIN DELETE *** 08/31/99 ***********************************
     C********** CMKY      READEPCM01                    50
      *** END DELETE ***** 08/31/99 ***********************************
      *** BEGIN ADD ****** 08/31/99 ***********************************
     C     CXRLBL        IFEQ      'Y'
     C     CMKY          READE     PCM01                                  50
     C                   MOVE      CMIDT         XXFDAT
     C                   ELSE
     C     CMKY          READE     PCM03                                  50
     C                   MOVE      CMENTD        XXFDAT
     C                   ENDIF
      *** END ADD ******** 08/31/99 ***********************************
      *
     C                   ENDDO                                                  *IN50 =*OFF TODT
      *
     C     FNKY          READE     PFN01                                  60
      *
     C                   ENDDO                                                  *IN60 =*OFF TODT
      *
     C                   ENDIF                                                  XXFND1=0
      *
      *================================================================
      *  PROCESS SELECTED FUND
      *
      *  READ THE CLAIM RECORDS BY INITIAL DATE
      *
     C     XXFND1        IFNE      *ZERO
     C     XXFND2        ORNE      *ZERO
      *
     C     XXFND1        IFNE      *ZERO
     C                   MOVE      XXFND1        XXFND
      *** BEGIN ADD *** 08/12/99 **************************************
      *
      * Read file after getting new Group and initializing XXFND in
      * variable.  This read is used to get value of CXRLBL.  If
      * CXRLBL='Y' then 'Record Only' claim labels will also be
      * printed with Medical (CMLTFL='M') and Indemnity (CMLTFL='I')
      * claim labels
      *
     C     CXKY          CHAIN     PCX01                              60
      *** END ADD ***** 08/12/99 **************************************
      *
      *** BEGIN CHANGE *** 08/31/99 ***********************************
     C     CXRLBL        IFEQ      'Y'
     C     CMKY4         SETLL     PCM01
     C     CMKY          READE     PCM01                                  60
     C                   MOVE      CMIDT         XXFDAT
     C                   ELSE
     C     CMKY1         SETLL     PCM03
     C     CMKY          READE     PCM03                                  60
     C                   MOVE      CMENTD        XXFDAT
     C                   ENDIF
      *** END CHANGE ***** 08/31/99 ***********************************
      *
     C     *IN60         DOWEQ     *OFF
      *** BEGIN CHANGE *** 08/12/99 ***********************************
     C********** CMENTD    ANDLEX1TODT
      *** BEGIN CHANGE *** 08/31/99 ***********************************
     C********** CMIDT     ANDLEX1TODT
     C     XXFDAT        ANDLE     X1TODT
      *** END CHANGE ***** 08/31/99 ***********************************
      *** END CHANGE ***** 08/12/99 ***********************************
      *** BEGIN DELETE *** 08/31/99 ***********************************
      *** BEGIN ADD *** 08/12/99 **************************************
      *
      * Check CXRLBL flag to see if 'Record Only' claim labels will be
      * printed for the specific claim along with Medical (CMLTFL='M')
      * and Indemnity (CMLTFL='I') claims
      *
     C********** CXRLBL    IFEQ 'Y'
     C********** CMLTFL    OREQ 'M'
     C********** CMLTFL    OREQ 'I'
      *** END ADD ***** 08/12/99 **************************************
     C*** END DELETE ** 08/31/99 **************************************
      *
     C     CMFND         IFEQ      XXFND1
      *
     C                   EXSR      SRLABL
      *
     C                   ENDIF
      *** BEGIN DELETE *** 08/31/99 ***********************************
      *** BEGIN ADD *** 08/12/99 **************************************
     C******************** ENDIF                           CXRLBL='Y' ?
      *** END ADD ***** 08/12/99 **************************************
     C*** END DELETE ** 08/31/99 **************************************
      *
      *** BEGIN DELETE *** 08/31/99 ***********************************
     C********** CMKY      READEPCM01                    60
      *** END DELETE ***** 08/31/99 ***********************************
      *** BEGIN ADD ****** 08/31/99 ***********************************
     C     CXRLBL        IFEQ      'Y'
     C     CMKY          READE     PCM01                                  60
     C                   MOVE      CMIDT         XXFDAT
     C                   ELSE
     C     CMKY          READE     PCM03                                  60
     C                   MOVE      CMENTD        XXFDAT
     C                   ENDIF
      *** END ADD ******** 08/31/99 ***********************************
      *
     C                   ENDDO
      *
      *  READ CLAIM RECORDS BY FYR, CASE, AND STATUS
      *
     C                   ELSE
      *
     C                   MOVE      XXFND2        XXFND
     C                   MOVE      XXFRYR        XXFYR
      *
     C     CMKY3         SETLL     PCM02
     C     CMKY2         READE     PCM02                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CMFYR         ANDLE     XXTOYR
      *
     C     CMCASE        IFGE      XXFCAS
     C     CMCASE        ANDLE     XXTCAS
      *
     C     XXSTAT        IFEQ      'B'
      *
     C     CMSTAT        IFEQ      'O'
     C     CMSTAT        OREQ      'C'
      *
     C                   EXSR      SRLABL
      *
     C                   ENDIF                                                  CMSTAT = O
      *
     C                   ELSE
      *
     C     CMSTAT        IFEQ      XXSTAT
     C                   EXSR      SRLABL
     C                   ENDIF
      *
     C                   ENDIF                                                  XXSTAT = B
      *
     C                   ENDIF                                                  CMCASE > XXFCAS
      *
     C     CMKY2         READE     PCM02                                  60
      *
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  XXFND1 NE 0
      *
     C                   SETON                                        LR
      *================================================================
      *  SUBROUTINE LABL - PRINTS CLAIMS LABELS
      *================================================================
     C     SRLABL        BEGSR
      *
      *  MOVE SS#, FND, AND EMP# TO KEY FIELDS
      *
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMEMP#        XXEMP#
     C***  BEGIN ADD  ***  06/05/98  *********************************
     C                   Z-ADD     CMDIV         XXDIV
     C***  END   ADD  ***  06/05/98  *********************************
      *
      *  GET THE CLAIMANT RECORD
      *
     C     CDKY          CHAIN     PCD01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
      *
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
      *
     C                   MOVEL     QQMID         QQFMN
      *
     C                   MOVE      '1'           QQTERM
     C                   MOVE      *BLANKS       QQLSN            56
     C                   MOVEL     CDLSN         QQLSN
      *
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
      *
     C                   MOVEL     QQLSN         XXCNAM           30
      *
     C                   ELSE
     C                   MOVEL     '********'    XXCNAM           30
     C                   ENDIF                                                  *IN60=*OFF
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXCNAM
     C                   MOVEL(P)  XXATTX        XXCNAM
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   Z-ADD     CMACDT        SRDTI
      *** BEGIN CHANGE *** 09/15/99 ***********************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C******************** MOVELSRDTO     XXDATE  60
     C                   MOVEL     SRDTO         XXDATE            8 0
      *
     C******************** Z-ADDCMFYR     X1FYR   20
      *
      *** END CHANGE ***** 09/15/99 ***********************************
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXNAME           40
      *
     C                   ELSE
     C                   MOVE      *BLANKS       XXNAME
     C                   ENDIF                                                  *IN60 IFEQ 0
      *
     C                   EXCEPT    LABEL
      *
     C                   ENDSR
      *================================================================
     OQSYSPRT   E            LABEL       1  1
      *** BEGIN CHANGE *** 09/15/99 ***********************************
     O************************ XXDATEY   10
     O                       XXDATE        Y     12
      *** END CHANGE ***** 09/15/99 ***********************************
     O          E            LABEL          1
     O**** BEGIN CHG **** 12/22/98 ************************************
     O*******                  XXCNAM    35
     O******* E  1             LABEL
     O*** Begin DELETE *** 02/05/98 ***********************************
     O******* E  3             LABEL
     O*** End DELETE ***** 02/05/98 ***********************************
     O*******                  CMFND      8
     O*******                             9 '-'
     O*******                  X1FYR     11
     O*******                            12 '-'
     O*******                  CMEMP#    21
     O*******                            22 '-'
     O*******                  CMDIV     27
     O*******                            28 '-'
     O*******                  CMCASE    35
     O******* E  2             LABEL
     O*******                  XXNAME    45
     O                       XXCNAM              32
     O          E            LABEL          1
     O                       CMFND                5
     O                                            6 '-'
      *** BEGIN CHANGE *** 09/15/99 ***********************************
     O************************ X1FYR      8
     O************************            9 '-'
     O************************ CMEMP#    18
     O************************           19 '-'
     O************************ CMDIV     24
     O************************           25 '-'
     O************************ CMCASE    32
     O                       CMFYR                9
     O                                           10 '-'
     O                       CMEMP#              19
     O                                           20 '-'
     O                       CMDIV               25
     O                                           26 '-'
     O                       CMCASE              33
      *** END CHANGE ***** 09/15/99 ***********************************
     O          E            LABEL          2
     O                       XXNAME              42
     O**** BEGIN CHG **** 12/22/98 ************************************
