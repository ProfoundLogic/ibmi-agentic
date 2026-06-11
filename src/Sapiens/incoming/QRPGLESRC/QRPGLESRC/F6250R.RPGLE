     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F6250R - EDI 837 LOG API
     H*
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM DETERMINES WHETHER AN EDI  *
     H*         LOG RECORD SHOULD BE WRITTEN AND IF SO, WHAT TYPE OF  *
     H*         TRANSACTION IT SHOULD BE.  THIS PROGRAM ALSO CREATES  *
     H*         AND COMPLETES THE EDI DIARIES AS NECESSARY.           *
     H*                                                               *
     H*   Called from:  W6251R                                        *
     H*                 F6251R                                        *
     H*                                                               *
     H* Created: 06/04/99    C:15     Project: KY010                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  03/25/14  C:212 Project: MC376                               *
     H*                  ANSI X12 Medical EDI : set maintenance type  *
     H*                  code based on STSTL                          *
     H*                                                               *
     H*  05/24/07  C:182 Project: MC281                               *
     H*                  Made changes for creating log records for    *
     H*                  bill with paid = 0                           *
     H*                                                               *
     H*  11/02/06  C:79  Support: 55605                               *
     H*                  Remove subscriber from the claim admin clm#  *
     H*                  Do not send Agency Claim Number              *
     H*                                                               *
     H*  10/04/06  C:79  Project: 25MNZ                               *
     H*                  Use recon in chaining to FELGP. Some recons  *
     H*                  were being skipped.                          *
     H*                                                               *
     H*  10/28/04  C:15  Support: MC072/44295                         *
     H*                  Added code to allow the '00' transaction to  *
     H*                  be resent if the original was rejected.      *
     H*                                                               *
     H*  11/06/01  C:053 SUPPORT MC048 # 29527                        *
     H*                  ADD INITIAL FILE FIELDS                      *
     H*                                                               *
     H*  02/07/00  C:062 PROJECT: Y2K                                 *
     H*                  CHANGE WCMS DATES TO YYMD.                   *
     H*                                                               *
     H* 02/04/00 C:62  SUPPORT MC044 #21354                           *
     H*                CHANGE EAWC DATE FIELDS.                       *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*
     FFMFNL     IF   E           K DISK
     F*****FDF2L   IF  E           K        DISK
     FFEEXL     IF   E           K DISK
     FFELGL1    UF A E           K DISK
     FFMCML1    IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     F*** BEG ADD *** 10/04/06 ****************************************
     FFELGL4    IF   E           K DISK    rename(plg01:plg02)
     F*** END ADD *** 10/04/06 ****************************************
     F***** beg add *** 03/25/14 **************************************
     FSTSTL     IF   E           K DISK
     F***** end add *** 03/25/14 **************************************
     F*

     D*________________________________________________________________
     D DN              S            150    DIM(999)
     D*________________________________________________________________
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D*** BEG DEL *** 11/02/06 ****************************************
     D***  XXCAC#                 1     25
     D***  XXCO#                  1      3  0
     D***  XXSUB#                 4      8  0
     D***  XXFND                  9     11  0
     D***  XXFYR                 12     14  0
     D***  XXCASE                15     21  0
     D*** END DEL *** 11/02/06 ****************************************
     D*** BEG ADD *** 11/02/06 ****************************************
     D  XXCAC#                 1     25
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D*** END ADD *** 11/02/06 ****************************************
     D*
     D***** beg add *** 03/25/14 **************************************
     D kkkey           s                   like(stkey) inz
     D***** end add *** 03/25/14 **************************************

     C*================================================================
     C*  FMFNL - Fund Master
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  FEEXL - EDI Control File
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTSET
     C*
     C*  FMCML1 - Claim Master
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  FELGL - EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEOB#
     C*** BEG ADD *** 10/04/06 ****************************************
     C*
     C*  FELGL4 - EDI Log File
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*** END ADD *** 10/04/06 ****************************************
     C*
     C*  FAEOL - EOMB Master File
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*  FAEAL - EOMB Master File
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            COMPANY
     C                   PARM                    QQSUB#            5            SUBSCRIBER #
     C                   PARM                    QQFND             3            GROUP #
     C                   PARM                    QQFYR             3            POLICY YR
     C                   PARM                    QQCASE            7            CASE/CLAIM
     C                   PARM                    QQEOB#            9            EOMB#
     C                   PARM                    QQRCN             3            RECON #
     C                   PARM                    QQACTF            1            ACTION FLAG
     C                   PARM                    QQRTCD            1
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
     C*** BEGIN ADD *** 02/07/00 **************************************
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** ENDIN ADD *** 02/07/00 **************************************
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    LGMTC         XXMTC
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C     *LIKE         DEFINE    CMAST         XXJRST
     C     *LIKE         DEFINE    *IN99         L@FROI
     C     *LIKE         DEFINE    LGCSDT        SVCSDT
     C     *LIKE         DEFINE    LGCSDT        XXDATE
     C     *LIKE         DEFINE    LGCSDT        XXWKDT
     C     *LIKE         DEFINE    LGLSEQ        XXLSEQ
     C*** BEG ADD *** 11/02/06 ****************************************
     C     *LIKE         DEFINE    LGSUB#        XXSUB#
     C*** END ADD *** 11/02/06 ****************************************
     C                   Z-ADD     0             XXRCN             3 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQEOB#        XXEOB#
     C                   MOVE      QQRCN         XXRCN
     C*
     C*  Retrieve group and group detail records
     C*
     C     FNKY          CHAIN     PFN01                              99
     C   99              GOTO      TERM
     C********** FNKY      CHAINPF201                99
     C***99*****           GOTO TERM
     C*
     C*  Retrieve claim record
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C********** CMAST     OREQ *BLANK
     C                   GOTO      TERM
     C                   ENDIF
     C******************** MOVELCMAST     XXJRST
     C*
     C*  If the jurisdiction state is 'OH', do not create a log record
     C*    unless the Group is state reporting.
     C*
     C*********  CMAST     IFEQ 'OH'
     C*********  F2TYPE    ANDNE'ST'
     C*********            GOTO TERM
     C*********            ENDIF
     C*
     C*  Retrieve EOMB records
     C*
     C     EOKY          CHAIN     PEO01                              99
     C   99              GOTO      TERM
     C                   MOVEL     EOAST         XXJRST
     C*
     C     EAKY          CHAIN     PEA01                              99
     C***99                GOTO TERM
     C*
     C*  Retrieve the EDI Master control record.
     C*
     C                   MOVE      *BLANKS       XXTSET
     C     EXKY          CHAIN     PEX01                              99
     C   99              GOTO      TERM
     C                   MOVE      EXCSDT        SVCSDT
     C*
     C*  Retrieve the EDI 837 control record.
     C*
     C                   MOVE      '837'         XXTSET
     C     EXKY          CHAIN     PEX01                              99
     C   99              GOTO      TERM
     C*
     C*  Determine if the original 837 transaction has been sent.
     C*  If an open transaction already exists, exit the pgm.
     C*
     C                   MOVE      *OFF          L@FROI
     C*** BEG CHG *** 10/04/06 ****************************************
     C********** LGKY      CHAINPLG01               N99
     C     LGKY1         CHAIN     PLG02                              99
     C*** END CHG *** 10/04/06 ****************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     LGTSET        IFEQ      XXTSET
     C*
     C     LGMTC         IFEQ      '00'
     C     LGSTS         ANDNE     'EI'
     C     LGSTS         ANDNE     'EC'
     C     LGSTS         ANDNE     'TR'
     C                   MOVE      *ON           L@FROI
     C                   ENDIF
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'EX'
     C     LGSTS         OREQ      'SN'
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEG CHG *** 10/04/06 ****************************************
     C********** LGKY      READEPLG01               N    99
     C     LGKY1         READE     PLG02                                  99
     C*** END CHG *** 10/04/06 ****************************************
     C                   ENDDO
     C*
     C*  If the EOMB entry date is less than the EDI start date, seton
     C*    L@FROI to indicate that the EOMB has been reported to the
     C*    state.
     C*
     C     EAWCPD        SUB       19000000      XXWKDT
     C*
     C     EOIDT         IFLT      SVCSDT
     C     EXLOG         ANDEQ     '1'
     C     XXWKDT        ORLT      SVCSDT
     C     EXLOG         ANDEQ     '2'
     C*** BEG ADD ***** 05/24/07 **************************************
     C     EAPAID        ANDGT     0
     C*** END ADD ***** 05/24/07 **************************************
     C                   MOVE      *ON           L@FROI
     C                   ENDIF
     C*
     C     L@FROI        IFEQ      *ON
     C     QQACTF        ANDEQ     *BLANKS
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  Determine the Maint Type Code
     C*
     C                   MOVE      *BLANKS       XXMTC
     C                   SELECT
     C*
     C     QQACTF        WHENEQ    *BLANKS                                      Original
     C                   MOVEL     '00'          XXMTC
     C*
     C     QQACTF        WHENEQ    'C'                                          Correction
     C                   MOVEL     '05'          XXMTC
     C*
     C     QQACTF        WHENEQ    'D'                                          Cancel
     C                   MOVEL     '01'          XXMTC
     C*
     C                   ENDSL
     C*
     C*** Begin add *** 10/28/04 **************************************
     C     L@FROI        IFEQ      *OFF
     C     XXMTC         ANDEQ     '05'
     C                   MOVEL     '00'          XXMTC
     C                   ENDIF
     C*** End add ***** 10/28/04 **************************************
     C***** beg add *** 03/25/14 **************************************
      /free
        kkkey = xxjrst;
        chain(n) kkkey ststl;
        if %found(ststl) and
           (stedirel = '005010|20');
           if xxmtc = '05';
              xxmtc = '02';
           endif;
        endif;
      /end-free
     C***** end add *** 03/25/14 **************************************
     C*
     C*  Create the 837 log record.
     C*
     C                   EXSR      SRLOG                                        Add/updt log
     C*
     C*  End the program.
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  SRLOG - Create the log history record.
     C*****************************************************************
     C     SRLOG         BEGSR
     C*
     C*  First, retrieve the next available seq#.
     C*
     C     LGKY          SETGT     PLG01
     C     LGKY          READPE(N) PLG01                                  99
     C  N99LGLSEQ        ADD       1             XXLSEQ
     C   99              Z-ADD     1             XXLSEQ
     C*
     C*  Load the Log History fields
     C*
     C                   CLEAR                   PLG01
     C                   Z-ADD     XXCO#         LGCO#
     C                   Z-ADD     XXSUB#        LGSUB#
     C                   Z-ADD     XXFND         LGFND
     C                   Z-ADD     XXFYR         LGFYR
     C                   Z-ADD     XXCASE        LGCASE
     C                   Z-ADD     XXEOB#        LGEOB#
     C                   Z-ADD     XXLSEQ        LGLSEQ
     C*
     C                   Z-ADD     XXRCN         LGRCN
     C                   MOVE      *BLANKS       LGBCH#
     C                   Z-ADD     CMEMP#        LGEMP#
     C                   Z-ADD     CMDIV         LGDIV
     C                   MOVE      XXCAC#        LGCAC#
     C*** BEG DEL *** 11/02/06 ****************************************
     C****************   MOVE      CMSCL#        LGSCL#
     C*** END DEL *** 11/02/06 ****************************************
     C                   Z-ADD     EOFEIN        LGFEIN
     C                   Z-ADD     EOMOD         LGMOD
     C                   MOVE      EOIUSR        LGEIUS
     C*
     C                   MOVE      XXTSET        LGTSET
     C                   MOVE      XXMTC         LGMTC
     C                   MOVE      XXJRST        LGJST
     C                   MOVE      'EC'          LGSTS
     C                   Z-ADD     0             LGEXDT
     C                   Z-ADD     0             LGOTDT
     C                   Z-ADD     0             LGOTTM
     C                   Z-ADD     0             LGARCV
     C                   MOVE      *BLANKS       LGTFLG
     C                   MOVE      *BLANKS       LGIVER
     C                   MOVE      *BLANKS       LGEFT#
     C*
     C*  Load the Submission Due Date
     C*
     C                   SELECT
     C     EXLOG         WHENEQ    '1'                                          Date bill entered
     C                   Z-ADD     EOIDT         SRDTI
     C*** BEGIN ADD *** 02/04/00 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*** END   ADD *** 02/04/00 **************************************
     C     EXLOG         WHENEQ    '2'                                          Date pmt processed
     C*** BEGIN CHG *** 02/04/00 **************************************
     C**********           Z-ADDEAWCPD    SRDTI
     C*** BEGIN CHG *** 02/07/00 **************************************
     C**********           Z-ADDEAWCPD    SRDATE
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SRDATE
     C*** END   CHG *** 02/07/00 **************************************
     C*** END   CHG *** 02/04/00 **************************************
     C                   OTHER                                                  Default
     C                   Z-ADD     XXDATE        SRDTI                          system date
     C*** BEGIN ADD *** 02/04/00 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C*** END   ADD *** 02/04/00 **************************************
     C                   ENDSL
     C*
     C*** BEGIN DEL *** 02/04/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SRDATE  80
     C*** END   DEL *** 02/04/00 **************************************
     C                   Z-ADD     EXSDUE        SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C     SRERR         IFNE      '1'
     C                   Z-ADD     SRDATE        SRDTI
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI                          system date
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C*
     C     L@FROI        IFEQ      *OFF
     C     LGMTC         ANDEQ     '01'
     C                   MOVEL     'I'           LGSTAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        LGUSER
     C                   Z-ADD     XXDATE        LGDT
     C                   TIME                    LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C     LGSTAT        IFEQ      'I'
     C                   DELETE    PLG01
     C                   ELSE
     C*
     C*** BEG ADD *** 11/06/01 *********************************
     C                   MOVEL     Q1USER        LGIUSR
     C                   Z-ADD     XXDATE        LGIDT
     C                   TIME                    LGITIM
     C                   MOVEL     Q1PGM         LGIPGM
     C*** END ADD *** 11/06/01 *********************************
     C*
     C                   WRITE     PLG01
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
