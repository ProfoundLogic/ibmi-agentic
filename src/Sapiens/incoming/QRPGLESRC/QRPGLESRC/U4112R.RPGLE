     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* ***************************************************************
     H/TITLE U4112R - PRINT PRE-CERT DEPT LETTER & MAINTAIN DARTS
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program reads file UXLXP1 created in the prior step.  It *
     H* then accesses other files to assign Fund, Employee, Claim     *
     H* data to the UXLCP1 record and updates the file prior to the   *
     H* PRTDOC command.  The program also writes a DARTS remark       *
     H* which points to the Claim letter that was generated.          *
     H* Also prior to the PRTDOC command, the document ID is checked  *
     H* to determine if an overlay is necessary and if so, an         *
     H* OVRPRTF command is executed.                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     U4111C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     *NONE                                                     *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*  PROGRAMMER: C:185                                            *
     H*                                                               *
     H*  MODIFIED:   12/15/06               PROJECT: 144MAH           *
     H*                                                               *
     H*                                                               *
     H*  PROGRAMMER: C:62                                             *
     H*                                                               *
     H*  CREATED:    12/11/98               PROJECT: MC184            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/03/14 C:283 Support: WC099 85252                           *
     H*                Correctly create the Command for SYPRTDOC.     *
     H*                                                               *
     H* 10/29/12 C:280 Project: 25WSJ                                 *
     H*                CHANGE TO USE SYPRTDOC                         *
     H*                REMOVED PRTDOC COMMAND                         *
     H*                                                               *
     H* 07/31/06 C:33 Project: 25INR                                  *
     H*               Implement DTM changes.                          *
     H*                                                               *
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H*                                                               *
     H* 11/19/02 C:62 PROJECT: 25MNE                                  *
     H*               REMOVE RATLTR PRINT ROUTINE.                    *
     H*                                                               *
     H* 10/01/99 C:62 PROJECT: MC087 19893                            *
     H*               BAD CONVERSION OF LCDIDT                        *
     H*                                                               *
     H* 05/26/99 C:62 PROJECT: MC145                                  *
     H*               LOAD DIDT IFNE *ZEROS.                          *
     H*                                                               *
     H* 03/24/99 C:56 PROJECT: MC194                                  *
     H*               Load the name and mailing address base on       *
     H*               DBA flag (FMPDP/PDDBA1)                         *
     H*                                                               *
     H* 03/23/99 C:62 PROJECT:MC025 17269                             *
     H*               PUT IN PLACE SPRTDOC2 FOR RATLTR'S.             *
     H*                                                               *
     H*****************************************************************
     FUXLCL     UF   E           K DISK
     FUXLCL11   UF   E           K DISK
     F                                     RENAME(PLC01:PLC02)
     FUDPOL     IF   E           K DISK
     FUXPOP     O    E             DISK
     F                                     RENAME(PPO01:PPO02)
     FUMCHL     IF   E           K DISK
     FUMPCP     IF   E             DISK
     FUMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FFMC1L     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PCS01)
     FSMCOL     IF   E           K DISK
     FSMDOL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK

     D*
     D CMD             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D CM2             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D OVR             S              1    DIM(75) CTDATA PERRCD(75)            OVRPRTF COMMAND
     D*** BEGIN DEL *** 03/23/99 **************************************
     D**********          CM3    75 225  1               QCMDEXC COMMAND
     D*** END   DEL *** 03/23/99 **************************************
     D FLR             S              1    DIM(80)                              FOLDER ARRAY
     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D FNB             S              1    DIM(40) ASCEND                       FUND NAME W/ BLANKS
     D FN              S              1    DIM(40)                              FUND NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D ED              S              8  0 DIM(5) ASCEND                        EFFECT. DATE
     D CD              S              8  0 DIM(5) ASCEND                        CANCEL DATE
     D DEN             S             60    DIM(16)                              DENIAL INFO
     D DT              S              8  0 DIM(16)                              WL/TP DT RANGES
     D*
     D***  Begin Add  ***  04/03/14  **************************************
     D XXlen           S              2  0
     D***  End   Add  ***  04/03/14  **************************************
     D***  Begin Add  ***  07/31/06  **************************************
     D JOBQNO          C                   CONST('JOBQ(*NO)')
     D***  End   Add  ***  07/31/06  **************************************
     D                 DS
     D  XXAGE                  1      7  0
     D  XXYR                   1      3  0
     D  XXMTH                  4      5  0
     D  XXDAY                  6      7  0
     D                 DS
     D  DC1                    1     25
     D                                     DIM(5)                               DIAG CODES - SCRN
     D  XXICD1                 1      5
     D  XXICD2                 6     10
     D  XXICD3                11     15
     D  XXICD4                16     20
     D  XXICD5                21     25
     D                 DS
     D  DS                     1    250
     D                                     DIM(5)                               DIAG CODES - DESC
     D  XXDGD1                 1     50
     D  XXDGD2                51    100
     D  XXDGD3               101    150
     D  XXDGD4               151    200
     D  XXDGD5               201    250
     D                 DS
     D  LOS                    1      5
     D                                     DIM(5)                               DIAG OR OPERATION
     D  XXLOS1                 1      1
     D  XXLOS2                 2      2
     D  XXLOS3                 3      3
     D  XXLOS4                 4      4
     D  XXLOS5                 5      5
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CM                     1    256
     D                                     DIM(256)                             DARTS COMMENTS
     D  ##COMM                 1    256
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CX                     1     30
     D  XXCLEX                 1     30
     D                 DS
     D  XXSS#                  1      9
     D  CFSS1                  1      3
     D  CFSS2                  4      5
     D  CFSS3                  6      9
     D                 DS
     D  AR                     1     75
     D                                     DIM(75)                              ADDT'L REMARKS
     D  ARCM75                 1     75
     D                 DS
     D  XXBLNK                 1      3
     D  XXDG1                  1      1
     D  XXDG2                  2      2
     D  XXDG3                  3      3
     D                 DS
     D  DC                     1     12
     D                                     DIM(12)                              CLDCID
     D  CLDCID                 1     12
     D                 DS
     D  FL                     1      8
     D                                     DIM(8)                               RECIPIENT FLAGS
     D  LCRCFL                 1      1
     D  LCRPFL                 2      2
     D  LCRTFL                 3      3
     D  LCRFFL                 4      4
     D  LCRSFL                 5      5
     D  LCRGFL                 6      6
     D  LCREFL                 7      7
     D  LCROFL                 8      8
     D*** BEGIN ADD *** 03/23/99 **************************************
     D                 DS
     D  XXRDOC                 1     12
     D  XXPERS                 1      3
     D  X1PERS                 1      1
     D  X2PERS                 2      2
     D  X3PERS                 3      3
     D  XXJDT                  4      6  0
     D  XXJYR                  7      8  0
     D  XXFILL                 9      9
     D  XXSEQ                 10     12  0
     D*** END   ADD *** 03/23/99 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 12/15/06 ****************************************
      *
     D                 DS
     D  PARA01                 1    210
     D  LC70A1                 1     70
     D  LC70A2                71    140
     D  LC70A3               141    210
     D                 DS
     D  PARA02                 1    210
     D  LC70A4                 1     70
     D  LC70A5                71    140
     D  LC70A6               141    210
     D                 DS
     D  PARA03                 1    210
     D  LC70A7                 1     70
     D  LC70A8                71    140
     D  LC70A9               141    210
     D                 DS
     D  PARA04                 1    210
     D  LC70AA                 1     70
     D  LC70AB                71    140
     D  LC70AC               141    210
     D                 DS
     D  PARA05                 1    210
     D  LC70AD                 1     70
     D  LC70AE                71    140
     D  LC70AF               141    210
     D                 DS
     D  PARA06                 1    210
     D  LC70AG                 1     70
     D  LC70AH                71    140
     D  LC70AI               141    210
     D                 DS
     D  PARA07                 1    210
     D  LC70AJ                 1     70
     D  LC70AK                71    140
     D  LC70AL               141    210
     D                 DS
     D  PARA08                 1    210
     D  LC70AM                 1     70
     D  LC70AN                71    140
     D  LC70AO               141    210
     D                 DS
     D  PARA09                 1    210
     D  LC70AP                 1     70
     D  LC70AQ                71    140
     D  LC70AR               141    210
     D                 DS
     D  PARA10                 1    210
     D  LC70AS                 1     70
     D  LC70AT                71    140
     D  LC70AU               141    210
      *

     C*** END ADD *** 12/15/06 ****************************************
     C*
     C*  COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    #1KEY
     C                   KFLD                    KKSEQ#            5 0
     C*
     C     C3KY2         KLIST
     C                   KFLD                    #1KEY
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C*   KEY LIST FOR THE PRE-CERT HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCERT
     C*
     C     CHKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C*  CLAIM MASTER FILE - SCREEN
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C*  DEPT/DOC MASTER FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    LCDPCD
      **** BEGIN CHG **** 04/03/14 *********************************************
     C***********        KFLD                    LCFLDR
     C                   KFLD                    XXfldr           80
      **** BEGIN CHG **** 04/03/14 *********************************************
     C                   KFLD                    LCDCID
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  DEPT LETTERS WORKFILE - SCREEN
     C*
     C     LCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C                   KFLD                    KKREQ#
     C*
     C     LCKY2         KLIST
     C                   KFLD                    LCCO#
     C                   KFLD                    LCSUB#
     C                   KFLD                    LCFND
     C                   KFLD                    LCFYR
     C                   KFLD                    LCCASE
     C                   KFLD                    LCCERT
     C                   KFLD                    LCEXT
     C                   KFLD                    LCREQ#
     C                   KFLD                    LCDPCD
     C                   KFLD                    LCFLDR
     C                   KFLD                    LCDCID
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*   KEY LIST FOR PROVIDER AUXILIARY FILE.
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*   KEY LIST FOR THE PRE-CERTIFICATION MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCERT
     C*
     C*   KEY LIST FOR PROVIDER MASTER FILE.
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*  PROCEDURE CODE WORKFILE - SCREEN
     C*
     C     POKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C*  SUBSCRIBER STAFF CODE FILE
     C*
     C     SFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKSTAF
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*  KEY LIST FOR SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*  USER PARM FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKSTAF
     C*
     C     *LIKE         DEFINE    LCCO#         KKCO#
     C     *LIKE         DEFINE    LCSUB#        KKSUB#
     C     *LIKE         DEFINE    LCFND         KKFND
     C     *LIKE         DEFINE    LCFYR         KKFYR
     C     *LIKE         DEFINE    LCCASE        KKCASE
     C     *LIKE         DEFINE    LCCERT        KKCERT
     C     *LIKE         DEFINE    LCEXT         KKEXT
     C     *LIKE         DEFINE    LCDPCD        KKDPCD
     C     *LIKE         DEFINE    LCFLDR        KKFLDR
     C     *LIKE         DEFINE    LCDCID        KKDCID
     C     *LIKE         DEFINE    LCSS#         KKSS#
     C     *LIKE         DEFINE    LCREQ#        KKREQ#
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    CMDT          KKDATE
     C     *LIKE         DEFINE    CMDT          XXDSDT
     C     *LIKE         DEFINE    LCTVST        XXTVST
     C     *LIKE         DEFINE    LCTRDY        XXTRDY
     C     *LIKE         DEFINE    LCTCDY        XXTCDY
     C     *LIKE         DEFINE    LCTADY        XXTADY
     C     *LIKE         DEFINE    LCPFDT        XXLOW
     C     *LIKE         DEFINE    LCPLDT        XXHIGH
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN            15 5
     C                   PARM                    @@REQ#           15 5
     C                   PARM                    @@DATE            8
     C*
     C                   Z-ADD     @@RRN         ##RRN             9 0
     C                   Z-ADD     @@REQ#        ##REQ#            5 0
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
     C     DTJUL         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*JUL'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD *** 12/15/06 ****************************************
     C*
     C     CERTLT        PLIST
     C                   PARM                    KKCO@
     C                   PARM                    KKFND@
     C                   PARM                    KKSUB@
     C                   PARM                    ##FLR
     C                   PARM                    LCEMNM
     C                   PARM                    ##DCID
     C                   PARM                    LCDCID
     C                   PARM                    LC#CPY
     C                   PARM                    LCRRN2
     C                   PARM                    LCRRN1
     C                   PARM                    LCRRA1
     C                   PARM                    LCRRA2
     C                   PARM                    LCRRCT
     C                   PARM                    LCCLNM
     C                   PARM                    LCSS#
     C                   PARM                    LCACDT
     C                   PARM                    LCSUB#
     C                   PARM                    LCFND
     C                   PARM                    LCFYR
     C                   PARM                    LCCASE
     C                   PARM                    LCCERT
     C                   PARM                    LCLIAB
     C                   PARM                    LCICD1
     C                   PARM                    LCIDS1
     C                   PARM                    LC40A1
     C                   PARM                    LCTPNM
     C                   PARM                    LC40A2
     C                   PARM                    LCCANM
     C                   PARM                    @@CLID
     C                   PARM                    PARA01
     C                   PARM                    PARA02
     C                   PARM                    PARA03
     C                   PARM                    PARA04
     C                   PARM                    PARA05
     C                   PARM                    PARA06
     C                   PARM                    PARA07
     C                   PARM                    PARA08
     C                   PARM                    PARA09
     C                   PARM                    PARA10
     C*
     C*** END ADD *** 12/15/06 ****************************************
     C*
     C*** ADD ADD *** 12/15/06 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLID            3
     C*** END ADD *** 12/15/06 ****************************************
     C*
     C     ##RRN         CHAIN     PPC01                              99
     C*
     C                   Z-ADD     PCCO#         KKCO#
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  PCCERT        KKCERT
     C                   Z-ADD     PCEXT         KKEXT
     C                   Z-ADD     ##REQ#        KKREQ#
     C*
     C                   MOVE      @@DATE        SRDTI                          MDCY --> CYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     *HIVAL        KKSEQ#
     C*
     C*** BEG ADD *** 12/15/06 ****************************************
     C                   MOVE      KKCO#         KKCO@             2
     C                   MOVE      KKFND         KKFND@            3
     C                   MOVE      KKSUB#        KKSUB@            5
     C*** END ADD *** 12/15/06 ****************************************
     C*
     C*
     C*   READ THE WORK FILE
     C*
     C     LCKY          CHAIN(N)  PLC02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      UPDATE
     C                   Z-ADD     1             CON               2 0
     C*
     C     CON           DOWLE     8
     C*
     C     FL(CON)       IFEQ      '1'
     C*
     C                   EXSR      RECIP
      *
     C*** BEG DEL *** 12/15/06 ****************************************
     C************         EXSR PRTDOC
     C*** END DEL *** 12/15/06 ****************************************
      *
     C*** BEG ADD *** 12/15/06 ****************************************
     C     @@CLID        IFEQ      '144'
     C                   CALL      'U4113C'      CERTLT
     C                   ELSE
     C                   EXSR      PRTDOC
     C                   ENDIF
     C*** END ADD *** 12/15/06 ****************************************
     C*
     C                   EXSR      SRDART
     C*
     C                   ENDIF
     C*
     C                   ADD       1             CON
     C*
     C                   ENDDO
     C*
     C*  READ NEXT LETTER REQUEST RECORD
     C*
     C     LCKY          READE(N)  PLC02                                  99
     C*
     C                   ENDDO                                                  *IN99
     C*
     C*  END OF PROGRAM
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*   MVINFO - MOVE INFORMATION TO UXLCP AND UXLCP1 AND UXPOP     *
     C*****************************************************************
     C     MVINFO        BEGSR
     C*
     C                   Z-ADD     PCCO#         LCCO#
     C                   Z-ADD     PCSUB#        LCSUB#
     C                   Z-ADD     PCFND         LCFND
     C                   Z-ADD     PCFYR         LCFYR
     C                   Z-ADD     PCCASE        LCCASE
     C                   MOVE      PCCERT        LCCERT
     C                   Z-ADD     PCEXT         LCEXT
     C*
     C     COKY          CHAIN     PCO01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C     C1KY          CHAIN     PC101                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVE      C1NAME        LCCNME
     C                   MOVEL     C1ADR1        LCCAD1
     C                   MOVEL     C1ADR2        LCCAD2
     C                   MOVEL     C1ST          XXST              2
     C                   Z-ADD     C1ZIP         XXZIP
     C                   MOVEL     C1CTY         XXCTY            25
     C*
     C                   EXSR      SRADDR
     C*
     C*** BEG ADD *** 10/20/03 *****************************
     C     CDCTRY        IFNE      *BLANKS
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        XXCSZ
     C                   MOVE      CDCTRY        XXCSZ
     C                   END
     C*** END ADD *** 10/20/03 *****************************
     C*
     C                   MOVEL(P)  XXCSZ         LCCCSZ
     C                   Z-ADD     C1AREA        LCCARA
     C                   Z-ADD     C1PHN#        LCCPHN
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      PCCERT        LCCERT
     C*
     C                   MOVE      PCSTAT        LCSTAT
     C                   MOVE      PCLIAB        LCLIAB
     C                   Z-ADD     PCADDT        LCADDT
     C                   Z-ADD     PCRVDT        LCRVDT
     C                   MOVEL     PCPASG        LCPASG
     C                   Z-ADD     PCREQD        LCREQD
     C                   Z-ADD     PCCERD        LCCERD
     C                   Z-ADD     PCRECD        LCRECD
     C                   MOVEL     PC10A1        LCPLSV
     C                   Z-ADD     PCENTD        LCENTD
     C                   MOVEL(P)  PCADST        LCADST
     C*
     C                   MOVEL(P)  PCADST        KKKEY
     C                   MOVEL(P)  'ADST'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              98
     C*
     C  N99              MOVEL(P)  GTDESC        LCASDS
     C   99              MOVEL(P)  *BLANKS       LCASDS
     C*
     C                   MOVEL(P)  PCTYAD        LCTYAD
     C*
     C                   MOVEL(P)  PCTYAD        KKKEY
     C                   MOVEL(P)  'TYAD'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              98
     C*
     C  N99              MOVEL(P)  GTDESC        LCTADS
     C   99              MOVEL(P)  *BLANKS       LCTADS
     C*
     C                   MOVEL(P)  PCPARF        LCPARF
     C                   MOVEL(P)  PC2OPN        LC2OPN
     C                   Z-ADD     PC2DT         LC2DT
     C                   Z-ADD     PC2PHY        LC2PHY
     C                   Z-ADD     PC2PHM        LC2PHM
     C                   MOVE      PC2CSC        LC2CSC
     C                   MOVE      PCIME         LCIME
     C                   Z-ADD     PCIMDT        LCIMDT
     C                   Z-ADD     PCIPHY        LCIPHY
     C                   Z-ADD     PCIPHM        LCIPHM
     C                   MOVE      PCICSC        LCICSC
     C                   MOVE      PCRINJ        LCRINJ
     C                   MOVE      PCHMHL        LCHMHL
     C                   Z-ADD     *HIVAL        XXLOW
     C                   Z-ADD     *ZERO         XXHIGH
     C                   Z-ADD     *ZERO         XXTVST
     C*
     C     POKY          CHAIN     PPO01                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C     POFRDT        IFLT      XXLOW
     C     POFRDT        ANDNE     *ZERO
     C*
     C                   Z-ADD     POFRDT        XXLOW
     C*
     C                   ENDIF
     C*
     C     POTODT        IFGT      XXHIGH
     C     POTODT        ANDNE     *ZERO
     C*
     C                   Z-ADD     POTODT        XXHIGH
     C*
     C                   ENDIF
     C*
     C                   ADD       POVST         XXTVST
     C*
     C                   Z-ADD     LCREQ#        POREQ#
     C                   MOVEL(P)  LCDPCD        PODPCD
     C                   MOVEL(P)  LCFLDR        POFLDR
     C                   MOVEL(P)  LCDCID        PODCID
     C*
     C                   WRITE     PPO02
     C*
     C     POKY          READE     PPO01                                  98
     C*
     C                   ENDDO
     C*
     C     XXLOW         IFEQ      *HIVAL
     C*
     C                   Z-ADD     *ZERO         XXLOW
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     XXLOW         LCPFDT
     C                   Z-ADD     XXHIGH        LCPLDT
     C                   Z-ADD     XXTVST        LCTVST
     C*
     C* GET PROVIDER NAME, ADDRESS
     C*
     C                   MOVE      PCFEIN        LCFEIN
     C                   MOVE      PCMOD         LCMOD
     C                   Z-ADD     PCFEIN        KKFEIN
     C                   Z-ADD     PCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        LCPAD1
     C                   MOVE(P)   *BLANKS       LCPAD2
     C                   MOVEL(P)  PDADR2        LCRPA1
     C                   MOVE      *BLANKS       LCRPA2
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C                   MOVEL(P)  PDADR1        LCPAD1
     C                   MOVEL(P)  PDADR2        LCPAD2
     C                   MOVEL(P)  PDADR1        LCRPA1
     C                   MOVEL(P)  PDADR2        LCRPA2
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = Y
     C*** END ADD *** 03/24/99 ****************************************
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVEL(P)  PDST          XXST
     C                   MOVEL(P)  PDST          LCRPST
     C                   Z-ADD     PDZIP         XXZIP
     C                   Z-ADD     PDZIP         LCRPZP
     C                   MOVEL(P)  PDCTY         XXCTY            25
     C                   MOVEL(P)  PDCTY         LCRPCT
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCPCSZ
     C                   MOVEL(P)  XXCSZ         LCPCSZ
     C                   Z-ADD     PDAREA        LCPARA
     C                   Z-ADD     PDPHN#        LCPPHN
     C                   MOVE      PDSPCD        LCPSPC
     C*
     C* COMBINE PROVIDER NAME INTO FIRST LAST ORDER
     C*
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        LCPNME
     C                   MOVEL(P)  PDADR1        LCRPNM
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C     PDFSN         IFNE      *BLANKS
     C*
     C     PDFSN         CAT(P)    PDNAME:1      LCPNME
     C                   MOVEL(P)  LCPNME        LCRPNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  PDNAME        LCPNME
     C                   MOVEL(P)  PDNAME        LCRPNM
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = Y
     C*** END ADD *** 03/24/99 ****************************************
     C*
     C* GET PROVIDER SPLTY CODE DESCRIPTION
     C*
     C                   MOVEL(P)  PDSPCD        LCPSPC
     C                   MOVEL(P)  PDSPCD        KKKEY
     C*
     C     TBKY          CHAIN     PPS01                              98
     C*
     C  N99              MOVEL(P)  PSDESC        LCSPDC
     C   99              MOVEL(P)  *BLANKS       LCSPDC
     C*
     C                   ENDIF
     C*
     C* GET DIAGNOSIS CODE AND DESCRIPTION
     C*
     C                   MOVEL     PCICD1        XXICD1
     C                   MOVEL     PCICD2        XXICD2
     C                   MOVEL     PCICD3        XXICD3
     C                   MOVEL     PCICD4        XXICD4
     C                   MOVEL     PCICD5        XXICD5
     C                   MOVEL     PCLOS1        XXLOS1
     C                   MOVEL     PCLOS2        XXLOS2
     C                   MOVEL     PCLOS3        XXLOS3
     C                   MOVEL     PCLOS4        XXLOS4
     C                   MOVEL     PCLOS5        XXLOS5
     C*
     C                   DO        5             X                 2 0
     C*
     C                   MOVEL(P)  DC1(X)        KKICD9
     C                   MOVEL(P)  LOS(X)        KKLOST
     C*
     C     DIKY          SETGT     PDI01
     C*
     C     NXTDC2        TAG
     C*
     C                   READP     PDI01                                  98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C     DISTAT        IFNE      'I'
     C*
     C                   MOVEL     DIDSC1        DS(X)
     C*
     C                   ELSE
     C*
     C                   GOTO      NXTDC2
     C*
     C                   ENDIF                                                  END DISTAT
     C*
     C                   ELSE                                                   *IN98 = 1
     C*
     C                   MOVE      *BLANK        DS(X)
     C*
     C                   ENDIF                                                  END *IN98
     C*
     C                   ENDDO                                                  END DO
     C*
     C                   MOVEL     XXICD1        LCICD1
     C                   MOVEL     XXDGD1        LCIDS1
     C                   MOVEL     XXICD2        LCICD2
     C                   MOVEL     XXDGD2        LCIDS2
     C                   MOVEL     XXICD3        LCICD3
     C                   MOVEL     XXDGD3        LCIDS3
     C                   MOVEL     XXICD4        LCICD4
     C                   MOVEL     XXDGD4        LCIDS4
     C                   MOVEL     XXICD5        LCICD5
     C                   MOVEL     XXDGD5        LCIDS5
     C                   MOVEL     XXLOS1        LCLOS1
     C                   MOVEL     XXLOS2        LCLOS2
     C                   MOVEL     XXLOS3        LCLOS3
     C                   MOVEL     XXLOS4        LCLOS4
     C                   MOVEL     XXLOS5        LCLOS5
     C*
     C* GET CLAIMANT NAME AND ADDRESS
     C*
     C     CMKY          CHAIN     PCM01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVE      CMSS#         LCSS#
     C                   Z-ADD     CMACDT        LCACDT
     C                   MOVEL     CMCCL#        LCCCL#
     C                   MOVEL     CMSCL#        LCSCL#
     C                   MOVEL     CMSS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C* GET AGE FROM CURRENT DATE MINU CLAIMANT BIRTH DATE
     C*
     C     CDDTOB        IFNE      *ZEROS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C     XXDSDT        SUB       CDDTOB        XXAGE
     C                   Z-ADD     XXYR          LCAGE
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        LCAGE
     C*
     C                   ENDIF
     C*
     C     CDFSN         IFNE      *BLANKS
     C*
     C     CDFSN         CAT(P)    CDLSN:1       LCCLNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CDLSN         LCCLNM
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  END *IN98
     C*
     C                   ENDIF                                                  END *IN98
     C*
     C* GET HISTORY DATES FROM PRE-CERT HISTORY FILE
     C*
     C     CHKY          CHAIN     PCH01                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C     CHOCST        IFEQ      'O'
     C*
     C                   Z-ADD     CHEFDT        LCODT
     C*
     C                   ELSE
     C*
     C     CHOCST        IFEQ      'C'
     C*
     C                   Z-ADD     CHEFDT        LCCDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CHKY          READE     PCH01                                  98
     C*
     C                   ENDDO
     C*
     C     CHKY1         CHAIN     PCH01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CHOCST        IFEQ      'R'
     C*
     C                   Z-ADD     CHEFDT        LCRCDT
     C*
     C                   ENDIF
     C*
     C     CHKY1         READE     PCH01                                  60
     C*
     C                   ENDDO
     C*
     C* GET TREATING PHYSICIAN NAME
     C*
     C                   Z-ADD     PCTPHY        LCTPHY
     C                   Z-ADD     PCTPHM        LCTPHM
     C                   Z-ADD     PCTPHY        KKFEIN
     C                   Z-ADD     PCTPHM        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        LCRTA1
     C                   MOVE      *BLANKS       LCRTA2
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C                   MOVEL(P)  PDADR1        LCRTA1
     C                   MOVEL(P)  PDADR2        LCRTA2
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END ADD *** 03/24/99 ****************************************
     C                   MOVEL(P)  PDST          LCRTST
     C                   Z-ADD     PDZIP         LCRTZP
     C                   MOVEL(P)  PDCTY         LCRTCT
     C*
     C* COMBINE PROVIDER NAME INTO FIRST LAST ORDER
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        LCTPNM
     C                   MOVEL(P)  PDADR1        LCRTNM
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C     PDFSN         IFNE      *BLANKS
     C*
     C     PDFSN         CAT(P)    PDNAME:1      LCTPNM
     C     PDFSN         CAT(P)    PDNAME:1      LCRTNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  PDNAME        LCTPNM
     C                   MOVEL(P)  PDNAME        LCRTNM
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END ADD *** 03/24/99 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       LCTPNM
     C                   MOVEL     *BLANKS       LCRTNM
     C*
     C                   ENDIF
     C*
     C* GET FACILITY NAME
     C*
     C                   Z-ADD     PCFFEI        LCFFEI
     C                   Z-ADD     PCFMOD        LCFMOD
     C                   Z-ADD     PCFFEI        KKFEIN
     C                   Z-ADD     PCFMOD        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        LCFAD1
     C                   MOVE(P)   *BLANKS       LCFAD2
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C                   MOVEL(P)  PDADR1        LCFAD1
     C                   MOVEL(P)  PDADR2        LCFAD2
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END ADD *** 03/24/99 ****************************************
     C*
     C                   MOVEL     PDST          XXST              2
     C                   Z-ADD     PDZIP         XXZIP
     C                   MOVEL     PDCTY         XXCTY            25
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCFCSZ
     C*
     C* COMBINE PROVIDER NAME INTO FIRST LAST ORDER
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        LCFNAM
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C     PDFSN         IFNE      *BLANKS
     C*
     C     PDFSN         CAT(P)    PDNAME:1      LCFNAM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  PDNAME        LCFNAM
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END ADD *** 03/24/99 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       LCFNAM
     C*
     C                   ENDIF
     C*
     C* GET INTERVIEWER NAME AND TITLE
     C*
     C                   MOVEL     PCINTR        LCINTR
     C                   MOVEL     PCINTR        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVEL     UPUSNM        LCINNM
     C                   MOVEL     UPTITL        LCINTL
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       LCINNM
     C                   MOVE      *BLANKS       LCINTL
     C*
     C                   ENDIF
     C*
     C* GET ALL CERT,REQ,REC DAYS FOR PRE-CERT AND ALL RE-CERT
     C*
     C     KKEXT         IFEQ      *ZEROS
     C*
     C                   MOVE      '1'           *IN12
     C*
     C                   ELSE
     C*
     C                   MOVE      '0'           *IN12
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXTRDY
     C                   Z-ADD     *ZEROS        XXTCDY
     C                   Z-ADD     *ZEROS        XXTADY
     C*
     C     *IN12         IFEQ      '1'
     C*
     C     PCKY          CHAIN     PPC02                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   Z-ADD     PCREQD        LCTRDY
     C                   Z-ADD     PCCERD        LCTCDY
     C                   Z-ADD     PCRECD        LCTADY
     C*
     C                   ENDIF                                                  END *IN98
     C*
     C                   ELSE                                                   ELSE *IN12=0
     C*
     C     PCKY          CHAIN     PPC02                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C     PCEXT         ANDLE     KKEXT
     C*
     C                   ADD       PCREQD        XXTRDY
     C                   ADD       PCCERD        XXTCDY
     C                   ADD       PCRECD        XXTADY
     C*
     C     PCKY          READE     PPC02                                  98
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     XXTRDY        LCTRDY
     C                   Z-ADD     XXTCDY        LCTCDY
     C                   Z-ADD     XXTADY        LCTADY
     C*
     C                   ENDIF                                                  END *IN12
     C*
     C*  SET SCREEN DIARY DATE 30 DAYS FROM SYSTEM DATE
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   MOVE      XXDATE        SRDATE            8 0
     C                   Z-ADD     LCTADY        SRFUTR            5 0
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDT1
     C*
     C* MOVE IN OTHER VALUES HERE
     C*
     C                   MOVEL     *BLANKS       LC20A1
     C                   MOVEL     *BLANKS       LC20A2
     C                   MOVEL     *BLANKS       LC20A3
     C                   MOVEL     *BLANKS       LC20A4
     C                   MOVEL     *BLANKS       LC20A5
     C                   MOVEL     *BLANKS       LC40A3
     C                   MOVEL     *BLANKS       LC40A4
     C                   MOVEL     *BLANKS       LC40A5
     C                   MOVEL     *BLANKS       LC60A1
     C                   MOVEL     *BLANKS       LC60A2
     C                   MOVEL     *BLANKS       LC60A3
     C                   MOVEL     *BLANKS       LC60A4
     C                   MOVEL     *BLANKS       LC60A5
     C                   Z-ADD     *ZEROS        LCDT2
     C                   Z-ADD     *ZEROS        LCDT3
     C                   Z-ADD     *ZEROS        LCDT4
     C                   Z-ADD     *ZEROS        LCDT5
     C                   Z-ADD     *ZEROS        LCDT6
     C                   Z-ADD     *ZEROS        LCDT7
     C                   Z-ADD     *ZEROS        LCDT8
     C                   Z-ADD     *ZEROS        LCDT9
     C                   Z-ADD     *ZEROS        LCDT10
     C                   Z-ADD     *ZEROS        LC3N1
     C                   Z-ADD     *ZEROS        LC3N2
     C                   Z-ADD     *ZEROS        LC3N3
     C                   Z-ADD     *ZEROS        LC3N4
     C                   Z-ADD     *ZEROS        LC3N5
     C                   Z-ADD     *ZEROS        LC3N6
     C                   Z-ADD     *ZEROS        LC3N7
     C                   Z-ADD     *ZEROS        LC3N8
     C                   Z-ADD     *ZEROS        LC3N9
     C                   Z-ADD     *ZEROS        LC3N10
     C                   Z-ADD     *ZEROS        LC3N11
     C                   Z-ADD     *ZEROS        LC3N12
     C                   Z-ADD     *ZEROS        LC5N1
     C                   Z-ADD     *ZEROS        LC5N2
     C                   Z-ADD     *ZEROS        LC5N3
     C                   Z-ADD     *ZEROS        LC5N4
     C                   Z-ADD     *ZEROS        LC5N5
     C                   Z-ADD     *ZEROS        LC5N6
     C                   Z-ADD     *ZEROS        LC5N7
     C                   Z-ADD     *ZEROS        LC5N8
     C                   Z-ADD     *ZEROS        LC5N9
     C                   Z-ADD     *ZEROS        LC5N10
     C                   Z-ADD     *ZEROS        LC5N11
     C                   Z-ADD     *ZEROS        LC5N12
     C                   Z-ADD     *ZEROS        LC9N1
     C                   Z-ADD     *ZEROS        LC9N2
     C                   Z-ADD     *ZEROS        LC9N3
     C                   Z-ADD     *ZEROS        LC9N4
     C                   Z-ADD     *ZEROS        LC9N5
     C                   Z-ADD     *ZEROS        LC9N6
     C                   Z-ADD     *ZEROS        LC9N7
     C                   Z-ADD     *ZEROS        LC9N8
     C                   Z-ADD     *ZEROS        LC9N9
     C                   Z-ADD     *ZEROS        LC9N10
     C                   MOVEL     *BLANKS       LC1A1
     C                   MOVEL     *BLANKS       LC1A2
     C                   MOVEL     *BLANKS       LC1A3
     C                   MOVEL     *BLANKS       LC1A4
     C                   MOVEL     *BLANKS       LC1A5
     C                   MOVEL     *BLANKS       LC1A6
     C                   MOVEL     *BLANKS       LC1A7
     C                   MOVEL     *BLANKS       LC1A8
     C                   MOVEL     *BLANKS       LC1A9
     C                   MOVEL     *BLANKS       LC1A10
     C                   Z-ADD     *ZEROS        LC3#1
     C                   Z-ADD     *ZEROS        LC3#2
     C                   Z-ADD     *ZEROS        LC3#3
     C                   Z-ADD     *ZEROS        LC3#4
     C                   Z-ADD     *ZEROS        LC3#5
     C*
     C     ##RRN         CHAIN     PPC01                              99
     C*
     C* ASSIGN VALUES TO KEY FIELDS
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C  N99              Z-ADD     CMEMP#        KKEMP#
     C  N99              Z-ADD     CMDIV         KKDIV
     C  N99              MOVEL(P)  CMCLAD        KKSTAF
     C   99              Z-ADD     *ZEROS        KKEMP#
     C   99              Z-ADD     *ZEROS        KKDIV
     C   99              MOVEL(P)  *BLANKS       KKSTAF
     C*
     C* SUBSCRIBER NAME
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C  N99              MOVEL(P)  SMNAME        LCSBNM
     C  N99              MOVEL(P)  SMST          LCSBST
     C   99              MOVEL(P)  *BLANKS       LCSBNM
     C   99              MOVEL(P)  *BLANKS       LCSBST
     C*
     C* GROUP NAME
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C  N99              MOVEL(P)  FNNAME        LCGRNM
     C   99              MOVEL(P)  *BLANKS       LCGRNM
     C*
     C* EMPLOYER NAME
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C  N99              MOVEL(P)  EMNAME        LCEMNM
     C   99              MOVEL(P)  *BLANKS       LCEMNM
     C*
     C* CLAIM ADJUSTOR NAME
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        LCCANM
     C   99              MOVEL(P)  *BLANKS       LCCANM
     C*
     C* PROVIDER FIRST NAME AND TITLE
     C*
     C                   Z-ADD     PCFEIN        KKFEIN
     C                   Z-ADD     PCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*** BEG ADD *** 03/24/99 ****************************************
     C  N99PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        LCPLSN
     C                   MOVE(P)   *BLANKS       LCPFSN
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C*
     C  N99              MOVEL(P)  PDNAME        LCPLSN
     C  N99              MOVEL(P)  PDFSN         LCPFSN
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END ADD *** 03/24/99 ****************************************
     C  N99              MOVEL(P)  PDTITL        LCPTIL
     C  N99              MOVEL(P)  PDST          LCPMST
     C   99              MOVEL(P)  *BLANKS       LCPLSN
     C   99              MOVEL(P)  *BLANKS       LCPFSN
     C   99              MOVEL(P)  *BLANKS       LCPTIL
     C   99              MOVEL(P)  *BLANKS       LCPMST
     C*
     C* TREATING PHYSICIAN FIRST NAME AND TITLE
     C*
     C                   Z-ADD     PCTPHY        KKFEIN
     C                   Z-ADD     PCTPHM        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C  N99PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        LCTLSN
     C                   MOVE      *BLANKS       LCTFSN
     C                   ELSE
     C*** END ADD *** 03/24/99 ****************************************
     C  N99              MOVEL(P)  PDNAME        LCTLSN
     C  N99              MOVEL(P)  PDFSN         LCTFSN
     C*** BEG ADD *** 03/24/99 ****************************************
     C                   ENDIF                                                  PDDBA1 = 1
     C*** END ADD *** 03/24/99 ****************************************
     C  N99              MOVEL(P)  PDTITL        LCTTIL
     C  N99              MOVEL(P)  PDST          LCTMST
     C   99              MOVEL(P)  *BLANKS       LCTLSN
     C   99              MOVEL(P)  *BLANKS       LCTFSN
     C   99              MOVEL(P)  *BLANKS       LCTTIL
     C   99              MOVEL(P)  *BLANKS       LCTMST
     C*
     C                   ENDSR
     C*****************************************************************
     C*   PRTDOC - SUBROUTINE TO EXECUTE PRTDOC COMMAND               *
     C*****************************************************************
     C     PRTDOC        BEGSR
     C*
     C*   IS AN OVERLAY FORM NECESSARY
     C*
     C     LCDOVR        IFNE      *BLANKS
     C*
     C                   Z-ADD     32            I
     C                   MOVEA     LCDOVR        OVR(I)
     C                   ADD       12            I
     C                   MOVEA     ')'           OVR(I)
     C                   MOVEA     OVR           CMDSTR
     C                   Z-ADD     75            LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C                   ENDIF
     C*   BUILD PRTDOC COMMAND
     C*
     C*** Begin chg *** 10/29/12 **************************************
     C                   MOVEA     LCDCID        CMD(15)                        DOCUMENT
     C*******              MOVEALCDCID    CMD,13           DOCUMENT
     C*** End   chg *** 10/29/12 **************************************
     C                   MOVE      LC#CPY        QQ#CPY            5
     C*** Begin chg *** 10/29/12 **************************************
     C                   MOVEA     QQ#CPY        CMD(37)                        COPIES
     C                   MOVEA     LCSDOC        CMD(67)                        DARTS DOC
     C*******              MOVEAQQ#CPY    CMD,35           COPIES
     C*******              MOVEALCSDOC    CMD,65           DARTS DOC
     C*** End   chg *** 10/29/12 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     LCFLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C*** Begin chg *** 10/29/12 **************************************
     C                   Z-ADD     86            II                3 0
     C*******              Z-ADD84        II      30
     C*** End   chg *** 10/29/12 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CMD(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CMD(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CMD(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CMD(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     LCSFLD        FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I                 3 0
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CMD(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CMD(II)
     C                   ADD       5             II
     C                   MOVEA     LCOUTQ        CMD(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CMD(II)
     C*
      **** BEGIN ADD **** 01/23/04 *********************************************
     C                   ADD       2             II
     C                   MOVEA     'JOBQ'        CMD(II)
     C                   ADD       4             II
     C                   MOVEA     '(*NO)'       CMD(II)
      **** END   ADD **** 01/23/04 *********************************************
     C                   MOVEA     CMD           CMDSTR          225
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C*
     C*   BUILD PRTDOC COMMAND
     C*
      **** BEGIN CHG **** 04/03/14 *********************************************
     C*********          MOVEA     LCDCID        CMD(13)                        DOCUMENT
     C*********          MOVE      LC#CPY        QQ#CPY            5
     C*********          MOVEA     QQ#CPY        CMD(35)                        COPIES
     C*********          MOVEA     LCSDOC        CMD(65)                        DARTS DOC
     C*
     C*********          MOVEA     *BLANK        FLR
     C*********          MOVEA     LCFLDR        FLR
     C*********          Z-ADD     1             I                 3 0
     C*********          Z-ADD     84            II                3 0
     C*
     C                   MOVEA     LCDCID        CMD(15)                        DOCUMENT
     C                   MOVE      LC#CPY        QQ#CPY            5
     C                   MOVEA     QQ#CPY        CMD(37)                        COPIES
     C                   MOVEA     LCSDOC        CMD(67)                        DARTS DOC
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     LCFLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C                   Z-ADD     86            II                3 0
      **** END   CHG **** 04/03/14 *********************************************
     C     FLR(I)        DOWNE     *BLANK
     C*
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C*
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CMD(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CMD(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CMD(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CMD(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     LCSFLD        FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I                 3 0
     C*
     C     FLR(I)        DOWNE     *BLANK
     C*
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CMD(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CMD(II)
     C                   ADD       5             II
     C                   MOVEA     LCOUTQ        CMD(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CMD(II)
     C*
     C                   MOVEA     CMD           CMDSTR          225
     C***  Begin Add  ***  07/31/06  **************************************
      **** BEGIN DEL **** 04/03/14 *********************************************
     C**************     'SY'          CAT       CMDSTR:0      CMDSTR
      **** END   DEL **** 04/03/14 *********************************************
     C     CMDSTR        CAT       JOBQNO:1      CMDSTR
     C***  End   Add  ***  07/31/06  **************************************
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C*   BUILD CHGDOCD COMMAND IF DOALCH NE 'Y'
     C*
     C     DOALCH        IFNE      'Y'
     C*
     C***  Begin Chg  ***  07/31/06  **************************************
     C**********           MOVEALCSDOC    CM2,14           SAVE DOCUMENT
     C                   MOVEA     LCSDOC        CM2(16)                        SAVE DOCUMENT
     C***  End   Chg  ***  07/31/06  **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     LCSFLD        FLR
     C                   Z-ADD     1             I
     C***  Begin Chg  ***  07/31/06  **************************************
     C**********           Z-ADD46        II
     C                   Z-ADD     48            II
     C***  End   Chg  ***  07/31/06  **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK                                       SAVE FOLDER
     C*
     C                   MOVEA     FLR(I)        CM2(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C*
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CM2(II)
     C                   ADD       1             II
     C                   MOVEA     ')'           CM2(II)
     C*
     C                   MOVEA     CM2           CM2STR          225
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CM2STR
     C                   PARM                    LENGTH
     C*
     C                   ENDIF
     C*
     C* PRINT ADDITIONAL DOCUMENT
     C*
     C*
     C* CALL SPRTDOC2 TO PRINT THE SECONDARY DOCUMENT
     C*
     C*** BEGIN DEL *** 11/19/02 **************************************
     C********** PCFOLD    IFNE *BLANKS
     C********** PCDCID    ANDNE*BLANKS
     C*** BEGIN DEL *** 03/23/99 **************************************
     C**********
     C*   BUILD PRTDOC COMMAND
     C*
     C**********           MOVEAPCDCID    CM3,13           DOCUMENT
     C**********           MOVE LC#CPY    QQ#CPY  5
     C**********           MOVEAQQ#CPY    CM3,35           COPIES
     C*
     C**********           MOVEA*BLANK    FLR
     C**********           MOVEAPCFOLD    FLR
     C**********           Z-ADD1         I       30
     C**********           Z-ADD62        II      30
     C*
     C********** FLR,I     DOWNE*BLANK
     C*
     C**********           MOVEAFLR,I     CM3,II
     C**********           ADD  1         I
     C**********           ADD  1         II
     C*
     C**********           ENDDO
     C*
     C**********           MOVEA''''      CM3,II
     C**********           ADD  1         II
     C*
     C**********           MOVEA')'       CM3,II
     C**********           ADD  2         II
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C**********           ADD  3         II
     C**********           MOVEA'OUTQ('   CM3,II
     C**********           ADD  5         II
     C**********           MOVEALCOUTQ    CM3,II
     C**********           ADD  10        II
     C**********           MOVEA')'       CM3,II
     C*
     C**********           MOVEACM3       CMDSTR225
     C**********           Z-ADD225       LENGTH 155
     C*
     C**********           CALL 'QCMDEXC'
     C**********           PARM           CMDSTR
     C**********           PARM           LENGTH
     C*
     C*   BUILD PRTDOC COMMAND
     C*
     C**********           CALL 'SPRTDOC2'
     C**********           PARM PCDCID    ##DOC  12
     C**********           PARM PCFOLD    ##FLD  80
     C**********           PARM *BLANKS   ##RDOC 12
     C**********           PARM *BLANKS   ##RFLD 80
     C**********           PARM *BLANKS   ##RTCD  1
     C**********           PARM Q1USER    ##USER 10
     C*** END   DEL *** 03/23/99 **************************************
     C*** BEGIN ADD *** 03/23/99 **************************************
     C*
     C* CALL SPRTDOC2 TO PRINT THE DOCUMENT
     C*
     C**********           MOVELPCDCID    XXRDOC    P
     C**********           ADD  1         XXSEQ
     C*
     C**********           MOVELPCDCID    ##DOC     P
     C**********           MOVELPCFOLD    ##FLD     P
     C**********           MOVELXXRDOC    ##RDOC    P
     C**********           MOVEL'PRE_RSLV'##RFLD    P
     C*
     C**********           CALL 'SPRTDOC2'
     C**********           PARM           ##DOC  12
     C**********           PARM           ##FLD  80
     C**********           PARM           ##RDOC 12
     C**********           PARM           ##RFLD 80
     C**********           PARM *BLANKS   ##RTCD  1
     C**********           PARM LCOUTQ    ##QUE  10
     C*** END   ADD *** 03/23/99 **************************************
     C*
     C**********           ENDIF
     C*** END   DEL *** 11/19/02 **************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C*   RECIP - SUBROUTINE TO LOAD THE CITY STATE & ZIP             *
     C*****************************************************************
     C     RECIP         BEGSR
     C*
     C*** GET AREA OF SERVICE FOR CREATION OF SAVE FOLDER/DOCUMENT
     C*
     C*** BEGIN ADD *** 04/03/14 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80

     C                   If        ##PFLR <> *Blanks
     C                   Eval      XXlen = %Len(%Trim(##PFLR))
     C                   Eval      XXfldr = %Subst(LCfldr : XXlen + 2)
     C                   Else
     C                   Eval      XXfldr = LCfldr
     C                   Endif
     C*** END   ADD *** 04/03/14 **************************************
     C     DOKY          CHAIN     PDO01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   ENDIF
     C*
     C                   MOVE      PCCO#         #1CO#
     C                   MOVE      PCSUB#        #1SUB#
     C                   MOVE      PCFND         #1FND
     C*
     C*** BEG DEL *** 12/15/06 ****************************************
     C**********           CALL 'SNXTDCID'
     C**********           PARM *BLANKS   ##FLR  80
     C**********           PARM *BLANKS   ##DCID 12
     C**********           PARM DOPRFX    ##PRE   2
     C**********           PARM           #1CO#   3
     C**********           PARM           #1FND   3
     C**********           PARM *BLANKS   ##RTCD  1
     C**********           PARM           #1SUB#  5
     C*** END DEL *** 12/15/06 ****************************************
     C*
     C*** BEG ADD *** 12/15/06 ****************************************
     C     @@CLID        IFEQ      '144'
     C                   MOVE      'P'           ##RTCD
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      DOPRFX        ##PRE             2
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1SUB#            5
      *
     C                   ELSE
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      DOPRFX        ##PRE             2
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1SUB#            5
     C                   ENDIF
     C*** END ADD *** 12/15/06 ****************************************
     C*
     C*
     C                   SELECT
     C*
     C     CON           WHENEQ    1
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  LCCLNM        LCRRN1
     C                   MOVEL(P)  *BLANKS       LCRRN2
     C                   MOVEL(P)  CDADR1        LCRRA1
     C                   MOVEL(P)  CDADR2        LCRRA2
     C*
     C                   MOVEL     CDST          XXST              2
     C                   Z-ADD     CDZIP         XXZIP
     C                   MOVEL     CDCTY         XXCTY            25
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    2
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  LCRPNM        LCRRN1
     C                   MOVEL(P)  *BLANKS       LCRRN2
     C                   MOVEL(P)  LCRPA1        LCRRA1
     C                   MOVEL(P)  LCRPA2        LCRRA2
     C*
     C                   MOVEL(P)  LCRPST        XXST
     C                   Z-ADD     LCRPZP        XXZIP
     C                   MOVEL(P)  LCRPCT        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    3
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  LCRTNM        LCRRN1
     C                   MOVEL(P)  *BLANKS       LCRRN2
     C                   MOVEL(P)  LCRTA1        LCRRA1
     C                   MOVEL(P)  LCRTA2        LCRRA2
     C*
     C                   MOVEL(P)  LCRTST        XXST
     C                   Z-ADD     LCRTZP        XXZIP
     C                   MOVEL(P)  LCRTCT        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    4
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  LCFNAM        LCRRN1
     C                   MOVEL(P)  *BLANKS       LCRRN2
     C                   MOVEL(P)  LCFAD1        LCRRA1
     C                   MOVEL(P)  LCFAD2        LCRRA2
     C*
     C                   MOVEL(P)  LCFCSZ        LCRRCT
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    5
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SMNAME        LCRRN1
     C                   MOVEL(P)  LCRSAT        LCRRN2
     C                   MOVEL(P)  SMADR1        LCRRA1
     C                   MOVEL(P)  SMADR2        LCRRA2
     C*
     C                   MOVEL(P)  SMST          XXST
     C                   Z-ADD     SMZIP         XXZIP
     C                   MOVEL(P)  SMCTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    6
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C     F1FLG1        IFEQ      'Y'
     C*
     C                   MOVEL(P)  F1ANME        LCRRN1
     C                   MOVEL(P)  LCRGAT        LCRRN2
     C                   MOVEL(P)  F1AAD1        LCRRA1
     C                   MOVEL(P)  F1AAD2        LCRRA2
     C*
     C                   MOVEL(P)  F1AST         XXST
     C                   Z-ADD     F1AZIP        XXZIP
     C                   MOVEL(P)  F1ACTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  FNNAME        LCRRN1
     C                   MOVEL(P)  LCRGAT        LCRRN2
     C                   MOVEL(P)  FNMAD1        LCRRA1
     C                   MOVEL(P)  FNMAD2        LCRRA2
     C*
     C                   MOVEL(P)  FNMST         XXST
     C                   Z-ADD     FNMZIP        XXZIP
     C                   MOVEL(P)  FNMCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    7
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EMMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  EMNAME        LCRRN1
     C                   MOVEL(P)  LCREAT        LCRRN2
     C                   MOVEL(P)  EMMAD1        LCRRA1
     C                   MOVEL(P)  EMMAD2        LCRRA2
     C*
     C                   MOVEL(P)  EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL(P)  EMMCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  EMNAME        LCRRN1
     C                   MOVEL(P)  LCREAT        LCRRN2
     C                   MOVEL(P)  EMPAD1        LCRRA1
     C                   MOVEL(P)  EMPAD2        LCRRA2
     C*
     C                   MOVEL(P)  EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL(P)  EMPCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    8
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  LCRONM        LCRRN1
     C                   MOVEL(P)  LCROAT        LCRRN2
     C                   MOVEL(P)  LCROA1        LCRRA1
     C                   MOVEL(P)  LCROA2        LCRRA2
     C*
     C                   MOVEL(P)  LCROST        XXST
     C                   Z-ADD     LCROZP        XXZIP
     C                   MOVEL(P)  LCROCT        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         LCRRCT
     C*
     C                   MOVEL(P)  ##FLR         LCSFLD
     C                   MOVEL(P)  ##DCID        LCSDOC
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*****************************************************************
     C*   SRADDR - SUBROUTINE TO LOAD THE CITY STATE & ZIP            *
     C*****************************************************************
     C     SRADDR        BEGSR
     C*
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4
     C*
     C     XXCTY         CAT(P)    ',':0         XXCSZ            40
     C                   CAT(P)    XXST:1        XXCSZ
     C                   CAT(P)    XXZIP1:1      XXCSZ
     C*
     C     XXZPM2        IFNE      *ZEROS
     C*
     C                   CAT(P)    '-':0         XXCSZ
     C                   CAT(P)    XXZIP2:0      XXCSZ
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*   SRDART - WRITE DARTS RECORD                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C*  WRITE RECORD TO DARTS
     C*
     C*  ESTABLISH KEY
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C                   MOVE      PCCO#         ##CO#
     C                   MOVE      PCSUB#        ##SUB#
     C                   MOVE      PCFND         ##FND
     C                   MOVE      PCFYR         ##FYR
     C                   MOVE      PCCASE        ##CASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       #1KEY            40
     C                   PARM      *BLANKS       #1PGM            10
     C*
     C                   MOVEL(P)  #1PGM         ##PNAM
     C*** BEGIN ADD *** 05/26/99 **************************************
     C     LCDIDT        IFEQ      *ZEROS
     C*** END   ADD *** 05/26/99 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##CDAT
     C*** BEGIN ADD *** 05/26/99 **************************************
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 05/26/99 **************************************
     C**********           MOVE LCDIDT    SRDTI
     C                   Z-ADD     LCDIDT        SRDTI
     C*** END   CHG *** 05/26/99 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##CDAT
     C*
     C                   ENDIF
     C*** END   ADD *** 05/26/99 **************************************
     C*
     C     LCDIUS        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  LCDIUS        ##USER
     C                   MOVEL(P)  'Y'           ##DIFL
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  PCINTR        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         ##USER
     C   99              MOVEL(P)  Q1USER        ##USER
     C*
     C                   MOVEL(P)  DODIFL        ##DIFL
     C*
     C                   ENDIF
     C*
     C*  ESTABLISH RECORD
     C*
     C                   MOVEA     *BLANKS       CM
     C*
     C                   SELECT
     C*
     C     CON           WHENEQ    1
     C                   MOVEA(P)  'Clm:'        CM(1)
     C*
     C     CON           WHENEQ    2
     C                   MOVEA(P)  'Prv:'        CM(1)
     C*
     C     CON           WHENEQ    3
     C                   MOVEA(P)  'TPh:'        CM(1)
     C*
     C     CON           WHENEQ    4
     C                   MOVEA(P)  'Fac:'        CM(1)
     C*
     C     CON           WHENEQ    5
     C                   MOVEA(P)  'Sub:'        CM(1)
     C*
     C     CON           WHENEQ    6
     C                   MOVEA(P)  'Grp:'        CM(1)
     C*
     C     CON           WHENEQ    7
     C                   MOVEA(P)  'Emp:'        CM(1)
     C*
     C     CON           WHENEQ    8
     C                   MOVEA(P)  'Oth:'        CM(1)
     C*
     C                   ENDSL
     C*
     C                   CALL      'DA200R'
     C                   PARM      DORMCD        ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   MOVEA(P)  PCCERT        CM(6)
     C                   MOVE      PCEXT         XXEXT             3
     C                   MOVEA(P)  '-'           CM(21)
     C                   MOVEA(P)  XXEXT         CM(22)
     C                   MOVEA(P)  ':'           CM(25)
     C                   MOVEA     PCLIAB        CM(27)
     C*
     C     ##RTCD        IFEQ      'Y'
     C*
     C                   MOVEL(P)  ##DESC        XXRMDS           40
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       XXRMDS
     C*
     C                   ENDIF
     C*
     C                   MOVEA     DODESC        CM(29)
     C*** BEGIN ADD *** 02/12/07 **************************************
     C     @@CLID        IFEQ      '144'
     C                   MOVEL     'O'           QQIMGF
     C                   ENDIF
     C*** END   ADD *** 02/12/07 **************************************
     C*
     C                   CALL      'DA700R'
     C                   PARM      #1KEY         #1KEY            40
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM      LCSDOC        ##DCID           12
     C                   PARM                    ##PNAM           10
     C                   PARM      '000'         ##SECL            3
     C                   PARM                    ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM      LCSFLD        ##FOLD           80
     C                   PARM                    ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      'N'           ##FL01            1
     C                   PARM      'N'           ##FL02            1
     C                   PARM      DORMCD        #QRMCD            5
     C                   PARM      '00000'       #QSEQ#            5
     C*** BEGIN ADD *** 02/12/07 **************************************
     C                   PARM                    QQSBYL            1            16
     C                   PARM                    QQCOM2           19            17
     C                   PARM                    QQAPID            2            18
     C                   PARM                    QQIDOC           12            19
     C                   PARM                    QQIQUE           10            20
     C                   PARM                    QQIMGF            1            21
     C                   PARM                    QQIPTH           10            22
     C                   PARM                    QQPIDX            5            23
     C                   PARM                    QQQPTY            1            24
     C                   PARM                    QQDCTP            5            25
     C                   PARM                    QQCSID            2            26
     C                   PARM                    QQCODE            2            27
     C*** END   ADD *** 02/12/07 **************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C*   UPDATE - SUBROUTINE TO UPDATE RECORDS                       *
     C*****************************************************************
     C     UPDATE        BEGSR
     C*
     C                   MOVEL(P)  LCDPCD        KKDPCD
     C                   MOVEL(P)  LCFLDR        KKFLDR
     C                   MOVEL(P)  LCDCID        KKDCID
     C*
     C                   EXSR      UPDATA
     C*
     C                   ENDSR
     C*****************************************************************
     C*   UPDATA - SUBROUTINE TO UPDATE RECORDS IN UXLCP,UXLCP1,UXPOP *
     C*****************************************************************
     C     UPDATA        BEGSR
     C*
     C*  GET WCMC PRE-CERT DEPARTMENT FILE 1 AND 2.
     C*
     C     LCKY2         CHAIN     PLC02                              99
     C*
     C*  UPDATE PRE-CERT DEPARTMENT FILE 1 AND 2.
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     LCKY2         CHAIN     PLC01                              99
     C*
     C                   EXSR      MVINFO
     C*
     C*  WRITE DEPARTMENT LETTERS WORKFILE RECORD FOR PRINTING
     C*
     C                   MOVEL     Q1USER        LCUSER
     C                   MOVEL     Q1PGM         LCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDT
     C*
     C                   Z-ADD     DSTIME        LCTIME
     C*
     C                   UPDATE    PLC01
     C*
     C                   UPDATE    PLC02
     C*
     C                   ENDIF
     C*
     C                   ENDSR
**  (CMD) QCMDEXC COMMAND
SYPRTDOC DOC('            ') COPIES(     ) SAVOUTPUT(*YES) SAVDOC(
   ) FLR('

**  (CM2) QCMDEXC COMMAND
SYCHGDA   DOC('            ') ALWRPL(*NO) FLR('


**  (OVR) QCMDEXC COMMAND
OVRPRTF FILE(QSYSPRT) FRONTOVL(
