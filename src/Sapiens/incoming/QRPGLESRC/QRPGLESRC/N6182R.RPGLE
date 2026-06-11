     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N6182R - UPDATE PRE-CERT AND MED CASE INFO
     H*****************************************************************
     H*                                                              **
     H*   P R O G R A M      S U M M A R Y                           **
     H*                                                              **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  02/01/00 C:62  PROJECT: MC221                               **
     H*                 DON'T UPDATE EOUSER.                         **
     H*                                                              **
     H*  11/12/99 C:62  PROJECT: MCMNT                               **
     H*                 ADD PARM CALL OF AA001R TO INDICATE BATCH.   **
     H*                                                              **
     H*  10/11/99 C:62 PROJECT:MC212                                 **
     H*                ADD PROVIDER BILL LOGIC                       **
     H*                                                              **
     H*****************************************************************
     F***
     FFAEOL2    UF   E           K DISK
     F*** BEGIN ADD *** 10/11/99 **************************************
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 10/11/99 **************************************

     D**
     D*** BEGIN ADD *** 10/11/99 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END   ADD *** 10/11/99 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D ARGUMT          DS
     D  XQCO#                  1      2P 0
     D  XQSUB#                 3      5P 0
     D  XQFND                  6      7P 0
     D  XQFYR                  8      9P 0
     D  XQCASE                10     13P 0
     D  XQEOB#                14     18P 0
     D CNTRL           DS
     D  EOCO#                  1      2P 0
     D  EOSUB#                 3      5P 0
     D  EOFND                  6      7P 0
     D  EOFYR                  8      9P 0
     D  EOCASE                10     13P 0
     D  EOEOB#                14     18P 0
     D*
     D                 DS
     D  XXPGM                  1      8
     D  XXTYPE                 4      5
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1SUB#            5
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1CASE            7
     C                   PARM                    Q1EOB#            9
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C* FAEOL2, WHEN XQEOB# NOT EQUAL ZERO
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XQCO#
     C                   KFLD                    XQSUB#
     C                   KFLD                    XQFND
     C                   KFLD                    XQFYR
     C                   KFLD                    XQCASE
     C                   KFLD                    XQEOB#
     C*
     C     *LIKE         DEFINE    EOSUB#        OLDSUB                          ADD 01/07/94
     C     *LIKE         DEFINE    EOFND         OLDFND                          ADD 01/07/94
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C* FIND OUT IF PROVIDER BILLS IS INSTALLED
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*** END   ADD *** 10/11/99 **************************************
     C                   Z-ADD     *ZEROS        X                 5 0
     C*
     C                   MOVE      Q1CO#         XQCO#
     C                   MOVE      Q1SUB#        XQSUB#
     C                   MOVE      Q1FND         XQFND
     C                   MOVE      Q1FYR         XQFYR
     C                   MOVE      Q1CASE        XQCASE
     C                   MOVE      Q1EOB#        XQEOB#
     C*
     C                   MOVE      *OFF          *IN60
     C*
     C     *LOVAL        SETLL     PEO01                              6060      GET ALL EOB
     C*
     C  N60              READ      PEO01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     EOSYS         IFEQ      'U'
     C     EOSYS         OREQ      'M'
     C*** BEGIN ADD *** 10/11/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C                   MOVE      'P'           ##BBLF
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAEA
     C*
     C*
     C* UPDATE VALUES IN FAEAP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C                   PARM      *ZEROS        ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM      *ZEROS        ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** BEGIN ADD *** 11/12/99 **************************************
23   C                   PARM      ' '           ##BPFL            1
     C                   PARM      'B'           ##MODE            1
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C                   MOVE      'P'           EOSTAT
     C                   MOVE      Q1PGM         EOPGM
     C*** BEGIN DEL *** 02/01/00 **************************************
     C**********           MOVE Q1USER    EOUSER
     C*** END   DEL *** 02/01/00 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME                         TIME
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   READ      PEO01                                  60
     C                   ENDDO
     C*
     C                   SETON                                          LR
