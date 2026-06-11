     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA033R - Retrieve EOMB Information - PBILLS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program retrieves information from the EOMB file.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 06/07/00  C:62  Project:MC230                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/08/11  C:53   PROJECT: 139WQK                              *
     H*                  Added retrieval of Source invoice #          *
     H*                                                               *
     H*  08/21/02 C:53 Project: 48WVS                                 *
     H*                Add bill type parm                             *
     H*                                                               *
     H*  08/07/01 C:062 PROJECT:48WVI                                 *
     H*                 ADD PARM TO PASS BACK PATIENT ACCOUNT #       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*  RETURNED VALUES:                                             *
     H*                                                               *
     H*  @@OTHR - EOOTHR                                              *
     H*  @@FEIN - EOFEIN                                              *
     H*  @@MOD  - EOMOD                                               *
     H*  @@AST  - EOAST                                               *
     H*  @@BINV - EABINV                                              *
     H*  @@NINV - EANINV                                              *
     H*  @@SUB# - FNSUB#                                              *
     H*  @@FND  - FNFND                                               *
     H*  @@RRN  - RRN of FAEOP                                        *
     H*  @@RC   - 'F' - If unable to get the above information.       *
     H*  @@ACCT - EOBTYP = 'P' THEN EHACCT                            *
     H*           EOBTYP = 'H' THEN EUACCT                            *
     H*           EOBTYP = 'D' THEN EDACCT                            *
     H*           EOBTYP = 'X' THEN EPACCT                            *
     H*                                                               *
     H*****************************************************************
     FFMFNL1    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFAEAL1    IF   E           K DISK
     F*** BEGIN ADD *** 08/07/01 **************************************
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEDL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     F*** END   ADD *** 08/07/01 **************************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I*

     C     *ENTRY        PLIST
1    C                   PARM                    @@CO#             3 0          Company #
2    C                   PARM                    @@WCF#            3 0          WCMS Fund
3    C                   PARM                    @@FYR             3 0          Policy Year
4    C                   PARM                    @@CASE            7 0          Claim #
5    C                   PARM                    @@EOB#            9 0          EOMB #
6    C                   PARM                    @@RCN             3 0          RECON #
7    C                   PARM                    @@FEIN            9 0          FEIN
8    C                   PARM                    @@MOD             3 0          MOD
9    C                   PARM                    @@CHK#            9 0          CHECK NUMBER
10   C                   PARM                    @@OTHR            9 2          EOMB billing amt
11   C                   PARM                    @@AST             2            ADJUDICATION ST
12   C                   PARM                    @@BINV            9 0          BRS INV#
13   C                   PARM                    @@NINV            9 0          NWS INV#
14   C                   PARM                    @@SUB#            5 0          SUBSCRIBER
15   C                   PARM                    @@FND             3 0          GROUP
16   C                   PARM                    @@RRN             9 0          RRN OF FAEOP
17   C                   PARM                    @@RC              1            Return Code
     C*** BEGIN ADD *** 08/07/01 **************************************
18   C                   PARM                    @@ACCT           30            Pat Account#
     C*** END   ADD *** 08/07/01 **************************************
     C*** BEG ADD *** 08/21/02 *****************************
19   C                   PARM                    @@BTYP            1            Bill type
     C*** END ADD *** 08/21/02 *****************************
     C*** Beg add *** 08/08/11 **************************************
20   C                   PARM                    @@SINV           35            Source Inv #
     C*** End add *** 08/08/11 **************************************
     C*
     C*  Key list for the WCMC Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C*
     C*  Key list for the EOMB File
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*** BEGIN ADD *** 08/07/01 **************************************
     C*
     C     GENKY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD *** 08/07/01 **************************************
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFND         KKWCF#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
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
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@WCF#        KKWCF#
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C                   Z-ADD     @@RCN         KKRCN
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKSUB#
     C*
     C*================================================================
     C*  Program Start
     C*================================================================
     C*
     C*  Translate the WCMS fund into WCMC Sub# and fund.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNSUB#        @@SUB#
     C                   Z-ADD     FNFND         @@FND
     C*
     C*  Update the EOMB billing amount.
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN         @@RRN
     C                   Z-ADD     EOOTHR        @@OTHR
     C                   MOVEL     EOAST         @@AST
     C*** BEGIN ADD *** 08/07/01 **************************************
     C     Q1PRMS        IFGT      17
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     GENKY         CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EHACCT        @@ACCT
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ACCT
     C                   ENDIF
     C     EOBTYP        WHENEQ    'H'
     C     GENKY         CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EUACCT        @@ACCT
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ACCT
     C                   ENDIF
     C     EOBTYP        WHENEQ    'D'
     C     GENKY         CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EDACCT        @@ACCT
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ACCT
     C                   ENDIF
     C     EOBTYP        WHENEQ    'X'
     C     GENKY         CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EPACCT        @@ACCT
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ACCT
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 08/07/01 **************************************
     C*** BEG ADD *** 08/21/02 *****************************
     C     Q1PRMS        IFGT      18
     C                   MOVE      EOBTYP        @@BTYP
     C                   ENDIF
     C*** END ADD *** 08/21/02 *****************************
     C*** Beg add *** 08/08/11 **************************************
     C     Q1PRMS        IFGE      20
20   C                   MOVEL(P)  EOSINV        @@SINV
     C                   ENDIF
     C*** End add *** 08/08/11 **************************************
     C*
B03  C     ##AR          IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
B04  C     *IN99         IFEQ      *OFF
     C*
B05  C     @@FEIN        IFNE      EOFEIN
     C     @@MOD         ORNE      EOMOD
     C*
     C                   SELECT
     C*
     C     @@CHK#        WHENEQ    EAWCBC
     C                   Z-ADD     *ZEROS        @@NINV
     C                   Z-ADD     EABINV        @@BINV
     C*
     C     @@CHK#        WHENEQ    EAWCNC
     C                   Z-ADD     EANINV        @@NINV
     C                   Z-ADD     *ZEROS        @@BINV
     C*
     C                   ENDSL
     C*
E05  C                   ENDIF
     C*
S04  C                   ELSE
     C*
     C                   MOVEL     'F'           @@RC
     C*
E04  C                   ENDIF
     C*
E03  C                   ENDIF
     C*
     C                   Z-ADD     EOFEIN        @@FEIN
     C                   Z-ADD     EOMOD         @@MOD
     C*
S02  C                   ELSE
     C*
     C                   MOVEL     'F'           @@RC
     C*
E02  C                   ENDIF
     C*
S01  C                   ELSE
     C*
     C                   MOVEL     'F'           @@RC
     C*
E01  C                   ENDIF
     C*
     C*  End of program
     C*
     C                   RETURN
