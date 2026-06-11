     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA034R - Retrieve EOMB Information - Bill Entry
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
     H*                                                               *
     H*****************************************************************
     FFMFNL1    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FNATRL     IF   E           K DISK

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** Beg add *** 08/08/11 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** End add *** 08/08/11 **************************************
      *

     C     *ENTRY        PLIST
01   C                   PARM                    @@CO#             3 0          Company #
02   C                   PARM                    @@WCF#            3 0          WCMS Fund
03   C                   PARM                    @@FYR             3 0          Policy Year
04   C                   PARM                    @@CASE            7 0          Claim #
05   C                   PARM                    @@EOB#            9 0          EOMB #
06   C                   PARM                    @@FEIN            9 0          FEIN
07   C                   PARM                    @@MOD             3 0          MOD
08   C                   PARM                    @@CHK#            9 0          CHECK NUMBER
09   C                   PARM                    @@OTHR            9 2          EOMB billing amt
10   C                   PARM                    @@AST             2            Adjudication St
11   C                   PARM                    @@BINV            9 0          BRS INV#
12   C                   PARM                    @@NINV            9 0          NWS INV#
13   C                   PARM                    @@SUB#            5 0          Subscriber #
14   C                   PARM                    @@FND             3 0          WCMC Group #
15   C                   PARM                    @@RRN             9 0          RRN of FAEOP
16   C                   PARM                    @@RC              1            Return Code
     C*** Beg add *** 08/08/11 **************************************
17   C                   PARM                    @@SINV           35            Source Inv #
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
     C     TRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
      *
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFND         KKWCF#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
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
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      FNSUB#        KKSUB#
     C                   MOVE      FNFND         KKFND
     C                   Z-ADD     FNSUB#        @@SUB#
     C                   Z-ADD     FNFND         @@FND
     C*
     C* Does the eomb exist
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN         @@RRN
     C                   MOVEL     EOAST         @@AST
     C                   Z-ADD     EOOTHR        @@OTHR
     C*** Beg add *** 08/08/11 **************************************
     C     Q1PRMS        IFGE      17
17   C                   MOVEL(P)  EOSINV        @@SINV
     C                   ENDIF
     C*** End add *** 08/08/11 **************************************
     C*
     C     ##AR          IFEQ      'Y'
     C*
     C     @@FEIN        IFNE      EOFEIN
     C     @@MOD         ORNE      EOMOD
     C*
     C                   Z-ADD     EOINV#        @@BINV
     C*
     C     TRKY          CHAIN     PTR01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     TRINV#        @@NINV
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        @@BINV
     C                   Z-ADD     *ZEROS        @@NINV
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     EOFEIN        @@FEIN
     C                   Z-ADD     EOMOD         @@MOD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RC
     C*
     C                   ENDIF
     C*
     C*  End of program
     C*
     C                   RETURN
