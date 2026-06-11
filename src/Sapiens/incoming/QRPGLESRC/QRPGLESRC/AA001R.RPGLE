     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  AA001R - API FOR UPDATE OF FAEAP               C:94 12/10/98 *
     H*                                                               *
     H*  CALLED FROM: EVERYWHERE                                      *
     H*                                                               *
     H*  CALLS TO   : SA001R - API THAT ASSIGNS VARIOUS SYSTEM FLAGS. *
     H*             SRCVTDAT - DATE CONVERSION PROGRAM.               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/13/06  C:62  PROJECT: 139MEM                              *
     H*                  UPDATE EABAMT WHEN ADDING FAEAP RECORD       *
     H*                                                               *
     H*  09/19/03  C:62  PROJECT: MCMNT                               *
     H*                  DON'T UPDATE THE PRINT FLAG                  *
     H*                                                               *
     H*  01/21/02  C:62  PROJECT: AR033                               *
     H*                  UPDATE NETWORK INVOICE DATE AND NETWORK      *
     H*                  CALCULATED BILLING AMOUNT.                   *
     H*                                                               *
     H*  03/13/00  C:62  PROJ: ARMNT                                  *
     H*                  POPULATE EATRDT WITH CURRENT DATE            *
     H*                  WHEN FAEAP RECORDS ARE WRITTEN.              *
     H*                                                               *
     H*  02/01/00  C:62  PROJ: MC221                                  *
     H*                  DON'T UPDATE EOUSER IF CALLED IN BATCH MODE. *
     H*                                                               *
     H*  12/21/99  C:62  PROJ: MCMNT                                  *
     H*                  POPULATE EOLSN, EOFSN, EOMID AND EOAST.      *
     H*                                                               *
     H*  11/12/99  C:62  PROJ: MC212                                  *
     H*                  -ADD PARM FOR DEMAND VERSUS BATCH CALL       *
     H*                  -IF BATCH MODE DO NOT UPDATE EAUSER.         *
     H*                                                               *
     H*  10/26/99  C:62  PROJ: MC216                                  *
     H*                  CHANGE WRITE TO FAEAL1 TO FAEAP              *
     H*                                                               *
     H*  10/21/99  C:62  PROJ: MC212                                  *
     H*                  ADD UPDATE OF BPFL.                          *
     H*                                                               *
     H*  10/13/99  C:62  PROJ: MC212                                  *
     H*                  ADD PROCESS TO LOAD TRCD AND STS IF AUTO-CHK *
     H*                                                               *
     H*  09/07/99  C:62  PROJ: MC212                                  *
     H*                  GETTING T/MS TO WORK.                        *
     H*                                                               *
     H*  06/08/99  C:62  PROJ: MC145                                  *
     H*                  NO LONGER CONDITION BASED UPON @@BAMT = 0    *
     H*                                                               *
     H*  03/17/99  C:94  PROJ: MC152                                  *
     H*                  COMMENT OUT UPDATE OF EOB# AND SYS           *
     H*                                                               *
     H*  03/03/99  C:94  PROJ: MC152                                  *
     H*                  DO NOT WRITE OR UPDATE RECON#.  THIS SHOULD  *
     H*                  ONLY BE DONE IN BILL ENTRY.                  *
     H*                                                               *
     H*****************************************************************
     H*  NOTE:                                                        *
     H*  @@WRUP IS A 7 POSITION ALPHA FIELD.  THE VALID OPTIONS ARE A *
     H*  'W' IF IT IS TO WRITE TO A FILE AND A 'U' IF IT IS TO UPDATE *
     H*  A FILE.  FAEOP, FAE1P, NAMBP, AND FAEAP CAN ALSO BE A 'D'    *
     H*  FOR A DELETE.                                                *
     H*  THE FOLLOWING ARE WHAT FILE EACH POSITION IS FOR:            *
     H*      POSITION 1:  FAEOP                                       *
     H*      POSITION 2:  FAE1P                                       *
     H*      POSITION 3:  FAHBP                                       *
     H*      POSITION 4:  NAPBP                                       *
     H*      POSITION 5:  NAMBP                                       *
     H*      POSITION 6:  NATRP                                       *
     H*      POSITION 7:  FAEAP                                       *
     H*  @@WRUP CAN THEREFORE LOOK LIKE THIS:  WU U D                 *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCMP     IF   E             DISK
     F*
     FNATRL     UF A E           K DISK
     F*
     FFAHBL     UF A E           K DISK
     F*
     FNAPBL1    UF A E           K DISK
     F*
     F*** BEGIN CHG *** 10/25/99 **************************************
     F*****FAEAL1  UF  E           K        DISK                      A
     FFAEAL1    UF   E           K DISK
     F*** END   CHG *** 10/25/99 **************************************
     F                                     INFDS(DBFDS)
     F*
     F*** BEGIN DEL *** 09/07/99 **************************************
     F**********FAEAP   UF  E                    DISK
     F**********  PEA01                             KRENAMEPEA02
     F*** END   DEL *** 09/07/99 **************************************
     F*** BEGIN ADD *** 10/25/99 **************************************
     FFAEAP     O    E             DISK
     F                                     RENAME(PEA01:PEA02)
     F*** END   ADD *** 10/25/99 **************************************
     F*
     FFAEOLG    UF A E           K DISK
     F*
     FFAE1L     UF A E           K DISK
     F                                     INFDS(DBFDS2)
     F*
     FFAE1P     UF   E             DISK
     F                                     RENAME(PE101:PE102)
     F*
     F*** BEGIN CHG *** 09/07/99 **************************************
     F***NAMBL   UF  E           K        DISK                      A
     FNAMBL1    UF A E           K DISK
     F*** END   CHG *** 09/07/99 **************************************
     F                                     INFDS(DBFDS3)
     F*
     FNAMBP     UF   E             DISK
     F                                     RENAME(PMB01:PMB02)
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRNB               397    400B 0
     D DBFDS3          DS
     D  DBRRNC               397    400B 0
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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

     I*

     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C*
     C     *ENTRY        PLIST
1    C                   PARM                    @@RRN             9 0          RRN OF CASE
2    C                   PARM                    @@EOB#            9 0          EOB #
3    C                   PARM                    @@RCN             3 0          RECON #
4    C                   PARM                    @@PGM            10            NAME OF PGM CALLING
5    C                   PARM                    @@BAMT            9 2          BRS AMOUNT
6    C                   PARM                    @@NAMT            9 2          NWS AMOUNT
7    C                   PARM                    @@BHRF            1            BRS INV H/R FLAG
8    C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
9    C                   PARM                    @@BINV            9 0          BRS INVOICE #
10   C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
11   C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
12   C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
13   C                   PARM                    @@NINV            9 0          NWS INVOICE #
14   C                   PARM                    @@SYS             1            TYPE OF SERVICE
15   C                   PARM                    @@WRUP            7            WRITE/UPDATE FLAG
16   C                   PARM                    @@TMBA            9 2          TIME/MAT BEFORE
17   C                   PARM                    @@TMAA            9 2          TIME/MAT AFTER
     C*** BEGIN ADD *** 09/07/99 **************************************
18   C                   PARM                    @@CERT           15            PRE-CERT
19   C                   PARM                    @@EXT             3 0          EXTENSION
20   C                   PARM                    @@FEIN            9 0          PROVIDER FEIN
21   C                   PARM                    @@MOD             3 0          PROVIDER MOD
22   C                   PARM                    @@BIDT            7 0          BRS INV DT
     C*** END   ADD *** 09/07/99 **************************************
     C*** BEGIN ADD *** 10/21/99 **************************************
23   C                   PARM                    @@BPFL            1            BRS INV DT
     C*** END   ADD *** 10/21/99 **************************************
     C*** BEGIN ADD *** 11/12/99 **************************************
24   C                   PARM                    @@MODE            1            CALL MODE
     C*** END   ADD *** 11/12/99 **************************************
     C*** BEGIN ADD *** 01/21/02 **************************************
25   C                   PARM                    @@NIDT            7 0          NET INV DT
26   C                   PARM                    @@NCAM            9 2          NET CALC AMT
     C*** END   ADD *** 01/21/02 **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     HBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   KFLD                    @@EOB#
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C     MBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   KFLD                    @@EOB#
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C     PBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           KFLD           EOEOB#
     C                   KFLD                    @@EOB#
     C*** END   CHG *** 09/07/99 **************************************
     C*
     C     TRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   KFLD                    @@EOB#
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    @@EOB#
     C*
     C     E1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    @@EOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    @@RCN
     C*
     C*----------------------------------------------------------------
     C*  START OF PROGRAM
     C*----------------------------------------------------------------
     C*
     C                   CLEAR                   PEA01
     C                   CLEAR                   PEO01
     C                   CLEAR                   PE101
     C                   CLEAR                   PMB01
     C                   CLEAR                   PPB01
     C                   CLEAR                   PTR01
     C                   CLEAR                   PHB01
     C*** BEG ADD *** 03/24/99 **************************************
     C                   MOVEL     @@PGM         XXPGM            10
     C*** END ADD *** 03/24/99 **************************************
     C*
     C                   MOVEL     @@WRUP        KKWRUP
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILL
     C*
     C                   EXSR      MAIN
     C*
     C     XXFAE1        IFEQ      'D'
     C     XXNAMB        OREQ      'D'
     C     XXFAEA        OREQ      'D'
     C*** BEGIN ADD *** 09/07/99 **************************************
     C     XXFAEO        OREQ      'D'
     C*** END   ADD *** 09/07/99 **************************************
     C                   EXSR      DELUPD
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           ENDIF
     C                   ELSE
     C*** END   CHG *** 09/07/99 **************************************
     C*
     C     @@PRVB        IFEQ      'Y'
     C                   EXSR      UPFAEA
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAEO
     C*** END   ADD *** 09/07/99 **************************************
     C                   ELSE
     C                   EXSR      UPREST
     C                   ENDIF
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   RETURN
     C*
     C*---------------------------------------------------------------
     C* MAIN SUBROUTINE - GET THE MAIN INFORMATION FROM FMCMP
     C*---------------------------------------------------------------
     C*
     C     MAIN          BEGSR
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   MOVEL(P)  CMSS#         KKSS#
     C                   Z-ADD     CMACDT        KKACDT
     C                   ENDIF                                                  *IN99
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C* CHECK TO SEE IF THERE IS AUTO CHECK ISSUANCE
     C*
     C                   CALL      'SA009R'
     C                   PARM      ' '           ##EOBD            1            EOMB DELETE SECURITY
     C                   PARM      ' '           ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      ' '           ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      ' '           ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      ' '           ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      ' '           ##SDTS            1            ALLOW FUTURE SERV DT
     C*
     C* INITIALIZE TRCD AND STS
     C*
     C                   MOVEL     *BLANKS       #2TRCD                         TRANSACTION CODE
     C                   MOVEL     *BLANKS       #2STS                          STS         CODE
     C*
     C* IF THERE IS AUTO CHECK ISSUANCE THEN GET TRCD AND STS
     C*
B01  C     @@WCIF        IFEQ      'Y'
     C     @@WCIN        ANDEQ     'Y'
     C     ##ACIS        ANDEQ     'Y'
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C* IF THERE IS A WCMS GROUP THEN LETS GET US SOME TRCD AND STS.
     C*
B02  C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   MOVE      CMCO#         #2CO#
     C*
     C                   CALL      'WA034R'
     C                   PARM                    #2CO#             3            Company
     C                   PARM      #1WCF#        #2FND             3            Group #
     C                   PARM      @@SYS         #2SYS             1            SYSTEM (U,M)
     C                   PARM      *BLANKS       #2TRCD            2            TRANSACTION CODE
     C                   PARM      *BLANKS       #2STS             2            STS         CODE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ENDSR
     C*
     C*---------------------------------------------------------------
     C* UPFAEA SUBROUTINE - UPDATE FAEAP IF PROVIDER BILL CONTROL FLAG
     C*                     EQUALS 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPFAEA        BEGSR
     C*** BEGIN ADD *** 09/07/99 **************************************
     C     XXFAEA        IFEQ      'U'
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEG DEL *** 03/17/99 ****************************************
     C********** @@EOB#    IFNE *ZEROS
     C**********           Z-ADD@@EOB#    EAEOB#
     C**********           ENDIF
     C*** END DEL *** 03/17/99 ****************************************
     C*
     C*** BEG DEL *** 03/03/99 ***************************************
     C********** @@RCN     IFNE *ZEROS
     C**********           Z-ADD@@RCN     EARCN
     C**********           ENDIF
     C*** END DEL *** 03/03/99 ***************************************
     C*
     C     @@BAMT        IFNE      *ZEROS
     C*** BEG ADD *** 03/24/99 ***************************************
     C     XXPGM         OREQ      'N7124R'
     C*** END ADD *** 03/24/99 ***************************************
     C                   Z-ADD     @@BAMT        EABAMT
     C                   ENDIF
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        EANAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        EABHRF
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C*** BEG ADD *** 03/17/99 ***************************************
     C     XXPGM         OREQ      'N7124R'
     C*** END ADD *** 03/17/99 ***************************************
     C                   MOVEL     @@BBLF        EABBLF
     C                   ENDIF
     C*** BEGIN ADD *** 10/21/99 **************************************
     C     Q1PRMS        IFGT      22
     C     @@BPFL        IFNE      *BLANK
     C*
     C                   MOVEL     @@BPFL        EABPFL
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 10/21/99 **************************************
     C*
     C     @@BINV        IFNE      *ZEROS
     C*** BEG ADD *** 03/24/99 ***************************************
     C     XXPGM         OREQ      'N7124R'
     C*** END ADD *** 03/24/99 ***************************************
     C                   Z-ADD     @@BINV        EABINV
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C*** BEG ADD *** 03/24/99 ***************************************
     C     XXPGM         OREQ      'N7124R'
     C*** END ADD *** 03/24/99 ***************************************
     C                   Z-ADD     @@BCAM        EABCAM
     C                   ENDIF
     C*
     C     @@NHRF        IFNE      *BLANK
     C                   MOVEL     @@NHRF        EANHRF
     C                   ENDIF
     C*
     C     @@NBLF        IFNE      *BLANK
     C                   MOVEL     @@NBLF        EANBLF
     C                   ENDIF
     C*
     C     @@NINV        IFNE      *ZEROS
     C                   Z-ADD     @@NINV        EANINV
     C                   ENDIF
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C* UPDATE BRS INV DT IF NOT EQUAL TO ZERO
     C*
     C     Q1PRMS        IFGT      21
     C*
     C     @@BIDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     @@BIDT        EABIDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 01/21/02 **************************************
     C*
     C* UPDATE NET INV DT IF NOT EQUAL TO ZERO
     C*
     C     Q1PRMS        IFGT      24
     C     @@NIDT        IFNE      *ZEROS
     C                   Z-ADD     @@NIDT        EANIDT
     C                   ENDIF
     C                   ENDIF
     C*
     C* UPDATE NET CALCULATED AMOUNT IF NOT EQUAL TO ZERO
     C*
     C     Q1PRMS        IFGT      25
     C     @@NCAM        IFNE      *ZEROS
     C                   Z-ADD     @@NCAM        EANCAM
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/21/02 **************************************
     C*
     C     @@TMBA        IFNE      *ZEROS
     C     @@TMAA        ORNE      *ZEROS
     C*
     C                   SUB       @@TMBA        EABAMT
     C                   ADD       @@TMAA        EABAMT
     C*
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C*** BEG DEL *** 03/17/99 **************************************
     C********** @@SYS     IFNE *BLANK
     C**********           MOVEL@@SYS     EASYS
     C**********           ENDIF
     C*** END DEL *** 03/17/99 **************************************
     C*
     C*** BEGIN ADD *** 11/12/99 **************************************
     C     Q1PRMS        IFGT      23
     C     @@MODE        IFEQ      'D'
     C*** END   ADD *** 11/12/99 **************************************
     C                   MOVEL     Q1USER        EAUSER
     C*** BEGIN ADD *** 11/12/99 **************************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  Q1USER        EAUSER
     C*
     C                   ENDIF
     C*** END   ADD *** 11/12/99 **************************************
     C                   MOVEL     @@PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   UPDATE    PEA01
     C*
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           ELSE
     C                   ENDIF
     C                   ENDIF
     C*** END   CHG *** 09/07/99 **************************************
     C*** BEGIN CHG *** 09/07/99 **************************************
     C     XXFAEA        IFEQ      'W'
     C*** END   CHG *** 09/07/99 **************************************
     C*
     C                   Z-ADD     CMCO#         EACO#
     C                   Z-ADD     CMSUB#        EASUB#
     C                   Z-ADD     CMFND         EAFND
     C                   Z-ADD     CMFYR         EAFYR
     C                   Z-ADD     CMCASE        EACASE
     C                   MOVEL     CMSS#         EASS#
     C                   Z-ADD     CMACDT        EAACDT
     C                   MOVE      'A'           EASTAT
     C*
     C     EAFEIN        IFEQ      *ZEROS
     C     EOKY          CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   ENDIF
     C                   ENDIF
     C*
     C     @@EOB#        IFNE      *ZEROS
     C                   Z-ADD     @@EOB#        EAEOB#
     C                   ENDIF
     C*
     C*** BEG DEL *** 03/03/99 ***************************************
     C********** @@RCN     IFNE *ZEROS
     C**********           Z-ADD@@RCN     EARCN
     C**********           ENDIF
     C*** END DEL *** 03/03/99 ***************************************
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        EABAMT
     C                   ENDIF
     C*** BEGIN ADD *** 08/13/06 **************************************
     C     @@TMBA        IFNE      *ZEROS
     C     @@TMAA        ORNE      *ZEROS
     C*
     C                   SUB       @@TMBA        EABAMT
     C                   ADD       @@TMAA        EABAMT
     C*
     C                   ENDIF
     C*** END   ADD *** 08/13/06 **************************************
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        EANAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        EABHRF
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        EABBLF
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        EABINV
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        EABCAM
     C                   ENDIF
     C*
     C     @@NHRF        IFNE      *BLANK
     C                   MOVEL     @@NHRF        EANHRF
     C                   ENDIF
     C*
     C     @@NBLF        IFNE      *BLANK
     C                   MOVEL     @@NBLF        EANBLF
     C                   ENDIF
     C*
     C     @@NINV        IFNE      *ZEROS
     C                   Z-ADD     @@NINV        EANINV
     C                   ENDIF
     C*
     C     @@SYS         IFNE      *BLANK
     C                   MOVEL     @@SYS         EASYS
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE(P)   #2TRCD        EATRCD
     C                   MOVE(P)   #2STS         EACKST
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     @@PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   Z-ADD     EADT          EATRDT
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   MOVEL     DSTIME        EATIME
     C*
     C*
     C                   MOVEL     Q1USER        EAIUSR
     C                   MOVEL     @@PGM         EAIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAIDT
     C*
     C                   MOVEL     DSTIME        EAITIM
     C*
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           WRITEPEA01
     C**********           FEOD FAEAL1
     C                   WRITE     PEA02
     C                   FEOD      FAEAP
     C*** END   CHG *** 10/25/99 **************************************
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  UPFAEA
     C*
     C*---------------------------------------------------------------
     C* UPREST SUBROUTINE - UPDATE FAHBP, NAPBP, NATRP, NAMBP, AND
     C*                     FAEOP IF PROVIDER BILL CONTROL FLAG NE 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPREST        BEGSR
     C*
     C                   SELECT
     C     @@SYS         WHENEQ    'P'
     C*** BEGIN DEL *** 09/07/99 **************************************
     C********** XXFAEO    OREQ 'U'
     C********** XXFAEO    OREQ 'W'
     C********** XXFAE1    OREQ 'U'
     C********** XXFAE1    OREQ 'W'
     C*** END   DEL *** 09/07/99 **************************************
     C                   EXSR      UPFAEO
     C                   EXSR      UPFAE1
     C*
     C     @@SYS         WHENEQ    'H'
     C*** BEGIN DEL *** 09/07/99 **************************************
     C********** XXFAHB    OREQ 'U'
     C********** XXFAHB    OREQ 'W'
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAEO
     C                   EXSR      UPFAE1
     C*** END   ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAHB
     C*
     C     @@SYS         WHENEQ    'U'
     C*** BEGIN DEL *** 09/07/99 **************************************
     C********** XXNAPB    OREQ 'U'
     C********** XXNAPB    OREQ 'W'
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAEO
     C                   EXSR      UPFAE1
     C*** END   ADD *** 09/07/99 **************************************
     C                   EXSR      UPNAPB
     C*
     C     @@SYS         WHENEQ    'M'
     C*** BEGIN DEL *** 09/07/99 **************************************
     C********** XXNAMB    OREQ 'U'
     C********** XXNAMB    OREQ 'W'
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAEO
     C*** END   ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAE1
     C                   EXSR      UPNAMB
     C*
     C     @@SYS         WHENEQ    'N'
     C*** BEGIN DEL *** 09/07/99 **************************************
     C********** XXNATR    OREQ 'U'
     C********** XXNATR    OREQ 'W'
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   EXSR      UPFAEO
     C                   EXSR      UPFAE1
     C*** END   ADD *** 09/07/99 **************************************
     C                   EXSR      UPNATR
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  UPFAEA
     C*
     C*---------------------------------------------------------------
     C* UPFAEO SUBROUTINE - UPDATE FAEOP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPFAEO        BEGSR
     C*
     C     XXFAEO        IFEQ      'U'
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     @@EOB#        IFNE      *ZEROS
     C                   Z-ADD     @@EOB#        EOEOB#
     C                   ENDIF
     C*
     C     @@RCN         IFNE      *ZEROS
     C                   Z-ADD     @@RCN         EORCN
     C                   ENDIF
     C*
     C                   SUB       @@TMBA        EOBAMT
     C                   ADD       @@TMAA        EOBAMT
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        EOBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        EOHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        EOBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        EOINV#
     C                   ENDIF
     C*** BEGIN ADD *** 11/12/99 **************************************
     C     Q1PRMS        IFGT      23
     C     @@MODE        IFEQ      'D'
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C                   MOVEL     Q1USER        EOUSER
     C*** BEGIN ADD *** 11/12/99 **************************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  Q1USER        EOUSER
     C*
     C                   ENDIF
     C*** END   ADD *** 11/12/99 **************************************
     C                   MOVEL     @@PGM         EOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   MOVEL     DSTIME        EOTIME
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  XXFAEO = U
     C*
     C*---------------------\
     C*  WRITE FAEOP        |
     C*---------------------/
     C     XXFAEO        IFEQ      'W'
     C*
     C                   Z-ADD     CMCO#         EOCO#
     C                   Z-ADD     CMSUB#        EOSUB#
     C                   Z-ADD     CMFND         EOFND
     C                   Z-ADD     CMFYR         EOFYR
     C                   Z-ADD     CMCASE        EOCASE
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVEL     @@SYS         EOSYS
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C     @@EOB#        IFNE      *ZEROS
     C                   Z-ADD     @@EOB#        EOEOB#
     C                   ENDIF
     C*
     C     @@RCN         IFNE      *ZEROS
     C                   Z-ADD     @@RCN         EORCN
     C                   ENDIF
     C*
     C                   SUB       @@TMBA        EOBAMT
     C                   ADD       @@TMAA        EOBAMT
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        EOBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        EOHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        EOBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        EOINV#
     C                   ENDIF
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*
     C**********           CALL 'SA009R'
     C**********           PARM ' '       ##EOBD  1        EOMB DELETE SECURITY
     C**********           PARM ' '       ##EOBI  1        EOMB INTERACIVE PROC.
     C**********           PARM ' '       ##ARIN  1        AUDIT/RECON INSTALLED
     C**********           PARM ' '       ##ACIS  1        AUTO CHECK ISSUANCE
     C**********           PARM ' '       ##PAY0  1        PRINT WCMC EOB PAY>0
     C**********           PARM ' '       ##SDTS  1        ALLOW FUTURE SERV DTS
     C*
B01  C********** @@WCIF    IFEQ 'Y'
     C********** @@WCIN    ANDEQ'Y'
     C********** ##ACIS    ANDEQ'Y'
     C*
     C**********           MOVE CMCO#     #1CO#
     C**********           MOVE CMSUB#    #1SUB#
     C**********           MOVE CMFND     #1FND
     C*
     C**********           CALL 'FA700R'
     C**********           PARM *BLANKS   ##RTCD  1
     C**********           PARM           #1CO#   3
     C**********           PARM *BLANKS   #1WCF#  3
     C**********           PARM           #1SUB#  5
     C**********           PARM           #1FND   3
     C*
B02  C********** ##RTCD    IFEQ *BLANKS
     C*
     C**********           MOVE CMCO#     #2CO#
     C*
     C**********           CALL 'WA034R'
     C**********           PARM           #2CO#   3        Company
     C**********           PARM #1WCF#    #2FND   3        Group #
     C**********           PARM @@SYS     #2SYS   1        SYSTEM (U,M)
     C**********           PARM           #2TRCD  2        TRANSACTION CODE
     C*
     C**********           MOVE #2TRCD    EOTRCD
     C*
E02  C**********           ENDIF                            ##RTCD
     C*
     C**********           ELSE
     C**********
     C*** END   DEL *** 09/07/99 **************************************
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           MOVE *BLANKS   EOTRCD
     C                   MOVE(P)   #2TRCD        EOTRCD
     C*** END   DEL *** 09/07/99 **************************************
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
E01  C**********           ENDIF                            @@SDTS
     C*** END   DEL *** 09/07/99 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C     Q1PRMS        IFGT      17
     C*
     C                   MOVEL     @@CERT        EOCERT
     C                   Z-ADD     @@EXT         EOEXT
     C                   Z-ADD     @@FEIN        EOFEIN
     C                   Z-ADD     @@MOD         EOMOD
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOCERT
     C                   Z-ADD     *ZEROS        EOEXT
     C                   Z-ADD     *ZEROS        EOFEIN
     C                   Z-ADD     *ZEROS        EOMOD
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        EOAMT
     C                   Z-ADD     *ZEROS        EOFRDT
     C                   Z-ADD     *ZEROS        EOTODT
     C                   MOVE      *BLANKS       EOPNAM
     C                   MOVE      *BLANKS       EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   MOVE      *BLANKS       EOCTY
     C                   MOVE      *BLANKS       EOST
     C                   Z-ADD     *ZEROS        EOZIP
     C*
     C                   MOVE      *BLANKS       EOEOB1
     C                   MOVE      *BLANKS       EOEOB2
     C                   MOVE      *BLANKS       EOEOB3
     C                   MOVE      @@SYS         EOSYS
     C                   MOVE      'X'           EOFLG1
     C                   MOVE      'R'           EOFLG2
     C                   MOVE      *BLANKS       EOFLG3
     C                   MOVE      'Y'           EOFLG4
     C                   MOVEL     *BLANKS       EOFLG5
     C                   MOVE      *BLANKS       EOFLG6
     C                   MOVE      *BLANKS       EOFLG7
     C                   MOVE      *BLANKS       EOFLG8
     C                   MOVE      *BLANKS       EOFLG9
     C                   Z-ADD     *ZEROS        EOAMTC
     C*
     C*** BEGIN CHG *** 09/19/03 **************************************
     C********** @@MED     IFEQ 'Y'                        ADD 01/28/99
     C*
     C**********           MOVE *BLANKS   EOSTAT
     C*
     C**********           ELSE
     C*
     C**********           MOVE 'P'       EOSTAT
     C*
     C**********           ENDIF
     C**********
     C                   MOVE      *BLANKS       EOSTAT
     C*** END   CHG *** 09/19/03 **************************************
     C*
     C                   Z-ADD     *ZEROS        EOWCCK
     C                   MOVE      *BLANKS       EODIA1
     C                   MOVE      *BLANKS       EODIA2
     C                   MOVE      *BLANKS       EODIA3
     C                   MOVE      *BLANKS       EODIA4
     C                   MOVE      *BLANKS       EOEMPL
     C                   Z-ADD     *ZEROS        EODFC
     C                   MOVE      *BLANKS       EOFLIC
     C                   MOVE      'N'           EOTAPE
     C                   Z-ADD     1             EO#BLL
     C                   MOVE      *BLANKS       EOCLSF
     C                   MOVE      *BLANKS       EOAST
     C                   MOVE      *BLANKS       EOAUDT
     C                   MOVE      *BLANKS       EOBTFL
     C                   MOVE      'H'           EOHRFL
     C                   MOVE      *BLANKS       EOBLFL
     C                   MOVE      *BLANKS       EONLFL
     C                   MOVE      *BLANKS       EODIFL
     C                   Z-ADD     *ZEROS        EOINV#
     C                   MOVE      *BLANKS       EORMDS
     C                   MOVE      *BLANKS       EORMK1
     C                   MOVE      *BLANKS       EORMK2
     C                   MOVE      *BLANKS       EORMK3
     C                   MOVE      *BLANKS       EOCKCN
     C                   Z-ADD     *ZEROS        EOOTHR
     C                   Z-ADD     *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     *ZEROS        EOBAMT
     C                   MOVE      *BLANKS       EOZIPA
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVEL     'C'           EOBLST
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C     @@SYS         IFEQ      'U'
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C*
     C*** END   ADD *** 09/07/99 **************************************
     C*** BEGIN ADD *** 12/21/99 **************************************
     C                   MOVEL     CMLSN         EOLSN
     C                   MOVEL     CMFSN         EOFSN
     C                   MOVEL     CMMID         EOMID
     C                   MOVEL     CMAST         EOAST
     C*** END   ADD *** 12/21/99 **************************************
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1USER        EOIUSR
     C                   MOVEL     @@PGM         EOPGM
     C                   MOVEL     @@PGM         EOIPGM
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           TIME           DSDTTM
     C*** END   DEL *** 09/07/99 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL     SRDTO         EOIDT
     C*
     C                   MOVEL     DSTIME        EOTIME
     C                   MOVEL     DSTIME        EOITIM
     C*
     C                   WRITE     PEO01
     C                   FEOD      FAEOLG
     C*
     C                   ENDIF                                                  XXFAEO = W
     C                   ENDSR                                                  UPFAEO
     C*
     C*
     C*---------------------------------------------------------------
     C* UPFAE1 SUBROUTINE - UPDATE FAE1P IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPFAE1        BEGSR
     C*
     C*---------------------\
     C*  UPDATE FAE1P       |
     C*---------------------/
     C*
     C     XXFAE1        IFEQ      'U'
     C*
     C     E1KY          CHAIN     PE101                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        E1AMT1
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVELQ1USER    EOUSER
     C**********           MOVEL@@PGM     EOPGM
     C                   MOVEL     Q1USER        E1USER
     C                   MOVEL     @@PGM         E1PGM
     C*** END   CHG *** 09/23/99 **************************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVELSRDTO     EODT
     C                   MOVEL     SRDTO         E1DT
     C*** END   CHG *** 09/23/99 **************************************
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVELDSTIME    EOTIME
     C*
     C**********           UPDATPEO01
     C                   MOVEL     DSTIME        E1TIME
     C*
     C                   UPDATE    PE101
     C*** END   CHG *** 09/23/99 **************************************
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  XXFAE1 = U
     C*
     C*---------------------\
     C*  WRITE FAE1P        |
     C*---------------------/
     C     XXFAE1        IFEQ      'W'
     C*
     C                   Z-ADD     CMCO#         E1CO#
     C                   Z-ADD     CMSUB#        E1SUB#
     C                   Z-ADD     CMFND         E1FND
     C                   Z-ADD     CMFYR         E1FYR
     C                   Z-ADD     CMCASE        E1CASE
     C                   Z-ADD     @@EOB#        E1EOB#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1CRDT
     C*
     C                   MOVEL     Q1USER        E1USER
     C                   MOVEL     Q1USER        E1IUSR
     C                   MOVEL     @@PGM         E1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C                   MOVEL     SRDTO         E1IDT
     C*
     C                   MOVEL     DSTIME        E1TIME
     C*
     C                   WRITE     PE101
     C                   FEOD      FAE1L
     C*
     C                   ENDIF                                                  XXFAE1 = W
     C*
     C                   ENDSR                                                  UPFAE1
     C*
     C*
     C*---------------------------------------------------------------
     C* UPFAHB SUBROUTINE - UPDATE FAHBP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPFAHB        BEGSR
     C*
     C*
     C*---------------------\
     C*  UPDATE FAHBP       |
     C*---------------------/
     C*
     C     XXFAHB        IFEQ      'U'
     C*
     C     HBKY          CHAIN     PHB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   SUB       @@TMBA        HB1AAM
     C                   ADD       @@TMAA        HB1AAM
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        HB1AAM
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        HBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        HBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        HBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        HB1CAM
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        HBUSER
     C                   MOVEL     @@PGM         HBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBDT
     C*
     C                   MOVEL     DSTIME        HBTIME
     C*
     C                   UPDATE    PHB01
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  XXFAHB = U
     C*
     C*---------------------\
     C*  WRITE  FAHBP       |
     C*---------------------/
     C*
     C     XXFAHB        IFEQ      'W'
     C*
     C                   Z-ADD     CMCO#         HBCO#
     C                   Z-ADD     CMSUB#        HBSUB#
     C                   Z-ADD     CMFND         HBFND
     C                   Z-ADD     CMFYR         HBFYR
     C                   Z-ADD     CMCASE        HBCASE
     C*
     C                   SUB       @@TMBA        HB1AAM
     C                   ADD       @@TMAA        HB1AAM
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        HB1AAM
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        HBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        HBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        HBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        HB1CAM
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        HBUSER
     C                   MOVEL     Q1USER        HBIUSR
     C                   MOVEL     @@PGM         HBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBDT
     C                   MOVEL     SRDTO         HBIDT
     C*
     C                   MOVEL     DSTIME        HBTIME
     C*
     C                   WRITE     PHB01
     C                   FEOD      FAHBL
     C*
     C                   ENDIF                                                  XXFAHB = W
     C*
     C                   ENDSR                                                  UPFAHB
     C*
     C*---------------------------------------------------------------
     C* UPNAPB SUBROUTINE - UPDATE NAPBP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPNAPB        BEGSR
     C*
     C*---------------------\
     C*  UPDATE NAPBP       |
     C*---------------------/
     C*
     C     XXNAPB        IFEQ      'U'
     C*
     C     PBKY          CHAIN     PPB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   SUB       @@TMBA        PBBAMT
     C                   ADD       @@TMAA        PBBAMT
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        PBBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        PBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        PBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        PBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        PBCAMT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        PBUSER
     C                   MOVEL     @@PGM         PBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PBDT
     C*
     C                   MOVEL     DSTIME        PBTIME
     C*
     C                   UPDATE    PPB01
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  XXNAPB = U
     C*
     C*---------------------\
     C*  WRITE  NAPBP       |
     C*---------------------/
     C*
     C     XXNAPB        IFEQ      'W'
     C*
     C     EOKY          CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     EOCERT        PBCERT
     C                   Z-ADD     EOEXT         PBEXT
     C                   MOVE      EOFEIN        PBFEIN
     C                   Z-ADD     EOMOD         PBMOD
     C                   ENDIF
     C*
     C                   Z-ADD     CMCO#         PBCO#
     C                   Z-ADD     CMSUB#        PBSUB#
     C                   Z-ADD     CMFND         PBFND
     C                   Z-ADD     CMFYR         PBFYR
     C                   Z-ADD     CMCASE        PBCASE
     C                   MOVEL(P)  CMSS#         PBSS#
     C                   Z-ADD     CMACDT        PBACDT
     C*
     C                   MOVE      'A'           PBSTAT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PBTRDT
     C*
     C     @@EOB#        IFNE      *ZEROS
     C                   Z-ADD     @@EOB#        PBEOB#
     C                   ENDIF
     C*
     C                   SUB       @@TMBA        PBBAMT
     C                   ADD       @@TMAA        PBBAMT
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        PBBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        PBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        PBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        PBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        PBCAMT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        PBUSER
     C                   MOVEL     Q1USER        PBIUSR
     C                   MOVEL     @@PGM         PBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PBDT
     C                   MOVEL     SRDTO         PBIDT
     C*
     C                   MOVEL     DSTIME        PBTIME
     C*
     C                   WRITE     PPB01
     C                   FEOD      NAPBL1
     C*
     C                   ENDIF                                                  XXNAPB = W
     C*
     C                   ENDSR                                                  UPNAPB
     C*
     C*---------------------------------------------------------------
     C* UPNAMB SUBROUTINE - UPDATE NAMBP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPNAMB        BEGSR
     C*
     C*---------------------\
     C*  UPDATE NAMBP       |
     C*---------------------/
     C*
     C     XXNAMB        IFEQ      'U'
     C*
     C     MBKY          CHAIN     PMB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   SUB       @@TMBA        MBBAMT
     C                   ADD       @@TMAA        MBBAMT
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        MBBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        MBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        MBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        MBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        MBCAMT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        MBUSER
     C                   MOVEL     @@PGM         MBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C*
     C                   MOVEL     DSTIME        MBTIME
     C*
     C                   UPDATE    PMB01
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  XXNAMB = U
     C*
     C*---------------------\
     C*  WRITE  NAMBP       |
     C*---------------------/
     C*
     C     XXNAMB        IFEQ      'W'
     C*
     C                   Z-ADD     CMCO#         MBCO#
     C                   Z-ADD     CMSUB#        MBSUB#
     C                   Z-ADD     CMFND         MBFND
     C                   Z-ADD     CMFYR         MBFYR
     C                   Z-ADD     CMCASE        MBCASE
     C                   MOVE      CMSS#         MBSS#
     C                   Z-ADD     CMACDT        MBACDT
     C                   Z-ADD     @@EOB#        MBEOB#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBTRDT
     C*
     C                   SUB       @@TMBA        MBBAMT
     C                   ADD       @@TMAA        MBBAMT
     C*
     C                   MOVE      @@BHRF        MBHRFL
     C                   MOVE      'A'           MBSTAT
     C*
     C                   MOVEL     Q1USER        MBUSER
     C                   MOVEL     Q1USER        MBIUSR
     C                   MOVEL     @@PGM         MBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C                   MOVEL     SRDTO         MBIDT
     C*
     C                   MOVEL     DSTIME        MBTIME
     C                   MOVEL     DSTIME        MBITIM
     C*
     C                   WRITE     PMB01
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           FEOD NAMBL
     C                   FEOD      NAMBL1
     C*** END   CHG *** 09/07/99 **************************************
     C*
     C                   ENDIF                                                  XXNAMB = W
     C*
     C                   ENDSR                                                  UPNAMB
     C*
     C*---------------------------------------------------------------
     C* UPNATR SUBROUTINE - UPDATE NATRP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPNATR        BEGSR
     C*
     C*---------------------\
     C*  UPDATE NATRP       |
     C*---------------------/
     C*
     C     XXNATR        IFEQ      'U'
     C*
     C     TRKY          CHAIN     PTR01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   SUB       @@TMBA        TRBAMT
     C                   ADD       @@TMAA        TRBAMT
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        TRBAMT
     C                   ENDIF
     C*
     C     @@NHRF        IFNE      *BLANK
     C                   MOVEL     @@NHRF        TRHRFL
     C                   ENDIF
     C*
     C     @@NBLF        IFNE      *BLANK
     C                   MOVEL     @@NBLF        TRBLFL
     C                   ENDIF
     C*
     C     @@NINV        IFNE      *ZEROS
     C                   Z-ADD     @@NINV        TRINV#
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        TRUSER
     C                   MOVEL     @@PGM         TRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C*
     C                   MOVEL     DSTIME        TRTIME
     C*
     C                   UPDATE    PTR01
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  XXNATR = U
     C*
     C*
     C*---------------------\
     C*  WRITE  NATRP       |
     C*---------------------/
     C*
     C     XXNATR        IFEQ      'W'
     C*
     C                   Z-ADD     CMCO#         TRCO#
     C                   Z-ADD     CMSUB#        TRSUB#
     C                   Z-ADD     CMFND         TRFND
     C                   Z-ADD     CMFYR         TRFYR
     C                   Z-ADD     CMCASE        TRCASE
     C*
     C                   SUB       @@TMBA        TRBAMT
     C                   ADD       @@TMAA        TRBAMT
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        TRBAMT
     C                   ENDIF
     C*
     C     @@NHRF        IFNE      *BLANK
     C                   MOVEL     @@NHRF        TRHRFL
     C                   ENDIF
     C*
     C     @@NBLF        IFNE      *BLANK
     C                   MOVEL     @@NBLF        TRBLFL
     C                   ENDIF
     C*
     C     @@NINV        IFNE      *ZEROS
     C                   Z-ADD     @@NINV        TRINV#
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        TRUSER
     C                   MOVEL     Q1USER        TRIUSR
     C                   MOVEL     @@PGM         TRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C                   MOVEL     SRDTO         TRIDT
     C*
     C                   MOVEL     DSTIME        TRTIME
     C*
     C                   WRITE     PTR01
     C                   FEOD      NATRL
     C*
     C                   ENDIF                                                  XXNATR = W
     C*
     C                   ENDSR                                                  UPNATR
     C*
     C*================================================================
     C* DELEB1 - Deletes record from EOMB auxiliary file FAE1P.
     C*================================================================
     C     DELEB1        BEGSR
     C*
     C     DBRRNB        CHAIN     PE102                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PE102
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DELMBT - Deletes record from Medical Bill Trans. File NAMBP.
     C*================================================================
     C     DELMBT        BEGSR
     C*
     C     DBRRNC        CHAIN     PMB02                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PMB02
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DELFAE - Deletes record from Bill Header Amount  File FAEAP.
     C*================================================================
     C     DELFAE        BEGSR
     C*
     C*** BEGIN CHG *** 09/07/99 **************************************
     C********** DBRRN     CHAINPEA02                99
     C     EAKY          CHAIN     PEA01                              99
     C*** END   CHG *** 09/07/99 **************************************
     C     *IN99         IFEQ      *OFF
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           DELETPEA02
     C                   DELETE    PEA01
     C*** END   CHG *** 09/07/99 **************************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*================================================================
     C* DELFAO - Deletes record from Bill Header Amount  File FAEOP.
     C*================================================================
     C     DELFAO        BEGSR
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/07/99 **************************************
     C*================================================================
     C* DELUPD - Deletes records from FAEAP, FAE1P and NAMBP.
     C*================================================================
     C     DELUPD        BEGSR
     C*
     C* Deletes record from EOMB auxiliary file (FAE1OP).
     C*
     C                   EXSR      DELEB1
     C*
     C* Deletes record from Medical Mgmt Bill transfer file (NAMBP).
     C*
     C                   EXSR      DELMBT
     C*
     C* Deletes record from Bill Header Amount File (FAEAP).
     C*
     C                   EXSR      DELFAE
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C* Deletes record from Bill Header Amount File (FAEOP).
     C*
     C                   EXSR      DELFAO
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ENDSR
