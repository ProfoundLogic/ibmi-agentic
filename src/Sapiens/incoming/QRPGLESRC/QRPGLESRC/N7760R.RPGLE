     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H* 01/27/00  C:62  Project: Y2K
     H*                 CHANGE DATES TO MDYY.
     H*
     H* 11/10/99  C:62  PROJECT: MC025 20372
     H*                 MAKE ROOM FOR SUMMARY TOTALS.
     H*
     H* 11/01/99  C:62  PROJECT: MC025 20372
     H*                 CONDITION EXCPT STATEMENT FOR ERMSG3
     H*
     H* 04/21/99  C:94  PROJECT: SUP 17727
     H*                 ADD DT CONVERSION SO THAT IT WOULD BREAK INTO
     H*                 WEEKS
     H*
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*
     H* 04/12/95  C:20  REQ/PROJ: NWMNT                                       *
     H*                 1. MODIFICATIONS TO PROCESS MANUAL INVOICES CORRECTLY *
     H*                    PROGRAM WAS NOT PROCESSING IF NO EOMB RECORD WAS   *
     H*                    FOUND.  THIS CAUSED INVALID RESULTS ON THE REPORT  *
     H*                    BECAUSE AN INVOICE AMOUNT WAS FOUND.  MANUAL       *
     H*                    INVOICES DO NOT CREATE AN EOMB RECORD.             *
     H*************************************************************************
     H/TITLE N7750R - INVOICED ACTIVITY REPORT BY COMPANY              AM
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFAEOL8    IF   E           K DISK
     F*** BEGIN ADD *** 11/03/99 **************************************
     FFAEAL11   IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** END   ADD *** 11/03/99 **************************************
     FNMARL3    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS1)
     F                                     OFLIND(*INOA)
     F                                     USROPN
     F*** BEGIN CHG *** 11/10/99 **************************************
     F*SYSPRT1O   F     132     OB     PRINTER      KINFDS PRTDS2     UC
     FQSYSPRT1  O    F  148        PRINTER INFDS(PRTDS2)
     F                                     OFLIND(*INOB)
     F                                     USROPN
     F*** END   CHG *** 11/10/99 **************************************

     D                 DS
     D*** BEGIN CHG *** 01/27/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 01/27/00 **************************************
     D PRTDS1          DS
     D  FIL11                  1    200
     D  FIL12                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D PRTDS2          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D                 DS
     D  CSFLD                  1     10  0
     D  CSFYR                  1      3  0
     D  CSNUM                  4     10  0
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXINV#
     C                   KFLD                    XXEOB#
     C*** BEGIN ADD *** 11/03/99 **************************************
     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EOKY2         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    ARCO#
     C                   KFLD                    ARSUB#
     C                   KFLD                    ARFND
     C                   KFLD                    ARINV#
     C                   KFLD                    AREOB#
     C                   KFLD                    ARRCN
     C*** END   ADD *** 11/03/99 **************************************
     C*
     C*   KEY LIST FOR THE A/R MASTER FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRTYP            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    UPDT          X1TRDT
     C     *LIKE         DEFINE    UPDT          X1WKND
     C     *LIKE         DEFINE    UPDT          X17FWD
     C*** BEGIN ADD *** 11/03/99 **************************************
     C*
     C*** GET SYSTEM INFO
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
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 11/03/99 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQRTYP        XXRTYP            1
     C                   MOVE      QQFRDT        X2FRDT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX2FRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFRDT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTODT        X2TODT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX2TODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTODT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*
     C*  DETERMINE IF ONE SUBSCRIBER OR ALL SUBSCRIBERS
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL SUBSC
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE SUBSC
     C                   END
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*
     C*  DETERMINE WHICH OUTPUT FILES WILL BE USED DETAIL, SUMMARY
     C*  OR BOTH, THEN GET ROUTING INFORMATION FOR EACH REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C     XXRTYP        IFEQ      'B'
     C                   MOVE      '1'           *IN05
     C                   MOVE      '1'           *IN06
     C                   END
     C     XXRTYP        IFEQ      'D'
     C                   MOVE      '1'           *IN05
     C                   END
     C     XXRTYP        IFEQ      'S'
     C                   MOVE      '1'           *IN06
     C                   END
     C*
     C     *IN05         IFEQ      '1'
     C                   OPEN      QSYSPRT
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        XXTTL1           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C                   END
     C*
     C     *IN06         IFEQ      '1'
     C                   OPEN      QSYSPRT1
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        XXTTL2           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE2
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*  GET SUBSCRIBER RECORD (CARRIER INFO)
     C*
     C     SMKEY         CHAIN     PSM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C*  ZERO OUT EOB DETAIL AMOUNTS (AMOUNTS FOR SINGLE EOB RECORD)
     C*
     C                   Z-ADD     0             X1#BLL            4 0
     C                   Z-ADD     0             X1AMTC            9 2
     C                   Z-ADD     0             X1ASAV            9 2
     C                   Z-ADD     0             X1AMTR            9 2
     C                   Z-ADD     0             X1IAMT            9 2
     C                   Z-ADD     0             X1NSAV            9 2
     C                   Z-ADD     0             X1PSAV            5 3
     C                   Z-ADD     0             X1BRAT            7 2
     C                   Z-ADD     0             X1SBLL            7 2
     C                   Z-ADD     0             X1ROI             4 2
     C*
     C*  ZERO OUT INVOICE LEVEL TOTALS
     C*
     C                   Z-ADD     0             X2#BLL            4 0
     C                   Z-ADD     0             X2AMTC            9 2
     C                   Z-ADD     0             X2ASAV            9 2
     C                   Z-ADD     0             X2AMTR            9 2
     C                   Z-ADD     0             X2IAMT            9 2
     C                   Z-ADD     0             X2NSAV            9 2
     C                   Z-ADD     0             X2PSAV            5 3
     C                   Z-ADD     0             X2BRAT            7 2
     C                   Z-ADD     0             X2SBLL            7 2
     C                   Z-ADD     0             X2ROI             4 2
     C*
     C*  ZERO OUT WEEKLY LEVEL TOTALS
     C*
     C                   Z-ADD     0             XXIVC3            5 0
     C                   Z-ADD     0             X3#BLL            4 0
     C*** BEGIN CHG *** 11/10/99 **************************************
     C**********           Z-ADD0         X3AMTC  92
     C**********           Z-ADD0         X3ASAV  92
     C**********           Z-ADD0         X3AMTR  92
     C**********           Z-ADD0         X3IAMT  92
     C**********           Z-ADD0         X3NSAV  92
     C                   Z-ADD     0             X3AMTC           11 2
     C                   Z-ADD     0             X3ASAV           11 2
     C                   Z-ADD     0             X3AMTR           11 2
     C                   Z-ADD     0             X3IAMT           11 2
     C                   Z-ADD     0             X3NSAV           11 2
     C*** END   CHG *** 11/10/99 **************************************
     C                   Z-ADD     0             X3PSAV            5 3
     C                   Z-ADD     0             X3BRAT            7 2
     C                   Z-ADD     0             X3SBLL            7 2
     C                   Z-ADD     0             X3ROI             4 2
     C*
     C*  ZERO OUT FINAL TOTALS
     C*
     C                   Z-ADD     0             XXIVC4            5 0
     C                   Z-ADD     0             X4#BLL            4 0
     C*** BEGIN CHG *** 11/10/99 **************************************
     C**********           Z-ADD0         X4AMTC  92
     C**********           Z-ADD0         X4ASAV  92
     C**********           Z-ADD0         X4AMTR  92
     C**********           Z-ADD0         X4IAMT  92
     C**********           Z-ADD0         X4NSAV  92
     C                   Z-ADD     0             X4AMTC           11 2
     C                   Z-ADD     0             X4ASAV           11 2
     C                   Z-ADD     0             X4AMTR           11 2
     C                   Z-ADD     0             X4IAMT           11 2
     C                   Z-ADD     0             X4NSAV           11 2
     C*** END   CHG *** 11/10/99 **************************************
     C                   Z-ADD     0             X4PSAV            5 3
     C                   Z-ADD     0             X4BRAT            7 2
     C                   Z-ADD     0             X4SBLL            7 2
     C                   Z-ADD     0             X4ROI             4 2
     C*
     C*  READ A/R FILE
     C*
     C   80              READ      PAR01                                  60     ALL COMPANY
     C  N80ARKEY         CHAIN     PAR01                              60         ONE COMPANY
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C*
     C     *IN05         IFEQ      '1'
     C                   EXCEPT    ERMSG1
     C                   END
     C*
     C     *IN06         IFEQ      '1'
     C                   EXCEPT    ERMSG2
     C                   END
     C                   GOTO      TERM2
     C                   ELSE
     C*
     C*  AN A/R RECORD WAS FOUND PRINT REPORT HDGS
     C*
     C     *IN05         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DDETHD
     C                   END
     C     *IN06         IFEQ      '1'
     C                   EXCEPT    HEAD2
     C                   EXCEPT    SDETHD
     C                   END
     C                   END                                                    *IN60
     C*
     C*
     C*  INITIALIZE PROGRAM VARIABLES AND A/R RELATED VARIABLES
     C*
     C                   Z-ADD     ARFND         XXFND             3 0
     C                   Z-ADD     ARFYR         XXFYR             3 0
     C                   Z-ADD     ARFYR         XXYR              2 0
     C                   Z-ADD     ARCASE        XXCASE            7 0
     C                   Z-ADD     AREOB#        XXEOB#
     C                   Z-ADD     ARTRDT        X1TRDT
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XXINV#  50
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVEL     ARDESC        XXDESC           30
     C*** BEG ADD *** 04/17/99 ***************************************
     C*
     C                   MOVE      '0'           *IN15
     C                   MOVE      '0'           *IN35
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADD0         XXOLD   50
     C                   Z-ADD     0             XXOLD             9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   Z-ADD     6             XX1WK             5 0
     C                   Z-ADD     XXFYR         CSFYR
     C                   Z-ADD     XXCASE        CSNUM
     C*
     C                   Z-ADD     X1TRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXTRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   Z-ADD     X2FRDT        X2STDT            8 0
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           Z-ADDXXFRDT    XXSTRT  60
     C                   Z-ADD     X2FRDT        XXSTRT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*  SKIP TO FIRST WEEK CONTAINING AN A/R RECORD
     C*
     C     *IN35         DOWEQ     '0'
     C                   EXSR      SRADV
     C                   END
     C*
     C                   Z-ADD     X2STDT        X2WKND            8 0
     C*
     C*  GET INVOICE INFORMATION
     C*
     C*** BEGIN CHG *** 11/03/99 **************************************
     C********** EOKEY     CHAINPEO01                51
     C  N16EOKEY         CHAIN     PEO01                              51
     C   16EAKY          CHAIN     PEA01                              51
     C   16
     CANN51EOKY2         CHAIN     PEO02                              51
     C*** END   CHG *** 11/03/99 **************************************
     C     *IN51         IFEQ      '0'
     C*
     C                   ADD       1             XXIVC3
     C*
     C*  INITIALIZE EOB RELATED PROGRAM VARIABLES
     C*
     C     EO#BLL        IFEQ      0
     C                   Z-ADD     1             X1#BLL            4 0
     C                   ELSE
     C                   Z-ADD     EO#BLL        X1#BLL            4 0
     C                   END
     C                   Z-ADD     ARAMT         X1IAMT            9 2
     C*** BEGIN ADD *** 11/03/99 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C     EAPAID        ADD       EANDIS        X1AMTR
     C                   Z-ADD     EACHRG        X1AMTC
     C*
     C                   ELSE
     C*** END   ADD *** 11/03/99 **************************************
     C                   Z-ADD     EOAMTC        X1AMTC            9 2
     C                   Z-ADD     EOAMT         X1AMTR            9 2
     C*** BEGIN ADD *** 11/03/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/03/99 **************************************
     C                   ELSE
     C     ARSYS         IFEQ      'N'
     C                   Z-ADD     *ZEROS        X1#BLL
     C                   Z-ADD     *ZEROS        X1AMTC
     C                   Z-ADD     *ZEROS        X1AMTR
     C                   Z-ADD     ARAMT         X1IAMT
     C                   ADD       1             XXIVC3
     C                   END
     C     ARTRCD        IFEQ      'MI'
     C                   Z-ADD     *ZEROS        X1#BLL
     C                   Z-ADD     *ZEROS        X1AMTC
     C                   Z-ADD     *ZEROS        X1AMTR
     C                   Z-ADD     ARAMT         X1IAMT
     C                   ADD       1             XXIVC3
     C                   END                                                    END ARTRCD EQ 'MI'
     C                   END
     C*
     C*  MAIN PROGRAM LOOP. THE A/R RECORD AND EOB RECORD JUST
     C*  READ ARE USED TO COMPUTE A SUM FOR THAT INVOICE IN THE
     C*  A/R FILE. INVOICE TOTALS ARE PRINTED ONCE A NEW INVOICE
     C*  IS ENCOUNTERED.  IF THE NEW INVOICE FALLS WITHIN A NEW
     C*  WEEK PERIOD AS DETERMINED BY THE FROM DATE
     C*  A DETAIL TOTAL LINE AND A SUMMARY DETAIL LINE ARE
     C*  OUTPUT (IF BOTH ARE REQUESTED). FINAL TOTALS ARE PRINTED
     C*  UPON EXITING THE WHILE MORE A/R RECORDS LOOP.
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  SUM EOB RECORD AMOUNTS FOR CURRENT INVOICE
     C*
     C     *IN51         IFEQ      '0'
     C     ARSYS         OREQ      'N'
     C     ARTRCD        OREQ      'MI'
     C                   EXSR      SRADDI
     C                   END
     C*
     C                   Z-ADD     ARINV#        XXOLD
     C*
     C*  READ A/R FILE
     C*
     C   80              READ      PAR01                                  60     ALL COMPANY
     C  N80ARKEY         READE     PAR01                                  60     ONE COMPANY
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     XXOLD         IFNE      ARINV#
     C*
     C*  INCREMENT INVOICE COUNT
     C*
     C                   ADD       1             XXIVC3
     C*
     C*  CALCULATE SPECIAL INVOICE AMOUNTS AT INVOICE LEVEL
     C*
     C     X2AMTC        SUB       X2AMTR        X2ASAV
     C     X2ASAV        SUB       X2IAMT        X2NSAV
     C     X2AMTC        IFNE      0
     C     X2ASAV        DIV(H)    X2AMTC        X2PSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        X2PSAV
     C                   END
     C     X2PSAV        MULT      100           X2PSAV
     C     X2#BLL        IFNE      0
     C     X2IAMT        DIV(H)    X2#BLL        X2BRAT
     C                   END
     C     X2#BLL        IFNE      0
     C     X2ASAV        DIV(H)    X2#BLL        X2SBLL
     C                   END
     C     X2IAMT        IFNE      0
     C     X2ASAV        DIV(H)    X2IAMT        X2ROI
     C                   END
     C*
     C*  PRINT ALL EOB SUMS AND CALCULATED TOTALS FROM ABOVE FOR
     C*  CURRENT INVOICE BEFORE A NEW ONE IS READ.
     C*
     C                   EXSR      SRINVC
     C                   END                                                    IF INV CHG
     C*
     C                   Z-ADD     ARFND         XXFND             3 0
     C                   Z-ADD     ARFYR         XXFYR             3 0
     C                   Z-ADD     ARFYR         XXYR              2 0
     C                   Z-ADD     ARCASE        XXCASE            7 0
     C                   Z-ADD     AREOB#        XXEOB#
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XXINV#  50
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVEL     ARDESC        XXDESC           30
     C*** BEG ADD *** 04/21/99 ****************************************
     C                   Z-ADD     ARTRDT        X1TRDT
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXTRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*** END ADD *** 04/21/99 ****************************************
     C*
     C                   Z-ADD     XXFYR         CSFYR
     C                   Z-ADD     XXCASE        CSNUM
     C*
     C                   END
     C*
     C**********************************
     C*  READ EOB RECORD               *
     C**********************************
     C*** BEGIN CHG *** 11/03/99 **************************************
     C********** EOKEY     CHAINPEO01                51     ALL COMPANY
     C  N16EOKEY         CHAIN     PEO01                              51
     C   16EAKY          CHAIN     PEA01                              51
     C   16
     CANN51EOKY2         CHAIN     PEO02                              51
     C*** END   CHG *** 11/03/99 **************************************
     C     *IN51         IFEQ      '0'
     C*
     C     EO#BLL        IFEQ      0
     C                   Z-ADD     1             X1#BLL
     C                   ELSE
     C                   Z-ADD     EO#BLL        X1#BLL
     C                   END
     C                   Z-ADD     ARAMT         X1IAMT            9 2
     C*** BEGIN ADD *** 11/03/99 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C     EAPAID        ADD       EANDIS        X1AMTR
     C                   Z-ADD     EACHRG        X1AMTC
     C*
     C                   ELSE
     C*** END   ADD *** 11/03/99 **************************************
     C                   Z-ADD     EOAMT         X1AMTR
     C                   Z-ADD     EOAMTC        X1AMTC
     C*** BEGIN ADD *** 11/03/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/03/99 **************************************
     C                   ELSE
     C     ARSYS         IFEQ      'N'
     C                   Z-ADD     *ZEROS        X1#BLL
     C                   Z-ADD     *ZEROS        X1AMTC
     C                   Z-ADD     *ZEROS        X1AMTR
     C                   Z-ADD     ARAMT         X1IAMT
     C                   END
     C     ARTRCD        IFEQ      'MI'
     C                   Z-ADD     *ZEROS        X1#BLL
     C                   Z-ADD     *ZEROS        X1AMTC
     C                   Z-ADD     *ZEROS        X1AMTR
     C                   Z-ADD     ARAMT         X1IAMT
     C                   END                                                    END ARTRCD EQ 'MI'
     C                   END
     C*
     C                   END                                                    END DOWEQ 60
     C**********************************
     C*     END THE MAIN PROG LOOP     *
     C**********************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C     *IN15         IFEQ      '1'
     C                   ADD       X2#BLL        X3#BLL
     C                   ADD       X2AMTC        X3AMTC
     C                   ADD       X2AMTR        X3AMTR
     C                   ADD       X2IAMT        X3IAMT
     C*
     C     *INOA         IFEQ      '1'
     C     *IN05         ANDEQ     '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DDETHD
     C                   MOVE      '0'           *INOA
     C                   END
     C     *IN05         IFEQ      '1'
     C                   Z-ADD     X2PSAV        X2PPRT            3 1
     C                   EXCEPT    DDET
     C                   END
     C*
     C                   ADD       XXIVC3        XXIVC4
     C                   ADD       X3#BLL        X4#BLL
     C                   ADD       X3AMTC        X4AMTC
     C                   ADD       X3AMTR        X4AMTR
     C                   ADD       X3IAMT        X4IAMT
     C*
     C*  CALCULATE SPECIAL INVOICE AMOUNTS AT WEEK LEVEL
     C*
     C     X3AMTC        SUB       X3AMTR        X3ASAV
     C     X3ASAV        SUB       X3IAMT        X3NSAV
     C     X3AMTC        IFNE      0
     C     X3ASAV        DIV(H)    X3AMTC        X3PSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        X3PSAV
     C                   END
     C     X3PSAV        MULT      100           X3PSAV
     C     X3#BLL        IFNE      0
     C     X3IAMT        DIV(H)    X3#BLL        X3BRAT
     C                   END
     C     X3#BLL        IFNE      0
     C     X3ASAV        DIV(H)    X3#BLL        X3SBLL
     C                   END
     C     X3IAMT        IFNE      0
     C     X3ASAV        DIV(H)    X3IAMT        X3ROI
     C                   END
     C*
     C     *IN05         IFEQ      '1'
     C                   Z-ADD     X3PSAV        X3PPRT            3 1
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DTOT
     C                   END
     C     *INOB         IFEQ      '1'
     C     *IN06         ANDEQ     '1'
     C                   EXCEPT    HEAD2
     C                   EXCEPT    SDETHD
     C                   MOVE      '0'           *INOB
     C                   END
     C     *IN06         IFEQ      '1'
     C                   Z-ADD     X3PSAV        X3PPRT            3 1
     C                   EXCEPT    SDET
     C                   END
     C*
     C*
     C*  CALCULATE SPECIAL INVOICE AMOUNTS AT TOTAL LEVEL
     C*
     C     X4AMTC        SUB       X4AMTR        X4ASAV
     C     X4ASAV        SUB       X4IAMT        X4NSAV
     C     X4AMTC        IFNE      0
     C     X4ASAV        DIV(H)    X4AMTC        X4PSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        X4PSAV
     C                   END
     C     X4PSAV        MULT      100           X4PSAV
     C     X4#BLL        IFNE      0
     C     X4IAMT        DIV(H)    X4#BLL        X4BRAT
     C                   END
     C     X4#BLL        IFNE      0
     C     X4ASAV        DIV(H)    X4#BLL        X4SBLL
     C                   END
     C     X4IAMT        IFNE      0
     C     X4ASAV        DIV(H)    X4IAMT        X4ROI
     C                   END
     C     *IN06         IFEQ      '1'
     C                   Z-ADD     X4PSAV        X4PPRT            3 1
     C                   EXCEPT    STOT
     C                   END
     C     *IN05         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DFTOT
     C                   END
     C                   ELSE
     C*** BEGIN CHG *** 11/01/99 **************************************
     C**********           EXCPTERMSG3
     C   06              EXCEPT    ERMSG3
     C*** BEGIN CHG *** 11/01/99 **************************************
     C                   END                                                    *IN15
     C     TERM2         TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (xxrtyp = 'B' or xxrtyp = 'D');
          Except Route1;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (xxrtyp = 'B' or xxrtyp = 'S');
          Except Route2;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C**********************************
     C*     END OF THE PROGRAM         *
     C**********************************
     C*
     C*  SUM TOTALS FOR CURRENT INVOICE
     C*
     C     SRADDI        BEGSR
     C*
     C                   MOVE      '1'           *IN15
     C                   ADD       X1#BLL        X2#BLL
     C                   ADD       X1AMTC        X2AMTC
     C                   ADD       X1AMTR        X2AMTR
     C                   ADD       X1IAMT        X2IAMT
     C*
     C                   ENDSR
     C*
     C*  INVOICE CHANGE, ADD TO WEEKLY TOTALS PRINT DETAIL LINE
     C*  AND CHECK TO SEE IF THE END OF CURRENT WEEK HAS BEEN
     C*  REACHED
     C*
     C     SRINVC        BEGSR
     C*
     C                   Z-ADD     X2WKND        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1WKND
     C*
     C     X1TRDT        IFGT      X1WKND
     C*
     C                   Z-ADD     X1WKND        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXWKND  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXWKND            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*  CALCULATE SPECIAL INVOICE AMOUNTS AT WEEK LEVEL
     C*
     C     X3AMTC        SUB       X3AMTR        X3ASAV
     C     X3ASAV        SUB       X3IAMT        X3NSAV
     C     X3AMTC        IFNE      0
     C     X3ASAV        DIV(H)    X3AMTC        X3PSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        X3PSAV
     C                   END
     C     X3PSAV        MULT      100           X3PSAV
     C     X3#BLL        IFNE      0
     C     X3IAMT        DIV(H)    X3#BLL        X3BRAT
     C                   END
     C     X3#BLL        IFNE      0
     C     X3ASAV        DIV(H)    X3#BLL        X3SBLL
     C                   END
     C     X3IAMT        IFNE      0
     C     X3ASAV        DIV(H)    X3IAMT        X3ROI
     C                   END
     C*
     C*  PRINT WEEKLY TOTALS
     C*
     C                   EXSR      SRWKB
     C*
     C                   MOVE      '0'           *IN35
     C                   Z-ADD     X2WKND        X2STDT
     C     *IN35         DOWEQ     '0'
     C                   EXSR      SRADV
     C                   END
     C                   Z-ADD     X2STDT        X2WKND
     C*
     C                   ADD       X2#BLL        X3#BLL
     C                   ADD       X2AMTC        X3AMTC
     C                   ADD       X2AMTR        X3AMTR
     C                   ADD       X2IAMT        X3IAMT
     C*
     C     *INOA         IFEQ      '1'
     C     *IN05         ANDEQ     '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DDETHD
     C                   MOVE      '0'           *INOA
     C                   END
     C     *IN05         IFEQ      '1'
     C                   Z-ADD     X2PSAV        X2PPRT            3 1
     C                   EXCEPT    DDET
     C                   END
     C     *IN05         IFEQ      '0'
     C     *IN06         ANDEQ     '1'
     C*
     C*  ZERO OUT INVOICE LEVEL TOTALS
     C*
     C                   Z-ADD     0             X2#BLL            4 0
     C                   Z-ADD     0             X2AMTC            9 2
     C                   Z-ADD     0             X2ASAV            9 2
     C                   Z-ADD     0             X2AMTR            9 2
     C                   Z-ADD     0             X2IAMT            9 2
     C                   Z-ADD     0             X2NSAV            9 2
     C                   Z-ADD     0             X2PSAV            5 3
     C                   Z-ADD     0             X2BRAT            7 2
     C                   Z-ADD     0             X2SBLL            7 2
     C                   Z-ADD     0             X2ROI             4 2
     C                   END
     C                   ELSE
     C*
     C                   Z-ADD     X1WKND        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2WKND
     C*
     C                   Z-ADD     X1WKND        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXWKND
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXWKND
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   ADD       X2#BLL        X3#BLL
     C                   ADD       X2AMTC        X3AMTC
     C                   ADD       X2AMTR        X3AMTR
     C                   ADD       X2IAMT        X3IAMT
     C*
     C     *INOA         IFEQ      '1'
     C     *IN05         ANDEQ     '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DDETHD
     C                   MOVE      '0'           *INOA
     C                   END
     C     *IN05         IFEQ      '1'
     C                   Z-ADD     X2PSAV        X2PPRT            3 1
     C                   EXCEPT    DDET
     C                   END
     C*
     C     *IN05         IFEQ      '0'
     C     *IN06         ANDEQ     '1'
     C*
     C*  ZERO OUT INVOICE LEVEL TOTALS
     C*
     C                   Z-ADD     0             X2#BLL            4 0
     C                   Z-ADD     0             X2AMTC            9 2
     C                   Z-ADD     0             X2ASAV            9 2
     C                   Z-ADD     0             X2AMTR            9 2
     C                   Z-ADD     0             X2IAMT            9 2
     C                   Z-ADD     0             X2NSAV            9 2
     C                   Z-ADD     0             X2PSAV            5 3
     C                   Z-ADD     0             X2BRAT            7 2
     C                   Z-ADD     0             X2SBLL            7 2
     C                   Z-ADD     0             X2ROI             4 2
     C                   END
     C                   END
     C                   ENDSR
     C*
     C*  SUBROUTINE FOR WEEKLY BREAK. ADVANCE END OF WEEK MARKER
     C*  FORWARD ONE WEEK. ADD WEEKLY TOTALS TO GRAND TOTALS.
     C*  PRINT DETAIL TOTAL LINE AND/OR SUMMARY DETAIL LINE
     C*
     C     SRWKB         BEGSR
     C*
     C                   ADD       XXIVC3        XXIVC4
     C                   ADD       X3#BLL        X4#BLL
     C                   ADD       X3AMTC        X4AMTC
     C                   ADD       X3AMTR        X4AMTR
     C                   ADD       X3IAMT        X4IAMT
     C*
     C     *IN05         IFEQ      '1'
     C                   Z-ADD     X3PSAV        X3PPRT            3 1
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DTOT
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DDETHD
     C                   END
     C*
     C     *INOB         IFEQ      '1'
     C     *IN06         ANDEQ     '1'
     C                   EXCEPT    HEAD2
     C                   EXCEPT    SDETHD
     C                   MOVE      '0'           *INOB
     C                   END
     C     *IN06         IFEQ      '1'
     C                   Z-ADD     X3PSAV        X3PPRT            3 1
     C                   EXCEPT    SDET
     C                   END
     C*
     C     *IN06         IFEQ      '0'
     C     *IN05         ANDEQ     '1'
     C*
     C*  ZERO OUT WEEKLY LEVEL TOTALS
     C*
     C                   Z-ADD     0             XXIVC3
     C                   Z-ADD     0             X3#BLL
     C                   Z-ADD     0             X3AMTC
     C                   Z-ADD     0             X3ASAV
     C                   Z-ADD     0             X3AMTR
     C                   Z-ADD     0             X3IAMT
     C                   Z-ADD     0             X3NSAV
     C                   Z-ADD     0             X3PSAV
     C                   Z-ADD     0             X3BRAT
     C                   Z-ADD     0             X3SBLL
     C                   Z-ADD     0             X3ROI
     C                   END
     C                   ENDSR
     C*
     C* SUBROUTINE TO SKIP AHEAD TO THE FIRST WEEK IN THE RANGE
     C* REQUESTED THAT CONTAINS THE FIRST A/R RECORD
     C*
     C*
     C     SRADV         BEGSR
     C                   Z-ADD     X2STDT        SRDATE            8 0
     C                   Z-ADD     XX1WK         SRFUTR            5 0
     C                   Z-ADD     7             XX1WK             5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        X27FWD            8 0
     C*
     C                   Z-ADD     X27FWD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X17FWD
     C*
     C     X1TRDT        IFGT      X17FWD
     C                   MOVE      '0'           *IN35
     C*
     C                   Z-ADD     X27FWD        X2STDT
     C                   EXSR      WKBEG
     C                   ELSE
     C                   MOVE      '1'           *IN35
     C*
     C                   Z-ADD     X27FWD        X2STDT
     C                   EXSR      WKBEG
     C                   END
     C*
     C                   ENDSR
     C*
     C*
     C*  THIS SUBROUTINE FINDS THE BEGINNING OF THE WEEK SO THAT
     C*  A WEEK RANGE MAY BE SPECIFIED FOR THE DETAIL REPORT TOTAL
     C*  LINE
     C*
     C     WKBEG         BEGSR
     C                   MOVE      X2STDT        SRDTE             8 0
     C                   Z-ADD     6             SRPST             5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           Z-ADDSRDTE     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXSTRT
     C                   Z-ADD     SRDTE         XXSTRT
     C*** BEGIN CHG *** 01/27/00 **************************************
     C*
     C                   ENDSR
     C*
     C*
     O**
     OQSYSPRT   E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      **** BEGIN ADD **** 05/11/98 ************************************
     O          E            ROUTE1      2  2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X2FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X2TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              21
      **** END   ADD **** 05/11/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 57 'INC. 1997     '
     O***                                46 'INC.'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** END   CHG **** 05/07/98 ************************************
     O          E            HEAD1          1 06
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DSDATE     8 '  /  /  '
     O                       DSDATE              10 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                       XXCONM              81
     O                       DSTIME             127 '  :  :  '
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                       XXTTL1              91
     O                       PAGE          Z    127
     O                                          122 'PAGE:'
     O*** BEG CHG *** 04/21/99 ****************************************
     O          E            HEAD1          1
     O                                           65 'DETAIL REPORT'
     O          E            HEAD1          2
     O                                           51 'SELECTED DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    60 '  /  /  '
     O**********                         65 'TO:'
     O**********               XXTODT    74 '  /  /  '
     O                       X2FRDT              62 '  /  /    '
     O                                           67 'TO:'
     O                       X2TODT              78 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O******* E  2             HEAD1
     O*******                            51 'FROM:'
     O*******                  XXFRDT    61 '  /  /  '
     O*******                            65 'TO:'
     O*******                  XXTODT    74 '  /  /  '
     O*** END CHG *** 04/21/99 ****************************************
     O          E            HEAD1          1
     O                                           10 'SUBSCRIBER'
     O                       XXSUB#              16
     O                                           18 '-'
     O                       XXSBNM              59
     O**
     O          E            DDETHD      2  1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          5 'TRANS'
     O**********                         21 'INVOICE'
     O**********                         30 'GRP'
     O**********                         40 'CASE'
     O**********                         57 '# OF'
     O**********                         72 'AMOUNT'
     O**********                         93 'AMOUNT SAVED'
     O**********                        110 'RECOMMENDED'
     O**********                        125 'AMOUNT'
     O                                            7 'TRANS'
     O                                           23 'INVOICE'
     O                                           32 'GRP'
     O                                           42 'CASE'
     O                                           59 '# OF'
     O                                           74 'AMOUNT'
     O                                           95 'AMOUNT SAVED'
     O                                          112 'RECOMMENDED'
     O                                          127 'AMOUNT'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DDETHD         3
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          4 'DATE'
     O**********                         18 '#'
     O**********                         58 'BILLS'
     O**********                         73 'OF BILLS'
     O**********                        109 'ALLOWANCE'
     O**********                        126 'INVOICED'
     O                                            6 'DATE'
     O                                           20 '#'
     O                                           60 'BILLS'
     O                                           75 'OF BILLS'
     O                                          111 'ALLOWANCE'
     O                                          128 'INVOICED'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DDET           1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTRDT    08 '  /  /  '
     O**********               XXINV#4   21
     O**********               XXFND     30
     O**********               CSFYR     39
     O**********                         40 '-'
     O**********               CSNUM     47
     O**********               X2#BLL1B  57
     O**********               X2AMTCJB  75
     O**********               X2ASAVJB  93
     O**********               X2AMTRJB 111
     O**********               X2IAMTJB 127
     O                       XXTRDT              10 '  /  /    '
     O                       XXINV#        4     23
     O                       XXFND               32
     O                       CSFYR               41
     O                                           42 '-'
     O                       CSNUM               49
     O                       X2#BLL        1B    59
     O                       X2AMTC        JB    77
     O                       X2ASAV        JB    95
     O                       X2AMTR        JB   113
     O                       X2IAMT        JB   129
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DTOT        3    12
     O                                           51 'WEEK'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXSTRT    60 '  /  /  '
     O**********                         62 '-'
     O**********               XXWKND    71 '  /  /  '
     O**********                         78 'TOTALS'
     O                       XXSTRT              62 '  /  /    '
     O                                           64 '-'
     O                       XXWKND              75 '  /  /    '
     O                                           82 'TOTALS'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DTOT        3
     O                                           62 'TOTAL NUMBER OF BILLS'
     O                                           70 ':'
     O                       X3#BLL        1     84
     O          E            DTOT        3
     O                                           65 'TOTAL NUMBER OF INVOICES'
     O                                           70 ':'
     O                       XXIVC3        1     84
     O          E            DTOT        3
     O                                           62 'TOTAL AMOUNT OF BILLS'
     O                                           70 ':'
     O                       X3AMTC        J     85 '$'
     O          E            DTOT        3
     O                                           59 'TOTAL AMOUNT SAVED'
     O                                           70 ':'
     O                       X3ASAV        J     85 '$'
     O          E            DTOT        3
     O                                           58 'TOTAL RECOMMENDED'
     O                                           68 ' ALLOWANCE'
     O                                           70 ':'
     O                       X3AMTR        J     85 '$'
     O          E            DTOT        3
     O                                           62 'TOTAL AMOUNT INVOICED'
     O                                           70 ':'
     O                       X3IAMT        J     85 '$'
     O          E            DTOT        3
     O                                           58 'TOTAL NET SAVINGS'
     O                                           70 ':'
     O                       X3NSAV        J     85 '$'
     O          E            DTOT        3
     O                                           56 'PERCENT SAVINGS'
     O                                           70 ':'
     O                       X3PPRT        J     82
     O                                           84 '%'
     O          E            DTOT        3
     O                                           61 'AVERAGE BILLING RATE'
     O                                           70 ':'
     O                       X3BRAT        J     85 '$'
     O          E            DTOT        3
     O                                           65 'AVERAGE SAVINGS PER BILL'
     O                                           70 ':'
     O                       X3SBLL        J     85 '$'
     O          E            DTOT        3
     O                                           61 'RETURN ON INVESTMENT'
     O                                           70 ':'
     O                                           78 '1:'
     O                       X3ROI         J     85
     O          E            DFTOT       3    12
     O                                           68 'FINAL TOTALS'
     O          E            DFTOT       3
     O                                           62 'TOTAL NUMBER OF BILLS'
     O                                           70 ':'
     O                       X4#BLL        1     84
     O          E            DFTOT       3
     O                                           65 'TOTAL NUMBER OF INVOICES'
     O                                           70 ':'
     O                       XXIVC4        1     84
     O          E            DFTOT       3
     O                                           62 'TOTAL AMOUNT OF BILLS'
     O                                           70 ':'
     O                       X4AMTC        J     85 '$'
     O          E            DFTOT       3
     O                                           59 'TOTAL AMOUNT SAVED'
     O                                           70 ':'
     O                       X4ASAV        J     85 '$'
     O          E            DFTOT       3
     O                                           58 'TOTAL RECOMMENDED'
     O                                           68 ' ALLOWANCE'
     O                                           70 ':'
     O                       X4AMTR        J     85 '$'
     O          E            DFTOT       3
     O                                           62 'TOTAL AMOUNT INVOICED'
     O                                           70 ':'
     O                       X4IAMT        J     85 '$'
     O          E            DFTOT       3
     O                                           58 'TOTAL NET SAVINGS'
     O                                           70 ':'
     O                       X4NSAV        J     85 '$'
     O          E            DFTOT       3
     O                                           56 'PERCENT SAVINGS'
     O                                           70 ':'
     O                       X4PPRT        J     82
     O                                           84 '%'
     O          E            DFTOT       3
     O                                           61 'AVERAGE BILLING RATE'
     O                                           70 ':'
     O                       X4BRAT        J     85 '$'
     O          E            DFTOT       3
     O                                           65 'AVERAGE SAVINGS PER BILL'
     O                                           70 ':'
     O                       X4SBLL        J     85 '$'
     O          E            DFTOT       3
     O                                           61 'RETURN ON INVESTMENT'
     O                                           70 ':'
     O                                           78 '1:'
     O                       X4ROI         J     85
     O          E            ERMSG1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     OQSYSPRT1  E            ROUTE2         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE2         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE2         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE2         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE2         1
     O               08      ORDIS2              60
     O          E            ROUTE2         1
     O               08      ORDIS3              60
     O          E            ROUTE2         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE2         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE2         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      **** BEGIN ADD **** 05/11/98 ************************************
     O          E            ROUTE2      2  2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X2FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X2TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              21
      **** END   ADD **** 05/11/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE2
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 57 'INC. 1997     '
     O****                               46 'INC.'
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** END   CHG **** 05/07/98 ************************************
     O*** BEGIN CHG *** 11/10/99 **************************************
     O**********QSYSPRT1E  106           HEAD2
     O**********               DSDATE     8 '  /  /  '
     O**********               XXCONM    81
     O**********               DSTIME   127 '  :  :  '
     O********E* 1             HEAD2
     O**********               ORPNAM    10
     O**********               XXTTL2    91
     O**********               PAGE1    127
     O**********                        122 'PAGE:'
     O*** BEG CHG *** 04/21/99 ***************************************
     O********E* 1             HEAD2
     O**********                         67 'SUMMARY REPORT'
     O********E* 2             HEAD2
     O**********                         51 'SELECTED DATE RANGE'
     O**********               XXFRDT    61 '  /  /  '
     O**********                         65 'TO:'
     O**********               XXTODT    74 '  /  /  '
     OQSYSPRT1  E            HEAD2          1 06
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DSDATE     8 '  /  /  '
     O                       DSDATE              10 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                       XXCONM              94
     O                       DSTIME             148 '  :  :  '
     O          E            HEAD2          1
     O                       ORPNAM              10
     O                       XXTTL2             104
     O                                          143 'PAGE:'
     O                       PAGE1              148
     O*** BEG CHG *** 04/21/99 ***************************************
     O          E            HEAD2          1
     O                                           82 'SUMMARY REPORT'
     O          E            HEAD2          2
     O                                           72 'SELECTED DATE RANGE'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    82 '  /  /  '
     O**********                         86 'TO:'
     O**********               XXTODT    95 '  /  /  '
     O                       X2FRDT              84 '  /  /    '
     O                                           88 'TO:'
     O                       X2TODT              99 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*** END   CHG *** 11/10/99 **************************************
     O******* E  2             HEAD2
     O*******                            51 'FROM:'
     O*******                  XXFRDT    61 '  /  /  '
     O*******                            65 'TO:'
     O*******                  XXTODT    74 '  /  /  '
     O*** END CHG *** 04/21/99 ***************************************
     O          E            HEAD2          1
     O                                           10 'SUBSCRIBER'
     O                       XXSUB#              16
     O                                           18 '-'
     O                       XXSBNM              59
     O          E            SDETHD      2  1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          4 'WEEK'
     O**********                         14 '# OF'
     O*** BEGIN CHG *** 11/10/99 **************************************
     O**********                         27 'TOTAL AMT'
     O**********                         41 'TOTAL AMT'
     O**********                         56 'TOTAL REC'
     O**********                         71 'TOTAL AMT'
     O**********                         81 'TOTAL'
     O**********                         93 'PERC'
     O**********                        107 'AVG BILLING'
     O**********                        120 'AVG SAVINGS'
     O**********                        127 'ROI'
     O**********                         31 'TOTAL AMT'
     O**********                         49 'TOTAL AMT'
     O**********                         68 'TOTAL REC'
     O**********                         87 'TOTAL AMT'
     O**********                        101 'TOTAL'
     O**********                        113 'PERC'
     O**********                        127 'AVG BILLING'
     O**********                        140 'AVG SAVINGS'
     O**********                        147 'ROI'
     O                                            6 'WEEK'
     O                                           16 '# OF'
     O                                           32 'TOTAL AMT'
     O                                           50 'TOTAL AMT'
     O                                           69 'TOTAL REC'
     O                                           88 'TOTAL AMT'
     O                                          102 'TOTAL'
     O                                          114 'PERC'
     O                                          128 'AVG BILLING'
     O                                          141 'AVG SAVINGS'
     O                                          148 'ROI'
     O*** END   CHG *** 01/27/00 **************************************
     O*** END   CHG *** 11/10/99 **************************************
     O          E            SDETHD         3
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          6 'ENDING'
     O**********                         15 'BILLS'
     O*** BEGIN CHG *** 11/10/99 **************************************
     O**********                         26 'OF BILLS'
     O**********                         39 'SAVED'
     O**********                         56 'ALLOWANCE'
     O**********                         70 'INVOICED'
     O**********                         85 'NET SAVINGS'
     O**********                         94 'SAVINGS'
     O**********                        104 'RATE'
     O**********                        118 'PER BILL'
     O**********                         30 'OF BILLS'
     O**********                         47 'SAVED'
     O**********                         68 'ALLOWANCE'
     O**********                         86 'INVOICED'
     O**********                        105 'NET SAVINGS'
     O**********                        114 'SAVINGS'
     O**********                        124 'RATE'
     O**********                        138 'PER BILL'
     O                                            8 'ENDING'
     O                                           17 'BILLS'
     O                                           31 'OF BILLS'
     O                                           48 'SAVED'
     O                                           69 'ALLOWANCE'
     O                                           87 'INVOICED'
     O                                          106 'NET SAVINGS'
     O                                          115 'SAVINGS'
     O                                          125 'RATE'
     O                                          139 'PER BILL'
     O*** END   CHG *** 01/27/00 **************************************
     O*** END   CHG *** 11/10/99 **************************************
     O          E            SDET           2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXWKND    08 '  /  /  '
     O**********               X3#BLL1B  14
     O                       XXWKND              10 '  /  /    '
     O                       X3#BLL        1B    16
     O*** END   CHG *** 01/27/00 **************************************
     O*** BEGIN CHG *** 11/10/99 **************************************
     O**********               X3AMTCJ   28 '$'
     O**********               X3ASAVJ   42 '$'
     O**********               X3AMTRJ   57 '$'
     O**********               X3IAMTJ   72 '$'
     O**********               X3NSAVJ   86 '$'
     O**********               X3PPRTJ   93
     O**********                         94 '%'
     O**********               X3BRATJ  105 '$'
     O**********               X3SBLLJ  119 '$'
     O**********                        122 '1:'
     O**********               X3ROI 1  127
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               X3AMTCJ +002 '$'
     O**********               X3ASAVJ +002 '$'
     O                       X3AMTC        J   +001 '$'
     O                       X3ASAV        J   +001 '$'
     O*** END   CHG *** 01/27/00 **************************************
     O                       X3AMTR        J   +002 '$'
     O                       X3IAMT        J   +002 '$'
     O                       X3NSAV        J   +002 '$'
     O                       X3PPRT        J   +002
     O                                         +000 '%'
     O                       X3BRAT        J   +002 '$'
     O                       X3SBLL        J   +002 '$'
     O                                         +002 '1:'
     O                       X3ROI         1   +000
     O*** END   CHG *** 11/10/99 **************************************
     O          E            STOT        1
     O                                           06 'TOTALS'
     O                       X4#BLL        1     14
     O*** BEGIN CHG *** 11/10/99 **************************************
     O**********               X4AMTCJ   28 '$'
     O**********               X4ASAVJ   42 '$'
     O**********               X4AMTRJ   57 '$'
     O**********               X4IAMTJ   72 '$'
     O**********               X4NSAVJ   86 '$'
     O**********               X4PPRTJ   93
     O**********                         94 '%'
     O**********               X4BRATJ  105 '$'
     O**********               X4SBLLJ  119 '$'
     O**********                        122 '1:'
     O**********               X4ROI 1  127
     O                       X4AMTC        J   +002 '$'
     O                       X4ASAV        J   +002 '$'
     O                       X4AMTR        J   +002 '$'
     O                       X4IAMT        J   +002 '$'
     O                       X4NSAV        J   +002 '$'
     O                       X4PPRT        J   +002
     O                                         +000 '%'
     O                       X4BRAT        J   +002 '$'
     O                       X4SBLL        J   +002 '$'
     O                                         +002 '1:'
     O                       X4ROI         1   +000
     O*** END   CHG *** 11/10/99 **************************************
     O          E            ERMSG2      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O          E            ERMSG3      3
     O                                           19 '*** NO MATCHING EOB'
     O                                           45 ' RECORDS WERE FOUND *'
