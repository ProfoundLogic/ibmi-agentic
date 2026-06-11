     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W2100R1 - UPDATE BILLING REQUEST FILE FOR EMPLOYER RANGE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*      THIS PROGRAM WILL CLEAR AND RELOAD ALL BILLING VALUES    *
     H*      FOR AN EMPLOYER RANGE WITHIN THE ENTERED FUND/FYR (FOR   *
     H*      A CONTROLLED MASS BILLING SUBMISSION). AN EDIT REPORT    *
     H*      WILL BE GENERATED TO SHOW THE ASSIGNED FLAGS AS THEY     *
     H*      EXIST IN THE BILLING FILE AND WHETHER AN ERROR OCCURRED. *
     H*                                                               *
     H*  CALLED FROM:  W2100C1 - OVERRIDE                             *
     H*  CALLS TO:     NONE                                           *
     H*                                                               *
     H*  PROGRAMMER: C:24                                             *
     H*  CREATED:    04/06/95                PROJECT: WC407           *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*     OF -  OVERFLOW FOR PRINTER                                *
     H*     08 -  DEMAND MODE                                         *
     H*     09 -  BATCH  MODE                                         *
     H*     21 -  PRIOR A/R EXISTS; PRINT '**'                        *
     H*     22 -  BILLING FLAG IS BLANK                               *
     H*     23 -  AUDIT FLAG IS BLANK                                 *
     H*     24 -  UPDATE A/R FLAG IS BLANK                            *
     H*     40 -  CHAIN/READ BILLING REQUEST FILE                     *
     H*     60 -  GENERAL READS/CHAINS                                *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   SMORL   POR01: PGM/PRT                      Output Override *
     H*   WMFNL   PFN01: CO/FND                           Fund Master *
     H*   WABLL   PBL01: CO/FND/FYR/EMP/DIV           Billing Request *
     H*   WMEML   PEM01: CO/FND/EMP#/DIV              Employer Master *
     H*   WMEAL6  PEA01: CO/FND/EMP#/DIV/FYR/PODT        Employer A/R *
     H*   WDELL1  PEL01: CO/FND/FYR/EMP#/DIV          Employer Detail *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/07/23 C:291 Project: WC2901                                *
     H*                Add processing to build the audit billing      *
     H*                request records for only new audits or for     *
     H*                only revised audits depending on the value of  *
     H*                the audit flag parameter.                      *
     H*                                                               *
     H* 05/17/21 C:65  SUPPORT: Phw#95945 (InHouse)                   *
     H*                Remove audit flag check from audit section, it *
     H*                was preventing audit payroll check from        *
     H*                occurring.                                     *
     H*                                                               *
     H* 04/03/19 C:062 Ticket: 76109(099)                             *
     H*                Change audit flag issue.                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 10/17/14 C:65  SUPPORT: WA129/87023                           *
     H*                Call CHKQUOTE prior to calling WA069R to       *
     H*                determine if quote library needs to be         *
     H*                removed by using the WA069C program.           *
     H*                                                               *
     H* 05/18/12 C:279 SUPPORT: WA045 79887                           *
     H*                Modified to populate BLRFLG from WA069R.       *
     H*                                                               *
     H* 03/27/11 C:235 SUPPORT: 71062                                 *
     H*                MASS BILLING ISSUES                            *
     H*                PREVENT MASS AUDIT BILLING REQUEST FROM        *
     H*                BEING WRITTEN ON DIV THAT HAS ALREADY HAD AN   *
     H*                UPDATE AUDIT. IF UPDATE AUDIT HAS NOT BEEN DONE*
     H*                MAKE SURE ALL ACTIVE PAYROLLS ARE AUDITED      *
     H*                ON THE POLICY                                  *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/20/09 C:136 SUPPORT: WA139 65954                           *
     H*                DO NOT WRITE MASS BILLING RECORD TO RUN AN     *
     H*                ESTIMATED BILL ON A POLICY IF ANY DIVISION     *
     H*                ON THE POLICY HAS BEEN BILLED PER GW.          *
     H*                                                               *
     H* 10/08/08 C:65  PROJECT: SUPPORT                               *
     H*                CHECK A/R ON STATE REPORTING DIVISION FOR      *
     H*                SUMMARIZED DIVISIONS.                          *
     H*                                                               *
     H* 10/10/03 C:33  PROJECT: WC1017                                *
     H*                Update renewal flag.                           *
     H*                                                               *
     H* 01/17/03 C:61  SUPPORT WA081 35574 WC1017                     *
     H*                CHAIN back to original Employer Policy record  *
     H*                in process after check for Include/Exclude     *
     H*                by State rep division jurisdiction state.      *
     H*                                                               *
     H*  11/12/02 C:132 PROJECT: WC1017                               *
     H*                Change to Include/Excude up to 10 States by    *
     H*                comparing each State code to the employer's    *
     H*                jurisdiction state                             *
     H*                                                               *
     H*  03/08/02 C:65 PROJECT: WC1030                                *
     H*                IF THE EMPLOYER HAS A PREVIOUS EMPLR# THEN     *
     H*                CONSIDER THE POLICY AS A RENEWAL.              *
     H*                                                               *
     H*  04/12/00 C:65 PROJECT SUPPORT#22551                          *
     H*                CHECK BOTH TRANSACTION AND POSTING DATE FOR    *
     H*                CREATING BILLING REQUEST RECORDS.              *
     H*                                                               *
     H*  03/13/98 C:65 PROJECT WC577                                  *
     H*                FIXED SIZE OF YEAR FIELD IN DATE DATA STRUCTS. *
     H*
     H*  01/22/96 C:48 PROJECT WC407                                  *
     H*                REMOVE DIVISION ENTRY OFF THE SCREEN AND REMOVE*
     H*                SELECTION CRITERIA ON DIV AND EXCLUDE ACCOUNTS *
     H*
     H*  05/24/96 C:09 PROJECT WC407                                  *
     H*                PROGRAM WAS IGNORING CANCELLED ACCOUNTS FOR    *
     H*                AUDITS AND WAS ALSO NOT GENERATING RECORDS FOR *
     H*                DIVISIONS.                                     *
     H*
     H*  11/21/96 C:65 PROJECT: CM023                                 *
     H*               CHANGED FYR TO POLICY DATE RANGE.  THE PROGRAM  *
     H*               SHOULD NOW READ WDELL3 BASED ON THE DATE RANGE. *
     H*               IF EMPLOYER RANGE IS SPECIFIED, THAT IS CHECKED *
     H*               AS WELL BUT ISN'T MANDATORY ANYMORE. AGT#, RFLG,*
     H*               AND PFDT ARE BELING UPDATED HERE AS WELL FOR    *
     H*               THE BILLING FILE.                               *
     H*****************************************************************
     H*                                                               *
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWABLL     UF A E           K DISK
     FWMEML     IF   E           K DISK
     FWMEAL6    IF   E           K DISK
     FWDELL3    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F* Rate Set Code File
     FWMRSL     IF   E           K DISK
     FWDEHLT2   IF   E           K DISK
     FWMEPL7E   IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  ##CDES                 1     20
     D  ST                     1     20
     D                                     DIM(10)                                I/E STATE CODES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* SYSTEM & A/R POST DATES TO COMPARE ON MONTH/YEAR
     D                 DS
     D  XXSYDT                 1      7  0
     D  XXSYYM                 1      5  0
     D                 DS
     D  XXPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D                 DS
     D  XXTRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D*
     D* DATA STRUCTURES FOR EMPLOYER EFFECTIVE & CANCELLATION DATES
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                                 EMPLOYER EFF DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)                                 EMPLR CANC DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*      KEY LISTINGS
     C****************************************************************
     C*
     C*    USER PARAMETERS FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*    COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*    FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   BILLING REQUEST FILE
     C*
     C     BLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV
     C                   KFLD                    XXFYR
     C*
     C     EHKYT2        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C*
     C*   EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * KEYS USED FOR STATE CODE CHECKING
      *
      * Employer Detail File
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C*   Employer Master File
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Rate Set Code Master File
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
     C*
     C     EHKYT3        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKFAF             1
     C*
     C     EPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     ELKY6         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELPFDT
     C                   KFLD                    ELEMP#
     C****************************************************************
     C*     *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    BLCO#         K2CO#
     C     *LIKE         DEFINE    BLFND         K2FND
     C     *LIKE         DEFINE    BLFYR         K2FYR
     C     *LIKE         DEFINE    BLEMP#        K2EMP#
     C     *LIKE         DEFINE    BLDIV         K2DIV
     C     *LIKE         DEFINE    BLCO#         XXCO#
     C     *LIKE         DEFINE    BLFND         XXFND
     C     *LIKE         DEFINE    BLFYR         XXFYR
     C     *LIKE         DEFINE    BLEMP#        XXEMP#
     C     *LIKE         DEFINE    BLDIV         XXDIV
     C     *LIKE         DEFINE    BLEMP#        XXFEM#
     C     *LIKE         DEFINE    BLEMP#        XXTEM#
     C     *LIKE         DEFINE    BLDIV         XXFDIV
     C     *LIKE         DEFINE    BLDIV         XXTDIV
     C     *LIKE         DEFINE    BLDT          XXDATE
     C     *LIKE         DEFINE    BLDT          XXEFDT
     C     *LIKE         DEFINE    BLDT          XXTODT
     C*
     C     *LIKE         DEFINE    BLEMP#        SVEMP#
     C     *LIKE         DEFINE    BLDIV         SVDIV
     C                   Z-ADD     *ZERO         #ICNT             7 0          Include St CNT
     C                   Z-ADD     *ZERO         #ECNT             7 0          Exclude St CNT
     C*
     C                   MOVE      *ZEROS        XXYEAR            4 0
     C                   MOVE      *ZERO         X                 1 0          INDEX
     C                   MOVE      *ZERO         Y                 1 0          |
     C                   MOVE      *ZERO         XXCNT             7 0          EMPLOYER COUNT
     C*
     C****************************************************************
     C*      *ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFEM#            9            FROM EMP#
     C                   PARM                    QQTEM#            9            TO EMP#
     C                   PARM                    QQFL1             1            BILLING FLAG
     C                   PARM                    QQFL2             1            AUDIT FLAG
     C                   PARM                    QQFL3             1            UPDATE A/R FLAG
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C                   PARM                    QQSTCD            1
     C                   PARM                    QQCDES           20
      *
     C                   MOVE      QQSTCD        XXSTCD            1
     C                   MOVE      QQCDES        ##CDES
      *
      * Check to see if have INCLUDE States Code
     C     XXSTCD        IFEQ      'I'
     C                   MOVE      *ON           *IN67
     C                   ENDIF
      *
      * Else Check to see if have EXCLUDE States Code
     C     XXSTCD        IFEQ      'E'
     C                   MOVE      *ON           *IN68
     C                   ENDIF
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEFDT        XXDTF             8 0
     C                   MOVE      QQTODT        XXDTT             8 0
     C*
     C                   Z-ADD     XXDTF         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DTF             6 0
     C*
     C                   Z-ADD     XXDTT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DTT             6 0
     C*
     C                   MOVE      QQFEM#        XXFEM#
     C                   MOVE      QQTEM#        XXTEM#
     C     QQFL1         COMP      ' '                                    22
     C     QQFL2         COMP      ' '                                    23
     C     QQFL3         COMP      ' '                                    24
     C*
     C****************************************************************
     C*   SET UP AND INITIALIZE
     C****************************************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     QQMODE        IFNE      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET COMPANY, FUND, AND USER INFORMATION
     C*
     C     COKY          CHAIN     PCO01                              60
     C     FNKY          CHAIN     PFN01                              60
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C                   END
     C                   MOVE      '1'           *INOF
     C****************************************************************
     C*     BEGIN MAIN LINE PROGRAM
     C****************************************************************
     C*
     C*  PROCESS EACH EMPLOYER IN THE RANGE THAT HAS EMP DETAIL RECORD
     C*
     C                   Z-ADD     XXFEM#        XXEMP#
     C                   Z-ADD     *ZERO         XXDIV
     C                   Z-ADD     *ZERO         SVDIV
     C     ELKY1         SETLL     PEL01                                         CO/FND/FYR/EMP/DIV
     C     ELKY          READE     PEL01                                  61     CO/FND/FYR
     C*
     C     *IN61         DOWEQ     '0'
     C     ELPFDT        IFGT      XXTODT
     C     ELPFDT        ORLT      XXEFDT
     C                   MOVE      '1'           *IN61
     C                   MOVE      '1'           *IN63
     C                   ELSE                                                   PFDT > TODT
     C     XXTEM#        IFNE      *ZERO
     C     ELEMP#        IFGT      XXTEM#
     C     ELEMP#        ORLT      XXFEM#
     C                   MOVE      '1'           *IN61
     C                   ENDIF                                                  ELEMP#
     C                   ENDIF                                                  XXTEM# NE 0
     C                   ENDIF                                                  PFDT > TODT
     C     *IN61         IFEQ      '0'
     C*
     C*  DO NOT LOAD RECORDS WITH BILLING OR UPDATE A/R FLAG IF
     C*  AN ACCOUNT HAS BEEN AUDITED
     C*
     C                   MOVE      '0'           *IN62
     C     ELFAF         IFEQ      'A'
     C     QQFL1         ANDEQ     'Y'
     C     ELFAF         OREQ      'A'
     C     QQFL3         ANDEQ     'Y'
     C                   MOVE      '1'           *IN62
     C                   END
      *
      *  A PREMIUM HISTORY RECORD WILL EXIST ON ONE OF THE DIVISIONS
      *  IF THE POLICY HAS BEEN PREVIOUSLY BILLED
      *
     C     *IN62         IFEQ      *OFF
     C     EHKYT2        CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C     QQFL1         IFEQ      'Y'
     C     QQFL3         OREQ      'Y'
     C                   MOVE      *ON           *IN62
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *PREVENT MASS AUDIT BILLING REQUEST FROM BEING WRITTEN ON DIV
      *THAT HAS ALREADY HAD AN UPDATE AUDIT. IF UPDATE AUDIT HAS
      *NOT BEEN DONE, MAKE SURE ALL ACTIVE PAYROLLS ARE AUDITED
      *ON THE POLICY
      *
     C     QQFL2         IFEQ      'Y'
     C                   MOVE      'A'           KKFAF
     C     EHKYT3        CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN62
     C                   ELSE
     C     EPKY          SETLL     WMEPL7E
     C     EPKY          READE     WMEPL7E                                60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSAFL        IFNE      'A'
     C     ELKY6         SETGT     PEL01
      *** Beg Chg ***** 07/07/23 ****************************
      * READPE on WDELL3 without key in Factor1 was loosing file pointer
      * and next read to WDELL3 turned end of the file indicator on so
      * none of the policies after this were processed for "New Only" Audit request.
      * Changed READPE to READP.
     C***                READPE    PEL01                                  60
     C                   READP     PEL01                                  60
      *** End Chg ***** 07/07/23 ****************************
     C                   MOVE      *ON           *IN62
     C                   LEAVE
     C                   ENDIF
     C     EPKY          READE     WMEPL7E                                60
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
      //*** Beg Add ***** 07/07/23 **********************************
       If QQFl2 = 'R';
          KKFAF = 'A';
          Chain (ELCO#:ELFND:ELFYR:ELEMP#:ELDIV:KKFAF) peh01;
          If Not %Found(WDEHLT2);
             *In62 = '1';
          Endif;
       Endif;

      //*** End Add ***** 07/07/23 **********************************
     C     *IN62         IFEQ      '0'
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   MOVE      ELFYR         XXFYR
     C                   Z-ADD     ELDIV         XXDIV
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN25
     C*
     C*  DON'T ALLOW UPDATE ON CANCELLED EMPLOYERS
     C*
     C                   Z-ADD     XXDATE        XXSYDT                          SYSTEM DATE
     C                   MOVE      *BLANK        EMNAME
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C     1             DO        5             X
     C     EF(X)         IFNE      *ZERO
     C     EF(X)         ANDLE     XXSYDT
     C                   Z-ADD     X             Y
     C                   END
     C                   END                                                    DO LOOP
     C     Y             IFNE      *ZERO
     C     CX(Y)         ANDNE     *ZERO
     C     QQFL2         ANDNE     'Y'
      //*** Beg Add ***** 07/07/23 **********************************
     C     QQFL2         ANDNE     'R'
      //*** End Add ***** 07/07/23 **********************************
     C                   MOVE      '1'           *IN25                          EMPLR CANCELLED
     C                   END
     C                   END                                                    IF *IN60
     C*
     C     *IN25         IFEQ      '0'
     C                   Z-ADD     0             XSDIV             5 0
     C     EMKY1         CHAIN     PEM01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   LEAVE
     C                   ENDIF
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   MOVE      *OFF          *IN65
     C     ELARSM        IFEQ      'Y'
     C     XXDIV         ANDNE     XSDIV
     C                   MOVE      *ON           *IN65
     C                   ENDIF
     C*
     C*  DO NOT ALLOW UPDATE OF BILLING & A/R FLAGS IF THERE ARE
     C*  A/R BILLING ENTRIES IN PRIOR MONTH (I.E., THERE ARE 'B' OR
     C*  'A' TRANSACTIONS IN A/R WITH A POSTING DATE LESS THAN THE
     C*  CURRENT SYSTEM MONTH)
     C*
     C     QQFL3         IFEQ      'Y'                                           A/R FLAG
     C  N65EAKY          CHAIN     PEA01                              60         CO/FND/EMP/DIV/FYR
     C   65EAKY1         CHAIN     PEA01                              60         CO/FND/EMP/DIV/FYR
     C     *IN60         DOWEQ     '0'
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C                   Z-ADD     EAPODT        XXPODT                          A/R POST DATE
     C                   Z-ADD     EATRDT        XXTRDT
     C     XXPOYM        IFLT      XXSYYM
     C     XXTRYM        ANDLT     XXSYYM
     C                   MOVE      '1'           *IN21                           PRIOR A/R EXISTS
     C                   END
     C                   END
     C  N65EAKY          READE     PEA01                                  60
     C   65EAKY1         READE     PEA01                                  60
     C   21              MOVE      '1'           *IN60                           NO UPDATE, END LOOP
     C                   END
     C                   END                                                     IF QQFL3 = Y
     C*
     C*  IF VALID, UPDATE OR WRITE NEW RECORD FOR BILLING FLAGS
     C*
     C     *IN21         IFEQ      '0'
      *
      *
      * If Have Flag for Include/Exclude State, then check for valid state
     C     *IN67         IFEQ      *ON
     C     *IN68         OREQ      *ON
      *
      * When Employer changes, Check for State match
     C     ELEMP#        IFNE      SVEMP#
     C                   EXSR      CHKSTC
     C                   ENDIF                                                  ElEMP#<>SVEMP#
      *
      * If Not to be Included/Excluded, bypass this employer's processing
     C     *IN69         IFEQ      *OFF
     C                   GOTO      NXTRCD
     C                   ENDIF
      *
     C                   ENDIF                                                  67 or 68 is ON
      *
     C     BLKY          CHAIN     PBL01                              40         CO/FND/FYR/EMP/DIV
     C                   MOVE      QQFL1         BLFLG1
      *** Beg Chg ***** 07/07/23 **********************************
     C***                MOVE      QQFL2         BLFLG2
       If QQFl2 = 'R';
          BLFLG2 = 'Y';
       Else;
          BLFLG2 = QQFL2;
       Endif;
      *** End Chg ***** 07/07/23 **********************************
     C                   MOVE      QQFL3         BLFLG3
     C                   MOVE      ' '           BLSTAT
     C                   MOVE      Q1USER        BLUSER
     C                   MOVE      Q1PGM         BLPGM
     C                   Z-ADD     DSTIME        BLTIME
     C                   Z-ADD     XXSYDT        BLDT
     C                   Z-ADD     ELPFDT        BLPFDT
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     XXCO#         BLCO#
     C                   Z-ADD     XXFND         BLFND
     C                   Z-ADD     XXFYR         BLFYR
     C                   Z-ADD     ELEMP#        BLEMP#
     C                   Z-ADD     ELDIV         BLDIV
     C                   CALL      'CHKQUOTE'
     C                   PARM                    ##qte             1
     C*
     C* Call the CL program to remove quote library prior to checking
     C* for renewal status if running in quote environment.
     C*
     C                   If        ##qte = 'Q'
     C*
     C                   Move      ELCo#         #2Co#
     C                   Move      ELFnd         #2Fnd
     C                   Move      ELEmp#        #2Emp#
     C                   Move      ELFyr         #2Fyr
     C                   CALL      'WA069C'
     C                   PARM                    #2CO#             3
     C                   PARM                    #2FND             3
     C                   PARM                    #2FYR             3
     C                   PARM                    #2EMP#            9
     C                   PARM      *BLANKS       #1NWRW            1
     C*
     C                   Else
     C                   CALL      'WA069R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM      *BLANKS       #1NWRW            1
     C                   Endif
     C*
     C                   MOVE      #1NWRW        BLRFLG
     C                   Z-ADD     *ZERO         BLAGT#
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   ENDDO                                                  *IN50 == 0
     C*
     C     *IN50         IFEQ      '0'
     C     XXCO#         ANDEQ     AHCO#
     C     XXFND         ANDEQ     AHFND
     C     XXEMP#        ANDEQ     AHEMP#
     C     XXDIV         ANDEQ     AHDIV
     C     XXFYR         ANDEQ     AHFYR
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ENDIF                                                  *IN50 == 0
     C                   WRITE     PBL01
     C                   ELSE
     C                   UPDATE    PBL01
     C                   END                                                    IF *40
     C                   END                                                    IF *21
     C*
     C*  PRINT EDIT REPORT DETAIL
     C*
     C   OF              EXCEPT    HEAD
     C   OF              MOVE      *OFF          *INOF
     C                   EXCEPT    DETAIL
     C                   ADD       1             XXCNT
     C                   END                                                     IF *IN25
     C*
     C                   END                                                     IF *62
     C                   ENDIF                                                   IF *61
      *
     C     NXTRCD        TAG
      *
     C  N63              MOVE      *OFF          *IN61
     C  N63ELKY          READE     PEL01                                  61     CO/FND/FYR
     C                   END                                                     DOW *61
     C*
     C*  PRINT TOTAL OR MESSAGE IF NO UPDATE OCCURRED
     C*
     C     XXCNT         IFEQ      *ZERO
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ELSE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOTAL
     C                   END
     C*
      /Free
         xxPrt = '0';
         Chain orKy POR01;
         If %found;
            Except Route;
         Endif;
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      ** ============================================================== **
      **  CHKSTC - Check for valid state code for Employer Number       **
      ** ============================================================== **
     C     CHKSTC        BEGSR
      *
     C                   Z-ADD     ELCO#         K2CO#
     C                   Z-ADD     ELFND         K2FND
     C                   Z-ADD     ELFYR         K2FYR
     C                   Z-ADD     ELEMP#        K2EMP#
     C                   Z-ADD     ELDIV         K2DIV
     C                   MOVE      *OFF          *IN69
     C                   Z-ADD     ELEMP#        SVEMP#
     C                   MOVE      *BLANKS       #STATE            2
     C                   MOVE      *BLANKS       ##STFG            1
      *
      * Get the EMPLOYER Division with the State Reports Flag
     C     EMKY1         SETLL     PEM01                              60
     C  N60EMKY1         READE     PEM01                                  60
     C     *IN60         DOWEQ     *OFF
      *
      * Get the EMPLOYER Detail Record for the State Flag
      *    Then get the State Code from WMRSL and Leave the DO loop
     C     EMSTRP        IFEQ      'Y'
     C     ELKY4         CHAIN     PEL02                              64
      *
     C     *IN64         IFEQ      *OFF
     C     RSKY          CHAIN     PRS01                              64
     C  N64              MOVEL     RSJRST        #STATE
     C                   ENDIF
      *
     C                   LEAVE
     C                   ENDIF
      *
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
      * See if get a match on State in the State Code array
      *
     C     #STATE        IFNE      *BLANKS
     C                   Z-ADD     *ZEROS        Z                 3 0
      *
     C     Z             DOWLT     10
     C                   ADD       1             Z
     C     #STATE        IFEQ      ST(Z)
     C                   MOVE      'Y'           ##STFG
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ENDIF
      *
      * Seton *IN69 if match on State and is INCLUDE Code    (*IN67)
     C     *IN67         IFEQ      *ON
     C     ##STFG        ANDEQ     'Y'
     C                   MOVE      *ON           *IN69
     C                   ENDIF
      *
      * Seton *IN69 if NO match on State and is EXCLUDE Code (*IN68)
     C     *IN68         IFEQ      *ON
     C     ##STFG        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN69
     C                   ENDIF
      *
      * If NOT *IN69 then print and Error Line DETLST
      *
     C     *IN69         IFEQ      *OFF
     C   OF              EXCEPT    HEAD
     C   OF              MOVE      *OFF          *INOF
     C                   EXCEPT    DETLST
     C   67              ADD       1             #ICNT
     C   68              ADD       1             #ECNT
     C                   ENDIF
      *
     C     ENDSTC        TAG
     C                   Z-ADD     K2CO#         XXCO#
     C                   Z-ADD     K2FND         XXFND
     C                   Z-ADD     K2EMP#        XXEMP#
     C                   Z-ADD     K2DIV         XXDIV
     C                   Z-ADD     K2FYR         XXFYR
     C     ELKY2         CHAIN     PEL02                              60
      *
     C                   ENDSR                                                  CHKSTC
      *
      *
      *
      ** ============================================================== **
     C*
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2 34
     O                                            8 'COMPANY:'
     O                       XXCO#         Z     26
     O          E            ROUTE          2
     O                                            5 'FUND:'
     O                       XXFND         Z     26
     O          E            ROUTE          2
     O                                           10 'FROM DATE:'
     O                       X1DTF               26 '  /  /  '
     O          E            ROUTE          2
     O                                            8 'TO DATE:'
     O                       X1DTT               26 '  /  /  '
     O          E            ROUTE          2
     O                                           14 'FROM EMPLOYER:'
     O                       XXFEM#        Z     30
     O          E            ROUTE          2
     O                                           12 'TO EMPLOYER:'
     O                       XXTEM#        Z     30
     O          E            ROUTE          2
     O                                           13 'BILLING FLAG:'
     O              N22      QQFL1               26
     O               22                          26 '*BLANK'
     O          E            ROUTE          2
     O                                           16 'UPDATE A/R FLAG:'
     O              N24      QQFL3               26
     O               24                          26 '*BLANK'
     O          E            ROUTE          2
     O                                           11 'AUDIT FLAG:'
     O              N23      QQFL2               26
     O               23                          26 '*BLANK'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              10 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    130
     O          E            HEAD           1
     O                       Q1PGM               12
     O                                           61 'BILLING REQUEST EDIT FOR'
     O                                           75 'POLICY DATES:'
     O                       X1DTF               84 '  /  /  '
     O                                           86 '-'
     O                       X1DTT               95 '  /  /  '
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           1
     O                                           60 'EMPLOYERS'
     O                       XXFEM#        Z     70
     O                                           76 '-'
     O                       XXTEM#        Z     86
     O          E            HEAD        1  1
     O                                            7 'FUND:'
     O                       XXFND         Z     11
     O                       FNNAME              72
     O          E            HEAD        1  1
     O                                           19 'FLAG ASSIGNMENTS:'
     O                                           35 'BILLING    ='
     O              N22      QQFL1               37
     O               22                          42 '*BLANK'
     O                                           90 '"**" MEANS THE EMPLOYER"'
     O                                          113 'S FLAGS WERE NOT LOADED'
     O          E            HEAD           1
     O                                           35 'UPDATE A/R ='
     O              N24      QQFL3               37
     O               24                          42 '*BLANK'
     O                                           94 'BECAUSE A/R DETAIL IN A'
     O                                          114 'PRIOR MONTH ALREADY'
     O                                          122 'EXISTS.'
     O          E            HEAD           1
     O                                           35 'AUDIT      ='
     O              N23      QQFL2               37
     O               23                          42 '*BLANK'
     O*
     O          E            DETAIL         1
     O               21                           4 '**'
     O                       XXEMP#        Z     11
     O                       EMNAME              59
     O*
     O          E            DETLST         1
     O               21                           4 '**'
     O                       XXEMP#        Z     11
     O                       XXDIV         3     16
     O                                           29 'State Code:'
     O                       #STATE              32
     O                  67                       58 'No Match on INCLUDE Code'
     O                  67                       64 's for:'
     O                  68                       58 'Match on EXCLUDE ST Code'
     O                  68                       64 's for:'
     O                       EMNAME             106
     O*
     O          E            TOTAL       1  1
     O                                           17 'TOTAL NUMBER OF'
     O                                           28 'EMPLOYERS:'
     O                       XXCNT         1     38
     O*
     O                  67                       80 'No Match on INCLUDE Code'
     O                  67                       82 's:'
     O                  67   #ICNT         1     92
     O                  68                       80 'Match on EXCLUDE ST Code'
     O                  68                       82 's:'
     O                  68   #ECNT         1     92
     O*
     O          E            NOREC       3  1
     O                                           28 '*** NO RECORD MEETS THE'
     O                                           47 'SELECTED CRITERIA.'
     O                                           72 'NO UPDATE OCCURRED. ***'
     O*
