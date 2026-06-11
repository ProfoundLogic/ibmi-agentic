     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7169R - REPORT - Auto Reinstatement
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: W7169C - OVERRIDE - Auto Reinstatement           *
     H*                                                               *
     H* CALLS TO:    W7167C - REINSTATEMENT PROCESSING                *
     H*                                                               *
     H* CREATED: 03/08/05  C:132 PROJECT WC1226                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 10/06/16 C:65  Project: SY371                                 *
     H*                Set up local data area for Scriptura values.   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 06/21/12  C:282  Project: WC2086                              *
     H*           REINSTATEMENT REASON CODE ADDED IN SYSTEM           *
     H*           TABLE VALUE.                                        *
     H*                                                               *
     H* 08/13/10  C:257  Project: 114WWV                              *
     H*           SUPPRESS ADDRESS LABELS FOR REINST NTC              *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/29/08 C:191 Seq#: WA045/66022                              *
     H*                Re-instate error for renewal year              *
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA099/44610                              *
     H*                Release record lock                            *
     H*                                                               *
     H* 09/25/06 C:132 Project: WC1446                                *
     H*                Auto-Reinstatement/Cancellation Billings       *
     H*                Through Date Flag.                             *
     H*                                                               *
     H* 11/07/06 C:61  SUPPORT WA129 53118                            *
     H*                Write Quote Status (WAQTP) record with current *
     H*                agent number.                                  *
     H*                                                               *
     H* 03/15/05 C:132 PROJECT WC1226                                 *
     H*                Re-Compile after restore from backup-No Chgs   *
     H*                                                               *
     H* 03/08/05 C:132 PROJECT WC1226                                 *
     H*                Calculate Bills and Payments as described in   *
     H*                the technical specifications document.         *
     H*                                                               *
     H* 01/11/05 C:61  SUPPORT WA109 45383                            *
     H*                Do not Reinstate if "CX issue date" is in      *
     H*                future.                                        *
     H*                                                               *
     H* 04/21/04 C:61  SUPPORT WA129 42216 129WDM                     *
     H*              - Calc A/R balance as of CX date.                *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: 129WBF (Implement WC1033 into mods)   *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 01/14/04 C:61  PROJECT 129WDM                                 *
     H*              - Add 'V'oids to bills in A/R Balance Calc.      *
     H*              - Include prior year uncontested audited A/R     *
     H*                balance if not agency billed if that was the   *
     H*                criteria used in the mass cancellation.        *
     H*              - Only include in A/R balance if posting date    *
     H*                is less than CX issue date.                    *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  90       *off= Auto Reinstate,  *on= Don't Auto Reinstate    *
     H*  91       Read primary file                                   *
     H*  92       Read secondary file                                 *
     H*  93       No records found                                    *
     H*  99       Pgm call                                            *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * User Parameter
     FSMUPL     IF   E           K DISK
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * Cancellation Reason Code
     FWTCRL     IF   E           K DISK
      *
      * Cancellation Detail
     FWDCXL16   IF   E           K DISK
     FWDCXL5    IF   E           K DISK
     F                                     RENAME(PCX01:PCX05)
     F                                     INFDS(CXFDS5)
     FWDCXP     UF   E             DISK
     F                                     RENAME(PCX01:PCX0P)
     FWDCXL10   IF   E           K DISK
     F                                     RENAME(PCX01:PCX10)
      *
      * Employer
     FWMEML     IF   E           K DISK
      *
      * Accounts Receivable
     FWMEAL6    IF   E           K DISK
      *
      * Cancellation History
     FWDXHL     IF A E           K DISK
      *
      * Quote
     FWAQTL7    IF A E           K DISK
      *
      * Employer Policy Year
     FWDELL     IF   E           K DISK
     F*** begin add ***  10/06/16  ************************************
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX00)
     FSTSPL     IF   E           K DISK    rename(psp01:psp00)
     F*** end add *****  10/06/16  ************************************
      *
      * printed output - Employer Auto Reinstated
     FQSYSPRT   O    F  120        PRINTER OFLIND(*INOA)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *** begin add ***  10/06/16  ************************************
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Reinstatements')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add *****  10/06/16  ************************************
      *
      *
      *================================================================
      * CONSTANTS
      *================================================================
      *
     D XTITL           C                   CONST('Employer Auto Reinst-
     D                                     atment Report')
      *
     D CXFDS5          DS
     D  CXRRN5               397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *================================================================
      *   KEY LISTS
      *================================================================
      *
      *  Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      *   Output Routing
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   User Parameter
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Cancellation Reason Code
      *
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      *
      *  Cancellation Detail
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CRST
     C                   KFLD                    XXRSN
      *
     C     CXKY2         KLIST
     C                   KFLD                    X2CO#
     C                   KFLD                    X2FND
     C                   KFLD                    X2FYR
     C                   KFLD                    X2CXDT
     C                   KFLD                    X2EMP#
      *
     C     CXKY3         KLIST
     C                   KFLD                    X2CO#
     C                   KFLD                    X2FND
     C                   KFLD                    X2EMP#
     C                   KFLD                    X2DIV
     C                   KFLD                    X2FYR
      *
     C     CXKY4         KLIST
     C                   KFLD                    X2CO#
     C                   KFLD                    X2FND
     C                   KFLD                    X2EMP#
     C                   KFLD                    X2DIV
      *
      *  Cancellation History
      *
     C     XHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXREQ#
      *
      *  Employer
      *
     C     EMKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      *  Accounts Recievable
      *
     C     EAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    X3FYR
      *
      *  Quote
      *
     C     QTKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXAGT#
      *
     C     QTKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    XXSTAT
      *
      *
      *
      *================================================================
      *   *LIKE Define Variables
      *================================================================
     C     *LIKE         DEFINE    CXAGT#        XXAGT#
     C     *LIKE         DEFINE    CXFYR         X3FYR
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXRSN         XXRSN
      *
     C     *LIKE         DEFINE    CXCO#         X2CO#
     C     *LIKE         DEFINE    CXFND         X2FND
     C     *LIKE         DEFINE    CXFYR         X2FYR
     C     *LIKE         DEFINE    CXEMP#        X2EMP#
     C     *LIKE         DEFINE    CXDIV         X2DIV
     C     *LIKE         DEFINE    CXCXDT        X2CXDT
      *
     C     *LIKE         DEFINE    CRST          XXST
     C     *LIKE         DEFINE    CRKEY         XXKEY
      *
      *
      *================================================================
      *   PARAMETER LISTS INCLUDING ENTRY PARMS
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DATE            8
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
      *   Initialize
      *
       //*** begin add ***  10/06/16  ****************************************
      /free

       isUseScript = UseScriptura(processType);

      /end-free
       //*** end add *****  10/06/16  ****************************************

     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            120
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *  PGMSTR executes first
      *
     C                   EXSR      PGMSTR
      *
      *  Print Routing Page
      *
     C                   EXSR      SRRTE
      *
     C                   SETON                                        OA
      *
      *  Read CX reason code file
      *
     C     CRKY          CHAIN     PCR01                              91
      *
     C     *IN91         DOWEQ     *OFF
      *
     C     CREFDT        IFLE      XXDATE
      *
     C     CRAREI        IFEQ      'Y'
      *
      *  Get Grace period date
      *
     C                   Z-ADD     DSDATE        ##GRDT
     C                   Z-ADD     CRRDAY        ##DAYS
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    ##GRDT            8 0
     C                   PARM                    ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C                   Z-ADD     ##GRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXGRDT            7 0
      *
      * Read CX Detail file
      *
     C                   MOVEL(P)  CRKEY         XXRSN
     C     CXKY          CHAIN     PCX01                              92
      *
     C     *IN92         DOWEQ     *OFF
     C                   EXSR      SRCK
     C     CXKY          READE     PCX01                                  92
     C                   ENDDO                                                  *IN92
      *
     C                   ENDIF                                                  CRAREI = 'Y'
      *
     C                   MOVEL(P)  CRST          XXST
     C                   MOVEL(P)  CRKEY         XXKEY
     C     CRKY1         SETGT     PCR01
     C                   ENDIF                                                  CREFDT <= XXDATE
      *
     C     CRKY          READE     PCR01                                  91
     C                   ENDDO                                                  *IN91
      *
      *  End of Processing
      *
     C   OA
     CANN93              EXCEPT    HEAD
     C  N93              EXCEPT    NOREC
      *
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
      *
      *
      *
      *================================================================
      * SRCK - Check if CX Detail record meets Auto Reinstate Criteria
      *================================================================
     CSR   SRCK          BEGSR
      *
      *  Assume Auto Reinstate
      *
     C                   SETOFF                                       90
      *
      *  Must be state Reporting Division
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *ON
     C     EMSTRP        ORNE      'Y'
     C                   SETON                                        90
     C                   ENDIF
      *
      *  Must be within Grace period
      *
     C  N90CXCXDT        IFLT      XXGRDT
     C                   SETON                                        90
     C                   ENDIF
      *
      *  Must have been issued.
      *
     C  N90CXCDAT        IFGT      SYSDAT
     C                   SETON                                        90
     C                   ENDIF
      * If flat cancelled for renewal year, then check current record.
      *   if current year record re-instated then only renewal year will
      *   re-instated
     C  N90CXAFLT        IFEQ      'Y'
     C                   SETON                                        90
     C* Check previous year cancellation
     C                   EXSR      SRPRYR
     C                   ENDIF                                                  CXAFLT = 'Y'
      *
      *  A/R balance must meet minimum percentage
      *
     C  N90              EXSR      SRARB
      *
     C                   ENDSR                                                  SRCK
      *
      *
      *
      *================================================================
      * SRARB - Verify A/R balance
      *================================================================
     CSR   SRARB         BEGSR
      *
      * Calculate A/R Balance for employer considering ALL transactions
      * across all divisions cancelled on the same date that have
      * not been reinstated.
      *
     C                   MOVE      CXCO#         X2CO#
     C                   MOVE      CXFND         X2FND
     C                   MOVE      CXFYR         X2FYR
     C                   MOVE      CXCXDT        X2CXDT
     C                   MOVE      CXEMP#        X2EMP#
      *
      *  BILLS = B + A + D + F + H
      *  PMTS  = P + Q + I - R + V - N
      *
     C                   Z-ADD     *ZERO         XXBILL           15 2
     C                   Z-ADD     *ZERO         XXPMTS           15 2
     C     CXKY2         CHAIN(N)  PCX05                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     CXREDT        IFEQ      *ZERO
      *
      *
      *  Current Estimated
      *  OR  Both (Current Estimated and Prior Audited)
      *  OR  Cancelled as an Individual Employer
      *
     C     CXEAFL        IFEQ      'E'
     C     CXEAFL        OREQ      'B'
     C     CXEAFL        OREQ      ' '
     C                   Z-ADD     CXFYR         X3FYR
     C                   EXSR      ACCFYR
     C                   ENDIF                                                  EAFL = E or B
      *
      *  Prior Audited
      *  OR  Both (Current Estimated and Prior Audited)
      *
     C     CXEAFL        IFEQ      'A'
     C     CXEAFL        OREQ      'B'
     C                   EXSR      SRADTA
     C                   ENDIF                                                  EAFL = A or B
      *
      *
     C                   ENDIF                                                  CXREDT = *ZERO
      *
     C     CXKY2         READE(N)  PCX05                                  60
      *
     C                   ENDDO                                                  *IN60
      *
      *  MIN = BILLS - (BILLS * (CRPCNT / 100) )
      *
     C                   Z-ADD     CRPCNT        XXTMP1           15 5
     C                   DIV       100           XXTMP1
     C                   MULT      XXBILL        XXTMP1
     C     XXBILL        SUB       XXTMP1        XXMIN            15 5
      *
      *  If A/R Balance within specified percentage,
      *    BILLS - PMTS <= MIN
      *  then auto reinstate employer divisions with same CX date
      *
     C     XXBILL        SUB       XXPMTS        XXTMP2           15 5
      *
     C     XXTMP2        IFLE      XXMIN
      *
     C                   CLEAR                   CXRRN5
     C     CXKY2         CHAIN(N)  PCX05                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     *LIKE         DEFINE    CXRRN5        CXRRN
     C                   Z-ADD     CXRRN5        CXRRN
      *
     C     CXREDT        IFEQ      *ZERO
      *
      *  Process reinstatement
      *
     C                   EXSR      SRUPD
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CXDT            8 0
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
     C                   SETON                                        93
      *
     C                   ENDIF                                                  CXREDT = *ZERO
      *
     C                   CLEAR                   CXRRN5
     C     CXKY2         READE(N)  PCX05                                  60
      *
     C                   ENDDO                                                  *IN60
      *
     C                   ENDIF                                                  XXTMP2 <= XXMIN
      *
     C                   ENDSR                                                  SRARB
      *
      *
      *
      *================================================================
      *  SRADTA - accumulate the balance for all prior audited policy
      *           years (not including current policy year)
      *================================================================
     C     SRADTA        BEGSR
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    #1AGT#            5 0
     C***********        PARM                    #1ANAM           60
     C                   PARM      *zeros        #1AGT#            5 0
     C                   PARM      *blanks       #1ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
     C                   PARM                    #1AREA            3 0
     C                   PARM                    #1PHON            7 0
     C                   PARM      '*'           #1PLAN            1
     C                   PARM      *ZERO         #1DATE            8 0
      *
     C     ELFYR         IFLT      CXFYR
     C     ELFAF         ANDEQ     'A'
     C     ELCTST        ANDNE     'Y'
     C     #1PLAN        ANDNE     'A'
     C                   Z-ADD     ELFYR         X3FYR
     C                   EXSR      ACCFYR
     C                   ENDIF
      *
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  SRADTA
      *
      *
      *
      *================================================================
      *  ACCFYR - accumulate balance for policy year
      *================================================================
     C     ACCFYR        BEGSR
      *
     C     EAKY          CHAIN     PEA01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'                                          FEE PAID
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'T'
     C                   ADD       EAAMT         XXPMTS
     C                   ENDIF                                                  EAARTY='P'
      *
     C     EAARTY        IFEQ      'R'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'V'
     C                   SUB       EAAMT         XXPMTS
     C                   ENDIF                                                  EAARTY=R/N
      *
      *
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'                                          FEE BILLED
     C     EAARTY        OREQ      'H'
      *
      *
      *
      *
     C                   SELECT
      * "I" = Cancellation Issue Date, use WDCXP/CXCDAT
     C     CRICE         WHENEQ    'I'
     C     EAPODT        ANDLE     CXCDAT
     C                   ADD       EAAMT         XXBILL
      *
      * "C" = Current Date, use current system date
     C     CRICE         WHENEQ    'C'
     C     EAPODT        ANDLE     SYSDAT
     C                   ADD       EAAMT         XXBILL
      *
      * "E" = Cancellation Effective Date, use WDCXP/CXCXDT
     C     CRICE         WHENEQ    'E'
     C     EAPODT        ANDLE     CXCXDT
     C                   ADD       EAAMT         XXBILL
      *
     C                   OTHER
      * " " = IF blank, use Cancellation Effective Date-WDCXP/CXCXDT
     C     EAPODT        IFLE      CXCXDT
     C     CRICE         ANDEQ     ' '
     C                   ADD       EAAMT         XXBILL
     C                   ENDIF                                                  EAPODT<=X1DATE
     C                   ENDSL
      *
     C                   ENDIF                                                  EAARTY IFEQ
      *
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ENDSR                                                  ACCFYR
      *
      *
      *
      *
      *
      *================================================================
      * SRUPD - Update files - process reinstatement
      *================================================================
     CSR   SRUPD         BEGSR
     C*
     C     CXRRN         CHAIN     WDCXP                              60
      *
      * Update Cancellation Detail with Reinstatement info
      *
     C                   Z-ADD     CXCXDT        CXREDT
     C                   MOVE      'Y'           CXREIN
     C                   MOVE      'N'           CXRCRT                         GEN CERT LETTERS
     C                   MOVE      *BLANK        CXRDRT                         DART CODE
     C                   MOVE      'N'           CXERR
      *
     C                   MOVEL(P)  *BLANKS       CXRCM1
     C                   MOVEL(P)  *BLANKS       CXRCM2
     C                   Z-ADD     0             CXCFDT
      *
     C                   Z-ADD     SYSDAT        CXRDAT
      *
     C***  Begin Chg  ***  06/21/12  **************************************
      /Free
       //For Auto-Reinstatement, Load Reinstatement Reason Code as 01
        CXreCd = '01';
      /End-Free
     C***  End   Chg  ***  06/21/12  **************************************
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     SYSDAT        CXDT
     C                   Z-ADD     DSTIME        CXTIME
      *
     C                   UPDATE    PCX0P
      *
      *===============================================================
      *
      *  Write Cancellation History record
      *
     C     XHKY          CHAIN     PXH01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     SYSDAT        XHDT
     C                   Z-ADD     DSTIME        XHTIME
     C                   MOVE      *BLANK        XHRVFL
     C                   MOVEL     CXRSN         XHRSN
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C                   MOVE      CXERR         XHERR
     C                   Z-ADD     SYSDAT        XHTRDT
     C***  Begin Chg  ***  06/21/12  **************************************
      /Free
       //For Auto-Reinstatement, Load Reinstatement Reason Code as 01
        XHreCd = '01';
      /End-Free
     C***  End   Chg  ***  06/21/12  **************************************
     C                   WRITE     PXH01
     C                   ENDIF                                                   *IN60=*OFF
      *
      *===============================================================
      *
      *  Write an 'R' type quote record
      *
      *
      *  Get current agent
      *
     C                   CALL      'WA002R'                             99
     C                   PARM                    CXCO#
     C                   PARM                    CXFND
     C                   PARM                    CXEMP#
     C                   PARM                    CXDIV
     C                   PARM      *ZERO         #1FYR             3 0
     C     XXAGT#        PARM      *ZERO         #1AGT#            5 0
     C                   PARM      *BLANK        #1ANAM           60
      *
     C     QTKY          SETGT     PQT01                                        CO/FND/EMP/DIV/ST/AG
     C     QTKY          READPE    PQT01                                  60
     C   60QTKY1         CHAIN     PQT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     XXAGT#        QTAGT#
     C                   MOVE      'R'           QTTYPE
     C                   TIME                    DSDTTM
     C                   Z-ADD     SYSDAT        QTDT
     C                   Z-ADD     DSTIME        QTTIME
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   WRITE     PQT01
     C                   ENDIF                                                  QTTYPE = C
      *
       //*** begin add ***  10/06/16  ****************************************
      /free
       If isUseScript;

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) (CXCo#:CXFnd) pex00;

          Select;
          When %found and EXRECD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXRECD;
          Other;
             xxDart = 'N';
             xxRMCD = ' ';
          Endsl;

          xxProcType = processType;

          Chain(n) xxProcType psp00;

          xxCopy = SPOutM;

          If xxDart = 'N';
             xxCopy = 'Y';
          Endif;

          xxPrinter = SPPKeyM;
          xxTray = SPTrayM;

          If xxPrinter <> ' ';
             xxPrint = 'Y';
             xx#CopiesA = '001';
          Else;
             xxPrint = ' ';
             xx#CopiesA = '000';
          Endif;

          Out *dtaara;

       Endif;

      /end-free
       //*** end add *****  10/06/16  ****************************************
      *
      *===============================================================
      *
      *  Submit reinstatement print program
      *
     C                   MOVE      CXCO#         ##CO#             3
     C                   MOVE      CXFND         ##FND             3
     C                   MOVE      CXFYR         ##FYR             3
     C                   MOVE      CXEMP#        ##EMP#            9
     C                   MOVE      CXDIV         ##DIV             5
     C                   MOVE      CXREQ#        ##REQ#            5
     C                   MOVEL(P)  OROUTQ        ##OUTQ           10
      *
     C                   CALL      'W7167C'                             99
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##REQ#            5
     C                   PARM      'N'           ##RCRT            1            PRT CERT LETTR?
     C                   PARM      'N'           ##REPR            1            REPRINT? (Y/N)
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##TYPE            1            LABEL R / L?
      *
     C                   ENDSR                                                  SRUPD
      *
      *
      *
      *================================================================
      * PGMSTR - Set-up and Initialize
      *================================================================
     CSR   PGMSTR        BEGSR
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT            7 0
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@DATE        X1DATE            8 0
      *
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
      *
     C                   SETOFF                                       93
      *
     C                   MOVE      'W'           XXSTAT            1            ACTIVE ENVIRONMENT
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * SRRTE - Print routing page
      *================================================================
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *  User Name if no Distribution specified in routing
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL         ORTITL
      *
     C                   ENDIF
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   ENDSR                                                  SRRTE
      *
      *================================================================
      * SRPRYR  - Check previous year cancellations
      *================================================================
     CSR   SRPRYR        BEGSR
     C*
     C                   MOVE      CXCO#         X2CO#
     C                   MOVE      CXFND         X2FND
     C                   MOVE      CXEMP#        X2EMP#
     C                   MOVE      CXDIV         X2DIV
     C                   MOVE      CXFYR         X2FYR
     C* Store current record cancel date for later use
     C                   MOVE      CXCXDT        X2CXDT
     C* Set lower limit on current year and read back for previous year cx rec.
     C     CXKY3         SETLL     PCX10                              60
     C     CXKY4         READPE(N) PCX10                                  60
     C     *IN60         IFEQ      *OFF
     C     CXREDT        IFNE      *ZERO
     C* Previous year record re-instated, so reinstate current year
     C                   SETOFF                                       90
     C                   ENDIF                                                  CXREDT=*ZERO
     C                   ELSE
     C* Previous year cancellation not found, reinstate current year
     C                   SETOFF                                       90
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C* Reset to current year and cancellation date
     C                   MOVE      X2FYR         CXFYR
     C                   MOVE      X2CXDT        CXCXDT
     C*
     C                   ENDSR                                                  SRPRYR
      *
      *
      *================================================================
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      **********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS2              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS3              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS4              56
      **********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
      **********
     O          E            ROUTE          1
     O                       ORDIS2              56
      **********
     O          E            ROUTE          1
     O                       ORDIS3              56
      **********
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
      **********
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      **********
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                           26 'COMPANY . . . . :'
     O                       XXCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'DATE  . . . . . :'
     O                       X1DATE            +001 '  /  /    '
      *
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *
      *
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    120
      *
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          110 'PAGE:'
     O                       PAGE          Z    120
      *
     O          E            HEAD        1  1
     O                                         +000 'Rating'
     O                                         +002 'Cancel'
     O                                         +032 'Policy'
     O                                         +004 'Cancel'
      *
     O          E            HEAD        0  0
     O                       LINE              +000
      *
     O          E            HEAD           2
     O                                         +000 'State'
     O                                         +003 'Reason'
     O                                         +002 'Group'
     O                                         +004 'Employer'
     O                                         +002 'Division'
     O                                         +004 'Year'
     O                                         +006 'Date'
      *
     O          E            DET            1
     O                       CXRST             +002
     O                       CXRSN             +006
     O                       CXFND         4   +005
     O                       CXEMP#        4   +005
     O                       CXDIV         4   +002
     O                       CXFYR         4   +006
     O                       X1CXDT        Y   +005
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
      ** ========================================================== **
