     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E8015R - EMP EXP MOD - CVT BSI-5  to EMPLOYER PAYROLL,
     H*               EMPLOYER BATCH YEAR, EMPLOYER MASTER FILES
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**  THIS PROGRAM CONVERTS THE  BSI - 5 PAYROLL WORK FILE  TO   **
     H**  EXPERIENCE MOD MASTER FILES                                **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E8011C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    03/05/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/21/01  C:136  PROJECT: WC577                               *
     H*                  Delete only the records in the workfile that *
     H*                  were written by the program.                 *
     H*                                                               *
     H* 09/18/98  C:35   PROJECT: SUPPORT WA071 13814                 *
     H*                  CORRECTED SIZE OF DATE FIELD                 *
     H*                                                               *
     H* 08/12/98  C:90   PROJECT: WC853                               *
     H*                  MODIFIED TO VERSION 3 STANDARDS              *
     H*                                                               *
     H* 11/07/94  C:30   PROJECT: WC356                               *
     H*                  ASSIGN RATE SET CODE TO EMEPP FILE.          *
     H*                  CO# PASSED TO PGM.                           *
     H*                                                               *
     H*  7-22-92  LJQ    CHANGED POLICY TO DATE TO SUB 1 DAY          *
     H*                  TO GET CORRECT POLICY END DATE               *
     H*                                                               *
     H*  6-05-92  LJQ    PROJECT #MD009                               *
     H*                  CHANGED CHAIN TO EMEMP FILE TO USE           *
     H*                  UPCO# INSTEAD OF B5CO#                       *
     H*                                                               *
     H* 12-19-91  LJQ    CHANGED UPDATE TO BATCH FILE; DO NOT         *
     H*                  RECORD IF DETAIL DOES NOT EXIST IN           *
     H*                  WORK COMP SYSTEM                             *
     H*                                                               *
     H*  9-19-91  LJQ    CHANGED FOR RATE DATE FOR BSI5  FILE         *
     H*                  ADDED EFFECT DATE FOR RATE                   *
     H*                                                               *
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    EMEML (PEM01): FUND/EMPLOYER                             **
     H**    EMEBL (PEB01): FUND/EMP#/BATCH                           **
     H**    EMEPL (PEP01): FUND/EMP#/BATCH/CLASS                     **
     H**    EMEPL1(PEP02): FUND/BATCH                                **
     H**    EXB5P (PB501)                                            **
     H*****************************************************************
     H*
     F*
     F* Employer Payroll File
     FEMEPL2    UF A E           K DISK
     FEMEPL1    UF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F*
     F* Employer Master File
     FEMEML     UF A E           K DISK
     F*
     F* Employer Batch File
     FEMEBL     UF A E           K DISK
     F*
     F* BSI-5 Loss Work File
     F*
     FEXB5P     IF   E           K DISK
     F*
     F* Work Comp Employer Master File
     F*
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F*
     F* Work Comp Employer Detail File
     F*
     FWDELL     IF   E           K DISK
     F*

     D*
     D* Array for Cancellation Dates for employer File - Work Comp Side
     D*
     D*
     D* Data Structure for Cancellation Dates
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*  RENAME THE DUPLICATE FIELD NAMES.
     IPEM02
     I              EMCO#                       WMCO#
     I              EMFND                       WMFND
     I              EMEMP#                      WMEMP#
     I              EMNAME                      WMNAME
     I              EMMAD1                      WMMAD1
     I              EMMCTY                      WMMCTY
     I              EMMST                       WMMST
     I              EMMZIP                      WMMZIP
     I              EMEFD1                      WMEFD1
     I              EMSEMP                      WMSEMP
     I              EMCNTY                      WMCNTY
     I              EMSTAT                      WMSTAT
     I              EMUSER                      WMUSER
     I              EMPGM                       WMPGM
     I              EMTIME                      WMTIME
     I              EMDT                        WMDT
     I*

     C*
     C*  RENAME THE DUPLICATE FIELD NAMES.
     C*
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    EBFND         XXFND
     C     *LIKE         DEFINE    EBYEAR        XXYEAR
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDT          XXRDAT
     C     *LIKE         DEFINE    ELDT          X1TO
     C     *LIKE         DEFINE    ELDT          XXPTDT
     C     *LIKE         DEFINE    ELDT          XXPFDT
     C     *LIKE         DEFINE    EBFND         SVFND
     C     *LIKE         DEFINE    EBYEAR        SVYEAR
     C     *LIKE         DEFINE    EBEMP#        SVEMP#
     C*
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* Key List for the Employer Payroll File
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXRDAT
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYEAR
     C*
     C* Key List for the Employer Batch File
     C*
     C     EBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C*
     C* Key for Work Comp Employer Master
     C*
     C     WEMKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
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
     C* Initialize and Set-up
     C*
     C                   MOVE      QQCO#         XXCO#
     C*
     C* First delete all records in Employer Payroll with same
     C* fund/batch year
     C*
     C                   EXSR      DELWK
     C                   Z-ADD     *ZERO         SVFND
     C                   Z-ADD     *ZERO         SVEMP#
     C                   Z-ADD     *ZERO         SVYEAR
     C*
     C* Begin Loop to Read All Records in Work File
     C*
     C                   READ      PB501                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check to see if payroll record exists; if so add payroll amt
     C* to existing record; if not write new payroll record
     C*
     C                   Z-ADD     B5FND         XXFND
     C                   Z-ADD     B5EMP#        XXEMP#
     C                   Z-ADD     B5FYR         XXYEAR
     C                   Z-ADD     B5CLAS        XXCLAS
     C                   Z-ADD     B5RDAT        XXRDAT
     C*
     C*   ACCESS FUND DETAIL RECORD TO OBTAIN 'RATE SET CODE'.
     C*
     C     XXFND         IFNE      SVFND
     C     XXEMP#        ORNE      SVEMP#
     C     XXYEAR        ORNE      SVYEAR
     C                   Z-ADD     XXYEAR        XXFYR
     C                   Z-ADD     *ZERO         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        ELRTCD
     C                   END
     C                   Z-ADD     XXFND         SVFND
     C                   Z-ADD     XXEMP#        SVEMP#
     C                   Z-ADD     XXYEAR        SVYEAR
     C                   END
     C*
     C     EPKEY         CHAIN     PEP01                              50
     C     *IN50         IFEQ      '0'
     C     B5AAPR        MULT(H)   1             XXAMT
     C     XXAMT         ADD       EPAMT         EPAMT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   Z-ADD     DSTIME        EPTIME
     C                   MOVE      Q1USER        EPUSER
     C                   MOVE      Q1PGM         EPPGM
     C                   MOVE      ELRTCD        EPDRSC
     C                   UPDATE    PEP01
     C                   ELSE
     C                   Z-ADD     XXCO#         EPCO#
     C                   Z-ADD     B5FND         EPFND
     C                   Z-ADD     B5FYR         EPYEAR
     C                   Z-ADD     B5EMP#        EPEMP#
     C                   Z-ADD     B5CLAS        EPCLAS
     C                   Z-ADD     B5RDAT        EPRDAT
     C     B5AAPR        MULT(H)   1             XXAMT
     C                   Z-ADD     XXAMT         EPAMT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   Z-ADD     DSTIME        EPTIME
     C                   MOVE      Q1USER        EPUSER
     C                   MOVE      Q1PGM         EPPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPIDT
     C*
     C                   MOVE      Q1USER        EPIUSR
     C                   MOVE      ELRTCD        EPDRSC
     C                   WRITE     PEP01
     C                   END
     C*
     C* Check to see if employer master record exists; if not write
     C* new emplyer master file
     C*
     C                   Z-ADD     B5FND         XXFND
     C                   Z-ADD     B5EMP#        XXEMP#
     C     EMKEY         CHAIN     PEM01                              50
     C     *IN50         IFEQ      '1'
     C                   EXSR      GETEMP
     C     XNOEMP        IFNE      'N'                                          LJQ 12-19-91
     C                   Z-ADD     XXCO#         EMCO#
     C                   Z-ADD     B5FND         EMFND
     C                   Z-ADD     B5EMP#        EMEMP#
     C                   MOVE      'Y'           EMMOD
     C                   MOVE      'A'           EMSTAT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   Z-ADD     DSTIME        EMTIME
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMIDT
     C*
     C                   MOVE      Q1USER        EMIUSR
     C                   WRITE     PEM01
     C                   END                                                    LJQ 12-19-91
     C                   END
     C*
     C* check for batch year record
     C*
     C                   Z-ADD     B5FND         XXFND
     C                   Z-ADD     B5EMP#        XXEMP#
     C                   Z-ADD     0             XXYEAR
     C                   MOVE      QQFYR         XXYEAR
     C     EBKEY         CHAIN     PEB01                              50
     C     *IN50         IFEQ      '1'
     C                   EXSR      GETEMP
     C     XNOEMP        IFNE      'N'                                          LJQ 12-19-91
     C                   Z-ADD     XXCO#         EBCO#
     C                   Z-ADD     B5FND         EBFND
     C                   Z-ADD     B5EMP#        EBEMP#
     C                   Z-ADD     0             EBYEAR
     C                   MOVE      QQFYR         EBYEAR
     C                   MOVE      'A'           EBSTAT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   Z-ADD     DSTIME        EBTIME
     C                   MOVE      Q1USER        EBUSER
     C                   MOVE      Q1PGM         EBPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBIDT
     C*
     C                   MOVE      Q1USER        EBIUSR
     C                   WRITE     PEB01
     C                   END                                                    LJQ 12-19-91
     C                   END
     C*
     C* Get the Next Record
     C*
     C                   READ      PB501                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*          Subroutine to Clear Fields in PC Payroll File        *
     C*****************************************************************
     C*
     C     CLR           BEGSR
     C*
     C                   Z-ADD     0             EPCO#
     C                   Z-ADD     0             EPFND
     C                   Z-ADD     0             EPEMP#
     C                   Z-ADD     0             EPYEAR
     C                   Z-ADD     0             EPCLAS
     C                   Z-ADD     0             EPAMT
     C                   MOVE      *BLANKS       EPSTAT
     C                   MOVE      *BLANKS       EPUSER
     C                   MOVE      *BLANKS       EPPGM
     C                   MOVE      *BLANKS       EPIUSR
     C                   Z-ADD     0             EPTIME
     C                   Z-ADD     0             EPDT
     C                   Z-ADD     0             EPIDT
     C                   Z-ADD     0             EPRDAT
     C*
     C                   Z-ADD     0             XXAMT            11 0
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
     C****************************************************************
     C*  DELWK - subroutine to delete loss file records
     C****************************************************************
     C*
     C* This subroutine will delete records from the work file for
     C* fund, batch year
     C*
     C     DELWK         BEGSR
     C*
     C                   Z-ADD     0             XXFND
     C                   MOVE      QQFND         XXFND
     C                   Z-ADD     0             XXYEAR
     C                   MOVE      QQFYR         XXYEAR
     C*
     C     EPKY1         CHAIN     PEP02                              60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD ******** 08/21/01 ****************************************
     C     EPPGM         IFEQ      'E8015R'
      *** END ADD ******** 08/21/01 ****************************************
     C                   DELETE    PEP02
      *** BEG ADD ******** 08/21/01 ****************************************
     C                   ENDIF                                                  EPPGM
      *** END ADD ******** 08/21/01 ****************************************
     C     EPKY1         READE     PEP02                                  60
     C                   END
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*  GETEMP - subroutine to retrieve work comp information
     C****************************************************************
     C*
     C* This subroutine will retrieve employer information from
     C* work comp employer master and detail files
     C*
     C     GETEMP        BEGSR
     C                   MOVE      *BLANKS       XNOEMP                         LJQ 12-19-91
     C*
     C*   CHAIN OUT TO THE EMPLOYER MASTER FILE AND GET EMPLOYER/DIV
     C*   THAT HAS THE STATE FLAG SET ELSE CHAIN TO EMPLOYER/DIV=0
     C*
     C     WEMKY         CHAIN     PEM02                              60
     C     *IN60         DOWEQ     '0'
     C     EMSTRP        ANDNE     'Y'
     C     WEMKY         READE     PEM02                                  60
     C                   END
     C*
     C     EMSTRP        IFNE      'Y'
     C     WEMKY         CHAIN     PEM02                              60
     C                   END
     C*
     C* get the detail record
     C*
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     WMCO#         XXCO#
     C                   Z-ADD     WMFND         XXFND
     C                   Z-ADD     WMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'                                          LJQ 12-19-91
     C                   Z-ADD     ELPFDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
     C*
     C*   FLIP POLICY DATES FROM YMD TO MDY
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      **** BEGIN CHG **** 09/18/98 *********************************************
     C******************** MOVELSRDTO     XXTO    60
     C                   MOVEL     SRDTO         XXTO              8 0
      **** BEGIN CHG **** 09/18/98 *********************************************
     C*
     C* SUBTRACT 1 DAY FROM POLICY TO DATE TO MAKE IT = THE LAST DAY OF
     C* OF THE POLICY
     C*
     C                   MOVE      XXTO          SRDTE             8 0
     C                   Z-ADD     1             SRPST             5 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C                   MOVE      SRDTE         XXTO
     C*
     C*
     C*
     C*   DETERMINE IF POLICY HAS BEEN CANCELED IF SO MOVE INTO
     C*   POLICY TO DATE
     C*
     C     1             DO        5             X                 2 0
     C     CD(X)         IFGT      XXPFDT
     C     CD(X)         ANDLT     XXPTDT
     C                   Z-ADD     CD(X)         X1TO
     C*
     C                   Z-ADD     X1TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO
     C*
     C                   END                                                    IF CD,X END
     C                   END                                                    DO END
     C                   Z-ADD     XXPFDT        EBFRDT
     C                   Z-ADD     XXPFDT        EMEFD1
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   MOVE      X1TO          EBTODT
     C                   ELSE                                                   LJQ 12-19-91
     C                   MOVE      'N'           XNOEMP            1            LJQ 12-19-91
     C                   END                                                    LJQ 12-19-91
     C*
     C* get the employer's correct address
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     WMMAD1        EMMAD1
     C                   MOVEL     WMMCTY        EMMCTY
     C                   MOVEL     WMMST         EMMST
     C                   Z-ADD     WMMZIP        EMMZIP
     C                   ELSE
     C                   MOVEL     EMPAD1        EMMAD1
     C                   MOVEL     EMPCTY        EMMCTY
     C                   MOVEL     EMPST         EMMST
     C                   Z-ADD     EMPZIP        EMMZIP
     C                   END
     C*
     C                   MOVEL     WMNAME        EMNAME
     C                   MOVEL(P)  WMSEMP        EMSEMP
     C                   MOVE      WMCNTY        EMCNTY
     C*
     C                   ELSE
     C*
     C                   MOVE      'N'           XNOEMP            1            LJQ 12-19-91
     C                   Z-ADD     0             EMCO#
     C                   Z-ADD     0             EMFND
     C                   Z-ADD     0             EMEMP#
     C                   MOVE      *BLANKS       EMNAME
     C                   MOVE      *BLANKS       EMMAD1
     C                   MOVE      *BLANKS       EMMCTY
     C                   MOVE      *BLANKS       EMMST
     C                   Z-ADD     0             EMMZIP
     C                   Z-ADD     0             EMEFD1
     C                   MOVEL     *BLANKS       EMSEMP
     C                   MOVE      *BLANKS       EMCNTY
     C                   MOVE      *BLANKS       EMMOD
     C                   MOVE      *BLANKS       EMSTAT
     C                   MOVE      *BLANKS       EMUSER
     C                   MOVE      *BLANKS       EMPGM
     C                   Z-ADD     0             EMTIME
     C                   Z-ADD     0             EMDT
     C                   MOVE      *BLANKS       EMIUSR
     C                   Z-ADD     0             EMIDT
     C*
     C                   Z-ADD     0             EBCO#
     C                   Z-ADD     0             EBFND
     C                   Z-ADD     0             EBEMP#
     C                   Z-ADD     0             EBYEAR
     C                   Z-ADD     0             EBFRDT
     C                   Z-ADD     0             EBTODT
     C                   MOVE      *BLANKS       EBSTAT
     C                   MOVE      *BLANKS       EBUSER
     C                   MOVE      *BLANKS       EBPGM
     C                   Z-ADD     0             EBTIME
     C                   Z-ADD     0             EBDT
     C                   MOVE      *BLANKS       EBIUSR
     C                   Z-ADD     0             EBIDT
     C*
     C                   END
     C*
     C                   ENDSR
