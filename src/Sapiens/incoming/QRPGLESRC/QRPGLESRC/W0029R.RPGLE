     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0029R - CANCELLATION CONFIRMATION CONTROL MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W0010R - WORK WITH GROUPS                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 09/21/98  PROGRAMMER: C:87  PROJECT: WC762          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/28/20  C:079 Project: WC2671                              *
     H*                  Add Audit Request Fields                     *
     H*                                                               *
     H*  05/03/10  C:220 PROJECT: WC1881                              *
     H*                  Add Ctrl flag to Final Audit Request Form    *
     H*                                                               *
     H*  11/06/07  C:183 PROJECT: 71WIL                               *
     H*                  Added a prompt for resolving the CX document *
     H*                  to DARTS.                                    *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H*  03/17/99  C:33  PROJECT: 48WOM                               *
     H*                  ALLOW FOR INQUIRY MODE                       *
     H*****************************************************************
     H*
     FW0029F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMCFL     UF A E           K DISK
     F                                     RENAME(PCF01:DBRCDF)
     F*
     FWMFNP     IF   E             DISK
     F*
     FSTCTL1    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Confirm-
     D                                     ation Control')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    $$RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     @@RRN         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Check that a value is entered in field
     C  N90SCCFSD        IFEQ      *ZERO
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C*  Check that date is valid
     C                   Z-ADD     SCCFSD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCCFSD
     C                   ELSE
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C*  Check that the following fields are 'Y' or ' '
     C*
     C  N90SCNOTC        IFNE      'Y'
     C     SCNOTC        ANDNE     ' '
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C  N90SCAURQ        IFNE      'Y'
     C     SCAURQ        ANDNE     ' '
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C  N90SCQTRK        IFNE      'Y'
     C     SCQTRK        ANDNE     ' '
     C                   SETON                                        75  90
     C                   ENDIF
     C*
     C  N90SCNECX        IFNE      'Y'
     C     SCNECX        ANDNE     ' '
     C                   SETON                                        76  90
     C                   ENDIF
     C*
     C*  Check that DARTS code is valid if entered
     C  N90SCCFDR        IFNE      *BLANK
     C     SCCFDR        CHAIN     PCT01                              77
     C     *IN77         IFEQ      *ON
     C                   SETON                                            90
     C                   ENDIF                                                  *IN77=*ON
     C                   ENDIF                                                  SCCFDR=*BLANK
     C*
     C*  Check that 'Resolve document to DARTS' code is valid
     C  N90SCCFDR        IFEQ      *BLANKS
     C     *IN77         OREQ      *ON
     C     SCDART        IFNE      *BLANK
     C                   SETON                                          7790
     C                   ENDIF                                                  SCDART NE BLANK
     C                   ENDIF                                                  SCCFDR NE BLANK
     C*
     C  N90SCDART        IFNE      *BLANK
     C     SCDART        ANDNE     'Y'
     C                   SETON                                          7990
     C                   ENDIF                                                  SCDART NE BLANK
     C*
     C*  Check that fields have not been updated since program entered
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CFDT          IFGT      SAVEDT
     C     CFDT          OREQ      SAVEDT
     C     CFTIME        ANDGT     SAVETM
     C                   SETON                                        78  90
     C                   ENDIF                                                  AUDT IFGT SAVEDT
     C*
     C     SCSRFL        IFNE      *BLANK
     C     SCSRFL        ANDNE     'Y'
     C                   SETON                                        90  53
     C                   ENDIF                                                  SCSRFL
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - Subroutine for processing F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCFDR'
     C*
     C                   MOVE      *BLANKS       QQRMCD            5
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVEL     QQRMCD        SCCFDR
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      *ON           *IN82
     C                   ENDIF                                                  QQRMCD IFNE
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CFCFSD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCFSD
     C*
     C                   MOVE      CFNOTC        SCNOTC
     C                   MOVE      CFAURQ        SCAURQ
     C                   MOVE      CFQTRK        SCQTRK
     C                   MOVE      CFNECX        SCNECX
     C                   MOVE      CFCFDR        SCCFDR
     C                   MOVE      CFDART        SCDART
     C                   MOVEL     CFSRFL        SCSRFL
       //*** Beg Add *** 10/28/20 *************************************
       scg301E = CFg301E;
       scg311E = CFg311E;
       scGaSrD = CFgAsRd;
       //*** End Add *** 10/28/20 *************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCCFSD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CFCFSD
     C*
     C                   MOVE      SCNOTC        CFNOTC
     C                   MOVE      SCAURQ        CFAURQ
     C                   MOVE      SCQTRK        CFQTRK
     C                   MOVE      SCNECX        CFNECX
     C                   MOVE      SCCFDR        CFCFDR
     C*
     C                   MOVEL     Q1USER        CFUSER
     C                   MOVEL     Q1PGM         CFPGM
     C                   Z-ADD     DSTIME        CFTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CFDT
     C                   MOVE      SCDART        CFDART
     C                   MOVEL     SCSRFL        CFSRFL
       //*** Beg Add *** 10/28/20 *************************************
       CFg301E = scG301E;
       CFg311E = scG311E;
       CFgAsRd = scGaSrD;
       //*** End Add *** 10/28/20 *************************************
     C*
     C*
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C*  Format date if valid
     C     SCCFSD        IFNE      *ZERO
     C*
     C                   Z-ADD     SCCFSD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCCFSD
     C                   ELSE
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR='0'
     C*
     C                   ENDIF                                                  SCCFSD<>*BLANK
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     $$RRN         CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNFND         KKFND
     C*
     C                   MOVE      *OFF          *IN40
     C*
     C     @@MODE        IFEQ      'D'
     C*
     C                   CLEAR                   DBRCDF
     C                   MOVE      *ON           *IN09
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN09
     C*
     C*   Check if record already exists for group, if not then
     C*   change @@MODE TO 'A' (add a record)
     C     @@RRN         CHAIN     DBRCDF                             61
     C     *IN61         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   MOVE      *ON           *IN40
     C                   ENDIF
     C*
     C                   ENDIF                                                  @@MODE=D
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   Z-ADD     KKCO#         CFCO#
     C                   Z-ADD     KKFND         CFFND
     C*
     C                   MOVEL     CFUSER        CFIUSR
     C                   MOVEL     CFPGM         CFIPGM
     C                   Z-ADD     CFTIME        CFITIM
     C                   Z-ADD     CFDT          CFIDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WMCFL
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR
