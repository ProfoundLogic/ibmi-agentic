     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3103R02 - AGENCY BILLING DETAIL INFORMATION
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM ALLOW FOR ADD, CHANGE OR DISPLAY THE AGENCY      *
     H* BILLING DETAIN INFORMATION.                                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W3103R                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H*  CREATED: 04/05/99  C:33  PROJECT: 96WAD                      *
     H*  IN BASE: 07/19/99  C:33  PROJECT: WC899                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/08/14 C:283 Support: WA089 84611                          *
     H*                 Fund should be required for Cash Transactions.*
     H*                                                               *
     H*  12/10/04 C:33  Project: WC1033                               *
     H*                 Include FEIN/MOD in file.                     *
     H*                                                               *
     H*  12/09/04 C:33  Seq#: WA132/44865                             *
     H*                -Add producer entry to 'Check' entry.          *
     H*                -Update editing on Group.                      *
     H*                                                               *
     H*  11/05/04  C:132  SUPPORT: WA132  44359                       *
     H*                   Program allowed employer information from   *
     H*                   Test Groups. Added code to check Test group *
     H*                   Flag (FNTEST) for "Y" and dsplay error msg. *
     H*                   This is checked for Adjustments & Checks.   *
     H*                                                               *
     H*  08/27/01 C:136 SEQ#: WA107/28446                             *
     H*                 Correct error check routine to not allow 0    *
     H*                 to be written to record if emp/div is selected*
     H*                 Also added error message WCR1272 to window.   *
     H*                                                               *
     H*  08/10/00 C:33 SEQ#: WA109/23913                              *
     H*                CHECK FOR INACTIVE RATE CODES.                 *
     H*                                                               *
     H*  02/25/00  C:33  PROJECT: WC899                               *
     H*                  ALLOW FOR CHECK ENTRYS TO BE 'I' OR 'Q' ALSO.*
     H*                                                               *
     H*  02/08/00  C:33  PROJECT: WC899                               *
     H*                  ALLOW FOR YEAR ENTRY ON CHECKS.              *
     H*                                                               *
     H*  12/16/99  C:33  PROJECT: WC961                               *
     H*                  IF THE CHECK ENTERED IS A REFUND, CREATE A   *
     H*                  VOUCHER FOR THE AGENT.                       *
     H*                                                               *
     H*  12/15/99  C:33  PROJECT: WC899                               *
     H*                  MOVE VOIDS AND REFUNDS TO CHECK ENTRY SCREEN *
     H*                                                               *
     H*  12/10/99  C:33  PROJECT: WC899           Y2K#: 25            *
     H*                 -CLEAR GROUP AND EMPLOYER NAMES.              *
     H*                 -FIX ADJUSTMENT GROUP SEARCH.                 *
     H*****************************************************************
     FW3103F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDABP     UF A E             DISK
     F                                     RENAME(PAB01:DBRCDF)
     FWSASP     UF   E             DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDS)
     FWMEML     IF   E           K DISK
     F***  BEGIN DEL  ***  02/08/00  *********************************
     F**********WMEMP   IF  E                    DISK
     F**********  PEM01                             KRENAMEPEM0P
     F***  END   DEL  ***  02/08/00  *********************************
     F***  Begin Add  ***  12/08/04  **************************************
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     F***  End   Add  ***  12/08/04  **************************************
     FWDELL     IF   E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL0P)
     FWMRSL     IF   E           K DISK
     FWMAHL1    IF   E           K DISK
     FWMAAL5    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     F                                     INFDS(AGFDS)
     FWMAPL     IF   E           K DISK
     FWMAPP     IF   E             DISK
     F                                     RENAME(PAP01:PAP0P)
     FWMARL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWDABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D*
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D AGFDS           DS
     D  AGRRN                397    400B 0
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  XXRWDT                 1      7  0
     D  XXRWYR                 1      3  0
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EMPLYR EFF DATES
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agency Billing Detai-
     D                                     l')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D***  BEGIN ADD  ***  12/16/99  *********************************
     D GLAPI           C                   CONST('SA002R')
     D***  END   ADD  ***  12/16/99  *********************************
     D #OFSCR          C                   CONST(1)
     D***  Begin Add  ***  10/08/14  *********************************
     D* Get client ID
     D GetClientID     pr                  ExtPgm('RTVCLTID')
     D   outCltID                     3

     D outClientID     s              3    inz('000')
     D***  End   Add  ***  10/08/14  *********************************

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN1            9 0          WSASP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     AGTPL         PLIST
     C                   PARM      ASCO#         ##CO#
     C                   PARM                    ##FND
     C                   PARM      *ZEROS        ##EMP#            9 0
     C                   PARM      *ZEROS        ##DIV             5 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      ASAGT#        ##AGT#            5 0
     C     SCANAM        PARM                    ##ANAM           60
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C     GLPL          PLIST
     C                   PARM      ' '           ##GLAP            1
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     FNKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    XXFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCFYR
     C*
     C     RSKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    ELRTCD
     C*
     C***  Begin Chg  ***  12/09/04  **************************************
     C********** AHKY      KLIST
     C**********           KFLD           ASCO#
     C**********           KFLD           SCFND
     C**********           KFLD           SCFYR
     C**********           KFLD           SCAGT#
     C**********           KFLD           SCEMP#
     C**********           KFLD           SCDIV
     C**********           KFLD           X7TRDT
     C**********
     C********** AHKY1     KLIST
     C**********           KFLD           ASCO#
     C**********           KFLD           SCFND
     C**********           KFLD           SCFYR
     C**********           KFLD           SCAGT#
     C**********           KFLD           SCEMP#
     C**********           KFLD           SCDIV
     C     AHKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCAGT#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    X7TRDT
     C*
     C     AHKY1         KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCAGT#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***  End   Chg  ***  12/09/04  **************************************
     C*
     C     AAKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    SCAGT#
     C*
     C     AGKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C     APKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C***  Begin Chg  ***  12/09/04  **************************************
     C**********           KFLD           SCPROD
     C                   KFLD                    XXPROD
     C***  End   Chg  ***  12/09/04  **************************************
     C*
     C     ARKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHRT#
     C                   KFLD                    X7TRDT
     C*
     C     ARKY1         KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHRT#
     C*
     C     TBKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    XXKEY
     C*
     C     ABKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    ASSTDT
     C                   KFLD                    ASAGT#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @@RRN         ASRRN
     C     *LIKE         DEFINE    SCEMP#        SVEMP#
     C     *LIKE         DEFINE    SCDIV         SVDIV
     C     *LIKE         DEFINE    SCFYR         SVFYR
     C     *LIKE         DEFINE    SCFND         SVFND
     C     *LIKE         DEFINE    SCPROD        SVPROD
     C     *LIKE         DEFINE    SCRATE        SVRATE
     C     *LIKE         DEFINE    SCAMT2        SVAMT2
     C***  Begin Add  ***  12/09/04  **************************************
     C     *LIKE         DEFINE    SCEMP         SVEMP
     C     *LIKE         DEFINE    SCDIV2        SVDIV2
     C     *LIKE         DEFINE    SCYR          SVYR
     C     *LIKE         DEFINE    SCGRP         SVGRP
     C     *LIKE         DEFINE    SCPRD2        SVPRD2
     C***  End   Add  ***  12/09/04  **************************************
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    ASBILL        XXBILL
     C     *LIKE         DEFINE    ASPAID        XXPAID
     C     *LIKE         DEFINE    AREFDT        X7TRDT
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C***  Begin Add  ***  12/09/04  **************************************
     C     *LIKE         DEFINE    AHFYR         XXFYR
     C     *LIKE         DEFINE    APPROD        XXPROD
     C***  End   Add  ***  12/09/04  **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* ADDWDW - Processing for the -ADD- window WDW01
     C*================================================================
     C*
     C     ADDWDW        BEGSR
     C*
B02  C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C*
B03  C     *INKC         IFEQ      *OFF
B03  C     *INKL         ANDEQ     *OFF
     C*
     C  N90SCTYPE        IFNE      'A'
     C     SCTYPE        ANDNE     'C'
     C                   SETON                                        71  90    WCR1272(INVALID TRAN
     C                   ENDIF                                                  SCTYPE<>A/C
     C*
     C                   ELSE
     C*
     C                   RETURN
     C*
E03  C                   ENDIF                                                  *INKC=*OFF=*INKL
     C*
E02  C                   ENDDO                                                  *IN90=*OFF
     C*
     C                   SELECT
     C     SCTYPE        WHENEQ    'A'
     C                   MOVE      *ON           *IN41
     C     SCTYPE        WHENEQ    'C'
     C                   MOVE      *OFF          *IN41
     C                   ENDSL
     C*
     C                   ENDSR                                                  ADDWDW
     C*
     C*================================================================
     C* AGENT  - Get Agent Assignment records
     C*================================================================
     C*
     C     AGENT         BEGSR
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     AASTAT        ANDEQ     'I'
     C     AAKY          READE     PAA01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  AGENT
     C*
     C*================================================================
     C* CALCS  - Calculate Total Commission Amount & Net Amount Due
     C*================================================================
     C*
     C     CALCS         BEGSR
     C*
     C     SCRATE        DIV       100           XXRATE            5 4
     C     SCAMT2        MULT      XXRATE        SCCOMM
     C     SCAMT2        SUB       SCCOMM        SCNET
     C*
     C                   Z-ADD     SCAMT2        SVAMT2
     C*
     C                   MOVE      *ON           *IN23
     C*
     C                   ENDSR                                                  CALCS
     C*
     C*===============================================================
     C* CHGSTS - Subroutine for changing status'
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     @@MODE        WHENEQ    'A'
     C*
     C     ABSTAT        IFEQ      'I'
     C                   MOVE      'A'           ABSTAT
     C                   ELSE
     C                   MOVE      'I'           ABSTAT
     C                   ENDIF                                                  ABSTAT=I
     C*_______________________________________________________________
     C*
     C     @@MODE        WHENEQ    'O'
     C*
     C     ABOPEN        IFEQ      'Y'
     C                   MOVE      ' '           ABOPEN
     C                   ELSE
     C                   MOVE      'Y'           ABOPEN
     C                   ENDIF                                                  AHSTAT=Y
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   MOVEL     Q1USER        ABUSER
     C                   MOVEL     Q1PGM         ABPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABDT
     C*
     C                   MOVEL     DSTIME        ABTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* CHKCHG - Check for screen changes to default information
     C*================================================================
     C*
     C     CHKCHG        BEGSR
     C*_______________________________________________________________
     C*
     C     SCEMP#        IFNE      SVEMP#
     C     SCDIV         ORNE      SVDIV
     C     SCFYR         ORNE      SVFYR
     C     SCFND         ORNE      SVFND
     C***  Begin Add  ***  12/09/04  **************************************
     C     SCEMP         ORNE      SVEMP
     C     SCDIV2        ORNE      SVDIV2
     C     SCYR          ORNE      SVYR
     C     SCGRP         ORNE      SVGRP
     C***  End   Add  ***  12/09/04  **************************************
     C*
     C                   EXSR      GETAH
     C*
     C                   Z-ADD     SCEMP#        SVEMP#
     C                   Z-ADD     SCDIV         SVDIV
     C                   Z-ADD     SCFYR         SVFYR
     C                   Z-ADD     SCFND         SVFND
     C                   Z-ADD     SCPROD        SVPROD
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     SCEMP         SVEMP
     C                   Z-ADD     SCDIV2        SVDIV2
     C                   Z-ADD     SCYR          SVYR
     C                   Z-ADD     SCGRP         SVGRP
     C                   Z-ADD     SCPRD2        SVPRD2
     C***  End   Add  ***  12/09/04  **************************************
     C*
     C                   MOVE      *ON           *IN31                          PC SCCOMM
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCEMP#<>SVEMP#
     C*_______________________________________________________________
     C*
     C     SCRATE        IFNE      SVRATE
     C*
     C     SCAMT2        IFNE      *ZEROS
     C                   EXSR      CALCS
     C                   ENDIF                                                  SCAMT2<>*ZEROS
     C*
     C                   Z-ADD     SCRATE        SVRATE
     C*
     C                   MOVE      *ON           *IN32                          PC SCPOD2
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCRATE<>SVRATE
     C*_______________________________________________________________
     C*
     C     SCAMT2        IFNE      SVAMT2
     C*
     C                   EXSR      CALCS
     C*
     C                   MOVE      *ON           *IN33                          PC SCCOMM
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  SCAMT2<>SVAMT2
     C*_______________________________________________________________
     C*
     C                   ENDSR                                                  CHKCHG
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   CLEAR                   RSJRST
     C                   CLEAR                   EMCHAP
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     SCTYPE        WHENEQ    'A'
     C*
     C  N90              EXSR      CHKCHG
     C*
     C*  GROUP MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCFND         XXFND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ELSE
     C                   MOVEL(P)  FNNAME        SCFNAM
      ***** BEG ADD ***** 11/05/04 ***********************************
     C     FNTEST        IFEQ      'Y'
     C***  Begin Chg  ***  12/08/04  **************************************
     C**********           SETON                     71  90
     C                   SETON                                        89  90
     C***  End   Chg  ***  12/08/04  **************************************
     C                   ENDIF                                                  FNTEST = 'Y'
      ***** END ADD ***** 11/05/04 ***********************************
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  POLICY MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ELSE
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   CLEAR                   RSJRST
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  PRODUCER MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   EXSR      AGENT
     C*
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     SCPROD        XXPROD
     C***  End   Add  ***  12/09/04  **************************************
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C*
     C                   SETON                                        74  90
     C*
     C                   ELSE
     C*
     C                   EXSR      GETDES
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  POSTING DATE MUST BE VALID
     C*
     C                   Z-ADD     SCPOD2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPOD2
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        75  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  TRANSACTION CODE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCARC2        XXKEY
     C*
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     TTSTAT        OREQ      'I'
     C                   SETON                                        76  90
     C                   ELSE
     C                   MOVEL(P)  TTTYPE        SCART2
     C                   MOVEL(P)  TTDESC        SCDES2
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  TRANSACTION TYPE MUST BE B, A, D, R, H, F, N OR V
     C*
     C  N90TTTYPE        IFNE      'A'
     C     TTTYPE        ANDNE     'B'
     C     TTTYPE        ANDNE     'D'
     C     TTTYPE        ANDNE     'F'
     C     TTTYPE        ANDNE     'H'
     C     TTTYPE        ANDNE     'N'
     C***  BEGIN DEL  ***  12/15/99  *********************************
     C********** TTTYPE    ANDNE'R'
     C********** TTTYPE    ANDNE'V'
     C***  END   DEL  ***  12/15/99  *********************************
     C                   SETON                                        77  90
     C                   ENDIF                                                  TTTYPE<>A/B/D/F/H/R/
     C*
     C*  GROSS AMOUNT MUST BE ENTERED
     C*
     C  N90SCAMT2        IFEQ      *ZEROS
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCAMT2=*ZEROS
     C*
     C*  OPEN FLAG MUST BE Y OR BLANK
     C*
     C  N90SCOPEN        IFNE      'Y'
     C     SCOPEN        ANDNE     *BLANK
     C                   SETON                                        79  90
     C                   ENDIF                                                  SCOPEN<>Y/*BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCPOD2        SCPODT
     C                   MOVEL     SCARC2        SCARCD
     C                   MOVEL     SCART2        SCARTY
     C                   MOVEL     SCDES2        SCDESC
     C                   Z-ADD     SCAMT2        SCAMT
     C                   ENDIF                                                  *IN90=*OFF
     C*_______________________________________________________________
     C*
     C     SCTYPE        WHENEQ    'C'
     C*
     C***  Begin Add  ***  12/08/04  **************************************
     C  N90              EXSR      CHKCHG
     C***  End   Add  ***  12/08/04  **************************************
     C*
     C*  POSTING DATE MUST BE VALID
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  TRANSACTION CODE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCARCD        XXKEY
     C*
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     TTSTAT        OREQ      'I'
     C                   SETON                                        72  90
     C                   ELSE
     C                   MOVEL(P)  TTTYPE        SCARTY
     C                   MOVEL(P)  TTDESC        SCDESC
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  TRANSACTION TYPE MUST BE P
     C*
     C  N90TTTYPE        IFNE      'P'
     C***  BEGIN ADD  ***  02/25/00  *********************************
     C     TTTYPE        ANDNE     'I'
     C     TTTYPE        ANDNE     'Q'
     C***  END   ADD  ***  02/25/00  *********************************
     C***  BEGIN ADD  ***  12/15/99  *********************************
     C     TTTYPE        ANDNE     'R'
     C     TTTYPE        ANDNE     'V'
     C***  END   ADD  ***  12/15/99  *********************************
     C                   SETON                                        73  90
     C                   ENDIF                                                  TTTYPE<>P
     C*
     C*  CHECK # MUST BE ENTERED, IF TYPE IS NOT A REFUND
     C*
     C  N90SCCHK#        IFLE      *ZEROS
     C***  BEGIN ADD  ***  12/15/99  *********************************
     C     TTTYPE        ANDNE     'R'
     C***  END   ADD  ***  12/15/99  *********************************
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCCHK#<=*ZEROS
     C*
     C*  AMOUNT MUST BE ENTERED
     C*
     C  N90SCAMT         IFEQ      *ZEROS
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCAMT=*ZEROS
     C*
     C*  GROUP MUST BE VALID, IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C***  Begin Add  ***  12/08/04  **************************************
     C     SCGRP         ANDNE     *ZERO
     C***  Begin Add  ***  10/08/14  *********************************
     C     *IN48         OREQ      *ON
     C     SCGRP         ANDEQ     *ZERO
     C***  End   Add  ***  10/08/14  *********************************
     C***  End   Add  ***  12/08/04  **************************************
      *** BEG DEL ******* 08/27/01 *********************************************
     C********** SCGRP     ANDNE*ZERO
      *** END DEL ******* 08/27/01 *********************************************
     C*
     C                   Z-ADD     SCGRP         XXFND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        76  90
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   ELSE
     C                   MOVEL(P)  FNNAME        SCGNAM
     C***  END   ADD  ***  02/08/00  *********************************
     C*
      ***** BEG ADD ***** 11/05/04 ***********************************
     C     FNTEST        IFEQ      'Y'
     C***  Begin Chg  ***  12/08/04  **************************************
     C**********           SETON                     76  90
     C                   SETON                                        89  90
     C***  End   Chg  ***  12/08/04  **************************************
     C                   ENDIF                                                  FNTEST = 'Y'
      ***** END ADD ***** 11/05/04 ***********************************
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  POLICY MUST BE VALID, IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCEMP         IFNE      *ZERO
     C     SCDIV2        ORNE      *ZERO
     C*
     C                   Z-ADD     SCEMP         XXEMP#
     C                   Z-ADD     SCDIV2        XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        77  90
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   ELSE
     C                   MOVEL(P)  EMNAME        SCENM2
     C***  END   ADD  ***  02/08/00  *********************************
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCEMP<>*ZERO<>SCDIV2
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C*
     C*  POLICY YEAR MUST BE VALID, IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C***  Begin Chg  ***  12/08/04  **************************************
     C********** SCYR      ANDNE*ZERO
     C     SCYR          IFNE      *ZERO
     C***  End   Chg  ***  12/08/04  **************************************
      *** BEG ADD ******** 08/27/01 *******************************************
     C     SCEMP         ORNE      *ZEROS
     C     SCDIV2        ORNE      *ZEROS
      *** BEG ADD ******** 08/27/01 *******************************************
     C*
     C                   Z-ADD     SCEMP         XXEMP#
     C                   Z-ADD     SCDIV2        XXDIV
     C                   Z-ADD     SCYR          SCFYR
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        78  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C***  Begin Add  ***  12/08/04  **************************************
     C                   ENDIF
     C***  End   Add  ***  12/08/04  **************************************
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C***  END   ADD  ***  02/08/00  *********************************
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCGRP         SCFND
      *** BEG CHG *** 08/27/01 ***************************************
     C*******************  MOVELSCEMP     SCEMP#
     C*******************  MOVELSCDIV2    SCDIV
     C                   Z-ADD     SCEMP         SCEMP#
     C                   Z-ADD     SCDIV2        SCDIV
      *** END CHG *** 08/27/01 ***************************************
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   Z-ADD     SCYR          SCFYR
     C***  END   ADD  ***  02/08/00  *********************************
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     SCPRD2        SCPROD
     C***  End   Add  ***  12/09/04  **************************************
     C                   ENDIF                                                  *IN90=*OFF
     C***  Begin Add  ***  12/09/04  **************************************
     C*
     C*  PRODUCER MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPRD2        ANDNE     *ZEROS
     C*
     C                   EXSR      AGENT
     C*
     C                   Z-ADD     SCPRD2        XXPROD
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C*
     C                   SETON                                        79  90
     C*
     C                   ELSE
     C*
     C                   EXSR      GETDES
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDSL
     C***  End   Add  ***  12/09/04  **************************************
     C*_______________________________________________________________
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ABDT          IFGT      SAVEDT
     C     ABDT          OREQ      SAVEDT
     C     ABTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ABDT>SAVEDT
     C***  Begin Del  ***  12/09/04  **************************************
     C**********           ENDSL
     C***  End   Del  ***  12/09/04  **************************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*================================================================
     C* GETAH  - Get Agent History information
     C*================================================================
     C*
     C     GETAH         BEGSR
     C*
     C                   EXSR      GETDES
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY1         READPE    PAH01                                  60
     C                   ENDDO                                                  *IN60=*OFF/AHSTAT=I
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     AHPROD        SCPROD
     C***  Begin Add  ***  12/08/04  **************************************
     C                   Z-ADD     AHPROD        SCPRD2
     C***  End   Add  ***  12/08/04  **************************************
     C*
     C                   EXSR      GETRAT
     C*
     C     SVPROD        IFNE      *ZERO
     C     SCPROD        ANDNE     *ZERO
     C                   MOVE      *ON           *IN21
     C                   ENDIF                                                  SVPROD<>*ZERO
     C*
     C     SVRATE        IFNE      *ZERO
     C     SCRATE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN22
     C                   ENDIF                                                  SVRATE<>*ZERO
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN34                          PC SCPROD
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   EXSR      AGENT
     C*
     C                   EXSR      GETDES
     C*
     C                   ENDSR                                                  GETAH
     C*
     C*================================================================
     C* GETDES - Get descriptions for displaying on screen
     C*================================================================
     C*
     C     GETDES        BEGSR
     C*
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C     *IN41         IFEQ      *OFF
     C                   Z-ADD     SCGRP         XXFND
     C                   Z-ADD     SCEMP         XXEMP#
     C                   Z-ADD     SCDIV2        XXDIV
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   Z-ADD     SCYR          SCFYR
     C***  END   ADD  ***  02/08/00  *********************************
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCPRD2        XXPROD
     C***  End   Add  ***  12/09/04  **************************************
     C                   ELSE
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCPROD        XXPROD
     C***  End   Add  ***  12/09/04  **************************************
     C                   ENDIF                                                  *IN41=*OFF
     C***  END   ADD  ***  12/10/99  *********************************
     C*
     C                   CLEAR                   EF
     C                   CLEAR                   ELPTDT
     C                   CLEAR                   FNNAME
     C                   CLEAR                   EMNAME
     C                   CLEAR                   APFSN
     C                   CLEAR                   APLSN
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   MOVEL(P)  FNNAME        SCGNAM
     C***  END   ADD  ***  02/08/00  *********************************
     C*
     C***  Begin Del  ***  12/08/04  **************************************
     c******BEG*ADD ***** 11/05/04 ***********************************
     C********** *IN41     IFEQ *ON
     C********** *IN60     IFEQ *ON
     C**********           MOVEL'Y'       FNTEST
     C**********           ENDIF
     C********** FNTEST    IFEQ 'Y'
     C**********           SETON                     71  90
     C**********           ENDIF                           FNTEST = 'Y'
     C**********           ENDIF                           *IN41 = *ON
     c******END*ADD ***** 11/05/04 ***********************************
     C***  End   Del  ***  12/08/04  **************************************
     C*
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCENAM
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   MOVEL(P)  EMNAME        SCENM2
     C***  END   ADD  ***  02/08/00  *********************************
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C     APFSN         IFNE      *BLANKS
     C     APLSN         CAT(P)    ',':0         SCPNAM
     C                   CAT       APFSN:1       SCPNAM
     C                   ELSE
     C                   MOVEL(P)  APLSN         SCPNAM
     C                   ENDIF                                                  APFNS<>*BLANKS
     C*
     C                   ENDSR                                                  GETDES
     C*
     C*================================================================
     C* GETRAT - Get new or renew rate
     C*================================================================
     C*
     C     GETRAT        BEGSR
     C*
     C*  USE THE EMPLOYER'S MOST CURRENT EFFECTIVE DATE PLUS ONE YEAR
     C*  AS THE RENEWAL DATE AND LOAD THE RATE BASE ON THAT DATE.
     C*
     C                   Z-ADD     *ZERO         XXRWDT
     C*
     C     1             DO        5             P                 1 0
     C*
     C     EF(P)         IFNE      *ZERO
     C                   Z-ADD     EF(P)         XXRWDT
     C                   ENDIF                                                  EF,P<>*ZERO
     C*
     C                   ENDDO                                                  P->5
     C*
     C                   ADD       1             XXRWYR
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C***  BEGIN ADD  ***  08/10/00  *********************************
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C***  END   ADD  ***  08/10/00  *********************************
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     XXRWDT        IFGE      ELPTDT
     C                   Z-ADD     ARNWRT        SCRATE                         NEW BUSINESS
     C                   ELSE
     C                   Z-ADD     ARRNRT        SCRATE                         RENEWAL
     C                   ENDIF                                                  XXRWDT>=ELPTDT
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  GETRAT
     C*
     C*================================================================
     C* KJDO   - Processing for F10=Search
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFND'
     C***  BEGIN CHG  ***  12/10/99  *********************************
     C********** HFLD      WHEQ 'SCGRP'
     C     HFLD          OREQ      'SCGRP'
     C***  END   CHG  ***  12/10/99  *********************************
     C*
     C                   MOVE      ASCO#         #@CO#
     C*
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM      *BLANKS       #@FND             3
     C*
     C     #@FND         IFNE      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVE      #@FND         SCFND
     C                   MOVE      #@FND         SCGRP
     C*
     C                   EXSR      GETDES
     C*
     C                   MOVE      *ON           *IN35
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN36
     C*
     C                   ENDIF                                                  #@FND<>*BLANKS
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCEMP'
     C     HFLD          OREQ      'SCDIV2'
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C     HFLD          OREQ      'SCYR'
     C***  END   ADD  ***  02/08/00  *********************************
     C*
     C*  GROUP MUST BE VALID
     C*
     C     *IN41         IFEQ      *OFF
     C                   Z-ADD     SCGRP         XXFND
     C                   ELSE
     C                   Z-ADD     SCFND         XXFND
     C                   ENDIF                                                  *IN41=*OFF
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C     *IN41         IFEQ      *OFF
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C**********           SETON                     77  90
     C                   SETON                                        76  90
     C***  END   ADD  ***  12/10/99  *********************************
     C                   ELSE
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN41=*OFF
     C*
     C                   ELSE
     C*
     C***  BEGIN CHG  ***  02/08/00  *********************************
     C********** *IN41     IFEQ *OFF
     C**********           MOVE '2'       ##CALL
     C**********           ELSE
     C**********           MOVE 'Y'       ##CALL
     C**********           ENDIF                           *IN41=*OFF
     C                   MOVE      'Y'           ##CALL
     C***  END   CHG  ***  02/08/00  *********************************
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      FNRRN         ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C*
     C     ##RRN         IFNE      *ZEROS
     C     *INKC         ANDEQ     *OFF
     C*
     C***  BEGIN CHG  ***  02/08/00  *********************************
     C********** *IN41     IFEQ *OFF
     C**********
     C********** ##RRN     CHAINWMEMP                60
     C**********
     C**********           Z-ADDEMEMP#    SCEMP
     C**********           Z-ADDEMDIV     SCDIV2
     C***  Begin Add  ***  12/08/04  **************************************
     C     ##RRN         CHAIN     WMEMP                              60
     C*
     C                   Z-ADD     EMEMP#        ELEMP#
     C                   Z-ADD     EMDIV         ELDIV
     C                   CLEAR                   ELFYR
     C*
     C     ##RRN2        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN37
     C                   ENDIF
     C***  End   Add  ***  12/08/04  **************************************
     C*
     C     ##RRN2        CHAIN     WDELP                              60
     C*
     C     *IN41         IFEQ      *OFF
     C*
     C                   Z-ADD     ELEMP#        SCEMP
     C                   Z-ADD     ELDIV         SCDIV2
     C                   Z-ADD     ELFYR         SCYR
     C***  END   CHG  ***  02/08/00  *********************************
     C*
     C***  Begin Add  ***  12/08/04  **************************************
     C                   EXSR      CHKCHG
     C***  End   Add  ***  12/08/04  **************************************
     C*
     C                   ELSE
     C***  BEGIN DEL  ***  02/08/00  *********************************
     C**********
     C********** ##RRN2    CHAINWDELP                60
     C***  END   DEL  ***  02/08/00  *********************************
     C*
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C                   EXSR      CHKCHG
     C*
     C                   ENDIF                                                  *IN41=*OFF
     C*
     C                   ELSE
     C*
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   EXSR      GETDES
     C***  END   ADD  ***  02/08/00  *********************************
     C*
     C                   MOVE      *ON           *IN35
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCPROD'
     C***  Begin Add  ***  12/09/04  **************************************
     C     HFLD          OREQ      'SCPRD2'
     C***  End   Add  ***  12/09/04  **************************************
     C*
     C                   EXSR      AGENT
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C                   CALL      'W0042R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      AGRRN         ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C     ##RRN         CHAIN     WMAPP                              60
     C*
     C                   Z-ADD     APPROD        SCPROD
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     APPROD        SCPRD2
     C***  End   Add  ***  12/09/04  **************************************
     C*
     C                   EXSR      GETDES
     C*
     C                   MOVE      *ON           *IN31
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN34
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCARCD'
     C     HFLD          OREQ      'SCARC2'
     C*
     C                   CALL      'W2161R'                             99
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C*
     C                   MOVEL     ##KEY         SCARCD
     C                   MOVEL     ##KEY         SCARC2
     C                   MOVEL(P)  ##KEY         XXKEY
     C*
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  TTTYPE        SCARTY
     C                   MOVEL(P)  TTDESC        SCDESC
     C                   MOVEL(P)  TTTYPE        SCART2
     C                   MOVEL(P)  TTDESC        SCDES2
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *IN82
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   Z-ADD     ABTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   SELECT
     C***  BEGIN CHG  ***  12/15/99  *********************************
     C********** ABCHK#    WHNE *ZEROS
     C     ABARTY        WHENEQ    'P'
     C***  BEGIN ADD  ***  02/25/00  *********************************
     C     ABARTY        OREQ      'I'
     C     ABARTY        OREQ      'Q'
     C***  END   ADD  ***  02/25/00  *********************************
     C     ABARTY        OREQ      'R'
     C     ABARTY        OREQ      'V'
     C***  END   CHG  ***  12/15/99  *********************************
     C                   MOVE      'C'           SCTYPE
     C                   MOVE      *OFF          *IN41
     C                   OTHER
     C                   MOVE      'A'           SCTYPE
     C                   MOVE      *ON           *IN41
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     DSDATE        SCTRDT
     C*
     C                   ENDIF                                                  *IN40=*OFF
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TRDT
     C*
     C                   Z-ADD     ABPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
     C                   MOVEL     SRDTO         SCPOD2
     C*
     C                   MOVEL     ABARCD        SCARCD
     C                   MOVEL     ABARCD        SCARC2
     C                   MOVEL     ABARTY        SCARTY
     C                   MOVEL     ABARTY        SCART2
     C                   MOVEL     ABDESC        SCDESC
     C                   MOVEL     ABDESC        SCDES2
     C                   Z-ADD     ABCHK#        SCCHK#
     C                   Z-ADD     ABAMT         SCAMT
     C                   Z-ADD     ABAMT         SCAMT2
     C                   Z-ADD     ABFND         SCFND
     C                   Z-ADD     ABFND         SCGRP
     C                   Z-ADD     ABEMP#        SCEMP#
     C                   Z-ADD     ABEMP#        SCEMP
     C                   Z-ADD     ABDIV         SCDIV
     C                   Z-ADD     ABDIV         SCDIV2
     C                   Z-ADD     ABFYR         SCFYR
     C***  BEGIN ADD  ***  02/08/00  *********************************
     C                   Z-ADD     ABFYR         SCYR
     C***  END   ADD  ***  02/08/00  *********************************
     C                   Z-ADD     ABPROD        SCPROD
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     ABPROD        SCPRD2
     C***  End   Add  ***  12/09/04  **************************************
     C                   Z-ADD     ABRATE        SCRATE
     C                   Z-ADD     ABCOMM        SCCOMM
     C                   Z-ADD     ABNET         SCNET
     C                   MOVE      ABOPEN        SCOPEN
     C                   MOVEL     ABCOM1        SCCOM1
     C*
     C                   EXSR      GETDES
     C*
     C                   Z-ADD     SCEMP#        SVEMP#
     C                   Z-ADD     SCDIV         SVDIV
     C                   Z-ADD     SCFYR         SVFYR
     C                   Z-ADD     SCFND         SVFND
     C                   Z-ADD     SCPROD        SVPROD
     C                   Z-ADD     SCRATE        SVRATE
     C                   Z-ADD     SCAMT2        SVAMT2
     C***  Begin Add  ***  12/09/04  **************************************
     C                   Z-ADD     SCEMP         SVEMP
     C                   Z-ADD     SCDIV2        SVDIV2
     C                   Z-ADD     SCYR          SVYR
     C                   Z-ADD     SCGRP         SVGRP
     C                   Z-ADD     SCPRD2        SVPRD2
     C***  Begin Add  ***  12/09/04  **************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ASCO#         ABCO#
     C                   Z-ADD     SCAGT#        ABAGT#
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         ABSTDT
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABDUE
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABTRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABPODT
     C*
     C                   MOVEL     SCARCD        ABARCD
     C                   MOVEL     SCARTY        ABARTY
     C                   MOVEL     SCDESC        ABDESC
     C                   Z-ADD     SCCHK#        ABCHK#
     C                   Z-ADD     SCAMT         ABAMT
     C                   Z-ADD     SCFND         ABFND
     C                   Z-ADD     SCEMP#        ABEMP#
     C                   Z-ADD     SCDIV         ABDIV
     C                   Z-ADD     SCFYR         ABFYR
     C                   Z-ADD     SCPROD        ABPROD
     C                   Z-ADD     SCRATE        ABRATE
     C                   Z-ADD     SCCOMM        ABCOMM
     C                   Z-ADD     SCNET         ABNET
     C                   MOVE      SCOPEN        ABOPEN
     C                   MOVEL     SCCOM1        ABCOM1
     C                   MOVE      'Y'           ABMADJ
     C                   MOVEL     RSJRST        ABRST
     C                   Z-ADD     EMCHAP        ABCHAP
     C*
     C***  Begin Add  ***  12/10/04  **************************************
     C                   EXSR      AGENT
     C*
     C                   Z-ADD     AAFEIN        ABFEIN
     C                   Z-ADD     AAMOD         ABMOD
     C***  End   Add  ***  12/10/04  **************************************
     C*
     C                   MOVEL     Q1USER        ABUSER
     C                   MOVEL     Q1PGM         ABPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABDT
     C*
     C                   MOVEL     DSTIME        ABTIME
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPODT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCPOD2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPOD2
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C     *IN09         IFEQ      *OFF
     C                   EXSR      UPDTAS
     C                   ENDIF                                                  *IN09=*OFF
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
      ***  Begin Add  ***  10/08/14  *********************************
      /Free
       // Get the client id
       GetClientID(outClientID);
       // Group is required for all clients. *IN48 is for Highlighting.
       *In48 = *On;
      /End-Free
      ***  End   Add  ***  10/08/14  *********************************
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C     @@MODE        IFEQ      'A'
     C                   EXSR      ADDWDW
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   Z-ADD     @@RRN1        ASRRN
     C     @@RRN1        CHAIN(N)  WSASP                              60
     C*
     C                   Z-ADD     ASAGT#        SCAGT#
     C*
     C                   CALL      AGTAPI        AGTPL                  99
     C*
     C                   Z-ADD     ASSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY                 99
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   Z-ADD     ASDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                 99
     C                   MOVEL     SRDTO         SCDUE
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C                   CALL      GLAPI         GLPL                   99
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   EXSR      PGMEND
     C                   RETURN
     C     @@MODE        WHENEQ    'O'
     C                   EXSR      CHGSTS
     C                   EXSR      PGMEND
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C*================================================================
     C* REFUND - Create a voucher for refund checks
     C*================================================================
     C*
     C     REFUND        BEGSR
     C*
     C     ##GLAP        IFEQ      'Y'
     C*
     C     TTTYPE        IFEQ      'R'
     C*
     C                   MOVE      ABCO#         #@CO#
     C                   MOVE      ABFND         ##GRP
     C                   MOVE      ABFYR         ##FYR
     C                   MOVE      ABEMP#        #@EMP#
     C                   MOVE      ABDIV         #@DIV
     C                   MOVE      ABAMT         ##AMT
     C                   MOVE      ABTRDT        ##TRDT
     C                   MOVE      SCPODT        ##PODT
     C                   MOVE      ABITIM        ##TIME
     C*
     C                   EXSR      AGENT
     C*
     C                   MOVE      AAFEIN        ##FEIN
     C                   MOVE      AAMOD         ##MOD
     C*
     C                   CALL      'AP002R'                             99
     C                   PARM                    #@CO#             3
     C                   PARM                    ##GRP             3
     C                   PARM                    ##YEAR            3
     C                   PARM                    #@EMP#            9
     C                   PARM                    #@DIV             5
     C                   PARM      ABARCD        ##ARCD            2
     C                   PARM      ABARTY        ##ARTY            1
     C                   PARM      ABDESC        ##DESC           30
     C                   PARM                    ##AMT            11
     C                   PARM                    ##TRDT            7
     C                   PARM                    ##PODT            8
     C                   PARM                    ##TIME            6
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      'A'           ##FLAG            1
     C*
     C                   ENDIF                                                  SCTYPE=C
     C*
     C                   ENDIF                                                  ##GLAP=Y
     C*
     C                   ENDSR                                                  REFUND
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C                   EXSR      REFUND
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* UPDTAS - Update WSASP record
     C*================================================================
     C*
     C     UPDTAS        BEGSR
     C*
     C                   CLEAR                   XXBILL
     C                   CLEAR                   XXPAID
     C*
     C     ABKY          CHAIN     PAB01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   SELECT
     C     ABARTY        WHENEQ    'A'
     C     ABARTY        OREQ      'B'
     C     ABARTY        OREQ      'D'
     C     ABARTY        OREQ      'F'
     C***  BEGIN CHG  ***  05/18/00  *********************************
     C********** ABARTY    OREQ 'G'
     C     ABARTY        OREQ      'H'
     C***  END   CHG  ***  05/18/00  *********************************
     C***  BEGIN DEL  ***  12/15/99  *********************************
     C********** ABARTY    OREQ 'R'
     C********** ABARTY    OREQ 'V'
     C***  END   DEL  ***  12/15/99  *********************************
     C     ABARTY        OREQ      'N'
     C                   ADD       ABNET         XXBILL
     C     ABARTY        WHENEQ    'P'
     C***  BEGIN ADD  ***  02/25/00  *********************************
     C     ABARTY        OREQ      'I'
     C     ABARTY        OREQ      'Q'
     C***  END   ADD  ***  02/25/00  *********************************
     C***  Begin Add  ***  12/10/04  **************************************
     C     ABARTY        OREQ      'T'
     C***  End   Add  ***  12/10/04  **************************************
     C                   ADD       ABAMT         XXPAID
     C***  BEGIN ADD  ***  12/15/99  *********************************
     C     ABARTY        WHENEQ    'R'
     C     ABARTY        OREQ      'V'
     C                   SUB       ABAMT         XXPAID
     C***  END   ADD  ***  12/15/99  *********************************
     C                   ENDSL
     C*
     C     ABKY          READE     PAB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     ASRRN         CHAIN     WSASP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     XXBILL        ASBILL
     C                   Z-ADD     XXPAID        ASPAID
     C     ASBILL        SUB       ASPAID        ASBAL
     C*
     C                   MOVEL     Q1PGM         ASPGM
     C                   MOVEL     Q1USER        ASUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ASDT
     C*
     C                   Z-ADD     DSTIME        ASTIME
     C*
     C                   UPDATE    PAS01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDTAS
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ABIUSR
     C                   MOVEL     Q1PGM         ABIPGM
     C                   Z-ADD     ABDT          ABIDT
     C                   Z-ADD     ABTIME        ABITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDABP
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C                   EXSR      REFUND
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     C                   ENDSR                                                  WRITER
