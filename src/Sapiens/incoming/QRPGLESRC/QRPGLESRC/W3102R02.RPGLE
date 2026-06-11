     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3102R02 - AGENCY BILLING STATEMENTS INFORMATION
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM ALLOW FOR ADD OR DISPLAY OF THE AGENCY BILLING   *
     H* STATEMENT INFORMATION.                                        *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W3102R                                                      *
     H* CALLS TO:                                                     *
     H*   W3103R                                                      *
     H*                                                               *
     H*  CREATED: 04/05/99  C:33  PROJECT: 96WAD                      *
     H*  IN BASE: 07/19/99  C:33  PROJECT: WC899                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*   C:58 6/10/00  PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H*  05/18/00  C:33  PROJECT: WC899                               *
     H*                  CALCULATE ASBILL AND ASPAID CORRECTLY.       *
     H*                                                               *
     H*  12/10/99  C:33  PROJECT: WC899          Y2K#: 29             *
     H*                 -REQUIRE ENTRY OF AGENT.                      *
     H*                 -DEFAULT DUE DATE FOR AGENT.                  *
     H*****************************************************************
     FW3102F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWSASP     UF A E             DISK
     F                                     RENAME(PAS01:DBRCDF)
     F                                     INFDS(ASFDS)
     FWSASL     IF   E           K DISK
     FWMAAL     IF   E           K DISK
     FWMAAL5    IF   E           K DISK
     F                                     RENAME(PAA01:PAA05)
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     F***  BEGIN ADD  ***  12/10/99  *********************************
     FWMAGL     IF   E           K DISK
     F***  END   ADD  ***  12/10/99  *********************************
     FWMAGP     IF   E             DISK
     F                                     RENAME(PAG01:PAG0P)
     FWDABL     UF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D ASFDS           DS
     D  ASRRN                397    400B 0
     D***  BEGIN ADD  ***  12/10/99  *********************************
     D                 DS
     D  X7STDT                 1      7  0
     D  X7STMM                 4      5  0
     D  X7STDD                 6      7  0
     D***  END   ADD  ***  12/10/99  *********************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agency Billing State-
     D                                     ments')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

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
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ASKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    X5STDT
     C                   KFLD                    SCAGT#
     C*
     C     AAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    AGFEIN
     C                   KFLD                    AGMOD
     C*
     C     AAKY5         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCAGT#
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFND
     C**** END ADD ***** 06/10/00 ************************************
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C     AGKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C***  END   ADD  ***  12/10/99  *********************************
     C*
     C     ABKY          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    ASAGT#
     C                   KFLD                    ASSTDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ASSTDT        X5STDT
     C     *LIKE         DEFINE    ASPAID        XXPAID
     C     *LIKE         DEFINE    ASBILL        XXBILL
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C     *LIKE         DEFINE    SCAGT#        SVAGT#
     C***  END   ADD  ***  12/10/99  *********************************
     C**** BEGIN ADD *** 06/10/00 ************************************
     C     *LIKE         DEFINE    AACO#         SVCO#
     C     *LIKE         DEFINE    AAFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C*================================================================
     C* CALCDU - Calculate due date for the agent
     C*================================================================
     C*
     C     CALCDU        BEGSR
     C*
     C     SCDUE         IFEQ      *ZEROS
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         X7STDT
     C*
     C                   SELECT
     C     X7STMM        WHENEQ    2
     C                   Z-ADD     28            X7STDD
     C     X7STMM        WHENEQ    4
     C     X7STMM        OREQ      6
     C     X7STMM        OREQ      9
     C     X7STMM        OREQ      11
     C                   Z-ADD     30            X7STDD
     C                   OTHER
     C                   Z-ADD     31            X7STDD
     C                   ENDSL
     C*
     C                   Z-ADD     X7STDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUE
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     AGACDU        XXDAYS
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SCDUE
     C                   PARM                    XXDAYS            5 0
     C                   PARM                    ERR               1
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SCDUE=*ZEROS
     C*
     C                   Z-ADD     SCAGT#        SVAGT#
     C*
     C                   ENDSR                                                  CALCDU
     C***  END   ADD  ***  12/10/99  *********************************
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C*
     C*  DELETE 'ALL' ASSOCIATED DETAIL RECORDS
     C*
     C     *IN60         DOUEQ     *ON
     C     ABKY          DELETE    PAB01                              60
     C                   ENDDO                                                  *IN60=*ON
     C*
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
     C* GETDTL - Get detail records (WDABP) for updating WSASP
     C*================================================================
     C*
     C     GETDTL        BEGSR
     C*
     C                   CLEAR                   XXBILL
     C                   CLEAR                   XXPAID
     C*
     C     ABKY          CHAIN(N)  PAB01                              60
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
     C***  BEGIN DEL  ***  05/18/00  *********************************
     C********** ABARTY    OREQ 'R'
     C********** ABARTY    OREQ 'V'
     C***  END   DEL  ***  05/18/00  *********************************
     C     ABARTY        OREQ      'N'
     C                   ADD       ABAMT         XXBILL
     C     ABARTY        WHENEQ    'P'
     C***  BEGIN ADD  ***  05/18/00  *********************************
     C     ABARTY        OREQ      'I'
     C     ABARTY        OREQ      'Q'
     C***  END   ADD  ***  05/18/00  *********************************
     C                   ADD       ABAMT         XXPAID
     C***  BEGIN ADD  ***  05/18/00  *********************************
     C     ABARTY        WHENEQ    'R'
     C     ABARTY        OREQ      'V'
     C                   SUB       ABAMT         XXPAID
     C***  END   ADD  ***  05/18/00  *********************************
     C                   ENDSL
     C*
     C     ABKY          READE(N)  PAB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   Z-ADD     XXBILL        ASBILL
     C                   Z-ADD     XXPAID        ASPAID
     C     ASBILL        SUB       ASPAID        ASBAL
     C*
     C                   ENDSR                                                  GETDTL
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  VALIDATE STATEMENT DATE
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         X5STDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  VALIDATE AGENT #
     C*
     C***  BEGIN CHG  ***  12/10/99  *********************************
     C**N90***** SCAGT#    IFNE *ZEROS
     C     *IN90         IFEQ      *OFF
     C***  END   CHG  ***  12/10/99  *********************************
     C*
     C     AAKY5         CHAIN     PAA05                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*   Get group master.
     C     AACO#         IFNE      SVCO#
     C     AAFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   Z-ADD     AACO#         SVCO#
     C                   Z-ADD     AAFND         SVFND
     C                   ENDIF                                                  .CO/FND <>
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C     AASTAT        IFNE      'I'
     C**** BEG CHANGE ** 06/10/00 ************************************
     C     FNTEST        ANDNE     'Y'
     C********** AAFND     ANDNE999
     C**** END CHANGE ** 06/10/00 ************************************
     C                   LEAVE
     C                   ENDIF                                                  AASTAT<>I/AAFND<>999
     C*
     C     AAKY5         READE     PAA05                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C   60              SETON                                        72  90
     C*
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C  N90SCAGT#        IFNE      SVAGT#
     C                   EXSR      CALCDU
     C                   SETON                                        82  90
     C                   ENDIF                                                  SCAGT#<>SVAGT#
     C***  END   ADD  ***  12/10/99  *********************************
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  RECORD CANNOT BE DUPLICATED
     C*
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C     ASKY          CHAIN     PAS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN40=*ON/*IN90=*OFF
     C*
     C*  VALIDATE DUE DATE
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUE
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ASDT          IFGT      SAVEDT
     C     ASDT          OREQ      SAVEDT
     C     ASTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ASDT>SAVEDT
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
     C*  KJDO   - Process F10; search
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCSTDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCDUE
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C*
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PAG0P                              60
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*   Get group master.
     C     AACO#         IFNE      SVCO#
     C     AAFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   Z-ADD     AACO#         SVCO#
     C                   Z-ADD     AAFND         SVFND
     C                   ENDIF                                                  .CO/FND <>
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C     AASTAT        IFNE      'I'
     C**** BEG CHANGE ** 06/10/00 ************************************
     C     FNTEST        ANDNE     'Y'
     C********** AAFND     ANDNE999
     C**** END CHANGE ** 06/10/00 ************************************
     C                   LEAVE
     C                   ENDIF                                                  AASTAT<>I/AAFND<>999
     C*
     C     AAKY          READE     PAA01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     AAAGT#        SCAGT#
     C***  BEGIN CHG  ***  12/10/99  *********************************
     C**********           MOVE *ON       *IN92
     C                   EXSR      CALCDU
     C                   MOVE      *ON           *IN82
     C***  END   CHG  ***  12/10/99  *********************************
     C                   ELSE
     C***  BEGIN CHG  ***  12/10/99  *********************************
     C**********           MOVE *ON       *IN91
     C                   MOVE      *ON           *IN81
     C***  END   CHG  ***  12/10/99  *********************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C***  BEGIN CHG  ***  12/10/99  *********************************
     C**********           MOVE *ON       *IN91
     C                   MOVE      *ON           *IN81
     C***  END   CHG  ***  12/10/99  *********************************
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
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
     C                   Z-ADD     ASSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   Z-ADD     ASAGT#        SCAGT#
     C***  BEGIN ADD  ***  12/10/99  *********************************
     C                   Z-ADD     ASAGT#        SVAGT#
     C***  END   ADD  ***  12/10/99  *********************************
     C*
     C                   Z-ADD     ASDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUE
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         ASCO#
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         ASSTDT
     C*
     C                   Z-ADD     SCAGT#        ASAGT#
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ASDUE
     C*
     C                   EXSR      GETDTL
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
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C     *IN40         IFEQ      *ON
     C*
     C  N09              MOVE      ' '           ##MODE
     C   09              MOVE      '1'           ##MODE
     C*
     C                   CALL      'W3103R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      ASRRN         ##RRN             9 0
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C                   ENDIF                                                  *INKC=*OFF=*INKL
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
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
     C                   ENDSR                                                  UPDATR
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
     C                   MOVEL     Q1PGM         ASIPGM
     C                   MOVEL     Q1USER        ASIUSR
     C                   Z-ADD     ASDT          ASIDT
     C                   Z-ADD     DSTIME        ASITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WSASP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
