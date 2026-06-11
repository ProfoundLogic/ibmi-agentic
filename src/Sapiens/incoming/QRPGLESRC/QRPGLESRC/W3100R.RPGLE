     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3100R-GENERATE AGENT ACCOUNT CURRENT BILLING TRANSACTIONS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM PROMPTS THE USER FOR THE FIELDS REQUIRED TO      *
     H* GENERATE AGENT ACCOUNT CURRENT BILLING TRANSACTIONS.          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*   W3100C                                                      *
     H*                                                               *
     H*  CREATED: 04/05/99  C:33  PROJECT: 96WAD                      *
     H*  IN BASE: 07/19/99  C:33  PROJECT: WC899                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  C:295 11/12/14 Redmine : 4760                                *
     H*                 To allow user to run Agency Billing process   *
     H*                 for previous month.                           *
     H*                                                               *
     H*  03/30/11  C:257 SUPPORT:76212                                 *
     H*                  Edit added to the Agency Billing process     *
     H*                  that will prevent users from accidentally    *
     H*                  over-writing past data.                      *
     H*                                                               *
     H*   C:58 6/10/00  PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H*  03/13/00  C:33  PROJECT: WC899   WA096/22081                 *
     H*                  DATE RANGE CAN NOT BE GREATER THAN THE SYSTEM*
     H*                  DATE.                                        *
     H*****************************************************************
     FW3100F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMAAP     UF A E             DISK
     F                                     RENAME(PAA01:DBRCDF)
     FWMAAL     IF   E           K DISK
     FWMAAL5    IF   E           K DISK
     F                                     RENAME(PAA01:PAA05)
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     FWMAGP     IF   E             DISK
     F                                     RENAME(PAG01:PAG0P)
     F**** BEGIN ADD *** 03/30/11 ************************************
     FWDABL     IF   E           K DISK
     F*
      /COPY SCOPYMT01D
     D*  DS FOR CURRENT YEAR AND MONTH
     D                 DS
     D  XXDATE                 1      8  0
     D  M@CURR                 1      2  0
     D  Y@CURR                 5      8  0
     D*  DS FOR FILE YEAR AND MONTH
     D                 DS
     D  XXIDT                  1      8  0
     D  M@FILE                 1      2  0
     D  Y@FILE                 5      8  0
     D*
     D**** END ADD ***** 03/30/11 ************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Generate Agent Accou-
     D                                     nt Current Billing -
     D                                     Transactions')
     D OPTNM1          C                   CONST('Generate Account -
     D                                     Current Transactions')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D #OFSCR          C                   CONST(1)

     D**** BEGIN ADD *** 11/12/14 ************************************
     D  P@CURR         S                   like(M@CURR)
     D**** END   ADD *** 11/12/14 ************************************
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @#MODE            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
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
     C**** BEGIN ADD *** 03/30/11 ************************************
     C     ABKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSTDT
     C*
     C     ABKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSTDT
     C                   KFLD                    SCAGT#
     C**** END   ADD *** 03/30/11 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @#MODE        @@MODE
     C     *LIKE         DEFINE    AADT          X7FRDT
     C     *LIKE         DEFINE    AADT          X7TODT
     C**** BEGIN ADD *** 06/10/00 ************************************
     C     *LIKE         DEFINE    AACO#         SVCO#
     C     *LIKE         DEFINE    AAFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
     C**** BEGIN ADD *** 03/30/11 ************************************
     C     *LIKE         DEFINE    ABSTDT        XXSTDT
     C**** END ADD ***** 03/30/11 ************************************
     C*
     C/COPY SCOPYMT01C
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
     C*  VALIDATE FROM DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  VALIDATE TO DATE
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE IN ASCENDING ORDER
     C*
     C  N90X7FRDT        IFGT      X7TODT
     C                   SETON                                        73  90
     C                   ENDIF                                                  X7FRDT>X7TODT
     C***  BEGIN ADD  ***  03/13/00  *********************************
     C*
     C*  DATES RANGE MUST BE LESS THAN THE CURRENT DATE
     C*
     C  N90X7TODT        IFGE      SAVEDT
     C                   SETON                                        76  90
     C                   ENDIF                                                  X7FRDT>=SAVEDT
     C***  END   ADD  ***  03/13/00  *********************************
     C*
     C*  VALIDATE STATEMENT DATE
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  VALIDATE AGENT #, IF ENTERED
     C*
     C  N90SCAGT#        IFNE      *ZEROS
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
     C*
     C********** AAFND     ANDNE999
     C**** END CHANGE ** 06/10/00 ************************************
     C                   LEAVE
     C                   ENDIF                                                  AASTAT<>I/AAFND<>999
     C*
     C     AAKY5         READE     PAA05                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C   60              SETON                                        75  90
     C*
     C                   ENDIF                                                  SCAGT#<>*ZEROS
     C**** BEGIN ADD *** 03/30/11 ************************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXSTDT
     C*
     C                   SELECT
     C     SCAGT#        WHENNE    *ZERO
     C     ABKY1         CHAIN     PAB01                              60
     C                   OTHER
     C     ABKY          CHAIN     PAB01                              60
     C                   ENDSL
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DSDATE        XXDATE
     C*
     C                   Z-ADD     ABIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT
     C*
     C* WHEN YEAR ENTERED IS LESS THAN THE CURRENT YEAR OR
     C* WHEN YEAR ENTERED IS SAME BUT MONTH IS LESS THAN CURRENT MONTH,
     C* DISPLAY EDIT.
     C*
     C**** BEGIN ADD *** 11/12/14 ************************************
     C     M@CURR        SUB       1             P@CURR
     C**** END   ADD *** 11/12/14 ************************************
     C                   SELECT
     C     Y@CURR        WHENGT    Y@FILE
     C                   SETON                                        77  90
     C                   MOVE      '1'           @FLAG             1
     C     Y@CURR        WHENEQ    Y@FILE
     C**** BEGIN ADD *** 11/12/14 ************************************
     C**** M@CURR        ANDGT     M@FILE
     C     P@CURR        ANDGT     M@FILE
     C**** END   ADD *** 11/12/14 ************************************
     C                   SETON                                        77  90
     C                   MOVE      '1'           @FLAG             1
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C**** END   ADD *** 03/30/11 ************************************
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
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCSTDT
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
     C                   ELSE
     C                   MOVE      *ON           *IN88
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN88
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C*  KMDO   - Process F13; user parameters
     C*================================================================
     C*
     C     KMDO          BEGSR
     C*
     C                   CALL      USPAPI                               99
     C*
     C                   EXSR      GETCOF
     C*
     C                   EXSR      PGMSTR
     C*
     C                   ENDSR                                                  KMDO
     C*
     C**** BEGIN ADD *** 03/30/11 ************************************
     C*================================================================
     C*  KADO   - Process F01; HELP KEY
     C*================================================================
     C*
     C     KADO          BEGSR
     C*
     C     @FLAG         IFEQ      *ON
     C                   SETON                                        78  90
     C                   MOVE      '0'           @FLAG
     C                   ENDIF
     C                   ENDSR                                                  KADO
     C**** END   ADD *** 03/30/11 ************************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCFRDT
     C                   CLEAR                   SCTODT
     C                   CLEAR                   SCSTDT
     C                   CLEAR                   SCAGT#
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      SUBMIT
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO                     90
     F**** BEGIN ADD *** 03/30/11 ************************************
     C     *INKA         CASEQ     *ON           KADO                     90
     F**** END   ADD *** 03/30/11 ************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      52            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRD2
     C*
     C                   MOVEL(P)  OPTNM1        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   CLEAR                   @@MODE
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  @#MODE        @@MODE
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   EXSR      MVDBSC
     C*
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* SUBMIT - Load parms and call submitter program
     C*================================================================
     C*
     C     SUBMIT        BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCFRDT        ##FRDT
     C                   MOVE      SCTODT        ##TODT
     C                   MOVE      SCSTDT        ##STDT
     C                   MOVE      SCAGT#        ##AGT#
     C*
     C                   CALL      'W3100C'                             99
     C                   PARM                    #@CO#             3
     C                   PARM                    ##FRDT            8
     C                   PARM                    ##TODT            8
     C                   PARM                    ##STDT            6
     C                   PARM                    ##AGT#            5
     C                   PARM                    ##OUTQ           10
     C*
     C                   ENDSR                                                  SUBMIT
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
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMAAP
     C*
     C                   ENDSR                                                  WRITER
