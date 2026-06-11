     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R13 - EMPLOYER ADDRESS CHANGE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO UPDATE THE   *
     H*                EMPLOYER ADDRESS FROM MID TERM ENDORSEMENTS.   *
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/09/09 C:33  Project: WC1118   SY184 Priss#4                *
     H*                Update the county code as appropriate.         *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 05/29/02 C:09  SUPPORT: WA072/32716                           *
     H*                The "Change both Addresses in employer master" *
     H*                flag in the endorsement file (ENCBAF/WMENP)    *
     H*                was not being loaded properly in all instances *
     H*
     H* 04/27/00  C:53   SUPPORT: WC065/19768
     H*                  SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H* 07/07/98  C:57  SEQ#: WA033/12164                             *
     H*                -LOAD EMPLOYER FEDERAL ID#.                    *
     H*                -SUPPRESS CTY/ST/ZIP LINE IN BLANK.            *
     H*                                                               *
     H*****************************************************************
     H*
     FW0051F13  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMEMP     UF   E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     FWDELP     IF   E             DISK
     FWTMYP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMDXL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWTABL     IF   E           K DISK
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PCN02)
     FWMENL3    IF A E           K DISK
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
     F***  Begin Add  ***  09/09/09  **************************************
     FWMCZL     IF   E           K DISK
     F***  End   Add  ***  09/09/09  **************************************
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*
     D                 DS                  INZ
     D  SCPZIP                 1      9  0
     D  SCPZP1                 1      5  0
     D  SCPZP2                 6      9  0
     D*
     D                 DS                  INZ
     D  SVPZIP                 1      9  0
     D  SVPZP1                 1      5  0
     D  SVPZP2                 6      9  0
     D*
     D                 DS                  INZ
     D  SVMZIP                 1      9  0
     D  SVMZP1                 1      5  0
     D  SVMZP2                 6      9  0
     D*
     D                 DS                  INZ
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D*
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Endorsement-
     D                                     s - Physical Address-
     D                                      Changes')
     D***  Begin Add  ***  09/09/09  **************************************
     D CGCNTY          C                   CONST('MESSAGE: Please note-
     D                                      the defaulted -
     D                                     change to the county-
     D                                      code.')
     D***  End   Add  ***  09/09/09  **************************************
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@EEDT            8 0          MMDDCCYY
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C     SCAGT#        PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD
     C                   PARM                    ##PNAM           60
     C*
     C*  Compress the employer header information parameter list
     C*
     C     CMPRPL        PLIST
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C*
     C*  COMPANY MASTER
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      **** END   ADD **** 10/14/98 *********************************************
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*
     C***  Begin Add  ***  09/09/09  **************************************
     C* Zip Codes by County File
     C*
     C     CZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XZST
     C                   KFLD                    XZZIP5
     C*
     C     *LIKE         DEFINE    CZST          XZST
     C     *LIKE         DEFINE    CZZIP         XZZIP5
     C***  End   Add  ***  09/09/09  **************************************
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ELCO#         ##CO#
     C     *LIKE         DEFINE    ELFND         ##FND
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    ELFYR         ##FYR
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    EMPAD1        SVPAD1
     C     *LIKE         DEFINE    EMPAD2        SVPAD2
     C     *LIKE         DEFINE    EMPCTY        SVPCTY
     C     *LIKE         DEFINE    EMPST         SVPST
     C     *LIKE         DEFINE    EMMAD1        SVMAD1
     C     *LIKE         DEFINE    EMMAD2        SVMAD2
     C     *LIKE         DEFINE    EMMCTY        SVMCTY
     C     *LIKE         DEFINE    EMMST         SVMST
     C     *LIKE         DEFINE    EMCNTY        SVCNTY
     C     *LIKE         DEFINE    SCCNTD        SVCNTD
     C     *LIKE         DEFINE    ENREQ#        XXREQ#
     C     *LIKE         DEFINE    ENSEQ#        XXSEQ#
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
     C*  PHYSICAL ADDRESS
     C*
B1   C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C*
     C*  MAIL/PHYSICAL ADDRESS FLAG = "P", PHYSICAL ADDRESS REQUIRED
     C*
B2   C     EMMPFL        IFEQ      'P'
     C     SCPAD1        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   GOTO      ERCKX1
E2   C                   ENDIF
     C*
     C*  IF ADDRESS 1 IS BLANK, ALL ADDRESS FIELDS MUST BE BLANK
     C*
B2   C     SCPAD1        IFEQ      *BLANKS
     C*
B3   C     SCPAD2        IFNE      *BLANK
     C     SCPCTY        ORNE      *BLANK
     C     SCPST         ORNE      *BLANK
     C     SCPZP1        ORNE      *ZERO
     C     SCPZP2        ORNE      *ZERO
     C                   SETON                                        72  90
     C                   GOTO      ERCKX1
E3   C                   ENDIF
     C*
X2   C                   ELSE                                                   SCPAD1=*BLANKS
     C*
     C*  IF ADDRESS 1 IS ENTERED, OTHER ADDRESS FIELDS MUST BE ENTERED
     C*    THEN THE CITY MUST BE ENTERED
     C*
B3   C     SCPCTY        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   GOTO      ERCKX1
E3   C                   ENDIF                                                  SCPCTY = *BLANKS
     C*
     C*    THE STATE MUST BE VALID
     C*
B3   C     SCPST         IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   GOTO      ERCKX1
E3   C                   ENDIF                                                  SCPST <>*BLANKS
     C*
     C                   MOVEL(P)  SCPST         XXKEY
     C     STKY          CHAIN     PST01                              60
B3   C     *IN60         IFEQ      *ON
     C                   SETON                                        75  90
     C                   GOTO      ERCKX1
E3   C                   ENDIF                                                  *IN60 = *ON
     C*
     C*    THE ZIP CODE MUST BE ENTERED
     C*
     C     SCPZP1        IFLE      *ZEROS
     C                   SETON                                        76  90
     C                   GOTO      ERCKX1
     C                   ENDIF                                                  SCPZP1<=*ZERO
     C*
E2   C                   ENDIF                                                  SCPAD1=*BLANKS
     C*
E1   C                   ENDIF                                                  *IN90 = *OFF
     C*
     C***  Begin Add  ***  09/09/09  **************************************
     C*  Default county code
     C     SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         XZST
     C                   MOVE      SCPZP1        XZZIP5
     C                   ELSE
     C                   MOVE      EMMST         XZST
     C                   MOVE      EMMZIP        XXZIP
     C                   MOVE      XXZIP5        XZZIP5
     C                   ENDIF
     C*
     C     CZKY          CHAIN     PCZ01                              60
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C     CZCNTY        IFNE      SCCNTY
     C                   MOVEL(P)  CGCNTY        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      78            ##LENG            3 0
     C                   MOVEL     ##DATA        SCMSG
     C                   SETON                                        808190
     C                   ENDIF
     C                   MOVE      CZCNTY        SCCNTY
     C                   ENDIF
     C***  End   Add  ***  09/09/09  **************************************
     C*
     C*  VALIDATE THE COUNTY CODE
     C*
     C     SCPST         IFNE      *BLANKS
     C     SCPST         CAT(P)    SCCNTY:0      XXKEY
     C                   ELSE
     C     EMMST         CAT(P)    SCCNTY:0      XXKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C***  Begin Add  ***  09/09/09  **************************************
     C                   CLEAR                   CNDESC
     C***  End   Add  ***  09/09/09  **************************************
     C*
     C     TBKY          CHAIN     PCN01                              60
     C*
     C***  Begin Chg  ***  09/09/09  **************************************
     C************IN60     IFEQ *ON
     C                   MOVEL     CNDESC        SCCNTD
     C  N90*IN60         IFEQ      *ON
     C***  End   Chg  ***  09/09/09  **************************************
     C                   SETON                                        77  90
     C                   MOVEL     *BLANKS       SCCNTD
     C                   GOTO      ERCKX1
     C*
     C                   ELSE
     C*
     C     CNSTAT        IFEQ      'I'
     C                   SETON                                        78  90
     C                   MOVEL     *BLANKS       SCCNTD
     C                   ELSE
     C                   MOVEL     CNDESC        SCCNTD
     C                   ENDIF                                                  CNSTAT=I
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  VALIDATE THE CHANGE BOTH ADDRESSES FLAG
     C*
     C***  Begin Chg  ***  09/09/09  **************************************
     C********** SCCHGB    IFNE 'N'
     C  N90SCCHGB        IFNE      'N'
     C***  End   Chg  ***  09/09/09  **************************************
     C     SCCHGB        ANDNE     'Y'
     C                   SETON                                        79  90
     C                   GOTO      ERCKX1
     C                   ENDIF                                                  SCCHGB <> Y/N
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     *IN90         IFEQ      *OFF
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     EMDT          IFGT      SAVEDT
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C     ERCKX1        ENDSR
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
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10 (SEARCHES)
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C*  COUNTY
     C*
     C     HFLD          IFEQ      'SCCNTY'
     C*
     C     SCPST         IFNE      *BLANKS
     C                   MOVEL     SCPST         ##ST
     C                   ELSE
     C                   MOVEL     EMMST         ##ST
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   CALL      'W2215R'
     C                   PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    XXKEY
     C                   PARM                    ##ST              2
     C                   PARM      2             ##VIEW            1 0
     C*
     C     ##RRN         CHAIN     WTCNP                              60
     C     *IN60         IFEQ      *ON
     C***  Begin Chg  ***  09/09/09  **************************************
     C**********           MOVEL*BLANKS   SCCNTY
     C**********           MOVEL*BLANKS   SCCNTD
     C                   SETON                                        81
     C***  End   Chg  ***  09/09/09  **************************************
     C                   ELSE
     C     2             SUBST     CNKEY:3       SCCNTY
     C                   MOVEL(P)  CNDESC        SCCNTD
     C***  Begin Add  ***  09/09/09  **************************************
     C                   SETON                                        82
     C***  End   Add  ***  09/09/09  **************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD="SCCNTY"
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EMPAD1        SCPAD1
     C                   MOVEL     EMPAD1        SVPAD1
     C*
     C                   MOVEL     EMPAD2        SCPAD2
     C                   MOVEL     EMPAD2        SVPAD2
     C*
     C                   MOVEL     EMPCTY        SCPCTY
     C                   MOVEL     EMPCTY        SVPCTY
     C*
     C                   MOVEL     EMPST         SCPST
     C                   MOVEL     EMPST         SVPST
     C*
     C                   MOVEL     EMPZIP        SCPZIP
     C                   MOVEL     EMPZIP        SVPZIP
     C*
     C                   MOVEL     EMCNTY        SCCNTY
     C                   MOVEL     EMCNTY        SVCNTY
     C*
     C                   MOVEL     EMMAD1        SVMAD1
     C                   MOVEL     EMMAD2        SVMAD2
     C                   MOVEL     EMMCTY        SVMCTY
     C                   MOVEL     EMMST         SVMST
     C                   MOVEL     EMMZIP        SVMZIP
     C*
     C                   MOVEL     'N'           SCCHGB
     C*
     C*  COUNTY CODE DESCRIPTION
     C*
     C     SCPST         IFNE      *BLANKS
     C     SCPST         CAT(P)    SCCNTY:0      XXKEY
     C                   ELSE
     C     EMMST         CAT(P)    SCCNTY:0      XXKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C     TBKY          CHAIN     PCN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     CNSTAT        OREQ      'I'
     C                   MOVEL     *BLANKS       SCCNTD
     C                   MOVEL     *BLANKS       SVCNTD
     C                   ELSE
     C                   MOVEL     CNDESC        SCCNTD
     C                   MOVEL     CNDESC        SVCNTD
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         XXREQ#
     C                   ENDIF
     C*
     C     SCPAD1        IFNE      SVPAD1
     C     SCPAD2        ORNE      SVPAD2
     C     SCPCTY        ORNE      SVPCTY
     C     SCPST         ORNE      SVPST
     C     SCPZIP        ORNE      SVPZIP
     C*
     C                   Z-ADD     *ZERO         XXSEQ#
     C                   ADD       1             XXREQ#
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C     SCCHGB        IFEQ      'Y'
     C                   MOVEL     *BLANKS       XXAF01
     C     'PHYSICAL'    CAT(P)    'ADDRES':1    XXB401
     C                   CAT       'S':0         XXB401
     C                   Z-ADD     1             XXRSN
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C                   ENDIF
     C*** END ADD ***** 07/07/98 ***********************************************
     C*  ADDRESS 1
     C                   MOVEL     SCPAD1        EMPAD1
     C                   MOVEL(P)  SCPAD1        XXAF01           50
     C                   MOVEL(P)  SVPAD1        XXB401           50
     C                   Z-ADD     1             XXRSN             3 0
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C*  ADDRESS 2
     C     SCPAD2        IFNE      *BLANK
     C     SVPAD2        ORNE      *BLANK
     C                   MOVEL     SCPAD2        EMPAD2
     C                   MOVEL(P)  SCPAD2        XXAF01
     C                   MOVEL(P)  SVPAD2        XXB401
     C                   Z-ADD     1             XXRSN
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C                   ENDIF                                                  SCPAD2<>' '
     C*  CITY/STATE/ZIP
     C                   MOVEL     SCPCTY        EMPCTY
     C                   MOVEL     SCPST         EMPST
     C                   MOVEL     SCPZIP        EMPZIP
     C*  BUILD UP AFTER CITY/ST/ZIP
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C     SCPCTY        IFNE      *BLANKS
     C     SCPST         ORNE      *BLANKS
     C     SCPZIP        ORNE      *ZEROS
     C*** END ADD ***** 07/07/98 ***********************************************
     C     SCPCTY        CAT(P)    ',':0         XXAF01
     C                   CAT       SCPST:1       XXAF01
     C                   MOVEL     SCPZIP        XXZIP
     C                   CAT       XXZIP5:1      XXAF01
     C     SCPZP2        IFNE      *ZERO
     C                   CAT       '-':0         XXAF01
     C                   CAT       XXZIP4:0      XXAF01
     C                   ENDIF
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   ENDIF
     C*** END ADD ***** 07/07/98 ***********************************************
     C*  BUILD UP BEFORE CITY/ST/ZIP
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C     SVPCTY        IFNE      *BLANKS
     C     SVPST         ORNE      *BLANKS
     C     SVPZIP        ORNE      *ZEROS
     C*** END ADD ***** 07/07/98 ***********************************************
     C     SVPCTY        CAT(P)    ',':0         XXB401
     C                   CAT       SVPST:1       XXB401
     C                   MOVEL     SVPZIP        XXZIP
     C                   CAT       XXZIP5:1      XXB401
     C     SVPZP2        IFNE      *ZERO
     C                   CAT       '-':0         XXB401
     C                   CAT       XXZIP4:0      XXB401
     C                   ENDIF
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   ENDIF
     C*** END ADD ***** 07/07/98 ***********************************************
     C*
     C                   Z-ADD     1             XXRSN
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C                   ENDIF                                                  SCREEN<>SAVE
     C*
     C     SCCHGB        IFEQ      'Y'
     C*
     C     SCPAD1        IFNE      SVPAD1
     C     SCPAD2        ORNE      SVPAD2
     C     SCPCTY        ORNE      SVPCTY
     C     SCPST         ORNE      SVPST
     C     SCPZIP        ORNE      SVPZIP
     C*
     C     SCPAD1        IFNE      *BLANKS
     C     SCPCTY        ANDNE     *BLANKS
     C     SCPST         ANDNE     *BLANKS
     C     SCPZIP        ANDNE     *ZEROS
     C                   ADD       1             XXREQ#
     C                   Z-ADD     *ZERO         XXSEQ#
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   MOVEL     *BLANKS       XXAF01
     C     'MAILING'     CAT(P)    'ADDRES':1    XXB401
     C                   CAT       'S':0         XXB401
     C                   Z-ADD     3             XXRSN
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C*** END ADD ***** 07/07/98 ***********************************************
     C*  ADDRESS 1
     C                   MOVEL     SCPAD1        EMMAD1
     C                   MOVEL(P)  SCPAD1        XXAF01           50
     C                   MOVEL(P)  SVMAD1        XXB401           50
     C*** BEG CHG ***** 07/07/98 ***********************************************
     C*****************    Z-ADD1         XXRSN   30
     C                   Z-ADD     3             XXRSN             3 0
     C*** END CHG ***** 07/07/98 ***********************************************
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C*  ADDRESS 2
     C     SCPAD2        IFNE      *BLANK
     C     SVMAD2        ORNE      *BLANK
     C                   MOVEL     SCPAD2        EMMAD2
     C                   MOVEL(P)  SCPAD2        XXAF01
     C                   MOVEL(P)  SVMAD2        XXB401
     C*** BEG CHG ***** 07/07/98 ***********************************************
     C*****************    Z-ADD1         XXRSN   30
     C                   Z-ADD     3             XXRSN             3 0
     C*** END CHG ***** 07/07/98 ***********************************************
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C                   ENDIF                                                  SCMAD2<>' '
     C*  CITY/STATE/ZIP
     C                   MOVEL     SCPCTY        EMMCTY
     C                   MOVEL     SCPST         EMMST
     C                   MOVEL     SCPZIP        EMMZIP
     C*  BUILD UP AFTER CITY/ST/ZIP
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C     SCPCTY        IFNE      *BLANKS
     C     SCPST         ORNE      *BLANKS
     C     SCPZIP        ORNE      *ZEROS
     C*** END ADD ***** 07/07/98 ***********************************************
     C     SCPCTY        CAT(P)    ',':0         XXAF01
     C                   CAT       SCPST:1       XXAF01
     C                   MOVEL     SCPZIP        XXZIP
     C                   CAT       XXZIP5:1      XXAF01
     C     SCPZP2        IFNE      *ZERO
     C                   CAT       '-':0         XXAF01
     C                   CAT       XXZIP4:0      XXAF01
     C                   ENDIF
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   ENDIF
     C*** END ADD ***** 07/07/98 ***********************************************
     C*  BUILD UP BEFORE CITY/ST/ZIP
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C     SVMCTY        IFNE      *BLANKS
     C     SVMST         ORNE      *BLANKS
     C     SVMZIP        ORNE      *ZEROS
     C*** END ADD ***** 07/07/98 ***********************************************
     C     SVMCTY        CAT(P)    ',':0         XXB401
     C                   CAT       SVMST:1       XXB401
     C                   MOVEL     SVMZIP        XXZIP
     C                   CAT       XXZIP5:1      XXB401
     C     SVMZP2        IFNE      *ZERO
     C                   CAT       '-':0         XXB401
     C                   CAT       XXZIP4:0      XXB401
     C                   ENDIF
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   ENDIF
     C*** END ADD ***** 07/07/98 ***********************************************
     C*
     C*** BEG CHG ***** 07/07/98 ***********************************************
     C*****************    Z-ADD1         XXRSN   30
     C                   Z-ADD     3             XXRSN             3 0
     C*** END CHG ***** 07/07/98 ***********************************************
     C                   EXSR      WRTPEN
     C                   ADD       1             XXSEQ#
     C                   ENDIF                                                  SCMAD1 <> *BLANKS
     C*
     C                   ENDIF                                                  SCREEN<>SAVE
     C*
     C                   ENDIF                                                  SCCHGB = 'Y'
     C*
     C     SCCNTY        IFNE      SVCNTY
     C                   ADD       1             XXREQ#
     C                   Z-ADD     *ZERO         XXSEQ#
     C*
     C                   MOVEL     SCCNTY        EMCNTY
     C     SCCNTY        CAT(P)    '-':1         XXAF01
     C                   CAT       SCCNTD:1      XXAF01
     C     SVCNTY        CAT(P)    '-':1         XXB401
     C                   CAT       SVCNTD:1      XXB401
     C                   Z-ADD     2             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C                   ENDSR
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C                   MOVE      *ON           FIRST             1
     C                   MOVEL     'C'           @@MODE            1
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   MOVE      @@EEDT        SCEEDT
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN1        CHAIN     PEL01                              60
     C*
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELFND         ##FND
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELFYR         ##FYR
     C                   MOVE      ELEMP#        ##EMP#
     C                   MOVE      ELDIV         ##DIV
     C*
     C                   MOVEL     *BLANK        ##CONM
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              99
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              99
      **** END   ADD **** 10/14/98 *********************************************
     C                   MOVEL     FNNAME        ##FNAM
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   MOVEL     ##FNAM        SCNAME
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      CMPRES        CMPRPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C                   MOVE      *OFF          *IN58                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN58                          CANCELLED
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   MOVEL     DSTIME        EMTIME
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRTPEN - Write data base record to Endorsement Print Workfile
     C*================================================================
     C     WRTPEN        BEGSR
     C*
     C                   MOVE      *BLANK        ENCNTF
     C                   MOVE      *BLANK        ENPAFL
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   MOVE      *BLANK        ENMAFL
     C*** END ADD ***** 07/07/98 ***********************************************
     C                   MOVE      *BLANK        ENCBAF
     C*
     C                   MOVE      ##CO#         ENCO#
     C                   MOVE      ##FND         ENFND
     C                   MOVE      ##EMP#        ENEMP#
     C                   MOVE      ##DIV         ENDIV
     C                   MOVE      ##FYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   Z-ADD     XXREQ#        ENREQ#
     C                   Z-ADD     XXSEQ#        ENSEQ#
     C*
     C                   MOVE      @@EEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C                   MOVE      EMFEIN        ENFEN1
     C*** END ADD ***** 07/07/98 ***********************************************
     C                   MOVEL     SVMAD1        ENMA11
     C                   MOVEL     SVMAD2        ENMA21
     C                   MOVEL     SVMCTY        ENMCT1
     C                   MOVEL     SVMST         ENMST1
     C                   MOVEL     SVMZIP        ENMZP1
     C                   MOVEL     DXEMPA        ENRMCD
     C*
     C                   MOVEL     SVPAD1        ENPA11
     C                   MOVEL     SVPAD2        ENPA21
     C                   MOVEL     SVPCTY        ENPCT1
     C                   MOVEL     SVPST         ENPST1
     C                   MOVEL     SVPZIP        ENPZP1
     C                   MOVEL     SVCNTY        ENCNT1
     C*
     C                   MOVEL     SCPAD1        ENPA12
     C                   MOVEL     SCPAD2        ENPA22
     C                   MOVEL     SCPCTY        ENPCT2
     C                   MOVEL     SCPST         ENPST2
     C                   MOVEL     SCPZIP        ENPZP2
     C                   MOVEL     SCCNTY        ENCNT2
     C*
     C*** BEG CHG ***** 07/07/98 ***********************************************
     C********** SCCHGB    IFEQ *ON
     C     SCCHGB        IFEQ      'Y'
     C*** END CHG ***** 07/07/98 ***********************************************
     C                   MOVEL     SCPAD1        ENMA12
     C                   MOVEL     SCPAD2        ENMA22
     C                   MOVEL     SCPCTY        ENMCT2
     C                   MOVEL     SCPST         ENMST2
     C                   MOVEL     SCPZIP        ENMZP2
     C                   ENDIF
     C*
     C                   SELECT
     C     XXRSN         WHENEQ    1                                            ADDRESS
     C     SCCHGB        IFEQ      'Y'
     C                   MOVE      'Y'           ENCBAF
     C                   ENDIF
     C                   MOVE      'Y'           ENPAFL
     C     XXRSN         WHENEQ    2                                            COUNTY
     C                   MOVE      'Y'           ENCNTF
     C*** BEG ADD ***** 07/07/98 ***********************************************
     C     XXRSN         WHENEQ    3                                            ADDRESS
     C     SCCHGB        IFEQ      'Y'
     C                   MOVE      'Y'           ENCBAF
     C                   ENDIF
     C                   MOVE      'Y'           ENMAFL
     C*** END ADD ***** 07/07/98 ***********************************************
     C                   ENDSL
     C*
     C                   MOVE      'Y'           ENADFL
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     @@RRN2        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
      **** END   ADD **** 11/23/98 *********************************************
      **** BEGIN ADD **** 05/29/02 *********************************************
     C                   MOVE      SCCHGB        ENCBAF
      **** END   ADD **** 05/29/02 *********************************************
     C*
     C                   WRITE     PEN01
     C*
     C                   ENDSR
     C**************************************************************************
