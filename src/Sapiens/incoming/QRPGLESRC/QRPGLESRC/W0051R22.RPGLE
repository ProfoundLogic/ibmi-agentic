     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*/TITLE MIDTERM ENDORSEMENT - CERT OF INSURANCE CHANGE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/03/08 C:136 SUPPORT: WA132 64919                           *
     H*                DO NOT CHANGE LOCATION NAMES WHEN DBA AND      *
     H*                FED NAME IN EMP MASTER IS CHANGED.             *
     H*                                                               *
     H* 08/31/07 C:61  SUPPORT WA045 59824 WC1202                     *
     H*                Correct overwrite of employer names.           *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 05/11/99 C:113 PROJECT: CM027                                 *
     H*                If the employer has an additional location,    *
     H*                update WMALP with changes to the name, federal *
     H*                name, federal ID or type of business.
     H*                                                               *
     H* 04/15/99 C:09 PROJECT: CM027                                  *
     H*               MIDTERM OWNER/OFFICER ENDORSEMENT REWRITE.      *
     H*               DO NOT ALLOW OWNER/OFFICER CHANGES ON A COI     *
     H*               CHANGE END, OWN/OFF END MUST BE DONE SEPARATE.  *
     H*****************************************************************
     FW0051F22  CF   E             WORKSTN
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
     FWDF2L     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMDXL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     F*** BEGIN ADD *** 05/11/99 **************************************
     FWMALL     UF   E           K DISK
     F*** END   ADD *** 05/11/99 **************************************
      /COPY SCOPYMT01D
     D*================================================================
     D*  Array and Table Definitions
     D*================================================================
     D*================================================================
     D*  Data Structure and Constant Definitions
     D*================================================================
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
     D                 DS
     D  XXDTDS                 1      8
     D  XXDTMM                 1      2
     D  XXDTDD                 3      4
     D  XXDTCY                 5      8
     D*
     D                 DS
     D  XXFEIN                 1      9
     D  XXFEI2                 1      2
     D  XXFEI7                 3      9
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
     D                                     s - Cert Of Insuranc-
     D                                     e Changes')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
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
     C*
     C*  Owner/Officer Work With parameter list
     C*
     C     OOPL          PLIST
     C     @@KC          PARM      '0'           ##KC              1
     C     @@KL          PARM      '0'           ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      *BLANK        ##ELFL            1
     C*
     C*  Agent/Producer Work With parameter list
     C*
     C     AGPL          PLIST
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM                    ##CALL
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      *BLANKS       ##QTE             1
     C                   PARM      'E'           ##MODE            1
     C*
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
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
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
     C*  ADMIN/DARTS CONTROL DEFINITION
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  FUND AUXILLIARY
     C*
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
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
     C*** BEGIN ADD *** 05/11/99 **************************************
     C     ALKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C*** END   ADD *** 05/11/99 **************************************
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
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
      ***** BEG CHG ******* 08/31/07 **********************************
     C*********  *LIKE     DEFN EMNAME    SVENAM
     C*********  *LIKE     DEFN EMFDNM    SVFDNM
     C     *LIKE         DEFINE    SCENAM        SVENAM
     C     *LIKE         DEFINE    SCNAM2        SVNAM2
     C     *LIKE         DEFINE    SCFDNM        SVFDNM
     C     *LIKE         DEFINE    SCFDN2        SVFDN2
      ***** END CHG ******* 08/31/07 **********************************
     C     *LIKE         DEFINE    EMFEIN        SVFEIN
     C     *LIKE         DEFINE    EMFND         SVPOL1
     C     *LIKE         DEFINE    EMEMP#        SVPOL2
     C     *LIKE         DEFINE    ELFYR         SVPOL3
     C     *LIKE         DEFINE    SCPFDT        SVPFDT
     C     *LIKE         DEFINE    SCPTDT        SVPTDT
     C     *LIKE         DEFINE    EMTPOB        SVTPOB
     C     *LIKE         DEFINE    SCTPBD        SVTPBD
     C     *LIKE         DEFINE    EMRSK#        SVRSK#
     C     *LIKE         DEFINE    SCCAR#        SVCAR#
     C     *LIKE         DEFINE    EMNAOB        SVNAOB
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
     C* DBA CANNOT BE BLANK
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCENAM        ANDEQ     *BLANK
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCENAM
     C*
     C*  EDIT THE FEDERAL ID NUMBER
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     EXFIFG        ANDEQ     'Y'
     C     SCFEIN        ANDEQ     *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
     C*
     C*  EDIT THE 1ST PART OF THE POLICY NUMBER (FUND)
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCPOL1        ANDLE     *ZERO
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  EDIT THE 2ND PART OF THE POLICY NUMBER (EMP#)
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCPOL2        ANDLE     *ZERO
     C                   SETON                                        74  90
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  EDIT THE 3RD PART OF THE POLICY NUMBER (FYR)
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCPOL3        ANDLE     *ZERO
     C                   SETON                                        75  90
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  EDIT THE POLICY FROM DATE
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPF              7 0
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        76  90
     C                   ENDIF                                                  SRERR=*ON
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  EDIT THE POLICY TO DATE
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPT              7 0
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        77  90
     C                   ENDIF                                                  SRERR=*ON
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  CROSS CHECK TO POLICY FROM/TO DATES
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     XXPF          ANDGT     XXPT
     C                   SETON                                        78  90
     C                   ENDIF                                                  XXPF > XXPT
     C*
     C*  VALIDATE TYPE OF BUSINESS
     C*
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  SCTPOB        XXKEY
     C*
     C                   MOVEL     *BLANKS       SCTPBD
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        79  90
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
     C*
     C* VALIDATE THE CARRIER NUMBER
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCCAR#        ANDLT     *ZERO
     C                   SETON                                        80  90
     C                   ENDIF                                                  *IN90 = *OFF
      **** BEGIN DEL **** 04/15/99 *********************************************
     C************
     C**VALIDATE*THE OWNER/OFFICER CHANGE FLAG
     C************
     C************IN90     IFEQ *OFF                       NO ERRORS SO FAR
     C***********SCOOCG    ANDNE'N'
     C***********SCOOCG    ANDNE'Y'
     C***********          SETON                     74  90
     C***********
     C***********          ENDIF                           *IN90 = *OFF
      **** END DEL ****** 04/15/99 *********************************************
     C*
     C* VALIDATE THE AGENT/PRODUCER CHANGE FLAG
     C*
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCAGCG        ANDNE     'N'
     C     SCAGCG        ANDNE     'Y'
     C                   SETON                                        82  90
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EMDT          IFGT      SAVEDT
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
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
     C                   SELECT
     C*
     C*  TYPE OF BUSINESS
     C*
     C     HFLD          WHENEQ    'SCTPOB'
     C*
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C     XXKEY         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN31
     C                   MOVEL(P)  *BLANKS       SCTPOB
     C                   MOVEL(P)  *BLANKS       SCTPBD
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN32
     C                   MOVEL(P)  XXKEY         SCTPOB
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ENDIF                                                  XXKEY=*BLANKS
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
      *
      *
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      ***** BEG DEL ******* 08/31/07 **********************************
     C******
     C***EMPLOYER NAME
     C******
     C******               MOVELEMNAME    SCENAM
     C******               MOVELEMNAME    SVENAM
     C******
     C***EMPLOYER FEDERAL NAME
     C******
     C******               MOVELEMFDNM    SCFDNM
     C******               MOVELEMFDNM    SVFDNM
     C******
     C*** BEGIN ADD ***  03/07/05  ************************************
     C******
     C******               MOVELEMNAME    SVENAM
     C******               MOVELEMNAM2    XXNM10 10 P
     C******               MOVE XXNM10    SVENAM
     C******               MOVE EMNAM2    SVNAM2 50
     C******
     C******               MOVE SVENAM    SCENAM
     C******               MOVE SVNAM2    SCNAM2
     C******
     C******               MOVELEMFDNM    SVFDNM
     C******               MOVELEMFDN2    XXNM10 10 P
     C******               MOVE XXNM10    SVFDNM
     C******               MOVE EMFDN2    SVFDN2 50
     C******
     C******               MOVE SVFDNM    SCFDNM
     C******               MOVE SVFDN2    SCFDN2
     C******
     C*** END ADD *****  03/07/05  ************************************
      ***** END DEL ******* 08/31/07 **********************************
      ***** BEG ADD ******* 08/31/07 **********************************
      *
      *  EMPLOYER NAME
      *
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        SCENAM
     C                   MOVEL(P)  SCENAM        SVENAM
      *
     C                   MOVE      EMNAM2        SCNAM2
     C                   MOVE      SCNAM2        SVNAM2
      *
      *  EMPLOYER FEDERAL NAME
      *
     C                   MOVEL(P)  EMFDNM        SCFDNM
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        SCFDNM
     C                   MOVE      SCFDNM        SVFDNM
      *
     C                   MOVE      EMFDN2        SCFDN2
     C                   MOVE      SCFDN2        SVFDN2
      *
      ***** END ADD ******* 08/31/07 **********************************
     C*
     C*  EMPLOYER FEDERAL ID #
     C*
     C                   MOVE      EMFEIN        SCFEIN
     C                   MOVE      EMFEIN        SVFEIN
     C*
     C*  POLICY NUMBER PART 1 - FUND
     C*
     C                   MOVE      EMFND         SCPOL1
     C                   MOVE      EMFND         SVPOL1
     C*
     C*  POLICY NUMBER PART 2 - EMPLOYER
     C*
     C                   MOVE      EMEMP#        SCPOL2
     C                   MOVE      EMEMP#        SVPOL2
     C*
     C*  POLICY NUMBER PART 3 - FUND YEAR
     C*
     C                   MOVE      ELFYR         SCPOL3
     C                   MOVE      ELFYR         SVPOL3
     C*
     C*  POLICY FROM DATE
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDT
     C                   MOVEL     SRDTO         SVPFDT
     C*
     C*  POLICY TO DATE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C                   MOVEL     SRDTO         SVPTDT
     C*
     C*  TYPE OF BUSINESS
     C*
     C                   MOVEL     EMTPOB        SCTPOB
     C                   MOVEL     EMTPOB        SVTPOB
     C*
     C*  TYPE OF BUSINESS DESCRIPTION
     C*
     C                   CLEAR                   SCTPBD
     C                   MOVEL     'BUSI'        XXTBID
     C                   MOVEL(P)  SCTPOB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCTPBD
     C                   MOVEL     GTDESC        SVTPBD
     C                   ENDIF
     C*
     C*  CARRIER NUMBER
     C*
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZEROS        SCCAR#
     C                   MOVE      *ZEROS        SVCAR#
     C                   ELSE
     C                   MOVE      F2CAR#        SCCAR#
     C                   MOVE      F2CAR#        SVCAR#
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  CALL OTHER PROGRAM FLAGS
     C*
      **** BEGIN DEL **** 04/15/99 *********************************************
     C*********************MOVEL'N'       SCOOCG
      **** END DEL ****** 04/15/99 *********************************************
     C                   MOVEL     'N'           SCAGCG
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     SCENAM        IFNE      SVENAM
     C*** BEGIN ADD ***  03/07/05  ************************************
     C     SCNAM2        ORNE      SVNAM2
     C                   MOVE(P)   SCENAM        XXNM10
     C                   MOVEL     XXNM10        EMNAM2
     C                   MOVE      SCNAM2        EMNAM2
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     SCENAM        EMNAME
     C                   MOVEL(P)  SCENAM        XXAF01           50
     C                   MOVEL(P)  SVENAM        XXB401           50
     C                   Z-ADD     1             XXRSN             3 0
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCFDNM        IFNE      SVFDNM
     C*** BEGIN ADD ***  03/07/05  ************************************
     C     SCFDN2        ORNE      SVFDN2
     C                   MOVE(P)   SCFDNM        XXNM10
     C                   MOVEL     XXNM10        EMFDN2
     C                   MOVE      SCFDN2        EMFDN2
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     SCFDNM        EMFDNM
     C                   MOVEL(P)  SCFDNM        XXAF01
     C                   MOVEL(P)  SVFDNM        XXB401
     C                   Z-ADD     2             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCFEIN        IFNE      SVFEIN
     C                   MOVE      SCFEIN        EMFEIN
     C                   MOVE      SCFEIN        XXFEIN
     C     XXFEI2        CAT(P)    '-':0         XXAF01
     C                   CAT       XXFEI7:0      XXAF01
     C                   MOVE      SVFEIN        XXFEIN
     C     XXFEI2        CAT(P)    '-':0         XXB401
     C                   CAT       XXFEI7:0      XXB401
     C                   Z-ADD     3             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCPOL1        IFNE      SVPOL1
     C     SCPOL2        ORNE      SVPOL2
     C     SCPOL3        ORNE      SVPOL3
     C                   MOVE      SCPOL1        XXPOL1            3
     C                   MOVE      SCPOL2        XXPOL2            9
     C                   MOVE      SCPOL3        XXPOL3            3
     C     XXPOL1        CAT(P)    '-':0         XXAF01
     C                   CAT       XXPOL2:0      XXAF01
     C                   CAT       '-':0         XXAF01
     C                   CAT       XXPOL3:0      XXAF01
     C                   MOVE      SVPOL1        XXPOL1
     C                   MOVE      SVPOL2        XXPOL2
     C                   MOVE      SVPOL3        XXPOL3
     C     XXPOL1        CAT(P)    '-':0         XXB401
     C                   CAT       XXPOL2:0      XXB401
     C                   CAT       '-':0         XXB401
     C                   CAT       XXPOL3:0      XXB401
     C                   Z-ADD     4             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCPFDT        IFNE      SVPFDT
     C     SCPTDT        ORNE      SVPTDT
     C                   MOVE      SCPFDT        XXDTDS
     C     'FROM:'       CAT(P)    XXDTMM:1      XXAF01
     C                   CAT(P)    '/':0         XXAF01
     C                   CAT       XXDTDD:0      XXAF01
     C                   CAT       '/':0         XXAF01
     C                   CAT       XXDTCY:0      XXAF01
     C                   MOVE      SCPTDT        XXDTDS
     C                   CAT       'THRU:':1     XXAF01
     C                   CAT       XXDTMM:1      XXAF01
     C                   CAT       '/':0         XXAF01
     C                   CAT       XXDTDD:0      XXAF01
     C                   CAT       '/':0         XXAF01
     C                   CAT       XXDTCY:0      XXAF01
     C*
     C                   MOVE      SVPFDT        XXDTDS
     C     'FROM:'       CAT(P)    XXDTMM:1      XXB401
     C                   CAT(P)    '/':0         XXB401
     C                   CAT       XXDTDD:0      XXB401
     C                   CAT       '/':0         XXB401
     C                   CAT       XXDTCY:0      XXB401
     C                   MOVE      SVPTDT        XXDTDS
     C                   CAT       'THRU:':1     XXB401
     C                   CAT       XXDTMM:1      XXB401
     C                   CAT       '/':0         XXB401
     C                   CAT       XXDTDD:0      XXB401
     C                   CAT       '/':0         XXB401
     C                   CAT       XXDTCY:0      XXB401
     C                   Z-ADD     5             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCTPOB        IFNE      SVTPOB
     C                   MOVEL     SCTPOB        EMTPOB
     C                   MOVEL(P)  SCTPBD        XXAF01
     C                   MOVEL(P)  SVTPBD        XXB401
     C                   Z-ADD     6             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCRSK#        IFNE      SVRSK#
     C                   MOVEL     SCRSK#        EMRSK#
     C                   MOVEL(P)  SCRSK#        XXAF01
     C                   MOVEL(P)  SVRSK#        XXB401
     C                   Z-ADD     7             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCCAR#        IFNE      SVCAR#
     C                   MOVEL(P)  SCCAR#        XXAF01
     C                   MOVEL(P)  SVCAR#        XXB401
     C                   Z-ADD     8             XXRSN
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C     SCNAOB        IFNE      SVNAOB
     C                   MOVEL     SCNAOB        EMNAOB
     C                   MOVEL(P)  SCNAOB        XXAF01
     C                   MOVEL(P)  SVNAOB        XXB401
     C                   Z-ADD     9             XXRSN
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
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
      **** BEGIN DEL **** 04/15/99 *********************************************
     C***********SCOOCG    IFEQ 'Y'
     C***********          CALL 'W1057R'  OOPL
     C***********          ENDIF
      **** END DEL ****** 04/15/99 *********************************************
     C*
     C     SCAGCG        IFEQ      'Y'
     C                   CALL      'W0553R'      AGPL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *INKC/*INKL = *OFF
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
     C*  GET ADMIN/DARTS INFORMATION
     C*
     C     EXKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PEX01
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
     C     SCENAM        IFNE      SVENAM
     C     SCFDNM        ORNE      SVFDNM
     C*** BEGIN ADD ***  03/07/05  ************************************
     C     SCNAM2        ORNE      SVNAM2
     C     SCFDN2        ORNE      SVFDN2
     C*** END ADD *****  03/07/05  ************************************
     C     SCFEIN        ORNE      SVFEIN
     C     SCTPOB        ORNE      SVTPOB
     C     SCRSK#        ORNE      SVRSK#
     C     SCNAOB        ORNE      SVNAOB
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
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
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCENAM <> SVENAM
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
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        ENNCFL
     C                   MOVE      *BLANK        ENFIFL
     C                   MOVE      *BLANK        ENTBFL
     C                   MOVE      *BLANK        ENRIDF
     C                   MOVE      *BLANK        ENNOBF
     C*
     C                   MOVE      ##CO#         ENCO#
     C                   MOVE      ##FND         ENFND
     C                   MOVE      ##EMP#        ENEMP#
     C                   MOVE      ##DIV         ENDIV
     C                   MOVE      ##FYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   MOVE      @@EEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     SVENAM        ENNAM1
     C                   MOVEL     SVFDNM        ENFDN1
     C****BEGIN ADD ***  03/07/05  ************************************
     C                   MOVEL     SVENAM        ENENM1
     C                   MOVEL     SVNAM2        ENENM2
      ***** BEG CHG ******* 08/31/07 **********************************
     C*********            MOVELSVFDNM    ENFDN1
     C*********            MOVELSVFDN2    ENFDN2
     C                   MOVEL     SVFDNM        ENEFN1
     C                   MOVEL     SVFDN2        ENEFN2
      ***** END CHG ******* 08/31/07 **********************************
     C*** END ADD *****  03/07/05  ************************************
     C*
     C                   MOVE      SVPOL1        XXPOL1
     C                   MOVE      SVPOL2        XXPOL2
     C                   MOVE      SVPOL3        XXPOL3
     C     XXPOL1        CAT(P)    '-':0         ENPOL1
     C                   CAT       XXPOL2:0      ENPOL1
     C                   CAT       '-':0         ENPOL1
     C                   CAT       XXPOL3:0      ENPOL1
     C*
     C                   MOVE      SVFEIN        ENFEN1
     C                   MOVEL     SVTPOB        ENTOB1
     C                   MOVEL     SVRSK#        ENRSK1
     C                   MOVEL     SVCAR#        ENCAR1
     C                   MOVEL     SVNAOB        ENNOB1
     C*
     C                   Z-ADD     SVPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENPFD1
     C*
     C                   Z-ADD     SVPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENPTD1
     C*
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C*
     C                   MOVEL     EMNAME        ENNAM2
     C                   MOVEL     EMFDNM        ENFDN2
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
     C*
     C                   MOVE      SCPOL1        XXPOL1
     C                   MOVE      SCPOL2        XXPOL2
     C                   MOVE      SCPOL3        XXPOL3
     C     XXPOL1        CAT(P)    '-':0         ENPOL2
     C                   CAT       XXPOL2:0      ENPOL2
     C                   CAT       '-':0         ENPOL2
     C                   CAT       XXPOL3:0      ENPOL2
     C*
     C                   MOVE      EMFEIN        ENFEN2
     C                   MOVEL     EMTPOB        ENTOB2
     C                   MOVEL     EMRSK#        ENRSK2
     C                   MOVEL     SCCAR#        ENCAR2
     C                   MOVEL     EMNAOB        ENNOB2
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENPFD2
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENPTD2
     C*
     C                   SELECT
     C     XXRSN         WHENEQ    1                                            EMNAME
     C     XXRSN         OREQ      2                                            EMFDNM
     C                   MOVE      'Y'           ENNCFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     XXRSN         WHENEQ    3                                            EMFEIN
     C                   MOVE      'Y'           ENFIFL
     C                   MOVEL     DXEMFI        ENRMCD
     C     XXRSN         WHENEQ    4                                            POLICY #
     C                   MOVE      'Y'           ENCIFL
     C                   MOVEL     DXCERT        ENRMCD
     C     XXRSN         WHENEQ    5                                            POLICY DT CHG
     C                   MOVE      'Y'           ENCIFL
     C                   MOVEL     DXCERT        ENRMCD
     C     XXRSN         WHENEQ    6                                            EMTPOB
     C                   MOVE      'Y'           ENTBFL
     C                   MOVEL     DXEMLS        ENRMCD
     C     XXRSN         WHENEQ    7                                            EMRSK#
     C                   MOVE      'Y'           ENRIDF
     C                   MOVEL     DXCERT        ENRMCD
     C     XXRSN         WHENEQ    8                                            CARRIER #
     C                   MOVE      'Y'           ENCIFL
     C                   MOVEL     DXCERT        ENRMCD
     C     XXRSN         WHENEQ    9                                            NATURE OF BUSINESS
     C                   MOVE      'Y'           ENNOBF
     C                   MOVEL     DXCERT        ENRMCD
     C                   ENDSL
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*** BEGIN ADD *** 05/11/99 **************************************
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ALELTP        IFEQ      'L'
     C*
      *** BEG DEL ***** 09/03/08 **************************************
     C*****************    MOVELEMNAME    ALNAME
     C*****************    MOVE EMFEIN    ALFEIN
      *** END DEL ***** 09/03/08 **************************************
     C                   MOVE      EMFDNM        ALFDNM
     C                   MOVE      EMTPOB        ALTPOB
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C*
     C                   MOVEL     DSTIME        ALTIME
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1PGM         ALPGM
     C                   MOVEL     Q1USER        ALUSER
     C*
     C                   UPDATE    PAL01
     C*
     C                   ENDIF                                                  ALELTP = 'L'
     C*
     C     ALKY          READE     PAL01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*** END   ADD *** 05/11/99 **************************************
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
     C*
     C                   WRITE     PEN01
     C*
     C                   ENDSR
