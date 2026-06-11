     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                                *
     H*                                                                     *
     H* THIS PROGRAM ALLOWS THE USER TO ENTER/UPDATE CAMPUS CODE            *
     H* TABLE RECORDS                                                       *
     H*                                                                     *
     H* CALLED FROM: W2267R                                                 *
     H* CALLS TO:                                                           *
     H*                                                                     *
     H* CREATED: 05/13/93               PROJECT: NW042                      *
     H***********************************************************************
     H*  REVISIONS:                                                         *
     H*                                                                     *
     H*  04/19/10 C:235 PROJECT: 25WRG                                      *
     H*                 CLAIMS GENERAL RETROFIT                             *
     H*                                                                     *
     H*  01/04/10 C:190 PROJECT: 25WRE                                      *
     H*                 Added pgm  to current release.                      *
     H*                                                                     *
     H*  12/10/01 C:062 PROJECT: WC025 29504
     H*                 Prevent duplicate records from being entered.
     H*                                                                     *
     H*  02/23/99 C:112 PROJECT: 25WJZ
     H*                 Added CAMPUS CODE modification to remove
     H*                 requirement for City and State for 'ODS'
     H*                 and 'BUS' Campus Codes
     H*
     H*  11/08/98 C:112 PROJECT: 25WJZ                                      *
     H*                 Claim Maintenance: Campus Code                      *
     H*                 Reimplementation of Project 25WGZ                   *
     H*                 Claim Campus Code / Postal Zip Link                 *
     H*                                                                     *
     H*  04/03/96 C:48 PROJECT: 25WGB                                       *
     H*                SCR01:  ADD CMD 8 - DISTRICT # SEARCH                *
     H*                SCR02:  ADD CAMPUS ADDRESS AND ZIPCODE               *
     H*                ADDED WMEML7 FOR DISTRICT # TO SELECT EMP/      DIV  *
     H*                                                                     *
     H*  01/07/94 C:24 PROJECT: NW052                                       *
     H*                EXPAND TO USE FULL KEY (CO#/FUND/EMP#/DIV/           *
     H*                CAMPUS CODE).  USE DATABASE MASTER FILE WM      CLL  *
     H*                IN PLACE OF THE TABLE FILE.                          *
     H***********************************************************      ******
     H*                                                                     *
     H* INDICATORS:                                                         *
     H*                                                                     *
     H*   09     SET TO ONE IF INQUIRY MODE                                 *
     H*   11     COMMAND KEY WAS PRESSED                                    *
     H*   60     GENERAL CHAIN INDICATOR                                    *
     H*   SCR01                                                             *
     H*   20    CAMPUS CODE CANNOT BE BLANKS                                *
     H*   21    IF INQUIRY MODE, CAMPUS CODE CANNOT BE BLANK                *
     H*   23    EMPLOYER MASTER RECORD MUST BE FOUND                        *
     H*   70-89 ERROR MESSAGES CODES                                        *
     H***********************************************************
     FW2267F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMCLP     UF A E             DISK
     F                                     RENAME(PCL01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*** BEGIN ADD *** 12/10/01 **************************************
     FWMCLL     IF   E           K DISK
     F*** END   ADD *** 12/10/01 **************************************
     FWMEMP     IF   E             DISK
     FWMFNL     IF   E           K DISK
      *** BEGIN ADD *** 02/23/99 *************************************
     FSTABL     IF   E           K DISK
      *** END ADD ***** 02/23/99 *************************************
      /COPY SCOPYMT01D
     D*                   ST     26  52  2
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*==========================================================      ======
     D*  /COPY required constants
     D*==========================================================      ======
     D OPTNM           C                   CONST('Campus Code Maintena-
     D                                     nce')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*==========================================================      ======
     C*  PLIST definitions
     C*==========================================================      ======
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
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
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CAMPUS CODE FILE - CO#/FND/EMP#/DIV
     C*
     C     CLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*** BEGIN ADD *** 12/10/01 **************************************
     C                   KFLD                    KKCMPS
     C*** END   ADD *** 12/10/01 **************************************
     C*
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *** BEGIN ADD *** 02/23/99 *************************************
      *  SYSTEM TABLE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *** END ADD ***** 02/23/99 *************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CLCO#         KKCO#
     C     *LIKE         DEFINE    CLFND         KKFND
     C     *LIKE         DEFINE    CLEMP#        KKEMP#
     C     *LIKE         DEFINE    CLDIV         KKDIV
     C     *LIKE         DEFINE    CLCMPS        KKCMPS
     C     *LIKE         DEFINE    CLCO#         @@CO#
     C     *LIKE         DEFINE    CLFND         @@FND
     C     *LIKE         DEFINE    CLEMP#        @@EMP#
     C     *LIKE         DEFINE    CLDIV         @@DIV
     C     *LIKE         DEFINE    CLEMP#        ##EMP#
     C     *LIKE         DEFINE    CLDIV         ##DIV
     C     *LIKE         DEFINE    CLCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLEMP#        XXEMP#
     C     *LIKE         DEFINE    CLDIV         XXDIV
     C     *LIKE         DEFINE    CLCMPS        XXCMPS
     C     *LIKE         DEFINE    CLDESC        XXDESC
     C     *LIKE         DEFINE    CLSTAT        XXSTAT
     C     *LIKE         DEFINE    CLUSER        XXUSER
     C     *LIKE         DEFINE    CLPGM         XXPGM
     C     *LIKE         DEFINE    CLTIME        XXTIME
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CLADDR        XXADDR
     C     *LIKE         DEFINE    CLCCTY        XXCCTY
     C     *LIKE         DEFINE    CLCST         XXCST
     C     *LIKE         DEFINE    CLCZIP        XXZIP
      *** BEGIN ADD *** 02/23/99 *************************************
     C     *LIKE         DEFINE    STKEY         XXKEY
      *** END ADD ***** 02/23/99 *************************************
     C*
     C/COPY SCOPYMT01C
     C*
      **** BEGIN ADD **** 11/08/98 ************************************
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     CLSTAT        IFEQ      'I'
     C     CLSTAT        OREQ      ' '
     C                   MOVE      'A'           CLSTAT
     C                   ELSE
     C                   MOVE      'I'           CLSTAT
     C                   ENDIF                                                  CPSTAT
     C*
     C                   MOVEL     Q1USER        CLUSER
     C                   MOVEL     Q1PGM         CLPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLDT
     C*
     C                   MOVEL     DSTIME        CLTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C**** END ADD ****** 11/08/98 ************************************
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
     C*** BEGIN ADD *** 12/10/01 **************************************
     C     @@MODE        IFEQ      'C'
     C                   SETON                                        89        PROTECT SCCMPS
     C                   ELSE
     C                   SETOFF                                       89
     C                   ENDIF
     C*** END   ADD *** 12/10/01 **************************************
     C*   The Campus Address, City, State and ZIP CODE are required,
     C*   except when CAMPUS CODE = 'ODS' OR 'BUS'. Description is
     C*   always required.
     C*
     C                   MOVEA     '000'         *IN(71)
     C                   MOVEA     '000'         *IN(81)
     C*
     C     SCDESC        IFEQ      *BLANK
     C                   MOVE      '1'           *IN71
     C                   MOVE      '1'           *IN90
     C                   ENDIF                                                  IFEQ BLANK
     C*
     C*** BEG   ADD *** 04/19/10 **************************************
     C     CLTID         IFEQ      '025'
     C*
     C     SCOFFD        IFNE      ' '
     C     SCOFFD        ANDNE     'Y'
     C                   SETON                                        6990
     C                   ENDIF
     C*
     C     SCOFFD        IFEQ      'Y'
     C     SCADDR        IFNE      ' '
     C                   SETON                                        4190
     C                   ENDIF
     C*
     C     SCCCTY        IFNE      ' '
     C                   SETON                                        4290
     C                   ENDIF
     C*
     C     SCCST         IFNE      ' '
     C                   SETON                                        4390
     C                   ENDIF
     C*
     C     SCCZP1        IFNE      0
     C                   SETON                                        4490
     C                   ENDIF
     C*
     C     SCCZP2        IFNE      0
     C                   SETON                                        4590
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ELSE
     C*** END   ADD *** 04/19/10 **************************************
     C*
     C     SCCMPS        IFNE      'ODS'
     C     SCCMPS        ANDNE     'BUS'                                        ADDR/Z      IP
     C*                                                    REQUIR      ED
     C     SCADDR        IFEQ      *BLANK
     C                   MOVE      '1'           *IN75
     C                   MOVE      '1'           *IN90
     C                   ENDIF                                                  IFEQ BLANK
     C*
     C     SCCCTY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN76
     C                   MOVE      '1'           *IN90
     C                   ENDIF                                                  IFEQ BLANK
     C*
     C     SCCST         IFEQ      *BLANK
     C                   MOVE      '1'           *IN74
     C                   MOVE      '1'           *IN90
     C                   ELSE
      *** BEGIN ADD *** 02/23/99 *************************************
      *
      *    THE MAILING STATE MUST BE VALID
      *
     C                   MOVEL(P)  SCCST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        77  90
      *** END ADD ***** 02/23/99 *************************************
     C                   ENDIF
     C*
     C     SCCZP1        IFEQ      *ZERO
     C                   MOVE      '1'           *IN78
     C                   MOVE      '1'           *IN90
     C                   ENDIF                                                  IFEQ BLANK
     C*
     C                   ENDIF                                                  SCCMPS
     C*** BEG ADD****** 04/19/10 **************************************
     C                   ENDIF
     C*** END ADD****** 04/19/10 **************************************
     C*** BEGIN ADD *** 12/10/01 **************************************
     C*
     C* NO DUPLICATE CAMPUS CODES ALLOWED
     C*
     C     *IN89         IFEQ      *OFF
     C                   Z-ADD     EMCO#         KKCO#
     C                   Z-ADD     EMFND         KKFND
     C                   Z-ADD     EMEMP#        KKEMP#
     C                   Z-ADD     EMDIV         KKDIV
     C                   MOVEL     SCCMPS        KKCMPS
     C*
     C     CLKY          CHAIN     PCL01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7990
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/10/01 **************************************
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'A'           CLSTAT
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF
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
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 12/10/01 **************************************
     C     @@MODE        IFEQ      'C'
     C                   SETON                                        89        PROTECT SCCMPS
     C                   ELSE
     C                   SETOFF                                       89
     C                   ENDIF
     C*** END   ADD *** 12/10/01 **************************************
      *
      *  Move Fund information to header fields
      *
     C                   MOVE      EMCO#         KKCO#
     C                   MOVE      EMFND         KKFND
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVEL     FNNAME        SCFNDN
     C                   MOVE      FNFND         SCFND
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
     C                   MOVE      CLCMPS        SCCMPS
     C*** BEG ADD****** 04/19/10 **************************************
     C                   MOVE      CLOFFD        SCOFFD
     C*** END ADD****** 04/19/10 **************************************
     C                   MOVE      CLDESC        SCDESC
     C                   MOVE      CLSTAT        SCSTAT
     C                   MOVE      CLADDR        SCADDR
     C                   MOVE      CLCCTY        SCCCTY
     C                   MOVE      CLCST         SCCST
     C                   MOVEL     CLCZIP        SCCZP1
     C                   MOVE      CLCZIP        SCCZP2            4 0
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      EMCO#         CLCO#
     C                   MOVE      SCFND         CLFND
     C                   MOVE      EMEMP#        CLEMP#
     C                   MOVE      EMDIV         CLDIV
     C                   MOVE      SCSTAT        CLSTAT
     C                   MOVE      SCCMPS        CLCMPS
     C*** BEG ADD****** 04/19/10 **************************************
     C                   MOVE      SCOFFD        CLOFFD
     C**** END ADD***** 04/19/10 **************************************
     C                   MOVE      SCDESC        CLDESC
     C                   MOVE      Q1USER        CLUSER
     C                   MOVE      Q1PGM         CLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLDT
     C                   Z-ADD     DSTIME        CLTIME
     C                   MOVE      SCADDR        CLADDR
     C                   MOVE      SCCCTY        CLCCTY
     C                   MOVE      SCCST         CLCST
     C                   MOVEL     SCCZP1        CLCZIP
     C                   MOVE      SCCZP2        CLCZIP
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
     C                   ENDSR
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
     C     @@MODE        IFEQ      'C'
     C                   SETON                                        89        PROTECT SCCMPS
     C*** BEGIN ADD *** 12/10/01 **************************************
     C                   ELSE
     C                   SETOFF                                       89
     C*** END   ADD *** 12/10/01 **************************************
     C                   ENDIF
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     @@RRN2        CHAIN     PEM01                              60
      **** BEG ADD ***** 04/19/10 *************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      **** END ADD ***** 04/19/10 *************************************
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
      **** BEGIN ADD **** 11/08/98 ************************************
     C     PREOTH        BEGSR
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      **** END ADD ***** 11/08/98 *************************************
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF                                                  *IN99
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WMCLP
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
