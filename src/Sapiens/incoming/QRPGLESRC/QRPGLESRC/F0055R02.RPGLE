     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0055R02 - SUBSCRIBER A/R INFORMATION  C:62 12/16/97         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*               F0050R01                                        *
     H*  CALLS TO:                                                    *
     H*               NONE                                            *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/30/99 C:62 PROJECT: MC206                                 *
     H*                ADD T/M RATES TO WORKFLOW IF CASE MGMT         *
     H*                IS INSTALLED.                                  *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED F14=DARTS TO PROGRAM                     *
     H*                                                               *
     H*  10/07/98 C:94 PROJECT: 87MFX                                 *
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL              *
     H*                                                               *
     H*  04/22/98 C:61 PROJECT XXXXX                                  *
     H*                FIX EDIT                                       *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0055F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMSMP     UF   E             DISK
     F                                     RENAME(PSM01:DBRCDF)
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Subscriber A/R In-
     D                                     formation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   PARM                    @@CONT            1
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKGKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SMCO#         ##CO#
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
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
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C* CALL T/M RATES IF CASE MANAGEMENT IS INSTALLED
     C*
     C     ##MED         IFEQ      'Y'
     C     @@CONT        ANDEQ     'Y'
     C*
     C                   CALL      'F0046R'
     C                   PARM      '1'           ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* A/R ROUTINE TYPE IF HAS TO BE VALID.
     C*
      ***** BEG DEL ******* 04/22/98 **********************************
     C*********            MOVEL'ARRT'    ##TBID
     C*********            MOVELSCAR01    KKGKEY
      ***** END DEL ******* 04/22/98 **********************************
      ***** BEG ADD ******* 04/22/98 **********************************
     C                   MOVEL(P)  'ARRT'        ##TBID
     C                   MOVEL(P)  SCAR01        KKGKEY
      ***** END ADD ******* 04/22/98 **********************************
     C*
     C  N90GTKY          CHAIN     PGT01                              99
     C*
     C   99
     CANN90              SETON                                        7190
     C*
     C* IF A/R ROUTINE TYPE ROUTINE TYPE PRINT NOT BLANK THEN VALID.
     C*
     C     SCAR02        IFNE      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C*
      ***** BEG DEL ******* 04/22/98 **********************************
     C*********            MOVEL'ARRP'    ##TBID
     C*********            MOVELSCAR02    KKGKEY
      ***** END DEL ******* 04/22/98 **********************************
      ***** BEG ADD ******* 04/22/98 **********************************
     C                   MOVEL(P)  'ARRP'        ##TBID
     C                   MOVEL(P)  SCAR02        KKGKEY
      ***** END ADD ******* 04/22/98 **********************************
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* IF A/R ROUTINE TYPE IS 'B' THEN A/R ROUTINE TYPE PRINT NOT ' '
     C*
     C     SCAR01        IFEQ      'B'
     C*
     C     SCAR02        IFEQ      *BLANKS
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SMDT          IFGT      SAVEDT
     C     SMDT          OREQ      SAVEDT
     C     SMTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCAR01'
     C*
     C                   MOVEL     'ARRT'        ##TBID
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        30
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SCAR01
     C                   MOVEL     GTDESC        SCAD01
     C                   SETON                                        31
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCAR02'
     C*
     C                   MOVEL     'ARRP'        ##TBID
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SCAR02
     C                   MOVEL     GTDESC        SCAD02
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ****************************************
     C*================================================================
     C* KNDO  - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C*
     C                   CALL      'FA717R'
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ****************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCNAME
     C                   MOVEL     SMAR01        SCAR01
     C*
     C                   MOVEL     'ARRT'        ##TBID
     C                   MOVEL     SMAR01        KKGKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL     GTDESC        SCAD01
     C   99              MOVEL     *BLANKS       SCAD01
     C*
     C                   MOVEL     SMAR02        SCAR02
     C*
     C                   MOVEL     'ARRP'        ##TBID
     C                   MOVEL     SMAR02        KKGKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL     GTDESC        SCAD02
     C   99              MOVEL     *BLANKS       SCAD02
     C*
     C                   MOVEL     SMSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCAR01        SMAR01
     C                   MOVEL     SCAR02        SMAR02
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
     C                   SETOFF                                       3031
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C*** BEG ADD *** 12/01/98 ***************************************
     C     *INKN         CASEQ     *ON           KNDO
     C*** END ADD *** 12/01/98 ***************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C** CHECK FOR DIARY FUNCTION
     C*
     C*                    MOVEL'FB'      KKKEY     P
     C*
     C*          FBKY      CHAINSFB01                99
     C*
     C*          *IN99     IFEQ '0'
     C*
     C*          FBFL02    IFEQ 'Y'
     C*
     C*                    MOVE '1'       *IN16
     C*
     C*                    ELSE                            ELSE FBFL02 = 'Y'
     C*
     C*                    MOVE '0'       *IN16
     C*
     C*                    ENDIF                           END FBFL02 = 'Y'
     C*
     C*                    ENDIF                           END *IN60 SFB01
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SMUSER
     C                   MOVEL     Q1PGM         SMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SMDT
     C*
     C                   MOVEL     DSTIME        SMTIME
     C*
     C                   UPDATE    DBRCDF
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C* CALL T/M RATES IF CASE MANAGEMENT IS INSTALLED
     C*
     C     ##MED         IFEQ      'Y'
     C     @@CONT        ANDEQ     'Y'
     C*
     C                   CALL      'F0046R'
     C                   PARM      ' '           ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
