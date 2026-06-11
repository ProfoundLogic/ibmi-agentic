     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0401R02 - LOSS CONTROL TASK MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0400R02 - LOSS CONTROL TRACKING MAINTENANCE               *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 12/03/98  PROGRAMMER: C:90  PROJECT: 102AE          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/29/09 C:62  PROJECT OP135 67535                            *
     H*                Swap out client file with generic.             *
     H*                                                               *
     H* 02/28/06 C:168 SUPPORT WA102 51081                            *
     H*                Use the screen field 'Visit date' instead in   *
     H*                DARTS comment instead, this is the date entered*
     H*                in the 'Loss Control Tracking option'          *
     H*                                                               *
     H* 07/22/99 C:61  PROJECT 102BH                                  *
     H*                Create DARTS remark when visit code is CT or   *
     H*                MT.                                            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - DISPLAY MODE                                            *
     H*                                                               *
     H*                                                               *
     H*  ERROR:                                                       *
     H*                                                               *
     H*  71 - WCR0505 - RECORD NOT FOUND FOR THIS INFORMATION.        *
     H*  73 - WCR0503 - THIS FIELD CANNOT BE BLANK.                   *
     H*  74 - WCR2205 - INVALID VALUE KEYED.                          *
     H*  75 - WCR1058 - CURSOR NOT ON PROPER FIELD FOR SEARCH.        *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  30 - POSITION CURSOR AT SCHRS                                *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW0401F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMVSP     UF A E             DISK
     F                                     RENAME(PVS01:DBRCDF)
     F*
     F*  Loss Control Tracking File
     FWMLCP     IF   E             DISK
     F*
     F*  Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F*  User Profile File
     FWMUPL1    IF   E           K DISK
     F*
     F*** Begin Chg *** 09/29/09 **************************************
     F**********WTVLL   IF  E           K        DISK
     FWTGTL     IF   E           K DISK
     F*** End   Chg *** 09/29/09 **************************************
     F*
      /COPY SCOPYMT01D
     D*
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCFND                  1      3  0
     D  QCEMP#                 4     12  0
     D  QCDIV                 13     17  0
      ***** BEG ADD ******* 07/22/99 **********************************
     D                 DS
     D  QC1KY                  1     40
     D  QC1CO#                 1      3  0
     D  QC1FND                 4      6  0
     D  QC1EMP                 7     15  0
     D  QC1DIV                16     20  0
      ***** END ADD ******* 07/22/99 **********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Loss Control Tasks')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D AGTAPI          C                   CONST('WA002R')
     D*
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*                                                                *********
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  XXHRS                  1      5  2
     D  XXHR                   1      3  0
     D  XXREM                  4      5  0

     I*
      *

     C*================================================================
     C*  PLIST definitions
     C*================================================================
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
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      LCFND         ##FND
     C                   PARM      LCEMP#        ##EMP#            9 0
     C                   PARM      LCDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C     SCAGTN        PARM                    ##ANAM           60
     C     SCPRD#        PARM                    ##PROD            5 0
     C     SCPRDN        PARM                    ##PNAM           60
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     UPKY          KLIST
     C                   KFLD                    SCSTF
      *
      *   KEY LIST FOR THE CLIENT TABLE FILE
      *
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           QQID
     C**********           KFLD           QQKEY
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    QQID
     C                   KFLD                    QQKEY
     C*** End   Chg *** 09/29/09 **************************************
      *
      *   KEY LIST FOR THE LOSS CONTROL TASK FILE
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
     C                   Z-ADD     0             I1                3 0
     C*
     C/COPY SCOPYMT01C
     C*
      ***** BEG ADD ******* 07/22/99 **********************************
      *
      *
      *
      *================================================================
      * CRTDRT - Create DARTS Remark
      *================================================================
     C     CRTDRT        BEGSR
      *
      *
     C                   MOVE      *BLANKS       QC1KY
     C                   MOVE      EMCO#         QC1CO#
     C                   MOVE      EMFND         QC1FND
     C                   MOVE      EMEMP#        QC1EMP
     C                   MOVE      EMDIV         QC1DIV
      *
     C                   MOVEL(P)  QC1KY         ##KEY
      *
      * Get comment for DARTS record
      *
      *   Visit type description
      *
     C                   MOVE      'LTRK'        QQID
     C                   MOVEL     SCVSTP        QQKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C     GTKY          CHAIN     PGT01                              60
     C*** End   Chg *** 09/29/09 **************************************
      *
      *   Current Date
      *
      ****  BEG  CHG  02/28/06 ****************************************
     C**                   MOVE VSVSDT    SRDTI
     C                   MOVE      XXLCVS        SRDTI
      ****  BEG  CHG  02/28/06 ****************************************
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXVSDT            8 0
      *
     C                   MOVE      XXVSDT        XXYY              4
     C                   MOVEL     XXVSDT        XXMMDD            4
     C                   MOVEL     XXMMDD        XXMM              2
     C                   MOVE      XXMMDD        XXDD              2
      *
      *   Hours
      *
     C                   MOVEL(P)  VSHRS         ##NUM
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C                   PARM      2             ##DEC             3 0
     C                   PARM      '1'           ##THOU            1
      *
      *
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLDESC    CAT  XXMM:2    #1COMM    P
     C     GTDESC        CAT(P)    XXMM:2        #1COMM
     C*** End   Chg *** 09/29/09 **************************************
     C                   CAT(P)    '/':0         #1COMM
     C                   CAT(P)    XXDD:0        #1COMM
     C                   CAT(P)    '/':0         #1COMM
     C                   CAT(P)    XXYY:0        #1COMM
     C                   CAT(P)    SCSTFN:2      #1COMM
     C                   CAT(P)    ##NUM:2       #1COMM
      *
     C                   MOVE      DSDATE        #1CDAT
     C                   MOVEL(P)  'W0050R'      #1PNAM
      *
      * CALL API TO WRITE DARTS RECORD
      *
     C                   CALL      'DA700R'
     C                   PARM                    ##KEY            40
     C                   PARM                    #1COMM          256
     C                   PARM                    #1CDAT            8
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM                    #1PNAM           10
     C                   PARM      *ZEROS        #1SECL            3
     C                   PARM      Q1USER        #1USER           10
     C                   PARM      Q1PGM         #1PGM            10
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      'N'           #1DIFL            1
     C                   PARM      Q1PGM         #1IPGM           10
     C                   PARM      'N'           #1FL01            1
     C                   PARM      'N'           #1FL02            1
     C                   PARM                    #1RMCD            5
     C                   PARM      *BLANKS       #1SEQ#            5
      *
     C                   ENDSR                                                  CRTDRT
      *
      *
      *
      ***** END ADD ******* 07/22/99 **********************************
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
     C*  VALIDATE VISIT TYPE
     C*
     C                   MOVE      'LTRK'        QQID
     C                   MOVEL     SCVSTP        QQKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C     GTKY          CHAIN     PGT01                              60
     C*** End   Chg *** 09/29/09 **************************************
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  VALIDATE HOURS
     C*
     C     SCHRS         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN73
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C     SCHRS         DIV       .5            XXHRS
     C     XXREM         IFNE      *ZEROS
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   ENDIF
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
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*  TYPE OF VISIT SEARCH
     C*
     C     HFLD          WHENEQ    'SCVSTP'
     C*
     C                   MOVE      *BLANKS       QQRTC
     C                   MOVE      *BLANKS       QQKEY
     C*
     C*** Begin chg *** 09/29/09 **************************************
     C**********           CALL 'W2497R'
     C                   CALL      'W2493R'
     C*** End   chg *** 09/29/09 **************************************
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      'LTRK'        QQID              4
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         IFEQ      'A'
     C                   EXSR      PGMEND
     C                   ENDIF
      *
     C     QQRTC         IFEQ      'B'
     C                   EXSR      PGMSTR
     C                   END
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQKEY         SCVSTP
     C*
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**********           MOVELVLDESC    SCVTYD
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL     GTDESC        SCVTYD
     C*** End   Chg *** 09/29/09 **************************************
     C*
     C                   MOVE      *ON           *IN30
     C*
     C                   END
      *
     C     HFLD          WHENNE    'SCVSTP'
     C                   MOVE      *ON           *IN75
     C*
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     SCEMP#        QCEMP#
     C                   Z-ADD     SCDIV         QCDIV
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0400R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         VSCO#
     C                   MOVE      SCFND         VSFND
     C                   MOVE      SCEMP#        VSEMP#
     C                   MOVE      SCDIV         VSDIV
     C*
     C                   MOVE      SCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VSVSDT
     C*
     C                   MOVE      SCSTF         VSSTF
     C                   MOVE      SCVSTP        VSVSTP
     C                   Z-ADD     SCHRS         VSHRS
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
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
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
     C                   MOVE      *OFF          *IN09
     C                   MOVE      *BLANKS       SCVSTP
     C                   MOVE      *BLANKS       SCVTYD
     C                   MOVE      *BLANKS       SCHRS
     C*
     C                   MOVE      ##FND         SCFND
     C                   MOVE      ##FNAM        SCFNDN
     C*
     C     @@RRN2        IFNE      *ZERO
     C*
     C     @@RRN2        CHAIN     PLC01                              60
     C*
     C                   MOVEL     LCEMP#        SCEMP#
     C                   MOVEL     LCDIV         SCDIV
     C*
      *****  BEG  ADD  02/28/06 ***************************************
     C                   Z-ADD     LCVSDT        XXLCVS            7 0
      *****  BEG  ADD  02/28/06 ***************************************
      *
     C                   Z-ADD     LCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVSDT
     C*
     C                   MOVEL     LCSTF         SCSTF
     C*
     C                   ENDIF
     C*
      *
      *  GET EMPLOYER NAME
      *
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        SCEMPN
     C*
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVEL     UPUSNM        SCSTFN
     C*
      *  GET AGENT/PRODUCER INFORMATION
      *
     C                   Z-ADD     *ZEROS        ##FYR
      *
     C                   CALL      AGTAPI        AGTPL
      *
      *  GET CANCEL DATE
      *
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C* LOAD VISIT TYPE AND HOURS
     C*
     C     @@MODE        IFNE      'A'
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   MOVEL     VSVSTP        SCVSTP
     C                   MOVEL     VSHRS         SCHRS
     C*
     C                   MOVEL     SCVSTP        QQKEY
     C                   MOVEL     'LTRK'        QQID
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**********           MOVELVLDESC    SCVTYD
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL     GTDESC        SCVTYD
     C*** End   Chg *** 09/29/09 **************************************
     C*
     C                   ENDIF
     C*
     C*  DISPLAY MODE
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09
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
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        VSUSER
     C                   MOVEL     Q1PGM         VSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VSDT
     C                   MOVEL     DSTIME        VSTIME
      *
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
     C                   MOVEL     Q1USER        VSUSER
     C                   MOVEL     Q1PGM         VSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VSDT
     C                   MOVEL     DSTIME        VSTIME
     C*
     C                   MOVE      VSUSER        VSIUSR
     C                   MOVE      VSPGM         VSIPGM
     C                   MOVE      VSDT          VSIDT
     C                   MOVE      VSTIME        VSITM
      *
     C                   WRITE     DBRCDF
      ***** BEG ADD ******* 07/22/99 **********************************
      *
      * Create DARTS remark if ADD mode and visit code - CT or MT
      *
     C     @@MODE        IFEQ      'A'
      *
     C     VSVSTP        IFEQ      'CT'
     C                   MOVEL(P)  'CSTR'        #1RMCD
     C                   EXSR      CRTDRT
     C                   ENDIF                                                  VSVSTP
      *
     C     VSVSTP        IFEQ      'MT'
     C                   MOVEL(P)  'MFTR'        #1RMCD
     C                   EXSR      CRTDRT
     C                   ENDIF                                                  VSVSTP
      *
     C                   ENDIF                                                  @@MODE
      *
      ***** END ADD ******* 07/22/99 **********************************
     C                   CLEAR                   SCVSTP
     C                   CLEAR                   SCVTYD
     C                   CLEAR                   SCHRS
     C                   FEOD      WMVSP
     C                   ENDSR
