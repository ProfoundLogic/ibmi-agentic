     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H* PROGRAM: F7094R
     H* DESCRIPTION: CASE TRANSFER "TO" CONFIRMATION PROGRAM
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H*
     H*  REVISIONS:
     H*
     H*  12/04/98 C:94  PROJECT: SUP 15351
     H*                 CHANGE LENGTH OF PARMS
     H*
     H*****************************************************************
     H*
     FF7094F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FFMCDL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Case Transfer "To" I-
     D                                     nformation - Conform-
     D                                     ation')
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
     C                   PARM                    @@RRN1            9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@TD              1
     C                   PARM                    @@FLAG            1
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
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
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
     C                   CALL      'F7093R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN1            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      XXCO#         ##CO#             3 0
     C                   PARM      XXSUB#        ##SUB#            5 0
     C                   PARM      XXFND         ##FND             3 0
     C                   PARM      XXFYR         ##FYR             3 0
     C                   PARM      XXCASE        ##CASE            7 0
     C                   PARM      XXEMP#        ##EMP#            9 0
     C                   PARM      XXDIV         ##DIV             5 0
     C                   PARM      XXSS#         ##SS#             9
     C*** BEG CHG *** 12/04/98 ***************************************
     C                   PARM      XXACDT        ##ACDT            8 0
     C**********           PARM XXACDT    ##ACDT  70
     C*** END CHG *** 12/04/98 ***************************************
     C                   PARM      XXLSN         ##LSN            30
     C                   PARM      XXFSN         ##FSN            15
     C                   PARM      XXMID         ##MID             1
     C                   PARM      XXADR1        ##ADR1           40
     C                   PARM      XXADR2        ##ADR2           40
     C                   PARM      XXCTY         ##CTY            25
     C                   PARM      XXST          ##ST              2
     C                   PARM      XXZIP         ##ZIP             9 0
     C                   PARM      XXDOB         ##DOB             7 0
     C                   PARM      XXSEX         ##SEX             1
     C                   PARM      @@FLAG        ##FLAG            1
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
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
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C     @@RRN3        CHAIN(N)  DBRCDF                             99
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   MOVE      CMSS#         KKSS#
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD3
     C                   MOVEL     #1L2          SCHD4
     C*
     C                   Z-ADD     CMCO#         XXCO#             3 0
     C                   Z-ADD     CMSUB#        XXSUB#            5 0
     C                   Z-ADD     CMFND         XXFND             3 0
     C                   Z-ADD     CMFYR         XXFYR             3 0
     C                   Z-ADD     CMCASE        XXCASE            7 0
     C                   Z-ADD     CMEMP#        XXEMP#            9 0
     C                   MOVEL(P)  CMDIV         XXDIV             5 0
     C                   MOVEL(P)  CMSS#         XXSS#             9
     C*** BEG CHG *** 12/04/98 ***************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C**********           Z-ADDCMACDT    XXACDT  70
     C*** END CHG *** 12/04/98 ***************************************
     C                   MOVEL(P)  CMLSN         XXLSN            30
     C                   MOVEL(P)  CMFSN         XXFSN            15
     C                   MOVEL(P)  CMMID         XXMID             1
     C*
     C     CDKY          CHAIN     PCD01                              99
     C                   MOVE      CDADR1        XXADR1           40
     C                   MOVE      CDADR2        XXADR2           40
     C                   MOVE      CDCTY         XXCTY            25
     C                   MOVE      CDST          XXST              2
     C                   MOVEL     CDZIP         XXZIP             9 0
     C                   Z-ADD     CDDTOB        XXDOB             7 0
     C                   MOVE      CDSEX         XXSEX             1
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
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMCMP
     C                   ENDSR
