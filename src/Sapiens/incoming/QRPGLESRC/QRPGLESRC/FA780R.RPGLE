     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE FA780R - Display EOB#
     H*****************************************************************
     H*  FA780R - API TO DISPLAY THE EOB# AND IMAGE DOC ID            *
     H*  CREATED: 08/07/12  C:280 Project: 144MCF                     *
     H*                                                               *
     H*  CALLED FROM: F0350R                                          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/15  C:280  Phaseware: 35252 tml                       *
     H*                   Convert screen to profound                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FFA780F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFAEOP     UF   E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     F*
     F* EOMB Transaction File
     F*
     D*
      /COPY SCOPYMT01D
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Display Image Doc ID#')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D RESMSG          C                   CONST('*** BACKED OUT ***')
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
 P1  C     *INKC         PARM      *INKC         @@KC              1
 P2  C     *INKL         PARM      *INKL         @@KL              1
 P4  C                   PARM                    @@RRN             9 0
 P3  C                   PARM                    @@MODE            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *like         define    @@rrn         @@rrn2
     C     *LIKE         DEFINE    EOBLST        KKBLST
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*
     C     CK1END        ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* CHKPMT - Check for an uprocessed payment at a previous recon
     C*================================================================
     C*
     C     CHKPMT        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KVDO - F21=WORK WITH PROVIDER BILL DETAIL
     C*================================================================
     C*
     C     KVDO          BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   eval      *inlr = *on
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C                   MOVE      EOBLST        KKBLST
     C*
     C                   eval      sceob# = eoeob#
     C                   eval      scidoc = eoidoc
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C     SCL3HD        PARM      *BLANKS       ##L1             78
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   ENDSR
     C*
