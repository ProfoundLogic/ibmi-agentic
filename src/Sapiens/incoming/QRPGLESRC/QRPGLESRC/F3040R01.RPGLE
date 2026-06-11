     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F3040R01 - T/M ACTIVITY MASTER INFORMATION     C:62 04/20/98 *
     H*                                                               *
     H*  CALLED FROM: F3040R - WW T/M ACTIVITY.                       *
     H*                                                               *
     H*  CALLS TO   : FA702R - TWO LINE HEADER API.                   *
     H*               F0276R - MEDICAL SERVICES.                      *
     H*               N2315R - T/M ACTIVITY CODES.                    *
     H*               SA001R - API THAT ASSIGNS VARIOUS SYSTEM FLAGS. *
     H*             SRCVTDAT - DATE CONVERSION PROGRAM.               *
     H*              SRFUTR8 - CALCULATES FUTURE DATE.                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/19/00 C:62 PROJECT: MCMNT                                 *
     H*                ADD OVERRIDE FLAGS FOR RATES.                  *
     H*                                                               *
     H*  10/09/00 C:62 PROJECT: MC035 24620                           *
     H*                REMOVE EDIT ON SVDT VS EOFRDT.                 *
     H*                                                               *
     H*  08/15/00 C:62 PROJECT: MC215                                 *
     H*                REPLACE SFBPF REFERENCES WITH APIS.            *
     H*                                                               *
     H*  05/19/00  C:62  PROJECT: ARMNT                               *
     H*                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.  *
     H*                                                               *
     H*  02/23/00  C:62  PROJECT: MCMNT  (BASE ONLY)                  *
     H*                  MAKE SURE STAFF CODE IS POPULATED FROM CM    *
     H*                  SERVICE IF LEFT BLANK.                       *
     H*                                                               *
     H*  01/11/00  C:62  PROJECT: MC035 21035 (BASE/INHOUSE ONLY)     *
     H*                  UPDATE TMINV# WITH EABINV IF CALLED FROM     *
     H*                  REINVOICE FACILITY.                          *
     H*                                                               *
     H*  10/25/99  C:62  PROJECT: MC216 (BASE/INHOUSE ONLY)           *
     H*                  ADD ABILITY TO HANDLE REHAB HOLD/RELEASE.    *
     H*                                                               *
     H*  10/05/99  C:62  PROJ: MC212                                  *
     H*                  UPDATE TMSYS WITH EOSYS.                     *
     H*                                                               *
     H*  09/07/99  C:62  PROJ: MC212                                  *
     H*                  GETTING T/MS TO WORK.                        *
     H*                                                               *
     H*  06/08/99  C:062 PROJECT: MC145                               *
     H*                  CORRECTED THE CALCULATION OF BILLING AMOUNT. *
     H*                                                               *
     H*  05/26/99  C:062 PROJECT: MC145                               *
     H*                  RE-ADDED THE TOTALS OF 6/29/98.              *
     H*                                                               *
     H*  03/05/99  C:117 PROJECT: MC172                               *
     H*                  CHANGE SCREEN TO INCLUDE T/M SEQ# AND MOVED  *
     H*                  A FEW FIELDS. NEED TO ADD SOME ERROR MESS.   *
     H*                  FOR MODIFIED ADD & UPDATE SCREENS.           *
     H*                                                               *
     H*  01/17/98  C:62  PROJECT: MC152                               *
     H*                  WRITE RECON NUMBER                           *
     H*                                                               *
     H*  12/11/98  C:94  PROJECT: MC152                               *
     H*                  DELETE ALL UPDATES TO FAHBP, NAMBP, NAPBP,   *
     H*                  NATRP, AND FAEOP.  CALL API (AA001R) TO DO   *
     H*                  ALL UPDATES.  THIS MAY INCLUDE DELETEING     *
     H*                  SOME OF THE REVISIONS MADE ON 8/13/98,       *
     H*                  11/17/98                                     *
     H*                                                               *
     H*  11/17/98  C:62  PROJECT: MC098 15287                         *
     H*                  1) EXTEND KEY FOR NAMBP TO EOB# - NAMBL1     *
     H*                                                               *
     H*  08/13/98  C:62  PROJECT: MC098 15287                         *
     H*                  CHANGE THE DELETION PROCESS WITH REGARDS TO  *
     H*                  UPDATING THE AMOUNT FILES.                   *
     H*                                                               *
     H*  08/06/98  C:94  PROJECT: MC145                               *
     H*                  PROTECTED SCMSCD AND SCMSEQ IF SELECTED FROM *
     H*                  F0276R.  THIS IS BASED ON RRN3.  F0276R IS   *
     H*                  THE ONLY PROGRAM THAT PASSES IN A RRN3 THAT  *
     H*                  IS GREATER THAN ZERO.                        *
     H*                                                               *
     H* 06/29/98  C:62 MC145                                          *
     H*           ADDED CALCULATION FOR TOTAL HOURS AMOUNT AND TOTAL  *
     H*           TOTAL MILES AMOUNT.                                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*    KJDO (PLACE INDICATOR)   : 31-36.                          *
     H*    ERRCK1 (ERROR INDICATORS): 71-77.                          *
     H*                                                               *
     H*****************************************************************
     H*  SPECIAL OPERATIONS:                                          *
     H*                                                               *
     H*    *IF CALLED FROM F0269R, F0350R, OR U0060R, @@RRN3 IS ZERO  *
     H*     AND *IN31 IS SETON TO POSITION ON CASE MGMT SVC CODE.     *
     H*    *IF CALLED FROM F0276R @@RRN3 HOLDS RRN OF SELECTED CASE   *
     H*     MGMT SVC CODE, F6=ADD DEFAULTS MGMT CODE, SEQ# AND STAFF  *
     H*     CODE, SETONS *IN33 TO POSITION CURSOR ON ACTIVITY CODE.   *
     H*                                                               *
     H*****************************************************************
     FF3040F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FNDTMP     UF A E             DISK
     F                                     RENAME(PTM01:DBRCDF)
     F*** BEGIN ADD *** 08/13/98 **************************************
     FNDTML1    IF   E           K DISK
     F*** END   ADD *** 08/13/98 **************************************
     F*** BEG CHG *** 12/11/98 ***************************************
     FFAEOP     IF   E             DISK
     F**********FAEOP   UF  E                    DISK
     F*** END CHG *** 12/11/98 ***************************************
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FFAMSP     IF   E             DISK
     FFAMSL     IF   E           K DISK
     F                                     RENAME(PMS01:PMS02)
     FFTMCL     IF   E           K DISK
     FSMUPP     IF   E             DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FFTATP     IF   E             DISK
     FFTABL     IF   E           K DISK
     F                                     RENAME(PAT01:PAT02)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRC01)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMFRL     IF   E           K DISK
     FFMSRL     IF   E           K DISK
     F*** BEGIN ADD *** 08/13/98 **************************************
     FUMPCL11   IF   E           K DISK
     F*** END   ADD *** 08/13/98 **************************************
     F*** BEG ADD *** 12/11/98 ***************************************
     F*** BEGIN DEL *** 09/07/99 **************************************
     F**********FAEAL1  IF  E           K        DISK
     F*** END   DEL *** 09/07/99 **************************************
     F*** END ADD *** 12/11/98 ***************************************
     F*** BEGIN ADD *** 01/11/00 **************************************
     FFAEAL1    IF   E           K DISK
     F*** END   ADD *** 01/11/00 **************************************
      /COPY SCOPYMT01D
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*
     D*** BEG ADD *** 12/11/98 **************************************
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D*** BEGIN CHG *** 06/08/99 **************************************
     D**********                              1   7 KKWRUP
     D  ##WRUP                 1      7
     D*** END   CHG *** 06/08/99 **************************************
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END ADD *** 12/11/98 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Time/Materials Activ-
     D                                     ity')
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
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   PARM                    @@CERT           15
     C                   PARM                    @@EXT             3 0
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM                    @@RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C* SEARCH PARM LIST
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ATKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     E1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     FRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     FRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEFDT
     C*
     C     SRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     SRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKEFDT
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     HBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 11/17/98 **************************************
     C                   KFLD                    KKEOB#
     C*** END   ADD *** 11/17/98 **************************************
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*** BEG ADD *** 12/11/98 ****************************************
     C********** EAKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C**********           KFLD           KKFYR
     C**********           KFLD           KKCASE
     C**********           KFLD           KKEOB#
     C*** END ADD *** 12/11/98 ****************************************
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 01/11/00 **************************************
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    @@RCN
     C*** END   ADD *** 05/19/00 **************************************
     C*** END   ADD *** 01/11/00 **************************************
     C*
     C     PBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   KFLD                    EOEOB#
     C*
     C     PCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    @@RCN
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C     TMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN CHG *** 05/19/00 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C**********           KFLD           KKRCN
     C                   KFLD                    @@RCN
     C*** END   ADD *** 01/17/99 **************************************
     C*** END   CHG *** 05/19/00 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C     TRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 11/17/98 **************************************
     C                   KFLD                    KKEOB#
     C*** END   ADD *** 11/17/98 **************************************
     C*
     C     MCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     MSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C                   KFLD                    KKMSCD
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKSTAF
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFND         K1FND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*** BEGIN ADD *** 01/17/99 **************************************
     C     *LIKE         DEFINE    EORCN         KKRCN
     C*** END   ADD *** 01/17/99 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    MCKEY         KKKEY
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    TMSVDT        KKSVDT
     C     *LIKE         DEFINE    MSSEQ#        KKSEQ#
     C     *LIKE         DEFINE    MSMSCD        KKMSCD
     C     *LIKE         DEFINE    MSENDT        X1ENDT
     C     *LIKE         DEFINE    TMSVDT        XXSVDT
     C     *LIKE         DEFINE    FREFDT        KKEFDT
     C     *LIKE         DEFINE    EOBAMT        XXBAMT
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     *LIKE         DEFINE    TMDT          XXDT
     C     *LIKE         DEFINE    TMSEQ#        HDSEQ#
     C     *LIKE         DEFINE    PCCERT        KKCERT
     C     *LIKE         DEFINE    PCEXT         KKEXT
     C     *LIKE         DEFINE    SCHRRT        ##HRRT
     C     *LIKE         DEFINE    SCMIRT        ##MIRT
     C*** BEGIN ADD *** 03/05/99 **************************************
     C     *LIKE         DEFINE    TMDT          XXDATE
     C*** END   ADD *** 03/05/99 **************************************
     C*
     C*
     C* PROCESS SPECIAL MODE 'M'
     C*
     C     @@MODE        IFEQ      'M'
     C*
     C                   MOVEL     'C'           @@MODE
     C                   MOVEL     'M'           HDMODE            1
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           HDMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*================================================================
     C* CHGHR - SUBROUTINE FOR CHANGING HOLD STATUS
     C*================================================================
     C*
     C     CHGHR         BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     TMHOLD        IFEQ      'R'
     C                   MOVE      'H'           TMHOLD
     C                   ELSE
     C                   MOVE      'R'           TMHOLD
     C                   ENDIF                                                  TMHOLD
     C*
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1PGM         TMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C                   MOVEL     DSTIME        TMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGHR
     C*
     C*================================================================
     C* CLCAMT - CALCULATES BILL AMOUNT.
     C*================================================================
     C     CLCAMT        BEGSR
     C*** BEGIN ADD *** 06/08/99 **************************************
     C*
     C     *IN40         IFEQ      *OFF
     C*** END   ADD *** 06/08/99 **************************************
     C*
     C* CALCULATE ORIGINAL AMOUNT.
     C*
     C     TMMILE        MULT      TMMIRT        XXMIRT            9 2
     C*
     C     TMHRS         MULT      TMHRRT        XXHRRT            9 2
     C*
     C                   Z-ADD     TMMSCG        XXMSCG            9 2
     C*
     C     XXMIRT        ADD       XXHRRT        XXOAMT            9 2
     C                   ADD       XXMSCG        XXOAMT
     C*** BEGIN ADD *** 06/08/99 **************************************
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XXOAMT
     C*
     C                   ENDIF
     C*** END   ADD *** 06/08/99 **************************************
     C*
     C* SUBTRACT ORIGINAL AMOUNT FROM TOTAL AMOUNT
     C*
     C*** BEGIN DEL *** 06/08/99 **************************************
     C**********           SUB  XXOAMT    XXBAMT
     C*** END   DEL *** 06/08/99 **************************************
     C*** BEGIN ADD *** 06/08/99 **************************************
     C                   Z-ADD     *ZEROS        XXRAMT
     C*** END   ADD *** 06/08/99 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     @@MODE        IFNE      'E'
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C* CALCULATE REVISED AMOUNT.
     C*
     C     SCMILE        MULT      SCMIRT        XXMIRT
     C*
     C     SCHRS         MULT      SCHRRT        XXHRRT
     C*
     C                   Z-ADD     SCMSCG        XXMSCG
     C*
     C     XXMIRT        ADD       XXHRRT        XXRAMT            9 2
     C                   ADD       XXMSCG        XXRAMT
     C*
     C* ADD REVISED AMOUNT TO TOTAL AMOUNT.
     C*
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/13/98 **************************************
     C*** BEG ADD *** 12/11/98 ***************************************
     C*** BEGIN DEL *** 06/08/99 **************************************
     C********** ##PRVB    IFEQ 'Y'
     C**********           Z-ADDXXRAMT    XXBAMT
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           ADD  XXRAMT    XXBAMT
     C*** END   CHG *** 06/08/99 **************************************
     C**********           Z-ADD*ZEROS    ##TMBA
     C**********           Z-ADD*ZEROS    ##TMAA
     C**********           ELSE
     C*** END   DEL *** 09/07/99 **************************************
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   Z-ADD     XXOAMT        ##TMBA
     C                   Z-ADD     XXRAMT        ##TMAA
     C*** BEGIN DEL *** 06/08/99 **************************************
     C**********           ENDIF
     C*** END   DEL *** 06/08/99 **************************************
     C*** END ADD *** 12/11/98 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSVDT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           EXSR GETAMT
     C*** END   DEL *** 09/07/99 **************************************
     C                   EXSR      CLCAMT
     C                   EXSR      UPDATA
     C*** END   ADD *** 08/13/98 **************************************
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
     C                   MOVEA     '000000'      *IN(31)
     C*
     C* ACTIVITY DATE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSVDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* ACTIVITY DATE MUST BE GREATER THAN OR EQUAL TO FROM DATE.
     C*
     C*** BEGIN DEL *** 10/09/00 **************************************
     C********** *IN90     IFEQ *OFF
     C*
     C**********           Z-ADDSCSVDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXSVDT
     C*
     C********** XXSVDT    IFLT EOFRDT
     C*
     C**********           SETON                     7290
     C*
     C**********           ENDIF
     C*
     C**********           ENDIF
     C*** END   DEL *** 10/09/00 **************************************
     C*
     C* SERVICE CODE AND SEQ# MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCMSEQ        KKSEQ#
     C                   MOVEL     SCMSCD        KKMSCD
     C*
     C     MSKY          CHAIN     PMS02                              99
     C*
     C   99              SETON                                        7390
     C*** BEGIN ADD *** 02/23/00 **************************************
     C  N99SCSTAF        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  MSSTAF        SCSTAF
     C*
     C                   ENDIF
     C*** END   ADD *** 02/23/00 **************************************
     C*
     C                   ENDIF
     C*
     C* ACTIVITY DATE MUST BE LESS THAN OR EQUAL TO TO DATE.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVDT
     C*
     C     XXSVDT        IFGT      MSENDT
     C     MSENDT        ANDNE     *ZEROS
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** 'FB'      CHAINSFB01                99
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C                   Z-ADD     MSENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXENDT            8 0
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      XXENDT        ##DATE            8 0
     C*** BEGIN CHG *** 08/15/00 **************************************
     C**********           PARM FBDT04    ##DAYS  50
     C                   PARM      ##DT04        ##DAYS            5 0
     C*** END   CHG *** 08/15/00 **************************************
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1ENDT
     C*
     C     XXSVDT        IFGT      X1ENDT
     C*
     C*** BEG CHG *** 03/05/1999 **************************************
     C***************      SETON                     7490
     C                   SETON                                        7990
     C*** END CHG *** 03/05/1999 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/05/1999 **************************************
     C*
     C* Activity date must be > than case mgmt. service date
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVDT
     C*
     C     XXSVDT        IFLT      MSBGDT
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Activity date may be greater than case mgmt. service date
     C*   based on FBDT04
     C*
     C*          *IN90     IFEQ *OFF
     C*
     C*          MSENDT    IFGT 0
     C*                    Z-ADDMSENDT    SRDTI
     C*                    CALL 'SRCVTDAT'DTCYMD
     C*                    MOVELSRDTO     SRDATE  80
     C*                    Z-ADDFBDT04    SRFUTR  50
     C*                    CALL 'SRFUTR8'
     C*                    PARM           SRDATE
     C*                    PARM           SRFUTR
     C*                    PARM           SRRTCD  1
     C*                    Z-ADDSRDATE    SRDTI
     C*                    CALL 'SRCVTDAT'DTCYMD
     C*                    MOVELSRDTO     XXDATE
     C*
     C*          XXDATE    IFLT SCSVDT
     C*                    SETON                     7990
     C*                    ENDIF
     C*
     C*                    ENDIF
     C*
     C*                    ENDIF
     C*
     C*** END ADD *** 03/05/1999 **************************************
     C*
     C* Staff code must be Valid, if blank default to code in FAMSP.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCSTAF        IFNE      *BLANKS
     C*
     C                   MOVEL     SCSTAF        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7590
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     MSSTAF        SCSTAF
     C*
     C                   ENDIF
     C*
     C* Activity code must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCACT         KKKEY
     C*
     C     ATKY          CHAIN     PAT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *OFF
     C     SCDESC        ANDEQ     *BLANKS
     C*
     C                   MOVEL(P)  ATDESC        SCDESC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Professional/Non-professional Flag must be valid IFNE ' '.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCPNFL        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPNFL        KKTKEY
     C                   MOVEL(P)  'TMPN'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7790
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ATTYP2        SCPNFL
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C                   MOVEL(P)  SCPNFL        KKTKEY
     C                   MOVEL(P)  'TMPN'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCPNDS
     C   99              MOVEL(P)  *BLANKS       SCPNDS
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C* MEDICAL/VOCATIONAL MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *ON
     C*
     C     SCMVFL        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCMVFL        KKTKEY
     C                   MOVEL(P)  'MVFL'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        8090
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ATTYP1        SCMVFL
     C*
     C                   MOVEL(P)  SCMVFL        KKTKEY
     C                   MOVEL(P)  'MVFL'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCMVDS
     C   99              MOVEL(P)  *BLANKS       SCMVDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 12/19/00 **************************************
     C*
     C* MILEAGE RATE OVERIDE MUST BE "Y" OR " "
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFLG3        ANDNE     'Y'
     C     SCFLG3        ANDNE     ' '
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C* HOURLY RATE OVERIDE MUST BE "Y" OR " "
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFLG2        ANDNE     'Y'
     C     SCFLG2        ANDNE     ' '
     C*
     C                   SETON                                        8890
     C*
     C                   ENDIF
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C* Hourly rate if zero, defaults to value in group or sub.
     C*
     C                   Z-ADD     SCSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   Z-ADD     KKFND         K1FND
     C*
     C     FRKY1         SETGT     PFR01
     C     FRKY          READPE    PFR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SCHRRT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 12/19/00 **************************************
     C     SCFLG2        ANDEQ     *BLANKS
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C     SCPNFL        IFEQ      'P'
     C*
     C                   Z-ADD     FRPRT1        SCHRRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     FRNRT1        SCHRRT
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C* Mile rate if zero, defaults to value from FMFRP.
     C*
     C     SCMIRT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 12/19/00 **************************************
     C     SCFLG3        ANDEQ     *BLANKS
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C                   Z-ADD     FRMRT1        SCMIRT
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C   99SRKY1         SETGT     PSR01
     C   99SRKY          READPE    PSR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SCHRRT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 12/19/00 **************************************
     C     SCFLG2        ANDEQ     *BLANKS
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C     SCPNFL        IFEQ      'P'
     C*
     C                   Z-ADD     SRPRT1        SCHRRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SRNRT1        SCHRRT
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C* Mile rate if zero, defaults to value from FMSRP.
     C*
     C     SCMIRT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 12/19/00 **************************************
     C     SCFLG3        ANDEQ     *BLANKS
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C                   Z-ADD     SRMRT1        SCMIRT
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEG DEL *** 03/05/1999 **************************************
     C***********SCMILE    MULT SCMIRT    SCTMIA
     C***********SCHRS     MULT SCHRRT    SCTHRA
     C*** END DEL *** 03/05/1999 **************************************
     C*
     C* Calculate bill amounts.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   EXSR      CLCAMT
     C*
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
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*================================================================
     C* GETAMT - Get the original amount value.
     C*================================================================
     C********** GETAMT    BEGSR
     C*
     C* AMOUNT
     C*
     C**********           SELEC
     C*
     C********** EOSYS     WHEQ 'P'
     C**********           Z-ADDEOBAMT    XXBAMT
     C*
     C********** EOSYS     WHEQ 'N'
     C*** BEG CHG *** 12/11/98 ***************************************
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDEANAMT    XXBAMT
     C**********           ENDIF
     C*** END CHG *** 12/11/98 ***************************************
     C*
     C********** EOSYS     WHEQ 'U'
     C*** BEG CHG *** 12/11/98 ***************************************
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDEABAMT    XXBAMT
     C**********           ENDIF
     C*** END CHG *** 12/11/98 ***************************************
     C*
     C********** EOSYS     WHEQ 'H'
     C*** BEG CHG *** 12/11/98 ***************************************
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDEABAMT    XXBAMT
     C**********           ENDIF
     C*** END CHG *** 12/11/98 ***************************************
     C*
     C********** EOSYS     WHEQ 'M'
     C*** BEG CHG *** 12/11/98 ***************************************
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDEABAMT    XXBAMT
     C**********           ENDIF
     C*** END CHG *** 12/11/98 ***************************************
     C**********           ENDSL
     C*
     C**********           ENDSR
     C**********
     C*** END   DEL *** 09/07/99 **************************************
     C*================================================================
     C* KJDO - SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** BEGIN DEL *** 10/25/99 **************************************
     C**********           MOVEA'000000'  *IN,31
     C*** END   DEL *** 10/25/99 **************************************
     C*
     C* Case Mgmt service code and sequence number search.
     C*
     C*** BEG ADD *** 08/06/98 **************************************
     C*
     C     @@RRN3        IFEQ      *ZEROS                                       NOT FROM F0276R
     C*
     C*** END ADD *** 08/06/98 **************************************
     C     HFLD          IFEQ      'SCMSCD'
     C     HFLD          OREQ      'SCMSEQ'
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   CALL      'F0276R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PMS01                              99
     C*
     C                   MOVEL     MSMSCD        SCMSCD
     C                   MOVE      MSSEQ#        SCMSEQ
     C*** BEGIN ADD *** 02/23/00 **************************************
     C     SCSTAF        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  MSSTAF        SCSTAF
     C*
     C                   ENDIF
     C*** END   ADD *** 02/23/00 **************************************
     C*
     C                   MOVEL     MSMSCD        KKKEY
     C*
     C     MCKY          CHAIN     PMC01                              99
     C*
     C                   MOVEL     MCDESC        SCSCDS
     C*
     C*** BEGIN CHG *** 02/23/00 **************************************
     C**********           SETON                     32
     C                   SETON                                        33
     C*** END   CHG *** 02/23/00 **************************************
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** BEG ADD *** 08/06/98 **************************************
     C*
     C                   ENDIF                                                  RRN3 IFEQ
     C*** END ADD *** 08/06/98 **************************************
     C*
     C* Staff code search.
     C*
     C     HFLD          IFEQ      'SCSTAF'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCSTAF
     C                   MOVEL     UPUSNM        SCSTFN
     C                   SETON                                        33
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C* Activity code search.
     C*
     C     HFLD          IFEQ      'SCACT'
     C*
     C                   CALL      'N2315R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PAT01                              99
     C*
     C                   MOVEL     ATKEY         SCACT
     C                   SETON                                        34
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C* Professional/Non-professional flag search.
     C*
     C     HFLD          IFEQ      'SCPNFL'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TMPN'        ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        35
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCPNFL
     C                   MOVEL(P)  GTDESC        SCPNDS
     C                   SETON                                        36
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C* MEDICAL/VOCATIONAL flag search.
     C*
     C     HFLD          IFEQ      'SCMVFL'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'MVFL'        ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        36
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   SETON                                        37
     C                   MOVEL(P)  GTKEY         SCMVFL
     C                   MOVEL(P)  GTDESC        SCMVDS
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   EXSR      DATCVT
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEG ADD *** 08/06/98 **************************************
     C                   SETOFF                                       60
     C*** END ADD *** 08/06/98 **************************************
     C*
     C*
     C     *IN40         IFEQ      *ON
     C*** BEGIN ADD *** 07/24/98 **************************************
     C     HDMODE        ANDNE     'M'
     C*** END   ADD *** 07/24/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSVDT
     C*
     C     @@RRN3        IFNE      *ZEROS
     C*
     C*** BEG ADD *** 08/06/98 **************************************
     C                   SETON                                        60
     C*** END ADD *** 08/06/98 **************************************
     C*
     C     @@RRN3        CHAIN     PMS01                              99
     C*
     C                   MOVEL     MSMSCD        SCMSCD
     C                   MOVEL     MSSEQ#        SCMSEQ
     C*
     C*
     C                   MOVEL     SCMSCD        KKKEY
     C*
     C     MCKY          CHAIN     PMC01                              99
     C*
     C  N99              MOVEL(P)  MCDESC        SCSCDS
     C   99              MOVEL(P)  *BLANKS       SCSCDS
     C*
     C                   MOVEL     MSSTAF        SCSTAF
     C*
     C                   MOVEL     SCSTAF        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL     UPUSNM        SCSTFN
     C   99              MOVEL     *BLANKS       SCSTFN
     C                   SETON                                        33
     C                   SETOFF                                       31
     C*
     C                   ELSE                                                   @@RRN3 = 0
     C*
     C                   MOVEL     *BLANKS       SCMSCD
     C                   MOVEL     *BLANKS       SCSCDS
     C                   Z-ADD     *ZEROS        SCMSEQ
     C*
     C*
     C                   MOVEL     *BLANKS       SCSTAF
     C                   MOVEL     *BLANKS       SCSTFN
     C                   SETON                                        31
     C                   SETOFF                                       33
     C*
     C                   ENDIF                                                   @@RRN3 = 0
     C*
     C*** BEGIN ADD *** 07/24/98 **************************************
     C     @@CERT        IFEQ      *BLANKS
     C*
     C                   MOVEL     *BLANKS       HDMSSC
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  @@CERT        KKCERT
     C                   Z-ADD     @@EXT         KKEXT
     C*
     C     PCKY          CHAIN     PPC01                              99
     C*
     C                   MOVEL(P)  PCURSC        HDMSSC
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 07/24/98 **************************************
     C                   MOVEL     *BLANKS       SCACT
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCPNFL
     C                   MOVEL     *BLANKS       SCPNDS
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   MOVEL     *BLANKS       SCMVFL
     C                   MOVEL     *BLANKS       SCMVDS
     C                   MOVEL     'R'           SCHOLD
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     *ZEROS        SCMILE
     C                   Z-ADD     *ZEROS        SCHRS
     C                   Z-ADD     *ZEROS        SCMIRT
     C                   Z-ADD     *ZEROS        SCHRRT
     C                   Z-ADD     *ZEROS        SCMSCG
     C*** BEGIN UND *** 05/26/99 **************************************
     C*** BEG DEL *** 03/05/1999 **************************************
     C*** BEGIN ADD *** 06/29/98 **************************************
     C                   Z-ADD     *ZEROS        SCTMIA
     C                   Z-ADD     *ZEROS        SCTHRA
     C*** END   ADD *** 06/29/98 **************************************
     C*** END DEL *** 03/05/1999 **************************************
     C*** END   UND *** 05/26/99 **************************************
     C*** BEGIN ADD *** 12/19/00 **************************************
     C                   MOVEL     *BLANKS       SCFLG3
     C                   MOVEL     *BLANKS       SCFLG2
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C*** BEG ADD *** 03/05/1999 **************************************
     C                   MOVE      *HIVAL        KKSEQ#
     C     TMKY2         SETGT     NDTML1
     C     TMKY          READPE    NDTML1                                 99
     C  N99TMSEQ#        ADD       1             SCTSEQ            5 0
     C   99              Z-ADD     1             SCTSEQ            5 0
     C*** END ADD *** 03/05/1999 **************************************
     C*
     C                   ELSE
     C*
     C*
     C                   Z-ADD     TMSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSVDT
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   MOVEL(P)  TM10A1        HDMSSC            4
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   MOVEL     TMMSCD        SCMSCD
     C*
     C                   MOVEL     SCMSCD        KKKEY
     C*
     C     MCKY          CHAIN     PMC01                              99
     C*
     C  N99              MOVEL(P)  MCDESC        SCSCDS
     C   99              MOVEL(P)  *BLANKS       SCSCDS
     C*
     C                   Z-ADD     TMMSEQ        SCMSEQ
     C                   Z-ADD     TMSEQ#        SCTSEQ
     C*
     C                   MOVEL     TMSTAF        SCSTAF
     C                   MOVEL     SCSTAF        KKSTAF
     C*
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL     UPUSNM        SCSTFN
     C   99              MOVEL     *BLANKS       SCSTFN
     C*
     C                   MOVEL     TMACT         SCACT
     C*
     C                   MOVEL     TMDESC        SCDESC
     C                   MOVEL     TMPNFL        SCPNFL
     C*
     C                   MOVEL     'TMPN'        KKTBID
     C                   MOVEL(P)  SCPNFL        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL     GTDESC        SCPNDS
     C   99              MOVEL     *BLANKS       SCPNDS
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   MOVEL     TMMVFL        SCMVFL
     C*
     C                   MOVEL     'MVFL'        KKTBID
     C                   MOVEL(P)  SCMVFL        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL     GTDESC        SCMVDS
     C   99              MOVEL     *BLANKS       SCMVDS
     C*
     C                   MOVEL     TMHOLD        SCHOLD
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 12/19/00 **************************************
     C                   MOVEL     TMFLG3        SCFLG3
     C                   MOVEL     TMFLG2        SCFLG2
     C*** END   ADD *** 12/19/00 **************************************
     C*
     C                   Z-ADD     TMMILE        SCMILE
     C                   Z-ADD     TMHRS         SCHRS
     C                   Z-ADD     TMMIRT        SCMIRT
     C                   Z-ADD     TMHRRT        SCHRRT
     C                   Z-ADD     TMMSCG        SCMSCG
     C*** BEGIN UND *** 05/26/99 **************************************
     C*** BEG DEL *** 03/05/99 ****************************************
     C*** BEGIN ADD *** 06/29/98 **************************************
     C     SCMILE        MULT      SCMIRT        SCTMIA
     C     SCHRS         MULT      SCHRRT        SCTHRA
     C*** END   ADD *** 06/29/98 **************************************
     C*** END DEL *** 03/05/99 ****************************************
     C*** END   UND *** 05/26/99 **************************************
     C*** BEGIN DEL *** 08/13/98 **************************************
     C*
     C* Get original amount
     C*
     C**********           EXSR GETAMT
     C*** END   DEL *** 08/13/98 **************************************
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*
     C* Get original amount
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           EXSR GETAMT
     C*** END   DEL *** 09/07/99 **************************************
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     EOCO#         TMCO#
     C                   Z-ADD     EOSUB#        TMSUB#
     C                   Z-ADD     EOFND         TMFND
     C                   Z-ADD     EOFYR         TMFYR
     C                   Z-ADD     EOCASE        TMCASE
     C                   Z-ADD     EOEOB#        TMEOB#
     C*** BEGIN CHG *** 05/19/00 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C**********           Z-ADDEORCN     TMRCN
     C                   Z-ADD     @@RCN         TMRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*** END   CHG *** 05/19/00 **************************************
     C*** BEGIN ADD *** 10/05/99 **************************************
     C                   MOVEL     EOSYS         TMSYS
     C*** END   ADD *** 10/05/99 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C   40              Z-ADD     SCTSEQ        TMSEQ#
     C                   MOVEL(P)  HDMSSC        TM10A1
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   Z-ADD     SCSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMSVDT
     C*
     C                   MOVEL     SCMSCD        TMMSCD
     C                   Z-ADD     SCMSEQ        TMMSEQ
     C                   MOVEL     SCSTAF        TMSTAF
     C                   MOVEL     SCACT         TMACT
     C                   MOVEL     SCDESC        TMDESC
     C                   MOVEL     SCPNFL        TMPNFL
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   MOVEL     SCMVFL        TMMVFL
     C                   MOVEL     SCHOLD        TMHOLD
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     SCMILE        TMMILE
     C                   Z-ADD     SCHRS         TMHRS
     C                   Z-ADD     SCMIRT        TMMIRT
     C                   Z-ADD     SCHRRT        TMHRRT
     C                   Z-ADD     SCMSCG        TMMSCG
     C*** BEGIN ADD *** 12/19/00 **************************************
     C                   MOVEL     SCFLG3        TMFLG3
     C                   MOVEL     SCFLG2        TMFLG2
     C*** END   ADD *** 12/19/00 **************************************
     C*** BEGIN ADD *** 01/11/00 **************************************
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EABINV        TMINV#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        TMINV#
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/11/00 **************************************
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
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   MOVEA     '0000000'     *IN(31)
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*** BEG ADD *** 03/05/1999 **************************************
     C     @@MODE        COMP      'C'                                    41
     C*** END ADD *** 03/05/1999 **************************************
     C*
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     HDMODE        IFEQ      'M'
     C                   SETON                                        40
     C                   ENDIF
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C*** BEG CHG *** 12/11/98 **************************************
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C**********           PARM *BLANK    ##WCIF  1        WC INST
     C**********           PARM *BLANK    ##MCIF  1        MC INST
     C**********           PARM *BLANK    ##WCIN  1        WC INTERFACE
     C**********           PARM *BLANK    ##AR    1        MC AR INST
     C**********           PARM *BLANK    ##NTWK  1        NETWORK INST
     C**********           PARM *BLANK    ##UR    1        PRECERT INST
     C**********           PARM *BLANK    ##MED   1        MEDMGMT INST
     C**********           PARM *BLANK    ##EDI   1        EDI INST
     C**********           PARM *BLANK    ##WCDK  1        WC DARTS KEY
     C*** END CHG *** 12/11/98 **************************************
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C                   PARM      *ZEROS        ##DT01            7 0
     C                   PARM      *ZEROS        ##DT02            7 0
     C                   PARM      *ZEROS        ##DT03            7 0
     C                   PARM      *ZEROS        ##DT04            7 0
     C                   PARM      *ZEROS        ##DT05            7 0
     C*** END   ADD *** 08/15/00 **************************************
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C* GET FAEOP RECORD.
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*** BEGIN ADD *** 01/17/99 **************************************
     C                   Z-ADD     EORCN         KKRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C* LOAD ADDITIONAL HEADER VALUES
     C*
     C                   MOVEL     EOSYS         SCSYS
     C                   MOVEL(P)  EOSYS         KKTKEY
     C                   MOVEL(P)  'SYS'         KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTDESC        SCSYNM
     C*
     C                   MOVEL     EOEOB#        SCEOB#
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGHR
     C                   RETURN
     C                   ENDSL
     C*** END   ADD *** 10/25/99 **************************************
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1PGM         TMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C                   MOVEL     DSTIME        TMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      UPDATA
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATA - Updates AMOUNTS
     C*================================================================
     C     UPDATA        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*** BEG ADD *** 12/11/98 ***************************************
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C*** BEGIN CHG *** 06/08/99 **************************************
     C**********           MOVE *BLANKS   KKWRUP
     C                   MOVE      *BLANKS       ##WRUP
     C*** END   CHG *** 06/08/99 **************************************
     C*** END ADD *** 12/11/98 ***************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'U'           XXFAEA
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEGIN ADD *** 05/19/00 **************************************
     C**********           PARM *ZEROS    ##RCN   30       RECON #
     C                   PARM      @@RCN         ##RCN             3 0          RECON #
     C*** END   ADD *** 05/19/00 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      XXBAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM                    ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ELSE
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   SELECT
     C*
     C     SCSYS         WHENEQ    'P'
     C*** BEG CHG *** 12/11/98 ****************************************
     C                   MOVE      'U'           XXFAEO
     C                   MOVE      'U'           XXFAE1
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM                    ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      XXBAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM                    ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 12/11/98 ****************************************
     C*
     C     SCSYS         WHENEQ    'N'
     C*** BEG CHG *** 12/11/98 ****************************************
     C                   MOVE      'U'           XXNATR
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM                    ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM      XXBAMT        ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM                    ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 12/11/98 ****************************************
     C*
     C     SCSYS         WHENEQ    'U'
     C*** BEG CHG *** 12/11/98 ****************************************
     C                   MOVE      'U'           XXNAPB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM                    ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      XXBAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM                    ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 12/11/98 ****************************************
     C*
     C     EOSYS         WHENEQ    'H'
     C*** BEG CHG *** 12/11/98 ****************************************
     C                   MOVE      'U'           XXFAHB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM                    ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      XXBAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM                    ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 12/11/98 ****************************************
     C*
     C     EOSYS         WHENEQ    'M'
     C*** BEG CHG *** 12/11/98 ****************************************
     C                   MOVE      'U'           XXNAMB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM                    ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      XXBAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM                    ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 12/11/98 ****************************************
     C*
     C                   ENDSL
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*** BEGIN ADD *** 01/17/99 **************************************
     C                   Z-ADD     EORCN         KKRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*********            MOVE *HIVAL    KKSEQ#
     C*
     C*********  TMKY2     SETGTPTM01
     C*********  TMKY      REDPEPTM01                    99
     C*
     C*********  *IN99     IFEQ *OFF
     C*
     C*********  TMSEQ#    ADD  1         HDSEQ#
     C*
     C*********            ELSE
     C*
     C*********            Z-ADD1         HDSEQ#
     C*
     C*********            ENDIF
     C*
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1USER        TMIUSR
     C                   MOVEL     Q1PGM         TMPGM
     C                   MOVEL     Q1PGM         TMIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C                   MOVEL     SRDTO         TMIDT
     C*
     C                   MOVEL     DSTIME        TMTIME
     C                   MOVEL     DSTIME        TMITIM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      NDTMP
     C*
     C                   EXSR      UPDATA
     C*
     C                   ENDSR
