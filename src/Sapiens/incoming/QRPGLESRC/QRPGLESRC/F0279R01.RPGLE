     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0279R01                                      *
     H* DESCRIPTION: WORK WITH TREATMENT ICD9 MAINTENANCE      *
     H**********************************************************
     H* INDICATORS:                                            *
     H**********************************************************
     H* REVISIONS:                                             *
     H*                                                        *
     H* 05/27/99 C:62 PROJECT: MC145                           *
     H*               IF PRIMARY CODE 'Y' THEN DATE IS REQUIRED*
     H*                                                        *
     H* 06/12/98 C:94 PROJECT: MC145                           *
     H*               CHANGED ERROR MESSAGE AND WHEN ERROR     *
     H*               MESSAGE WILL COME ON.                    *
     H*                                                        *
     H* 02/09/98 C:62 PROJECT: MC145                           *
     H*               1) REDUCED PARM LIST BY INCLUDING RRN    *
     H*                  OF FATRP.                             *
     H*               2) PUT IN PLACE WW/MAINT STANDARDS.      *
     H*                                                        *
     H* 02/01/98 C:62 PROJECT: MC145                           *
     H*               1) FA703R WAS ADDED.                     *
     H*               2) SA001R WAS ADDED.                     *
     H*               3) MAINTENACE FIELDS ADDED TO UPD AND ADD*
     H*               4) TWO LINE CLAIM HEADERS ADDED.         *
     H*                                                        *
     H* 01/30/98 C:62 PROJECT: MC145                           *
     H*               1) ADDED NEW SEARCH                      *
     H*                  FROM F0510R TO F2010R                 *
     H*               2) NEW HEADER ADDED.
     H*                                                        *
     H*  01/28/98 C:56  Projedt: 87MDP                         *
     H*                 Prevent entry of duplicate ICD9 code   *
     H*                 on same treatment plan                 *
     H*                                                        *
     H* 09/23/97 C:59 PROJECT: SUPPORT MC087/#8828             *
     H*               Calling incorrect program on darts       *
     H*               correct is F0270R, Change Both keys to   *
     H*               be blank instead of zeros.  Causing      *
     H*               problem in the file.                     *
     H*                                                        *
     H*  08/25/97 C:31  Project: 87MAZ  CO# 48                 *
     H*                 Remove case management fields          *
     H*                 (Compile only)                         *
     H*                                                        *
     H*  09/17/97 C:59  Project:                               *
     H*                 Update to live code - need to reset    *
     H*                 diagnosis codes on changes, not just add
     H**********************************************************
     H*
     FF0279F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFATIP     UF A E             DISK
     F                                     RENAME(PTI01:DBRCDF)
     FFATIL     IF   E           K DISK
     FFATRP     IF   E             DISK
     FFMDIL     IF   E           K DISK
     F*** BEGIN ADD *** 01/28/98 **************************************
     FFATIL2    IF   E           K DISK
     F                                     RENAME(PTI01:PTI02)
      /COPY SCOPYMT01D
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Treatment Plan ICD9 -
     D                                     Code Maintenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     F*** END ADD *** 01/28/98 ****************************************

     I*
     I*
     I***PY SCOPYWW01E
     I*
     I*** BEGIN ADD *** 01/28/98 **************************************
     IPTI02
     I              TICO#                       T2CO#
     I              TISUB#                      T2SUB#
     I              TIFND                       T2FND
     I              TIFYR                       T2FYR
     I              TICASE                      T2CASE
     I              TITX#                       T2TX#
     I              TIICD9                      T2ICD9
     I              TILOST                      T2LOST
     I              TIPRIM                      T2PRIM
     I              TIBGDT                      T2BGDT
     I              TISTAT                      T2STAT
     I              TIUSER                      T2USER
     I              TIPGM                       T2PGM
     I              TITIME                      T2TIME
     I              TIDT                        T2DT
     I              TIIUSR                      T2IUSR
     I              TIIPGM                      T2IPGM
     I              TIITIM                      T2ITIM
     I              TIIDT                       T2IDT
     I*** END ADD *** 01/28/98 ****************************************
     I*
     I*  USED TO FIND LAST TREATMENT PLAN #
     I*
     I*
     I***PY SCOPYMT01I
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          RRN FATIP
     C                   PARM                    @@RRN2            9 0          RRN FATRP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C* MAKE HEADERS PARM LIST
     C*
     C     MKHDPL        PLIST
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     TIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C                   KFLD                    KKICD9
     C*
     C     TIKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     TIKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*** END ADD *** 01/28/98 ****************************************
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*
     C     *LIKE         DEFINE    TICO#         ##CO#
     C     *LIKE         DEFINE    TISUB#        ##SUB#
     C     *LIKE         DEFINE    TIFND         ##FND
     C     *LIKE         DEFINE    TIFYR         ##FYR
     C     *LIKE         DEFINE    TICASE        ##CASE
     C     *LIKE         DEFINE    TICO#         KKCO#
     C     *LIKE         DEFINE    TISUB#        KKSUB#
     C     *LIKE         DEFINE    TIFND         KKFND
     C     *LIKE         DEFINE    TIFYR         KKFYR
     C     *LIKE         DEFINE    TICASE        KKCASE
     C     *LIKE         DEFINE    TIICD9        KKICD9
     C     *LIKE         DEFINE    TILOST        KKLOST
     C     *LIKE         DEFINE    TITX#         KKTX#
     C     *LIKE         DEFINE    TIICD9        HLICD9
     C     *LIKE         DEFINE    TILOST        HLLOST
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCIBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCIBDT
     C*
     C                   ENDSR
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
     C* SETTING OFF THE PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00'          *IN(30)
     C*
     C*********  *IN40     IFEQ *ON                        DEL 09/17/97
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C*********            ENDIF                           DEL 09/17/97
     C*
     C* CHECK IF DIAGNOSIS CODE IS BLANK
     C*
     C  N90SCICD9        IFEQ      *BLANKS
     C   99              SETON                                        9073          ERROR
     C                   ENDIF
     C*
     C* CHECK FOR VALID LOST TYPE
     C*
     C  N90SCLOST        IFNE      'D'
     C     SCLOST        ANDNE     'O'
     C                   SETON                                        9074
     C                   ENDIF
     C*
     C* CHECK FOR VALID ICD9 CODE
     C*
     C  N90DIKY          CHAIN     PDI01                              99
     C   99              SETON                                        9073          ERROR
     C  N99              MOVEL(P)  DIDSC1        SCDESC
     C*
     C* CHECK PRIM ICD9 CODE
     C*
     C  N90SCPRIM        IFNE      'Y'
     C     SCPRIM        ANDNE     'N'
     C*
     C                   SETON                                        9075
     C*
     C                   ENDIF
     C*
     C*
     C* CHECK PRIM ICD9 START DATE
     C*
     C  N90SCPRIM        IFEQ      'Y'
     C*
     C     TIKY2         CHAIN     PTI01                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C     TIPRIM        IFEQ      'Y'
     C     SCPRIM        ANDNE     TIPRIM
     C*
     C                   SETON                                        9078
     C                   LEAVE
     C*
     C                   ENDIF                                                  TIPRIM EQ 'Y'
     C*
     C     TIKY2         READE     PTI01                                  99
     C*
     C                   ENDDO                                                  *IN90 DOEQ *OFF
     C*
     C                   ENDIF                                                  SCPRIM EQ 'Y'
     C*
     C  N90SCPRIM        IFEQ      'Y'
     C*
     C     SCIBDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCIBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXIBDT            7 0
     C*** BEGIN ADD *** 05/27/99 **************************************
     C                   ELSE
     C*
     C                   SETON                                        7790
     C*** END   ADD *** 05/27/99 **************************************
     C*
     C                   ENDIF                                                  SCIBDT EQ 0
     C*
     C*** BEGIN ADD *** 05/27/99 **************************************
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT            7 0
     C*** END   ADD *** 05/27/99 **************************************
     C*
     C*** BEG CHG *** 06/12/98 ***************************************
     C*** BEGIN CHG *** 05/27/99 **************************************
     C********** XXIBDT    IFNE *ZEROS
     C  N90XXIBDT        IFNE      *ZEROS
     C*** END   CHG *** 05/27/99 **************************************
     C***        XXIBDT    IFEQ *ZEROS
     C***        XXIBDT    ORLT TRBGDT
     C     SCENDT        ANDNE     *ZEROS
     C*** BEGIN ADD *** 05/27/99 **************************************
     C     XXIBDT        ANDGT     XXENDT
     C     XXIBDT        ORNE      *ZEROS
     C     XXIBDT        ANDLT     XXBGDT
     C*** END   ADD *** 05/27/99 **************************************
     C****       XXIBDT    ORGT TRENDT
     C*** END CHG *** 06/12/98 ***************************************
     C*
     C                   SETON                                        9076
     C*
     C                   ENDIF                                                  XXIBDT EQ 0
     C*
     C                   ELSE                                                   SCPRIM
     C*
     C  N90SCIBDT        IFNE      *ZEROS
     C*
     C                   SETON                                        9078
     C*
     C                   ENDIF                                                  END NE SCIBDT
     C*
     C                   ENDIF                                                  SCPRIM EQ 'Y'
     C*
     C  N90SCENDT        IFNE      *ZEROS
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9077
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*BLANK
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C  N90@@MODE        IFEQ      'A'
     C*
     C     TIKY3         CHAIN     PTI02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        9071
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   ENDIF                                                  @@MODE IFEQ A
     C*
     C  N90@@MODE        IFEQ      'C'
     C*
     C     HLICD9        IFNE      SCICD9
     C     HLLOST        ORNE      SCLOST
     C*
     C     TIKY3         CHAIN     PTI02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF                                                  *IN99 IFEQ *
     C*
     C                   ENDIF                                                  HLICD9 IFNE
     C*
     C                   ENDIF                                                  @@MODE IFEQ
     C*** END ADD *** 01/28/98 ****************************************
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* SETTING OFF THE PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00'          *IN(30)
     C*
     C                   EXSR      DATCVT
     C*
     C     HFLD          IFEQ      'SCICD9'
     C*
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##ICD9            5
     C                   PARM      *BLANKS       ##LOST            1
     C*
     C     *INKL         IFEQ      *ON
     C     ##ICD9        OREQ      *BLANKS
     C*
     C                   SETON                                        3090
     C*
     C                   ELSE
     C*
     C                   MOVEL     ##ICD9        SCICD9
     C                   MOVEL     ##LOST        SCLOST
     C                   MOVEL     ##LOST        KKLOST
     C                   MOVEL     ##ICD9        KKICD9
     C                   SETON                                        3190
     C*
     C     DIKY          CHAIN     PDI01                              99
     C*
     C   99              MOVEL(P)  DIDSC1        SCDESC
     C*
     C                   ENDIF                                                  *INKL *ON
     C*
     C                   ENDIF                                                  HFLD EQ SCICD9
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   EXSR      DATCVT
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      TRCO#         #1CO#
     C                   MOVE      TRSUB#        #1SUB#
     C                   MOVE      TRFND         #1FND
     C                   MOVE      TRFYR         #1FYR
     C                   MOVE      TRCASE        #1CASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      *BLANKS       SCICD9
     C                   Z-ADD     *ZEROS        SCIBDT
     C                   MOVE      *BLANK        SCLOST
     C                   MOVE      *BLANK        SCPRIM
     C                   MOVE      *BLANKS       SCDESC
     C*
     C                   ELSE
     C*
     C                   MOVEL     TIICD9        SCICD9
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           Z-ADDTIBGDT    SCIBDT
     C**********           Z-ADDSCIBDT    SRDTI
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   Z-ADD     TIBGDT        SRDTI
     C*** END   CHG *** 12/02/97 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIBDT
     C                   MOVE      TILOST        SCLOST
     C                   MOVE      TIPRIM        SCPRIM
     C*
     C                   MOVE      TILOST        KKLOST
     C                   MOVE      TIICD9        KKICD9
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDESC
     C*
     C                   ENDIF                                                  END *IN40
     C*
     C*** BEGIN ADD *** 01/28/98 *************************************
     C     @@MODE        IFEQ      'C'
     C                   MOVEL(P)  TIICD9        HLICD9
     C                   MOVE(P)   TILOST        HLLOST
     C                   ENDIF
     C*** END ADD *** 01/28/98 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      TRCO#         TICO#
     C                   MOVE      TRSUB#        TISUB#
     C                   MOVE      TRFND         TIFND
     C                   MOVE      TRFYR         TIFYR
     C                   MOVE      TRCASE        TICASE
     C                   MOVE      TRTX#         TITX#
     C                   MOVE      SCICD9        TIICD9
     C                   Z-ADD     SCIBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVELSRDTO     XXDATE  70
     C**********           Z-ADDXXDATE    TIBGDT
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         TIBGDT
     C*** END   CHG *** 12/02/97 **************************************
     C                   MOVE      SCLOST        TILOST
     C                   MOVE      SCPRIM        TIPRIM
     C                   MOVE      'A'           TISTAT
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
     C     *INKJ         CASEQ     *ON           KJDO                           F10=SEARCH
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
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
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C*
     C* MOVE VALUES INTO KEY FIELDS
     C*
     C     @@RRN2        CHAIN     PTR01                              99
     C*
     C                   MOVE      TRCO#         KKCO#
     C                   MOVE      TRSUB#        KKSUB#
     C                   MOVE      TRFND         KKFND
     C                   MOVE      TRFYR         KKFYR
     C                   MOVE      TRCASE        KKCASE
     C                   MOVE      TRTX#         KKTX#
     C*
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1            WC INST
     C                   PARM      ' '           ##MCIF            1            MC INST
     C                   PARM      ' '           ##WCIN            1            WC INTERFACE
     C                   PARM      ' '           ##AR              1            MC AR INST
     C                   PARM      ' '           ##NTWK            1            NETWORK INST
     C                   PARM      ' '           ##UR              1            PRECERT INST
     C                   PARM      ' '           ##MED             1            MEDMGMT INST
     C                   PARM      ' '           ##EDI             1            EDI INST
     C                   PARM      ' '           ##WCDK            1            WC DARTS KEY
     C*
     C* CREATE HEADERS USING FA702R
     C*
     C                   MOVE      TRCO#         #1CO#
     C                   MOVE      TRSUB#        #1SUB#
     C                   MOVE      TRFND         #1FND
     C                   MOVE      TRFYR         #1FYR
     C                   MOVE      TRCASE        #1CASE
     C*
     C                   CALL      'FA702R'      MKHDPL
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C* N99                Z-ADDTRBGDT    SCBGDT
     C**********           Z-ADDSCBGDT    SRDTI
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     TRBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C                   ENDIF                                                  IN99 = OFF
     C*** END   CHG *** 12/02/97 **************************************
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C* N99                Z-ADDTRENDT    SCENDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     TRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENDT
     C                   ENDIF                                                  IN99 = OFF
     C*** END   CHG *** 12/02/97 **************************************
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
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        TIUSER
     C                   MOVEL     Q1PGM         TIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TIDT
     C*
     C                   MOVEL     DSTIME        TITIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        TIIUSR
     C                   MOVEL     Q1PGM         TIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      DSTIME        TIITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TIIDT
     C*
     C                   MOVEL     Q1USER        TIUSER
     C                   MOVEL     Q1PGM         TIPGM
     C                   MOVEL     Q1PGM         TIIPGM
     C                   MOVE      DSTIME        TITIME
     C                   MOVEL     SRDTO         TIDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FATIP
     C                   MOVE      '1'           OUTFLG
     C*
     C                   ENDSR
