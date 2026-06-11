     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0463RFL - Work With EOMBs Detail Information            *
     H*****************************************************************
     H* F0463RFL - Work With EOMBs Detail Information - Florida       *
     H*                                                               *
     H*****************************************************************
     H* CREATED ON 08/08/2003 BY C:62 FOR PROJECT FL045
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 10/28/10  C:257   PROJECT: FL087                              *
     H*                    Changes to Medical EDI revision E          *
     H*                  - Remove Validation for HCPCS codes.-        *
     H*
     H* 03/11/10  C:227  PROJECT: FL083
     H*                  - ADD 4 NEW MODIFIER FIELDS
     H*
     H* 09/20/07  C:079  PROJECT: 10MJD
     H*                  - FIX SEARCHES/EDITS ON REV/HCPC FIELDS
     H*
     H* 10/17/06  C:181  PROJECT: 10MJD
     H*                  - ADD FIELD SCUNTS; ALLOW SCRVCD AND
     H*                    SCRATE TO BE UPDATED AND EDITED.
     H*
     H*  04/23/02 C:79 Support: MC048 32199                           *
     H*                1. Use billed code fields instead of Paid code.*
     H*                Note: Make sure FIX32199 is run prior to instal*
     H*                2. Chain to correct record in FDDUP            *
     H*                                                               *
     H*****************************************************************
     F*** BEGIN CHG *** 08/08/03 **************************************
     F**********F0463F  CF  E                    WORKSTN
     FF0463FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*** END   CHG *** 08/08/03 **************************************
     F*
     F*  Physical file definition (for maintaining record)
     FFDDUP     UF A E             DISK
     F                                     RENAME(PDU01:DBRCDF)
     FFDDUL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFAEOP     IF   E             DISK
     FFDCAP     IF   E             DISK
     FFAEUL     IF   E           K DISK
     F***FTABL   IF  E           K        DISK
     F***         PRC01                             KIGNORE
     F***         PPS01                             KIGNORE
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F* Service Code File
     FFMSVL1    IF   E           K DISK
     F                                     INFDS(DBFSV)
     F*** BEG ADD *** 10/17/06 ****************************************
     FFDDEL     IF   E           K DISK
     FFDDEP     O    E             DISK
     F                                     RENAME(PDE01:PDE02)
     F*** BEG DEL *** 09/20/07 ****************************************
     F**** FMPRL   IF  E           K        DISK
     F****                                          KINFDS DBFPR
     F*** END DEL *** 09/20/07 ****************************************
     F*** END ADD *** 10/17/06 ****************************************
      /COPY SCOPYMT01D
     D*** BEGIN ADD *** 08/08/03 **************************************
     D BT              S              1    DIM(3)
     D CPT             S              1    DIM(5)
     D*** END   ADD *** 08/08/03 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3
     D  K1HCPC                 4      8
     D  K1UNTS                 9     11
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Detail - Data Subm C-
     D                                     hgs')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** BEG ADD *** 10/17/06 ****************************************
     D NT1             C                   CONST('Paid code ')
     D NT3             C                   CONST(' was changed for EDI-
     D                                      correction')
     D*** END ADD *** 10/17/06 ****************************************
     D*** BEG DEL *** 09/20/07 ****************************************
     D***** DBFPR       DS
     D*****                               B 397 4000PRRRN
     D*** END DEL *** 09/20/07 ****************************************
     D DBFSV           DS
     D  SVRRN                397    400B 0
     D*** BEG ADD *** 10/17/06 ****************************************
     D                 DS
     D  XXNOT1                 1     55
     D  XXNT1                  1     10
     D  XXNT2                 11     19
     D  XXNT3                 20     55

     I*** END ADD *** 10/17/06 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDHP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN4            9 0          FDCAP
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
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFDAT
     C*
     C     RCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     DIKY          KLIST
     C                   KFLD                    XXDIA
     C*
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  Key List for Header File
     C*
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  Key List for Detail File
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEG ADD *** 04/23/02 ****************************************
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 04/23/02 ****************************************
     C*
     C     SVKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    K1CMSV
     C                   KFLD                    K1EFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    K1CMSV
     C*** BEG ADD *** 10/17/06 ****************************************
     C*
     C     DEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKSEQ#
     C*** END ADD *** 10/17/06 ****************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DUCO#         KKCO#
     C     *LIKE         DEFINE    DUSUB#        KKSUB#
     C     *LIKE         DEFINE    DUFND         KKFND
     C     *LIKE         DEFINE    DUFYR         KKFYR
     C     *LIKE         DEFINE    DUCASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    DUSS#         KKSS#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EOCNTL        QQCNTL
     C     *LIKE         DEFINE    CACPT4        HLCPT4
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    SVEFDT        K1EFDT
     C*** BEG ADD *** 04/23/02 ****************************************
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*** END ADD *** 04/23/02 ****************************************
     C*** BEG ADD *** 10/17/06 ****************************************
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    SCRVCP        HLRVCP
     C     *LIKE         DEFINE    DUPOS         XXRVCD
     C     *LIKE         DEFINE    SCRATP        HLRATP
     C*** END ADD *** 10/17/06 ****************************************
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
     C* REVENUE CODES AND CPT/HCPCS CANNOT BE EMPTY
     C*
     C  N90SCRVCD        IFEQ      *ZEROS
     C     SCRATE        ANDEQ     *BLANKS
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRVCD = *ZEROS
     C*** BEG ADD *** 10/17/06 ****************************************
     C*
     C  N90SCRVCP        IFEQ      *ZEROS
     C*** BEG ADD *** 09/20/07 ****************************************
     C     SCRATP        ANDEQ     *BLANKS
     C*** END ADD *** 09/20/07 ****************************************
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRVCP = *ZEROS
     C*
     C*** BEG DEL *** 09/20/07 ****************************************
     C**N90      SCRATP    IFEQ *BLANKS
     C****                 SETON                     9080
     C****                 GOTO CK1END
     C****                 ENDIF                           SCRATP = *BLANKS
     C*** END DEL *** 09/20/07 ****************************************
     C*** END ADD *** 10/17/06 ****************************************
     C*** BEGIN ADD *** 08/08/03 **************************************
     C*
     C* MUST BE VALID BILLED REVENUE CODE
     C*
     C                   MOVE      SCRVCD        K1RVCD
     C                   MOVE      *BLANKS       K1HCPC
     C                   MOVE      *ZEROS        K1UNTS
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C*
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF
     C     SVSTAT        ANDEQ     'A'
     C                   MOVEL(P)  SVDESC        SCDSC1
     C                   ELSE
     C                   MOVE      SCRATE        K1HCPC
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        SCDSC1
     C                   ELSE
     C                   SETON                                        7890
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* VALIDATE THE HCPC CODE
     C*
     C                   MOVEA     EUBLTY        BT
     C****BEGIN CHG *** 10/28/10 **************************************
     C*******    BT,2      IFEQ '3'
     C*******    BT,2      OREQ '4'
     C*******    SCRATE    ORNE *BLANKS
     C     SCRATE        IFNE      *BLANKS
     C*** END   CHG *** 10/28/10 **************************************
     C                   MOVEA     SCRATE        CPT
     C*
     C     1             DO        5             X                 2 0
     C*
     C     X             IFEQ      1
     C     CPT(X)        IFLT      'A'
     C     CPT(X)        ORGT      'Z'
     C     CPT(X)        IFLT      '0'
     C     CPT(X)        ORGT      '9'
     C                   SETON                                        7990
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CPT(X)        IFLT      '0'
     C     CPT(X)        ORGT      '9'
     C                   SETON                                        7990
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C*** END   ADD *** 08/08/03 **************************************
     C*
     C*** BEGIN ADD *** 10/17/06 **************************************
     C*
     C*MUST BE VALID PAID REVENUE CODE
     C*
     C*** BEG DEL *** 09/20/07 ****************************************
     C*******              MOVE SCRVCP    K1RVCD
     C*******              MOVE *BLANKS   K1HCPC
     C*******              MOVE *ZEROS    K1UNTS
     C*******
     C*******              Z-ADDSCFDAT    SRDTI
     C*******              CALL 'SRCVTDAT'DTCYMD
     C*******              MOVELSRDTO     K1EFDT
     C*******
     C*******    SVKY1     SETGTPSV01
     C*******    SVKY2     REDPEPSV01                    99
     C*******    *IN99     IFEQ *OFF
     C*******    SVSTAT    ANDEQ'A'
     C*******              MOVELSVDESC    SCDSCP    P
     C*******              ELSE
     C*******              MOVE SCRATP    K1HCPC
     C*******    SVKY1     SETGTPSV01
     C*******    SVKY2     REDPEPSV01                    99
     C*******
     C*******    *IN99     IFEQ *OFF
     C*******              MOVELSVDESC    SCDSCP    P
     C*******              ELSE
     C*******              SETON                     8290
     C*******              GOTO CK1END
     C*******              ENDIF
     C*******              ENDIF                           *IN99 = *OFF
     C*******
     C*** END DEL *** 09/20/07 ****************************************
     C*
     C*VALIDATE THE HCPC CODE
     C*
     C                   MOVEA     EUBLTY        BT
     C****BEGIN CHG *** 10/28/10 **************************************
     C*******    BT,2      IFEQ '3'
     C*******    BT,2      OREQ '4'
     C*******    SCRATE    ORNE *BLANKS
     C*******              MOVEASCRATE    CPT
     C     SCRATP        IFNE      *BLANKS
     C                   MOVEA     SCRATP        CPT
     C****END   CHG *** 10/28/10 **************************************
     C*
     C     1             DO        5             X                 2 0
     C*
     C     X             IFEQ      1
     C     CPT(X)        IFLT      'A'
     C     CPT(X)        ORGT      'Z'
     C     CPT(X)        IFLT      '0'
     C     CPT(X)        ORGT      '9'
     C                   SETON                                        8390
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CPT(X)        IFLT      '0'
     C     CPT(X)        ORGT      '9'
     C                   SETON                                        8390
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C*** END   ADD *** 10/17/06 **************************************
     C     CK1END        ENDSR
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
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* REVENUE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRVCD'
     C                   MOVEL     SCRVCD        HLRVCD            3 0
     C                   MOVEL     SCRATE        HLRATE            5
     C                   MOVEL     SCDSC1        HLDSC1           40
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      *BLANKS       ##CMSV           15
     C                   MOVE      *BLANKS       ##CDSC           40
     C*
     C     SCRVCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRVCD        ##CMSV
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLRVCD        SCRVCD
     C                   MOVEL     HLRATE        SCRATE
     C                   MOVEL(P)  HLDSC1        SCDSC1
     C                   SETON                                        9071
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCD
     C     5             SUBST     ##CMSV:4      SCRATE
     C                   MOVEL(P)  ##CDSC        SCDSC1
     C                   SETON                                        9072
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* HCPCS/CPT4 CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRATE'
     C                   MOVEL     SCRVCD        HLRVCD
     C                   MOVEL     SCRATE        HLRATE
     C                   MOVEL     SCDSC1        HLDSC1
     C                   MOVE      EOAST         ##ST
     C                   MOVE      *BLANKS       ##CMSV
     C                   MOVE      *BLANKS       ##CDSC
     C*
     C     SCRVCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRVCD        ##CMSV
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLRVCD        SCRVCD
     C                   MOVEL     HLRATE        SCRATE
     C                   MOVEL(P)  HLDSC1        SCDSC1
     C                   SETON                                        9072
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCD
     C     5             SUBST     ##CMSV:4      SCRATE
     C                   MOVEL(P)  ##CDSC        SCDSC1
     C                   SETON                                        9071
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* PAID REV/DRG CODE SEARCH
     C*
     C*** BEG ADD *** 09/20/07 ****************************************
     C     HFLD          WHENEQ    'SCRVCP'
     C                   MOVEL     SCRVCP        HLRVCD            3 0
     C                   MOVEL     SCRATP        HLRATE            5
     C                   MOVEL     SCDSCP        HLDSC1           40
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      *BLANKS       ##CMSV           15
     C                   MOVE      *BLANKS       ##CDSC           40
     C*
     C     SCRVCP        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRVCP        ##CMSV
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLRVCD        SCRVCP
     C                   MOVEL     HLRATE        SCRATP
     C                   MOVEL(P)  HLDSC1        SCDSCP
     C                   SETON                                        9055
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCP
     C     5             SUBST     ##CMSV:4      SCRATP
     C                   MOVEL(P)  ##CDSC        SCDSCP
     C                   SETON                                        9056
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* HCPCS/CPT4 CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRATP'
     C                   MOVEL     SCRVCP        HLRVCD
     C                   MOVEL     SCRATP        HLRATE
     C                   MOVEL     SCDSCP        HLDSC1
     C                   MOVE      EOAST         ##ST
     C                   MOVE      *BLANKS       ##CMSV
     C                   MOVE      *BLANKS       ##CDSC
     C*
     C     SCRVCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRVCP        ##CMSV
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLRVCD        SCRVCP
     C                   MOVEL     HLRATE        SCRATP
     C                   MOVEL(P)  HLDSC1        SCDSCP
     C                   SETON                                        9056
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCP
     C     5             SUBST     ##CMSV:4      SCRATP
     C                   MOVEL(P)  ##CDSC        SCDSCP
     C*** BEG CHG *** 03/11/10 ****************************************
     C***********          SETON                     9057
     C                   SETON                                        9058
     C*** END CHG *** 03/11/10 ****************************************
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*** END ADD *** 09/20/07 ****************************************
     C*** BEG ADD *** 10/17/06 ****************************************
     C*** BEG DEL *** 09/20/07 ****************************************
     C***********HFLD**    WHEQ 'SCRVCP'
     C*****************
     C*****************    MOVE SCRVCP    HLRVCP
     C*****************    MOVE *BLANKS   ##PROC  5
     C*****************    MOVE *BLANKS   ##PROD 50
     C*****************    Z-ADD0         ##RRN1  90
     C*****************    MOVE EOAST     ##AST   2
     C*****************    CALL 'F2013R'
     C*****************    PARM ' '       ##KC    1
     C*****************    PARM ' '       ##KL    1
     C*****************    PARM           ##RRN1
     C*****************    PARM           ##PROC
     C*****************    PARM           ##PROD
     C*****************    PARM ' '       ##RTCD
     C*****************    PARM           ##AST
     C*****************
     C***********##RTCD    IFEQ '1'
     C***********##PROD    OREQ *BLANKS
     C*****************    MOVELHLRVCP    SCRVCP
     C*****************    SETON                     9055  pos cursor
     C*****************    ELSE
     C*****************    MOVEL##PROC    SCRVCP
     C*****************    SETON                     9056
     C*****************    ENDIF                           ##RTCD = 1
     C*****************
     C*PAID*HCPCS/DRG*CODE SEARCH
     C*****************
     C***********HFLD**    WHEQ 'SCRATP'
     C*****************
     C*****************    MOVE SCRATP    HLRATP
     C*****************    MOVELSCDSCP    HLDESC 40 P
     C*****************    MOVE *BLANKS   ##PROC  5
     C*****************    MOVE *BLANKS   ##PROD 50
     C*****************    Z-ADD0         ##RRN1  90
     C*****************    MOVE EOAST     ##AST   2
     C*****************    CALL 'F2013R'
     C*****************    PARM ' '       ##KC    1
     C*****************    PARM ' '       ##KL    1
     C*****************    PARM           ##RRN1
     C*****************    PARM           ##PROC
     C*****************    PARM           ##PROD
     C*****************    PARM ' '       ##RTCD
     C*****************    PARM           ##AST
     C*****************
     C***********##RTCD    IFEQ '1'
     C***********##PROD    OREQ *BLANKS
     C*****************    MOVELHLRATP    SCRATP
     C*****************    MOVELHLDESC    SCDSCP    P
     C*****************    SETON                     9056  pos cursor
     C*****************    ELSE
     C*****************    MOVEL##PROC    SCRATP
     C*****************    MOVEL##PROD    SCDSCP    P
     C*****************    SETON                     9056  pos cursor
     C*****************    ENDIF                           ##RTCD = 1
     C*****************
     C*** END DEL *** 09/20/07 ****************************************
     C*** END ADD *** 10/17/06 ****************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*
     C     @@RRN2        CHAIN     PEO01                              90
     C*
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C                   MOVEL     EOEOB#        KKEOB#
     C*
     C** Get 'Amount & type code' headings
     C*
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMR1
     C                   MOVEL     CXHNET        SCHNTW
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C                   MOVEL     EOEOB#        SCEOMB
     C                   MOVEL     EOCNTL        SCCNTL
     C*
     C*** BEG DEL *** 04/23/02 ****************************************
     C********** DUKY      CHAINPDU01               N60
     C*** END DEL *** 04/23/02 ****************************************
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     EUKY          CHAIN     PEU01                              60
     C     @@RRN4        CHAIN     PCA01                              60
     C*
     C*** BEG ADD *** 04/23/02 ****************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DUKY          CHAIN(N)  PDU01                              60
     C*** END ADD *** 04/23/02 ****************************************
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*** BEG CHG *** 04/23/02 ****************************************
     C******************** MOVELDURVCD    SCRVCD    P
     C******************** MOVELDUDESC    SCDSC2    P
     C******************** MOVELDURATE    SCRATE
     C                   MOVEL(P)  DUBPRC        SCRVCD
     C                   MOVEL     DUBRAT        SCRATE
     C*** END CHG *** 04/23/02 ****************************************
     C                   MOVE      SCRVCD        K1RVCD
     C*** BEGIN CHG *** 08/08/03 **************************************
     C**********           MOVE SCRATE    K1HCPC
     C                   MOVE      *BLANKS       K1HCPC
     C*** END   CHG *** 08/08/03 **************************************
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF
     C     SVSTAT        ANDEQ     'A'
     C                   MOVEL(P)  SVDESC        SCDSC1
     C                   ENDIF                                                  *IN99 = *OFF
     C*** BEGIN ADD *** 08/19/03 **************************************
     C                   MOVEL(P)  DURVCD        SCRVCP
     C                   MOVEL(P)  DUDESC        SCDSCP
     C                   MOVEL     DURATE        SCRATP
     C*** END   ADD *** 08/19/03 **************************************
     C*** BEGIN ADD *** 10/17/06 **************************************
     C                   Z-ADD     DUUNTS        SCUNTS
     C*** END   ADD *** 10/17/06 **************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL     DUPMOD        SCPMOD
     C                   MOVEL     DUMOD2        SCMOD2
     C                   MOVEL     DUMOD3        SCMOD3
     C                   MOVEL     DUMOD4        SCMOD4
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C                   Z-ADD     CACHRG        SCTOTC
     C                   Z-ADD     CAPAID        SCTOTP
     C                   Z-ADD     CAMRA         SCMRA
     C                   Z-ADD     CANDIS        SCNTWK
     C                   Z-ADD     CAOTHR        SCOTH1
     C                   Z-ADD     CAOTH2        SCOTH2
     C*
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   SETOFF                                       59
     C     EOREVI        IFGE      'E'
     C                   SETON                                        59
     C                   ENDIF
     C*** END ADD *** 03/11/10 ****************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*** BEG ADD *** 04/23/02 ****************************************
     C                   MOVEL(P)  SCRVCD        DUBPRC
     C                   MOVEL     SCRATE        DUBRAT
     C                   MOVEL(P)  SCDSC1        DUBPRD
     C*
     C*** END ADD *** 04/23/02 ****************************************
     C*** BEG ADD *** 10/17/06 ****************************************
     C*
     C* IF THE PAID CODE IS CHANGED THEN WRITE AN EOB NOTE
     C*
     C     SCRVCP        IFNE      DURVCD
     C     SCRATP        ORNE      DURATE
     C*
     C                   EXSR      WRTDEP
     C*** BEG ADD *** 09/20/07 ****************************************
     C                   MOVEL(P)  SCRVCP        DURVCD
     C                   MOVEL     SCRATP        DURATE
     C                   MOVEL(P)  SCDSCP        DUDESC
     C                   ENDIF                                                  SCRVCP
     C*** END ADD *** 09/20/07 ****************************************
     C*** BEG DEL *** 09/20/07 ****************************************
     C********             MOVELSCRVCD    DURVCD    P
     C********
     C********             MOVELSCRATP    KKCPT4    P
     C********
     C********   PRKY      SETLLPPR01
     C********   NXTCD1    TAG
     C********   PRKY      READEPPR01                    99
     C********
     C********   *IN99     IFEQ *OFF
     C********   PRSTAT    ANDEQ'I'
     C********
     C********             GOTO NXTCD1
     C********
     C********             ELSE                            + *IN99
     C**N99***             MOVELSCDSCP    DUDESC    P
     C********
     C********             ENDIF                           *IN99
     C********
     C********             ENDIF                           SCRVCP
     C********
     C*** END DEL *** 09/20/07 ****************************************
     C*** END ADD *** 10/17/06 ****************************************
     C*
     C*** BEG ADD *** 09/20/07 ****************************************
     C                   Z-ADD     SCUNTS        DUUNTS
     C*** END ADD *** 09/20/07 ****************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL     SCPMOD        DUPMOD
     C                   MOVEL     SCMOD2        DUMOD2
     C                   MOVEL     SCMOD3        DUMOD3
     C                   MOVEL     SCMOD4        DUMOD4
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C                   MOVE      Q1USER        DUUSER
     C                   MOVE      Q1PGM         DUPGM
     C                   MOVE      DSTIME        DUTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
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
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
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
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
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
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C*******    @@RRN     CHAINDBRCDF               99
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
     C                   FEOD      FDDUP
     C                   ENDSR
     C*** BEG ADD *** 10/17/06 ****************************************
     C*================================================================
     C*WRTDEP - Write note indicating change on paid code
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     DEKY2         SETGT     PDE01
     C     DEKY1         READPE    PDE01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DESEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   PDE01
     C*
     C                   Z-ADD     EOCO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C                   MOVEL     *BLANKS       DEEOB
     C                   MOVEL(P)  NT1           XXNT1
     C                   MOVE      DURVCD        XXRVCD
     C     XXRVCD        CAT       DURATE:1      XXNT2
     C                   MOVEL(P)  NT3           XXNT3
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVEL(P)  *BLANKS       DENOT2
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  'N'           DEPRNT
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   MOVE      'U'           DEESRC                         user entered
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1USER        DEIUSR
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C                   MOVEL     DSTIME        DEITIM
     C*
     C                   WRITE     PDE02
     C*
     C                   ENDSR
     C*** END ADD *** 10/17/06 ****************************************
