     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  S0555R01 - DEPARTMENT DOCUMENT CODE INFORMATION C:62 05/14/98*
     H*                                                               *
     H*  CALLED FROM: S0555R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/18/01 C:62 PROJECT MC048 29357                            *
     H*                COMPILE ONLY.                                  *
     H*                                                               *
     H*  09/22/99 C:65 PROJECT: SUPPORT                               *
     H*                REMOVE CO# FROM THE KEY FOR THE FOLDER FILES.  *
     H*                                                               *
     H*  02/22/99 C:62 PROJECT: MC184                                 *
     H*                REMOVE EDITS ON SCREEN FIELDS THAT HAVE EDIT   *
     H*                BASED UPON WHETHER THE DIARY FLAG IS SET TO 'Y'*
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FS0555F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMDOP     UF A E             DISK
     F                                     RENAME(PDO01:DBRCDF)
     FSMDOL     IF   E           K DISK
     FSMDEP     IF   E             DISK
     FSMFCP     IF   E             DISK
     FSMFCL     IF   E           K DISK
     F                                     RENAME(PFC01:PFC02)
     FSSYLF     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Department Documents')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D DELETE          C                   CONST('****** Delete ******')

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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     DOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDPCD
     C                   KFLD                    KKFOLD
     C                   KFLD                    KKDCID
     C*
     C     FCKY          KLIST
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          KFLD           KKCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   KFLD                    KKPRE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    DECO#         KKCO#
     C     *LIKE         DEFINE    DEDPCD        KKDPCD
     C     *LIKE         DEFINE    DOFOLD        KKFOLD
     C     *LIKE         DEFINE    DODCID        KKDCID
     C     *LIKE         DEFINE    FCPRE         KKPRE
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DOSTAT        IFEQ      'I'
     C                   MOVE      'A'           DOSTAT
     C                   ELSE
     C                   MOVE      'I'           DOSTAT
     C                   ENDIF                                                  DOSTAT
     C*
     C                   MOVEL     Q1USER        DOUSER
     C                   MOVEL     Q1PGM         DOPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DODT
     C*
     C                   MOVEL     DSTIME        DOTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C* FOLDER CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SCFOLD        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C*  THE FOLDER NAME MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   CALL      'SVALFLR'
     C                   PARM      SCFOLD        ##FOLD           80
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C*  FOLDER MUST EXIST AND USER MUST HAVE AUTHORITY.
     C*
     C                   SELECT
     C*
     C     ##RTCD        WHENEQ    '1'
     C*
     C                   SETON                                        7290
     C*
     C     ##RTCD        WHENEQ    '2'
     C*
     C                   SETON                                        7390
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* DOCUMENT ID CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SCDCID        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C*  THE DOCUMENT ID MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   CALL      'SVALDOC'
     C                   PARM      SCDCID        ##DCID           12
     C                   PARM      SCFOLD        ##FOLD           80
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      '1'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* FOLDER/DOCUMENT MUST BE UNIQUE.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   MOVEL(P)  SCFOLD        KKFOLD
     C                   MOVEL(P)  SCDCID        KKDCID
     C*
     C     DOKY          CHAIN     PDO01                              99
     C*
     C  N99              SETON                                        7690
     C*
     C                   ENDIF
     C*
     C* DESCRIPTION MUST BE ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDESC        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF
     C*
     C* PREFIX MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCPRFX        KKPRE
     C*
     C     FCKY          CHAIN     PFC02                              99
     C*
     C   99              SETON                                        7890
     C*
     C                   ENDIF
     C*
     C* REMARK CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *ON
     C*
     C                   CALL      'DA200R'
     C                   PARM      SCRMCD        ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'N'
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DIARY FLAG MUST BE 'Y' OR 'N'.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCDIFL        IFNE      'Y'
     C     SCDIFL        ANDNE     'N'
     C*
     C                   SETON                                        8090
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 02/22/99 **************************************
     C*
     C* DIARY USER (FILE) CAN'T BE BLANK IF DIARY FLAG = 'Y'
     C*
     C********** *IN90     IFEQ *OFF
     C********** SCDIFL    ANDEQ'Y'
     C********** SCDUFL    ANDEQ*BLANKS
     C*
     C**********           SETON                     8190
     C*
     C**********           ENDIF
     C*
     C* DIARY USER (FIELD) CAN'T BE BLANK IF DIARY FLAG = 'Y'
     C*
     C********** *IN90     IFEQ *OFF
     C********** SCDIFL    ANDEQ'Y'
     C********** SCDUFD    ANDEQ*BLANKS
     C*
     C**********           SETON                     8290
     C*
     C**********           ENDIF
     C*
     C* DIARY FROM DATE (FIELD) CAN'T BE BLANK IF FILE NE BLANKS.
     C*
     C********** *IN90     IFEQ *OFF
     C********** SCFDFL    ANDNE*BLANKS
     C********** SCFDFD    ANDEQ*BLANKS
     C*
     C**********           SETON                     8390
     C*
     C**********           ENDIF
     C*** END   DEL *** 02/22/99 **************************************
     C*
     C* OUTQ IF ENTERED MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOUTQ        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCOUTQ        ##OUTQ
     C*
     C                   CALL      'S0003C'
     C                   PARM                    ##OUTQ           10
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      '1'
     C*
     C                   SETON                                        8490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* RESTRICT OUTQ MUST BE ENTERED IF OUTQ IS ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOUTQ        ANDNE     *BLANKS
     C     SCOUTR        ANDNE     'Y'
     C     SCOUTR        ANDNE     'N'
     C*
     C                   SETON                                        8590
     C*
     C                   ENDIF
     C*
     C* ALLOW CHANGE OF RESOLVED DOCUMENT MUST BE ' ' OR 'Y'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCALCH        ANDNE     *BLANKS
     C     SCALCH        ANDNE     'Y'
     C*
     C                   SETON                                        3090
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
     C*================================================================
     C* KHDO - F8=Create document
     C*================================================================
     C     KHDO          BEGSR
     C*
     C                   CALL      'SVALFLR'
     C                   PARM      SCFOLD        ##FOLD           80
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C*  FOLDER MUST EXIST AND USER MUST HAVE AUTHORITY.
     C*
     C                   SELECT
     C*
     C     ##RTCD        WHENEQ    '1'
     C*
     C                   SETON                                        7290
     C*
     C     ##RTCD        WHENEQ    '2'
     C*
     C                   SETON                                        7390
     C*
     C                   OTHER
     C*
     C* Get Office CLP Program Name from SDRPF/DRAL02
     C*
     C                   MOVEL     'SC'          XXSYK
     C*
     C     SYKY          CHAIN     SDR01                              60
     C*
     C                   CALL      'S0555C01'
     C                   PARM      SCFOLD        ##FOLD           80
     C                   PARM      SCDCID        ##DCID           12
     C                   PARM      DRFL06        ##FL06            1
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*   FOLDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFOLD'
     C*
     C                   MOVEL(P)  '*ALL'        ##FLDR
     C*
     C                   CALL      'SWRKFLR'                                    "DSPFLR" OFC API
     C                   PARM                    ##FLDR           80            FOLDER
     C                   PARM      *BLANKS       ##RTCD            1            ERR ID CODE
     C*
     C                   SELECT
     C*
     C     ##RTCD        WHENEQ    '1'
     C                   SETON                                        86
     C*
     C     ##RTCD        WHENEQ    '2'
     C                   SETON                                        87
     C*
     C                   ENDSL
     C*
     C                   SETON                                        31
     C*
     C*   DOC ID SEARCH
     C*
     C     HFLD          WHENEQ    'SCDCID'
     C*
     C*  MUST HAVE FOLDER NAME TO SEARCH FOR DOCUMENTS
     C*
     C     SCFOLD        IFEQ      *BLANKS
     C*
     C                   SETON                                        71
     C*
     C                   ELSE
     C*
     C                   CALL      'SDSPFLR'                                    "DSPFLR" OFC API
     C                   PARM      SCFOLD        ##FLDR           80            FOLDER
     C                   PARM      *BLANKS       ##DCID           12            RTN DOC ID SLCT
     C                   PARM      *BLANKS       ##RTCD            1            ERR ID CODE
     C*
     C                   SELECT
     C*
     C     ##RTCD        WHENEQ    '1'
     C                   SETON                                        8689
     C*
     C     ##RTCD        WHENEQ    '2'
     C                   SETON                                        7389
     C*
     C     ##RTCD        WHENEQ    '3'
     C                   SETON                                        8889
     C*
     C     ##RTCD        WHENEQ    '4'
     C                   SETON                                        8989
     C*
     C                   ENDSL
     C*
     C     ##DCID        IFEQ      *BLANK                                       VALID DOC ID
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C                   MOVEL     ##DCID        SCDCID                         USE IT, GOTO
     C                   SETON                                        33
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPRFX'
     C*
     C                   CALL      'S0007R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PFC01                              99
     C*
     C                   MOVEL(P)  FCPRE         SCPRFX
     C                   MOVEL(P)  FCFLRS        SCFLDS
     C                   MOVEL(P)  FCDESC        SCPRDS
     C*
     C   16              SETON                                        35
     C  N16              SETON                                        36
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCRMCD'
     C*
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C     ##RMCD        OREQ      *BLANKS
     C*
     C                   SETON                                        35
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##RMCD        SCRMCD
     C                   MOVEL(P)  ##DESC        SCRMDS
     C                   SETON                                        36
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  DOFOLD        SCFOLD
     C                   MOVEL(P)  DODCID        SCDCID
     C                   MOVEL(P)  DODESC        SCDESC
     C                   MOVEL(P)  DOPRFX        SCPRFX
     C*
     C                   MOVEL     SCPRFX        KKPRE
     C*
     C     FCKY          CHAIN     PFC02                              99
     C*
     C  N99              MOVEL(P)  FCFLRS        SCFLDS
     C  N99              MOVEL(P)  FCDESC        SCPXDS
     C   99              MOVEL(P)  *BLANKS       SCFLDS
     C   99              MOVEL(P)  *BLANKS       SCPXDS
     C*
     C                   MOVEL(P)  DORMCD        SCRMCD
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   CALL      'DA200R'
     C                   PARM      SCRMCD        ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'Y'
     C*
     C                   MOVEL(P)  ##DESC        SCRMDS
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCRMDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  DODIFL        SCDIFL
     C                   MOVEL(P)  DODUFL        SCDUFL
     C                   MOVEL(P)  DODUFD        SCDUFD
     C                   MOVEL(P)  DOFDFL        SCFDFL
     C                   MOVEL(P)  DOFDFD        SCFDFD
     C                   MOVE(P)   DODDYS        SCDDYS
     C                   MOVEL(P)  DOOUTQ        SCOUTQ
     C                   MOVEL(P)  DOOUTR        SCOUTR
     C*
     C     DOCOPY        IFEQ      *ZEROS
     C*
     C                   Z-ADD     1             SCCOPY
     C*
     C                   ELSE
     C*
     C                   MOVE      DOCOPY        SCCOPY
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  DOFORM        SCFORM
     C                   MOVEL(P)  DOOVRL        SCOVRL
     C                   MOVEL(P)  DOPGMN        SCPGMN
     C                   MOVEL(P)  DOSTAT        SCSTAT
     C                   MOVEL(P)  DOALCH        SCALCH
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCFOLD
     C                   MOVEL(P)  *BLANKS       SCDCID
     C                   MOVEL(P)  *BLANKS       SCDESC
     C                   MOVEL(P)  *BLANKS       SCPRFX
     C                   MOVEL(P)  *BLANKS       SCFLDS
     C                   MOVEL(P)  *BLANKS       SCPXDS
     C                   MOVEL(P)  *BLANKS       SCRMCD
     C                   MOVEL(P)  *BLANKS       SCRMDS
     C                   MOVEL(P)  *BLANKS       SCDIFL
     C                   MOVEL(P)  *BLANKS       SCDUFL
     C                   MOVEL(P)  *BLANKS       SCDUFD
     C                   MOVEL(P)  *BLANKS       SCFDFL
     C                   MOVEL(P)  *BLANKS       SCFDFD
     C                   MOVE(P)   *ZEROS        SCDDYS
     C                   MOVEL(P)  *BLANKS       SCOUTQ
     C                   MOVEL(P)  *BLANKS       SCOUTR
     C                   Z-ADD     1             SCCOPY
     C                   MOVEL(P)  *BLANKS       SCFORM
     C                   MOVEL(P)  *BLANKS       SCOVRL
     C                   MOVEL(P)  *BLANKS       SCPGMN
     C                   MOVEL(P)  'A'           SCSTAT
     C                   MOVEL(P)  *BLANKS       SCALCH
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      KKCO#         DOCO#
     C                   MOVEL(P)  SCDPCD        DODPCD
     C                   MOVEL(P)  SCFOLD        DOFOLD
     C                   MOVEL(P)  SCDCID        DODCID
     C                   MOVEL(P)  SCDESC        DODESC
     C                   MOVEL(P)  SCPRFX        DOPRFX
     C                   MOVEL(P)  SCRMCD        DORMCD
     C                   MOVEL(P)  SCDIFL        DODIFL
     C                   MOVEL(P)  SCDUFL        DODUFL
     C                   MOVEL(P)  SCDUFD        DODUFD
     C                   MOVEL(P)  SCFDFL        DOFDFL
     C                   MOVEL(P)  SCFDFD        DOFDFD
     C                   MOVE      SCDDYS        DODDYS
     C                   MOVEL(P)  SCOUTQ        DOOUTQ
     C                   MOVEL(P)  SCOUTR        DOOUTR
     C*
     C     SCCOPY        IFEQ      *ZEROS
     C*
     C                   Z-ADD     1             DOCOPY
     C*
     C                   ELSE
     C*
     C                   MOVE      SCCOPY        DOCOPY
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCFORM        DOFORM
     C                   MOVEL(P)  SCOVRL        DOOVRL
     C                   MOVEL(P)  SCPGMN        DOPGMN
     C*
     C     SCPGMN        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'Y'           DOAIFL
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'N'           DOAIFL
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCSTAT        DOSTAT
     C                   MOVEL(P)  SCALCH        DOALCH
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
     C                   MOVEA     '000000'      *IN(31)
     C*
     C     *INKH         CASEQ     *ON           KHDO
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVEL     *ON           *INLR
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
     C*** GET SYSTEM INFO
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
     C*
     C     ##WCDK        IFEQ      'N'
     C*
     C                   SETOFF                                       16
     C*
     C                   ELSE
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*
     C     @@RRN2        CHAIN     PDE01                              99
     C*
     C                   Z-ADD     DECO#         KKCO#
     C                   MOVEL     DEDPCD        KKDPCD
     C                   MOVEL     DEDPCD        SCDPCD
     C                   MOVEL     DENAME        SCDPTN
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
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
     C                   MOVEL     Q1USER        DOUSER
     C                   MOVEL     Q1PGM         DOPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DODT
     C*
     C                   MOVEL     DSTIME        DOTIME
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
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DOUSER
     C                   MOVEL     Q1PGM         DOPGM
     C                   MOVEL     Q1USER        DOIUSR
     C                   MOVEL     Q1PGM         DOIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DODT
     C                   MOVEL     SRDTO         DOIDT
     C*
     C                   MOVEL     DSTIME        DOTIME
     C                   MOVEL     DSTIME        DOITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      SMDOP
     C*
     C                   ENDSR
