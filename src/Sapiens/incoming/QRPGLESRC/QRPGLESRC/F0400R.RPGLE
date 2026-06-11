     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H*  F0400R - PROVIDER BILLS WORK WITH ERRORS MAINTENANCE         *
     H*                                                               *
     H*  CREATED: 03/02/07 BILLS WORK WITH ERRORS MAINTENANCE         *
     H*  F0400R - C:188 PROJECT: 10MJCW- EDI EDITS (P2P IMPORT)       *
     H*                                                               *
     H*  CALLED FROM: F0390R                                          *
     H*     CALLS TO: F0400R01                                        *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  08/28/08 C:47  PROJECT: 10MJC
     H*                 ADD ANOTHER KEY TO FDELL1 SO SUBFILE PAGING
     H*                 WORKS CORRECTLY.
     H*
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0400F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDELP     IF   E             DISK
     F                                     RENAME(PEL01:PFRCDF)
     F*  Primary logical file definition (for displaying items)
     FFDELL1    UF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEL01:DBRCDF)
     FFAEOP     IF   E             DISK
     FFTEDL     IF   E           K DISK
     FFDCAL5    UF   E           K DISK
     FFDDHL     UF   E           K DISK
     FFDDUL     UF   E           K DISK
     FFDDDL     UF   E           K DISK
     FFDDPL     UF   E           K DISK
     F*TABL   IF  E           K        DISK
     F*           PRC01                             KIGNORE
     F*           PRX01                             KIGNORE
     FFMRXL     IF   E           K DISK
     FFMRCL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01E
     D*
     D HD              S             79    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(27) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(27) ALT(OF)
     D*
     D***PY SCOPYWW01I
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D                 DS
     D  ##NUM                  1     80
     D  XXNUM                  6      9
     D*
     D                 DS
     D  XADATA                 1     40
     D  XALIN#                 1      4
     D  XAFDAT                 6     15
     D  XACPT4                17     28
     D  XAPMOD                30     31
     D  XADESC                33     40
     D*
     D                 DS
     D  XBDATA                 1     40
     D  XBFDAT                 1     10
     D  XBLIN#                12     15
     D  XBCPT4                17     28
     D  XBPMOD                30     31
     D  XBDESC                33     40
     D*
     D                 DS
     D  XCDATA                 1     40
     D  XCCPT4                 1     12
     D  XCPMOD                14     15
     D  XCDESC                17     24
     D  XCLIN#                26     29
     D  XCFDAT                31     40
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Errors')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D SLMSG1          C                   CONST('MC40081')
     D SLMSG2          C                   CONST('FSMESG')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D DELETE          C                   CONST('** Delete  **')
     D MRAADJ          C                   CONST('MRA adjustment')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(999)
     D*================================================================
     D*  Program specific constants
     D*================================================================
     D*
     D HDR2A           C                   CONST('         Code')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          faeop
     C                   PARM                    @@PGM            10
     C                   PARM                    @@PGM2           10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    @@RRN             9 0          faeop
     C                   PARM      SCDBRR        ##RRN             9 0          fdelp
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     EDIPL         PLIST
     C                   PARM                    @@MODE            1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          faeop
     C                   PARM                    @@PGM            10
     C                   PARM                    @@PGM2           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
    C*
    C* KEY LIST FOR TRANSACTION DETAIL FILES
    C*
     C     DDKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     RCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA            3
     C                   KFLD                    KKCPT4            5
     C                   KFLD                    KKEFDT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC
     C*
     C     ELKY          KLIST
     C                   KFLD                    EOEOB#
     C                   KFLD                    SCDSEQ
     C                   KFLD                    KKLIN#            9 0
     C                   KFLD                    SCCODE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CAFDAT        KKEFDT
     C     *LIKE         DEFINE    RXNDC         KKNDC
     C*
     C* CONVERT @@CALL
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'A'
     C     @@MODE        OREQ      'C'
     C                   MOVEL     ' '           @@CALL            1
     C*
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL     '1'           @@CALL
     C*
     C                   ENDSL
     C*
     C                   MOVEL(P)  HDR2A         SCHDL2
     C*
     C/COPY SCOPYWW01C
     C*
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KBDO - F2=Complete
     C*================================================================
     C     KBDO          BEGSR
     C*
     C                   SETOFF                                       98
     C*
     C     SFRRN1        IFGE      1
     C*
     C                   Z-ADD     1             RRN               3 0
     C     RRN           DOWLE     @RCDS
     C     RRN           CHAIN     SCR01S                             98
     C*
     C     *IN98         IFEQ      *OFF
     C                   MOVE      SCLIN#        KKLIN#
     C     ELKY          CHAIN     DBRCDF                             97
     C*
     C     *IN97         IFEQ      *OFF
     C     ELNVAL        ANDNE     *BLANKS
     C                   MOVE      ELDSEQ        KKDSEQ
     C                   SELECT
     C*
     C* DETAIL CHARGE UPDATE
     C*
     C     ELFLD         WHENEQ    'CACHRG'
     C     DDKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ELNVAL        CACHRG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'CAFDAT'
     C     DDKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ELNVAL        CAFDAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'CAUNTS'
     C     DDKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ELNVAL        CAUNTS
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C                   ENDIF
     C*
     C* DENTAL DETAIL
     C*
     C     ELFLD         WHENEQ    'DDDXR1'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DDDXR1            1
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DDDXR2'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DDDXR2            1
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DDDXR3'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DDDXR3            1
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DDDXR4'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DDDXR4            1
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DDPOS'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     3             SUBST     ELNVAL:POS    DDPOS             3
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DDPRCD'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     5             SUBST     ELNVAL:POS    DDPRCD
     C* GET DESCRIPTION FOR PROCEDURE CODE
     C                   MOVEL     EOZIPA        KKZIPA
     C                   MOVEL(P)  DDPRCD        KKCPT4
     C                   MOVEL     CAFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  50
     C     *IN50         IFEQ      *OFF
     C     RCSTAT        ANDNE     'I'
     C                   MOVEL(P)  RCDSC1        DDPRDS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C* PHYSICIAN DETAIL
     C*
     C     ELFLD         WHENEQ    'DHDXR1'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DHDXR1            1
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHDXR2'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DHDXR2            1
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHDXR3'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DHDXR3            1
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHDXR4'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DHDXR4            1
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHMOD1'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     2             SUBST     ELNVAL:POS    DHMOD1
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHMOD2'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     2             SUBST     ELNVAL:POS    DHMOD2
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHMOD3'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     2             SUBST     ELNVAL:POS    DHMOD3
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHMOD4'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     2             SUBST     ELNVAL:POS    DHMOD4
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHPOS'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     3             SUBST     ELNVAL:POS    DHPOS             3
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHPRCD'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     5             SUBST     ELNVAL:POS    DHPRCD
     C* GET DESCRIPTION FOR PROCEDURE CODE
     C                   MOVEL     EOZIPA        KKZIPA
     C                   MOVEL(P)  DHPRCD        KKCPT4
     C                   MOVEL     CAFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  50
     C     *IN50         IFEQ      *OFF
     C     RCSTAT        ANDNE     'I'
     C                   MOVEL(P)  RCDSC1        DHPRDS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DHNDC'
     C     DDKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C                   SUBST     ELNVAL:POS    DHNDC
     C* GET DESCRIPTION
     C                   MOVEL(P)  DHNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     RXNAME        DPDESC
     C                   ENDIF
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C* PHARMACY DETAIL
     C*
     C     ELFLD         WHENEQ    'DPCERT'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DPCERT
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPLIC#'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C                   SUBST     ELNVAL:POS    DPLIC#
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPNDC'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C                   SUBST     ELNVAL:POS    DPNDC
     C* GET DESCRIPTION
     C                   MOVEL(P)  DPNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     RXNAME        DPDESC
     C                   ENDIF
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPNWRF'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DPNWRF
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPRX#'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C                   SUBST     ELNVAL:POS    DPRX#
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPRXDY'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ELNVAL        DPRXDY
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPRXFD'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ELNVAL        DPRXFD
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DPRXPT'
     C     DDKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     1             SUBST     ELNVAL:POS    DPRXPT
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C* HOSPITAL DETAIL
     C*
     C     ELFLD         WHENEQ    'DUPMOD'
     C     DDKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     2             SUBST     ELNVAL:POS    DUPMOD
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDU01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DUPOS'
     C     DDKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     3             SUBST     ELNVAL:POS    DUPOS             3
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDU01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DURATE'
     C     DDKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C     ' '           CHECK     ELNVAL        POS               2 0
     C     5             SUBST     ELNVAL:POS    DURATE
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDU01
     C                   ENDIF
     C*
     C     ELFLD         WHENEQ    'DURVCD'
     C     DDKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C     3             SUBST     ELNVAL        WK3               3
     C                   MOVE      WK3           DURVCD
     C                   SUBST     ELNVAL:4      DURATE
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   UPDATE    PDU01
     C                   ENDIF
     C*
     C                   ENDSL
     C                   ENDIF
     C*
     C* FDELP: SET PROCESSED FLAG TO 'P'
     C*
     C                   MOVE      'P'           ELPRFL
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELPRDT
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  chain DBRCDF
     C*
     C                   ADD       1             RRN
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C* CALL NEW PROVIDER BILL ERROR API TO GENERATE NEW ERRORS
     C*
     C                   CALL      'FA745R'      EDIPL
     C*
     C                   EXSR      CLRGET
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     ELEOB#        SCEOB#
     C                   Z-ADD     ELDSEQ        SCDSEQ
     C*
     C* FORMAT LINE # FOR DISPLAY
     C*
     C                   MOVEL(P)  ELLIN#        ##NUM
     C                   MOVE(P)   *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVE      XXNUM         SCLIN#            4
     C*
     C                   MOVE      ELCODE        SCCODE
     C*
     C* GET CODE DESCRIPTION
     C*
     C     ELCODE        CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EDDESC        SCDESC
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/28/08 **************************************
     C                   MOVEL(P)  ELFLD         SCFLD
     C*** END   ADD *** 08/28/08 **************************************
     C                   MOVE      ELVALU        SCVALU
     C                   MOVE      ELNVAL        SCNVAL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKB         CASEQ     *ON           KBDO                           F2
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* SET PROGRAM VARIABLES
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   MOVEL     '2'           XXMODE            1
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  'F0400R01'    ACDPGM           10
     C*
     C     @@MODE        IFEQ      'E'
     C                   MOVEL(P)  'F0400R01'    ACDPGM
     C                   ENDIF
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOR            1            CORRECT CODING
     C                   PARM      *BLANK        ##CRVW            1
     C                   PARM      *BLANK        ##PROM            1
     C                   PARM      *BLANK        ##MCRV            1
     C                   PARM      *BLANK        ##FREQ            1
     C                   PARM      *BLANK        ##CRAC            1
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANKS       ##L1             78
     C     SCL2HD        PARM      *BLANKS       ##L2             78
     C*
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN         ##RRN             9 0
     C     SCL3HD        PARM      *BLANKS       ##L1             78
     C*
     C                   MOVE      *ON           *IN20
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C     EOEOB#        READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C     EOEOB#        READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21EOEOB#        SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21EOEOB#        SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
** Column headings
Opt Lin# Error Description                     Current Value   Corrected Value


** Description--------------MISN
1022=Change                 YNN
1033=Copy                   NNN
1044=Delete                 NNN
1055=Display                YYN
1066=Messages               NNN
1077=Network Info           NNN
1088=Work w/Amounts         NNN
11111=EOB/Notes             NNN
11212=Display State Rule    NNN
11313=Correct Coding Rule   NNN
202F2=Re-process            YNY
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Header Info           NNN
208F8=Header EOB/Notes      NNN
211F11=Change View          NNN
212F12=Cancel               YYY
214F14=DARTS                NNN
215F15=Fold/Truncate        NNN
216F16=History              NNN
217F17=Subset               NNN
218F18=View Pre-Cert        NNN
219F19=View Prov info       NNN
220F20=Pended items         NNN
221F21=Dtl Chrg List        NNN
3
