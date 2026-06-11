     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*      F2500R - EDI DATA ELEMENT CRITERIA MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  04/11/02 C:62  PROJECT: MCMNT LOG# 216
     H*                 USE SY600R INSTEAD OF SY601R.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*****************************************************************
     FF2500F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTDNP     IF   E             DISK
     F                                     RENAME(PDN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTDNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDN01:DBRCDF)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN DEL *** 04/11/02 **************************************
     F**********STSTP   IF  E                    DISK
     F*** END   DEL *** 04/11/02 **************************************
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F                                     RENAME(PST01:PST02)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Data Element -
     D                                     Criteria')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2500R01')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@JST             2
     C                   PARM                    @@TSET            3
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
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
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKJST
     C                   KFLD                    KKTSET
     C                   KFLD                    KKDN#
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DNJST         KKJST
     C     *LIKE         DEFINE    DNTSET        KKTSET
     C     *LIKE         DEFINE    DNDN#         KKDN#
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C     SSJST         ORNE      *BLANKS
     C     SSTSET        ORNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN11
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SSJST         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SSJST         KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SSTSET        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SSTSET        KKTKEY
     C                   MOVEL(P)  'TSET'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   SETOFF                                       16
     C*
     C     Q1PRMS        IFGT      4
     C*
     C                   MOVEL(P)  @@JST         SSJST
     C                   MOVEL(P)  @@TSET        SSTSET
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       KKJST
     C                   MOVEL(P)  *BLANKS       KKTSET
     C                   Z-ADD     *ZEROS        KKDN#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKJST
     C                   MOVEL(P)  SCPSTT        KKTSET
     C                   Z-ADD     SCPSTD        KKDN#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCJST         KKJST
     C                   MOVEL(P)  SCTSET        KKTSET
     C                   Z-ADD     SCDN#         KKDN#
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSJST'
     C*
     C*** BEGIN CHG *** 04/11/02 **************************************
     C**********           CALL 'SY601R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM '0'       ##KC    1
     C********** *INKL     PARM '0'       ##KL    1
     C**********           PARM *ZEROS    ##RRN   90
     C*
     C********** *INKL     IFEQ *ON
     C********** ##RRN     OREQ *ZEROS
     C*
     C**********           SETON                     31
     C*
     C**********           ELSE
     C*
     C********** ##RRN     CHAINPST01                99
     C*
     C**********           MOVELSTKEY     SSJST     P
     C**********           SETON                     32
     C*
     C**********           ENDIF
     C**********
     C                   CALL      'SY600R'
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##ST          OREQ      *BLANKS
     C                   SETON                                        31
     C                   ELSE
     C                   MOVEL     ##ST          SSJST
     C                   SETON                                        32
     C                   ENDIF
     C*** END   CHG *** 04/11/02 **************************************
     C*
     C     HFLD          WHENEQ    'SSTSET'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TSET'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SSTSET
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
     C                   MOVEL(P)  DNJST         SCJST
     C                   MOVEL(P)  DNTSET        SCTSET
     C                   Z-ADD     DNDN#         SCDN#
     C                   MOVEL(P)  DNDESC        SCDESC
     C                   MOVEL(P)  DNCRTR        SCCRTR
     C                   MOVEL(P)  DNSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
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
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     DNSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSJST         IFNE      *BLANKS
     C     SSJST         ANDNE     DNJST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSTSET        IFNE      *BLANKS
     C     SSTSET        ANDNE     DNTSET
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Jst TSET   DN#  Description                      Criteria   Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
213F13=User parms           YYN
217F17=Subset               YYY
3
