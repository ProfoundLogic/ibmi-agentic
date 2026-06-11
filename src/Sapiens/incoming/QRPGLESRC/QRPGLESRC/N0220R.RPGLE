     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE N0220R - WORK WITH H/R FOR INVOICING
     H*
     H*****************************************************************
     H* Description: This program will allow the user to display      *
     H*              Uninvoiced EOMBs for Hold and Release.           *
     H*                                                               *
     H* Called by: MENU                                               *
     H* Calls    :                                                    *
     H*                                                               *
     H* Created: 05/10/00  C:62  Project: AR028                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  03/17/16 C:79 Support: 90/39200                              *
     H*                Initialize DBRRN's in READ and READP           *
     H*                                                               *
     H*  05/23/01 C:62 PROJECT:MCMNT  #187                            *
     H*                FIX THE SUBSET ON FND/FYR/CASE.                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FN0220F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAEAL21   UF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEA01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFAEAL22   IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEA01:DBRCDF2)
     F*
     FFAEAL23   IF   E           K DISK
     F                                     INFDS(DBFDSZ)
     FFAEAL24   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F                                     INFDS(DBFDSY)
     FFAEAL25   IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     F                                     INFDS(DBFDSX)
     FFAEAL26   IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     F                                     INFDS(DBFDSW)
     FFAEAL27   IF   E           K DISK
     F                                     RENAME(PEA01:PEA05)
     F                                     INFDS(DBFDSV)
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDSA)
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     FFMCXL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FSMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(19) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(19) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSZ          DS
     D  DBRRNZ               397    400B 0
     D DBFDSY          DS
     D  DBRRNY               397    400B 0
     D DBFDSX          DS
     D  DBRRNX               397    400B 0
     D DBFDSW          DS
     D  DBRRNW               397    400B 0
     D DBFDSV          DS
     D  DBRRNV               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('H/R for Invoicing')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('N0220R01')
     D COMAPI          C                   CONST('FA013R')
     D CHGNUM          C                   CONST('SA020R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM                    ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Change number fields to alpha fields parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C     CNMPL2        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
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
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C*
     C     KEY5          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY6          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY7          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C*
     C     KEY8          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY9          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSBHRF
     C*
     C     KEY10         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSBHRF
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY11         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSBHRF
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY12         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSSYS
     C*
     C     KEY13         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSSYS
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY14         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSSYS
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C*
     C     CMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     CXKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    SSFND
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSER
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    UPUSER        KKUSER
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C*
     C   21              Z-ADD     DBRRN         SCDBRR
     C   22              Z-ADD     DBRRN         SCDBRR
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRN         SCDBRR
     C   22              Z-ADD     DBRRNZ        SCDBRR
     C*
     C     SSBHRF        WHENNE    *BLANKS
     C*
     C   21              Z-ADD     DBRRNY        SCDBRR
     C   22              Z-ADD     DBRRNX        SCDBRR
     C*
     C     SSSYS         WHENNE    *BLANKS
     C*
     C   21              Z-ADD     DBRRNW        SCDBRR
     C   22              Z-ADD     DBRRNV        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C     SSBHRF        ORNE      *BLANKS
     C     SSSYS         ORNE      *BLANKS
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVE      'Y'           VLDOPT            1
     C*
     C     SCSEL         IFEQ      '15'
     C     SCSYS         ANDNE     'U'
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '16'
     C     SCSYS         ANDNE     'P'
     C     SCSYS         ANDNE     'H'
     C*
     C                   SETON                                        9072
     C*
     C                   ENDIF
     C*
     C*          SCSEL     IFEQ '17'
     C*          SCTMFL    ANDNE'*'
     C*
     C*                    SETON                     9073
     C*
     C*                    ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C  N90*INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C  N90SSFND         IFNE      *ZEROS
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C   99              SETON                                        7190
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSGNAM
     C*
     C                   ENDIF
     C*
     C  N90SSFYR         IFNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C   99              SETON                                        7290
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSCNAM
     C*
     C                   ENDIF
     C*
     C  N90SSBHRF        IFNE      *BLANKS
     C     SSBHRF        ANDNE     'H'
     C     SSBHRF        ANDNE     'R'
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C  N90SSSYS         IFNE      *BLANKS
     C*
     C                   MOVEL     'SYS '        KKTBID
     C                   MOVEL(P)  SSSYS         KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7490
     C  N99              MOVEL(P)  GTDESC        SSSYSD
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SSSYSD
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   MOVEL     SCUSER        KKUSER
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C* Check to see if Group # is restricted.
     C*
     C     UPMRGR        IFEQ      'Y'
     C     UPFND         ANDNE     *ZEROS
     C*
     C                   Z-ADD     UPFND         SSFND
     C                   SETON                                        16
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C                   MOVEL(P)  FNNAME        SSGNAM
     C*
     C                   ENDIF                                                  UPMRGR 'Y'
     C*
     C                   ENDIF                                                  *IN99 =*OFF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     ##SUB#        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKFYR
     C                   Z-ADD     *ZEROS        KKCASE
     C                   Z-ADD     *ZEROS        KKEOB#
     C                   Z-ADD     *ZEROS        KKRCN
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKFND
     C   21              Z-ADD     SCPSTY        KKFYR
     C   21              Z-ADD     SCPSTC        KKCASE
     C   21              Z-ADD     *ZEROS        KKEOB#
     C   21              Z-ADD     *ZEROS        KKRCN
     C   22              MOVE      SCPST2        KKEOB#
     C   22              Z-ADD     *ZEROS        KKRCN
     C   22              Z-ADD     *ZEROS        KKFND
     C   22              Z-ADD     *ZEROS        KKFYR
     C   22              Z-ADD     *ZEROS        KKCASE
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCEOB#        KKEOB#
     C                   Z-ADD     SCRCN         KKRCN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10 Search Subroutine
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* Indicators for cursor positioning
     C*
     C                   SETOFF                                       3132
     C                   SETOFF                                       3334
     C*
     C                   SELECT
     C*
     C* Group Search
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          WHENEQ    'SSFND'
     C******IN16         ANDEQ     *OFF
     C*****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL
     C*****              PARM      ' '           ##KC
     C*****              PARM      ' '           ##KL
     C*****              PARM      *ZEROS        ##RRN
     C*****
     C*****##KC          IFEQ      '1'
     C*****##KL          OREQ      '1'
     C*****##RRN         OREQ      *ZEROS
     C*****
     C*****              SETON                                        31
     C*****
     C*****              ELSE
     C*****
     C*****##RRN         CHAIN     PFN01                              99
     C*****
     C**N99              Z-ADD     FNFND         SSFND
     C**N99              MOVEL(P)  FNNAME        SSGNAM
     C*****              SETON                                        32
     C*****
     C*****              ENDIF                                                  ##KC = '1'
     C******** End Del ***** 12/18/24 ***************************
     C*
     C* Fund Year/Case Search
     C*
     C     HFLD          WHENEQ    'SSFYR'
     C     HFLD          OREQ      'SSCASE'
     C*
     C     SSFND         IFNE      *ZEROS
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        71
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSFND         ##FND
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ##FND
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM                    ##FND
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ##FYR         SSFYR
     C                   Z-ADD     ##CASE        SSCASE
     C                   Z-ADD     ##FND         SSFND
     C*
     C* get claimaint name
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C*
     C                   MOVEL(P)  ##LAST        SSCNAM
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* Get group name
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  FNNAME        SSGNAM
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   SETON                                        33        pos cursor
     C*
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SSSYS'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'SYS '        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SSSYS
     C                   MOVEL     GTDESC        SSSYSD
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KUDO - F20=RELEASE ALL
     C*================================================================
     C     KUDO          BEGSR
     C*
     C     EAKY          CHAIN     DBRCDF                             99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      HRSUBS
     C*
     C     MEETHR        IFEQ      *ON
     C*
     C                   MOVEL(P)  'R'           EABHRF
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   ENDIF
     C*
     C                   UPDATE    DBRCDF
     C*
     C     EAKY          READE     DBRCDF                                 99
     C*
     C                   ENDDO
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=HOLD ALL
     C*================================================================
     C     KVDO          BEGSR
     C*
     C     EAKY          CHAIN     DBRCDF                             99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      HRSUBS
     C*
     C     MEETHR        IFEQ      *ON
     C*
     C                   MOVEL(P)  'H'           EABHRF
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   ENDIF
     C*
     C                   UPDATE    DBRCDF
     C*
     C     EAKY          READE     DBRCDF                                 99
     C*
     C                   ENDDO
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C                   EXSR      ASSRRN
     C*
     C                   ENDIF                                                  VLDOPT<>Y
     C*
     C                   MOVEL     EABHRF        SCBHRF
     C                   Z-ADD     EAFND         SCFND
     C                   Z-ADD     EAFYR         SCFYR
     C                   Z-ADD     EACASE        SCCASE
     C                   Z-ADD     EAEOB#        SCEOB#
     C                   Z-ADD     EARCN         SCRCN
     C*
     C                   MOVEL     EAFND         ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXFND             3
     C*
     C                   MOVEL     EAFYR         ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXFYR             3
     C*
     C                   MOVEL     EACASE        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXCASE            7
     C*
     C     XXFND         CAT(P)    XXFYR:1       XXCAS#           15
     C                   CAT       XXCASE:1      XXCAS#
     C*
     C                   MOVEL(P)  EAEOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXEOB#            9
     C*
     C     EARCN         IFEQ      *ZEROS
     C*
     C                   MOVEL     '  0'         XXRCN             3
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  EARCN         ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXRCN
     C*
     C                   ENDIF
     C*
     C     XXEOB#        CAT(P)    XXRCN         XXEOMB           13
     C*
     C   21              MOVEL(P)  XXCAS#        SCDATA
     C   21              MOVE      XXEOMB        SCDATA
     C   22              MOVEL(P)  XXEOMB        SCDATA
     C   22              MOVE      XXCAS#        SCDATA
     C*
     C     EATMFL        IFEQ      'Y'
     C*
     C                   MOVEL     '*'           SCTMFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCTMFL
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     EABAMT        SCBAMT
     C                   MOVEL     EASYS         SCSYS
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C  N99              Z-ADD     DBRRNB        SCDBR2
     C  N99              MOVEL     EOCERT        SCCERT
     C   99              Z-ADD     *ZEROS        SCDBR2
     C   99              MOVEL     *BLANKS       SCCERT
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C                   MOVEL(P)  CXHMRA        SCHMRA
     C                   MOVE      ':'           SCHMRA
     C                   MOVEL(P)  CXHNET        SCHNET
     C                   MOVE      ':'           SCHNET
     C*
     C                   Z-ADD     EACHRG        SCCHRG
     C                   Z-ADD     EAMRA         SCMRA
     C                   Z-ADD     EANDIS        SCNDIS
     C*
     C                   ENDSR
     C*================================================================
     C* OPT15 - View Pre-Cert
     C*================================================================
     C     OPT15         BEGSR
     C*
     C     CMKY2         CHAIN     PCM01                              99
     C*
     C                   CALL      'U0060R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '1'           ##CALL            1
     C                   PARM      DBRRNA        ##RRN             9 0
     C                   PARM      SCCERT        ##CERT           15
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C                   SETOFF                                       KCKL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT16 - View EOMB
     C*================================================================
     C     OPT16         BEGSR
     C*
     C     CMKY2         CHAIN     PCM01                              99
     C*
     C                   MOVE      CMFND         ##FND             3 0
     C                   MOVE      CMFYR         ##FYR             3 0
     C                   MOVE      CMSUB#        ##SUB#            5 0
     C                   MOVE      CMCASE        ##CAS#            7 0
     C                   MOVE      SCEOB#        ##EOB#            9 0
     C*
     C                   CALL      'F0350R'
     C                   PARM      '1'           ##CALL
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CAS#
     C                   PARM                    ##EOB#            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C*
     C                   ENDSR
     C*================================================================
     C* OPT17 - T/M Activity
     C*================================================================
     C     OPT17         BEGSR
     C*
     C                   CALL      'F3040R'
     C                   PARM      @@CALL        ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBR2        ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C                   PARM      'Y'           ##EOB             1
     C                   PARM      SCRCN         ##RCN             3 0
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C                   SETOFF                                       KCKL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT21 - HOLD/RELEASE
     C*================================================================
     C     OPT21         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKU         CASEQ     *ON           KUDO
     C     *INKV         CASEQ     *ON           KVDO
     C                   CAS                     KEDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '15'          OPT15
     C     SCSEL         CASEQ     '16'          OPT16
     C     SCSEL         CASEQ     '17'          OPT17
     C     SCSEL         CASEQ     '21'          OPT21
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
     C     *IN99         IFEQ      *ON
     C     *IN99         ANDEQ     *OFF
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETON                                        12
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSNAM
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*** Beg Add *** 03/17/16 **************************************
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
     C                   EVAL      DBRRN6=0
     C*** End Add *** 03/17/16 **************************************
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C   21KEY7          READE(N)  DBRCDF                                 95
     C   22KEY7          READE(N)  DBRCDF                                 95
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY4          READE(N)  DBRCDF                                 95
     C   22KEY4          READE(N)  PEA01                                  95
     C*
     C     SSBHRF        WHENNE    *BLANKS
     C   21KEY9          READE(N)  PEA02                                  95
     C   22KEY9          READE(N)  PEA03                                  95
     C*
     C     SSSYS         WHENNE    *BLANKS
     C   21KEY12         READE(N)  PEA04                                  95
     C   22KEY12         READE(N)  PEA05                                  95
     C*
     C                   OTHER
     C   21KEY1          READE(N)  DBRCDF                                 95
     C   22KEY1          READE(N)  DBRCDF2                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** Beg Add *** 03/17/16 **************************************
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   EVAL      DBRRN4=0
     C                   EVAL      DBRRN5=0
     C                   EVAL      DBRRN6=0
     C*** End Add *** 03/17/16 **************************************
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C   21KEY7          READPE(N) DBRCDF                                 95
     C   22KEY7          READPE(N) DBRCDF                                 95
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY4          READPE(N) DBRCDF                                 95
     C   22KEY4          READPE(N) PEA01                                  95
     C*
     C     SSBHRF        WHENNE    *BLANKS
     C   21KEY9          READPE(N) PEA02                                  95
     C   22KEY9          READPE(N) PEA03                                  95
     C*
     C     SSSYS         WHENNE    *BLANKS
     C   21KEY12         READPE(N) PEA04                                  95
     C   22KEY12         READPE(N) PEA05                                  95
     C*
     C                   OTHER
     C   21KEY1          READPE(N) DBRCDF                                 95
     C   22KEY1          READPE(N) DBRCDF2                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C   21KEY8          SETGT     DBRCDF                             96
     C   22KEY8          SETGT     DBRCDF                             96
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY5          SETGT     DBRCDF                             96
     C   22KEY6          SETGT     PEA01                              96
     C*
     C     SSBHRF        WHENNE    *BLANKS
     C   21KEY10         SETGT     PEA02                              96
     C   22KEY11         SETGT     PEA03                              96
     C*
     C     SSSYS         WHENNE    *BLANKS
     C   21KEY13         SETGT     PEA04                              96
     C   22KEY14         SETGT     PEA05                              96
     C*
     C                   OTHER
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C*** BEGIN CHG *** 05/23/01 **************************************
     C*
     C********** SSFND     WHNE *ZEROS
     C***21***** KEY5      SETLLDBRCDF               96
     C***22***** KEY6      SETLLPEA01                96
     C*
     C********** SSFYR     WHNE *ZEROS
     C********** SSCASE    ORNE *ZEROS
     C***21***** KEY8      SETLLDBRCDF               96
     C***22***** KEY8      SETLLDBRCDF               96
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C   21KEY8          SETLL     DBRCDF                             96
     C   22KEY8          SETLL     DBRCDF                             96
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY5          SETLL     DBRCDF                             96
     C   22KEY6          SETLL     PEA01                              96
     C*** END   CHG *** 05/23/01 **************************************
     C*
     C     SSBHRF        WHENNE    *BLANKS
     C   21KEY10         SETLL     PEA02                              96
     C   22KEY11         SETLL     PEA03                              96
     C*
     C     SSSYS         WHENNE    *BLANKS
     C   21KEY13         SETLL     PEA04                              96
     C   22KEY14         SETLL     PEA05                              96
     C*
     C                   OTHER
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
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
     C* HRSUBS - Subset for release/hold all records
     C*================================================================
     C     HRSUBS        BEGSR
     C*
     C                   MOVEL     *ON           MEETHR            1
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     EAFND
     C*
     C                   MOVEL     *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSFYR         IFNE      *ZEROS
     C     SSFYR         ANDNE     EAFYR
     C*
     C                   MOVEL     *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSCASE        IFNE      *ZEROS
     C     SSCASE        ANDNE     EACASE
     C*
     C                   MOVEL     *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSBHRF        IFNE      *BLANKS
     C     SSBHRF        ANDNE     EABHRF
     C*
     C                   MOVEL     *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSSYS         ANDNE     EASYS
     C*
     C                   MOVEL     *OFF          MEETHR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*          SSFND     IFNE *ZEROS
     C*          SSFND     ANDNEEAFND
     C*
     C*                    MOVEL*OFF      MEETSS
     C*
     C*                    ENDIF
     C*
     C*          SSFYR     IFNE *ZEROS
     C*          SSFYR     ANDNEEAFYR
     C*
     C*                    MOVEL*OFF      MEETSS
     C*
     C*                    ENDIF
     C*
     C*          SSCASE    IFNE *ZEROS
     C*          SSCASE    ANDNEEACASE
     C*
     C*                    MOVEL*OFF      MEETSS
     C*
     C*                    ENDIF
     C*
     C     SSBHRF        IFNE      *BLANKS
     C     SSBHRF        ANDNE     EABHRF
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSSYS         ANDNE     EASYS
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt H/R Grp  Yr    Case  EOB#/Svc# Rcn  T/M  Pre-cert         Bill Amount  Sys
Opt H/R EOB#/Svc# Rcn  Grp  Yr    Case  T/M  Pre-cert         Bill Amount  Sys
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
11515=View Pre-Cert         YYN
11616=View EOMB             YYN
11717=T/M Activity          YYN
12121=Hold/Release          YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
211F11=Change view          YYY
212F12=Cancel               NNY
213F13=User Parms           YYN
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
220F20=Release All          YNN
221F21=Hold All             YNN
3
