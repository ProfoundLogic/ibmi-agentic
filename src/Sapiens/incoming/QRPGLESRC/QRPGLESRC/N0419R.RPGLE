     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE N0419R - WORK WITH INVOICES
     H*
     H*****************************************************************
     H* Description: This program will allow the user to display      *
     H*              Invoices, across all cases.                      *
     H*                                                               *
     H* Called by: MENU                                               *
     H* Calls    :                                                    *
     H*                                                               *
     H* Created: 04/07/00  C:62  Project: AR001                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                 Recompile only                                *
     H*                                                               *
     H*  05/23/01 C:62 PROJ:MCMNT #188   BASE ONLY                    *
     H*                CHANGE SF1PAG TO 12                            *
     H*                                                               *
     H*  11/03/00 C:62 PROJ:AR001                                     *
     H*                CHANGE THE SF1PAG VALUE FROM 12 TO 6.          *
     H*                                                               *
     H*  09/29/00 C:62 PROJ:MCMNT                                     *
     H*                SHOW ONLY ONE RECORD FOR GROUP INVOICES        *
     H*                                                               *
     H*  08/24/00 C:62 PROJ:MCMNT                                     *
     H*                REMOVE HEADER AMT PARMS.                       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FN0419F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FNMIMP     IF   E             DISK
     F                                     RENAME(PIM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FNMIML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PIM01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FNMIML1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PIM01:DBRCDF2)
     FNMIML8    IF   E           K DISK
     F                                     INFDS(DBFDSZ)
     FNMIML4    IF   E           K DISK
     F                                     INFDS(DBFDSY)
     F                                     RENAME(PIM01:PIM02)
     FNMIML5    IF   E           K DISK
     F                                     INFDS(DBFDSX)
     F                                     RENAME(PIM01:PIM03)
     FNMIML6    IF   E           K DISK
     F                                     INFDS(DBFDSW)
     F                                     RENAME(PIM01:PIM04)
     FNMIML7    IF   E           K DISK
     F                                     INFDS(DBFDSV)
     F                                     RENAME(PIM01:PIM05)
     FNMIML2    IF   E           K DISK
     F                                     INFDS(DBFDSU)
     F                                     RENAME(PIM01:PIM06)
     FNMIML3    IF   E           K DISK
     F                                     INFDS(DBFDST)
     F                                     RENAME(PIM01:PIM07)
     FNMARL6    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     FFMSMP     IF   E             DISK
     FFMSML     IF   E           K DISK
     F                                     RENAME(PSM01:PSM02)
     F                                     INFDS(DBFDSA)
     FFMFNP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FSMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
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
     D DBFDSU          DS
     D  DBRRNU               397    400B 0
     D DBFDST          DS
     D  DBRRNT               397    400B 0
     D*
     D                 DS
     D  ##L1                   1     78
     D  SCCNAM                16     46
     D                 DS
     D  ##L2                   1     78
     D  SCCAS#                53     73
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Invoices')
     D OPTNM2          C                   CONST('Re-Invoice')
     D OPTNM3          C                   CONST('              A/R In-
     D                                     quiry               ')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('N0440R')
     D COMAPI          C                   CONST('SA005R')
     D CHGNUM          C                   CONST('SA020R')
     D USPAPI          C                   CONST('S0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEGIN ADD *** 05/23/01 **************************************
     D SF1PAG          C                   CONST(12)

     I*** END   ADD *** 05/23/01 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
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
     C                   PARM      *BLANKS       ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKINV#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXINVD
     C*
     C     KEY5          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXINVD
     C                   KFLD                    KKINV#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY6          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXINVD
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY7          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C*
     C     KEY8          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKINV#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY9          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY10         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C*
     C     KEY11         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKINV#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY12         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY13         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C*
     C     KEY14         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C                   KFLD                    KKINV#
     C*
     C     KEY15         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C                   KFLD                    KKTRDT
     C*
     C     ARKY          KLIST
     C                   KFLD                    IMCO#
     C                   KFLD                    IMSUB#
     C                   KFLD                    IMFND
     C                   KFLD                    IMFYR
     C                   KFLD                    IMCASE
     C                   KFLD                    IMINV#
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSFYR
     C                   KFLD                    SSCASE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSER
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    IMCO#         KKCO#
     C     *LIKE         DEFINE    ARSUB#        KKSUB#
     C     *LIKE         DEFINE    ARFND         KKFND
     C     *LIKE         DEFINE    ARFYR         KKFYR
     C     *LIKE         DEFINE    ARCASE        KKCASE
     C     *LIKE         DEFINE    ARINV#        KKINV#
     C     *LIKE         DEFINE    ARTRDT        KKTRDT
     C     *LIKE         DEFINE    AREOB#        KKEOB#
     C     *LIKE         DEFINE    ARRCN         KKRCN
     C     *LIKE         DEFINE    IMTRDT        XXINVD
     C     *LIKE         DEFINE    UPUSER        KKUSER
     C*** BEGIN ADD *** 09/29/00 **************************************
     C     *LIKE         DEFINE    ARINV#        ZZINV#
     C*** END   ADD *** 09/29/00 **************************************
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
     C     SSINVD        WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRNZ        SCDBRR
     C   22              Z-ADD     DBRRN2        SCDBRR
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C*
     C   21              Z-ADD     DBRRNU        SCDBRR
     C   22              Z-ADD     DBRRNT        SCDBRR
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRNW        SCDBRR
     C   22              Z-ADD     DBRRNV        SCDBRR
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRNY        SCDBRR
     C   22              Z-ADD     DBRRNX        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SSFND         ORNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C     SSINVD        ORNE      *ZEROS
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
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C  N90*INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
AAA  C  N90SSSUB#        IFNE      *ZEROS
     C     SSFND         ORNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
     C   99              SETON                                        7190
     C*
AAA  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSSNAM
     C*
AAA  C                   ENDIF
     C*
BBB  C  N90SSFND         IFNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C   99              SETON                                        7290
     C*
BBB  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSGNAM
     C*
BBB  C                   ENDIF
     C*
CCC  C  N90SSFYR         IFNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C   99              SETON                                        7390
     C*
CCC  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSCNAM
     C*
CCC  C                   ENDIF
     C*
DD1  C     SSINVD        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSINVD
     C*
DD2  C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7490
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXINVD
     C*
DD2  C                   ENDIF
DD1  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   MOVEL     SCUSER        KKUSER
     C*
     C     UPKY          CHAIN(N)  PUP01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C* Check to see if Subscriber # is restricted.
     C*
     C     UPMRSU        IFEQ      'Y'
     C     UPSUB#        ANDNE     *ZEROS
     C*
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   SETON                                        16
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
     C                   MOVEL(P)  SMNAME        SSSNAM
     C*
     C                   ENDIF                                                  UPMRSU 'Y'
     C*
     C* Check to see if Group # is restricted.
     C*
     C     UPMRGR        IFEQ      'Y'
     C     UPSUB#        ANDNE     *ZEROS
     C     UPFND         ANDNE     *ZEROS
     C*
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   Z-ADD     UPFND         SSFND
     C                   SETON                                        1617
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
     C                   MOVEL(P)  SMNAME        SSSNAM
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
     C                   Z-ADD     *ZEROS        KKINV#
     C                   Z-ADD     *ZEROS        KKEOB#
     C                   Z-ADD     *ZEROS        KKRCN
     C                   Z-ADD     *HIVAL        KKTRDT
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKFYR
     C                   Z-ADD     *ZEROS        KKCASE
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKINV#
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTDAT'    DTCYMD
     C   22              MOVEL     SRDTO         KKTRDT
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKFYR
     C                   Z-ADD     *ZEROS        KKCASE
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              Z-ADD     SCINV#        KKINV#
     C   22              Z-ADD     SCTRDT        KKTRDT
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
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
     C* Subscriber Search
     C*
WH1  C     HFLD          WHENEQ    'SSSUB#'
     C     *IN16         ANDEQ     *OFF
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
AAA  C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
AAA  C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM01                              99
     C*
     C  N99              Z-ADD     SMSUB#        SSSUB#
     C  N99              MOVEL(P)  SMNAME        SSSNAM
     C                   SETON                                        32
     C*
AAA  C                   ENDIF                                                  ##KC = '1'
     C*
     C* Group Search
     C*
WH2  C     HFLD          WHENEQ    'SSFND'
     C     *IN17         ANDEQ     *OFF
     C*
     C                   Z-ADD     *ZEROS        ##RRN
     C*
     C     SSSUB#        IFNE      *ZEROS
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        71
     C*
     C                   ELSE
     C*
     C                   Z-ADD     DBRRNA        ##RRN
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C******IN90         IFEQ      *OFF
     C*****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL
     C*****              PARM      ' '           ##KC
     C*****              PARM      ' '           ##KL
     C*****              PARM                    ##RRN
     C*****
BBB  C*****##KC          IFEQ      '1'
     C*****##KL          OREQ      '1'
     C*****##RRN         OREQ      *ZEROS
     C*****
     C*****              SETON                                        32
     C*****
BBB  C*****              ELSE
     C*****
     C*****##RRN         CHAIN     PFN01                              99
     C*****
     C**N99              Z-ADD     FNFND         SSFND
     C**N99              MOVEL(P)  FNNAME        SSGNAM
     C**N99              MOVEL(P)  FNSUB#        SSSUB#
     C*****
     C*****SMKY          CHAIN     PSM02                              99
     C**N99              MOVEL(P)  SMNAME        SSSNAM
     C*****              SETON                                        33
     C*****
BBB  C*****              ENDIF                                                  ##KC = '1'
     C*****
     C*****              ENDIF
     C******** End Del ***** 12/18/24 ***************************
     C*
     C* Fund Year/Case Search
     C*
WH3  C     HFLD          WHENEQ    'SSFYR'
     C     HFLD          OREQ      'SSCASE'
     C*
B01  C     SSFND         IFNE      *ZEROS
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
XXX  C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        71
     C*
XXX  C                   ELSE
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
B02  C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        72
     C*
S02  C                   ELSE
     C*
     C                   Z-ADD     SSFND         ##FND
     C*
E02  C                   ENDIF
     C*
XXX  C                   ENDIF
     C*
S01  C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ##FND
     C*
E01  C                   ENDIF
     C*
B03  C     *IN90         IFEQ      *OFF
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL
     C                   PARM      SSSUB#        ##SUB#            5 0
     C                   PARM                    ##FND
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
B04  C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C*
     C                   SETON                                        33
     C*
S04  C                   ELSE
     C*
     C                   Z-ADD     ##FYR         SSFYR
     C                   Z-ADD     ##CASE        SSCASE
     C                   Z-ADD     ##FND         SSFND
     C                   Z-ADD     ##SUB#        SSSUB#
     C*
     C* get claimaint name
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
B05  C     *IN99         IFEQ      *OFF
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
E05  C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* Get subscriber name
     C*
     C     SMKY          CHAIN     PSM02                              99
     C*
B06  C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SMNAME        SSSNAM
     C*
E06  C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* Get group name
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
B06  C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  FNNAME        SSGNAM
     C*
E06  C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   SETON                                        34        pos cursor
     C*
E04  C                   ENDIF                                                  ##RTCD = A
     C*
E03  C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KGDO   - F7=A/R SUMMARY
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   CALL      'N0421R'
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FMCMP
     C                   PARM      SSSUB#        ##SUB#            5 0
     C                   PARM      SSFND         ##FND             3 0
     C                   PARM      SSFYR         ##FYR             3 0
     C                   PARM      SSCASE        ##CASE            7 0
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
     C                   Z-ADD     IMTRDT        SCTRDT
     C                   Z-ADD     IMINV#        SCINV#
     C                   Z-ADD     IMSUB#        SCSUB#
     C                   Z-ADD     IMFND         SCFND
     C                   Z-ADD     IMFYR         SCFYR
     C                   Z-ADD     IMCASE        SCCASE
     C*** BEGIN ADD *** 09/29/00 **************************************
     C                   Z-ADD     IMINV#        ZZINV#
     C*** END   ADD *** 09/29/00 **************************************
     C*
     C                   MOVEL(P)  IMINV#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXINV#            9
     C*
     C                   Z-ADD     IMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   MOVEL(P)  XXTRDT        ##NUM
     C                   CALL      CHGNUM        CNMPL2
     C                   MOVEL     ##NUM         X1TRDT           10
     C*
     C   21              MOVEL(P)  XXINV#        SCDATA
     C   21              MOVE      X1TRDT        SCDATA
     C   22              MOVEL(P)  X1TRDT        SCDATA
     C   22              MOVE      XXINV#        SCDATA
     C*
     C                   MOVE      IMCO#         #1CO#             3
     C                   MOVE      IMSUB#        #1SUB#            5
     C                   MOVE      IMFND         #1FND             3
     C                   MOVE      IMFYR         #1FYR             3
     C                   MOVE      IMCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM      *BLANK        ##L1             78
     C                   PARM      *BLANK        ##L2
     C*
     C                   Z-ADD     *ZEROS        SCINAM
     C                   Z-ADD     *ZEROS        SCPAAM
     C                   Z-ADD     *ZEROS        SCAJAM
     C*
     C     ARKY          CHAIN     PAR01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     ARTRTP        WHENEQ    'P'
     C                   ADD       ARAMT         SCPAAM
     C*
     C     ARTRTP        WHENEQ    'A'
     C                   ADD       ARAMT         SCAJAM
     C*
     C     ARTRTP        WHENEQ    'I'
     C                   ADD       ARAMT         SCINAM
     C*
     C                   ENDSL
     C*
     C     ARKY          READE     PAR01                                  99
     C*
     C                   ENDDO
     C*
     C     SCINAM        SUB       SCPAAM        SCTDUE
     C                   ADD       SCAJAM        SCTDUE
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Invoice detail
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   CALL      'N0425R'
     C                   PARM      @@CALL        @@CALL            1
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      SCDBRR        @@RRN2            9 0
     C*** BEGIN DEL *** 08/24/00 **************************************
     C**********           PARM SCINAM    ##IAMT  92
     C**********           PARM SCPAAM    ##PAMT  92
     C**********           PARM SCAJAM    ##AAMT  92
     C**********           PARM SCTDUE    ##TDUE  92
     C*** END   DEL *** 08/24/00 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C     *INKC         OREQ      *ON
     C*
     C                   SETOFF                                       KLKC
     C*                    SETON                     90
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - Re-invoice
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   CALL      'N7146C1'
     C                   PARM      OPTNM2        ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      KKCO#         ##CO              3 0
     C                   PARM      SCSUB#        ##SUB             5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCFYR         ##FYR             3 0
     C                   PARM      SCCASE        ##CASE            7 0
     C                   PARM      SCINV#        ##INV#            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - Cash posting
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'N0430R'
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0          NMIMP
     C                   PARM      SCINAM        ##IAMT            9 2
     C                   PARM      SCPAAM        ##PAMT            9 2
     C                   PARM      SCAJAM        ##AAMT            9 2
     C                   PARM      SCTDUE        ##TDUE            9 2
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
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKG         CASEQ     *ON           KGDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
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
     C                   MOVEL     OPTNM3        SCPRDN
     C*
     C*** BEGIN ADD *** 09/29/00 **************************************
     C                   Z-ADD     *ZEROS        ZZINV#
     C*** END   ADD *** 09/29/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     SSINVD        WHENNE    *ZEROS
     C   21KEY4          READE     PIM01                                  95
     C   22KEY4          READE     DBRCDF2                                95
     C*
     C     SSCASE        WHENNE    *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C   21KEY13         READE     PIM06                                  95
     C   22KEY13         READE     PIM07                                  95
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY10         READE     PIM04                                  95
     C   22KEY10         READE     PIM05                                  95
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY7          READE     PIM02                                  95
     C   22KEY7          READE     PIM03                                  95
     C*
     C                   OTHER
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     SSINVD        WHENNE    *ZEROS
     C   21KEY4          READPE    PIM01                                  95
     C   22KEY4          READPE    DBRCDF2                                95
     C*
     C     SSCASE        WHENNE    *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C   21KEY13         READPE    PIM06                                  95
     C   22KEY13         READPE    PIM07                                  95
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY10         READPE    PIM04                                  95
     C   22KEY10         READPE    PIM05                                  95
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY7          READPE    PIM02                                  95
     C   22KEY7          READPE    PIM03                                  95
     C*
     C                   OTHER
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
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
     C     SSINVD        WHENNE    *ZEROS
     C   21KEY5          SETGT     PIM01                              96
     C   22KEY6          SETGT     DBRCDF2                            96
     C*
     C     SSCASE        WHENNE    *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C   21KEY14         SETGT     PIM06                              96
     C   22KEY15         SETGT     PIM07                              96
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY11         SETGT     PIM04                              96
     C   22KEY12         SETGT     PIM05                              96
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY8          SETGT     PIM02                              96
     C   22KEY9          SETGT     PIM03                              96
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
     C*
     C     SSINVD        WHENNE    *ZEROS
     C   21KEY5          SETLL     PIM01                              96
     C   22KEY6          SETLL     DBRCDF2                            96
     C*
     C     SSCASE        WHENNE    *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C   21KEY14         SETLL     PIM06                              96
     C   22KEY15         SETLL     PIM07                              96
     C*
     C     SSFND         WHENNE    *ZEROS
     C   21KEY11         SETLL     PIM04                              96
     C   22KEY12         SETLL     PIM05                              96
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY8          SETLL     PIM02                              96
     C   22KEY9          SETLL     PIM03                              96
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
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SSSUB#        ANDNE     IMSUB#
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     IMFND
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSFYR         IFNE      *ZEROS
     C     SSFYR         ANDNE     IMFYR
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSCASE        IFNE      *ZEROS
     C     SSCASE        ANDNE     IMCASE
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSINVD        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXINVD
     C*
     C     XXINVD        IFNE      IMTRDT
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/29/00 **************************************
     C     IMINV#        IFEQ      ZZINV#
     C     IMFYR         ANDEQ     *ZEROS
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 09/29/00 **************************************
     C*
     C                   ENDSR
** Column headings
Opt   Invoice#   Date         Inv amt    Payments  Adjustments  Total Due
Opt    Date      Invoice#     Inv amt    Payments  Adjustments  Total Due
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
1066=Invoice detail         YYN
1077=Re-invoice             YYN
1088=Cash posting           YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=A/R summary           YYY
211F11=Change view          YYY
212F12=Cancel               NNY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
