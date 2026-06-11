     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE N0425R - WORK WITH INVOICE DETAIL
     H*
     H*****************************************************************
     H* Description: This program will allow the user to display all  *
     H*              A/R activity for a particular case.              *
     H*                                                               *
     H* Called by: N0419R - A/R INQUIRY - COMPANY LEVEL               *
     H*            N0420R - A/R INQUIRY - CASE LEVEL                  *
     H* Calls    : NONE                                               *
     H*                                                               *
     H* Created: 04/07/00  C:62  Project: AR001                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/24/02 C:62 PROJ:AR033                                     *
     H*                ALLOW 'N' SYSTEM TYPES TO ACCESS WORK WITH     *
     H*                PROVIDER BILLS.                                *
     H*                                                               *
     H*  03/23/01 C:62 PROJ:MCMNT                                     *
     H*                MAKE OPTION 8 UNAVAILABLE IN INQUIRY MODE.     *
     H*                                                               *
     H*  08/24/00 C:62 PROJ:MCMNT                                     *
     H*                UPDATE HEADER AMOUNTS.                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FN0425F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FNMARP     IF   E             DISK
     F                                     RENAME(PAR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FNMARL11   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAR01:DBRCDF)
     F*** BEGIN ADD *** 08/24/00 **************************************
     FNMARL6    IF   E           K DISK
     F*** END   ADD *** 08/24/00 **************************************
     F*
     F*  Secondary logical file definition (for displaying items)
     FNMARL10   IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAR01:DBRCDF2)
     FNMIMP     IF   E             DISK
     FNDTML1    IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FFTACP     IF   E             DISK
     FFTABL     IF   E           K DISK
     F                                     RENAME(PAC01:PAC02)
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDSA)
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDSB)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Invoice Detail')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('N0419R01')
     D COMAPI          C                   CONST('SA005R')
     D CHGNUM          C                   CONST('SA020R')
     D USPAPI          C                   CONST('S0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(6)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*** BEGIN DEL *** 08/24/00 **************************************
     C**********           PARM           @@IAMT  92
     C**********           PARM           @@PAMT  92
     C**********           PARM           @@AAMT  92
     C**********           PARM           @@TDUE  92
     C*** END   DEL *** 08/24/00 **************************************
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
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKTIME
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKTIME
     C*** BEGIN ADD *** 08/24/00 **************************************
     C     ARKY          KLIST
     C                   KFLD                    ARCO#
     C                   KFLD                    ARSUB#
     C                   KFLD                    ARFND
     C                   KFLD                    ARFYR
     C                   KFLD                    ARCASE
     C                   KFLD                    ARINV#
     C*** END   ADD *** 08/24/00 **************************************
     C*
     C     CMKY          KLIST
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
     C                   KFLD                    AREOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    AREOB#
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
     C     *LIKE         DEFINE    ARTIME        KKTIME
     C     *LIKE         DEFINE    AREOB#        KKEOB#
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSEOB#        ORNE      *ZEROS
     C     SSTRCD        ORNE      *BLANKS
     C     SSTRTP        ORNE      *BLANKS
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
     C     SCSEL         IFEQ      '8'
     C     SCEOB#        ANDEQ     *ZEROS
     C*
     C                   SETON                                        9074
     C*
     C                   ENDIF
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
     C*** BEGIN ADD *** 01/24/02 **************************************
     C     SCSYS         ANDNE     'N'
     C*** END   ADD *** 01/24/02 **************************************
     C*
     C                   SETON                                        9072
     C*
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '17'
     C     SCTMFL        ANDNE     '*'
     C*
     C                   SETON                                        9073
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   MOVEA     '000000'      *IN(31)
     C                   MOVEA     '000000'      *IN(71)
     C*
     C  N90*INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C     *IN90         IFEQ      *OFF
     C     SSSYS         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'SYS'         KKTBID
     C                   MOVEL(P)  SSSYS         KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7190
     C  N99              MOVEL(P)  GTDESC        SCSYSD
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCSYSD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SSTRCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SSTRCD        KKKEY
     C*
     C     TBKY          CHAIN     PAC02                              99
     C*
     C   99              SETON                                        7290
     C  N99              MOVEL(P)  ACDESC        SCTCDS
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCTCDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SSTRTP        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'ARTY'        KKTBID
     C                   MOVEL(P)  SSTRTP        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7390
     C  N99              MOVEL(P)  GTDESC        SCTTDS
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCTTDS
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
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN2        CHAIN     PIM01                              99
     C*
     C                   Z-ADD     IMCO#         KKCO#
     C                   Z-ADD     IMSUB#        KKSUB#
     C                   Z-ADD     IMFND         KKFND
     C                   Z-ADD     IMFYR         KKFYR
     C                   Z-ADD     IMCASE        KKCASE
     C                   Z-ADD     IMINV#        KKINV#
     C                   Z-ADD     *ZEROS        KKEOB#
     C                   Z-ADD     *HIVAL        KKTRDT
     C                   Z-ADD     *HIVAL        KKTIME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKEOB#
     C   21              Z-ADD     *HIVAL        KKTRDT
     C   21              Z-ADD     *HIVAL        KKTIME
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTDAT'    DTCYMD
     C   22              MOVEL     SRDTO         KKTRDT
     C   22              Z-ADD     *HIVAL        KKTIME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              Z-ADD     SCEOB#        KKEOB#
     C                   Z-ADD     SCTRDT        KKTRDT
     C                   Z-ADD     SCTIME        KKTIME
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSSYS'
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
     C                   SETON                                        3190
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SSSYS
     C                   MOVEL     GTDESC        SCSYSD
     C*
     C                   SETON                                        3290
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SSTRCD'
     C                   CALL      'N2305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        3390
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PAC01                              99
     C*
     C                   MOVEL     ACKEY         SSTRCD
     C                   MOVEL     ACDESC        SCTCDS
     C*
     C                   SETON                                        3490
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SSTRTP'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'ARTY'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        3490
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SSTRTP
     C                   MOVEL     GTDESC        SCTTDS
     C*
     C                   SETON                                        90
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
     C                   Z-ADD     ARTRDT        SCTRDT
     C                   Z-ADD     ARTIME        SCTIME
     C                   Z-ADD     AREOB#        SCEOB#
     C                   Z-ADD     ARRCN         SCRCN
     C                   MOVEL     ARSYS         SCSYS
     C                   MOVEL     ARTRCD        SCTRCD
     C                   MOVEL     ARTRTP        SCTRTP
     C                   MOVEL     ARDESC        SCDESC
     C                   Z-ADD     ARAMT         SCAMT
     C                   Z-ADD     ARCK#         SCCK#
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C  N99              Z-ADD     DBRRNB        SCDBR2
     C   99              Z-ADD     *ZEROS        SCDBR2
     C  N99              MOVEL     EOCERT        SCCERT
     C   99              MOVEL     *BLANKS       SCCERT
     C*
     C     TMKY          CHAIN     PTM01                              99
     C*
     C  N99              MOVEL     '*'           SCTMFL
     C   99              MOVEL     *BLANKS       SCTMFL
     C*
     C                   MOVEL(P)  AREOB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXEOB#            9
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   MOVEL(P)  XXTRDT        ##NUM
     C                   CALL      CHGNUM        CNMPL2
     C                   MOVEL     ##NUM         X1TRDT           10
     C*
     C   21              MOVEL(P)  XXEOB#        SCDATA
     C   21              MOVE      X1TRDT        SCDATA
     C   22              MOVEL(P)  X1TRDT        SCDATA
     C   22              MOVE      XXEOB#        SCDATA
     C*** BEGIN ADD *** 08/24/00 **************************************
     C     @RCDS         IFEQ      1
     C*
     C                   EXSR      SRAMT
     C*
     C                   ENDIF
     C*** END   ADD *** 08/24/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - Adjustment Posting
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'N0435R'
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@RRN2        ##RRN             9 0          NMIMP
     C                   PARM      SCEOB#        ##EOB#            9 0
     C                   PARM      SCRCN         ##RCN             3 0
     C                   PARM      SCSYS         ##SYS             1
     C*** BEGIN DEL *** 08/24/00 **************************************
     C**********           PARM @@IAMT    ##IAMT  92
     C**********           PARM @@PAMT    ##PAMT  92
     C**********           PARM @@AAMT    ##AAMT  92
     C**********           PARM @@TDUE    ##TDUE  92
     C*** END   DEL *** 08/24/00 **************************************
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
     C* OPT15 - View Pre-Cert
     C*================================================================
     C     OPT15         BEGSR
     C*
     C     CMKY          CHAIN     PCM01                              99
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
     C     CMKY          CHAIN     PCM01                              99
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
     C                   PARM      '1'           ##CALL
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
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '15'          OPT15
     C     SCSEL         CASEQ     '16'          OPT16
     C     SCSEL         CASEQ     '17'          OPT17
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
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      KKCO#         #1CO#             3
     C                   MOVE      KKSUB#        #1SUB#            5
     C                   MOVE      KKFND         #1FND             3
     C                   MOVE      KKFYR         #1FYR             3
     C                   MOVE      KKCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCHD1         PARM      *BLANK        ##L1             78
     C     SCHD2         PARM      *BLANK        ##L2             78
     C*
     C                   Z-ADD     IMINV#        SCINV#
     C                   Z-ADD     IMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINVD
     C*
     C*** BEGIN DEL *** 08/24/00 **************************************
     C**********           Z-ADD@@IAMT    SCIAMT
     C**********           Z-ADD@@PAMT    SCPAMT
     C**********           Z-ADD@@AAMT    SCAAMT
     C**********           Z-ADD@@TDUE    SCTDUE
     C*** END   DEL *** 08/24/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*** BEGIN ADD *** 08/24/00 **************************************
     C     *IN95         IFEQ      *ON
     C*
     C                   EXSR      SRAMT
     C*
     C                   ENDIF
     C*** END   ADD *** 08/24/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
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
     C*** BEGIN ADD *** 08/24/00 **************************************
     C*================================================================
     C* SRAMT - Calculate the header amounts.
     C*================================================================
     C     SRAMT         BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCPAMT
     C                   Z-ADD     *ZEROS        SCAAMT
     C*
     C     ARKY          CHAIN     PAR01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     ARTRTP        WHENEQ    'P'
     C                   ADD       ARAMT         SCPAMT
     C*
     C     ARTRTP        WHENEQ    'A'
     C                   ADD       ARAMT         SCAAMT
     C*
     C     ARTRTP        WHENEQ    'I'
     C                   ADD       ARAMT         SCIAMT
     C*
     C                   ENDSL
     C*
     C     ARKY          READE     PAR01                                  99
     C*
     C                   ENDDO
     C*
     C     SCIAMT        SUB       SCPAMT        SCTDUE
     C                   ADD       SCAAMT        SCTDUE
     C*
     C                   ENDSR
     C*** END   ADD *** 08/24/00 **************************************
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSSYS         ANDNE     ARSYS
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSEOB#        IFNE      *ZEROS
     C     SSEOB#        ANDNE     AREOB#
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSTRCD        IFNE      *BLANKS
     C     SSTRCD        ANDNE     ARTRCD
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSTRTP        IFNE      *BLANKS
     C     SSTRTP        ANDNE     ARTRTP
     C*
     C                   MOVEL     *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt Sys EOB#/Svc# Trans Date T/M Cd Tp  Description          Amount   Check#
Opt Sys Trans Date EOB#/Svc# T/M Cd Tp  Description          Amount   Check#
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
1088=Adjustment posting     YNN
11515=View Pre-Cert         YYN
11616=View EOMB             YYN
11717=T/M Activity          YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
