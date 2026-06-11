     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N0350R - WORK WITH EOMB'S INVOICES
     H*****************************************************************
     H*  Created: 03/08/99  C:56 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*
     FN0350F    CF   E             WORKSTN
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
     FFAEAL8    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEA01:DBRCDF)
     FFAEAL9    IF   E           K DISK
     FFAEOP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D DATA            S             74    DIM(1)
     D DTA1            S              1    DIM(74)                              VIEW 1 DATA
     D*
     D***PY SCOPYWW01E
     D*
     D                 DS
     D  ##MASK                 1     10
     D  XX1ST                  1      7
     D  XXLAST                 8     10
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB''s Invoices')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('N0164R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
     D NOINVM          C                   CONST('No invoices for thi-
     D                                     s EOMB')
     D X1PRDS          C                   CONST('RE-INVOICE FACILITY')
     D X2PRDS          C                   CONST('      ACCOUNTS RECE-
     D                                     IVABLE INQUIRY     ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN1            9 0          faeop
     C*
     C*  Add/change/display program parameter list
     C*
     C*** BEG CHG *** 03/18/99 ****************************************
     C     ACDPLX        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C* THE ADD/CHANGE/DISPLAY PROGRAM WILL BE AR INQUIRY
     C*
     C     ACDPL         PLIST
     C                   PARM                    ##PRDS           40
     C                   PARM      ' '           ##CALL            1
     C*** END CHG *** 03/18/99 ****************************************
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
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY11         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
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
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C     @@RRN1        CHAIN     PEO01                              99
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     0             KKRCN
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKRCN
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   ENDSR
     C*================================================================
     C* OPT6 - Reinvoice
     C*================================================================
     C     OPT6          BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EACO#         XXCO#
     C                   MOVE      EASUB#        XXSUB#
     C                   MOVE      EAFND         XXFND
     C                   MOVE      EAFYR         XXFYR
     C                   MOVE      EACASE        XXCASE
     C     EABINV        IFNE      *ZEROS
     C                   MOVE      EABINV        XXINV#
     C                   ENDIF
     C     EANINV        IFNE      *ZEROS
     C                   MOVE      EABINV        XXINV#
     C                   ENDIF
     C                   MOVE      SCINV#        XXINV#
     C                   MOVE      X1PRDS        XXPRDS
     C*
     C                   CALL      'N7146C3'
     C                   PARM                    XXPRDS           40
     C                   PARM                    XXCALL            1
     C                   PARM                    XXCO#             3
     C                   PARM                    XXSUB#            5
     C                   PARM                    XXFND             3
     C                   PARM                    XXFYR             3
     C                   PARM                    XXCASE            7
     C                   PARM                    XXINV#            9
     C                   ENDIF
     C                   SETON                                        31
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   SETOFF                                       31
     C                   CLEAR                   SCDATA
     C                   CLEAR                   SCRCN#
     C                   CLEAR                   SCINV#
     C                   CLEAR                   SCAMT
     C                   CLEAR                   SCSYS
     C                   CLEAR                   SCDATE
     C                   CLEAR                   XXAMT
     C                   CLEAR                   XXDATE
     C*
     C                   MOVE      EARCN         SCRCN#
     C*
     C                   SELECT
     C     EABINV        WHENNE    *ZEROS
     C                   MOVE      EABINV        SCINV#
     C     EANINV        WHENNE    *ZEROS
     C                   MOVE      EANINV        SCINV#
     C                   ENDSL
     C*
     C                   MOVE      SCINV#        XXINV             9
     C*
     C                   SELECT
     C     EABAMT        WHENNE    *ZEROS
     C*
     C     EABAMT        IFLT      *ZEROS
     C                   MOVE      '-'           XSIGN             1
     C                   ELSE
     C                   MOVE      *BLANKS       XSIGN
     C                   ENDIF
     C*
     C                   Z-ADD     EABAMT        SCAMT
     C                   MOVEL(P)  SCAMT         ##NUM            80
     C                   MOVEL(P)  'J'           ##MASK           10
     C                   Z-ADD     2             ##DEC             3 0
     C                   MOVE      *BLANKS       ##THOU            1
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXAMT1           12
     C                   MOVE      XXAMT1        XXAMT            10
     C*
     C     EANAMT        WHENNE    *ZEROS
     C*
     C     EANAMT        IFLT      *ZEROS
     C                   MOVE      '-'           XSIGN             1
     C                   ELSE
     C                   MOVE      *BLANKS       XSIGN
     C                   ENDIF
     C*
     C                   Z-ADD     EANAMT        SCAMT
     C                   MOVEL(P)  SCAMT         ##NUM
     C                   MOVEL(P)  'J'           ##MASK           10
     C                   Z-ADD     2             ##DEC
     C                   MOVE      *BLANKS       ##THOU
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXAMT1           12
     C                   MOVE      XXAMT1        XXAMT
     C                   ENDSL
     C*
     C     EABINV        IFNE      *ZEROS
     C                   MOVE      EASYS         SCSYS
     C                   ENDIF                                                  EABINV NE *ZEROS
     C*
     C     EANINV        IFNE      *ZEROS
     C                   MOVE      'N'           SCSYS
     C                   ENDIF                                                  EANINV NE *ZEROS
     C*
     C                   SELECT
     C     EABIDT        WHENNE    *ZEROS
     C                   Z-ADD     EABIDT        SCDATE
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   Z-ADD     0             ##DEC
     C                   MOVE      *BLANKS       ##THOU
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXDATE           10
     C     EANIDT        WHENNE    *ZEROS
     C                   Z-ADD     EANIDT        SCDATE
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   Z-ADD     0             ##DEC
     C                   MOVE      *BLANKS       ##THOU
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXDATE
     C                   ENDSL
     C*
     C                   SELECT
     C*
     C*** LOAD VIEW# 1 FIELDS
     C*
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCRCN#        DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     XXINV         DTA1(Y)
     C                   ADD       13            Y
     C                   MOVEA     XXAMT         DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     XSIGN         DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     SCSYS         DTA1(Y)
     C                   ADD       9             Y
     C                   MOVEA     XXDATE        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C                   ENDSL
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
     C     SCSEL         CASEQ     '6'           OPT6
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           @@KC
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      '1'           @@KL
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         ANDEQ     *OFF
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        9071
     C                   MOVE      '1'           RSTPGM
     C                   GOTO      ENDEND
     C                   ENDIF
     C*
     C                   ENDIF                                                  *INKC = OFF
     C*
     C     ENDEND        ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   Z-ADD     EOEOB#        SCEOMB
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
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
     C* GET HEADING LINES
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN1                         faeop
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C                   MOVEL(P)  X2PRDS        ##PRDS
     C                   MOVEL(P)  NOINVM        SCMESG
     C                   SETON                                        31
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY11         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY11         READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt  RCN#    INV#       Inv Amount   Inv Type     Inv Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=A/R Inquiry            YYY
1066=Reinvoice              YYY
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   YNN
212F12=Cancel               NYY
217F17=Subset               NNN
3
