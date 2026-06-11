     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0584R - WORK WITH EOMB'S CHECK REQUESTS
     H*****************************************************************
     H*  Created: 03/08/99  C:56 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*
     FW0584F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWARPP     IF   E             DISK
     F                                     RENAME(PRP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWARPLN    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRP01:DBRCDF)
     FWMPDL     IF   E           K DISK
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
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB''s Check Reques-
     D                                     ts')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0584R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D NOREQ           C                   CONST('No check requests f-
     D                                     or this EOMB')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN1            9 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*
     C     KEY11         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPEOB#        KKEOB#
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C     *LIKE         DEFINE    RPFEIN        KKFEIN
     C     *LIKE         DEFINE    RPMOD         KKMOD
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
     C                   ENDSR
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
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C                   Z-ADD     0             KKEOBS
     C*
     C** GET WCMS FUND NUMBER
     C*
     C                   MOVEL     ##CO#         #1CO#
     C                   MOVEL     @@SUB#        #1SUB#
     C                   MOVEL     @@FND         #1FND
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C     ##RTCD        IFNE      'F'
     C                   MOVE      ##WCF#        KKFND
     C                   ENDIF                                                  ##RTCD NE F
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKEOBS
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   SETOFF                                       31
     C                   CLEAR                   SCDATA
     C*
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         XXDATE           10
     C*
     C                   MOVE      RPEOBS        XXRCN             3
     C     RPAMT         IFLT      *ZEROS
     C                   MOVE      '-'           XSIGN             1
     C                   ELSE
     C                   MOVE      *BLANKS       XSIGN
     C                   ENDIF                                                  RPAMT LT 0
     C*
     C                   MOVEL(P)  RPAMT         ##NUM            80
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
     C                   MOVEL     RPFEIN        XXFEIN            9
     C                   MOVE      '-'           XXDASH            1
     C                   MOVEL     RPMOD         XXMOD             3
     C*
     C                   Z-ADD     RPFEIN        KKFEIN
     C                   Z-ADD     RPMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        XXNAME           20
     C                   ELSE
     C                   MOVE      *BLANKS       XXNAME
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** LOAD VIEW# 1 FIELDS
     C*
     C                   SELECT
     C*
     C     HD#           WHENEQ    1
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     XXRCN         DTA1(Y)
     C                   ADD       7             Y
     C                   MOVEA     XXAMT         DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     XSIGN         DTA1(Y)
     C                   ADD       2             Y
     C                   MOVEA     XXFEIN        DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     XXDASH        DTA1(Y)
     C                   ADD       2             Y
     C                   MOVEA     XXMOD         DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     XXNAME        DTA1(Y)
     C                   ADD       22            Y
     C                   MOVEA     XXDATE        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT21 - Check Info
     C*================================================================
     C     OPT21         BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   MOVE      RPCO#         #1CO#
     C                   MOVE      RPFND         #1FND
     C                   MOVE      RPFYR         #1FYR
     C                   MOVE      RPCASE        #1CASE
     C                   MOVE      RPEOB#        #1EOB#            9
     C                   CALL      'W0070R50'                           99
     C                   PARM      ' '           ##KC
     C     *INKL         PARM      ' '           ##KL
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM      ' '           @@CALL
     C                   PARM                    #1EOB#
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '21'          OPT21
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      @@SUB#        #1SUB#
     C                   MOVE      @@FND         #1FND
     C                   MOVE      @@FYR         #1FYR
     C                   MOVE      @@CASE        #1CASE
     C                   Z-ADD     @@EOB#        SCEOMB
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
     C                   SETON                                        12
     C*
     C                   MOVEL(P)  NOREQ         SCMESG
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
Opt  RCN#       Amount  Payee                                    Trans Dt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12121=Check Info            NYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
