     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* PROGRAM: F7090R03
     H* DESCRIPTION: CASE TRANSFER/DELETION PRE-CHECK PROGRAM
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*  11/02/99 C:31  Support: 20439
     H*                 A/R balance Xfer issue
     H*                 1. Change ARAMT calculation
     H*
     H*  09/27/99 C:31  Support: 19796
     H*                 Outstanding A/R issue
     H*                 1. Change check A/R routine
     H*
     H*  06/17/99 C:062 PROJECT: MC098 16387
     H*                 IF A/R EXIST FOR CASE AND THE A/R HAS AN AMOUNT
     H*                 THEN HAVE WINDOW POP UP TO NOTIFY USER THAT THE
     H*                 CASE CAN'T BE TRANSFERED OR DELETED.
     H*
     H*****************************************************************
     H*
     FF7090F03  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for screen heading)
     FFMCMP     IF   E             DISK
     F*  Physical file definition (for refreshing subfile record)
     FFAT3PQ    IF   E             DISK
     F                                     RENAME(PT302:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAT3LQ    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PT302:DBRCDF)
     F*** BEGIN ADD *** 06/17/99 **************************************
     FNMARLA    IF   E           K DISK
     F*** END   ADD *** 06/17/99 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D DATA            S             70    DIM(1)
     D DTA             S              1    DIM(70)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Case Transfer/Deleti-
     D                                     on Pre-Check program')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F7092R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D TRAOKY          C                   CONST('06T=Transfer        -
     D                                            YYN')
     D DELOKY          C                   CONST('07D=Delete          -
     D                                            YNN')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN1            9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@TD              1
     C                   PARM                    @@FLAG            1
     C                   PARM                    @@WCMS            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN1            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@FLAG        ##FLAG            1
     C*
     C*
     C     ACDPE         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN1            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@FLAG        ##FLAG            1
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
     C                   KFLD                    KKFILE
     C*** BEGIN ADD *** 06/17/99 **************************************
     C     ARKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** END   ADD *** 06/17/99 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    TQFILE        KKFILE
     C*** BEGIN ADD *** 06/17/99 **************************************
     C     *LIKE         DEFINE    ARCO#         KKCO#
     C     *LIKE         DEFINE    ARSUB#        KKSUB#
     C     *LIKE         DEFINE    ARFND         KKFND
     C     *LIKE         DEFINE    ARFYR         KKFYR
     C     *LIKE         DEFINE    ARCASE        KKCASE
     C*** END   ADD *** 06/17/99 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*** BEGIN ADD *** 06/17/99 **************************************
     C*================================================================
     C* CHKAR - CHECK TO SEE IF CASE HAS ANY AR RECORDS WITH A VALUE
     C*================================================================
     C     CHKAR         BEGSR
     C*
     C                   MOVEL     *OFF          STOP              1
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C*
     C*** BEGIN DEL *** 09/27/99 **************************************
     C********   ARKY      CHAINPAR01                99
     C********
     C********   *IN99     DOWEQ*OFF
     C********
     C********   ARAMT     IFNE *ZEROS
     C********
     C********             MOVEL*ON       STOP
     C********             LEAVE
     C********
     C********             ENDIF
     C********
     C********   ARKY      READEPAR01                    99
     C********
     C********             ENDDO
     C*** END DEL *** 09/27/99 ****************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     *IN98         IFEQ      *ON
     C                   Z-ADD     0             XXAMT            11 2
     C     ARKY          CHAIN     PAR01                              99
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN CHG *** 11/02/99 **************************************
     C********             ADD  ARAMT     XXAMT
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         XXAMT
     C                   ELSE
     C                   ADD       ARAMT         XXAMT
     C                   END
     C*** END ADD *** 11/02/99 ****************************************
     C     ARKY          READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C     XXAMT         IFNE      0
     C                   MOVEL     *ON           STOP
     C                   ENDIF                                                  AMT <> 0
     C                   ENDIF                                                  *IN98 = Off
     C*** END ADD *** 07/27/99 ****************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 06/17/99 **************************************
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
     C                   MOVE      *BLANKS       KKFILE
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *BLANKS       KKFILE
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCFILE        KKFILE
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   SETOFF                                       9093
     C*
     C                   CLEAR                   SCDATA
     C*
     C                   MOVEL(P)  TQFILE        SCFILE
     C                   MOVEL(P)  TQDESC        SCDESC
     C                   MOVEL(P)  TQNOTE        SCNOTE
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCFILE        DTA(Y)
     C                   Z-ADD     8             Y
     C                   MOVEA     SCDESC        DTA(Y)
     C                   Z-ADD     33            Y
     C                   MOVEA     SCNOTE        DTA(Y)
     C                   MOVEA     DTA           SCDATA
     C*
     C     @@TD          IFEQ      'T'
     C                   MOVEL     'T'           SCSELE
     C                   ELSE
     C                   MOVEL     'D'           SCSELE
     C                   ENDIF
     C*
     C                   MOVE      SCSELE        TQDECD
     C*
     C                   ENDSR
     C*================================================================
     C* OPT06  - OPTION T = TRANSFER
     C*================================================================
     C     OPT06         BEGSR
     C                   SETOFF                                       90
     C     @@TD          IFEQ      'T'
     C                   CALL      'F7092R'      ACDPL
     C                   ELSE
     C                   CALL      'F7092R'      ACDPE
     C                   ENDIF
     C*
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
     C     SCSEL         CASEQ     '6'           OPT06
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     *INKL         IFEQ      *OFF
     C*** BEGIN ADD *** 06/17/99 **************************************
     C*
     C* CHECK A/R TO SEE IF SHOULD CONTINUE WITH DELETE/TRANSFER
     C*
     C                   EXSR      CHKAR
     C*
     C     STOP          IFEQ      *OFF
     C*
     C*** END   ADD *** 06/17/99 **************************************
     C*
     C     @@FLAG        IFNE      'X'
     C                   SETON                                        60
     C                   ELSE
     C                   SETOFF                                       60
     C                   ENDIF
     C*
     C     @@WCMS        IFNE      'Y'
     C*
     C   60              CALL      'F7094R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN1            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@FLAG        ##FLAG            1
     C*
     C  N60@@TD          IFEQ      'T'
     C                   CALL      'F7092R'      ACDPL
     C                   ELSE
     C                   CALL      'F7092R'      ACDPE
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF                                                  *INKL = ON
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C                   ENDIF                                                  @@WCMS = Y
     C*** BEGIN ADD *** 06/17/99 **************************************
     C*
     C                   ELSE
     C*
     C                   EXFMT     SCR03
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ENDIF
     C*** END   ADD *** 06/17/99 **************************************
     C*
     C                   ENDIF                                                  *INKL = OFF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       6566
     C*
     C     @@TD          IFEQ      'T'
     C                   SETON                                        65
     C                   MOVEL     TRAOKY        OFD(5)
     C                   ELSE
     C                   SETON                                        66
     C                   MOVEL     DELOKY        OFD(6)
     C                   ENDIF
     C     @@WCMS        IFEQ      'Y'
     C                   SETON                                        75
     C                   ENDIF
     C*
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN(N)  PCM01                              99
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
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
Opt  File   File description          Notes
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
106T=Transfer               NNN
107D=Delete                 NNN
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
