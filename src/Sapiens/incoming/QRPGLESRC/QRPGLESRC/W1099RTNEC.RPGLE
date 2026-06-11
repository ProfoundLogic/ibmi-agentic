     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1099RT - 1099 Transmitter 'T' Record update/display
     H* ============================================================ *
     H*    1099 TRANSMITTER 'T' RECORD UPDATE/DISPLAY                *
     H* ============================================================ *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW1099FTNECCF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FWATNPNEC  UF A E             DISK    USROPN
     F                                     RENAME(PTN01NEC:DBRCDF)
     FWXTNP     IF   E             DISK
     FSMCOL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DTA             S              1    DIM(750)
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS                  INZ
     D  SCCTC#                 1     15  0
     D  SCAREA                 1      3  0
     D  SCPHN1                 4      6  0
     D  SCPHN2                 7     10  0
     D  SCEXT#                11     15  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('1099 Transmitter-
     D                                      ''T'' Record')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D CMD1            C                   CONST('ADDPFM FILE(WATNPNEC) -
     D                                     MBR(TRCD)')
     D CMD2            C                   CONST('OVRDBF FILE(WATNPNEC) -
     D                                     MBR(TRCD)')
     D CMD3            C                   CONST('CLRPFM FILE(WATNPNEC) -
     D                                     MBR(TRCD)')
     D CMD4            C                   CONST('DLTOVR FILE(WATNPNEC)')
     D*
     D*** Beg Add ***** 02/13/12 **********************************
     D XXNAME          C                   CONST('SUPPORT DEPARTMENT')
     D*** End Add ***** 02/13/12 **********************************
      ***** BEG ADD ***** 02/17/06 ************************************
      ***** BEG DEL ***** 01/28/10 ************************************
     D***           'FISERV INSURANCE SO- C         VNAME
     D***           'LUTIONS, INC. '
     D***           '12001 RESEARCH PRKW- C         VADDR
     D***           'Y. SUITE 200'
     D***           'ORLANDO'             C         VCITY
     D***           'FL'                  C         VST
     D***           '32826'               C         VZIP
     D***           'SUPPORT DEPARTMENT'  C         VCNAME
     D***           '4073825920123'       C         VCPHON
     D***           'wcsupport@fiserv.co- C         VCEMAL
     D***           'm'
      ***** END DEL ***** 01/28/10 ************************************
      ***** END ADD ***** 02/17/06 ************************************
     D*
      ***** BEG DEL ***** 02/17/06 ************************************
      ***** BEG ADD **** 10/11/02 *************************************
     D************  'PROGRESSIVE DATA SO- C         VNAME
     D************  'LUTIONS, INC. '
     D************  '1300 N. SEMORAN BLV- C         VADDR
     D************  'D. SUITE 200'
     D************  'ORLANDO'             C         VCITY
     D************  'FL'                  C         VST
     D************  '32807'               C         VZIP
     D************  'SUPPORT DEPARTMENT'  C         VCNAME
     D************  '4073825920123'       C         VCPHON
     D************  'support@progressdat- C         VCEMAL
     D************  'a.com'
      ***** END DEL ***** 02/17/06 ************************************
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     I*
      ***** END ADD **** 10/11/02 *************************************
     I*

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
      ***** BEG ADD **** 10/11/02 *************************************
      *                                   QQMODE = 'D' - DISPLAY
      *                                   QQMODE = 'P' - COPY
      *                                   QQMODE = 'C' - UPDATE
      *
     C                   PARM                    @@TAPD            1
     C                   PARM                    @@CCNT            8
     C                   PARM                    @@SEQN            8
      *
     C                   MOVE      @@TAPD        XXTAPD            1
      ***** END ADD **** 10/11/02 *************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SCYEAR        SVYEAR
     C     *LIKE         DEFINE    SCYEAR        SVSCYR
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* COUNTB - Count the number of 'B' Records in a particular member
     C*================================================================
     C*
     C     COUNTB        BEGSR
     C*
     C                   MOVEL(P)  CMD2          XXCMD            36
     C     'IRS'         CAT(P)    TNCO#:0       XXMBR            10
     C                   CAT       TNGRP:0       XXMBR
     C                   CAT       ')':0         XXMBR
     C                   MOVE      XXMBR         XXCMD
     C*
     C                   CALL      'QCMDEXC'                            99      OVRDBF
     C                   PARM      XXCMD         CMDSTR           50
     C                   PARM      36            LENGTH           15 5
     C*
     C                   OPEN      WATNPNEC
     C*
      ***** BEG CHG **** 10/11/02 *************************************
     C* Change file read to allow update for sequence # (Yr 2002 Chg)
     C***************      READ WATNP               N    99
     C                   READ      WATNPNEC                               99
      ***** END CHG **** 10/11/02 *************************************
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     TNRTYP        IFEQ      'B'
     C                   ADD       1             XXCNTB
     C                   ENDIF                                                  TNRTYP=B
      ***** BEG ADD **** 10/11/02 *************************************
     C     TNRTYP        IFEQ      'A'
     C                   ADD       1             XXCNTA
     C                   ENDIF                                                  TNRTYP=B
      * Add Seq nbr to WATNPNEC records in postion 550-507 (for 2002 Yr Chg)
     C                   CLEAR                   DTA
     C                   MOVEA     TNDATA        DTA(2)
     C                   ADD       1             TNCNT
     C                   MOVE      TNCNT         TNCNTA            8
     C                   Z-ADD     500           X
     C                   MOVEA     TNCNTA        DTA(X)
     C                   MOVEA     DTA(2)        TNDATA
     C                   UPDATE    DBRCDF
      ***** END ADD **** 10/11/02 *************************************
     C*
      ***** BEG CHG **** 10/11/02 *************************************
     C*************        READ WATNP               N    99
     C                   READ      WATNPNEC                               99
      ***** END CHG **** 10/11/02 *************************************
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   CLOSE     WATNPNEC
     C*
     C                   CALL      'QCMDEXC'                            99      DLTOVR
     C                   PARM      CMD4          CMDSTR
     C                   PARM      22            LENGTH
     C*
     C                   ENDSR                                                  COUNTB
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C  N90SCYEAR        IFLE      *ZERO
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCYEAR<=*ZERO
     C*
     C  N90SCYEAR        IFNE      SVYEAR
     C     SCYEAR        ANDNE     SVSCYR
     C*
     C                   CALL      'WA033R'                             99
     C*
     C                   SETON                                        72  90
     C                   Z-ADD     SCYEAR        SVSCYR
     C*
     C                   ENDIF                                                  SCYEAR<>SVYEAR
     C*
     C  N90SCTYPE        IFNE      'O'
     C     SCTYPE        ANDNE     'R'
     C     SCTYPE        ANDNE     'T'
     C     SCTYPE        ANDNE     'C'
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCTYPE<>O/R/T/C
     C*
     C  N90SCTCC         IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCTCC=*BLANKS
     C*
     C  N90SCCTCT        IFEQ      *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCCTCT=*BLANKS
     C*
     C  N90SCCTC#        IFEQ      *ZERO
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCYEAR<=*ZERO
     C*
     C***  BEGIN DEL  ***  03/01/11  *********************************
     C***  N90      SCTYPE    IFEQ 'R'
     C*
     C***  N90      SCFLNM    IFEQ *BLANKS
     C*
     C***  BEGIN ADD  ***  03/10/99  *********************************
     C***           CHECK1    IFEQ *ON
     C***  END   ADD  ***  03/10/99  *********************************
     C***                     SETON                     77  90
     C***  BEGIN ADD  ***  03/10/99  *********************************
     C***                     MOVE *OFF      CHECK1
     C***                    ENDIF                           CHECK1=*ON
     C***  END   ADD  ***  03/10/99  *********************************
     C*
     C***                  ENDIF                           SCFLNM=*BLANKS
     C*
     C***  N90      SCRPLC    IFEQ *BLANKS
     C***                     SETON                     78  90
     C***                  ENDIF                           SCRPLC=*BLANKS
     C*
     C***                  ENDIF                           SCTYPE=R
     C*
     C***  END   DEL  ***  03/01/11  *********************************
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEA     DTA(16)       SCTCC
     C                   Z-ADD     304           X
     C                   MOVEA     DTA(X)        SCCTCT
     C                   Z-ADD     344           X
     C                   MOVEA     DTA(X)        XXCTC#           15
     C                   MOVE      XXCTC#        SCCTC#
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C     ##CO#         CHAIN     PCO01                              99
     C*
     C                   CLEAR                   DTA
     C*
     C                   MOVEL     'T'           TNRTYP
     C                   MOVEL     SCYEAR        TPTYR             4
     C*
     C     SCYEAR        IFLT      SVYEAR
     C                   MOVEL     'P'           TPYR              1
     C                   ENDIF                                                  SCYEAR<SVYEAR
     C*
     C                   MOVEL     COFEIN        TPRIN             9
     C                   MOVEL     SCTCC         TTCC              5
     C*** BEG CHG 03/01/11***************************************
     C***                  MOVELSCRPLC    TRPLC   2
     C                   MOVEL     *BLANKS       TRPLC             2
     C*** END CHG 03/01/11***************************************
     C*
     C     SCTYPE        IFEQ      'T'
     C                   MOVE      SCTYPE        TTEST             1
     C                   ENDIF                                                  SCTYPE=T
     C*
     C                   MOVEL(P)  CONAME        TTRNM            40
     C                   MOVEL(P)  CONAME        TCONM            40
     C                   MOVEL(P)  COADR1        TCOADR           40
     C                   MOVEL(P)  COCTY         TCOCTY           40
     C                   MOVEL(P)  COST          TCOST             2
     C                   MOVEL(P)  COZIP         TCOZIP            9
     C*
     C                   MOVE(P)   COZIP         ZIP2              4
      ***** BEG ADD **** 01/28/10 *************************************
     C                   MOVEL(P)  CONAME        VNAME            40
     C                   MOVEL(P)  COADR1        VADDR            40
     C                   MOVEL(P)  COCTY         VCITY            40
     C                   MOVEL(P)  COST          VST               2
     C                   MOVEL(P)  COZIP         VZIP              5
     C*** Beg Chg ***** 02/13/12 **********************************
     C********             MOVELCOANAM    VCNAME 40 P
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  COANAM        VCNAME           40
     C                   ELSE
     C                   MOVEL     XXNAME        VCNAME
     C                   ENDIF
     C*** End Chg ***** 02/13/12 **********************************
     C                   MOVEL(P)  COAREA        VCPHON           10
     C                   MOVE      COPHN#        VCPHON
      ***** BEG ADD **** 01/28/10 *************************************
     C*
     C     ZIP2          IFEQ      '0000'
     C                   MOVE      '    '        TCOZIP
     C                   ENDIF                                                  ZIP2='0000'
     C*
     C                   MOVE      XXCNTB        TTTLP             8
     C                   MOVEL(P)  SCCTCT        TCTCT            40
     C                   MOVEL(P)  SCCTC#        TCTCT#           15
     C*
     C     SCEXT#        IFEQ      *ZEROS
     C                   MOVE      '     '       TCTCT#
     C                   ENDIF                                                  SCEXT#=*ZEROS
      *
      ***** BEG CHG **** 10/11/02 *************************************
     C************         MOVELSCEMAL    TEMAL  35 P
      *
      ***** BEG CHG ***** 12/01/06 ************************************
     C********   HI:LO     XLATESCEMAL    TEMAL  35 P      MAKE ALL L.C.
     C     HI:LO         XLATE(P)  SCEMAL        TEMAL            50            MAKE ALL L.C.
      ***** END CHG ***** 12/01/06 ************************************
      *
     C     XXTAPD        IFEQ      'Y'
     C                   MOVEL(P)  'LS'          TFINC
     C                   ELSE
     C                   MOVEL(P)  '  '          TFINC             2
     C                   ENDIF
     C*************        MOVEL'LS'      TFINC   2 P
      ***** END CHG **** 10/11/02 *************************************
     C*
     C***** BEG  DEL  03/01/11 ***************************************
     C***                  MOVELSCFLNM    TFLNM  15 P
     C                   MOVEL(P)  *BLANKS       TFLNM            15
     C***** END  DEL  03/01/11 ***************************************
     C*
     C                   MOVEA     TPTYR         DTA(2)
     C                   MOVEA     TPYR          DTA(6)
     C                   MOVEA     TPRIN         DTA(7)
     C                   MOVEA     TTCC          DTA(16)
     C                   MOVEA     TRPLC         DTA(21)
     C                   MOVEA     TTEST         DTA(28)
     C                   MOVEA     TTRNM         DTA(30)
     C*
     C                   Z-ADD     110           X                 3 0
     C                   MOVEA     TCONM         DTA(X)
     C                   Z-ADD     190           X
     C                   MOVEA     TCOADR        DTA(X)
     C                   Z-ADD     230           X
     C                   MOVEA     TCOCTY        DTA(X)
     C                   Z-ADD     270           X
     C                   MOVEA     TCOST         DTA(X)
     C                   Z-ADD     272           X
     C                   MOVEA     TCOZIP        DTA(X)
     C                   Z-ADD     296           X
     C                   MOVEA     TTTLP         DTA(X)
     C                   Z-ADD     304           X
     C                   MOVEA     TCTCT         DTA(X)
     C                   Z-ADD     344           X
     C                   MOVEA     TCTCT#        DTA(X)
      *
      ***** BEG CHG **** 10/11/02 *************************************
     C                   Z-ADD     359           X
     C                   MOVEA     TEMAL         DTA(X)                         EMAIL ADDR
      *
      ***** BEG CHG ***** 12/01/06 ************************************
      * Remove TFINC to make room for expanded email address, shift
     C*************        Z-ADD394       X
     C                   Z-ADD     409           X
      ***** END CHG ***** 12/01/06 ************************************
     C                   MOVEA     TFINC         DTA(X)
      *
      ***** BEG DEL ***** 12/01/06 ************************************
     C*************        Z-ADD396       X
     C***************      MOVEATFLNM     DTA,X
      ***** END DEL ***** 12/01/06 ************************************
      *
     C***********          Z-ADD359       X
     C***********          MOVEATFINC     DTA,X
     C***********          Z-ADD361       X
     C***********          MOVEATFLNM     DTA,X
      ***** END CHG **** 10/11/02 *************************************
      *
      ***** BEG ADD **** 10/11/02 *************************************
     C                   Z-ADD     1             #XSEQ#            8 0
     C                   MOVE      #XSEQ#        T#SEQ#            8
     C                   Z-ADD     500           X
     C                   MOVEA     T#SEQ#        DTA(X)
     C                   Z-ADD     518           X
     C                   MOVEA     'V'           DTA(X)
     C                   Z-ADD     519           X
     C                   MOVEA     VNAME         DTA(X)
     C                   Z-ADD     559           X
     C                   MOVEA     VADDR         DTA(X)
     C                   Z-ADD     599           X
     C                   MOVEA     VCITY         DTA(X)
     C                   Z-ADD     639           X
     C                   MOVEA     VST           DTA(X)
     C                   Z-ADD     641           X
     C                   MOVEA     VZIP          DTA(X)
     C                   Z-ADD     650           X
     C                   MOVEA     VCNAME        DTA(X)
     C                   Z-ADD     690           X
     C                   MOVEA     VCPHON        DTA(X)
      ***** BEF DEL **** 01/18/07 *************************************
     C***********          Z-ADD705       X
     C***********          MOVEAVCEMAL    DTA,X
      ***** END DEL **** 01/18/07 *************************************
     C*
      ***** END ADD **** 10/11/02 *************************************
     C*
     C                   MOVEA(P)  DTA(2)        TNDATA
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   CLOSE     WATNPNEC
     C*
     C                   CALL      'QCMDEXC'                            99      DLTOVR
     C                   PARM      CMD4          CMDSTR
     C                   PARM      19            LENGTH
     C*
     C     1             SETLL     WXTNP
     C                   READ      WXTNP                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      UPDATA
     C*
     C                   READ      WXTNP                                  99
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
      ***** BEG ADD **** 10/11/02 *************************************
      * Save counts for creating the "F" Record
     C                   MOVE      XXCNTA        @@CCNT
     C                   ADD       1             TNCNT
     C                   MOVE      TNCNT         @@SEQN
      ***** END ADD **** 10/11/02 *************************************
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C                   Z-ADD     *ZEROS        XXCNTB            8 0
     C***  BEGIN ADD  ***  03/10/99  *********************************
     C                   MOVE      *ON           CHECK1            1
     C***  END   ADD  ***  03/10/99  *********************************
     C*
     C***  BEGIN ADD  ***  10/11/02  *********************************
     C                   Z-ADD     *ZEROS        XXCNTA            8 0
     C                   Z-ADD     1             TNCNT             8 0
     C***  END   ADD  ***  10/11/02  *********************************
     C*** Beg Add ***** 02/13/12 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End Add *** 02/13/12 **********************************      ****
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      DSDATE        SCYEAR
     C                   SUB       1             SCYEAR
     C                   Z-ADD     SCYEAR        SVYEAR
     C                   Z-ADD     SCYEAR        SVSCYR
     C*
     C                   READ      WXTNP                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      COUNTB
     C*
     C                   READ      WXTNP                                  99
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   CALL      'QCMDEXC'                            99      ADDPFM
     C                   PARM      CMD1          CMDSTR
     C                   PARM      31            LENGTH
     C*
     C                   CALL      'QCMDEXC'                            99      OVRDBF
     C                   PARM      CMD2          CMDSTR
     C                   PARM      31            LENGTH
     C*
     C                   OPEN      WATNPNEC
     C*
     C                   READ(N)   WATNPNEC                               99
     C*
     C  N99              MOVEA(P)  TNDATA        DTA(2)
     C*
     C                   CLOSE     WATNPNEC
     C*
     C                   OPEN      WATNPNEC
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATA - Update the file indicators in the 'A' records
     C*================================================================
     C*
     C     UPDATA        BEGSR
     C*
     C                   MOVEL(P)  CMD2          XXCMD            36
     C     'IRS'         CAT(P)    TNCO#:0       XXMBR            10
     C                   CAT       TNGRP:0       XXMBR
     C                   CAT       ')':0         XXMBR
     C                   MOVE      XXMBR         XXCMD
     C*
     C                   CALL      'QCMDEXC'                            99      OVRDBF
     C                   PARM      XXCMD         CMDSTR           50
     C                   PARM      36            LENGTH           15 5
     C*
     C                   OPEN      WATNPNEC
     C*
     C                   READ      WATNPNEC                               99
     C*
     C     *IN99         DOWEQ     *OFF
      *
      ***** BEG ADD **** 10/11/02 *************************************
     C                   ADD       1             #XSEQ#
     C                   MOVE      #XSEQ#        T#SEQ#
     C                   Z-ADD     500           X
     C                   MOVEA     T#SEQ#        DTA(X)
     C                   MOVEA(P)  TNDATA        DTA(2)
      ***** END ADD **** 10/11/02 *************************************
     C*
     C     TNRTYP        IFEQ      'A'
     C*
      ***** BEG DEL **** 10/11/02 *************************************
     C************         MOVEATNDATA    DTA,2     P
      ***** END DEL **** 10/11/02 *************************************
     C                   MOVEA     '   '         DTA(48)
     C*
     C                   SELECT
     C     SCTYPE        WHENEQ    'O'
     C                   MOVEA     '1'           DTA(48)
     C     SCTYPE        WHENEQ    'R'
     C                   MOVEA     '1'           DTA(49)
     C     SCTYPE        WHENEQ    'C'
     C                   MOVEA     '1'           DTA(50)
     C                   ENDSL
     C*
     C                   MOVEA(P)  DTA(2)        TNDATA
     C*
     C                   UPDATE    DBRCDF
      *
      ***** BEG ADD **** 10/11/02 *************************************
      *
     C                   ELSE
      *
     C                   UPDATE    DBRCDF
      ***** END ADD **** 10/11/02 *************************************
     C*
     C                   ENDIF                                                  TNRTYP=A
     C*
     C***  BEGIN CHG  ***  03/29/00  *********************************
     C**********           READ WATNP               N    99
     C                   READ      WATNPNEC                               99
     C***  END   CHG  ***  03/29/00  *********************************
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   CLOSE     WATNPNEC
     C*
     C                   CALL      'QCMDEXC'                            99      DLTOVR
     C                   PARM      CMD4          CMDSTR
     C                   PARM      19            LENGTH
     C*
     C                   ENDSR                                                  UPDATA
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLOSE     WATNPNEC
     C*
     C                   CALL      'QCMDEXC'                            99      CLRPFM
     C                   PARM      CMD3          CMDSTR
     C                   PARM      31            LENGTH
     C*
     C                   OPEN      WATNPNEC
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WATNPNEC
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
