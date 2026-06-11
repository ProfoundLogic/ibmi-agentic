     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0077R - WW Employer Contacts
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0050R                                             *
     H*                                                               *
     H* CALLS TO:  W0077R02                                           *
     H*                                                               *
     H* CREATED:   01/31/02 C:61  PROJECT WC1070                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/08/09 C:136 PROJECT: 133WGY                                *
     H*                OPEN W/W CONTACTS IN POSITION SORT FIRST FOR   *
     H*                LUBA (CLT133)                                  *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
     FW0077F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWMECP     IF   E             DISK
     F                                     RENAME(PEC01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMECL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEC01:DBRCDF)
      *
      *  Employer Master
     FWMEMP     IF   E             DISK
      *
      *  Group Master
     FWMFNL     IF   E           K DISK
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
      *
      * Effective and Cancel Date Arrays
      *
      *
      *
      *
     D***PY SCOPYWW01E
      *
      *
      *
     D***PY SCOPYWW01I
      *
      *
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  INFO1                  1     74
     D  I1LSN                  1     20
     D  I1FSN                 23     37
     D  I1PH#                 40     59
     D  I1CELL                61     75
      *
     D                 DS
     D  INFO2                  1     74
     D  I2LSN                  1     20
     D  I2FSN                 23     37
     D  I2MID                 40     40
     D  I2POSN                44     64
     D  I2PCNT                69     69
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D DELETD          C                   CONST('*Deleted')
     D OPTNM           C                   CONST('Employer Contacts')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0077R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          WMECP
     C                   PARM                    @@RRN1            9 0          WMEMP
      *
      *  NOTE: Relative Record for WMEMP must be passed in.
      *
      *
      *================================================================
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          WMECP
     C                   PARM      @@RRN1        ##RRN1            9 0          WMEMP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKPOSN
     C                   KFLD                    KKPCNT
      *
      *  Group Master
      *
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C     *LIKE         DEFINE    ECCO#         KKCO#
     C     *LIKE         DEFINE    ECFND         KKFND
     C     *LIKE         DEFINE    ECEMP#        KKEMP#
     C     *LIKE         DEFINE    ECDIV         KKDIV
     C     *LIKE         DEFINE    ECLSN         KKLSN
     C     *LIKE         DEFINE    ECFSN         KKFSN
     C     *LIKE         DEFINE    ECMID         KKMID
     C     *LIKE         DEFINE    ECPOSN        KKPOSN
     C     *LIKE         DEFINE    ECPCNT        KKPCNT
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHDSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * GETPH# - Get phone numbers screen 1
      *================================================================
     C     GETPH#        BEGSR
      *
      *  PHONE #
      *
     C                   MOVEL(P)  *BLANKS       XXCPH            20
      *
     C     ECAREA        IFNE      *ZERO
     C                   MOVEL(P)  ECAREA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C     ECPHN#        IFNE      *ZERO
     C                   MOVEL(P)  ECPHN#        XXTMP1            3
     C                   MOVE(P)   ECPHN#        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
     C                   ENDIF
      *
     C     ECEXT#        IFNE      *ZERO
     C                   MOVEL(P)  ECEXT#        XXTMP3            5
     C     '0'           CHECK     XXTMP3        X
     C                   SUBST(P)  XXTMP3:X      XXTMP             5
     C                   CAT(P)    'x':0         XXCPH
     C                   CAT(P)    XXTMP:0       XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  XXCPH         I1PH#
      *
      * CELL PHONE #
      *
     C                   MOVEL(P)  *BLANKS       XXCPH            20
      *
     C     ECCARA        IFNE      *ZERO
     C                   MOVEL(P)  ECCARA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C     ECCPHN        IFNE      *ZERO
     C                   MOVEL(P)  ECCPHN        XXTMP1            3
     C                   MOVE(P)   ECCPHN        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  XXCPH         I1CELL
     C                   ENDSR                                                  GETPH#
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE(P)   *BLANKS       KKLSN
     C                   MOVE(P)   *BLANKS       KKFSN
     C                   MOVE(P)   *BLANKS       KKMID
     C                   MOVE(P)   *BLANKS       KKPOSN
     C                   MOVE(P)   *BLANKS       KKPCNT
      *
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   MOVEL(P)  SCPST1        KKLSN
     C                   MOVEL(P)  SCPFSN        KKFSN
     C                   MOVEL(P)  *LOVAL        KKMID
     C                   MOVEL(P)  *LOVAL        KKPOSN
     C                   MOVEL(P)  *LOVAL        KKPCNT
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   MOVEL(P)  SCLSN         KKLSN
     C                   MOVEL(P)  SCFSN         KKFSN
     C                   MOVEL(P)  SCMID         KKMID
     C                   MOVEL(P)  SCPOSN        KKPOSN
     C                   MOVEL(P)  SCPCNT        KKPCNT
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C                   Z-ADD     EMCO#         SCCO#
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVEL(P)  ECLSN         SCLSN
     C                   MOVEL(P)  ECFSN         SCFSN
     C                   MOVEL(P)  ECMID         SCMID
     C                   MOVEL(P)  ECPOSN        SCPOSN
     C                   MOVEL(P)  ECPCNT        SCPCNT
      *
     C                   CLEAR                   SCDATA
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  ECLSN         I1LSN
     C                   MOVEL(P)  ECFSN         I1FSN
     C                   EXSR      GETPH#
     C                   MOVEL(P)  INFO1         SCDATA
      *  ZZZ
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  ECLSN         I2LSN
     C                   MOVEL(P)  ECFSN         I2FSN
     C                   MOVEL(P)  ECMID         I2MID
     C                   MOVEL(P)  ECPOSN        I2POSN
     C                   MOVEL(P)  ECPCNT        I2PCNT
     C                   MOVEL(P)  INFO2         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      *  Get Employer record
      *
     C     @@RRN1        CHAIN     PEM01                              LR
     C                   Z-ADD     EMCO#         KKCO#
     C                   Z-ADD     EMFND         KKFND
     C                   Z-ADD     EMEMP#        KKEMP#
     C                   Z-ADD     EMDIV         KKDIV
      *
      * Get CX date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     ED(X)         XXEFDT
     C                   Z-ADD     CD(X)         XXCXDT
      *
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
      *
     C     SRERR         COMP      *ON                                    20
      *
      * Get Header Information
      *
     C                   Z-ADD     EMFND         SCFND#
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C   60              MOVEL(P)  *BLANKS       SCNAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
      *
     C                   MOVEL(P)  ##EDN         SCEDN
      *
      *** BEG ADD ***** 05/08/09 *************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C     XXCLT#        IFEQ      '133'
     C                   Z-ADD     2             INFO#
     C                   ENDIF
      *** END ADD ***** 05/08/09 *************************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN
      *
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Opt Last Name             First Name       Phone #              Cell Phone #
Opt Last Name             First Name      M/I  Position              Primary
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
