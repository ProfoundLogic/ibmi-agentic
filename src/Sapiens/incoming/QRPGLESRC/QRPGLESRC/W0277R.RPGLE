     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0077R - WW Agent Contacts
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0040R                                             *
     H*                                                               *
     H* CALLS TO:  W0277R02                                           *
     H*                                                               *
     H* CREATED:   01/18/06 C:168 PROJECT 93WFP                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* XX/XX/XX C:XX  PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
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
     FW0277F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)

      *
      *  Physical file definition (for refreshing subfile record)
     FWMA5P     IF   E             DISK
     F                                     RENAME(PAC01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMA5L     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAC01:DBRCDF)
      *
      *  Agent Master
     F*WMAGP   IF  E                    DISK
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
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
     D*                                       1  20 I1LSN
     D*                                      23  37 I1FSN
     D*                                      40  59 I1PH#
     D*                                      61  75 I1CELL
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
     D  I2STAT                69     69
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D DELETD          C                   CONST('*Deleted')
     D OPTNM           C                   CONST('Agent Contacts')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0277R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FEIN            9 0
     C                   PARM                    @@MOD             5 0
      *
      *  NOTE: Relative Record for WMAGP must be passed in.
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
     C                   PARM      @@CO#         ##CO              3 0          WMEMP
     C                   PARM      @@FEIN        ##FEIN            9 0          WMEMP
     C                   PARM      @@MOD         ##MOD             5 0          WMEMP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
      *
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C*                    KFLD           KKPOSN
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C     *LIKE         DEFINE    A5CO#         KKCO#
     C     *LIKE         DEFINE    A5FEIN        KKFEIN
     C     *LIKE         DEFINE    A5MOD         KKMOD
     C     *LIKE         DEFINE    A5LSN         KKLSN
     C     *LIKE         DEFINE    A5FSN         KKFSN
     C     *LIKE         DEFINE    A5MID         KKMID
     C     *LIKE         DEFINE    A5POSN        KKPOSN
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
      *
     C                   ENDSR                                                  CHDSSM
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
     C                   ENDSR                                                  ERRCK1
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
      *
     C                   ENDSR                                                  ERRCK2
      *================================================================
      * GETPH# - Get phone numbers screen 1
      *================================================================
     C     GETPH#        BEGSR
      *
      *  PHONE #
      *
     C                   MOVEL(P)  *BLANKS       XXCPH            20
      *
     C     A5AREA        IFNE      *ZERO
     C                   MOVEL(P)  A5AREA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C     A5PHN#        IFNE      *ZERO
     C                   MOVEL(P)  A5PHN#        XXTMP1            3
     C                   MOVE(P)   A5PHN#        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
     C                   ENDIF
      *
     C     A5EXT#        IFNE      *ZERO
     C                   MOVEL(P)  A5EXT#        XXTMP3            5
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
     C     A5CARA        IFNE      *ZERO
     C                   MOVEL(P)  A5CARA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C     A5CPHN        IFNE      *ZERO
     C                   MOVEL(P)  A5CPHN        XXTMP1            3
     C                   MOVE(P)   A5CPHN        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  XXCPH         I1CELL
      *
     C                   ENDSR                                                  GETPH#
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
      *
     C                   ENDSR                                                  INZSST
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE(P)   *BLANKS       KKLSN
     C                   MOVE(P)   *BLANKS       KKFSN
     C                   MOVE(P)   *BLANKS       KKMID
     C                   MOVE(P)   *BLANKS       KKPOSN
      *
     C                   ENDSR                                                  KEYINZ
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   MOVEL(P)  SCPST1        KKLSN
     C                   MOVEL(P)  SCPFSN        KKFSN
     C                   MOVEL(P)  *LOVAL        KKMID
     C                   MOVEL(P)  *LOVAL        KKPOSN
      *
     C                   ENDSR                                                  KEYSET
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C                   MOVEL(P)  SCLSN         KKLSN
     C                   MOVEL(P)  SCFSN         KKFSN
     C                   MOVEL(P)  SCMID         KKMID
     C                   MOVEL(P)  SCPOSN        KKPOSN
      *
     C                   ENDSR                                                  KEYSFL
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     A5CO#         SCCO#
     C                   Z-ADD     A5FEIN        SCFEIN
     C                   Z-ADD     A5MOD         SCMOD
     C                   MOVEL(P)  A5LSN         SCLSN
     C                   MOVEL(P)  A5FSN         SCFSN
     C                   MOVEL(P)  A5MID         SCMID
     C                   MOVEL(P)  A5POSN        SCPOSN
      *
     C                   CLEAR                   SCDATA
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  A5LSN         I1LSN
     C                   MOVEL(P)  A5FSN         I1FSN
     C                   EXSR      GETPH#
     C                   MOVEL(P)  INFO1         SCDATA
      *  ZZZ
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  A5LSN         I2LSN
     C                   MOVEL(P)  A5FSN         I2FSN
     C                   MOVEL(P)  A5MID         I2MID
     C                   MOVEL(P)  A5POSN        I2POSN
     C                   MOVEL(P)  A5STAT        I2STAT
     C                   MOVEL(P)  INFO2         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
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
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C                   ENDSR                                                  OTHFKS
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
     C                   ENDSR                                                  PGMEND
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      *  Get Employer record
      *
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FEIN        KKFEIN
     C                   Z-ADD     @@MOD         KKMOD
      *
     C                   Z-ADD     *ZEROS        X                 3 0
      *
     C                   ENDSR                                                  PGMSTR
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C   21KEY1          READE     DBRCDF                                 95
      *
     C                   ENDSR                                                  READ
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
      *
     C   21KEY1          READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
      *
     C   21KEY1A         SETGT     DBRCDF                             96
      *
     C                   ENDSR                                                  SETGT
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C   21KEY1A         SETLL     DBRCDF                             96
      *
     C                   ENDSR                                                  SETLL
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCDATA
      *
     C                   ENDSR                                                  SF1UPD
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN             9 0
      *
     C                   ENDSR                                                  SLCTRD
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
Opt Last Name             First Name      M/I  Position               Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
