     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0060R - Work With Ballast Values
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR BALLAST VALUES                          *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: E0060R02                                            *
     H*                                                               *
     H* CREATED: 07/18/2001                                           *
     H*                                                               *
     H* 06/14/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SSEFDT - INVALID DATE                               *
     H*  72       SSFRLO CANNOT BE LARGER THAN SSTOLO                 *
     H*  73       SPST - STATE MUST BE VALID                          *
     H*  74       SPEFDT - DATE MUST BE VALID                         *
     H*                                                               *
     H*****************************************************************
     FE0060F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FEMBAP     IF   E             DISK
     F                                     RENAME(PBA01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FEMBAL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBA01:DBRCDF)
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
      *
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  Data Structures
     D*================================================================
     D                 DS
     D  VIEW1                  1     60
     D  V1ST                   1      2
     D  V1EFDT                 7     16
     D  V1LOSS                21     37
     D  V1BALL                42     53
     D  V1STAT                60     60
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Ballast Values')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0060R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
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
     C                   PARM                    ##CONM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKLOSS
      *
     C     STKY          KLIST
     C                   KFLD                    K1ST             10
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    BACO#         KKCO#
     C     *LIKE         DEFINE    BAST          KKST
     C     *LIKE         DEFINE    BAEFDT        KKEFDT
     C     *LIKE         DEFINE    BALOSS        KKLOSS
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      *
     C     SSST          IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSFRLO        ORNE      *ZEROS
     C     SSTOLO        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
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
      *
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
      *
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7190
     C                   ENDIF                                                  IF SRERR = 1
     C                   ENDIF                                                  IF 90 OFF
      *
     C  N90SSFRLO        IFGT      SSTOLO
     C                   SETON                                        72  90
     C                   ENDIF
      *
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
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     *LOVAL        KKST
     C                   Z-ADD     *LOVAL        KKEFDT
     C                   Z-ADD     *LOVAL        KKLOSS
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVE(P)   SCPST1        KKST
     C*
     C                   MOVE(P)   SCPSTA        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   Z-ADD     SCPSTB        KKLOSS
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     SCST          KKST
      *
     C                   MOVEL(P)  SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C                   Z-ADD     SCLOSS        KKLOSS
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C*
     C                   MOVEL(P)  BAST          SCST
     C*
     C                   Z-ADD     BAEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCEFDT
      *
     C                   Z-ADD     BALOSS        SCLOSS
     C                   Z-ADD     BABALL        SCBALL
     C                   MOVEL(P)  BASTAT        SCSTAT
      *
     C                   MOVEL(P)  BAST          V1ST
      *
     C                   MOVEL(P)  SCEFDT        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C                   MOVEL     ##NUM         V1EFDT
      *
     C                   MOVEL(P)  SCLOSS        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM      'X,XXX.XX'    ##MASK
     C                   MOVEL     ##NUM         V1LOSS
      *
     C                   MOVEL(P)  SCBALL        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM      'X,XXX.XX'    ##MASK
     C                   MOVEL     ##NUM         V1BALL
      *
     C                   MOVEL(P)  BASTAT        V1STAT
     C                   MOVEL     VIEW1         SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
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
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
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
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
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
      *
     C     SSST          IFNE      *BLANKS
     C     SSST          ANDNE     BAST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   Z-ADD     SSEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C     XXEFDT        IFNE      *ZEROS
     C     XXEFDT        ANDNE     BAEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSFRLO        IFNE      *ZEROS
     C     SSFRLO        ANDGT     BALOSS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSTOLO        IFNE      *ZEROS
     C     SSTOLO        ANDLT     BALOSS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
** Column headings
Opt   St      Eff Date      Expected Losses   Ballast Value    Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
