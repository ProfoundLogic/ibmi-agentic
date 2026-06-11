     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  PROJECT: MC234 - WORK WITH BEAST RECORD
     H*     DATE: 09/28/00  C: 117
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/04/16 C:79 Support: 42706/099
     H*               On Add/Change load and protect State
     H*
     H* 12/14/00 C:079  PROJECT: FS218
     H*                 Add STSTP and logic to chain to get the state
     H*****************************************************************
     FF2411F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMBEP     IF   E             DISK
     F                                     RENAME(PBE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMBEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBE01:DBRCDF)
     FFMBEL5    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PBE01:DBRCDF2)
     F*** BEG ADD *** 12/14/00 ****************************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     F*** END ADD *** 12/14/00 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('BEAST Record')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2411R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D*
     D* Data Structures for VIEWS and INFO
     D*
     D                 DS
     D  VIEW1                  1     32
     D  V1AST                  6      7
     D  V1SYS                 16     16
     D  V1BTYP                26     26
     D*
     D                 DS
     D  VIEW2                  1     32
     D  V2SYS                  5      5
     D  V2BTYP                15     15
     D  V2AST                 25     26
     D*
     D                 DS
     D  INFO                   1     38
     D  XXPPLN                 4      5
     D  XXSTAT                15     15

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C*** BEG ADD *** 12/14/00 ****************************************
     C                   PARM                    @@RRN             9 0
     C*** END ADD *** 12/14/00 ****************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CO#
     C*** Beg Add *** 08/04/16 ***************************************
     C                   PARM      @@RRN         ##RRN2            9 0
     C*** End Add *** 08/04/16 ***************************************
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
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKAST
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    BEHEPG        KKHEPG
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
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
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*** BEG CHG *** 12/14/00 ****************************************
     C******************** MOVE *LOVAL    KKAST
     C                   MOVE      QQST          KKAST
     C*** END CHG *** 12/14/00 ****************************************
     C                   MOVE      *LOVAL        KKSYS
     C                   MOVE      *LOVAL        KKBTYP
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKAST
     C                   MOVEL     SCPST2        KKSYS
     C                   MOVEL     SCPSTA        KKBTYP
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCAST         KKAST
     C                   MOVEL     SCSYS         KKSYS
     C                   MOVEL     SCBTYP        KKBTYP
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   VIEW1
     C                   CLEAR                   VIEW2
     C                   CLEAR                   INFO
     C*
     C* Load data in hidden fields
     C*
     C                   MOVEL     BEAST         SCAST
     C                   MOVEL     BESYS         SCSYS
     C                   MOVEL     BEBTYP        SCBTYP
     C*
     C* Load INFO DS
     C*
     C                   MOVEL     BEPPLN        XXPPLN
     C                   MOVEL     BESTAT        XXSTAT
     C                   MOVE      INFO          SCDATA
     C*
     C* Load Views
     C*
     C                   SELECT
     C* VIEW 1
     C     *IN21         WHENEQ    '1'
     C                   MOVEL     BEAST         V1AST
     C                   MOVEL     BESYS         V1SYS
     C                   MOVEL     BEBTYP        V1BTYP
     C                   MOVEL     VIEW1         SCDATA
     C* VIEW 2
     C     *IN22         WHENEQ    '1'
     C                   MOVEL     BEAST         V2AST
     C                   MOVEL     BESYS         V2SYS
     C                   MOVEL     BEBTYP        V2BTYP
     C                   MOVEL     VIEW2         SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
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
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         QQST              2
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
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
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     BESTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/14/00 ****************************************
     C     QQST          IFNE      *BLANKS
     C     QQST          ANDNE     BEAST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END ADD *** 12/14/00 ****************************************
     C*
     C                   ENDSR
** Column headings
Opt  Adj State  System  Bill Type  Payment Plan  Status
Opt  System  Bill Type  Adj State  Payment Plan  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YNY
217F17=Subset               YYY
3
