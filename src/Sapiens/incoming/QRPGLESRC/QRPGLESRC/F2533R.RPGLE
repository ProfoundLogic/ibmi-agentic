     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F2533R - Work With Comprehensive and Component Codes
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: F2412R                                             *
     H* CALLS TO:  F2533R01 - Comprehensive and Component Code Info   *
     H*                                                               *
     H*****************************************************************
     H* Created on 01/16/02 c:62 for Project 114MAO                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FF2533F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMBNP     IF   E             DISK
     F                                     RENAME(PBN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMBNL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBN01:DBRCDF)
     FFMBNL2    IF   E           K DISK
     F                                     INFDS(DBFDSA)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D***PY SCOPYWW01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Comprehensive and C-
     D                                     omponent codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2533R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)

     I*================================================================
     I*  PLIST definitions
     I*================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
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
     C*
     C*  CONVERTS DATE FROM MDYY TO CYMD
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
     C     KEY1          KLIST
     C                   KFLD                    KKCPCD
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKCOCD
     C*
     C     KEY2          KLIST
     C                   KFLD                    SSCPCD
     C*
     C     KEY3          KLIST
     C                   KFLD                    SSCPCD
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKCOCD
     C*
     C     KEY4          KLIST
     C                   KFLD                    S1EFDT
     C*
     C     KEY5          KLIST
     C                   KFLD                    S1EFDT
     C                   KFLD                    KKCPCD
     C                   KFLD                    KKCOCD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    BNCPCD        KKCPCD
     C     *LIKE         DEFINE    BNCOCD        KKCOCD
     C     *LIKE         DEFINE    BNEFDT        KKEFDT
     C     *LIKE         DEFINE    BNEFDT        S1EFDT
     C*
     C/COPY SCOPYWW01C
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C     SSCPCD        WHENNE    *BLANKS
     C   21              Z-ADD     DBRRN         SCDBRR
     C     SSEFDT        WHENNE    *ZEROS
     C   21              Z-ADD     DBRRNA        SCDBRR
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSCPCD        IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVE      'Y'           VLDOPT            1
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*  Effective Date
     C*
     C     SSEFDT        IFNE      *ZEROS
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        8890
     C                   ELSE
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C                   ENDIF                                                  SRERR
     C                   ENDIF                                                  SSEFDT
     C*
     C* Status
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF                                                  SSSTAT
     C*
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C*
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKCPCD
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKCOCD
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C                   MOVEL(P)  SCPST1        KKCPCD
     C                   Z-ADD     SCPSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C                   MOVEL     *BLANKS       KKCOCD
     C                   ENDIF                                                  *IN21
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCCPCD        KKCPCD
     C   21              Z-ADD     SHEFDT        KKEFDT
     C   21              MOVEL     SCCOCD        KKCOCD
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     VLDOPT        IFEQ      'N'
     C                   EXSR      ASSRRN
     C                   ENDIF                                                  VLDOPT<>Y
     C*
     C                   MOVEL(P)  BNCPCD        SCCPCD
     C                   Z-ADD     BNEFDT        SHEFDT
     C                   Z-ADD     BNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     BNDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
     C                   MOVEL(P)  BNCOCD        SCCOCD
     C                   MOVEL(P)  BNSTAT        SCSTAT
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20                          Change Status
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C     SSCPCD        WHENNE    *BLANKS
     C     KEY2          READE     DBRCDF                                 95
     C     SSEFDT        WHENNE    *ZEROS
     C     KEY4          READE     PBN01                                  95
     C                   OTHER
     C                   READ      DBRCDF                                 95
     C                   ENDSL
     C*
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C     SSCPCD        WHENNE    *BLANKS
     C     KEY2          READPE    DBRCDF                                 95
     C     SSEFDT        WHENNE    *ZEROS
     C     KEY4          READPE    PBN01                                  95
     C                   OTHER
     C                   READP     DBRCDF                                 95
     C                   ENDSL
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C     SSCPCD        WHENNE    *BLANKS
     C     KEY3          SETGT     DBRCDF                             96
     C     SSEFDT        WHENNE    *ZEROS
     C     KEY5          SETGT     PBN01                              96
     C                   OTHER
     C     KEY1          SETGT     DBRCDF                             96
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C     SSCPCD        WHENNE    *BLANKS
     C     KEY3          SETLL     DBRCDF                             96
     C     SSEFDT        WHENNE    *ZEROS
     C     KEY5          SETLL     PBN01                              96
     C                   OTHER
     C     KEY1          SETLL     DBRCDF                             96
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*  Component code
     C*
     C     SSCPCD        IFNE      *BLANKS
     C     SSCPCD        ANDNE     BNCPCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*  Effective date
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     BNEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*  Status
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     BNSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
**
   Code        Date          Code         Date        Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
