     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('FTBNDDIR')
     H*
     H*****************************************************************
     H*  F4105R - WORK WITH WCMS CLAIM DEPT LETTERS   C:62 05/14/98   *
     H*                                                               *
     H*  CALLED FROM: F4105C                                          *
     H*  CALLS TO:    NONE
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/10/15  C:79  Support: 36524/144                           *
     H*                  Change from STBNDDIR to FTBNDDIR             *
     H*                                                               *
     H*  05/01/15  C:65  Project: SY251                               *
     H*                  Call SpiLetter to display new prompt/process *
     H*                  if system is flagged for Scriptura.          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF4105F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMDEP     IF   E             DISK
     F                                     RENAME(PDE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMDEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDE01:DBRCDF)
     FFMCMP     IF   E             DISK
     FUMPCP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEG CHG *** 02/11/99 ****************************************
     D*******       'WCMC Claim Depart-   C         OPTNM
     D*******       'ment Letters'
     D OPTNM           C                   CONST('WCMC Department -
     D                                     Letters')
     D*** END CHG *** 02/11/99 ****************************************
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F4105R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D*** begin add ***  04/08/15  *********************************************
     d xxCat           s              5
     d xxRrn           s              9  0

      /copy SprLetter
     D*** end add *****  04/08/15  *********************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          SMDEP
     C                   PARM                    @@RRN2            9 0          FMCMP
     C                   PARM                    @@TYPE            1            FMCMP
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
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
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDPCD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DECO#         KKCO#
     C     *LIKE         DEFINE    DEDPCD        KKDPCD
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*** BEGIN DEL *** 12/14/98 **************************************
     C*
     C********** SSSTAT    IFNE *BLANKS
     C*
     C**********           MOVE *ON       *IN11
     C*
     C**********           ENDIF
     C*** END   DEL *** 12/14/98 **************************************
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
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL     *BLANKS       KKDPCD
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKDPCD
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCDPCD        KKDPCD
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  DEDPCD        SCDPCD
     C*
     C                   MOVEL(P)  DEDPCD        SCDATA
     C                   MOVE      DENAME        SCDATA
     C*
     C                   MOVEL(P)  DEDLTY        SCDLTY
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVEL     *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** begin add ***  04/08/15  *********************************************
     C*
     C                   CALL      'SA047R'
     C                   PARM      *BLANK        QQScrip           1
     C*
     C                   If        QQScrip = 'Y'
     C*
     C                   Move      @@RRN2        xxRRN
     C*
     C                   Select
     C     @@TYPE        WhenEq    'C'
     C                   Eval      xxCat = 'MC'
     C     @@TYPE        WhenEq    'P'
     C                   Eval      xxCat = 'PC'
     C                   Endsl
     C*
      /free
                         DocWindow(xxRRN:xxCat);
      /end-free
     C*
     C                   Exsr      PgmEnd
     C                   Return
     C*
     C                   Endif
     C*** end add *****  04/08/15  *********************************************
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   SELECT
     C*
     C     @@TYPE        WHENEQ    'C'
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C     @@TYPE        WHENEQ    'P'
     C*
     C     @@RRN2        CHAIN     PPC01                              99
     C*
     C                   MOVE      PCCO#         #1CO#             3
     C                   MOVE      PCSUB#        #1SUB#            5
     C                   MOVE      PCFND         #1FND             3
     C                   MOVE      PCFYR         #1FYR             3
     C                   MOVE      PCCASE        #1CASE            7
     C*
     C                   ENDSL
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        #1L1             78
     C     SCL2HD        PARM      *BLANK        #1L2             78
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C*
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
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     DESTAT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     @@TYPE        IFNE      DEDLTY
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Dept    Description
** Description--------------MISN
1011=Select                 YYY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
