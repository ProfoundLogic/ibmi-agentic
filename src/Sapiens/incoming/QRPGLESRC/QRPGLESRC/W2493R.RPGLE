     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H******************************************************************
     H/TITLE W2493R - SYSTEM TABLE FILE SEARCH                        *
     H******************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM DISPLAYS THE SYSTEM TABLE   *
     H*    KEY LIST FOR THE SELECTED TABLE.                           *
     H*                                                               *
     H* CALLED FROM:  MENU                                            *
     H*                                                               *
     H* CALLS TO:  W2493R02                                           *
     H*                                                               *
     H* CREATED:  08/07/96 PROJECT: WC523  REWRITTEN: 04/04/04        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/04/09 C:62 PROJECT WC1807                                 *
     H*                Add pre-subset to type of qualifier.           *
     H*                                                               *
     H*  04/02/07 C:62 PROJECT WA114 54108                            *
     H*                CORRECT POSITION TO ISSUE.                     *
     H*                                                               *
     H* 01/03/05  C: 132  WA102 44503                                 *
     H*                 - Value Heading does not show on Add record   *
     H*                 - Status of "A" not appearing on Add record   *
     H*                                                               *
     H*  07/23/04 C:62 PROJECT SY068                                  *
     H*                COMPILE ONLY                                   *
     H*                                                               *
     H*****************************************************************
     FW2493F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWTGTL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PGT01:DBRCDF)
     F*  Secondary logical file definition (for displaying items)
     FWTGTL3    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PGT01:DBRCDF2)
     FWTGIL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D***PY SCOPYWW01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('System Table -
     D                                     Values              ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2493R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D DLTC            C                   CONST('*** DELETED ***')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*================================================================
     D*  Data Structures
     D*================================================================
     D VIEW1           DS
     D  V1TBID                 1      4
     D  V1KEY                  9     18
     D  V1DESC                21     60
     D  V1STAT                63     63
     D VIEW2           DS
     D  V2TBID                 1      4
     D  V2DESC                 8     47
     D  V2KEY                 21     60
     D  V2STAT                63     63

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQTBID            4
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   PARM                    QQFG03            1
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      ***** BEG ADD ***** 01/03/05 ************************************
     C                   PARM                    QQTBID            4
      ***** END ADD ***** 01/01/05 ************************************
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
     C                   KFLD                    ##CO#
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C*
     C     KEY2A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C                   KFLD                    KKDESC
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C*
     C     KEY2B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKDESC
     C                   KFLD                    KKKEY
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTDESC        KKDESC
     C     *LIKE         DEFINE    GTDT          KKDT
     C     *LIKE         DEFINE    GTTIME        KKTIME
      *
     C/COPY SCOPYWW01C
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSSTAT        IFNE      *BLANKS
     C     SSDESC        ORNE      *BLANKS
     C                   SETON                                        11
     C                   ENDIF
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
     C   51              MOVE      *LOVAL        KKTBID
     C                   MOVE      *LOVAL        KKKEY
     C                   MOVE      *LOVAL        KKDESC
     C                   MOVE      *LOVAL        KKDT
     C                   MOVE      *LOVAL        KKTIME
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C  N51              MOVE      QQTBID        KKTBID
     C*** BEGIN CHG *** 04/02/07 **************************************
     C***51*****           MOVE SCPSID    KKTBID
     C   51              MOVE      SCPST5        KKTBID
     C*** END   CHG *** 04/02/07 **************************************
     C  N51
     CAN 21              MOVE      SCPST1        KKKEY
     C   51
     CAN 21              MOVE      SCPST3        KKKEY
     C  N51
     CAN 22              MOVE      SCPST2        KKDESC
     C   51
     CAN 22              MOVE      SCPST4        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C   51              MOVE      SCTBID        KKTBID
     C                   MOVE      SCKEY         KKKEY
     C                   MOVE      SCDESC        KKDESC
     C                   MOVE      SCDT          KKDT
     C                   MOVE      SCTIME        KKTIME
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C   51              MOVE(P)   GTTBID        SCTBID
     C                   MOVE(P)   GTKEY         SCKEY
     C                   MOVE(P)   GTDESC        SCDESC
     C                   MOVE(P)   GTSTAT        SCSTAT
     C                   MOVE(P)   GTDT          SCDT
     C                   MOVE(P)   GTTIME        SCTIME
      *
     C     *IN21         IFEQ      *ON
     C                   MOVE      GTTBID        V1TBID
     C                   MOVE      GTKEY         V1KEY
     C                   MOVE      GTDESC        V1DESC
     C                   MOVE      GTSTAT        V1STAT
     C                   MOVEL(P)  VIEW1         SCDATA
     C                   ENDIF
      *
     C     *IN22         IFEQ      *ON
     C                   MOVE      GTTBID        V2TBID
     C                   MOVE      GTDESC        V2DESC
     C                   MOVE      GTKEY         V2KEY
     C                   MOVE      GTSTAT        V2STAT
     C                   MOVEL(P)  VIEW2         SCDATA
     C                   ENDIF
      *
     C                   ENDSR
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 - CHANGE STATUS
     C*===============================================================
     C     OPT20         BEGSR
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR                                                  OPT20
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
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
      *
     C                   MOVE      QQPRDS        SCPRDS
     C     SCPRDS        IFEQ      *BLANKS
     C                   MOVEL     OPTNM         SCPRDS
     C                   ENDIF
      *
     C     QQTBID        IFEQ      *BLANK
     C                   SETON                                        51
     C                   MOVEL     'VALUE'       SCFN01
     C                   MOVEL     'VALUE'       SCFN02
     C                   ELSE
     C                   SETOFF                                       51
     C                   MOVE      QQTBID        SCTBID
     C                   MOVE      QQTBID        KKTBID
     C     KEY1          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GIFN01        SCFN01
     C                   MOVEL     GIFN01        SCFN02
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFN01
     C                   MOVEL     *BLANKS       SCFN02
     C                   ENDIF
     C                   ENDIF
      *
      * Interface 'old' entry parm into work with copycode standard
      *
     C     QQCALL        IFEQ      'X'
     C                   MOVE      *BLANK        @@CALL            1
     C                   SETON                                        52
     C                   ELSE
     C                   MOVE      '2'           @@CALL
     C                   SETOFF                                       52
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C   51KEY1          READE     DBRCDF                                 95
     C  N51KEY1A         READE     DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C   51KEY1          READE     DBRCDF2                                95
     C  N51KEY1A         READE     DBRCDF2                                95
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C   51KEY1          READPE    DBRCDF                                 95
     C  N51KEY1A         READPE    DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C   51KEY1          READPE    DBRCDF2                                95
     C  N51KEY1A         READPE    DBRCDF2                                95
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2A         SETGT     DBRCDF                             96
     C   22KEY2B         SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2A         SETLL     DBRCDF                             96
     C   22KEY2B         SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVE      DLTC          SCDATA
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   MOVE      SCKEY         QQKEY
     C                   MOVE      '*'           QQRTCD
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C     GTSTAT        ANDNE     'A'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     GTSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSDESC        IFNE      *BLANKS
      *
     C     LO:UP         XLATE(P)  GTDESC        U1PDSC
     C     LO:UP         XLATE(P)  SSDESC        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C* Subset to passed in value of GTFG03
     C*
     C     Q1PRMS        IFGT      5
     C     QQFG03        ANDNE     *BLANKS
     C     QQFG03        ANDNE     GTFG03
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 05/04/09 **************************************
      *
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC           40
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC           40
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
** Column headings
Opt
Opt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
