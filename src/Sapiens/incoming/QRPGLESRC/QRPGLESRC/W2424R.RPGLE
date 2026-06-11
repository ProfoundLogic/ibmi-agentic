     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2424R - Check Signature - Work With Program
     H*
     H*  Description:
     H*
     H*    This program displays Check Signature records.
     H*    Records with INACTIVE status are NOT DISPLAYED.
     H*    This program has the ability to execute the following
     H*    functions:
     H*
     H*      1) Re-position at a specific record.
     H*      2) F12 to cancel selection.
     H*         SELECT Mode:
     H*           Option '1' to Select
     H*
     H*  Called From:
     H*    Menu
     H*    Select programs
     H*
     H*
     H*  Calls To:
     H*
     H*  Created:  04/15/97  C:30  Project:  WC500
     H*
     H*****************************************************************
     H*  Revisions:
     H*
     H*  05/10/00 C:53 PROJECT: FIX
     H*                CORRECTED ROLL PROBLEM.
     H*
     H*****************************************************************
     H*  Indicators:
     H*
     H*   09 - Inquiry
     H*   40 - LOKUP result indicator
     H*   99 - Subfile access
     H*
     H*****************************************************************
      * Sub File Display Screen
     FW2424F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      * Check Signature
     FWMSGL1    IF   E           K DISK
     F                                     RENAME(PSG01:DBRCDF)
     F                                     INFDS(DBFDS)
      *
      * Check Signature
     FWMSGP     IF   E             DISK
     F                                     RENAME(PSG01:PFRCDF)
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D***PY SCOPYWW01I
      *
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D OPTNM           C                   CONST('Check Signature')
     D OPTNM2          C                   CONST('Select Name ')
     D DELETD          C                   CONST('*Deleted')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D ACDPGM          C                   CONST('W2424R01')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
      *

     C     *LIKE         DEFINE    SGCO#         XXCO#
     C     *LIKE         DEFINE    SGNAME        XXNAME
     C     *LIKE         DEFINE    SGCO#         @@CO#
     C     *LIKE         DEFINE    SGNAME        @@NAME
     C     *LIKE         DEFINE    SGFONT        @@FONT
     C     *LIKE         DEFINE    SGCMD         @@CMD
     C     *LIKE         DEFINE    SCPST1        SVPST1
      *
      *  W2424R01
      *
     C     ACDPL         PLIST
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *  Company Name
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Keylist for Check Signature file
      *
     C     KEY1          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXNAME
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CO#
     C                   PARM                    @@NAME
     C                   PARM                    @@FONT
     C                   PARM                    @@CMD
      *
     C/COPY SCOPYWW01C
      *
      **********************************************************************
      *  CHKSSM
      **********************************************************************
     C     CHKSSM        BEGSR
      *
     C                   ENDSR
      *
      **********************************************************************
      *  ERRCK1 - Edit OPT screen field
      **********************************************************************
     C     ERRCK1        BEGSR
      *
     C                   ENDSR
      *
      **********************************************************************
      *  ERRCK2 - Edit SCR03 Fields
      **********************************************************************
     C     ERRCK2        BEGSR
      *
      *  Edit Search Status
      *
     C                   ENDSR
      *
      **********************************************************************
      *  INZSST
      **********************************************************************
     C     INZSST        BEGSR
     C                   ENDSR
      *
      **********************************************************************
      *  KEYINZ - Initialize Both File Key Fields to Low Values
      **********************************************************************
     C     KEYINZ        BEGSR
      *
     C                   MOVE      ##CO#         XXCO#
     C                   MOVE      *LOVAL        XXNAME
      *
     C                   ENDSR
      *
      **********************************************************************
      *  KEYSET - Assign File Key Field
      **********************************************************************
     C     KEYSET        BEGSR
      *
     C*** BEG CHG *** 05/10/00 **************************
     C*****************    MOVELSCPST1    XXNAME
     C                   MOVEL(P)  SCPST1        XXNAME
     C*** END CHG *** 05/10/00 **************************
      *
     C                   ENDSR
      *
      **********************************************************************
      *  KEYSFL - Assign Sub File Fields to File Key Fields
      **********************************************************************
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SCNAME        XXNAME
      *
     C                   ENDSR
      *
      **********************************************************************
      *  MVDBSC - Move File Fields to Screen Fields
      **********************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     SGNAME        SCNAME
     C                   MOVEL     SGFONT        SCFONT
     C                   MOVEL     SGCMD         SCCMD
      *
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C     VIEW#         WHENEQ    2
     C                   ENDSL
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OTHFKS - Process Other Function Keys
      **********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   EXSR      ENTRDO
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OTHOPT - Process Other Options.
      **********************************************************************
     C     OTHOPT        BEGSR
     C                   ENDSR
      *
      **********************************************************************
      *  PGMEND
      **********************************************************************
     C     PGMEND        BEGSR
      *
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
      *
     C                   ENDSR
      *
      **********************************************************************
      *  PGMSTR - Assign Initial field values
      **********************************************************************
     C     PGMSTR        BEGSR
     C                   MOVEL     *BLANKS       @@NAME
     C                   MOVEL     *BLANKS       @@FONT
     C                   MOVEL     *BLANKS       @@CMD
      *
      *  Initialize Default values
      *
     C                   MOVE      *OFF          *IN09                          NOT INQUIRY
     C*                    MOVEA'00'      *IN,21
     C*** BEG CHG *** 05/10/00 **************************
     C****************     Z-ADD11        SF1PAG  50
     C                   Z-ADD     12            SF1PAG            5 0
     C*** END CHG *** 05/10/00 **************************
     C                   MOVEL(P)  OPTNM         ##DATA
      *
      *  Determine what function program is performing.
      *
     C                   SELECT
     C     @@CALL        WHENEQ    '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
     C     @@CALL        WHENEQ    '2'
     C                   MOVE      *ON           SELMOD                         SELECT MODE
     C                   MOVE      *ON           *IN19                          CALL FROM PGM
     C                   OTHER
      *                    "Work With" Function
     C                   ENDSL
      *
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
      *
     C                   ENDSR
      *
      **********************************************************************
      *  READ - Access Check Signature file
      **********************************************************************
     C     READ          BEGSR
      *
     C   21XXCO#         READE     DBRCDF                                 95
      *
     C                   ENDSR
      *
      **********************************************************************
      *  READP - Access Check Signature file
      **********************************************************************
     C     READP         BEGSR
      *
     C   21XXCO#         READPE    DBRCDF                                 95
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SETGT - Position Check Signature for Read Access
      **********************************************************************
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SETLL - Position Check Signature for Read Access
      **********************************************************************
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SF1UPD - Assign Deleted message
      **********************************************************************
     C     SF1UPD        BEGSR
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SLCTRD - Used to move fields for selected records
      **********************************************************************
     C     SLCTRD        BEGSR
      *
     C                   Z-ADD     ##CO#         @@CO#
     C                   MOVEL(P)  SCNAME        @@NAME
     C                   MOVEL(P)  SCFONT        @@FONT
     C                   MOVEL(P)  SCCMD         @@CMD
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SUBSET - Process record against Subset Criteria
      **********************************************************************
     C     SUBSET        BEGSR
      *
      * Check Check Signature Status Active
      *
     C     SGSTAT        IFNE      'A'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
      *
**  Heading
Opt  Name
**  Description-------------MISN
1011=Select                 NNY
1022=Change                 NNN
1033=Copy                   NNN
1044=Delete                 NNN
1055=Display                NNN
1066=Print                  NNN
12020=Change Status         NNN
16060=Claim Check Sched HistNNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
210F10=Search               NNN
212F12=Cancel               NNY
213F13=Chg Usr Parm         NNN
217F17=Subset               NNN
3
