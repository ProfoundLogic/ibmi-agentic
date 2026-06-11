     *========================================================================*
     * (c) 2009 StoneRiver, Inc. All rights Reserved.                *
     * StoneRiver is a trademark of StoneRiver, Inc.                 *
     *========================================================================*
     * IS999INC - Includes for IS999R/R02                                     *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
      * 01/28/10  C:XXX  Project: SY215                               *
      *                  Retrieve brand Name
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     D svDate          S              7  0
     D svTime          S              6  0
     D DateOut         S             10a

      ** Workstation data structure
     D WSDS            DS
     D  Fkey                 369    369a

     D EXIT            C                   CONST(03)
     D CANCEL          C                   CONST(12)
     D ERROR           C                   CONST(99)

     D ADD_MODE        C                   CONST('A')
     D CHG_MODE        C                   CONST('C')
     D DSP_MODE        C                   CONST('D')
     D DLT_MODE        C                   CONST('E')
     D CPY_MODE        C                   CONST('P')
     D SLC_MODE        C                   CONST('S')

     D BLANK           C                   *blanks

     D MODEA           C                   '-- Add --'
     D MODEC           C                   '- Change -'
     D MODED           C                   '-Display-'

     D YES             C                   CONST('Y')
     D NO              C                   CONST('N')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** PDSNAME      C                   'Progressive Data Solutions, Inc.'
     D pdsName         C                   'Stoneriver, Inc.'
     D*** END CHG ************* 01/28/10 ************************************
     D DELETED         C                   '** DELETED'

      *  Date check/conversion fields
     D dt_CYMD         C                   Const('*CYMD   ')
     D dt_MDYY         C                   Const('*MDYY   ')
     D dt_MDY          C                   Const('*MDY    ')
     D dt_YMD          C                   CONST('*YMD    ')
     D dt_YYMD         C                   CONST('*YYMD   ')
     D BLNKS           C                   CONST(*blanks)
     D date_in         S             10a

     D PgmStatus       S              2  0 Inz(0)

     D @@RRN           S              9  0
     D @@Mode          S              1a
     D @@Return        S              1a

     D PSDS          ESDS                  ExtName(szq1p)

     D                 DS
     D dsDateTime                    14  0
     D  dsTime                        6  0 Overlay(dsDateTime)
     D  dsDate                        8  0 Overlay(dsDateTime:7)
      ** Function Key Identifications
     D F1              C                   Const(x'31')
     D F2              C                   Const(x'32')
     D F3              C                   Const(x'33')
     D F4              C                   Const(x'34')
     D F5              C                   Const(x'35')
     D F6              C                   Const(x'36')
     D F7              C                   Const(x'37')
     D F8              C                   Const(x'38')
     D F9              C                   Const(x'39')
     D F10             C                   Const(x'3A')
     D F11             C                   Const(x'3B')
     D F12             C                   Const(x'3C')
     D F13             C                   Const(x'B1')
     D F14             C                   Const(x'B2')
     D F15             C                   Const(x'B3')
     D F16             C                   Const(x'B4')
     D F17             C                   Const(x'B5')
     D F18             C                   Const(x'B6')
     D F19             C                   Const(x'B7')
     D F20             C                   Const(x'B8')
     D F21             C                   Const(x'B9')
     D F22             C                   Const(x'BA')
     D F23             C                   Const(x'BB')
     D F24             C                   Const(x'BC')
     D CLEAR           C                   Const(x'BD')
     D ENTER           C                   Const(x'F1')
     D HELP            C                   Const(x'F3')
     D ROLLUP          C                   Const(x'F4')
     D ROLLDOWN        C                   Const(x'F5')
     D PRINT           C                   Const(x'F6')
     D RECDBKSP        C                   Const(x'F8')
     D AUTOENTR        C                   Const(x'3F')

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
      *  System API to get operational descriptors
     DGetOpDesc        PR                  ExtProc('CEEDOD')
     D ParmPos                       10I 0 const
     D o_desctype                    10I 0
     D o_datatype                    10I 0
     D o_descinf1                    10I 0
     D o_descinf2                    10I 0
     D o_fieldlen                    10I 0
     D o_errors                      12a   options(*omit)

     D Center          PR          1024a   opdesc
     D  @@string                   1024a   CONST

     D CvtDate         PR            10a
     D  date_in                       8  0 Const
     D  from_format                   8a   Const
     D  to_format                     8a   Const

     D CheckFkys       PR             2  0
     D   screen#                      2  0 Const
     D   mode                         1a   Const

     D GetDate         PR

     D Deleter         PR

     D Displayer       PR

     D MoveDB2SC       PR

     D MoveSC2DB       PR

     D DspScreen       PR
     D   screen#                      2  0 Const

     D ErrorChk        PR             2  0
     D   screen#                      2  0 Const
     D   mode                         1a   Const

     D Finish          PR             2  0
     D   mode                         1a   Const

     D OtherFkys       PR             2  0
     D   screen#                      2  0 Const
     D   mode                         1a   Const
     D   keypressed                   1a   Const

     D OtherModes      PR             2  0
     D   mode                         1a   Const

     D PgmEnd          PR

     D PgmStart        PR

     D ProcEndors      PR
     D   group                        3  0 Const

     D ProcScreen      PR             2  0
     D   screen#                      2  0 Const
     D   mode                         1a   Const

     D PreAdd          PR
     D   mode                         1a   Const

     D PreChange       PR

     D PreDelete       PR

     D PreDisplay      PR

     D Refresh         PR
     D   screen#                      2  0 Const
     D   mode                         1a   Const
     D   flag                         1a   Const

     D Updater         PR

     D Writer          PR

     D ErrorChk1       PR             2  0
     D   screen#                      2  0 Const
     D   mode                         1a   Const
