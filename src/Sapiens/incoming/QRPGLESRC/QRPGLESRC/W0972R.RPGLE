     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     H*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     H*
     H*****************************************************************
     H/TITLE W0972R - Work-with Auto DARTS Control
     H*****************************************************************
     H*  DESCRIPTION:  This program displays a list of WC Auto-DARTS
     H*                control records by Key, remark code, descrip-
     H*                tion, or Diary User. Options available include
     H*                change, display and change status.
     H*****************************************************************
     H*
     H*****************************************************************
     H*  CREATED: 02/10/05       C:58        PROJECT: WC1128
     H*
     H*---------------------------------------------------------------*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/22/15  C:279 Project: 114WKA                               *
     H*                 Modified to run program only in inquiry mode. *
     H*                                                               *
     H* 05/14/07  C:33  Support: WC139/57413                          *
     H*                 Address paging and reposition issues.         *
     H*                                                               *
     H* 07/31/06  C:15  Support: WC099/53906                          *
     H*                 Added code to subset to the current group.    *
     H*                                                               *
     H*   03/16/06   C:168  SUPPORT:WA139:51238
     H*                     Activated F12 so the user could cancel
     H*                                                               *
     H*---------------------------------------------------------------*
     H*  CALLED FROM:                                                 *
     H*    W0010R   - WORK WITH GROUPS                                *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*
     H*  01 - Used on the PAGEDOWN keyword
     H*  02 - Used on the PAGEUP keyword
     H*  03 - Used to condition SFLCLR
     H* N03 - Used to condition SFLDSPCTL
     H*  04 - Used to condition SFLDSP
     H*  05 - Used to condition DSPATR(RI) on the selection field
     H* N06 - Used to condition DSPATR(PC) on the position to field
     H*  07 - Used to condition DSPATR(PC) on the selection field
     H*  08 - Used to condition SFLNXTCHG
     H*  09 - INQUIRY
     H* N09 - UPDATE
     H* N10 - Used to condition SFLEND (on message subfile)
     H*  11 - Used to indicate the list is Subsetted
     H*  12 - Used to condition SFLDROP/SFLFOLD
     H*  13 - Used to condition SFLCLR (on confirm delete)
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)
     H*  14 - Used to condition SFLDSP (on confirm delete)
     H*  15 - Used to condition SFLEND (on confirm delete)
     H*  21 - Key
     H*  22 - Remark code
     H*  23 - Description
     H*  24 - Diary user
     H*  30 - Called as a search
     H*  31 - Client rpts IAIABC/POC (WCFL14='Y' in SWCPF)
     H*  40 - More Option Controller
     H*  41 - More Option Controller
     H*  71 - These indicators are to used as error indicators for
     H*       thru the various screens. They will set off after
     H*       displaying each input capable screen.
     H*  90 - General purpose error indicator
     H*  91 - Used to display CPF5203; roll up/down past last record
     H*  95 - Used to condition SFLEND; also used to indicate EOF and
     H*       BOF on the file being read.
     H*****************************************************************
     H*       F    I    L    E    S
     F*  Display File
     FW0972F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Auto-DARTS Control    PF (for refreshing subfile record)
     FWMADP     IF   E             DISK
     F                                     RENAME(PAD01:PFRCDF)
     F*                         LF (for displaying items)
     FWMADL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAD01:DBRCDF)
     F*
     F*          Secondaries    LF (for displaying views)
     F*   VIEW 1
     FWMADL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAD01:DBRCDF2)
     F*   VIEW 2
     FWMADL2    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PAD01:DBRCDF3)
     F*
     F*   Group master file.
     FWMFNP     IF   E             DISK
     F                                     INFDS(DBFDS5)
     F*
      /COPY SCOPYWW01D
     D*---------------------------------------------------------------
     D*       A   R   R   A   Y   S
     D*---------------------------------------------------------------
     D HD              S             78    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*---------------------------------------------------------------
     D*       D   A   T   A
     D*---------------------------------------------------------------
     D*
     D***PY SCOPYWW01I
     D*
     D*
     D*    Date and Time
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D***  VIEW DEFINITIONS +++++++++++++++++++++++++++++++++++++++++*
     D                 DS
     D  SCDATA                 1     74
     D*  View 1
     D  V1KEY                  1     10
     D  V1RMCD                12     16
     D  V1DESC                19     70
     D*  View 2
     D  V2RMCD                 1      5
     D  V2DESC                 8     59
     D  V2KEY                 61     70
     D*  View 3
     D  V3DESC                 1     53
     D  V3RMCD                55     59
     D  V3KEY                 61     70
     D*
     D  DSSTAT                73     73
     D***  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Auto DARTS Control')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0972R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(12)

      ******* beg add ********** 05/22/15 *****************************
      /copy sPrModule
      ******* end add ********** 05/22/15 *****************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0              WMFNP
     C                   PARM                    @@FND             3 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
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
     C***  Begin Add  ***  05/14/07  **************************************
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C***  End   Add  ***  05/14/07  **************************************
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKKEY
     C***  Begin Add  ***  05/14/07  **************************************
     C                   KFLD                    KKRMCD
     C***  End   Add  ***  05/14/07  **************************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDESC
     C                   KFLD                    KKKEY
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKDESC
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKKEY
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKUSR
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDESC
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    ADCO#         KKCO#
     C     *LIKE         DEFINE    ADFND         KKFND
     C     *LIKE         DEFINE    ADKEY         KKKEY
     C     *LIKE         DEFINE    ADRMCD        KKRMCD
     C     *LIKE         DEFINE    ADDESC        KKDESC
     C     *LIKE         DEFINE    ADUSR         KKUSR
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C*
     C                   Z-ADD     @@FND         KKFND
     C                   MOVE(P)   *LOVAL        KKKEY
     C                   MOVE(P)   *LOVAL        KKRMCD
     C                   MOVE(P)   *LOVAL        KKDESC
     C                   MOVE(P)   *LOVAL        KKUSR
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKKEY
     C                   MOVEL(P)  SCP1RM        KKRMCD
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  SCPST2        KKRMCD
     C     *IN23         WHENEQ    *ON
     C                   MOVEL(P)  SCPST3        KKDESC
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C***  Begin Add  ***  05/14/07  **************************************
     C                   MOVEL(P)  SCKEY         KKKEY
     C                   MOVEL(P)  SCRMCD        KKRMCD
     C                   MOVEL(P)  SCDESC        KKDESC
     C                   MOVEL(P)  SCUSR         KKUSR
     C***  End   Add  ***  05/14/07  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL(P)  *BLANKS       SCDATA
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  ADKEY         V1KEY
     C                   MOVEL(P)  ADRMCD        V1RMCD
     C                   MOVEL(P)  ADDESC        V1DESC
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  ADKEY         V2KEY
     C                   MOVEL(P)  ADRMCD        V2RMCD
     C                   MOVEL(P)  ADDESC        V2DESC
     C     *IN23         WHENEQ    *ON
     C                   MOVEL(P)  ADKEY         V3KEY
     C                   MOVEL(P)  ADRMCD        V3RMCD
     C                   MOVEL(P)  ADDESC        V3DESC
     C                   ENDSL
     C                   MOVEL(P)  ADSTAT        DSSTAT
     C*
     C***  Begin Add  ***  05/14/07  **************************************
     C                   MOVEL(P)  ADKEY         SCKEY
     C                   MOVEL(P)  ADRMCD        SCRMCD
     C                   MOVEL(P)  ADDESC        SCDESC
     C                   MOVEL(P)  ADUSR         SCUSR
     C***  End   Add  ***  05/14/07  **************************************
     C*
     C                   ENDSR
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C     OPT20         BEGSR
     C*
     C* OPTION 20=CHANGE STATUS
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR                                                  OPT20
     C*
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      /free
       //******** beg add ********* 05/22/15 *************************
       If IsModEnabled('Auto-Diary Triggers');
          @@Call = '1';
       Endif;
       //******** end add ********* 05/22/15 *************************
      /end-free
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNFND         KKFND
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C***  Begin Chg  ***  05/14/07  **************************************
     C***21*****           READ DBRCDF                   95
     C***22*****           READ DBRCDF2                  95
     C***23*****           READ DBRCDF3                  95
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C   23KEY           READE     DBRCDF3                                95
     C***  End   Chg  ***  05/14/07  **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C***  Begin Chg  ***  05/14/07  **************************************
     C***21*****           READPDBRCDF                   95
     C***22*****           READPDBRCDF2                  95
     C***23*****           READPDBRCDF3                  95
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
     C***  End   Chg  ***  05/14/07  **************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
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
     C*** Begin add *** 07/31/06 **************************************
     C     @@FND         IFNE      0
     C     ADFND         ANDNE     @@FND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** End add ***** 07/31/06 **************************************
     C*
     C                   ENDSR
     H*****************************************************************
      *  1         2         3         4         5         6         7       E
** Column headings
Opt Key        Rmk Cd Description                                          Sts
Opt Rmk Cd Description                                          Key        Sts
Opt Description                                          Rmk Cd Key        Sts
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
212F12=Cancel               YYY
3
