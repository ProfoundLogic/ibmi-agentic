     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC305R - WORK WITH REMARK TYPE CODES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the DARTS Remark    **
     H**     Type Code file.                                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   - 'Work with Remark Type Codes'                  **
     H**   CALLS TO:                                                 **
     H**     SC305F01 - Work With Remark Code Maintenance            **
     H**     F2493R - Generic Table Search                           **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   11/06/97      PROJECT: 87MCK                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 02/21/10 C:33  Project: 25WRE  Priss #14/19
     H*               -Check parms before updating @@KC or @@KL
     H*               -Allow for F17 in search mode
     H*
     H* 05/29/09 C:189 Project: 139WLM
     H*                 -EDI-Create diary when EDI Log History
     H*                  Not Recieving F3, F12 Values
     H*
     H* 05/15/09 C:189 PROJECT : 139WLJ
     H*                Generate automatic diary - audit
     H*
     H* 09/29/04 C:062 Support 43885
     H*                Un-Remove option 20=Change Status.
     H*
     H* 07/30/03 C:128 Support:38280 Project:VI121
     H*               Remove option 20=Change Status.
     H*               A Remark Code should never be inactivated.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*  12/03/98  C:94  PROJECT: DR073
     H*                  CHANGE WHERE LIST IS POSITIONED DEPENDING ON
     H*                  WHAT REMARK CODE IS PASSED IN.
     H*
     H*  10/14/98  C:110 MC145
     H*            CHANGED SCREEN TO INITIALIZE TO ACTIVE STATUS ONLY
     H*            AND PLACE AN 'A' IN SUBSET STATUS FIELD.
     H*
     H*  03/31/98  C:86  DR095/10791
     H*            CHANGED REMARK CODE TO 5 LONG. CHANGED OPTIONS AT
     H*            BOTTOM TO INCLUDE SEARCH SELECTION AND ADDED CODE
     H*            TO SLCTRD TO RETURN THE SELECTED REMARK CODE.
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      01   Used on the PAGEDOWN keyword
     H**      02   Used on the PAGEUP keyword
     H**      03   Used to condition SFLCLR
     H**     N03   Used to condition SFLDSPCTL
     H**      04   Used to condition SFLDSP
     H**      05   Used to condition DSPATR(RI) on the sel field
     H**     N06   Used to condition DSPATR(PC) on the pos to field
     H**      07   Used to condition DSPATR(PC) on the sel field
     H**      08   Used to condition SFLNXTCHG
     H**      09   Reserved for maintenance/inquiry conditions
     H**     N10   Used to condition SFLEND (on message subfile)
     H**      11   Used to indicate the list is subsetted
     H**      12   Used to condition SFLDROP/SFLFOLD
     H**      13   Used to condition SFLCLR (on confirm delete)
     H**     N13   Used to condition SFLDSPCTL (on confirm delete)
     H**      14   Used to condition SFLDSP (on confirm delete)
     H**      15   Used to condition SFLEND (on confirm delete)
     H**      21   View 1 - Remark code sequence
     H**      22   View 2 - Description Sequence
     H**      23 - 29 are reserved for other views
     H**      60   Used for chaining
     H**      71   SCM0601 Error - Invalid value (Summary code)
     H**      72   SCM0501 Error - Invalid value (Diary type)
     H**      73   SCM0510 Error - Invalid value (Status)
     H**      75 - 88 are reserved for error screen error indicators
     H**      89   Used to position cursor after search
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      STCTP  (PFRCDF) Remark Type Table                      **
     H**      STCTL1 (DBRCDF) Remark Type Table (RMCD)               **
     H**      STCTL3 (DBRCD2) Remark Type Table (DESC)               **
     H*****************************************************************
     H*
     FSC305F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTCTP     IF   E             DISK
     F                                     RENAME(PCT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTCTL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCT01:DBRCDF)
     FSTCTL3    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCT01:DBRCD2)
     FSTSML     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     F***** BEG CHG ******* 05/15/09 **********************************
     D*****************   OF      1  12  1   OFD    35   Options, Function
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     F***** END CHG ******* 05/15/09 **********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Remark Type Codes')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SCMESG')
     D ACDPGM          C                   CONST('SC305R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For remark code sequence
     D* ---------------------------------------------------------------
     D                 DS
     D  X1DATA                 1     73
     D  X1RMCD                 1      5
     D  X1DESC                 7     39
     D  X1SMCD                41     44
     D  X1TYPE                47     48
     D  X1DEPG                52     61
     D  X1FL01                65     65
     D  X1DPRG                71     73
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For Description sequence
     D* ---------------------------------------------------------------
     D                 DS
     D  X2DATA                 1     73
     D  X2DESC                 1     33
     D  X2RMCD                35     39
     D  X2SMCD                41     44
     D  X2TYPE                47     48
     D  X2DEPG                52     61
     D  X2FL01                65     65
     D  X2DPRG                71     73

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C******** BEGIN CHANGE *** 03/31/98 ******************************
     C********             PARM           @@RMCD  4
     C                   PARM                    @@RMCD            5
     C******** END   CHANGE *** 03/31/98 ******************************
     C                   PARM                    @@DESC           40
     C                   PARM                    @@RTCD            1
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
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*          SRCTL1 (DBRCDF)  Remark Type Code Table (RMCD)
     C     KEY1          KLIST
     C                   KFLD                    KKRMCD
     C*
     C*          SRCTL3 (DBRCD2)  Remark Type Code Table (DESC)
     C     KEY2          KLIST
     C                   KFLD                    KKDESC
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CTRMCD        KKRMCD
     C     *LIKE         DEFINE    CTDESC        KKDESC
     C     *LIKE         DEFINE    CTDESC        U1PDSC
     C     *LIKE         DEFINE    CTDESC        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSPRE         IFNE      *BLANKS
     C     SSTYPE        ORNE      *BLANK
     C     SSSMCD        ORNE      *BLANK
     C     SSFL01        ORNE      *BLANK
     C     SSSTAT        ORNE      *BLANK
     C     SSDESC        ORNE      *BLANK
     C                   SETON                                        11
     C                   ENDIF                                                  DESC <> Blanks
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      *** BEG ADD **** 05/15/09 ***************************************
     C  N90SCSEL         IFEQ      '6'
     C     CTFL01        ANDNE     'Y'
     C                   SETON                                        90  93
     C                   ENDIF                                                   CTFL01 <> 'Y'
      *** END ADD **** 05/15/09 ***************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   DO
     C*
     C*          Check function keys
     C*          ------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C                   EXSR      OTHFKS
     C                   LEAVE                                                   VVV Leave
     C                   ENDIF                                                   *INKJ = On
     C*
     C*          Check Summary Code
     C*          ------------------------------------------------------
     C                   MOVEL     *BLANKS       SSSMCT
     C     SSSMCD        IFNE      *BLANK
     C     SSSMCD        CHAIN     PSM01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9071
     C                   LEAVE                                                   VVV Leave
     C                   ELSE                                                   *IN60 = On
     C                   MOVEL(P)  SMDESC        SSSMCT
     C                   ENDIF                                                  *IN60 = On
     C                   ENDIF                                                   SMCD <> Blank
     C*
     C*          Check Diary Type
     C*          ------------------------------------------------------
     C     SSFL01        IFNE      *BLANK
     C     SSFL01        ANDNE     'Y'
     C     SSFL01        ANDNE     'N'
     C                   SETON                                        9072
     C                   LEAVE                                                   VVV Leave
     C                   ENDIF                                                   FL01 <> Blank
     C*
     C*          Check Status
     C*          ------------------------------------------------------
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        9073
     C                   LEAVE                                                   VVV Leave
     C                   ENDIF                                                   STAT <> Blank
     C*
     C                   ENDDO                                                   DO Loop
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*** BEG DEL *** 10/14/98 ****************************************
     C********   @@CALL    IFEQ '2'
     C********             MOVEL'A'       SSSTAT
     C********             ENDIF                           CALL = '2'
     C*** END DEL *** 10/14/98 ****************************************
     C     INZSST        BEGSR
     C*** BEG ADD *** 10/14/98 ****************************************
     C                   MOVEL     'A'           SSSTAT
     C*** END ADD *** 10/14/98 ****************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*** BEG CHG *** 12/03/98 ****************************************
     C     Q1PRMS        IFGT      1
     C                   MOVE      @@RMCD        KKRMCD
     C                   ELSE
     C                   MOVE      *LOVAL        KKRMCD
     C                   ENDIF
     C*** END CHG *** 12/03/98 ****************************************
     C                   MOVE      *LOVAL        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKRMCD
     C   22              MOVEL     SCPST2        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCRMCD        KKRMCD
     C                   MOVEL     SCDESC        KKDESC
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     CTRMCD        SCRMCD
     C                   MOVEL     CTRMCD        X1RMCD
     C                   MOVEL     CTRMCD        X2RMCD
     C                   MOVEL     CTDESC        SCDESC
     C                   MOVEL     CTDESC        X1DESC
     C                   MOVEL     CTDESC        X2DESC
     C                   MOVEL     CTSMCD        X1SMCD
     C                   MOVEL     CTSMCD        X2SMCD
     C                   MOVEL     CTTYPE        X1TYPE
     C                   MOVEL     CTTYPE        X2TYPE
     C                   MOVEL     CTDEPG        X1DEPG
     C                   MOVEL     CTDEPG        X2DEPG
     C                   MOVEL     CTFL01        X1FL01
     C                   MOVEL     CTFL01        X2FL01
     C                   MOVEL     CTDPRG        X1DPRG
     C                   MOVEL     CTDPRG        X2DPRG
     C   21              MOVEL     X1DATA        SCDATA
     C   22              MOVEL     X2DATA        SCDATA
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*          Process Function 10: Search Provider
     C*          ------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C                   DO
     C     HFLD          IFEQ      'SSSMCD'
     C                   MOVE      *BLANKS       QQSMCD            4
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'SC585R'
     C                   PARM                    QQSMCD
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           OUTFLG
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = 'A'
     C*
     C     QQRTC         IFEQ      'B'
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = 'B'
     C*
     C                   SETON                                        90
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQSMCD        SSSMCD
     C     SSSMCD        CHAIN     PSM01                              60
     C                   MOVEL(P)  SMDESC        SSSMCT
     C                   SETON                                        89
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = '*'
     C                   ENDIF                                                  HFLD = SRC
     C*
     C                   ENDDO
     C                   ENDIF                                                  INKJ = On
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
      *** BEGIN UND *** 09/29/04 **************************************
      *** Begin Change 07/30/03 ***************************************
     C*
     C*          Process option 20 - Change status
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = '20'
      *** End   Change 07/30/03 ***************************************
      *** END   UND *** 09/29/04 **************************************
      *** BEG ADD **** 05/15/09 ***************************************
     C*
     C*          Process option 6 - multiple Recipients
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      '6'
     C     CTFL01        ANDEQ     'Y'
     C                   CALL      'SC995R'
     C                   PARM                    QQCALL            1
     C     *INKC         PARM                    QQQKCC            1
     C     *INKL         PARM                    QQQKLC            1
     C                   PARM      CTRMCD        QQRMCD            5
     C                   ENDIF                                                  SEL = '6 '
     C*
     C*
      *** END ADD **** 05/15/09 ***************************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEG ADD ***** 05/29/09 **************************************
     C***  Begin Add  ***  02/21/10  **************************************
     C     Q1PRMS        IFGE      2
     C***  End   Add  ***  02/21/10  **************************************
     C                   MOVE      *INKC         @@KC
     C***  Begin Add  ***  02/21/10  **************************************
     C                   ENDIF
     C*
     C     Q1PRMS        IFGE      3
     C***  End   Add  ***  02/21/10  **************************************
     C                   MOVE      *INKL         @@KL
     C***  Begin Add  ***  02/21/10  **************************************
     C                   ENDIF
     C***  End   Add  ***  02/21/10  **************************************
     C*** END ADD ***** 05/29/09 **************************************
     C                   SETON                                        LR
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
     C   22              READ      DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C     *IN21         IFEQ      *ON
     C                   MOVEL     *BLANKS       X1DATA
     C                   MOVEL(P)  'DELETED'     X1DESC
     C                   ENDIF                                                  *IN21 = On
     C     *IN22         IFEQ      *ON
     C                   MOVEL     *BLANKS       X2DATA
     C                   MOVEL(P)  'DELETED'     X2DESC
     C                   ENDIF                                                  *IN22 = On
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C******* BEGIN ADD ***** 03/31/98 ********************************
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     SCDESC        @@DESC
     C                   MOVEL     SCRMCD        @@RMCD
     C                   MOVEL     '*'           @@RTCD
     C                   ENDIF
     C******* END   ADD ***** 03/31/98 ********************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   MOVEL     CTRMCD        XXWRK2            2
     C     SSPRE         IFNE      *BLANKS
     C     SSPRE         ANDNE     XXWRK2
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSTYPE        IFNE      *BLANKS
     C     SSTYPE        ANDNE     CTTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSMCD        IFNE      *BLANKS
     C     SSSMCD        ANDNE     CTSMCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFL01        IFNE      *BLANKS
     C     SSFL01        ANDNE     CTFL01
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CTSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  CTDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  NRES = 0
     C                   ENDIF                                                  PDSC <> Blanks
     C*
     C                   ENDSR
** Column headings     20        30        40        50        60        70
Opt  Code   Description                      Code Area  Program ID Type  Days
Opt  Description                      Code   Code Area  Program ID Type  Days
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=Multiple Recipients    YYN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
211F11=Change View          YYN
212F12=Cancel               YYY
217F17=Subset               YYY                              02/21/10
3
