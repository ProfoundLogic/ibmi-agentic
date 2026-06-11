     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     *===============================================================*
     H/TITLE W1056R - WORK WITH MIDTERM ENDORSEMENT FORMS
     *===============================================================*
     * Program Description:                                          *
     *   This program will display a list of Midterm Endorsements    *
     *   for a selected group.  Each Endorsement can be accessed     *
     *   for update, display or selection depending on the calling   *
     *   program.                                                    *
     *                                                               *
     * Created   : 09/07/99                                          *
     * Programmer: C:113                                             *
     * Project   : WC904                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     H* 07/16/18 C:267 Project: WC2584                                *
     H*                Add call to NEW Midterm Endorsment forms       *
     H*                definition table                               *
     H*                                                               *
     H* 03/23/05 C:61  PROJECT CM027                                  *
     H*                Change access to options 31 and 32.            *
     H*                                                               *
     H* 03/03/03 C:61  PROJECT CM027                                  *
     H*                Add options (31, 32) to write to end install   *
     H*                files: WTMYPwrt, WTMYPupd                      *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     * ------------------------------------------------------------- *
     *===============================================================*
     ** Display file definition
     FW1056F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
     ** Physical file definition (for refreshing subfile record)
     FWTMYP     IF   E             DISK
     F                                     RENAME(PMY01:PFRCDF)
      *
     ** Primary logical file definition (for displaying items)
     FWTMYL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMY01:DBRCDF)
      *
     ** Secondary logical file definition (for displaying items)
     FWTMYL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PMY01:DBRCDF2)
     ** Group Master File
     FWMFNP     IF   E             DISK
      ***** BEG ADD ******* 03/03/03 **********************************
      *
      ** Install Files
     FWTMYPWRT  UF A E           K DISK
     F                                     RENAME(PMY01:PMYWRT)
     FWTMYPUPD  UF A E           K DISK
     F                                     RENAME(PMY01:PMYUPD)
      ***** END ADD ******* 03/03/03 **********************************
      *
      *
      *
     *=========================================================================
     *  Arrays & Tables
     *=========================================================================
    D***PY SCOPYWW01E
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
      ***** BEG CHG ******* 03/03/03 **********************************
     D*********           OF      1  14  1   OFD    35   Options, Function
      ***** BEG CHG ******* 03/23/05 **********************************
     D**********          OF      1  16  1   OFD    35   Options, Function
     D OF              S              1    DIM(17) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(17) ALT(OF)
      ***** END CHG ******* 03/23/05 **********************************
      ***** END CHG ******* 03/03/03 **********************************
      *
      *
     *=========================================================================
     *  Data structures and constants
     *=========================================================================
    D***PY SCOPYWW01I
      *
     ** View 1
     D                 DS
     D  SCDAT1                 1     40
     D  SCSRT1                 1      3
     D  SCDSC1                 6     35
      *
     ** View 2
     D                 DS
     D  SCDAT2                 1     40
     D  SCDSC2                 1     30
     D  SCSRT2                38     40
     *===============================================================*
     *  /COPY required constants
     *===============================================================*
     D OPTNM           C                   CONST('Midterm Endorsements')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1056R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
     *===============================================================*
     *  PLIST definitions
     *===============================================================*
      *
     ** Entry parameters

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@INKC            1
     C                   PARM      *INKL         @@INKL            1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
      *
     ** Add/change/display program parameter list
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##FND
     *** BEGIN ADD *** 07/16/18 **************************************
     C     PLMEND        PLIST                                                  Policy Page Endorsem
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM                    ##NEXT           10
     *** END   ADD *** 07/16/18 **************************************
      *
     ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     *===============================================================*
     *  KLIST definitions
     *===============================================================*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKSORT
      *
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKDESC
     *===============================================================*
     *  *LIKE DEFN statements
     *===============================================================*
      ***** BEG ADD ******* 03/03/03 **********************************
     C     *LIKE         DEFINE    MYSORT        K1SORT
      ***** END ADD ******* 03/03/03 **********************************
     C     *LIKE         DEFINE    MYCO#         KKCO#
     C     *LIKE         DEFINE    MYFND         KKFND
     C     *LIKE         DEFINE    MYSORT        KKSORT
     C     *LIKE         DEFINE    MYSORT        @@SORT
     C     *LIKE         DEFINE    MYDESC        KKDESC
     C     *LIKE         DEFINE    MYDESC        U1PDSC
     C     *LIKE         DEFINE    MYDESC        U2PDSC
     *===============================================================*
     * Main
     *===============================================================*
      *
    C/COPY SCOPYWW01C
      *
     *===============================================================*
     * CHKSSM - Set indicator 11 for subset message
     *===============================================================*
     C     CHKSSM        BEGSR
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSDESC        ORNE      *BLANKS
     C     SSSRFL        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * ERRCK1 - Additional errors on subfile selection
     *===============================================================*
     C     ERRCK1        BEGSR
      *
      ***** BEG ADD ******* 03/23/05 **********************************
      *
     C     SCSEL         IFEQ      '99'
     C                   MOVEL     Q1USER        XXTMP             3
      *
     C     Q1USER        IFEQ      'ANDERSON'
     C     XXTMP         OREQ      'PDS'
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '31'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '32'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   EXSR      BLDLNS
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 03/23/05 **********************************
     C                   ENDSR
     *===============================================================*
     * ERRCK2 - Error checking for the subset screen
     *===============================================================*
     C     ERRCK2        BEGSR
      *
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
      *
     C     SSSRFL        IFNE      ' '
     C     SSSRFL        ANDNE     'Y'
     C     SSSRFL        ANDNE     'N'
     C                   SETON                                        8890
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * INZSST - Initialize the subset fields
     *===============================================================*
     C     INZSST        BEGSR
     C                   ENDSR
     *===============================================================*
     * KEYINZ - Initialize key values for program start
     *===============================================================*
     C     KEYINZ        BEGSR
      *
     C   21              MOVE      *LOVAL        KKSORT
     C   22              MOVE      *LOVAL        KKDESC
      *
     C                   ENDSR
     *===============================================================*
     * KEYSET - Set key values when doing "position to"
     *===============================================================*
     C     KEYSET        BEGSR
      *
     C   21              MOVEL     SCPST1        KKSORT
     C   21              MOVEL     SCPS1A        KKDESC
     C   22              MOVEL     SCPST2        KKDESC
     C   22              MOVEL     SCPS2A        KKSORT
      *
     C                   ENDSR
     *===============================================================*
     * KEYSFL - Set key values from subfile records for paging
     *===============================================================*
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SCFND         KKFND
     C   21              MOVEL     SCSORT        KKSORT
     C   22              MOVEL     SCDESC        KKDESC
      *
     C                   ENDSR
     *===============================================================*
     * KPDO - Subroutine for F15=Mass copy
     *===============================================================*
     C     KPDO          BEGSR
      *
     C                   CALL      'W1053R'
     C     *INKC         PARM                    @@INKC
     C     *INKL         PARM                    @@INKL
     C                   PARM      'C'           XXMODE            1
     C                   PARM                    @@RRN
     C                   PARM      'M'           @@PMF             1
      *
     C                   ENDSR                                                  KPDO
     *===============================================================*
     * MVDBSC - Move data base fields to screen fields
     *===============================================================*
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     MYSTAT        SCSTAT
     C                   MOVEL     MYSORT        SCSORT
     C                   MOVE      MYSORT        XXSORT            3
     C                   MOVEL(P)  MYDESC        SCDESC
     C                   MOVEL     MYSRFL        SCSRFL
      *
     C     *IN21         IFEQ      *ON
     C                   MOVEL(P)  XXSORT        SCSRT1
     C                   MOVEL(P)  MYDESC        SCDSC1
     C                   MOVEL     SCDAT1        SCDATA
     C                   ENDIF
      *
     C     *IN22         IFEQ      *ON
     C                   MOVEL(P)  XXSORT        SCSRT2
     C                   MOVEL(P)  MYDESC        SCDSC2
     C                   MOVEL     SCDAT2        SCDATA
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * OPT20 - SUBROUTINE FOR OPTION 20=CHANGE STATUS
     *===============================================================*
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      ***** BEG ADD ******* 03/03/03 **********************************
      *
      *
      *
      *===============================================================*
      * OPT31 - Update WTENPWRT
      *===============================================================*
     C     OPT31         BEGSR
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   Z-ADD     MYSORT        K1SORT
     C     K1SORT        CHAIN     PMYWRT                             60
      *
     C     *IN60         IFEQ      *OFF
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   UPDATE    PMYWRT
      *
     C                   ELSE
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   WRITE     PMYWRT
     C                   ENDIF                                                  K1SORT
      *
     C                   ENDSR                                                  OPT31
      *
      *
      *
      *===============================================================*
      * OPT32 - Update WTENPUPD
      *===============================================================*
     C     OPT32         BEGSR
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   Z-ADD     MYSORT        K1SORT
     C     K1SORT        CHAIN     PMYUPD                             60
      *
     C     *IN60         IFEQ      *OFF
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   UPDATE    PMYUPD
      *
     C                   ELSE
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   WRITE     PMYUPD
     C                   ENDIF                                                  K1SORT
      *
     C                   ENDSR                                                  OPT32
      *
      *
      *
      ***** END ADD ******* 03/03/03 **********************************
     *===============================================================*
     * OTHFKS - Process other function keys
     *===============================================================*
     C     OTHFKS        BEGSR
      *
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKP         CASEQ     *ON           KPDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
      *
     C                   ENDSR
     *===============================================================*
     * OTHOPT - Process other options
     *===============================================================*
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
      ***** BEG ADD ******* 03/03/03 **********************************
     C     SCSEL         CASEQ     '31'          OPT31
     C     SCSEL         CASEQ     '32'          OPT32
      ***** END ADD ******* 03/03/03 **********************************
     C                   ENDCS
      *
     C                   ENDSR
     *===============================================================*
     * PGMEND - End of program processing
     *===============================================================*
     C     PGMEND        BEGSR
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
     *===============================================================*
     * PGMSTR - Start of program processing
     *===============================================================*
     C     PGMSTR        BEGSR
      *
     C                   MOVE      *ON           *IN20
      *
     C                   MOVE      ##CO#         KKCO#
     ** Get the current group
     C     @@RRN         CHAIN     WMFNP                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNFND         ##FND
     C                   Z-ADD     FNFND         SCFND
     C                   ELSE
     ** or default to group 999
     C                   Z-ADD     999           KKFND
     C                   Z-ADD     999           ##FND
     C                   Z-ADD     999           SCFND
     C                   ENDIF                                                  *IN60 = *OFF
     C*                    MOVE ##FND     KKFND
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     c***  Beg Add *******  07/16/18  *********************************
     c                   If        @@CALL = '1'
     c                   Eval      ##MODE = 'D'
     c                   Else
     c                   Eval      ##MODE = 'C'
     c                   EndIf

     c                   CALL      'WTGRPMEND'   PLMEND
     c                   EVAL      *INLR  = *ON
     C                   Return
     c***  End Add *******  07/16/18  *********************************
      *
      ***** beg DEL ******* 03/23/05 **********************************
      ******BEG ADD ******* 03/03/03 **********************************
      *********
      ***Disallow unauthorised use of options
      *********
     C*********  Q1USER    IFEQ 'ANDERSON'
     C*********  Q1USER    OREQ 'PDSPGMR'
      *********
     C*********            MOVE '1'       XXOPFN
     C*********            MOVE '31'      XXNMBR
     C*********            MOVE 'YYN'     XXMIS
     C*********            EXSR UPDOFA
      *********
     C*********            MOVE '1'       XXOPFN
     C*********            MOVE '32'      XXNMBR
     C*********            MOVE 'YYN'     XXMIS
     C*********            EXSR UPDOFA
      *********
     C*********            ENDIF
      *********
      ******END ADD ******* 03/03/03 **********************************
      ***** end DEL ******* 03/23/05 **********************************
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
     *===============================================================*
     * READ   - Read next record from file
     *===============================================================*
     C     READ          BEGSR
      *
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
      *
     C                   ENDSR
     *===============================================================*
     * READP  - Read previous record from file
     *===============================================================*
     C     READP         BEGSR
      *
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
      *
     C                   ENDSR
     *===============================================================*
     * SETGT  - Set greater than on file
     *===============================================================*
     C     SETGT         BEGSR
      *
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
      *
     C                   ENDSR
     *===============================================================*
     * SETLL  - Set lower limits on file
     *===============================================================*
     C     SETLL         BEGSR
      *
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
      *
     C                   ENDSR
     *===============================================================*
     * SF1UPD - Update subfile record when item is deleted
     *===============================================================*
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCDATA
      *
     C                   ENDSR
     *===============================================================*
     * SLCTRD - Move fields for selected record to parameters
     *===============================================================*
     C     SLCTRD        BEGSR
      *
     C                   MOVEL     SCSORT        @@SORT
      *
     C                   ENDSR
     *===============================================================*
     * STRING - SUBROUTINE TO SEARCH STRINGS
     *===============================================================*
     C     STRING        BEGSR
      *
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
      *
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
      *
     C                   ENDSR                                                  STRING
     *===============================================================*
     * SUBSET - Determine if record should be put in subfile
     *===============================================================*
     C     SUBSET        BEGSR
      *
     C     SSDESC        IFNE      *BLANKS
      *
     C     LO:HI         XLATE(P)  MYDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
      *
     C                   EXSR      STRING
      *
     C                   ENDIF                                                  SSPDSC<>*BLANKS
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     MYSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSSRFL        IFNE      *BLANKS
     C     SSSRFL        ANDNE     MYSRFL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
** Column headings
Opt  Sort Description                           State Rpt  Status
Opt  Description                          Sort  State Rpt  Status
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
13131=Export to wtmypWRT    NNN
13232=Export to wtmypUPD    NNN
199                         YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
215F15=Mass copy            YNN
217F17=Subset               YYY
3
