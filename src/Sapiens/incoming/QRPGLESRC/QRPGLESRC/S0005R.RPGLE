     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/06/16  C:33  Project: WC2405                              *
     H*                  Pass-thru to STUSRPRM for anything but       *
     H*                   selection mode.                             *
     H*                                                               *
     H*  08/28/00  C:69  Support:                                     *
     H*                  Conditioned option 10 only when WC is        *
     H*                  installed; changed code to use UPDOFA        *
     H*                                                               *
     H* 08/01/00 C:61  PROJECT XXXXX                                  *
     H*                Add option 20=chg status.  Default subset to   *
     H*                status = 'A'.                                  *
     H*                                                               *
     H*  07/10/98  C:69  Support:                                     *
     H*                  Changed program to use SA005R API            *
     H*                                                               *
     H*  05/12/98  C:69  Support: 11494                               *
     H*                  Changed program to pass blanks for the       *
     H*                  security and desc parms when calling SC030R  *
     H*****************************************************************
     FS0005F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     FSSYLF     IF   E           K DISK
     F*  Physical file definition (for refreshing subfile record)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PFRCDF)
     F*
     FSMUPL1    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PUP01:DBRCDF3)
     FSMUPL3    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PUP01:DBRCDF2)
     F*  Primary logical file definition (for displaying items)
     FSMUPL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PUP01:DBRCDF)
      /COPY SCOPYWW01D
     D***  Begin Add  ***  07/06/16  **************************************
     d StUsrPrms       pr                  Extpgm('STUSRPRM')
     d  outCall                       1a
     D***  End   Add  ***  07/06/16  **************************************
     D*
     D HD              S             78    DIM(3) CTDATA PERRCD(1)              Column headings
      ***** BEG DEL ******* 08/01/00 **********************************
     D***************     OF      1  15  1   OFD    35   Options, Function
      ***** END DEL ******* 08/01/00 **********************************
      ***** BEG ADD ******* 08/01/00 **********************************
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
      ***** END ADD ******* 08/01/00 **********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  SCDATA                 1     70
     D  USNM                  12     51
     D  USR                   52     61
     D  STAF                  64     66
     D  STAF3                  2      4
     D  USNM3                  7     56
     D  USR3                  58     67
      ***** BEG ADD ******* 08/01/00 **********************************
     D  USSTS                 70     70
      ***** END ADD ******* 08/01/00 **********************************
     D*
     D                 DS
     D  DATA                   1     35
     D  MF                    28     28
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('User Parameters')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('S0005R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(11)
     D*
     D* Program specific constants
     D*
     D DELETD          C                   CONST('*Deleted')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@VIEW            1 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Work with program parameter list
     C*
     C     ACDPL2        PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKUSR
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKUSNM
     C                   KFLD                    KKUSR
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKSTAF
     C*
     C     SYKEY1        KLIST
     C                   KFLD                    KKCNTL
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C     *LIKE         DEFINE    UPUSNM        KKUSNM
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    SYKEY         KKCNTL
     C     *LIKE         DEFINE    UPUSNM        U1DESC
     C     *LIKE         DEFINE    SSUSNM        U2DESC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSUSNM        IFNE      *BLANKS
      ***** BEG ADD ******* 08/01/00 **********************************
B01  C     SSSTAT        ORNE      *BLANKS
      ***** END ADD ******* 08/01/00 **********************************
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
     C*
     C     *IN90         IFEQ      *OFF
     C     SSLVL         IFEQ      *ZEROS
     C     SSLVLC        ANDNE     *BLANKS
     C     SSLVL         ORNE      *ZEROS
     C     SSLVLC        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SSTYPE        IFEQ      *BLANKS
     C     SSTYPC        ANDNE     *BLANKS
     C     SSTYPE        ORNE      *BLANKS
     C     SSTYPC        ANDEQ     *BLANKS
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*
      ***** BEG ADD ******* 08/01/00 **********************************
      *
      *  USER STATUS
      *
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        7390
     C                   ENDIF
      *
      ***** END ADD ******* 08/01/00 **********************************
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
      ***** BEG ADD ******* 08/01/00 **********************************
     C                   MOVEL     'A'           SSSTAT
      ***** END ADD ******* 08/01/00 **********************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *LOVAL        KKUSR
     C                   MOVEL     *LOVAL        KKUSNM
     C                   MOVEL     *LOVAL        KKSTAF
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKUSR
     C                   MOVEL     SCPST2        KKUSNM
     C                   MOVEL     SCPST3        KKSTAF
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCUSR         KKUSR
     C                   MOVEL     SCUSNM        KKUSNM
     C                   MOVEL     SCSTAF        KKSTAF
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   MOVEL     UPSTAF        SCSTAF
     C                   MOVEL     UPLVL         SCLVL
      *
B01  C                   SELECT
      *
S01  C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  UPUSR         SCDATA
     C                   MOVEL     UPUSNM        USNM
     C                   MOVEL     UPSTAF        STAF
      *
S02  C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  UPUSNM        SCDATA
     C                   MOVEL     UPUSR         USR
     C                   MOVEL     UPSTAF        STAF
      *
S02  C     *IN23         WHENEQ    *ON
     C                   MOVEL     *BLANKS       SCDATA
     C                   MOVEL     UPSTAF        STAF3
     C                   MOVEL     UPUSNM        USNM3
     C                   MOVEL     UPUSR         USR3
      *
E01  C                   ENDSL
      *
      ***** BEG ADD ******* 08/01/00 **********************************
     C                   MOVEL     UPSTAT        USSTS
      ***** END ADD ******* 08/01/00 **********************************
      *
     C                   ENDSR
      *
      *
      *
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   SELECT
     C     SCSEL         WHENEQ    '7'
     C*                    MOVELSCUSNM    QQUSNM 40
     C                   CALL      'SC030R'                             99
     C                   PARM      SCUSR         QQUSR            10
     C                   PARM      *BLANKS       QQLVL             3
     C                   PARM      *BLANKS       QQUSNM           40
     C                   PARM      '3'           QQCALL            1
     C                   SETON                                        90
     C     SCSEL         WHENEQ    '8'
     C                   MOVEL     'C'           ##MODE
     C                   CALL      'W0004R02'    ACDPL                  99
     C     SCSEL         WHENEQ    '9'
     C                   MOVEL     'C'           ##MODE
     C                   CALL      'F0004R02'    ACDPL                  99
     C     SCSEL         WHENEQ    '10'
     C                   MOVE      *BLANKS       ##CALL
B02  C     Q1PRMS        IFGT      0
     C                   MOVE      @@CALL        ##CALL
E02  C                   ENDIF
     C                   CALL      'S0008R'      ACDPL2                 99
      *
      ***** BEG ADD ******* 08/01/00 **********************************
     C     SCSEL         WHENEQ    '20'
     C                   EXSR      OPT20
      ***** END ADD ******* 08/01/00 **********************************
      *
     C                   ENDSL
     C                   ENDSR
      *
      *
      *
      ***** BEG ADD ******* 08/01/00 **********************************
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
      ***** END ADD ******* 08/01/00 **********************************
      *
      *
      *
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C***  Begin Add  ***  07/06/16  **************************************
      /Free
       Select;
       // Continue with W/W
       When %parms >= 1 and @@call = 'O';
          @@call = ' ';
       // Call new user parm program
       When %parms < 1 or @@call <> '2';
          Clear ##call;
          If %parms >= 1;
             ##call = @@call;
          Endif;
          Monitor;
             StUsrPrms(##call);
             Return;
          On-error;
          // If there is an issue with STUSRPRM, continue with W/W
          Endmon;
       Endsl;
      /End-free
     C***  End   Add  ***  07/06/16  **************************************
     C                   MOVE      'SY'          KKCNTL
     C     SYKEY1        CHAIN     SSY01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     SYSCIF        IFEQ      'Y'
     C*** Begin Del *** 08/28/00 **************************************
     C****                 MOVE OFD,5     DATA
     C****                 MOVE 'Y'       MF
     C****                 MOVE DATA      OFD,5
     C*** End   Del *** 08/28/00 **************************************
     C*** Begin Add *** 08/28/00 **************************************
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '07'          XXNMBR
     C                   MOVEL     'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*** End   Add *** 08/28/00 **************************************
     C                   ENDIF
     C*
     C     SYWCIF        IFEQ      'Y'
     C*** Begin Del *** 08/28/00 **************************************
     C****                 MOVE OFD,6     DATA
     C****                 MOVE 'Y'       MF
     C****                 MOVE DATA      OFD,6
     C*** End   Del *** 08/28/00 **************************************
     C*** Begin Add *** 08/28/00 **************************************
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '08'          XXNMBR
     C                   MOVEL     'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '10'          XXNMBR
     C                   MOVEL     'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*** End   Add *** 08/28/00 **************************************
     C                   ENDIF
     C*
     C     SYFBIF        IFEQ      'Y'
     C*** Begin Del *** 08/28/00 **************************************
     C****                 MOVE OFD,7     DATA
     C****                 MOVE 'Y'       MF
     C****                 MOVE DATA      OFD,7
     C*** End   Del *** 08/28/00 **************************************
     C*** Begin Add *** 08/28/00 **************************************
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '09'          XXNMBR
     C                   MOVEL     'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C*** End   Add *** 08/28/00 **************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      2
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C*
     C                   Z-ADD     @@VIEW        VIEW#
     C*
     C                   ENDIF                                                  Q1PRMS>2
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
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
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C     Q1PRMS        IFGT      1
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
B01  C     SSUSNM        IFNE      *BLANKS
     C     LO:UP         XLATE     UPUSNM        U1DESC
     C     LO:UP         XLATE     SSUSNM        U2DESC
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1DESC
     C                   PARM      50            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2DESC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF
E01  C                   ENDIF
     C**
     C     MEETSS        IFEQ      *ON
     C     SSTYPC        ANDNE     *BLANKS
     C*
     C                   SELECT
     C     SSTYPC        WHENEQ    '1'
     C     SSTYPE        IFLE      UPTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSTYPC        WHENEQ    '2'
     C     SSTYPE        IFNE      UPTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSTYPC        WHENEQ    '3'
     C     SSTYPE        IFGE      UPTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C**
     C     MEETSS        IFEQ      *ON
     C     SSLVLC        ANDNE     *BLANKS
     C*
     C                   SELECT
     C     SSLVLC        WHENEQ    '1'
     C     SSLVL         IFLE      UPLVL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSLVLC        WHENEQ    '2'
     C     SSLVL         IFNE      UPLVL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSLVLC        WHENEQ    '3'
     C     SSLVL         IFGE      UPLVL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
      *
      ***** BEG ADD ******* 08/01/00 **********************************
      *
      *  STATUS
      *
     C     MEETSS        IFEQ      *ON
      *
     C     SSSTAT        IFEQ      'A'
     C     UPSTAT        ANDEQ     'I'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  STAT=A
      *
     C     SSSTAT        IFEQ      'I'
     C     SSSTAT        ANDNE     UPSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  STAT=A
      *
     C                   ENDIF                                                  MEETSS
      *
      ***** END ADD ******* 08/01/00 **********************************
     C**
     C                   ENDSR
** Column headings
Opt  User       Description                                        Staff Stat
Opt  Description                                        User       Staff Stat
Opt  Staff Description                                        User       Stat
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=DARTS info             NNN
1088=WCMS info              NNN
1099=WCMC/FSMS info         NNN
11010=License numbers       NNN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
