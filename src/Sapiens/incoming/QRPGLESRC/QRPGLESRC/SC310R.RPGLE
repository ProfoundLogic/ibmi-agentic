     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC310R - WORK WITH CUSTOM GROUP ID CODES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Custom Group ID **
     H**     Code file.                                              **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   -  Work with Custom Group ID Codes               **
     H**   CALLS TO:                                                 **
     H**     SC310R01 - Work With Custom Group ID Codes Information  **
     H**     SC311R - Work With Custom Group Fields                  **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/22/98      PROJECT: DR048                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 05/15/25 C:342 PROJECT: MC447 Phase 2/119035
     H*                Changed Group field to Type Ahead on Subset screen
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*  10/01/98  C:94  PROJECT: DR048
     H*                  CORRECTED ERROR MESSAGE FOR SUBSET LIST
     H*
     H*  08/06/98  C:94  PROJECT: DR049
     H*                  PASSED IN *INKC AND *INKL TO SC311R
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
     H**      71 - 88 are reserved for error screen error indicators
     H**      89   Used to position cursor after search
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     FSC310F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMCGP     IF   E             DISK
     F                                     RENAME(PCG01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMCGL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCG01:DBRCDF)
     FSMCGL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCG01:DBRCD2)
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D***  BEGIN ADD  ***  02/01/01  *********************************
     D HD2             S             78    DIM(2) CTDATA PERRCD(1)              Group column heading
     D***  END   ADD  ***  02/01/01  *********************************
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Custom Group IDs')
     D*** BEG CHG *** 10/01/98 ***************************************
     D SLMSGI          C                   CONST('SC49998')
     D**********    'WC39998'             C         SLMSGI
     D*** END CHG *** 10/01/98 ***************************************
     D SLMSGF          C                   CONST('SCMESG')
     D ACDPGM          C                   CONST('SC310R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
     D*** BEG ADD *** 06/24/98 **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** END ADD *** 06/24/98 **************************************
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For custom group ID sequence
     D* ---------------------------------------------------------------
     D                 DS
     D  X1DATA                 1     48
     D  X1CSID                 3      4
     D  X1DESC                 9     48
     D* ---------------------------------------------------------------
     D* SCDATA For Description sequence
     D* ---------------------------------------------------------------
     D                 DS
     D  X2DATA                 1     46
     D  X2DESC                 1     42
     D  X2CSID                43     46

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C                   PARM                    @@CSID            2
     C                   PARM                    @@RTCD            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
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
     C     KEY1          KLIST
     C                   KFLD                    KKCSID
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKDESC
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    @@CALL        XXCALL
     C***  END   ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    CGCSID        KKCSID
     C     *LIKE         DEFINE    CGDESC        KKDESC
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    CGGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*** BEG ADD *** 06/24/98 **************************************
     C     *LIKE         DEFINE    CGDESC        U1PDSC
     C     *LIKE         DEFINE    CGDESC        U2PDSC
     C     *LIKE         DEFINE    CGCSID        U1PSID
     C     *LIKE         DEFINE    CGCSID        U2PSID
     C*** END ADD *** 06/24/98 **************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    SCGRP         ##GRP
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSSTAT        IFNE      *BLANKS
     C*** BEG ADD *** 06/24/98 ****************************************
     C     SSDESC        ORNE      *BLANKS
     C     SSCSID        ORNE      *BLANKS
     C     SSFMT         ORNE      *BLANKS
     C*** END ADD *** 06/24/98 ****************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     SSGRP         ORNE      *ZEROS
     C***  END   ADD  ***  02/01/01  *********************************
     C                   SETON                                        11
     C                   ENDIF                                                              ks
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
     C* CHECK ACTIVE/INACTIVE
     C*
     C     SSSTAT        IFNE      'A'
     C     SSSTAT        ANDNE     'I'
     C     SSSTAT        ANDNE     *BLANKS
     C                   SETON                                        9054
     C                   ENDIF                                                    A/orB/or b
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   SSGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCSID
     C                   MOVE      *LOVAL        KKDESC
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   Z-SUB     1             KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C   21              MOVEL     SCPST1        KKCSID
     C   22              MOVEL     SCPST2        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVEL     SCGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C   21              MOVEL     SCCSID        KKCSID
     C   22              MOVEL     SCDESC        KKDESC
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     CGCSID        SCCSID
     C                   MOVEL     CGDESC        SCDESC
     C   21              MOVEL     CGCSID        X1CSID
     C   21              MOVEL     CGDESC        X1DESC
     C   22              MOVEL     CGCSID        X2CSID
     C   22              MOVEL     CGDESC        X2DESC
     C                   MOVEL     CGRFMT        SCFRMT
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVEL     CGGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     CGRCDS        SCSCLV
      *
     C                   MOVEL     CGSTAT        SCSTAT
     C   21              MOVEL     X1DATA        SCDATA
     C   22              MOVEL     X2DATA        SCDATA
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
     C*          Process option 20 - Change status
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = '20'
     C*
     C*          Process option 7 - FIELD
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      '7'
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      XXCALL        ##CALL
     C*
     C     XXCALL        IFEQ      '2'
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  XXCALL=2
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   MOVE      SCCSID        QQCS              2
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'SC311R'
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C**********           PARM @@CALL    ##CALL  1
     C                   PARM                    ##CALL            1
     C***  END   CHG  ***  02/01/01  *********************************
     C                   PARM                    QQCS
     C                   PARM                    QQRTC
     C*** BEG ADD *** 08/06/98 **************************************
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C*** END ADD *** 08/06/98 **************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM      SCGRP         ##GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDIF                                                  SEL = '7'
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     USEGRP        IFEQ      'Y'
     C                   MOVE      *ON           *INLR
     C                   ENDIF                                                     USEGRP=Y
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C*
     C     USEGRP        IFEQ      'Y'
     C                   MOVEA     HD2           HD
     C                   ENDIF                                                  USEGRP=Y
     C*
     C                   CLEAR                   XXCALL
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@CALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=1
     C***  END   ADD  ***  02/01/01  *********************************
     C*
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
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     SCDATA        X1DATA
     C                   MOVEL     X1CSID        @@CSID
     C                   MOVEL     '*'           @@RTCD
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     CGSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEG ADD *** 06/24/98 *************************************
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  CGDESC        U1PDSC
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
     C                   ENDIF                                                  SCNRES = 0
     C                   ENDIF                                                  SCDESC NE BL
     C***
     C     SSCSID        IFNE      *BLANK
     C****       SSCSID    ANDNECGCSID
     C****                 MOVE *OFF      MEETSS
     C     LO:HI         XLATE(P)  CGCSID        U1PSID
     C     LO:HI         XLATE(P)  SSCSID        U2PSID
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PSID
     C                   PARM      2             STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PSID
     C                   PARM      2             PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES = 0
     C                   ENDIF
     C***
     C     SSFMT         IFNE      *BLANK
     C     SSFMT         ANDNE     CGRFMT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END ADD *** 06/24/98 *************************************
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     SSGRP         IFNE      *ZEROS
     C     SSGRP         ANDNE     CGGRP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     CGGRP         IFNE      *ZEROS
     C     USEGRP        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
** Column headings
Opt  Grp ID  Description                              Format  Lvl  Sts
Opt  Description                             Grp ID   Format  Lvl  Sts
** Group column headings
Opt  Grp ID  Description                              Format  Lvl  Sts  Grp
Opt  Description                             Grp ID   Format  Lvl  Sts  Grp
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYN
1077=Fields                 YNN
12020=Change Status         YNN
203F3=Exit                  YNN
205F5=Refresh               NNN
206F6=Add                   YNN
211F11=Change View          YNN
212F12=Cancel               NYY
217F17=Subset               YYY
3
