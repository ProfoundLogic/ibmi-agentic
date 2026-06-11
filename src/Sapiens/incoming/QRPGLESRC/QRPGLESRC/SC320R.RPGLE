     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC320R - WORK WITH PATH ID CODES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Path ID         **
     H**     codes file                                              **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   - Work with Path ID codes                        **
     H**   CALLS TO:                                                 **
     H**     SC320F01 - Work With PATH ID Codes                      **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   12/16/97      PROJECT: DR050                   **
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 05/15/25 C:342 PROJECT: MC447 Phase 2/119035
     H*                Changed Group field to Type Ahead on Subset screen
     H*
     H*  04/08/02 C:128 PROJECT: DR099 *31884
     H*                 Allow F12 exit from program in select mode.
     H*
     H* 02/28/01 C:128 PROJECT: DR108
     H*                Wrong Message file used.
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H** 06/22/98 C:94  PROJECT: DR050
     H**                1) SCREEN COMES UP IN FOLD MODE
     H**                2) F12=CANCEL TAKEN OUT
     H**                3) FIXED SEARCHING PROBLEM
     H**
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
     H**      22 - 29 are reserved for other views
     H**      60   Used for chaining
     H**      75 - 88 are reserved for error screen error indicators
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     FSC320F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMPIP     IF   E             DISK
     F                                     RENAME(PPI01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMPIL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPI01:DBRCDF)
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D***  BEGIN ADD  ***  02/01/01  *********************************
     D HD2             S             78    DIM(1) CTDATA PERRCD(1)              Group column heading
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
     D OPTNM           C                   CONST('Path ID Codes')
     D SLMSGI          C                   CONST('WC39998')
      *** 02/28/01 Begin Change ***************************************
     D******        'SCMESG'              C         SLMSGF
     D SLMSGF          C                   CONST('WMESGG')
      *** 02/28/01 End   Change ***************************************
     D ACDPGM          C                   CONST('SC320R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    1 = Inquiry
     C*                                                    2 = Search
     C                   PARM                    @@PATH            8
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
     C     KEY1          KLIST
     C                   KFLD                    KKPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PIPATH        KKPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    PIGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    PIDESC        U1PDSC
     C     *LIKE         DEFINE    PIDESC        U2PDSC
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
     C*
     C     SSDESC        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     SSGRP         ORNE      *ZEROS
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  IFNE blanks
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
     C                   MOVE      *LOVAL        KKPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     SCPST1        KKPATH
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C*** BEGIN CHG *** 06/01/07 **************************************
     C**********           CLEARKKGRP
     C                   MOVEL     SCGRP         KKGRP
     C*** END   CHG *** 06/01/07 **************************************
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     SCPATH        KKPATH
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     PIPATH        SCPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVEL     PIGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     PIDESC        SCDESC
     C                   MOVEL     PISTAT        SCSTAT
     C                   MOVEL     PICI01        SCCS01
     C                   MOVEL     PICI02        SCCS02
     C                   MOVEL     PICI03        SCCS03
     C                   MOVEL     PICI04        SCCS04
     C                   MOVEL     PICI05        SCCS05
     C                   MOVEL     PICI06        SCCS06
     C                   MOVEL     PICI07        SCCS07
     C                   MOVEL     PICI08        SCCS08
     C                   MOVEL     PICI09        SCCS09
     C                   MOVEL     PICI10        SCCS10
     C                   MOVEL     PINXPI        SCNXPI
     C                   MOVEL     PIALOR        SCALOR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
      *** Begin Add 04/08/02 ******************************************
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
      *** End   Add 04/08/02 ******************************************
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = 20
     C*
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
     C*** BEG ADD *** 06/22/98 **************************************
     C                   SETON                                        12
     C*** END ADD *** 06/22/98 **************************************
     C                   MOVEL     SCPATH        KKPATH
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
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C*** BEG ADD *** 06/22/98 ************************************
     C                   MOVE      *ON           *IN95
     C*** END ADD *** 06/22/98 ************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   MOVEL     SCPATH        @@PATH
     C                   MOVEL     '*'           @@RTCD
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     PISTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTAT NE blank
     C*
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  PIDESC        U1PDSC
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
     C*
     C                   ENDIF                                                  SSDESC NE blank
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     SSGRP         IFNE      *ZEROS
     C     SSGRP         ANDNE     PIGRP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     PIGRP         IFNE      *ZEROS
     C     USEGRP        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*
** Column headings
Opt    Path ID   The path order of Custom Group IDs    Next Path Range     Sts
** Group column headings
Opt    Path ID   The path order of Custom Group IDs    Next Path Range Grp Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
210F10=Search               NNN
212F12=Cancel               YYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
