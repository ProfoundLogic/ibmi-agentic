     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC330R - WORK WITH KEY DEFINITIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Key Definition  **
     H**     codes file                                              **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   - Work with Key Definitions                      **
     H**   CALLS TO:                                                 **
     H**     SC330F01 - Key Definition Maintenance                   **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/19/98      PROJECT: DR052                   **
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 02/28/01 C:128 PROJECT: DR108
     H*                Wrong Message file used.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*    06/10/98 C:94  PROJECT: DR052
     H*                   1) TURNED FUNCTION 5 KEY ON
     H*                   2) ADDED DESCRIPTION SEARCH TO SUBSET IN
     H*                      DISPLAY
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
     FSC330F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMKDP     IF   E             DISK
     F                                     RENAME(PKD01:PFRCDF)
     FSTGTP     IF   E             DISK
     F*
     F*  Primary logical file definition (for displaying items)
     FSMKDL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PKD01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Key Definitions')
     D SLMSGI          C                   CONST('WC39998')
      *** 02/28/01 Begin Change ***************************************
     D******        'SCMESG'              C         SLMSGF
     D SLMSGF          C                   CONST('WMESGG')
      *** 02/28/01 End   Change ***************************************
     D ACDPGM          C                   CONST('SC330R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Work With Generic Table Item parameter list
     C*
     C     ACDPL2        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
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
     C                   KFLD                    KKPGMN
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    KDPGMN        KKPGMN
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    KDDESC        U1PDSC
     C     *LIKE         DEFINE    KDDESC        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSKYTP        IFNE      *BLANKS
     C     SSDESC        ORNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
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
     C                   DO
     C*
     C*          Check function keys
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      OTHFKS
     C                   LEAVE
     C                   ENDIF                                                   *INKJ = On
     C*
     C* CHECK ACTIVE/INACTIVE
     C*
     C     SSSTAT        IFNE      'A'
     C     SSSTAT        ANDNE     'I'
     C     SSSTAT        ANDNE     *BLANKS
     C                   SETON                                        9054
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
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
     C                   MOVE      *LOVAL        KKPGMN
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKPGMN
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCPGMN        KKPGMN
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     KDPGMN        SCPGMN
     C                   MOVEL     KDDESC        SCDESC
     C                   MOVEL     KDTYPE        SCKYTP
     C                   MOVEL     KDSTAT        SCSTAT
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*******************
     C* F10=SEARCH      *
     C*******************
     C     *INKJ         IFEQ      '1'
     C                   DO
     C     HFLD          IFEQ      'SSKYTP'
     C                   MOVEL(P)  'KTYP'        ##TBID
     C*
     C                   CALL      'S2380R'      ACDPL2
     C*
     C     ##RRN         CHAIN     PGT01                              60
     C                   MOVEL     GTKEY         SSKYTP
     C                   MOVEL     GTDESC        SSDESC
     C*
     C                   SETON                                        90
     C                   ENDIF                                                  HFLD = SRC
     C*
     C                   ENDDO
     C                   ENDIF                                                  *INKJ
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF
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
     C                   MOVEL     SCPGMN        KKPGMN
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     KDSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  KDDESC        U1PDSC
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
     C*
     C     SSKYTP        IFNE      *BLANK
     C     SSKYTP        ANDNE     KDTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Program      Description                                Type   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YNY
206F6=Add                   YNN
210F10=Search               NYY
212F12=Cancel               NYY
217F17=Subset               YYY
3
