     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0575R - WORK WITH CLAIM STATUS HISTORY
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                CLAIM STATUS HISTORY.                          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R   - WORK WITH CLAIMS                                *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/18/98  PROGRAMMER: C:33  PROJECT: WC577          *
     H*                                       SEQ#: WC091/10667       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H* 06/23/11 C:235 Project: 133WLN                                *
     H*                Claim Status Pop up Scrn-add status change     *
     H*                User wants to hit a key other than enter to    *
     H*                make the screen go away                        *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 12/14/99 C:15 Project: WC577                                  *
     H*               Y2K - change @@FYR from 2A to 3A.               *
     H*                                                               *
     H*****************************************************************
     FW0575F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMCHP     IF   E             DISK
     F                                     RENAME(PCH01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMCHL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCH01:DBRCDF)
     FWMCML     IF   E           K DISK
     FWMEML     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Claim Status History')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0575R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(14)
     D*
     D                 DS
     D  LN1                    1     79
     D  LN1A                   1      6
     D  LN1B                   8     10
     D  LN1C                  12     71
     D*
     D                 DS
     D  LN2                    1     79
     D  LN2CSH                 1      6
     D  LN2CS                  8     18
     D  LN2A                  21     24
     D  LN2BB                 26     36
     D  LN2B1                 26     28
     D  LN2B2                 29     29
     D  LN2B3                 30     31
     D  LN2B4                 32     32
     D  LN2B5                 33     36
     D  LN2C                  39     60
     D  LN2D                  63     66
     D  LN2E                  69     78

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@EMP#
     C                   PARM                    @@DIV
     C                   PARM                    @@SS#             9
     C*** BEGIN CHG *** 12/14/99 **************************************
     C******************** PARM           @@FYR   20
     C                   PARM                    @@FYR             3 0
     C*** END CHG ***** 12/14/99 **************************************
     C                   PARM                    @@CASE
     C                   PARM                    @@EFDT            8 0
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
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    #@CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKTIME
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CHCO#         KKCO#
     C     *LIKE         DEFINE    CHFND         KKFND
     C     *LIKE         DEFINE    CHFYR         KKFYR
     C     *LIKE         DEFINE    CHCASE        KKCASE
     C     *LIKE         DEFINE    CHEFDT        KKEFDT
     C     *LIKE         DEFINE    CHTIME        KKTIME
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CHCO#         @@CO#
     C     *LIKE         DEFINE    CHFND         @@FND
     C     *LIKE         DEFINE    CMEMP#        @@EMP#
     C     *LIKE         DEFINE    CMDIV         @@DIV
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    @@SS#         KKSS#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     *ZEROS        KKTIME
     C                   Z-ADD     *ZEROS        KKEFDT
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCTIME        KKTIME
     C                   Z-ADD     SCEFDT        KKEFDT
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVE      CHOCST        SCSTS
     C                   MOVEL(P)  CHEXPL        SCDESC
     C                   MOVEL(P)  CHUSER        SCUSER
     C                   Z-ADD     CHTIME        SCTIME
     C                   Z-ADD     CHEFDT        SCEFDT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C*******BEG ADD******06/23/11*************************************
     C*
     C     CLTID         IFEQ      '133'
     C     @@RTCD        ANDEQ     'J'
     C     *INKL         DOWEQ     *OFF
     C                   EXFMT     SCR01C
     C                   ENDDO
     C                   ENDIF
     C*
     C*******END ADD******06/23/11*************************************
     C*
     C                   SELECT
     C     *INKC         WHENEQ    *ON
     C                   MOVE      'A'           @@RTCD
     C                   Z-ADD     *ZERO         @@CASE
     C                   Z-ADD     *ZERO         @@EFDT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      'B'           @@RTCD
     C                   Z-ADD     *ZERO         @@EFDT
     C                   ENDSL
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@EMP#        KKEMP#
     C                   MOVE      @@DIV         KKDIV
     C                   MOVE      @@SS#         KKSS#
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@FYR         KKFYR
     C*
     C     @@EFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     @@EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        KKEFDT
     C*
     C                   ENDIF                                                  QQEFDT NE 0
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      @@CO#         #@CO#
     C                   MOVE      @@FND         #@FND
     C                   MOVE      @@FYR         #@FYR
     C                   MOVE      @@CASE        #@CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      'B'           @@RTCD
     C                   RETURN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C*  USE CLAIM HEADER API
     C*
     C                   CALL      'WA006R'
     C                   PARM                    ##CO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    LN1
     C                   PARM                    LN2
     C*
     C                   MOVEL(P)  LN1B          SCFND
     C                   MOVEL(P)  LN1C          SCFNAM
     C                   MOVEL(P)  LN2BB         SCSS#
     C                   MOVEL(P)  LN2C          SCCNAM
     C                   MOVEL(P)  LN2E          SCACDT
     C                   MOVEL(P)  LN2CS         SCYRCS
     C*
     C*  USE EMP/DIV/NAME API
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL(P)  XXEDN         SCEDN
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL(P)  CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   ENDIF                                                  CMCVT#<>*BLANK
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1A         READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1A         READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
  Eff Date  Sts  Explanation for change                             User
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
