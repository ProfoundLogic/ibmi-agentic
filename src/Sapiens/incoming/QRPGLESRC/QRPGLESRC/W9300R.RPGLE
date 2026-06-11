     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W9300R - Work With Trial Information
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0070R16                                           *
     H*                                                               *
     H* CALLS TO:  W9300R02  Trial Information Maintenance            *
     H*                                                               *
     H* CREATED    02/19/01  C:61  114WBC                             *
     H* IN BASE    06/20/07  C:53  114WMF                             *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H* XX/XX/XX C:XX  PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  64       CONVERTED CLAIM                                     *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW9300F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWMTIP     IF   E             DISK
     F                                     RENAME(PTI01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMTIL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PTI01:DBRCDF)
      *
      *  Claim Master
     FWMCMP     IF   E             DISK
      *
      *  Fund Master
     FWMFNL     IF   E           K DISK
      *
      *  Employer Master
     FWMEML     IF   E           K DISK
      *
      *  Claimaint Demographics
     FWMCDL     IF   E           K DISK
      *
      *
      *
      /COPY SCOPYWW01D
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
      *
      *
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      *
      *
     D                 DS                  INZ
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS                  INZ
     D  SCDATA                 1     74
     D  XXSUDT                 1     10
     D  SCTRTP                13     22
     D  SCJUDG                25     34
     D  SCISSU                37     46
     D  SCCLVL                50     59
     D  SCVERD                63     72
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Trial Information')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W9300R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          WMTIP (Selected)
     C                   PARM                    @@RRN1            9 0          WMCMP
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          WMTIP
     C                   PARM      @@RRN1        ##RRN1            9 0          WMCMP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSUDT
     C                   KFLD                    KKTRTP
     C                   KFLD                    KKJUDG
     C                   KFLD                    KKISSU
     C                   KFLD                    KKVERD
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITM
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *  Fund Master
      *
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *  Claimant Demographics
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    TICO#         KKCO#
     C     *LIKE         DEFINE    TIFND         KKFND
     C     *LIKE         DEFINE    TIFYR         KKFYR
     C     *LIKE         DEFINE    TICASE        KKCASE
     C     *LIKE         DEFINE    TISUDT        KKSUDT
     C     *LIKE         DEFINE    TITRTP        KKTRTP
     C     *LIKE         DEFINE    TIJUDG        KKJUDG
     C     *LIKE         DEFINE    TIISSU        KKISSU
     C     *LIKE         DEFINE    TIVERD        KKVERD
     C     *LIKE         DEFINE    TIIDT         KKIDT
     C     *LIKE         DEFINE    TIITM         KKITM
      *
      *
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR                                                  ERRCK2
     C                   ENDSR
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE      *HIVAL        KKSUDT
     C                   MOVE      *LOVAL        KKTRTP
     C                   MOVE      *LOVAL        KKJUDG
     C                   MOVE      *LOVAL        KKISSU
     C                   MOVE      *LOVAL        KKVERD
     C                   MOVE      *LOVAL        KKIDT
     C                   MOVE      *LOVAL        KKITM
      *
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPST1
      *
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKSUDT
      *
     C     SRERR         IFEQ      *ON
     C                   Z-ADD     *HIVAL        KKSUDT
     C                   ENDIF
      *
     C                   MOVE      *LOVAL        KKTRTP
     C                   MOVE      *LOVAL        KKJUDG
     C                   MOVE      *LOVAL        KKISSU
     C                   MOVE      *LOVAL        KKVERD
     C                   MOVE      *LOVAL        KKIDT
     C                   MOVE      *LOVAL        KKITM
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCSUDT        KKSUDT
     C                   MOVEL(P)  SCTRTP        KKTRTP
     C                   MOVEL(P)  SCJUDG        KKJUDG
     C                   MOVEL(P)  SCISSU        KKISSU
     C                   MOVEL(P)  SCVERD        KKVERD
     C                   Z-ADD     SCIDT         KKIDT
     C                   Z-ADD     SCITM         KKITM
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
      *  LOAD DATA INTO SCREEN FIELD
      *
     C                   CLEAR                   SCDATA
      *
     C                   SELECT
      *****
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
      *
     C                   Z-ADD     TICO#         SCCO#
     C                   Z-ADD     TIFND         SCFND
     C                   Z-ADD     TIFYR         SCFYR
     C                   Z-ADD     TICASE        SCCASE
      *
     C                   Z-ADD     TISUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##SUDT
      *
     C                   CALL      'WA003R'
     C                   PARM                    ##SUDT           80
     C                   PARM      'DATE'        ##FMT            10
      *
     C                   MOVEL(P)  ##SUDT        XXSUDT           10
      *
     C                   Z-ADD     TISUDT        SCSUDT
     C                   MOVEL(P)  TITRTP        SCTRTP
     C                   MOVEL(P)  TIJUDG        SCJUDG
     C                   MOVEL(P)  TIISSU        SCISSU
     C                   MOVEL(P)  TICLVL        SCCLVL
     C                   MOVEL(P)  TIVERD        SCVERD
     C                   Z-ADD     TIIDT         SCIDT
     C                   Z-ADD     TIITM         SCITM
      *
      *****
      *
E01  C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
      *
      * Get Claim master record
      *
     C     @@RRN1        CHAIN     PCM01                              60
     C   60              SETON                                        LR
     C   60              RETURN
      *
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
      *
      *  Get Fund Name
      *
     C                   Z-ADD     CMFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
     C   60              MOVE(P)   *BLANK        SCFNAM
      *
      *  Get Employer Name
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      CMEMP#        ##EMP#            9 0
     C                   PARM      CMDIV         ##DIV             5 0
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    ##EDN            80
     C                   END                                                    EMKY
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
      ******  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ******  End Chg  ***  06/09/20  **********************
      *
      *  CLAIMANT NAME
      *
     C     CDKY          CHAIN     PCD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
      *
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   ENDIF                                                  CDMID NE BLANK
      *
     C                   ENDIF                                                  CDKY
      *
     C                   ENDIF                                                  SS# <> BLANK
      *
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *
      *  FYR-CASE
      *
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C   21KEY2          READE     DBRCDF                                 95
      *
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
      *
     C   21KEY2          READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
      *
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
      *
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
     C                   MOVE      SCDBRR        @@RRN
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Opt Suit date   Trial type  Judge       Legal issue  Court level  Verdict
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
