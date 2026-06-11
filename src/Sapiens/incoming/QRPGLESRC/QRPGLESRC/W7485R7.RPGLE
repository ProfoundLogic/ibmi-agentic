     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Log History Function 7 Info - ICD9 Codes
     H Bnddir('WTBNDDIR')
     H*====================================================================
     H*  Created: 07/28/09   C:65   Project: WC1819
     H*====================================================================
    H*  Revisions:
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H*    04/27/15 C:53  Project: WC2261
     H*                   Revise for ICD-10 codes.
     H*
     H*====================================================================
     Fw7485f7   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     Fwmcmp     if   e             disk
    F*  Physical file definition (for maintaining record)
     Fwdd1p     uf   e             disk    rename(pd101:dbrcdf)
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FSMDIL     IF   E           K DISK
     F*** beg add *** 04/27/15 *********************
     FWMICD10L  IF   E           K DISK
     F*** end add *** 04/27/15 *********************
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D*** beg chg *** 04/27/15 *********************
     D**optnm ********* c                   'CMS Log History ICD-9 Codes'
     D optnm           c                   'CMS Log History -
     D                                       Diagnosis Codes'
     D*** end chg *** 04/27/15 *********************
     D comapi          c                   'WA001R'
     D #ofscr          c                   2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     **
     D*** beg del *** 04/27/15 *********************
     D**               ds
     D**I9S                    1     95    dim(19)
     D**XXI9S                  1     95
     D**SCI901                 1      5
     D**SCI902                 6     10
     D**SCI903                11     15
     D**SCI904                16     20
     D**SCI905                21     25
     D**SCI906                26     30
     D**SCI907                31     35
     D**SCI908                36     40
     D**SCI909                41     45
     D**SCI910                46     50
     D**SCI911                51     55
     D**SCI912                56     60
     D**SCI913                61     65
     D**SCI914                66     70
     D**SCI915                71     75
     D**SCI916                76     80
     D**SCI917                81     85
     D**SCI918                86     90
     D**SCI919                91     95
     D*** end del *** 04/27/15 *********************
     D*** beg add *** 04/27/15 *********************
     D                 ds
     D  I9S                    1    133    dim(19)
     D  XXI9S                  1    133
     D  SCI901                 1      7
     D  SCI902                 8     14
     D  SCI903                15     21
     D  SCI904                22     28
     D  SCI905                29     35
     D  SCI906                36     42
     D  SCI907                43     49
     D  SCI908                50     56
     D  SCI909                57     63
     D  SCI910                64     70
     D  SCI911                71     77
     D  SCI912                78     84
     D  SCI913                85     91
     D  SCI914                92     98
     D  SCI915                99    105
     D  SCI916               106    112
     D  SCI917               113    119
     D  SCI918               120    126
     D  SCI919               127    133
     D                 ds
     D  I1F                    1    133    dim(19)
     D  XXI1F                  1    133
     D  D1I101                 1      7
     D  D1I102                 8     14
     D  D1I103                15     21
     D  D1I104                22     28
     D  D1I105                29     35
     D  D1I106                36     42
     D  D1I107                43     49
     D  D1I108                50     56
     D  D1I109                57     63
     D  D1I110                64     70
     D  D1I111                71     77
     D  D1I112                78     84
     D  D1I113                85     91
     D  D1I114                92     98
     D  D1I115                99    105
     D  D1I116               106    112
     D  D1I117               113    119
     D  D1I118               120    126
     D  D1I119               127    133
     D*** end add *** 04/27/15 *********************
     **
     D                 ds
     D  I9F                    1     95    dim(19)
     D  XXI9F                  1     95
     D  D1I901                 1      5
     D  D1I902                 6     10
     D  D1I903                11     15
     D  D1I904                16     20
     D  D1I905                21     25
     D  D1I906                26     30
     D  D1I907                31     35
     D  D1I908                36     40
     D  D1I909                41     45
     D  D1I910                46     50
     D  D1I911                51     55
     D  D1I912                56     60
     D  D1I913                61     65
     D  D1I914                66     70
     D  D1I915                71     75
     D  D1I916                76     80
     D  D1I917                81     85
     D  D1I918                86     90
     D  D1I919                91     95
     **
     D                 ds
     D  I9D                    1    950    dim(19)
     D  XXI9D                  1    950
     D  SCID01                 1     50
     D  SCID02                51    100
     D  SCID03               101    150
     D  SCID04               151    200
     D  SCID05               201    250
     D  SCID06               251    300
     D  SCID07               301    350
     D  SCID08               351    400
     D  SCID09               401    450
     D  SCID10               451    500
     D  SCID11               501    550
     D  SCID12               551    600
     D  SCID13               601    650
     D  SCID14               651    700
     D  SCID15               701    750
     D  SCID16               751    800
     D  SCID17               801    850
     D  SCID18               851    900
     D  SCID19               901    950
     **
     d*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT05C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C*** beg add *** 04/27/15 *********************
     C     *LIKE         DEFINE    ICCODE        KKICD1
     C*** end add *** 04/27/15 *********************
     c*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C     DIKEY         klist
     c                   kfld                    KKICD9
    C*
     c*** beg add *** 04/27/15 *********************
     C*
     C     DIKEY1        klist
     c                   kfld                    KKICD1
     c
     c*** end add *** 04/27/15 *********************
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK4 - Error checking for srceen 4 (SCR04)
    C*================================================================
     C     errck4        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK5 - Error checking for srceen 5 (SCR05)
    C*================================================================
     C     errck5        begsr
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     c                   move      d1type        schtyp
     c*
     c                   select
     c                   when      d1type = '0'
     c                   eval      schdes = '- Add'
     c                   when      d1type = '1'
     c                   eval      schdes = '- Delete'
     c                   when      d1type = '2'
     c                   eval      schdes = '- Update'
     c                   endsl
     c*
     C                   Z-ADD     d1exdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scrsdt
     c*
     c*** beg del *** 04/27/15 *********************
     c***********        move      xxi9f         xxi9s
     c*** end del *** 04/27/15 *********************
     c*** beg add *** 04/27/15 *********************
     c*
     c*** If using ICD-10's load those to screen else load 9's
     c                   if        xxi1f <> *blanks
     c                   move      xxi1f         xxi9s
     c                   else
     c                   movel(p)  d1i901        sci901
     c                   movel(p)  d1i902        sci902
     c                   movel(p)  d1i903        sci903
     c                   movel(p)  d1i904        sci904
     c                   movel(p)  d1i905        sci905
     c                   movel(p)  d1i906        sci906
     c                   movel(p)  d1i907        sci907
     c                   movel(p)  d1i908        sci908
     c                   movel(p)  d1i909        sci909
     c                   movel(p)  d1i910        sci910
     c                   movel(p)  d1i911        sci911
     c                   movel(p)  d1i912        sci912
     c                   movel(p)  d1i913        sci913
     c                   movel(p)  d1i914        sci914
     c                   movel(p)  d1i915        sci915
     c                   movel(p)  d1i916        sci916
     c                   movel(p)  d1i917        sci917
     c                   movel(p)  d1i918        sci918
     c                   movel(p)  d1i919        sci919
     c                   endif
     c*** end add *** 04/27/15 *********************
     c                   clear                   xxi9d
     c*
     c     1             do        19            x                 2 0
     c*
     c                   if        i9s(x) <> *blanks
     c*
     c*** beg add *** 04/27/15 *********************
     c* 10's
     c                   if        xxi1f <> *blanks
     C                   move      i9s(x)        KKICD1
     C     dikey1        chain     pic01                              60
     C     *in60         ifeq      '0'
     C                   movel(p)  icdsc1        i9d(x)
     C                   endif
     c* 9's
     c                   else
     c*** end add *** 04/27/15 *********************
     C                   MOVE      i9s(x)        KKICD9
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DIDSC1        i9d(x)
     C                   ENDIF
     c*** beg add *** 04/27/15 *********************
     c                   endif
     c*** end add *** 04/27/15 *********************
     c*
     c                   endif
     c                   enddo
     c*
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   endsr
    C*
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@MODE        COMP      'C'                                    10
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     c*
     c*
     c     @@rrn2        chain     pcm01
     c*
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     CMFND         SCFND
     C  N60              MOVEL     FNNAME        SCFNAM
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      ******  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCHSS#
     C*****SCHSS#        CAT       SS2:0         SCHSS#
     C*****SCHSS#        CAT       '-':0         SCHSS#
     C*****SCHSS#        CAT       SS3:0         SCHSS#
      /Free
       scHSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ******  End Chg  ***  06/09/20  **********************
     C                   ENDIF                                                  CMSS# <> *BLANKS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCHADT
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF                                                  CDLSN <> ' '
      *
     c*
     C                   endsr
    C*
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C                   endsr
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDFLD        BEGSR
     C                   ENDSR
    C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   endsr
