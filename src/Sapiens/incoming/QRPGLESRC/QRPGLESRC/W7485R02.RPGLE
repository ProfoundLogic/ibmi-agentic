     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Log History Information
     H Bnddir('WTBNDDIR')
     H*====================================================================
     H*  Created: 07/28/09   C:65   Project: WC1819
     H*====================================================================
    H*  Revisions:
     H*
     H* 03/06/25 C:333  Project WC2998
     H*                 Changes to CMS Settlement Reporting.
     H*                 - Recompiled.
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H*====================================================================
     Fw7485f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     Fwmcmp     if   e             disk
    F*  Physical file definition (for maintaining record)
     Fwdd1p     uf   e             disk    rename(pd101:dbrcdf)
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMS Log History Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   5
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D*** Beg Add *** 08/14/09 ******************************************
     D ##rrn2          s              9  0
     D*** End Add *** 08/14/09 ******************************************
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D*** Beg Add *** 08/14/09 ******************************************
     D @@rrn2          s              9  0
     D*** End Add *** 08/14/09 ******************************************
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS
     D  d1rph#                 1     10  0
     D  xxrpa                  1      3
     D  xxrp1                  4      6
     D  xxrp2                  7     10
     D                 DS
     D  d1pahm                 1    200
     D  scpah1                 1     50
     D  scpah2                51    100
     D  scpah3               101    150
     D  scpah4               151    200
     D                 DS
     D  d1name                 1     70
     D  scnam1                 1     50
     D  scnam2                51     70
     D                 DS
     D  d1fdnm                 1     70
     D  scfdn1                 1     50
     D  scfdn2                51     70
     D                 DS
     D  d1rfnm                 1     70
     D  scrfn1                 1     50
     D  scrfn2                51     70
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
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C*** Beg Add *** 08/14/09 ******************************************
     C                   parm                    @@rrn2
     C*** End Add *** 08/14/09 ******************************************
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      'D'           ##mode
     C                   parm      @@rrn         ##rrn
     C*** Begin Add *** 11/17/09 *******************************************
     C                   parm      @@rrn2        ##rrn2
     C*** End   Add *** 11/17/09 *******************************************
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
    C*
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
    C* KGDO - Process F7=ICD9 Program
    C*================================================================
     C     kgdo          begsr
     C*
     C                   call      'W7485R7'     ACDPL
     C*
     c                   if        *inkc
     C                   EVAL      outflg = *ON
     c                   else
     c                   seton                                        90
     c                   endif
     C*
     C                   endsr                                                  kgdo
    C*
    C*================================================================
    C* KHDO - Process F8=Additional Claimants Program
    C*================================================================
     C     khdo          begsr
     C*
     C                   call      'W7485R8'     ACDPL
     C*
     c                   if        *inkc
     C                   EVAL      outflg = *ON
     c                   else
     c                   seton                                        90
     c                   endif
     C*
     C                   endsr                                                  khdo
    C*
    C*================================================================
    C* KIDO - Process F8=Multiple Settlements Program
    C*================================================================
     C     kido          begsr
     C*
     C                   call      'W7485R9'     ACDPL
     C*
     c                   if        *inkc
     C                   EVAL      outflg = *ON
     c                   else
     c                   seton                                        90
     c                   endif
     C*
     C                   endsr                                                  kido
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
     c* Screen 1
     c*
     c                   move      d1type        sctype
     c                   move      schdes        scdesc
     c                   move      d1dcn#        scdcn#
     c                   move      d1hic#        schic#
     c                   move      d2hic#        s2hic#
      *****  Begin Chg  ***  06/09/20  **********************
     c*****              move      d1ss#         scss#
      /Free
       scss# = MaskData('SSN':'d1ss#':d1ss#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     c                   move      d1lsn         sclsn
     c                   move      d2lsn         s2lsn
     c                   move      d1fsn         scfsn
     c                   move      d2fsn         s2fsn
     c                   move      d1mid         scmid
     c                   move      d2mid         s2mid
     c                   move      d1sex         scsex
     c                   clear                   scsexd
     c                   select
     c                   when      d1sex = '0'
     c                   eval      scsexd = '- Unknown'
     c                   when      d1sex = '1'
     c                   eval      scsexd = '- Male'
     c                   when      d1sex = '2'
     c                   eval      scsexd = '- Female'
     c                   endsl
     c*
     c                   move      d2sex         s2sex
     c                   clear                   s2sexd
     c                   select
     c                   when      d2sex = '0'
     c                   eval      s2sexd = '- Unknown'
     c                   when      d2sex = '1'
     c                   eval      s2sexd = '- Male'
     c                   when      d2sex = '2'
     c                   eval      s2sexd = '- Female'
     c                   endsl
     c*
     c* Screen 2
     c*
     C                   Z-ADD     d1dtob        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scdtob
     c*
     C                   Z-ADD     d2dtob        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         s2dtob
     c*
     C                   Z-ADD     d1cmdi        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         sccmdi
     c*
     C                   Z-ADD     d1acdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scacdt
     c*
     C                   move      d1jrst        scjrst
     C                   move      d1dinj        scdinj
     C                   move      d1prli        scprli
     C                   move      d1prnm        scprnm
     C                   move      d1prbn        scprbn
     C                   move      d1prmn        scprmn
     c*
     c* Screen 3
     c*
     C                   move      d1slfi        scslfi
     C                   move      d1slft        scslft
     C                   move      d1plnm        scplnm
     C                   move      d1pfnm        scpfnm
     C                   move      d1pity        scpity
     C                   move      d1fein        scfein
     C                   move      d1off#        scoff#
     C                   move      d1cap#        sccap#
     C                   move      d1clm#        scclm#
     c*
     c* Screen 4
     c*
     C                   move      d1clri        scclri
     C                   move      d1rpln        scrpln
     C                   move      d1rpfn        scrpfn
     C                   move      d1plaa        scplaa
     C                   move      d1rma1        scrma1
     C                   move      d1rma2        scrma2
     C                   move      d1rcty        scrcty
     C                   move      d1rst         scrst
     C                   move      d1rzip        scrzip
     C                   move      d1rzp4        scrzp4
     C*** begin chg *** 11/17/09 *******************************************
     C**********         movel     xxrpa         scrph#
     c**********         cat       '-':0         scrph#
     c**********         cat       xxrp1:0       scrph#
     c**********         cat       '-':0         scrph#
     c**********         cat       xxrp2:0       scrph#
     C                   if        d1rph#=0
     c                   eval      scrph#='000-000-0000'
     c                   else
     c                   eval      scrph#=xxrpa + '-' + xxrp1 + '-' + xxrp2
     c                   endif
     C*** end   chg *** 11/17/09 *******************************************
     C                   move      d1rpec        scrpec
     c*
     c* Screen 5
     c*
     C                   move      d1ormi        scormi
     c*
     C                   Z-ADD     d1ormd        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scormd
     c*
     C                   Z-ADD     d1t1sd        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         sct1sd
     c*
     C                   MOVE      d1t1am        sct1am
     c*
     C                   Z-ADD     d1t1fd        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         sct1fd
     c*
     C                   Z-ADD     d2mefd        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scmefd
     c*
     C                   Z-ADD     d2mexd        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scmexd
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
    C*
     C     *inkg         caseq     *on           kgdo                       f7=ICD9
     C     *inkh         caseq     *on           khdo                       f7=Addl Clmts
     C     *inki         caseq     *on           kido                       f7=Mult Settle
     C                   endcs
    C*
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
     C*** Beg Chg *** 08/14/09 ******************************************
     c*****@@rrn         chain     pcm01
     c     @@rrn2        chain     pcm01
     C*** End Chg *** 08/14/09 ******************************************
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
