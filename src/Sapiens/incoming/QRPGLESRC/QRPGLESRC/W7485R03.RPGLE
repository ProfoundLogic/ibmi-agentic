     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Log History Information
     H Bnddir('WTBNDDIR')
     H*====================================================================
     H*  Created: 08/14/09   C:232  Project: WC1819 Phase 5.
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
     H*  05/18/10   C:198  Project: WC1879
     H*                    Added display of DCN1 and DCN2.
     H*
     H*====================================================================
     Fw7485f03  cf   e             workstn
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
     D optnm           c                   'Submitted/Applied Query Data'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
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
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      'D'           ##mode
     C                   parm      @@rrn         ##rrn
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
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     c***                move      d1type        schtyp
     c*
     c***                select
     c***                when      d1type = '0'
     c***                eval      schdes = '- Add'
     c***                when      d1type = '1'
     c***                eval      schdes = '- Delete'
     c***                when      d1type = '2'
     c***                eval      schdes = '- Update'
     c***                endsl
     c***
     C***                Z-ADD     d1exdt        SRDTI
     C***                CALL      'SRCVTDAT'    DTMDYY
     C***                MOVEL     SRDTO         scrsdt
     c*
     c* Screen 1
     c*
     c****               move      d1dcn#        scdcn#
     c                   movel     d1hic#        sc1hic#
     c                   movel     d2hic#        sc2hic#
      *****  Begin Chg  ***  06/09/20  **********************
     c*****              movel     d1ss#         sc1ss#
     c*****              movel     d2ss#         sc2ss#
      /Free
       sc1ss# = MaskData('SSN':'d1ss#':d1ss#);
       sc2ss# = MaskData('SSN':'d2ss#':d2ss#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     c                   movel     d1lsn         sc1lsn
     c                   movel     d2lsn         sc2lsn
     c                   movel     d1fsn         sc1fsn
     c                   movel     d2fsn         sc2fsn
     c***                movel     d1mid         scmid
     c****               movel     d2mid         s2mid
     c                   movel     d1sex         sc1sex
     c                   movel     d2sex         sc2sex
     c*** Beg Add *** 05/18/10 **********************************
     c                   movel     d1dcn1        scdcn1
     c                   movel     d2dcn2        scdcn2
     c*** End Add *** 05/18/10 **********************************
     C                   Z-ADD     d1dtob        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         sc1dtob
     c*
     C                   Z-ADD     d2dtob        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     SRDTO         sc2dtob
      *
     C*********          movel     d1code        sc1code
     C*********          movel     d1dcn#        sc1dcn#
     c*
     c*
     c*
     c* Screen 2
     c*
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
     C***  *inkg         caseq     *on           kgdo                       f7=ICD9
     C***  *inkh         caseq     *on           khdo                       f7=Addl Clmts
     C***  *inki         caseq     *on           kido                       f7=Mult Settle
     C***                endcs
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
