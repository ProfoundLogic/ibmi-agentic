     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Log History Function 8 Info - Additional Claimants
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
     Fw7485f8   cf   e             workstn
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
     D optnm           c                   'CMS Log History Addl Claimants'
     D comapi          c                   'WA001R'
     D #ofscr          c                   4
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
     c* 1st Claimant
     c*
     c                   movel     d1c1rl        scc1rl
     c                   movel     d1c1ss        scc1ss
     c                   movel     d1c1ph        scc1ph
     c                   movel     d1c1ln        scc1ln
     c                   movel     d1c1fn        scc1fn
     c                   movel     d1c1mi        scc1mi
     c                   movel     d1c1cy        scc1cy
     c                   movel     d1c1st        scc1st
     c                   movel     d1c1zp        scc1zp
     c                   movel     d1c1z4        scc1z4
     c                   movel     d1c1a1        scc1a1
     c                   movel     d1c1a2        scc1a2
     c                   movel     d1c1r1        scc1r1
     c                   movel     d1c1r2        scc1r2
     c                   movel     d1c2a1        scc2a1
     c                   movel     d1c2a2        scc2a2
     c                   movel     d1c2r1        scc2r1
     c                   movel     d1c2r2        scc2r2
     c                   movel     d1c3a1        scc3a1
     c                   movel     d1c3a2        scc3a2
     c                   movel     d1c3r1        scc3r1
     c                   movel     d1c3r2        scc3r2
     c                   movel     d1c4a1        scc4a1
     c                   movel     d1c4a2        scc4a2
     c                   movel     d1c4r1        scc4r1
     c                   movel     d1c4r2        scc4r2
     c*
     c                   movel     d1c1ri        scc1ri
     c                   movel     d1c1rt        scc1rt
     c                   movel     d1c1rp        scc1rp
     c                   movel     d1c1rn        scc1rn
     c                   movel     d1c1rf        scc1rf
     c                   movel     d1c1rm        scc1rm
     c                   movel     d1c1rc        scc1rc
     c                   movel     d1c1rs        scc1rs
     c                   movel     d1c1rz        scc1rz
     c                   movel     d1c1r4        scc1r4
     c*
     c* 2nd Claimant
     c*
     c                   movel     d1c2rl        scc2rl
     c                   movel     d1c2ss        scc2ss
     c                   movel     d1c2ph        scc2ph
     c                   movel     d1c2ln        scc2ln
     c                   movel     d1c2fn        scc2fn
     c                   movel     d1c2mi        scc2mi
     c                   movel     d1c2cy        scc2cy
     c                   movel     d1c2st        scc2st
     c                   movel     d1c2zp        scc2zp
     c                   movel     d1c2z4        scc2z4
     c*
     c                   movel     d1c2ri        scc2ri
     c                   movel     d1c2rt        scc2rt
     c                   movel     d1c2rp        scc2rp
     c                   movel     d1c2rn        scc2rn
     c                   movel     d1c2rf        scc2rf
     c                   movel     d1c2rm        scc2rm
     c                   movel     d1c2rc        scc2rc
     c                   movel     d1c2rs        scc2rs
     c                   movel     d1c2rz        scc2rz
     c                   movel     d1c2r4        scc2r4
     c*
     c* 3rd Claimant
     c*
     c                   movel     d1c3rl        scc3rl
     c                   movel     d1c3ss        scc3ss
     c                   movel     d1c3ph        scc3ph
     c                   movel     d1c3ln        scc3ln
     c                   movel     d1c3fn        scc3fn
     c                   movel     d1c3mi        scc3mi
     c                   movel     d1c3cy        scc3cy
     c                   movel     d1c3st        scc3st
     c                   movel     d1c3zp        scc3zp
     c                   movel     d1c3z4        scc3z4
     c*
     c                   movel     d1c3ri        scc3ri
     c                   movel     d1c3rt        scc3rt
     c                   movel     d1c3rp        scc3rp
     c                   movel     d1c3rn        scc3rn
     c                   movel     d1c3rf        scc3rf
     c                   movel     d1c3rm        scc3rm
     c                   movel     d1c3rc        scc3rc
     c                   movel     d1c3rs        scc3rs
     c                   movel     d1c3rz        scc3rz
     c                   movel     d1c3r4        scc3r4
     c*
     c* 4th Claimant
     c*
     c                   movel     d1c4rl        scc4rl
     c                   movel     d1c4ss        scc4ss
     c                   movel     d1c4ph        scc4ph
     c                   movel     d1c4ln        scc4ln
     c                   movel     d1c4fn        scc4fn
     c                   movel     d1c4mi        scc4mi
     c                   movel     d1c4cy        scc4cy
     c                   movel     d1c4st        scc4st
     c                   movel     d1c4zp        scc4zp
     c                   movel     d1c4z4        scc4z4
     c*
     c                   movel     d1c4ri        scc4ri
     c                   movel     d1c4rt        scc4rt
     c                   movel     d1c4rp        scc4rp
     c                   movel     d1c4rn        scc4rn
     c                   movel     d1c4rf        scc4rf
     c                   movel     d1c4rm        scc4rm
     c                   movel     d1c4rc        scc4rc
     c                   movel     d1c4rs        scc4rs
     c                   movel     d1c4rz        scc4rz
     c                   movel     d1c4r4        scc4r4
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
