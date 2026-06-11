     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Log History - WW/Compliance Codes
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
     H*  02/16/10 C:198  Project WC1819 Phase 2 changes
     H*                   Removed subset functionality, status field
     H*                   option 6 from display.
     H*
     H*  11/17/2009 c:62 Project wc1819
     H*                  change from Error to compliance
     H*
     H*====================================================================
     Fw7487f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
     Fwdd1p     if   e             disk
     Fwmcmp     if   e             disk
    F*  Physical file definition (for refreshing subfile record)
     Fwdc5p     if   e             disk    rename(pc501:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     FwdC5l     if   e           k disk    rename(pC501:dbrcdf)
     F                                     infds(dbfds)
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTDML     IF   E           K DISK
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D#ofOpts          c                   15
     D of              s              1    dim(#ofOpts) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOpts) alt(of)
     D*
     D*
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMS Compliance Codes'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W7487R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   7
     D LO              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
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
     D ##rrn2          s              9  0
     D ##rrn3          s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@call          s              1
     D kkcode          s              2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2
     C                   parm      @@rrn2        ##rrn3
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
     C     key1          klist
     C                   kfld                    D1CO#
     C                   kfld                    D1FND
     C                   kfld                    D1FYR
     C                   kfld                    D1CASE
     C                   kfld                    D1EXDT
     C                   kfld                    kkcode
     C*
     C     key2          klist
     C                   kfld                    D1CO#
     C                   kfld                    D1FND
     C                   kfld                    D1FYR
     C                   kfld                    D1CASE
     C                   kfld                    D1EXDT
     C*
     C* CMS Compliance code file.
     C*
     C     dmky          klist
     C                   kfld                    D1CO#
     C                   kfld                    C5CODE
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
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   eval      *in11 = sscode <> *blanks
     C                                  or ssstat <> *blanks
     C*
     C                   endsr
     C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
     C*
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   select
     C                   when      ssStat <> *blanks
     C                              and ssStat <> 'C'
     C                              and ssStat <> 'N'
     C                   seton                                        71  90
     C                   endsl
     C*
     C*
     C                   endsr
     C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   eval      sscode = *blanks
     C                   eval      ssstat = 'N'
     C*
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkcode = *blanks
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     c                   move      scpst1        kkcode
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkcode = sccode
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      sccode = C5code
     C*** Beg Del *** 02/16/10 *********************************************
     C***********        eval      scstat = C5stat
     C*** End Del *** 02/16/10 *********************************************
     C*
     c                   clear                   xxdesc
     c     dmky          chain     pdm01                              60
     c                   if        *in60 = *off
     c                   movel     dmdesc        xxdesc           50
     c                   endif
     C*
     C                   select
     C                   when      info# = 1 and view# = 1
     C*
     C*** Begin chg *** 11/17/09 *******************************************
     C**********         eval      scData =  ' ' + C5code + '     '
     C**********                          + xxdesc + '      '
     C**********                          + C5stat
     C                   eval      scData =  ' ' + C5code + '       '
     C                                    + xxdesc + '    '
     C*** Beg Del *** 02/16/10 *********************************************
     C***********                         + C5stat
     C*** End Del *** 02/16/10 *********************************************
     C*** End   chg *** 11/17/09 *******************************************
     c*
     C                   endsl
     C*
     C                   endsr
     C*
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6 (Compliance Codes)
     C*===============================================================
     C     OPT6          BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      'W7487R02'    ACDPL                    99
     C*
     C                   ENDSR
     C*
      *
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
     C*
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C                   END
     C*
     C                   endsr
     C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
     C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     c*
     C                   movel(p)  hd(2)         schdl2
     c*
     c     @@rrn         chain     pd101
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
     c*
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
     C                   endsr
     C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key2          reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     key2          readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
     C*
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
     C*
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   select
     C                   when      sscode <> *blanks
     C                              and sscode <> C5code
     C                   eval      meetSS = *off
     C                   when      ssStat <> *blanks
     C                              and ssStat <> C5stat
     C                   eval      meetSS = *off
     C                   endsl
     C*
     C                   endsr
     C*



** Column headings
Opt Compliance  Description
      Code
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Mark Completed         NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
