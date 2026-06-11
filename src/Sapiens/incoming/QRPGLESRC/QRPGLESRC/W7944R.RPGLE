     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**************************************************************************************
     H/Title W7944R - Allow for group selection                                            *
    H**************************************************************************************
    H*                                                                                    *
    H*  !!!!  Avoid hard coding changes into this program.  Use WTGSP so that chgs !!!!   *
    H*  !!!!  are table driven. For example, look at GSDIEF, GSCOPE, and GSCRRE as !!!!   *
    H*  !!!!  examples. Changes will be made to WTGSP, LODWTGSP, and possibly      !!!!   *
    H*  !!!!  W7944R01.                                                            !!!!   *
    H*                                                                                    *
    H*  Description: Work With Renewal tracking Group Workfile.                           *
    H*               Allows selection of multiple groups for download.                    *
    H*                                                                                    *
     H*  Add new programs to call by  adding records to WTGSP (Reference 114WUK)           *
     H*  WTGSP has the following fields:                                                   *
     H*          GSID   - Key field in WTGSP. This is the value that you will pass through *
     H*                   @@PGM. @@PGM will be chained to WTGSP to pull in the following:  *
    H*          GSTITL - screen title for the Group Select (this program)                 *
    H*          GSPPGM - program to call                                                  *
    H*          GSPLST - a value of 1 to 4 corresponds to which PLIST (PLIST1 - 4) to     *
    H*                   use for the calling program. Add new PLIST's as needed. A        *
     H*                   value of 0 means a PLIST is not needed.                          *
    H*          GSKC   - KC parm value, if needed                                         *
    H*          GSKL   - KL parm value, if needed                                         *
    H*          GSPRDS - screen title for next called program, if needed                  *
    H*          GSCALL - call value for next called program, if needed                    *
    H*          GSHDG  - provides a way to control which heading                          *
    H*                   this screen will display. A value of 1 will pull in hd(1), a     *
    H*                   value of 2 will in hd(2).                                        *
    H*          GSDIEF - Default Inc/Exc Flag                                             *
    H*          GSCOPE - Condition on Policy Export                                       *
    H*          GSCRRE - Condition on Medicare RRE#                                       *
    H*                                                                                    *
    H*  Called from: W7944C                                                               *
    H*                                                                                    *
    H*  Calls to:                                                                         *
    H*                                                                                    *
    H*  Created: C:53   Project: 129WJW/WC1277  03/16/05                                  *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*                                                                                    *
     H*  08/24/17  C:33  Project: 146WGV                                                   *
     H*                  Allow for GS access.                                              *
     H*                                                                                    *
     H*  06/10/15  C:280 Project: WC2261                                                   *
     H*                  Change verbage on submit window                                   *
     H*                                                                                    *
     H*  10/14/14  C:279 Project: 114WNC                                                   *
     H*                  Added processing for check schedule maintenance.                  *
     H*                                                                                    *
     H*  08/26/11  C:53  Project: WC2029                                                   *
     H*                  add W7483C to the load for GSRRE#                                 *
     H*                                                                                    *
     H*  09/22/11  C:278 Support 78028  WA099                                              *
     H*                  F10 search issue on subset screen SCR03.                          *
     H*                                                                                    *
     H*  05/12/11  C:33  Project WC1851  Priss #144                                        *
     H*                  Use GSCREE to determine if RRE# should be on subset               *
     H*                   screen.                                                          *
     H*                                                                                    *
     H*  02/15/11  C:061 Project WC1851                                                    *
     H*                  Remove processing for EXFIRM error.                               *
     H*                                                                                    *
     H*  11/23/10  C:189 Project 139WPQ                                                    *
     H*                  Add the subset functionality on NCCI Carrier#                     *
     H*                                                                                    *
     H*  10/19/10  C:198 Project WC1879 Priss# 71                                          *
     H*                  Fixed resubmission issue.                                         *
     H*                                                                                    *
     H*  10/05/10  C:251 Project WC1944                                                    *
     H*                  Remove Confirmation Window based on *IN31                         *
     H*                                                                                    *
     H*  07/26/10  C:190 Project 25WQL  PRISS 26                                           *
     H*                  Corrected error message issue.                                    *
     H*                                                                                    *
     H*  07/23/10  C:198 Project WC1879 Priss# 35                                          *
     H*                  Search functionality on subset screen                             *
     H*                                                                                    *
     H*  07/08/10  C:198 Project WC1879                                                    *
     H*                  Re-submission processing                                          *
     H*                  -If allowed to re-submit,then making sure if user whas taken 20   *
     H*                   on group to be re-submitted, but does want to come out w/o       *
     H*                   re-submission                                                    *
     H*                                                                                    *
     H*  05/18/10  C:198 Project WC1879                                                    *
     H*                  -Added a funtion key to for F17=subset                            *
     H*                  -Added functionality to check whether user has authority to       *
     H*                   re-submit.                                                       *
     H*                                                                                    *
     H*  05/03/10  C:136 Project WC1694 PRISS                                              *
     H*                  The exclusion of groups without Policy Export set up ws moved to  *
     H*                  W7944R01                                                          *
     H*                                                                                    *
     H*  04/06/10  C:33  Project: WC1838    25WQW Priss# 18                                *
     H*                  F12 should not be available as the user could press enter and the *
     H*                   job will not run without going to the rest of the prompts.       *
     H*                                                                                    *
     H*  03/05/10 C:136 - Project: 25WQL PRISS 13                                          *
     H*                   Prevent Fee Invoice from being run on a group in which a         *
     H*                   DARTS Remark code has not been set up.                           *
     H*                                                                                    *
     H*  02/04/10  C:198 Project: WC1819 Priss 57                                          *
    H*                  Corrected F12 or No on are you sure window to go back to Group    *
    H*                  Selection screen.                                                 *
     H*                                                                                    *
     H*  12/15/09  C:53  Project: WC1819                                                   *
    H*                  Remove code that conflicts with new processing.    rictions.      *
     H*                                                                                    *
     H*  11/10/09  C:235 Project: 114wvd                                                   *
    H*                  Selection fo multiple groups.                      rictions.      *
     H*                                                                                    *
     H*  11/02/09  C:65  Project: WC1819                                                   *
    H*                  Pass calling program name along for select all restrictions.      *
     H*                                                                                    *
     H*  10/26/09  C:198 Project: WC1819 Phase 4
     H*                  Changed so that if call parameter = 'E' then do not display       *
     H*                  "Are you sure" window.                                            *
     H*                                                                                    *
     H*  09/22/09  C:198 Project: WC1819 Priss# 14                                         *
     H*                  Added to changes made during 114WUK so that option heading is     *
     H*                   changed in case of W7480R.                                       *
     H*                                                                                    *
     H*  08/14/09  C:232 Project: WC1819 Phase 5.                                          *
    H*                  Add processing for Medicare Query File Submission.                *
     H*                                                                                    *
     H*  07/27/09  C:136 Project: 114WUK                                                   *
     H*                  Change program so that new Group Select screens                   *
     H*                  can be added via a table (WTGSP)                                  *
     H*                                                                                    *
     H*  06/26/09  C:198 Project: WC1694                                                   *
    H*                 -Added Group Select to Policy Export process.                      *
     H*                 -Added condition on subset (C:136)                                 *
     H*                                                                                    *
     H*  06/12/09  C:65  Project: WC1819                                                   *
    H*                  Add processing for Medicare extract, listing RRE# in a column,    *
     H*                  displaying an "Are you Sure (Y/ )?" window, and verifying if      *
     H*                  prior extract has received a response to allow a new file.        *
     H*                                                                                    *
     H*  05/14/09  C: 58 Project: 139WLN                                                   *
    H*                  -Added a new constant TtlW7084R                                   *
     H*                  -Call to W7084R if XXPGM = 'W7084R'                        *
     H*                                                                                    *
     H*  07/25/08  C:183 Project: 114WQS                                                   *
    H*                  -Added a new constant TtlW3071R                                   *
     H*                  -Gave a call to W3071R if XXPGM = 'W3017R'                        *
     H*                                                                                    *
     H*  05/02/08  C:33  Project: 81WZB  WA081/62871                                       *
    H*                  Re-activate F3 for CLT081.                                        *
     H*                                                                                    *
     H*  12/26/07  C:183 Project: 81WZB  Dividend Processing                               *
    H*                  Call W7995R if @@PGM contains 'W7995R'. Also change the title of  *
     H*                  the program if @@PGM = 'W7995R'                                   *
     H*                                                                                    *
     H*  08/16/06  C:33  Project: WC1143                                                   *
    H*                  Aging Report Across Groups                                        *
    H**************************************************************************************
     Fw7944f    cf   e             workstn
     F***  Begin Del  ***  08/24/17  *************************************
      **********/IF DEFINED (WEB)
     F**********                           HANDLER('PROFOUNDUI(HANDLER)')
      **********/ENDIF
     F***  End   Del  ***  08/24/17  *************************************
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxgsp     if a e             disk
     F                                     rename(pgs01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwxgsl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pgs01:dbrcdf)
     Fwmd1l     if   e           k disk
     Fwtgsp     if   e           k disk
     Fwmexl     if   e           k disk
     Fwmcxl     if   e           k disk
     Fwdd1l     uf   e           k disk    rename(pd101:pd102)
     F                                     prefix(d2:2)
     Fwdf2l     if   e           k disk
    F*
    D*
     D*** Beg Chg *** 08/19/19 ****************************************
     D*** hd              s             78    dim(2) ctdata perrcd(1)
     D hd              s             78    dim(3) ctdata perrcd(1)
     D*** End Chg *** 08/19/19 ****************************************
     D of              s              1    dim(14) ctdata perrcd(1)
     D ofd             s             35    dim(14) alt(of)
     D sys_date        s               d   datfmt(*usa)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D gsFound         s               n
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D TtlW7942R       c                   'Renewal Tracking Group Select'
     D TtlW7290R1      c                   'Aging Report Group Select'
     D TtlW7995R       c                   'Surplus Dividend Gr+
     D                                     oup Select'
     D ttlW8370R       c                   'Policy Export Group Select'
     D TtlW3017R       c                   'Agent Commission An+
     D                                     alysis Report Group Select'
     D TtlW7084R       c                   'Ullico Claim Export'
     D TtlW7480R       c                   'Medicare Extract Gr+
     D                                     oup Select'
     D TtlW7488R       c                   'Medicare Query File+
     D                                     Submission'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W7944R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
     D normal          c                   x'20'
     D underscore      c                   x'24'
     D nonDisp         c                   x'27'
     D nonDispProtect  c                   x'A7'
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
     D @@call          s              1
     D @@pgm           s             10
     D xxPgm           s                   like(@@pgm)
     D optNm           s                   like(SCPRDS)
     D kkco#           s                   like(gsco#)
     D kkfnd           s                   like(gsfnd)
     d rcdFoundGS      s               n   inz(*off)
     D ##prds          s             25    inz(*blank)
     D ##call          s              1    inz(*blank)
     Dxxrsbm           s              1    inz(' ')
     Dsvfnd            s                   like(d2fnd)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     D                 ds
     D xxdata                  1     75
     D   xxfnd                 1      3  0
     D   xxfnnm                6     60
     D   xxiefl               51     58
     D   xxrre#               60     68
     D   xxcar#               70     74

     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D LO              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     DMatchString      pr             1n   opdesc
     D string1                      999    options(*varsize) const
     D string2                      999    options(*varsize) const
     D*
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     d VldFund1        pr                  extPgm('WTCOMCHK01')
     d ##errFnd                            like(##errFnd)

     d ValidFnd        pr                  extPgm('WTCOMCHK02')
     d ##errFnd                            like(##errFnd)

     d ##errFnd        s              1
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================

     c     plist1        plist
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl

     c     plist2        plist
     c                   parm                    ##prds
     c                   parm                    ##call
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl

     c     plist3        plist
     c                   parm                    ##prds
     c                   parm                    ##call

     c     plist4        plist
     c                   parm                    ##prds
     c     plist5        plist
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##call
     c                   parm                    ##rrn#            9 0

     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@pgm
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##pgm
     C                   parm                    @@car#            5 0
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C     fnky          klist
     C                   kfld                    kkco#
     C     d1ky          klist
     c                   kfld                    kkco#
     c                   kfld                    scfnd
     C     d1ky1         klist
     c                   kfld                    xxexdt            7 0
     c                   kfld                    kkco#
     c                   kfld                    scfnd
     c*
     C     cxky          klist
     c                   kfld                    kkco#
     c                   kfld                    scfnd
     C     f2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    gsfnd
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr                                                  chkssm
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C  n90              if        xxPgm = 'W7480R'
     c                   if        scsel = '20'
     c*
     c     d1ky          setgt     pd101
     c     d1ky          readpe    pd101                                  60
     c                   if        *in60 = *off and d1rsdt = 0
     c*** only user with proper authority can take 20 option again to re-submit
     c
     C                   move      '1'           SRRES             1
     C     cxky          chain     pcx01                              60
      *
     C     *IN60         IFEQ      *off
     C                   move      ##LVL         SRULVL
     C                   MOVE      CXRBS         SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        srutyp            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXRBS1        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM                    SRRES             1
     C                   PARM      CXRBS1        SRCTP2            1
     C                   PARM      CXRBS2        SRCTP3            1
     C                   PARM      CXRBS3        SRCTP4            1
     C                   PARM      CXRBS4        SRCTP5            1
     C                   PARM      CXRBS5        SRCTP6            1
      *
     C                   ENDIF
      *
      *         SRRES = '0' if "Passed" Security Check
      *                 '1' if "Did Not Pass" Security Check
      *
     C                   MOVEL     'N'           ##AUTH            1
     C     SRRES         IFEQ      '0'
     C                   MOVEL     'Y'           ##AUTH
     C                   ENDIF
     C*
     C                   if        ##auth <> 'Y'
     c                   move      *on           *in90
     c                   move      *on           *in35
     C                   else
     C                   select
     C*
     C                   when      gsiefl = 'E'
     C*
     C     d1ky          setgt     pd102                              60
     C     d1ky          readpe    pd102                                  60
     C                   dow       *in60 = *off
     C                   if        d2rsbm = ' ' and d2rsdt = 0 and d2type <> 'Q'
     C                   eval      d2rsbm = 'Y'
     C                   move      'Y'           xxrsbm
     C                   update    pd102
     C                   endif                                                  d2type <> 'Q'
     C     d1ky          readpe    pd102                                  60
     C                   enddo                                                  *in60 = *off
     C*
     C                   when      gsiefl = 'I'
     C*
     C     d1ky          setgt     pd102                              60
     C     d1ky          readpe    pd102                                  60
     C                   dow       *in60 = *off
     C                   if        d2rsbm = 'Y' and d2rsdt = 0 and d2type <> 'Q'
     C                   eval      d2rsbm = ' '
     C                   eval      xxrsbm = ' '
     C                   update    pd102
     C                   endif                                                  d2type <> 'Q'
     C     d1ky          readpe    pd102                                  60
     C                   enddo                                                  *in60 = *off
     C*
     C                   endsl
     C*
     C                   endif                                                  ##auth = 'N'
     c                   endif
     c*
     c                   endif                                                  scsel = '20'
     c                   endif


     C                   endsr                                                  errck1
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   if        *inkj = *on
     C                   exsr      kjdo
     C                   endif
     c                   if        *in90 = *off
     c                   setoff                                       79
     c     ssstat        ifne      'E'
     c     ssstat        andne     'I'
     c     ssstat        andne     ' '
     C                   SETON                                        7990
     c                   endif
     c                   endif
     C                   endsr                                                  errck2
    C*================================================================
    C* kjdo - F10 on subset screen.
    C*================================================================
     C     kjdo          begsr
     c*
     C                   seton                                        90
     c*
     c                   select
     c*
     c                   when      hfld = 'SSFND'
     c                   move      ##co#         #1co#
     C                   call      'W0010R'
     C     *INKC         parm                    ##kc
     C     *INKL         parm                    ##kl
     C                   parm      '2'           #2call            1
     C                   parm                    #1co#             3
     C                   parm      *blanks       #1fnd             3
      *
     C                   if        #1fnd <> ' '
     C                   move      #1fnd         ssfnd
     C                   seton                                        8190
     C                   else
     C                   seton                                        8090
     C                   endif
      *
      *
     C                   endsl
      *
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     c                   eval      ssfnd  = 0
     c                   eval      ssdesc = *blanks
     c                   eval      ssstat = ' '
     c                   eval      ssrre# = 0
     c                   eval      sscar# = 0
     C                   endsr                                                  inzsst
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      *loval        kkfnd
     C*
     C                   endsr                                                  keyinz
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      scpst1        kkfnd
     C*
     C                   endsr                                                  keyset
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = scfnd
     C*
     C                   endsr                                                  keysfl
    C*================================================================
    C* KGDO - Process F7=Submit
    C*================================================================
     C     kgdo          begsr
     C*
     C                   eval      *in80 = *off
     C*  Read through file to make sure at least one group is included
     C*
     c                   eval      gsFound = *off
     C     fnky          chain     dbrcdf                             71
     C                   dow       *in71=*off

     c                   if        GSIEFL = 'I'
     c                   eval      gsFound = *on
     c                   leave
     c                   endif
     C     fnky          reade     dbrcdf                                 71
     C                   enddo

     c                   if        not(gsFound)
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   endif
      /free
       If *in90 = *off and xxPgm = 'WTCOMCHK';
          ##errFnd = ' ';
          ValidFnd(##errFnd);

          If ##errFnd = 'Y';
             *in90 = *on;
          Else;
             VldFund1(##errFnd);
             If ##errFnd = 'Y';
                *in90 = *on;
             Endif;
          Endif;

       Endif;
      /end-free

     c*
     c* Determine if multiple RRE's have been selected for Medicare 2nd Pay.
     c*
     C  n90              if        xxPgm = 'W7480R'
     c                             or *in31 = *on
     c                             or xxPgm = 'W7480RE'
     c                   exsr      submit
     c                   endif
     c*
     c* Display a confirmation window before proceeding for Medicare 2nd Pay.
     c*
     C  n90              if        xxPgm = 'W7480R'
     c*
     c                   move      *blank        scsure
     c                   eval      *in71 = *off
     c*
     C                   if        xxpgm <> 'W7480RE   '
     c                   dou       *inkl = *on or scsure = 'N' or scsure = 'Y'
     c                   exfmt     wdwsur
     c                   if        scsure <> 'Y' or scsure <> 'N'
     c                   eval      *in72 = *on
     c                   else
     c                   eval      *in72 = *off
     c                   endif
     c                   enddo
     C                   endif
     c
     c                   if        *inkl = *on
     c                             or  scsure = 'N'
     c                   move      *off          *inkl
     c                   move      *on           *in90
     c                   endif                                                  if *inkl =
     c
     c                   endif
     C*
     C*    Go on only if no error
     C                   if        *in90 = *off
     C*
     C*** clear of make sure that F7 is pressed once a group is selected for re-submission
     C                   move      ' '           xxrsbm
     C                   select
     c                   when      rcdFoundGS
     c                   select
     c                   when      GSPLST = 0
     c                   call      GSPPGM
     c                   when      GSPLST = 1
     c                   call      GSPPGM        plist1
     c                   when      GSPLST = 2
     c                   call      GSPPGM        plist2
     c                   when      GSPLST = 3
     c                   call      GSPPGM        plist3
     c                   when      GSPLST = 4
     c                   call      GSPPGM        plist4
     c                   when      GSPLST = 5
     c                   call      GSPPGM        plist5
     c                   endsl
     C                   when      xxPgm = 'W7942R'
     C                   call      'W7942R'
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C                   when      xxPgm = 'W7290R1'
     C                   call      'W7290R1'
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C                   when      xxPgm = 'W7995R'
     C                   call      'W7995R'
     C                   parm      *BLANKS       @@PRDS           40
     C                   parm      *BLANK        @@CALL            1
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C                   IF        @@KC = '1'
     C                   EVAL      OUTFLG = *ON
     C                   ENDIF                                                  @@KC = '1'
     C                   when      xxPgm = 'W8370R'
     C                   eval      @@prds = 'Group Policy Select'
     C                   eval      @@call = 'G'
     C                   call      'W8370R'
     C                   parm                    @@PRDS           40
     C                   parm                    @@CALL            1
     C                   when      xxPgm = 'W3017R'
     C                   call      'W3017R'
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C                   when      xxPgm = 'W7084R'
     C                   call      'W7084R'
     C                   parm      *BLANKS       @@PRDS           40
     C                   when      xxPgm = 'W7480R'
     C                   call      'W7480R'
     C                   parm      *BLANK        @@CALL            1
     C                   EVAL      OUTFLG = *ON
     C                   endsl
     C                   EVAL      OUTFLG = *ON
     C                   endif
     C*
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* KUDO - Process F20=Chg status on all
    C*================================================================
     C     kudo          begsr
     C*
     C                   eval      ##mode = 'A'
     C                   move      xxpgm         ##pgm            10
     C                   Z-Add     sscar#        @@car#
     C                   call      acdpgm        acdpl                  99
     C                   exsr      kedo
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C*  Move values into hidden subfile fields
     C                   eval      scfnd = gsfnd
     C*
     C                   eval      xxfnd = gsfnd
     C                   eval      xxfnnm = gsname
     C                   if        xxPgm = 'W7480R'
     c                             or xxPgm = 'W7480RE'
     c                             or xxPgm = 'W7488R'
     c                             or GSCRRE = 'Y'
     c                   move      gsrre#        xxrre#
     c                   endif
     C                   if        gsiefl='E'
     C                   eval       xxiefl = 'Exclude'
     C                   else
     C                   eval       xxiefl = 'Include'
     C                   endif
     C                   Clear                   xxcar#
     C     f2ky          chain     pf201                              60
     C                   If         *in60 = *off
     C                   Movel     f2car#        xxcar#
     C                   else
     C                   Movel     '00000'       xxcar#
     C                   endif
     C*
     C                   eval      scdata = xxdata
     C*
     C                   endsr                                                  mvdbsc
    C*================================================================
    C* Opt20 - Subroutine to change the status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr                                                  opt20
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkg         caseq     *on           kgdo                       f7=submit
     C     *inku         caseq     *on           kudo                       f7=submit
     C                   endcs
     C*
     C                   endsr                                                  othfks
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
     C                   endsr                                                  othopt
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   if        xxrsbm = 'Y'
     C*** if user is exiting without taking F7 for re-submission groups
     C*** then display a window telling them that there are some re-submissions
     C*** to be done
     C                   exfmt     wdwrsb
     C*** Give them options to end the program, clear the resubmit flags (this may be difficult,have
     C*** to be careful only to clear those that were set with the current session), or return to
     C*** the program
     C                   select
     C*** chose to end the program
     C                   when      scend = 'Y'
     C     kkco#         setgt     pd102                              60
     C     kkco#         readpe    pd102                                  60
     C                   dow       *in60 = *off
     C                   if        d2rsdt <> 0 and d2type <> 'Q'
     C*** skip the group
     C                   eval      svfnd = d2fnd
     C                   dow       svfnd = d2fnd and *in60 = *off
     C     kkco#         readpe    pd102                                  60
     C                   enddo
     C                   endif                                                  *d2rsdt <> 0
     C                   if        d2rsbm = 'Y' and d2rsdt = 0 and d2type <> 'Q'
     C                   eval      d2rsbm = ' '
     C                   update    pd102
     C                   Endif
     C     kkco#         readpe    pd102                                  60
     C                   enddo                                                  *in60 = *off
     C*** chose to return to the program
     C                   when      scend = 'N'
     C*** turn on rstpgm
     C                   move      *on           rstpgm
     C                   endsl
     C                   endif                                                  xxrsbm = 'Y'
     C                   if        xxpgm  = 'W7417R1' and *inkc = *on
     C                   Eval      @@pgm = '*ON'
     C                   Endif
     C                   endsr                                                  pgmend
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   select
     C                   when      %parms >= 2 and  @@pgm <> *blanks
     C                   eval      xxPgm = @@pgm
     C                   other
     C                   eval      xxPgm = 'W7942R'
     C                   endsl
     C*
     c     xxpgm         chain     wtgsp
     c                   eval      rcdFoundGS = %found
     C*
     C                   select
     c                   when      rcdFoundGS
     c                   eval      optNm  = GSTITL
     c                   eval      hd(1)  = hd(GSHDG)
     c                   eval      ##kc   = GSKC
     c                   eval      ##kl   = GSKL
     c                   eval      ##prds = GSPRDS
     c                   eval      ##call = GSCALL
     C                   select
     C                   when      xxpgm = 'W7480R'
     C                   eval      *in30 = *on
     C                   when      xxpgm = 'W7488R'
     C                   eval      *in31 = *on
     C                   endsl
     C                   if        gsf17 <> 'Y'
     C                   move      '2'           xxopfn
     C                   move      'NNN'         xxmis
     C                   move      '17'          xxnmbr
     C                   exsr      updofa
     C                   endif
     C*
     C                   select
     C                   when      GSCRRE = 'Y'
     C                   eval      rreP = normal
     C                   eval      rreV = underscore
     C                   other
     C                   eval      rreP = nonDisp
     C                   eval      rreV = nonDispProtect
     C                   endsl
     C*
     C                   if        xxpgm = 'W7480R'
     C                   eval      hd(1) = hd(2)
     C                   endif
     C                   when      xxPgm = 'W7942R'
     C                   eval      optNm = ttlW7942R
     C                   when      xxPgm = 'W7290R1'
     C                   eval      optNm = ttlW7290R1
     C                   when      xxPgm = 'W7995R'
     C                   eval      optNm = ttlW7995R
     C                   when      xxPgm = 'W8370R'
     C                   eval      optNm = ttlW8370R
     C                   when      xxPgm = 'W3017R'
     C                   eval      optNm = ttlW3017R
     C                   when      xxPgm = 'W7084R'
     C                   eval      optNm = ttlW7084R
     C                   when      xxPgm = 'W7480R'
     C                   eval      optNm = ttlW7480R
     c                   eval      hd(1) = hd(2)
     C                   when      xxPgm = 'W7417R1'
     C                   eval      optNm = 'groups'
     C                   endsl
     C*
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3

     C*
     C                   endsr                                                  pgmstr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   if        *in21 = *on
     C     fnky          reade     dbrcdf                                 95
     C                   endif
     C*
     C                   endsr                                                  read
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   if        *in21 = *on
     C     fnky          readpe    dbrcdf                                 95
     C                   endif
     C*
     C                   endsr                                                  readp
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C*
     C                   endsr                                                  setgt
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C*
     C                   endsr                                                  setll
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C*
     C*
     C                   endsr                                                  sf1upd
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C*
     C*
     C                   endsr                                                  slctrd
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   select
     C*
     C                   when      ssStat <> *blanks
     C                              and ssStat <> gsiefl
     C                   eval      meetSS = *off
     C                   when      ssfnd <> 0
     C                              and ssfnd <> gsfnd
     C                   eval      meetSS = *off
     C                   when      ssDesc <> *blanks
     C                              and not MatchString(gsname:ssDesc)
     C                   eval      meetSS = *off
     C                   when      ssrre# <> 0
     C                              and ssrre# <> gsrre#
     C                   eval      meetSS = *off
     C                   when      sscar# <> 0
     C     f2ky          chain     pf201                              60
     C                   If        (*in60 = *off and sscar# <> F2CAR#)
     C                             or *in60 = *on
     C                   eval      meetSS = *off
     C                   endif
     c*
     c                   endsl
     C*
     C                   endsr                                                  subset

    C*================================================================
    C* SUBMIT - Subroutine to validate group selection and proceed.
    C*================================================================
     C     submit        begsr
     c
     c                   z-add     0             svrre             9 0
     c                   move      *blank        xxerr             1
     c                   move      *blank        xxfrst            1
     c*
     c* Read through file to confirm if all rre id's match.
     c*
     c     fnky          chain(n)  dbrcdf                             60
     c     *in60         doweq     *off
     c
     C                   If        gsiefl = 'I'
     c
     c                   if        xxfrst = *blank
     c                   z-add     gsrre#        svrre
     c                   move      'Y'           xxfrst
     c                   endif                                                  if xxfrst =
     c
     c                   z-add     gsrre#        xxrre             9 0
     c
     c                   if        xxrre  <> svrre
     c                   move      'Y'           xxerr
     c                   leave
     c                   endif                                                  if svrre  <>
     c
     C                   Endif                                                  if gsiefl = 'I'
     c
     c     fnky          reade(n)  dbrcdf                                 60
     C                   Enddo
     c*
     c* Pop up error window if they have different carrier id's selected.
     c*
     c                   if        xxerr = 'Y'
     c
     c                   dou       *inkl = *on
     c                   exfmt     wdwmsg
     c                   enddo
     c
     c                   if        *inkl = *on
     c                   move      *off          *inkl
     c                   move      *on           *in90
     c                   endif                                                  if *inkl =
     c
     c                   endif                                                  if xxerr =
     c
     C                   endsr

     P*====================================================================
     P* Determine if the strings match
     P*====================================================================
     PMatchString      b
     DMatchString      pi             1n   opdesc
     D string1                      999    options(*varsize) const
     D string2                      999    options(*varsize) const
     D*
     D u1pdsc          s            999
     D u2pdsc          s            999
     D inLen1          s             10I 0
     D inLen2          s             10I 0
     C*
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen1:*omit)
     C                   callp     CEEDOD(2:descType:dataType:descInfo1:
     C                             descInfo2:inLen2:*omit)
     C*
     C                   eval      u1pdsc = %trim(
     C                                      %xlate(lo:hi:
     C                                      %subst(string1:1:inLen1)
     C                                            )
     C                                           )
     C                   eval      u2pdsc = %trim(
     C                                      %xlate(lo:hi:
     C                                      %subst(string2:1:inLen2)
     C                                            )
     C                                           )
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    u1pdsc                         ADD 01/16/99
     C                   PARM      inLen1        STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    u2pdsc                         ADD 01/16/99
     C                   PARM      inLen2        PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C                   select
     C                   when      scnres = 0
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl
     C*
     PMatchString      e
** Column headings
OptGroup Description                                  Status     NCCI Carrier#              11/23/10
OptGroup Description                                  Status   RRE#   Carrier#              11/23/10
OptGroup Description                                  Status   NCCI Carr#  NCCI Carr Group  08/19/19
** Description--------------MISN
1011=Include/Exclude        NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Change Status         YNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Submit                YNN
212F12=Cancel               NNN                              04/06/10
217F17=Subset               YYY                              05/18/10
220F20=Chg status on all    YNN
3
