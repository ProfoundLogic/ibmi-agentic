     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**************************************************************************************
     H/Title W7946R - Allow for state selection                                            *
    H**************************************************************************************
    H*                                                                                    *
    H*  !!!!  Avoid hard coding changes into this program.  Use WTGSP so that chgs !!!!   *
    H*  !!!!  are table driven. For example, look at GSDIEF, GSCOPE, and GSCRRE as !!!!   *
    H*  !!!!  examples. Changes will be made to WTGSP, LODWTGSP, and possibly      !!!!   *
    H*  !!!!  W7946R01.                                                            !!!!   *
    H*                                                                                    *
    H*  Description: Work With Renewal tracking State Workfile.                           *
    H*               Allows selection of multiple States for download.                    *
    H*                                                                                    *
     H*  Add new programs to call by  adding records to WTSSP (Reference 114WUK)           *
     H*  WTGSP has the following fields:                                                   *
     H*          SSID   - Key field in WTGSP. This is the value that you will pass through *
     H*                   @@PGM. @@PGM will be chained to WTGSP to pull in the following:  *
    H*          SSTITL - screen title for the Group Select (this program)                 *
    H*          SSPPGM - program to call                                                  *
    H*          SSPLST - a value of 1 to 4 corresponds to which PLIST (PLIST1 - 4) to     *
    H*                   use for the calling program. Add new PLIST's as needed. A        *
     H*                   value of 0 means a PLIST is not needed.                          *
    H*          SSKC   - KC parm value, if needed                                         *
    H*          SSKL   - KL parm value, if needed                                         *
    H*          SSPRDS - screen title for next called program, if needed                  *
    H*          SSCALL - call value for next called program, if needed                    *
    H*          SSHDG  - provides a way to control which heading                          *
    H*                   this screen will display. A value of 1 will pull in hd(1), a     *
    H*                   value of 2 will in hd(2).                                        *
    H*          SSDIEF - Default Inc/Exc Flag                                             *
    H*          SSCOPE - Condition on Policy Export                                       *
    H*          SSCRRE - Condition on Medicare RRE#                                       *
    H*                                                                                    *
    H*  Called from:                                                                      *
    H*                                                                                    *
    H*  Calls to:                                                                         *
    H*                                                                                    *
    H*  Created: C:296  Project: 114MQA Date: 11/25/14                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*  10/09/15 C: 079 Project: SY360                                                    *
    H*                  Use STSTL3 to exclude non-states                                  *
     H*                                                                                    *
    H**************************************************************************************
     Fw7946f    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Primary file definition (for refreshing subfile record)
     Fwxssell   if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pss01:dbrcdf)
     Fwxsselp   if a e             disk
     F                                     rename(pss01:PFRCDF)
     F*** Beg Chg *** 10/09/15 ***********************************
     F*****ststl     if   e           k disk    rename(pst01:pst02)
     Fststl3    if   e           k disk    rename(pst01:pst02)
     F*** End Chg *** 10/09/15 ***********************************
     Fststp     if   e             disk
    F*
     Fwtsselp   if   e           k disk
     D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(14) ctdata perrcd(1)
     D ofd             s             35    dim(14) alt(of)
     D sys_date        s               d   datfmt(*usa)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D stFound         s               n
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W7946R02'
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
     D kkco#           s                   like(ssco#)
     D kksst           s                   like(ssst)
     d rcdFoundSt      s               n   inz(*off)
     D ##prds          s             25    inz(*blank)
     D ##call          s              1    inz(*blank)
     Dxxrsbm           s              1    inz(' ')
     D*xxstnm           s                   like(ssdesc)
     D xxkey           s             10A
     D*xxsst            s                   like(ssst)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     D                 ds
     D xxdata                  1     75
     D   xxsst                 1      2
     D   xxstnm                6     60
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
     C                   kfld                    kksst
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kksst
     C     fnky          klist
     C                   kfld                    kkco#
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr                                                  chkssm
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
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
     c                   when      hfld = 'SSSTE'
     C                   call      'F0601R'
     C                   parm      '2'           #2call            1
     C     *INKC         parm                    ##kc
     C     *INKL         parm                    ##kl
     C                   parm      *ZEROS        ##RRN             9 0
     C                   parm      *ZEROS        ##RRN1            9 0
      *
     C                   if        ##RRN <> *zero and ##kl <> *on
     C     ##RRN         chain     pst01
     C                   MOVEL(P)  STKEY         SSSTE
     C                   MOVEL(P)  STNAME        SSDESC
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
     c                   eval      ssste  = *blanks
     c                   eval      ssdesc = *blanks
     c                   eval      ssstat = ' '
     c                   eval      ssrre# = 0
     c*                  eval      sscar# = 0
     C                   endsr                                                  inzsst
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      *loval        kksst
     C*
     C                   endsr                                                  keyinz
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      scpst1        kksst
     C*
     C                   endsr                                                  keyset
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kksst = scst
     C*
     C                   endsr                                                  keysfl
      /Free
      //*================================================================
      //  * KGDO - Process F7=Submit
      //*================================================================
         begsr kgdo;

         *in80 = *off;
         // Read through file to make sure at least one state is included

         stFound = *off;

         chain 1 PFRCDF;
           dow not %eof(wxsselp);

              if ssIeFl = 'I';
                 stFound = *on;
                 leave;
              endif;

           read PFRCDF;
           enddo;

              if not(stFound);
                 *in71 = *on;
                 *in90 = *on;
              endif;

           // Go on only if no error
              if *in90 = *off;

           // clear of make sure that F7 is pressed once a group is selected for re-submission
              xxrsbm = *blanks;
      /End-Free
     c                   if        rcdFoundSt = *on
     c                   select
     c                   when      SSPLST = 0
     c                   call      SSPPGM
     c                   when      SSPLST = 1
     c                   call      SSPPGM        plist1
     c                   when      SSPLST = 2
     c                   call      SSPPGM        plist2
     c                   when      SSPLST = 3
     c                   call      SSPPGM        plist3
     c                   when      SSPLST = 4
     c                   call      SSPPGM        plist4
     c                   when      SSPLST = 5
     c                   call      SSPPGM        plist5
     c                   endsl
     c                   endif
     C                   EVAL      OUTFLG = *ON
     C                   endif
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* KUDO - Process F20=Chg status on all
    C*================================================================
     C     kudo          begsr
     C*
     C                   eval      ##mode = 'A'
     C                   move      xxpgm         ##pgm            10
     C*                  Z-Add     sscar#        @@car#
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
     C                   eval      scst = ssst
     C*
     C                   eval      xxsst = ssst
     C                   eval      xxstnm = ssname
     C                   if        ssiefl='E'
     C                   eval       xxiefl = 'Exclude'
     C                   else
     C                   eval       xxiefl = 'Include'
     C                   endif
     C                   Clear                   xxcar#
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
     C*** if user is exiting without taking F7 for re-submission state
     C*** then display a window telling them that there are some re-submissions
     C*** to be done
     C                   exfmt     wdwrsb
     C*** Give them options to end the program, clear the resubmit flags (this may be difficult,have
     C*** to be careful only to clear those that were set with the current session), or return to
     C*** the program
     C                   select
     C*** chose to end the program
     C                   when      scend = 'Y'
     C*** chose to return to the program
     C                   when      scend = 'N'
     C*** turn on rstpgm
     C                   move      *on           rstpgm
     C                   endsl
     C                   endif                                                  xxrsbm = 'Y'
     C                   endsr                                                  pgmend
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   select
     C                   when      %parms >= 2 and  @@pgm <> *blanks
     C                   eval      xxPgm = @@pgm
     C                   endsl
     C*
     c     xxpgm         chain     wtsselp
     c                   eval      rcdFoundSt = %found
     C*
     c                   if        rcdFoundSt = *on
     c                   eval      optNm  = SSTITL
     c                   eval      hd(1)  = hd(SSHDG)
     c                   eval      ##kc   = SSKC
     c                   eval      ##kl   = SSKL
     c                   eval      ##prds = SSPRDS
     c                   eval      ##call = SSCALL
     C                   if        ssf17 <> 'Y'
     C                   move      '2'           xxopfn
     C                   move      'NNN'         xxmis
     C                   move      '17'          xxnmbr
     C                   exsr      updofa
     C                   endif
     C                   select
     C                   when      sSCRRE = 'Y'
     C                   eval      rreP = normal
     C                   eval      rreV = underscore
     C                   other
     C                   eval      rreP = nonDisp
     C                   eval      rreV = nonDispProtect
     C                   endsl
     C                   endif
     C*
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3

     C                   endsr                                                  pgmstr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C*
     C                   endsr                                                  read
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
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
     C     Key1          setll     dbrcdf                             96
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
     C                              and ssStat <> ssiefl
     C                   eval      meetSS = *off
     C                   when      ssste <> *blanks
     C                              and ssste <> ssst
     C                   eval      meetSS = *off
     C                   when      ssDesc <> *blanks
     C                              and not MatchString(ssname:ssDesc)
     C                   eval      meetSS = *off
     C                   when      ssrre# <> 0
     C                              and ssrre# <> ssrre#
     C                   eval      meetSS = *off
     c*
     c                   endsl
      /Free
       xxkey = %trim(ssst);
       chain xxkey pst02;
       //*** Beg Chg *** 10/09/15 ***********************************
       //*******  if not %found(ststl);
         if not %found(ststl3);
       //*** End Chg *** 10/09/15 ***********************************
            meetss = *off;
         endif;
      /End-Free
     C*
     C                   endsr                                                  subset

    C*================================================================
    C* SUBMIT - Subroutine to validate state selection and proceed.
    C*================================================================
     C     submit        begsr
     c
     c                   z-add     0             svrre             9 0
     c                   move      *blank        xxerr             1
     c                   move      *blank        xxfrst            1
     c*
     c* Read through file to confirm if all rre id's match.
     c*
     c                   read      dbrcdf                                 60
     c     *in60         doweq     *off
     c
     C                   If        ssiefl = 'I'
     c
     c                   if        xxfrst = *blank
     c                   z-add     ssrre#        svrre
     c                   move      'Y'           xxfrst
     c                   endif                                                  if xxfrst =
     c
     c                   z-add     ssrre#        xxrre             9 0
     c
     c                   if        xxrre  <> svrre
     c                   move      'Y'           xxerr
     c                   leave
     c                   endif                                                  if svrre  <>
     c
     C                   Endif                                                  if gsiefl = 'I'
     c
     c                   read(n)   dbrcdf                                 60
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
OptState Description                                  Status     NCCI                       11/23/10
OptState Description                                  Status   RRE#                         11/23/10
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
