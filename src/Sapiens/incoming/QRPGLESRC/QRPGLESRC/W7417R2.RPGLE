     H*  (C) Copyright Stoneriver, 2009
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *
      *****************************************************************
      *  DESCRIPTION:  Electronic submission process group selection  *
      *                Screen.                                        *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 04/03/06 Programmer: c:168  project: 114WLK         *
      *    - COPY OF: 01/10/05 Programmer: c:65 project: 93wew        *
      *****************************************************************
      *  NOTE:  @@RRN is used for returning the RRN of Subfile for    *
      *         selection.                                            *
      *                                                               *
      *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  04/07/09  C:65   Project:  139WKB                            *
      *                   Process for WCSTAT when mdoe = 'S',         *
      *                   disabling the carrier # check on submit.    *
      *                                                               *
      *  08/07/07  C:61   Project:  114WLK                            *
      *                   Correct displayed program name.             *
      *                                                               *
      *  02/09/07  C:65   Project:  114WLK                            *
      *                   Submit groups through F7 key instead of     *
      *                   pressing enter after selection enabled.     *
      *                                                               *
      *  10/19/06  C:65   Project:  114WLK/Supp# 53886                *
      *                   Add carrier code to group selection screen  *
      *                   and add a soft edit on it matching.         *
      *                                                               *
      *   04/03/06  c:168  project:114WLK                             *
      *                    copied this program from client 93 to base *
      *                                                               *
      *****************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *                                                               *
      *****************************************************************
     *
     *
     Fw7417f2   cf   e             workstn
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
     *
     *  Physical file definition (for refreshing subfile record)
     Fwxpgp     uf   e             disk    usropn
     F                                     rename(xpg01:pfrcdf)
     *
     *  Primary logical file definition (for displaying items)
     Fwxpgl     uf a e           k disk    usropn
     F                                     infds(dbfds)
     F                                     rename(xpg01:dbrcdf)
      *
      *  Fund file used to load the subfile
     FWMFNL     If   E           K Disk
     f*** begin add ***  10/19/06  ********************************************
     FWDF2L     If   E           K Disk
     f*** end add *****  10/19/06  ********************************************
      *
     *
     *
     *
     *
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
     D Arr10           S              1    Dim(10)
     *
     *
    D*================================================================
    D*  /COPY required constants
    D*================================================================
      ******* beg chg ***** 08/07/07 **********************************
     D*optnm*******    c                   'Work with Group Selection'
     D optnm           c                   'Group Selection'
      ******* beg chg ***** 08/07/07 **********************************
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W7417R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D H6              S              6
     D H8              S              8
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(pgco#)
     D kkgrp           s                   like(pggrp)
     D Kkco#A          S              3
     D Srdfld          S             80
     D Srdlen          S              3  0
     D Datea           S              6
     D Datea8          S              8
     D DateJan         S              8
     D Mth             S              4  0
     D Basedate        S               D   DATFMT(*ISO)
     D ME_Date         S               D   DATFMT(*ISO)
     D Today           S               D   DATFMT(*USA)
     D Datusa          S               D   DATFMT(*USA)
     D Asofdt          S               D   DATFMT(*USA)
     D Timestmp        S             12  0
      *
     D Edited          S              1n
     D Six_Pos         S              1n
     D Eight_Pos       S              1n
     D Ten_Pos         S              1n
      *
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     *
     *
     C/COPY SCOPYWW01C
     *
     *
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@call
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##outq
    
    
    
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkgrp

     C     key2          Klist
     C                   Kfld                    kkco#

     c*** begin add ***  10/19/06  ********************************************
     C     f2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    xxfnd             3 0
     c*** end add *****  10/19/06  ********************************************


    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================


    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr



    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr


    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr



    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr


    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   z-add     *loval        Kkgrp
     C                   endsr


    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   Move      Scpst1        Kkgrp
     C                   endsr



    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   z-add     scgrp         Kkgrp
     C                   endsr



    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   Eval      scgrp  = Pggrp
     C                   Eval      scgrpn = Pgname
     c*** begin add ***  10/19/06  ********************************************
     c                   z-add     0             sccarr
     c                   move      scgrp         xxfnd
     c     f2ky          chain     pf201                              60
     c  N60              z-add     f2car#        sccarr
     c*** end add *****  10/19/06  ********************************************

     c                   clear                   scsts
     C                   Eval      *in31 = *off

     C                   If        Pgflag = 'E'
     C                   Eval      scsts  = 'Exclude'
     C                   Endif

     C                   If        Pgflag = 'I'
     C                   Eval      scsts  = 'Include'
     C                   Eval      *in31 = *on
     C                   Endif

     C                   endsr



      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         Begsr

     c                   unlock    wxpgp
     c                   unlock    wxpgl
     C                   Move      'S'           ##Mode
     C                   Call      ACDPGM        ACDPL                  99

     C                   Endsr



    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr


     c*** begin add ***  02/09/07  *************************************
     c     *inkg         caseq     *on           submit
     c                   endcs
     c*** end add *****  02/09/07  *************************************

     C                   endsr



    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
      *
     c     scsel         caseq     '20'          opt20
     c                   endcs

     C                   endsr



    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c*** begin chg ***  02/09/07  *************************************
     c*** begin add ***  10/19/06  ********************************************
     c*********          if        *inkc = *off and *inkl = *off
     c*********
     c*********          z-add     0             svcarr            5 0
     c*********          move      *blank        xxerr             1
     c*********          move      *blank        xxfrst            1
     c*
     c* Read through file to confirm if all carrier id's match.
     c*
     c*****key2          chain(n)  dbrcdf                             60
     c******in60         doweq     *off
     c*********
     C*********          If        Pgflag = 'I'
     c*********
     c*********          move      pggrp         xxfnd
     c*********          z-add     0             xxcarr            5 0
     c*****f2ky          chain     pf201                              60
     c*********
     c*********          if        *in60 = *off
     c*********
     c*********          if        xxfrst = *blank
     c*********          z-add     f2car#        svcarr
     c*********          move      'Y'           xxfrst
     c*********          endif                                                  if xxfrst =
     c*********
     c*********          z-add     f2car#        xxcarr
     c*********          endif                                                  if *in60 =
     c*********
     c*********          if        xxcarr <> svcarr
     c*********          move      'Y'           xxerr
     c*********          leave
     c*********          endif                                                  if svcarr <>
     c*********
     C*********          Endif                                                  if pgflag =
     c*********
     c*****key2          reade(n)  dbrcdf                                 60
     C*********          Enddo
     c*
     c* Pop up error window if they have different carrier id's selected.
     c*
     c*********          if        xxerr = 'Y'
     c*********
     c*********          exfmt     wdwmsg
     c*********
     c*********          if        *inkl = *on
     c*********          move      *off          *inkl
     c*********          move      *on           rstpgm
     c*********          endif                                                  if *inkl =
     c*********
     c*********          endif                                                  if xxerr =
     c*********
     c*********          endif                                                  if *inkc =
     c*********
     c*********          if        rstpgm <> *on
     c*** end add *****  10/19/06  ********************************************
     c*********          close     wxpgp
     c*********          close     wxpgl
     C*********          Eval        *Inlr = *On
     c*********          return
     c*********
     c*** end add *****  10/19/06  ********************************************
     c**********         endif                                                  if rstpgm <>
     c*** end add *****  10/19/06  ********************************************
     c                   if        *inkc = *off and *inkl = *off
     c                             and *inlr = *off

     c                   move      *on           rstpgm

     c                   else

     c                   close     wxpgp
     c                   close     wxpgl
     C                   Eval        *Inlr = *On
     c                   return

     c                   endif
     c*** end chg *****  02/09/07  *************************************
     C                   endsr



    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     c*
     c*** begin add ***  04/07/09  ***************************************
     c                   move      @@call        xxcall            1
     c                   if        @@call = 'S'
     c                   clear                   @@call
     c                   endif
     c*** end add *****  04/07/09  ***************************************
     c                   open      wxpgp
     c                   open      wxpgl
     c*
     C                   move      @@co#         ##co#
     C                   move      @@co#         Kkco#
      *
     C                   Time                    Timestmp
     C     *mdy          Move      Timestmp      Today
     C     *USA          Move      '01/31/1980'  basedate
      *
     C                   Movel(P)  ##conm        Srdfld
     C                   CALL      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      40            Srdlen
     C                   Movel     Srdfld        Scconm
      *
     C                   Exsr      Load_Wxpgp

     C                   endsr



    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read(n)   dbrcdf                                 95
     C                   endif
     C                   endsr



    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp(n)  dbrcdf                                 95
     C                   endif
     C                   endsr



    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr



    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   endsr



    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr



    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr



    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr



    C*================================================================
      * LOAD WORK FILE                                                *
    C*================================================================
     C     Load_Wxpgp    Begsr
      *
      * Clear file first
      *
     C                   Dou       %eof
     C                   Read      Wxpgp
      *
     C                   If        not %EOF
     C                   Delete    pfrcdf
     C                   Endif
      *
     C                   Enddo
      *
      * Load from group master
      *
     C     Kkco#         Setll     Wmfnl
      *
     C                   Dou       %eof
     C     Kkco#         Reade     Wmfnl
      *
     C                   If        not %eof
     C                   Eval      Pgco#  = Kkco#
     C                   Eval      Pggrp  = Fnfnd
     C                   Eval      Pgname = Fnname
     C                   Eval      Pgflag = 'E'
     C                   Write     Dbrcdf
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
     c*** begin add ***  02/09/07  *************************************

    C*================================================================
    C* SUBMIT - Subroutine to validate group selection and proceed.
    C*================================================================
     C     submit        begsr
     c
     c                   move      *off          rstpgm
     c
     c                   z-add     0             svcarr            5 0
     c                   move      *blank        xxerr             1
     c                   move      *blank        xxfrst            1
     c*
     c* Read through file to confirm if all carrier id's match.
     c*
     c     key2          chain(n)  dbrcdf                             60
     c     *in60         doweq     *off
     c
     C                   If        Pgflag = 'I'
     c
     c                   move      pggrp         xxfnd
     c                   z-add     0             xxcarr            5 0
     c     f2ky          chain     pf201                              60
     c
     c                   if        *in60 = *off
     c
     c                   if        xxfrst = *blank
     c                   z-add     f2car#        svcarr
     c                   move      'Y'           xxfrst
     c                   endif                                                  if xxfrst =
     c
     c                   z-add     f2car#        xxcarr
     c                   endif                                                  if *in60 =
     c
     c                   if        xxcarr <> svcarr
     c                   move      'Y'           xxerr
     c                   leave
     c                   endif                                                  if svcarr <>
     c
     C                   Endif                                                  if pgflag =
     c
     c     key2          reade(n)  dbrcdf                                 60
     C                   Enddo
     c*
     c* Pop up error window if they have different carrier id's selected.
     c*
     c                   if        xxerr = 'Y'
     c*** begin add ***  04/07/09  ***************************************
     c                             and xxcall <> 'S'
     c*** end add *****  04/07/09  ***************************************
     c
     c                   exfmt     wdwmsg
     c
     c                   if        *inkl = *on
     c                   move      *off          *inkl
     c                   move      *on           rstpgm
     c                   endif                                                  if *inkl =
     c
     c                   endif                                                  if xxerr =
     c*
     c* Pop up error window if they have selected no groups.
     c*
     c                   if        xxfrst = *blank
     c
     c     *inkl         doueq     *on
     c                   exfmt     wdwerr
     c                   enddo
     c
     c                   move      *off          *inkl
     c                   move      *on           rstpgm
     c
     c                   endif                                                  if xxerr =
     c*
     c* Exit out of program if no errors have occured or override has been taken.
     c*
     c                   if        rstpgm = *off

     c                   close     wxpgp
     c                   close     wxpgl
     C                   Eval        *Inlr = *On
     c                   return

     c                   endif
     c
     C                   endsr

     c*** end add *****  02/09/07  *************************************


** Column headings
Opt  Group  Carr#   Group Name                                     Status
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYN
207F7=Submit                YNN
212F12=Cancel               YYN
3
