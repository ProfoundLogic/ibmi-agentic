     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W8310R - Work With DCI Data Export                       *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: Work With DCI Data Export                *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: W8310R01                                            *
     H*                                                               *
     H* CREATED: 05/21/09  C:191 PROJECT WC1792                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/08/11 C:220  PROJECT: WC1987                               *
     H*                 Added the open & close Ratios column in the   *
     H*                 display                                       *
     H*                                                               *
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8310f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxf1p     if   e             disk
     F                                     rename(pf101:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwxf1l     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pf101:dbrcdf)
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'DCI Data Export'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W8310R01'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D kkco#           s                   like(f1co#)
     D kkfnd           s                   like(f1fnd)
     D kkst            s                   like(f1st)
     D xxco#           s              3
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D xxdt7                          7  0
     D  xxym                          4  0 overlay(xxdt7:2)
     D  xxyy                          2  0 overlay(xxdt7:2)
     D  xxmm                          2  0 overlay(xxdt7:4)
     D  xxdd                          2  0 overlay(xxdt7:6)
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
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
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     datck1        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
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
     C                   eval      *in90 = *off
     C                   eval      *in71 = *off
     C* On option 20, the group/state chosen must have a ratio > 0 to have a status of "Included"
     C                   If        scsel = '20' and scrtio <= 0 and scflag = 'E'
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   eval      *in07 = *on
     C                   eval      *in05 = *on
     C                   endif                                                  scsel = '20'
     C
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK4 - Error checking for SCR04
    C*================================================================
     C     errck4        begsr
     C                   eval      *in90 = *off
     C*
     C* Run period must be entered
     C                   if        scprd = *zeros
     C                   seton                                        9072
     C                   leavesr
     C                   endif                                                  scprd = *zeros
     C*
     C* Run period should be a valid month year
     C                   move      scprd         xxyy
     C                   movel     scprd         xxmm
     C                   eval      xxdd = 01
     C                   eval      srdti = xxdt7
     C                   call      'SRCVTDAT'    datck1
     C                   if        srerr <> '0'
     C                   seton                                        9073
     C                   leavesr
     C                   endif                                                  scprd = *zeros
     C*
     C* Transaction code should be 1, 2 or 3
     C                   if        sctrcd <> '1' and sctrcd <> '2'
     C                             and sctrcd <> '3'
     C                   seton                                        9074
     C                   leavesr
     C                   endif                                                  sctrcd <> 1,2,3
     C
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
     C                   eval      kkfnd = *loval
     C                   eval      kkst = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   move      scpst1        kkfnd
     C                   eval      kkst = scpst1a
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkfnd = scfnd
     C                   eval      kkst = scst
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scfnd = f1fnd
     C                   movel(p)  f1name        scname
     C                   eval      scst = f1st
     C                   eval      scrtio = f1rtio
     C*
     C                   eval      scflag = f1flag
     C                   if        scflag = 'I'
     C                   eval      scstat = 'Include'
     C                   else
     C                   eval      scstat = 'Exclude'
     C                   endif                                                  f1flag = 'I'
     C*
     C* Build SCDATA for subfile
     C***** Beg Chg ***** 03/08/11 ************************************
     C*************      eval      scdata = ' ' + %editc(scfnd:'4') + '   '
     C*************                       + scst + '  '
     C*************                       + %triml(scname) + '     '
     C*************                       + %editc(scrtio:'L') + '   '
     C*************                       + scstat
     C                   movel     scname        x1name           27
     C                   eval      scdata = ' ' + %editc(scfnd:'4') + '   '
     C                                    + scst + '  '
     C                                    + %triml(x1name) + '  '
     C                                    + %editc(scrtio:'L') + '     '
     C                                    + %editc(f1rtop:'L') + '   '
     C                                    + %editc(f1rtcl:'L') + '    '
     C                                    + scstat
     C***** End Chg ***** 03/08/11 ************************************
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C* Process F7=Submit
     C                   if        *inkg
     C                   exsr      kgdo
     C                   endif                                                  *inkg
     C*
     C                   endsr
    C*================================================================
    C* kgdo - Process F7=Submit
    C*================================================================
     C     kgdo          begsr
    C*
     C* Set screen defaults - Transaction code to '1'
     C                   eval      sctrcd = '1'
     C* Run Period to - previous month/year e.g. today is May, 26 2009; Run Period = 04/09 (Apr 09)
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt7
     C* Calculate previous month/year
     C                   if        xxmm = 1
     C                   eval      xxmm = 12
     C                   eval      xxyy = xxyy-1
     C                   else
     C                   eval      xxmm = xxmm-1
     C                   endif                                                  xxmm = 1
     C*
     C                   move      xxyy          scprd
     C                   movel     xxmm          scprd
     C**** Display Screen 04 and call W8310C
     C                   dou       *inkc or *inkl or not *in90
     C                   exfmt     scr04
     C*
     C* Process function keys
     C                   select
     C* F3=Exit
     C                   when      *inkc
     C                   leave
     C* F12=Cancel
     C                   when      *inkl
     C                   eval      *in90 = *on
     C                   leave
     C* Enter
     C                   other
     C* check errors
     C                   exsr      errck4
     C                   endsl
     C* If no errors found, process
     C                   if        not *in90
     C                   move      scprd         xxprd
     C                   move      sctrcd        xxtrcd
      **** call submitter
     C                   call      'W8310C'
     C                   parm                    xxco#
     C                   parm                    xxprd             4
     C                   parm                    xxtrcd            1
     C                   endif                                                  not *in90
     C*
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C                   endsr
    C*================================================================
    C* OPT20  - Process option 20
    C*================================================================
     C     opt20         begsr
     C                   eval      ##mode = 'S'
      **** call W8310R01
     C                   call      acdpgm        acdpl
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      kkco# = upco#
     C                   move      upco#         xxco#
     C                   endif                                                  %found
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     kkco#         reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     kkco#         readpe    dbrcdf                                 95
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
     C                   eval      scdata = '*** Deleted ***'
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
** Column headings
Opt  Group  St  Group Name                   Ratio   Open   Close   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=Submit                YNN
212F12=Cancel               NNY
217F17=Subset               NNN
3
