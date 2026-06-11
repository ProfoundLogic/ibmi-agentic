     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Compliance Codes
     H*====================================================================
     H*  Created: 06/24/09   C:33   Project: WC1819
     H*====================================================================
    H*  Revisions:
     H*
     H*====================================================================
     Fw2106f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtdmp     if   e             disk    rename(pdm01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtdml     if   e           k disk    rename(pdm01:dbrcdf)
     F                                     infds(dbfds)
     Fwtdml1    if   e           k disk    rename(pdm01:dbrcdf2)
     F                                     infds(dbfds2)
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D#ofOpts          c                   15
     D of              s              1    dim(#ofOpts) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOpts) alt(of)
     D*
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     DMatchString      pr             1n   opdesc
     D string1                      999    options(*varsize) const
     D string2                      999    options(*varsize) const
     D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMS Compliance Codes'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2106R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D @@call          s              1
     D kkCode          s                   like(DMCODE)
     D kkDesc          s                   like(DMDESC)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    ##Co#
     C                   kfld                    kkCode
     C*
     C     key2          klist
     C                   kfld                    ##Co#
     C                   kfld                    kkDesc
     C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   eval      *in11 = ssDesc <> *blanks
     C                                  or ssStat <> *blanks
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
     C                              and ssStat <> 'A'
     C                              and ssStat <> 'I'
     C                   seton                                        71  90
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   eval      ssDesc = *blanks
     C                   eval      ssStat = 'A'
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkCode = *blanks
     C                   eval      kkDesc = *blanks
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkCode = scPst1
     C                   eval      kkDesc = scPst2
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkCode = scCode
     C                   eval      kkDesc = scDesc
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scCode = DMCODE
     C                   eval      scDesc = DMDESC
     C                   eval      scStat = DMSTAT
     C*
     c                   movel     dmdesc        xxdesc           50
     C*
     C                   select
     C                   when      info# = 1 and view# = 1
     C                   eval      scData = DMCODE + '   '
     C                                    + XXDESC + '        '
     C                                    + DMSTAT
     C                   when      info# = 2 and view# = 1
     C                   eval      scData = XXDESC + ' '
     C                                    + DMCODE + '        '
     C                                    + DMSTAT
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* OPT20 - Process option 20
    C*================================================================
     C     Opt20         begsr
     C*
     C                   eval      ##Mode = 'S'
     C                   call      acdPgm        acdPl
     C*
     C                   endsr
     C*
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
     C     scSel         caseq     '20'          Opt20
     C                   endcs
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
     C                   endsr
     C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     ##Co#         reade     dbrcdf                                 95
     C                   when      *in22
     C     ##Co#         reade     dbrcdf2                                95
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
     C     ##Co#         readpe    dbrcdf                                 95
     C                   when      *in22
     C     ##Co#         readpe    dbrcdf2                                95
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
     C                   when      *in22
     C     key2          setgt     dbrcdf2                            96
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
     C                   when      *in22
     C     key2          setll     dbrcdf2                            96
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
     C                   when      ssDesc <> *blanks
     C                              and not MatchString(dmdesc:ssdesc)
     C                   eval      meetSS = *off
     C                   when      ssStat <> *blanks
     C                              and ssStat <> DMSTAT
     C                   eval      meetSS = *off
     C                   endsl
     C*
     C                   endsr
     C*

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
Opt Code  Description                                              Sts
Opt Description                                            Code    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
