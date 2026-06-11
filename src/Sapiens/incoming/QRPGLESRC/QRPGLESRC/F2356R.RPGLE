      *=========================================================================
     H copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================
     H/TITLE - Work With IPPS Provider Specific File                           *
      *                                                                        *
      *  DESCRIPTION: This program Shows the W/W  Screen for FTPFP             *
      *                                                                        *
      *   CALLED BY: MENU                                                      *
      *                                                                        *
      *  CREATED: 06/12/2008  C:189 PROJECT TX039                              *
      *=========================================================================
      * REVISIONS:
     H*
     H*  11/16/12 C:287 Project: MC367
     H*                 Disable function code F17 and option 5 for GS
     H*                                                                        *
     H*  05/15/09 C:198 Support: 68468                                         *
     H*                 Compilation only                                       *
     H*                                                                        *
     H* 08/05/08 C:189 Project: TX039                                          *
     H*                Change PFEFDT To PFFFDT( Eff. Date Of First Period)     *
     H*                                                                        *
     H*                                                                        *
     H*                                                                        *
      *
      ***************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2356f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftpfp     if   e             disk
     F                                     rename(ptp01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fftpfl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ptp01:dbrcdf)
    F*  Secondary logical file definition (for subset option)
     Fftpfl1    if   e           k disk
     F                                     rename(ptp01:dbrcdf1)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IPPS Hosp. Prov. Specific File'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2356R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
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
     D kkprov          s                   like(pfprov)
      *** BEG CHG ***** 08/05/08 ******************************************
     D****** kkefdt          s                   like(pfefdt)
     D kkefdt          s                   like(pfffdt)
      *** END CHG ***** 08/05/08 ******************************************
      *** BEG DEL ***** 08/05/08 ******************************************
     D****** kkrcd           s                   like(pfrcd)
      *** END DEL ***** 08/05/08 ******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
      ****
      ****
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@rrn             9 0
    C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
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
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkprov
     C                   kfld                    kkefdt
      *** BEG DEL ***** 08/05/08 ******************************************
     C***********        kfld                    kkrcd
      *** BEG DEL ***** 08/05/08 ******************************************
      *
     C     key2          klist
     C                   kfld                    kkefdt
     C                   kfld                    kkprov
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssprov <> *blank or ssefdt <> 0 or
     C                             ssstat <> ' '
     C                   seton                                        11
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*********          move      '0'           *in57
     C*********          if        scpstd <> 0
     C*
     C*********          z-add     scpstd        srdti
     C*********          call      'SRCVTDAT'    datchk
     C*
     C*********          if        srerr = '1'
     C*********          seton                                        57
     C*********          endif
     C*
     C*********          endif                                                  scpstd <> 0
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   eval      *in57 = *off
     C                   eval      *in58 = *off
     C*
     C                   if        ssefdt <> *zeros
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endif
     C*
     C                   if        ssstat <> ' '
     C                             and ssstat <> 'A' and ssstat <> 'I'
     C                   eval      *in58 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
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
     C                   eval      kkprov = *loval
     C                   eval      kkefdt = *loval
      *** BEG DEL ***** 08/05/08 ******************************************
     C********           eval      kkrcd  = *loval
      *** END DEL ***** 08/05/08 ******************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C   21              eval      kkprov = scpst1
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
      *** BEG CHG ***** 08/05/08 ******************************************
     C********           call      'SRCVTDAT'    dtcymd
     C                   call      'SRCVTDAT'    dtcymd2
      *** END CHG ***** 08/05/08 ******************************************
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   movel(p)  scprov        kkprov
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C*
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C*
      *** BEG DEL ***** 08/05/08 ******************************************
     C*********          Z-add     scrcd         kkrcd
      *** END DEL ***** 08/05/08 ******************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     pfprov        scprov
      *** BEG CHG ***** 08/05/08 ******************************************
     C********           z-add     pfefdt        srdti
     C                   z-add     pfffdt        srdti
      *** BEG CHG ***** 08/05/08 ******************************************
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scefdt
     C                   Movel     pfstat        scstat
     C*
     C                   eval      scdata = scprov  + '                '
     C                                    + %editw(scefdt:'  /  /    ')
     C                             + '                               '
     C                                          + %trim(scstat)
      *** BEG DEL ***** 08/05/08 ******************************************
     C******             Z-add     pfrcd         scrcd
      *** END DEL ***** 08/05/08 ******************************************
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*** Beg Add *** MC367 *** 11/16/12 ****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/16/12 *****************************
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
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
     C                   MOVE      SCDBRR        @@RRN
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   if        ssprov <>  ' ' and ssprov <> pfprov
     C                   eval      meetss = *off
     C                   endif
      *
     C                   z-add     ssefdt        srdti
      *** BEG CHG ***** 08/05/08 ******************************************
     C*******            call      'SRCVTDAT'    dtcymd
     C                   call      'SRCVTDAT'    dtcymd2
      *** END CHG ***** 08/05/08 ******************************************
     C                   if        srerr = '0'
      *** BEG CHG ***** 08/05/08 ******************************************
     C************       MOVEL     SRDTO         xxefdt            7 0
     C                   MOVEL     SRDTO         xxefdt            8 0
      *** END CHG ***** 08/05/08 ******************************************
     C                   endif
      *
      *** BEG CHG ***** 08/05/08 ******************************************
     C***********        if        ssefdt <>  *zeros and xxefdt <> pfefdt
     C                   if        ssefdt <>  *zeros and xxefdt <> pfffdt
      *** BEG CHG ***** 08/05/08 ******************************************
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> pfstat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
** Column headings
Opt  Provider number       Effective date                            Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
