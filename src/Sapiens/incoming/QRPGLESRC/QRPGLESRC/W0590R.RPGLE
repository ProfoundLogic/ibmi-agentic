     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/TITLE W0590R - Work with Reserve Worksheet
     H Bnddir('WTBNDDIR')
     H*********************************************************************
     H*  CREATED: 03/29/10  PROGRAMMER: C:190 PROJECT: 25WQD              *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 12/13/13  C:062  Project: WC025 84292                             *
     H*                  Correct paging settings                          *
     H*                                                                   *
     H* 06/11/10  C:190  Project: 25WQD-                                  *
     H*                  Made changes to parameter list due to changes    *
     H*                  done under project 25WQO.                        *
     H*********************************************************************
    H*
     fw0590f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     sfile(scr01s:sfrrn1)
     f                                     sfile(scr02s:sfrrn2)
     f                                     infds(fids01)
    f*
     fwmrwp     if   e             disk
     f                                     rename(prw01:pfrcdf)
    f*
     fwmrwl     if   e           k disk
     f                                     infds(dbfds)
     f                                     rename(prw01:dbrcdf)

     fwmcmp     if   e             disk
     f                                     rename(pcm01:pcm02)
     fwmcdl     if   e           k disk
     Fsmcol     if   e           k disk
     fwmcml1    if   e           k disk
     fwmeml     if   e           k disk
     fwmfnl     if   e           k disk
     fwmupl     if   e           k disk
     fwtabl     if   e           k disk
     f                                     ignore(pat01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f*************                        ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcs01)
     f                                     ignore(pcn01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f*************                        ignore(pna01)
     f                                     ignore(pnc01)
     f*************                        ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
     D @@conm          s                   like(coname)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Reserve Worksheet'
     D slmsgi          c                   'PRJ9998'
     D slmsgf          c                   'PJMESG'
     D acdpgm          c                   'W0590R01'
     D comapi          c                   'SA005R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D*** Begin chg *** 12/13/13 *****************************
     D*sf1pag          c                   12
     D sf1pag          c                   7
     D*** End   chg *** 12/13/13 *****************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------

     D                 ds
     D  cmss#                  1      9
     D  ss1                    1      3
     D  ss2                    4      5
     D  ss3                    6      9
     D*
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##erchk         s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@call          s              1    inz(*blanks)
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@endt          s              8  0
     D @@dedt          s              8  0
     D* kkmnam          s                   like(opmnam)
     d*xztbid          s                   like(gttbid)
     d*xxkey           s                   like(gtkey)
     d xxacdt          s                   like(cmacdt)
     d xxcase          s                   like(cmcase)
     d xxco#           s                   like(cmco#)
     d xxfnd           s                   like(cmfnd)
     d xxfyr           s                   like(cmfyr)
     d*** Begin Add *** 12/13/13 **************************************
     d kkidt           s                   like(rwidt)
     d kkitm           s                   like(rwitm)
     d*** End   Add *** 12/13/13 **************************************
     d******* BEG CHG ************ 06/11/10 ***********************
     D CD2             s              2    DIM(9999)                            BR2 CODES
     D WK#             s              3  0 DIM(9999)                            BR2 # OF WKS
     D DA#             s              1  0 DIM(9999)                            BR2 # OF DYS
     D AM2             s             11  2 DIM(9999)                            BR2 CODE AMT
     D***** CD2             s              2    DIM(100)                             BR2 CODES
     D***** WK#             s              3  0 DIM(100)                             BR2 # OF WKS
     D***** DA#             s              1  0 DIM(100)                             BR2 # OF DYS
     d******* END CHG ************ 06/11/10 ***********************
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
    c*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    ##mode
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2
     C                   parm                    ##erchk
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

     C     dtmmdy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key           klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase

     C     key1          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     c*** Begin Add *** 12/13/13 **************************************
     C                   kfld                    kkidt
     C                   kfld                    kkitm
     c*** End   Add *** 12/13/13 **************************************

     c*  Key list for the claimant demographics file
     c*
     c     cdky          klist
     c                   kfld                    cmss#

     c*  Employer master file
     c*
     c     emky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    cmemp#
     c                   kfld                    cmdiv

     c*  Key list for the fund master file
     c*
     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     C*
     C*  Key list for the user parameter file
     C*
     C     upky          klist
     C                   kfld                    q1user
     C*
     C     cokey         klist                                                  COMPANY
     C                   kfld                    upco#
     *
     C*   Table file
     C     tbky          klist
     C                   kfld                    cmco#
     C                   kfld                    kkkey            10
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    c*================================================================
    c* ERRCK1 - Additional errors on subfile selection
    c*================================================================
     c     errck1        begsr

      *  If a 2=Change is selected on record that has been decided
      *  (RWSTS not equal "P") the program will be called in display mode.
     c                   if        scsel = '2'  and
     c                             rwsts <> 'P'
     c                   eval      scsel = '5'
     c                   endif


     C                   endsr                                                  errck1
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
     c                   if        ##erchk = 'Y'
     c                   seton                                        42  90
     c                   endif
     c*** Begin Add *** 12/13/13 **************************************
     c                   eval      kkidt=0
     c                   eval      kkitm=0
     c*** End   Add *** 12/13/13 **************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     c*** Begin Add *** 12/13/13 **************************************
     c                   eval      kkidt=scidt
     c                   eval      kkitm=scitm
     c*** End   Add *** 12/13/13 **************************************
     C                   endsr
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   if        ##erchk <> 'Y'
     c                   z-add     rwendt        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   Movel     srdto         @@endt

     c                   z-add     rwdedt        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   Movel     srdto         @@dedt
     c*** Begin Add *** 12/13/13 **************************************
     c                   eval      scidt=rwidt
     c                   eval      scitm=rwitm
     c*** End   Add *** 12/13/13 **************************************

     c                   if        *in21 = *on
     c                   eval      scdata = rwtype
     c                              + '   ' + rwauid
     c                              +%editw(@@endt:'  /  /    ')
     c                              + '    '  + rwsts
     c                              + '    ' + rwduid
     c                              +  '  ' + %editw(@@dedt:'  /  /    ')
     c                              +  '     ' + %editc(rwtars:'4')
     c                   endif
     c                   else
     c                   seton                                        42  90
     c                   endif

     c*****              if        ##erchk = 'Y'
     c****               seton                                        42  90
     c****               endif

     c                   endsr
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
      *
     C*
     C*  Retrieve the user parameter, company and fund records
     C*
     C                   z-add     0             xxco#
     C     upky          chain     pup01                              60
     C                   if        *in60 = *off

 001 C     cokey         chain     pco01                              60
     C                   if        *in60 = *off
 002 C                   movel(p)  coname        ##data
 002 C                   call      'SRCENTER'
 002 C                   parm                    ##data
 002 C                   parm      40            ##leng
 002 C                   movel     ##data        @@conm
E002 C                   endif
     C                   movel     @@conm        scconm

     C                   z-add     upco#         xxco#
 002 C                   movel(p)  coname        ##data
     C                   endif                                                  *in60 = 0
     C*
     C     @@rrn         chain(n)  pcm02                              99
     C                   move      cmfnd         xxfnd
     C                   move      cmfyr         xxfyr
     C                   move      cmcase        xxcase
     C*
     C     fnky          chain     pfn01                              60
     C                   z-add     xxfnd         scfnd
     C  n60              movel     fnname        scname
     C*
     C*  Retrieve the claimant information
     C*
      *  fyr-case
     c                   move(p)   xxfyr         xafyr             3
     c                   move(p)   XXCASE        xacase            7
     c     xafyr         cat(p)    '-':0         scyrcl
     c     scyrcl        cat       xacase:0      scyrcl
     c*
      *  Claimant ss#
     c                   if        cmss# <> *blanks
      *****  Begin Chg  ***  06/09/20  **********************
     c*****ss1           cat(p)    '-':0         scss#
     c*****scss#         cat       ss2:0         scss#
     c*****scss#         cat       '-':0         scss#
     c*****scss#         cat       ss3:0         scss#
      /Free
       scss# = DisplaySS#(MaskData('SSN':'cmss#':cmss#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   endif                                                  cmss# <> *blanks
      *
     c     emky          chain     pem01                              60
     c                   if        *in60 = *off
      *
      * Concatenate emp #, div, & name
      *
     c                   call      'WA005R'
     c                   parm                    cmemp#
     c                   parm                    cmdiv
     c                   parm                    emname
     c     scedn         parm                    xxedn            80
     c                   endif                                                  *IN60='0'
     c*
     c                   movel     cmacdt        xxacdt
     c                   z-add     cmacdt        srdti
     c                   call      'SRCVTDAT'    DTMDYY
     c                   movel     srdto         scacdt

     c                   move      *blank        scscl#
     c                   movel     cmacl#        scscl#
     C*
     c*  Retrieve injury desc
     c                   movel(p)  cmnatr        kkkey
     c     tbky          chain     pna01                              99
     c                   if        (not *in99)
     c                   movel(p)  nadesc        scinjd                         NCCI NATURE INJ DESC
     c                   endif

     c*  Retrieve the claimant name
     c*
     c     cdky          chain     pcd01                              60
     c                   move      *blanks       sccnam
     c                   movel     cdlsn         sccnam
     c                   if        cdlsn <> *blanks
     c     sccnam        cat       ',':0         sccnam
     C     sccnam        cat       cdfsn:1       sccnam
     C     sccnam        cat       cdmid:1       sccnam
     C                   else
     C                   movel     cdfsn         sccnam
     C     sccnam        cat       cdmid:1       sccnam
     C                   endif                                                  CDLSN <> ' '

     C                   movea     *zeros        wk#
     C                   movea     *zeros        da#
     d******* BEG CHG ************ 06/11/10 ***********************
     C                   movea     *zeros        am2
     d******* BEG CHG ************ 06/11/10 ***********************
     c                   movel     cmco#         qqco#
     c                   movel     cmfnd         qqfnd
     c                   movel     cmfyr         qqfyr
     c                   movel     cmcase        qqcase

     c                   call      'WA576R5'
     c                   parm      *blanks       qqrtcd            1
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    qqfyr             3
     c                   parm                    qqcase            7
     c                   parm      *blanks       qqtrcd            2
     c                   parm      *blanks       qqtype            1
     c                   parm      *blanks       qqrecd            2
     c                   parm      *blanks       qqretp            1
     c                   parm      *blanks       cd2
     c                   parm                    wk#
     c                   parm                    da#
     c                   parm      *zeros        qqtamt           11 2
     c                   parm      *zeros        qqtrsc           11 2
     c                   parm                    am2              11 2
     C*
     c                   eval      sctinr = qqtrsc

     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key           reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key           readpe    dbrcdf                                 95
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
** Column headings
Opt Type Adjuster   Enter date Status Decided by   Decision dt   Reserves
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   YNN
212F12=Cancel               YYN
217F17=Subset               NNN
3
