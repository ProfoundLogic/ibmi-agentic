     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*
     H**********************************************************************
     H**   WORK WITH BILL RATE CODES                                      **
     H**                                                                  **
     H**   CREATED 09/27/00 BY C:062 FOR PROJECT:AR032                    **
     H**********************************************************************
     H** REVISIONS:
     H*
     H*  12/02/02 c:62 Project MCMNT
     H*                Correct array index
     H*
     H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) actgrp(*caller)
      /ENDIF
     Fn2335f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fnmbdp     if   e             disk
     F                                     rename(pbd01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fnmbdl4    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pbd01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Fnmbdl3    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pbd01:dbrcdf2)
     Ffmfnl     if   e           k disk
    D*
     D br2             ds
     D  br3                          30a
     D  fnbr01                 1      3
     D  fnbr02                 4      6
     D  fnbr03                 7      9
     D  fnbr04                10     12
     D  fnbr05                13     15
     D  fnbr06                16     18
     D  fnbr07                19     21
     D  fnbr08                22     24
     D  fnbr09                25     27
     D  fnbr10                28     30
     D br                             3    dim(10) overlay(br3 : 1)
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
     D pgm             s             70    dim(2) ctdata perrcd(1)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Bill Rate Codes'
     D optn2           c                   'Bill Rate Code Information'
     D dltmsg          c                   '**Deleted**'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'N2335R01'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   2
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
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkcode          s                   like(bdcode)
     D kktype          s                   like(bdtype)
     D kkefdt          s                   like(bdefdt)
     D xxsel           s                   like(scsel)
     D xxcmd           ds           140    inz
     D xxtype                  9     10
     D xxprds                 19     58
     D xxcall                 62     62
     D xxcode                 72     74
     D xxefdt                 78     85
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@rrn
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
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kktype
     C                   kfld                    kkcode
     C                   kfld                    kkefdt

     C     key2          klist
     C                   kfld                    kkcode
     C                   kfld                    kktype
     C                   kfld                    kkefdt
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

     C                   if        scsel = '2' or scsel = '5'
     C                   eval      xxsel = scsel
     C                   eval      scsel = '99'
     C                   endif

     C                   if        scsel = '4' and scstat = 'A'
     C                   seton                                        7190
     C                   endif

     C                   if        scsel = '20' and scstat = 'A'
     C     ##co#         chain     pfn01                              99
     C     *in99         doweq     *off

     C*** begin add *** 12/02/02 *******************************************
     C**********         z-add     *zeros        x                 3 0
     C                   z-add     1             x                 3 0
     C*** end   add *** 12/02/02 *******************************************
     C     sccode        lookup    br(x)                                  99
     C                   if        *in99 = *on
     C                   seton                                        7290
     C                   leave
     C                   endif

     C     ##co#         reade     pfn01                                  99

     C                   enddo
     C                   endif

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

     C                   eval      kkcode = *blanks
     C                   eval      kktype = *blanks
     C                   eval      kkefdt = *zeros

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr

     C   22              eval      kkcode = scpst2
     C   22              eval      kktype = *blanks
     C   21              eval      kkcode = *blanks
     C   21              eval      kktype = scpst1
     C                   eval      kkefdt = *zeros

     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr

     C                   eval      kkcode = sccode
     C                   eval      kktype = sctype
     C                   eval      kkefdt = scefdt

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   eval      sccode = bdcode
     C                   eval      sctype = bdtype
     C                   eval      scefdt = bdefdt

     C   22              movel(p)  bdcode        scdata
     C   22              move      bdtype        scdata

     C   21              movel(p)  bdtype        scdata
     C   21              move      bdcode        scdata

     C                   movel     bdcom1        scdesc
     C                   movel     bdstat        scstat

     C                   z-add     bdefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefd2

     C                   endsr
    C*================================================================
    C* OPT20 = Change Status
    C*================================================================
     C     opt20         begsr

     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99

     C                   endsr
    C*================================================================
    C* OPT99 = Change/Display
    C*================================================================
     C     opt99         begsr

     C** Call QCMDEXC to get program based on type entered.

     C                   z-add     140           xxlen
     C                   movel(p)  pgm(1)        xxcmd
     C                   move      pgm(2)        xxcmd
     C                   movel(p)  sctype        xxtype
     C                   movel(p)  sccode        xxcode

     C                   Z-ADD     scefdt        srdti
     C                   CALL      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt

     C                   movel(p)  optn2         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##DATA           80
     C                   parm      40            ##LENG            3 0
     C                   movel     ##data        xxprds

     C                   select
     C     xxsel         wheneq    '2'
     C                   movel     ' '           xxcall
     C     xxsel         wheneq    '5'
     C                   movel     '1'           xxcall
     C                   endsl

     C                   call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxlen            15 5

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

     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '99'          opt99
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr

     C                   seton                                        lr

     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr

     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif

     C                   if        *in22 = *on
     C                   read      dbrcdf2                                95
     C                   endif

     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr

     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif

     C                   if        *in22 = *on
     C                   readp     dbrcdf2                                95
     C                   endif

     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr

     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif

     C                   if        *in22 = *on
     C     key2          setgt     dbrcdf2                            96
     C                   endif

     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr

     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif

     C                   if        *in22 = *on
     C     key2          setll     dbrcdf2                            96
     C                   endif

     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr

     C                   movel(p)  dltmsg        scdata

     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr

     C                   eval      @@rrn = scdbrr

     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt  type    code  Description                                Eff. date Sts
Opt  code    type  Description                                Eff. date Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               NNN
3
** pgm
CALL N92XXR PARM('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 'x'
'xxx' 'xxxxxxxx')
