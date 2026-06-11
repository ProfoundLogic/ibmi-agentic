     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
    H*********************************************************************
     H/Title W7945R - Allow for Bank selection
    H*********************************************************************
    H*  Description: Work With Bank Select Workfile                      *
    H*               Allows selection of multiple banks for EFT/ACH      *
    H*                                                                   *
    H*  Called from: W7945C                                              *
    H*  Calls to: W1265R                                                 *
    H*  Created: C:191  Project: WC1806  Dated: 06/29/09                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*
    H* 05/28/14 C:136 Project: 129WTG
    H*               -Chg Sort to IDST/EFTF/ACCT (Code not commented)
    H*               -Don't display WDW01 (Submit job msg). It locks
    H*                up the W1266C job.
    H*
/* 07h* 07/29/10  C:180  SUPPORT 73695
    H*
    H**************************************************************************************
     Fw7945f    cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*  Physical file definition (for refreshing subfile record)
     Fwxckp     if   e             disk    rename(pck01:pfrcdf)
    F*  Primary logical file definition (for displaying items)
     Fwxckl2    if   e           k disk    rename(pck01:dbrcdf)
     F                                     infds(dbfds)
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(14) ctdata perrcd(1)
     D ofd             s             35    dim(14) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Bank Selection'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W7945R02'
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
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(ckco#)
     D kkacct          s                   like(ckacct)
     D kkidst          s                   like(ckidst)
     D kkeftf          s                   like(ckeftf)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C/COPY SCOPYWW01C
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
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##outq
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkidst
     C                   kfld                    kkeftf
     C                   kfld                    kkacct
     C*
     c     upky          klist
     c                   kfld                    q1user
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
     C                   eval      kkidst = *loval
     C                   eval      kkeftf = *loval
     C                   eval      kkacct = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   eval      kkco# = ##co#
     C                   movel     scpst1        kkidst
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkco# = ##co#
     C                   eval      kkidst = scidst
     C                   eval      kkeftf = sceftf
     C                   eval      kkacct = scacct
     C                   endsr
    C*================================================================
    C* KGDO - Process F7=Submit
    C*================================================================
     C     kgdo          begsr
     C*
     C*  Read through file to make sure at least one group is included
     C     ##co#         chain     dbrcdf                             71
     C                   dow       *in71=*off
     C                             and ckinex <> 'I'
     C     ##co#         reade     dbrcdf                                 71
     C                   enddo
     C*
     C   71              eval      *in90 = *on
     C*
     C*    Go on only if no error
     C                   if        *in90 = *off
     C*
     C                   move      ##co#         #1co#
     C                   call      'W1265R'
     C                   parm                    #1co#             3
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C***** BEGIN ADD  ******** 07/29/10 **********************************
     C                   parm                    @@call
     c                   if        @@call = *ON
      *** beg del ***** 05/28/14 ******************************************
     c***********        EXFMT     wdw01
      *** end del ***** 05/28/14 ******************************************
     c                   move      *on           *inkc
     c                   endif
     C***** END   ADD  ******** 07/29/10 **********************************
     C*
     C                   endif
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* KHDO - Process F8=Edit Report
    C*================================================================
     C     khdo          begsr
     C*  Read through file to make sure at least one group is included
     C     ##co#         chain     dbrcdf                             71
     C                   dow       *in71=*off
     C                             and ckinex <> 'I'
     C     ##co#         reade     dbrcdf                                 71
     C                   enddo
     C*
     C   71              eval      *in90 = *on
     C*    Go on only if no error
     C                   if        *in90 = *off
     C                   move      ##co#         #1co#
     C                   call      'W1265CA'
     C                   parm                    #1co#             3
     C                   parm                    ##outq
     C                   endif
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* KUDO - Process F20=Chg status on all
    C*================================================================
     C     kudo          begsr
     C*
     C                   eval      ##mode = 'A'
     C                   call      acdpgm        acdpl                  99
     C                   exsr      kedo
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
      /free
       begsr mvdbsc;
          scIdst = CKidst;
          scEftf = CKeftf;
          scAcct = CKacct;
          scBkNm = CKbknm;
          scInEx = CKinex;
          If CKinex='E';
             scStat = 'N';
          Else;
             scStat = 'Y';
          Endif;
          scData = scIdst + '  ' + scEftf + '  ' +  scAcct + '  ' +
           scBkNm + '  ' + scStat;
       endsr;
      /end-free
    C*================================================================
    C* Opt20 - Subroutine to change the status
    C*================================================================
     C     opt20         begsr
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C                   endsr                                                  opt20
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C     *inkg         caseq     *on           kgdo                       f7=submit
     C     *inkh         caseq     *on           khdo                       f8=edit report
     C     *inku         caseq     *on           kudo                       f20=change all
     C                   endcs
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C     scsel         caseq     '20'          opt20
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
     C     ##co#         reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     ##co#         readpe    dbrcdf                                 95
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
Opt  Immed Dest  File Name   Bank Acct #           Bank Name           Slct
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Submit                YNN
208F8=Edit Report           YNN
212F12=Cancel               NNY
217F17=Subset               NNN
220F20=Change Status on all YNN
3
