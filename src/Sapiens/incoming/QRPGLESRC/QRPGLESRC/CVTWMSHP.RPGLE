     HCopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
      *================================================================
     H* Description:  Generate WMSHP submission history records for
     H*               all policies within a specified group.  Policy
     H*               records will be created for each policy and
     H*               cancellation records will be created for each
     H*               cancelled policy.
     H*
     H* CREATED: 09/23/13  C:65   Project: 147WAC - Optacomp Conversion
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     fWDELL     if   e           k disk
     F                                     rename(pel01:pel00)
     fWDELL1    if   e           k disk
     F                                     rename(pel01:pel01)
     fWMEML     if   e           k disk
     F                                     rename(pem01:pem00)
     fWMSHL1    if a e           k disk
     F                                     rename(psh01:psh01)
     fWDCXL10   if   e           k disk
     F                                     rename(pcx01:pcx10)
     Fwtgtl     if   e           k disk
     Fwmahl     if   e           k disk
      *
     d i               s              3  0 inz
     d x               s              3  0 inz
     d xxtype          s              1    inz
     D xxtbid          s                   like(gttbid)
     D xxkey           s                   like(gtkey)
     d xxst            s              2    inz
     d xzst            s              2    inz
     d xxTrdt          s              7  0 inz
     d xxCXDT          s              7  0 inz
     d xxco#           s              3  0 inz
     d xxfnd           s              3  0 inz
     d xxemp#          s              9  0 inz
     d xxdiv           s              5  0 inz
     d xxfyr           s              3  0 inz
     D sysDt7          s              7  0
      *
     D                 ds
     D  ed                     1     35  0 dim(5) ascend
     D  emefd1               001    007  0
     D  emefd2               008    014  0
     D  emefd3               015    021  0
     D  emefd4               022    028  0
     D  emefd5               029    035  0
     D                 ds
     D  cd                     1     35  0 dim(5) ascend
     D  emcad1               001    007  0
     D  emcad2               008    014  0
     D  emcad3               015    021  0
     D  emcad4               022    028  0
     D  emcad5               029    035  0
     D                 ds
     D  sst                    1     30    dim(15)
     D  shst01                 1      2
     D  shst02                 3      4
     D  shst03                 5      6
     D  shst04                 7      8
     D  shst05                 9     10
     D  shst06                11     12
     D  shst07                13     14
     D  shst08                15     16
     D  shst09                17     18
     D  shst10                19     20
     D  shst11                21     22
     D  shst12                23     24
     D  shst13                25     26
     D  shst14                27     28
     D  shst15                29     30
     D                 ds
     D  sss                    1     15    dim(15)
     D  shss01                 1      1
     D  shss02                 2      2
     D  shss03                 3      3
     D  shss04                 4      4
     D  shss05                 5      5
     D  shss06                 6      6
     D  shss07                 7      7
     D  shss08                 8      8
     D  shss09                 9      9
     D  shss10                10     10
     D  shss11                11     11
     D  shss12                12     12
     D  shss13                13     13
     D  shss14                14     14
     D  shss15                15     15
     D                 ds
     D  ssc                    1     15    dim(15)
     D  shcd01                 1      1
     D  shcd02                 2      2
     D  shcd03                 3      3
     D  shcd04                 4      4
     D  shcd05                 5      5
     D  shcd06                 6      6
     D  shcd07                 7      7
     D  shcd08                 8      8
     D  shcd09                 9      9
     D  shcd10                10     10
     D  shcd11                11     11
     D  shcd12                12     12
     D  shcd13                13     13
     D  shcd14                14     14
     D  shcd15                15     15
      *
     D                 DS
     D  SSD                    1    105  0
     D                                     DIM(15)
     D  SHDT01                 1      7  0
     D  SHDT02                 8     14  0
     D  SHDT03                15     21  0
     D  SHDT04                22     28  0
     D  SHDT05                29     35  0
     D  SHDT06                36     42  0
     D  SHDT07                43     49  0
     D  SHDT08                50     56  0
     D  SHDT09                57     63  0
     D  SHDT10                64     70  0
     D  SHDT11                71     77  0
     D  SHDT12                78     84  0
     D  SHDT13                85     91  0
     D  SHDT14                92     98  0
     D  SHDT15                99    105  0
     D                 DS
     D  STI                    1     90  0
     D                                     DIM(15)
     D  SHTM01                 1      6  0
     D  SHTM02                 7     12  0
     D  SHTM03                13     18  0
     D  SHTM04                19     24  0
     D  SHTM05                25     30  0
     D  SHTM06                31     36  0
     D  SHTM07                37     42  0
     D  SHTM08                43     48  0
     D  SHTM09                49     54  0
     D  SHTM10                55     60  0
     D  SHTM11                61     66  0
     D  SHTM12                67     72  0
     D  SHTM13                73     78  0
     D  SHTM14                79     84  0
     D  SHTM15                85     90  0
     D*
     D psds          esds                  extname(szq1p)
     D*
     D/copy sprString
     D/copy sprDate
     D/copy wPrEmplr
     D/copy wPrRateSet
    C*
    C*  KEY LIST FOR GENERIC TABLE FILE
    C*
     C     gtky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxtbid
     C                   kfld                    xxkey
    C*
    C*  KEY LIST FOR EMPLOYER MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
    C*
    C*  KEY LIST FOR EMPLOYER DETAIL FILE
    C*
     C     elky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
     C     keyWDELL1     klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
     C     key1WDELL1    klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
    C*
    C*  KEY LIST FOR SUBMISSION HISTORY FILE
    C*
     C     shky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxfyr
     C                   kfld                    xxtype
    C*
    C*  KEY LIST FOR CANCELLATION DETAIL FILE
    C*
     C     cxky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxCXDT
    C*
    C*  AGENT HISTORY FILE
    C*
     C     ahky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
      *
      * Entry List
      *
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
      *
     C                   Move      qqco#         xxco#
     C                   Move      qqfnd         xxfnd
     C                   eval      sysDt7 = DateTo7(SystemDate)
      *
     C     emky          Setll     PEM00
     C     emky          Reade     PEM00
      *
     C                   Dow       not %eof
      *
     C                   If        EMStrp = 'Y'
      *
     C                   Z-add     EMEMP#        xxEMP#
     C                   Z-add     EMDIV         xxDIV
      *
     c                   eval      i = 5
     C                   Z-add     0             xxCXDT
     c                   do        5
     c                   if        cd(i) <> 0 and ed(i) <> 0
     c                   eval      xxCXDT = cd(i)
     c                   leave
     c                   else                                                   cd(xl) <> 0
     c                   eval      i = i - 1
     c                   endif                                                  cd(xl) <> 0
     c                   enddo
      *
     C     elky          Setll     PEL00
     C     elky          Reade     PEL00
     C                   Dow       not %eof
     C
     C                   Z-add     ELFYR         xxFYR
     C                   Exsr      CreatePolicy
     C
     C     elky          Reade     PEL00
     C                   Enddo
      *
     C                   If        xxCXDT <> 0
     C                   Exsr      CreateCX
     C                   Endif
      *
     C                   Endif                                                  If EMStrp = 'Y'
     C
     C     emky          Reade     PEM00
     C                   Enddo
      *
     C                   Move      *on           *inlr
     C***************************************************************************************
     C*
     C* Create submission history record for policy year if it does not already exist.
     C*
     C     CreatePolicy  Begsr
     C
     C                   Move      'P'           xxType
     C     shky          Chain     PSH01
     C
     C                   If        not %found
     C
     C                   Clear                   psh01
     C                   Z-add     ELPFDT        xxTrdt
     C                   Exsr      popFields
     C
     C                   Write     psh01
     C
     C                   Endif                                                  If not %found
     C
     C                   Endsr                                                  CreatePolicy
     C***************************************************************************************
     C*
     C* Create submission history cancellation record if the policy is cancelled.
     C*
     C     CreateCX      Begsr
     C
     C     elky          Setgt     PEL00
     C     elky          Readpe    PEL00
     C
     C                   Dow       not %eof
     C
     C                   If        xxCXDT >= ELPFDT
     C                             and xxCXDT <= ELPTDT
     C
     C                   Z-add     ELFYR         xxfyr
     C
     C     cxky          Chain     PCX10
     C                   If        %found
     C
     C                   Move      'C'           xxType
     C
     C     shky          Chain     PSH01
     C
     C                   If        not %found
     C
     C                   Clear                   psh01
     C                   Z-add     xxCxdt        xxTrdt
     C                   Exsr      popFields
     C
     C                   Write     psh01
     C
     C                   Endif                                                  If not %found
     C
     C                   Endif                                                  If %found
     C
     C                   Endif                                                  If xxCXDT >= ELPFDT
     C
     C     elky          Readpe    PEL00
     C                   Enddo
     C
     C                   Endsr                                                  CreateCX
     C***************************************************************************************
     C*
     C* Populate common fields for submission history records.
     C*
     C     popFields     Begsr
     C*
     C                   Z-add     xxco#         shco#
     C                   Z-add     xxfnd         shfnd
     C                   Z-add     xxemp#        shemp#
     C                   Z-add     xxdiv         shdiv
     C                   Z-add     xxfyr         shfyr
     C                   Move      xxType        shType
     C                   Z-add     ELPfdt        shPfdt
     C                   Z-add     xxTrDt        shTrdt
     C                   Move      '4'           shStat
     C*
     C* Set New/Renewal for Code.
     C*
     C                   Move      'O'           shCode
     C                   Sub       1             xxFyr
     C     keyWDELL1     Chain     PEL01
     C                   If        %found
     C                   Move      'R'           shCode
     C                   Endif
     C                   Add       1             xxFyr
     C*
     C                   eval      SHDT = sysDt7
     C                   eval      SHTIME = TimeTo6(SystemTime)
     C                   eval      shidt = shdt
     C                   eval      shsbdt = shdt
     C                   eval      shsbtm = shtime
     C*
     C* Populate the Stat list for record.
     C*
     C                   Z-add     1             x
     C*
     C     key1WDELL1    Setll     PEL01
     C     key1WDELL1    Reade     PEL01
     C                   Dow       not %eof
     C
     C                   eval      xxST  = RatingSt(elco#:elrtcd)
    C*
     C                   movel(p)  'NCCI'        xxtbid
     C                   movel(p)  xxST          xxkey
     C     gtky          chain     pgt01                              60
    C*
     C                   if        *in60 = *off
     C                   move      'XX'          xzst
     C                   eval      i = 1
     C     xzst          lookup    sst(i)                                 60
    C*
     C                   if        *in60 = *off
     C                             and x <= 15
     C                   move      xzst          sst(x)
     C                   move      '4'           sss(x)
     C                   move      shCode        ssc(x)
     C                   Z-add     shsbdt        ssd(x)
     C                   z-add     shsbtm        sti(x)
     C
     C                   eval      x = x + 1
     C                   endif
    C*
     C                   else
    C*
     C                   move      xxSt          xzst
     C                   eval      i = 1
     C     xzst          lookup    sst(i)                                 60
    C*
     C                   if        *in60 = *off
     C                             and x <= 15
     C                   move      xzst          sst(x)
     C                   move      '4'           sss(x)
     C                   move      shCode        ssc(x)
     C                   Z-add     shsbdt        ssd(x)
     C                   z-add     shsbtm        sti(x)
     C
     C                   eval      x = x + 1
     C                   endif
    C*
     C                   endif
    C*
     C     key1WDELL1    Reade     PEL01
     C                   Enddo
    C*
     C                   eval      shagt# = *zero
     C     ahky          setgt     pah01
     C     ahky          readpe    pah01                                  60
     C                   dow       *in60 = *off
     C                             and ahstat = 'I'
     C     ahky          readpe    pah01                                  60
     C                   enddo
    C*
     C                   if        *in60 = *off
     C                   movel     ahagt#        shagt#
     C                   endif
    C*
     C                   movel     q1user        shuser
     C                   movel     q1pgm         shpgm
     C                   movel     q1user        shiusr
     C
     C                   Endsr                                                  popFields
     C***************************************************************************************
