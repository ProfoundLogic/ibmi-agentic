     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H* This program will be used to calculate Medicarer ASC payments *
     H*****************************************************************
     H*  CREATED BY:                                                  *
     H*  10/01/10  C:198 Project: WV009                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/18/15  C:212  Project: NC039                              *
     H*                   For NC bills lookup the rate table using    *
     H*                   type = 'A'                                  *
     H*                                                               *
     H*  06/17/13  C:284  Project: SC022                              *
     H*                   For SC bills lookup the rate table using    *
     H*                   type = 'A'                                  *
     H*                                                               *
     H*  08/19/11  C:079  Project: 114MYS                             *
     H*                   If state = P1 change to PA                  *
     H*                                                               *
     H*  07/27/11  C:079  Project: WV010                              *
     H*                   Do not call FA763R for P3 or Z3             *
     H*                                                               *
     H*  07/20/11  C:273  Project: WV010                              *
     H*                   Updated for 2011 ASC data                   *
     H*                   Dup code from 114MWT when @@type = 'W'      *
     H*                                                               *
     H*  04/08/11  C:251  Project: 114MWT                             *
     H*                   Calculate true medicare ASC Payment         *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffaeolg    if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Fftrtl2    if   e           k disk
     Ffmasl1    if   e           k disk
      *
     F*** Beg Add *** 04/08/11 ****************************************
     Ffaeul     if   e           k disk
     Fftwgl1    if   e           k disk
     Fftwgl2    if   e           k disk    rename(pwg01:pwg02)
     Fststl1    if   e           k disk
     Ffddel3    uf a e           k disk
     FFddel     if   e           k disk
     F                                     rename(pde01:pde02)
     F*** End Add *** 04/08/11 ****************************************
     F*
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     Dkkst             s                   like(eoast)
     Dxxamt1           s              9  2
     D xxtype          s                   like(rttype)
     D xxst            s                   like(rtst)
     D kkefdt          s                   like(asefdt)
     D*** Beg Add *** 04/08/11 ************************************
     D Wpmt            s                   like(astpmt)
     D kkSeq#          s                   like(deseq#)
     D xxSeq#          s                   like(deseq#)
     D xxmra           s                   like(camra)
     D Total_Payment   s             14  4
     D camd50          s              3  2
     D camd51          s              3  2
     D mod1            s              8
     D mod             s              2    dim(4) ascend
     D*** End Add *** 04/08/11 ************************************
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@call            1
     C                   parm                    @@type            1
     C                   parm                    @@pi              2
     C                   parm                    @@eob             3
     C                   parm                    @@msg             7
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@alw4            9 2
     C                   parm                    @@rtcd            1
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     eoky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C*
     C     dhky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C     rtky1         klist
     C                   kfld                    xxst
     C                   kfld                    xxtype
     C                   kfld                    cafdat
     C*
     C     rtky2         klist
     C                   kfld                    xxst
     C                   kfld                    xxtype
     C*
     C     asky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C*
     C     asky2         klist
     C                   kfld                    cacpt4
     C*** Beg Add *** 04/08/11 **************************************
     C*
     C     euky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C*
     C     wgky          klist
     C                   kfld                    ##cbsa            5
     C                   kfld                    kkefdt
     C*
     C     wgky1         klist
     C                   kfld                    ##cbsa
     C*
     C     wgky2         klist
     C                   kfld                    ##nam            40
     C                   kfld                    kkefdt
     C*
     C     wgky21        klist
     C                   kfld                    ##nam
     C*
     C     stky          klist
     C                   kfld                    ##key            10
     C*
     C     deky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
     C*
     C     deky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C     duky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C*** End Add *** 04/08/11 **************************************
     C*
     C*** Main Processing
     C*
     C*Set the pointer for the FDCAP from the RRN
     C*
     C                   move      ' '           @@rtcd
     C     @@rrn         chain(n)  pca01                              99
     C                   if        *in99 = *on
     C                   Move      'F'           @@rtcd
     C                   return
     C*
     C                   else                                                   *in99=*on
     C*
     C     eoky          chain     peo01
     C*
     C                   if        %found
     C*
     C                   movel     eoast         xxst
     C*** Beg chg *** 06/17/13 *************************************************
     C                   if        eoast = 'SC'
     C*** Beg add *** 05/18/15 *************************************************
     C                             Or eoast = 'NC'
     C*** End add *** 05/18/15 *************************************************
     C                   movel     'A'           xxtype
     C                   else
     C*** End chg *** 06/17/13 *************************************************
     C                   movel     'M'           xxtype
     C*** Beg chg *** 06/17/13 *************************************************
     C                   endif
     C*** End chg *** 06/17/13 *************************************************
     C*
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C                   if        rtmrel = '1'
     C                   z-add     rtefdt        kkefdt
     C                   else
     C                   z-add     cafdat        kkefdt
     C                   endif                                                  rtmrel = '1'
     C*
     C                   endif                                                  *in99 = *off
     C*
     C     asky1         setgt     pas01
     C     asky2         readpe    pas01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        asstat = 'I'
     C     asky2         readpe    pas01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   eval      @@alw1 = 0
     C*
     C                   if        *in99 = *off
     C*
     C*** Beg Del *** 07/27/11 ************************************************
     C***************
     C***************    select
     C***************
     C***************    when      aspi = 'A2' or aspi = 'G2' or aspi = 'H8'
     C***************              or aspi = 'J8' or aspi = 'K2' or aspi = 'P2'
     C***************              or aspi = 'R2' or aspi = 'Z2'
     C*** End Del *** 07/27/11 ************************************************
     C*
     C                   select
     C                   when      @@type = 'M'
     C*** Beg Add *** 04/08/11 ************************************************
     C                   movel     aspi          @@pi
     C     euky          chain     peu01
     C                   if        %found
     C                   eval      ##cbsa =  eucbsa
     C                   endif
     C*
     C     wgky          setgt     pwg01
     C     wgky1         readpe    pwg01                                  99
     C                   if        *in99 = *off
     C                   z-add     wgwind        ##wind            5 4
     C                   else
     C*
     C                   movel     xxst          ##key
     C*** BEG ADD *** 08/19/11 *********************************************
     C                   if        xxst = 'P1'
     C                   eval      ##key = 'PA'
     C                   endif
     C*** END ADD *** 08/19/11 *********************************************
     C     stky          chain     pst01                              99
     C                   if        *in99 = *off
     C                   movel     stname        ##nam
     C*
     C     wgky2         setgt     pwg02                              99
     C     wgky21        readpe    pwg02                                  99
     C  N99              z-add     wgwind        ##wind
     C                   endif
     C*
     C                   endif
     C*
     C*** Calculation for True medicare payment
     C*
     C*** If the date of service is prior to 1/1/2010 use the transitioned payment
     C*** allowance .Otherwise use the Fully Implemented allowance
     C*
     C                   if        cafdat < 1100101
     C                   eval      Wpmt = astpmt
     C                   else
     C                   eval      Wpmt = asalow
     C                   endif
     C*
     C                   eval      @@alw1=(Wpmt * 0.5 * ##wind) + (0.5 * Wpmt)
     C*
     C*** End Add *** 04/08/11 ************************************************
     C***
     C                   when      @@type = 'W'
     C*** Beg Chg *** 07/20/11 ************************************************
      ***                eval(r)   @@alw1 = astpmt
     C*** If the date of service is prior to 1/1/2010 use the transitioned payment
     C*** allowance .Otherwise use the Fully Implemented allowance
     C*
     C                   if        cafdat < 1100101
     C                   eval      @@alw1 = astpmt
     C                   else
     C                   eval      @@alw1 = asalow
     C                   endif
     C*
     C*** End Chg *** 07/20/11 ************************************************
     C                   movel     aspi          @@pi
     C                   endsl
     C*
     C*** Beg Del *** 07/27/11 ************************************************
     C**************
     C**************     when      aspi = 'P3' or aspi = 'Z3'
     C**************     call      'FA763R'
     C**************     parm                    @@rrn
     C**************     parm      'F'           ##call            1
     C**************     parm                    ##eob             3
     C**************     parm                    ##msg             7
     C**************     parm      ' '           ##modr            1
     C**************     parm      'M'           ##rnd             1
     C**************     parm                    ##alw1            9 2
     C**************     parm                    ##alw2            9 2
     C**************     parm                    ##alw3            9 2
     C**************     parm                    ##rtcd            1
     C**************
     C**************     movel     aspi          @@pi
     C**************     if        ##rtcd =  ' '
     C**************     eval      @@alw1 = ##alw1
     C**************     endif
     C*** End Del *** 07/27/11 ************************************************
     C*
     C*** Beg Del *** 07/27/11 ************************************************
     C***************    endsl
     C*** End Del *** 07/27/11 ************************************************
     C*
     C*** Beg Add *** 04/08/11 ************************************************
     C*** If call is in network mode
     C                   if        @@call = 'N'
     C                   exsr      PMod
     C                   exsr      PInd
     C                   endif
     C*** End Add *** 04/08/11 ************************************************
     C                   else
     C*
     C                   eval      @@pi = '00'
     C*
     C                   endif                                                  FMASP
     C*
     C                   endif                                                  FAEOP
     C*
     C                   endif                                                  FDCAP
     C*
     C                   return
     C***  Beg Add *** 04/08/11 ******************************************
    C********************************************************************
     C     pmod          begsr
    C*
     C                   clear                   mod
     C                   clear                   mod1
    C*
     C     duky          chain     pdu01                              99
     C                   if        *in99 = *off
     C                   movel     dupmod        mod(1)
     C                   movel     dumod2        mod(2)
     C                   movel     dumod3        mod(3)
     C                   movel     dumod4        mod(4)
     C                   endif
    C*
     C                   movea     mod           mod1
     C*
     C                   eval      camd50 = 1.50
     C                   eval      camd51 = 0.5
    C*
     C                   if        mod1 <> *blanks
    C* Adjust MRA depending on MOD
     C     '50'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      @@alw1 = @@alw1 * camd50
     C                   eval      @@eob = '383'
     C                   endif
    C*
     C     '51'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      @@alw1 = @@alw1 * camd51
     C                   eval      @@eob = '387'
     C                   endif
    C*
     C                   endif                                                  mod1 <> *blank
    C*
     C     pmode         endsr
    C*================================================================
     C*    PInd - EOB code based on Payment indicator
    C*================================================================
     C     PInd          begsr
    C*
     C                   select
     C*
     C                   when      aspi = 'N1'
     C                   eval      @@eob = '384'
     C
     C                   when      aspi = 'D5' or aspi = 'F4' or aspi = 'J7' or
     C                             aspi = 'K7' or aspi = 'L6' or aspi = 'L1' or
     C                             aspi = 'H2'
     C                   eval      @@eob = '385'
     C*
     C                   when      aspi = 'A2' or aspi = 'G2' or aspi = 'H8' or
     C                             aspi = 'J8' or aspi = 'K2' or aspi = 'P2' or
     C                             aspi = 'P3' or aspi = 'R2' or aspi = 'Z2' or
     C                             aspi = 'Z3'
     C*
     C     '50'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C     '51'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C                   eval      @@eob = '395'
     C                   endif
     C                   endif
    C*
     C                   endsl
    C*
     C                   endsr
     C***  End Add *** 04/08/11 ******************************************
