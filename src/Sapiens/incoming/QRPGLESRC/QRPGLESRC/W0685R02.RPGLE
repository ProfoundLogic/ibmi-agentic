     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *****************************************************************
      /title W0685R02 - Agent Commission History Maintenance
      *****************************************************************
      *
      * CALLED FROM: W0685R - Work with Agent Commission processing History
      * CALLED TO:   None
      *
      * created 12/21/06  C:183 Project 114WND
      *****************************************************************
      * Revised:
      *
      *
      *****************************************************************

     *
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0685F02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwmphp     uf a e             disk
     F                                     rename(pph01:dbrcdf)
    
      * employee
     Fwmphl     if   e           k disk

    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Agent Commission Processing'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------

     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@qte           s              1

     D/COPY SPRDATE
     D/COPY SPRSTRING
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     C/COPY SCOPYMT41C

     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
     C                   parm                    @@qte
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     *
     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr

     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr
     C*    @@rrn         chain     wmphp

     C*    @@RRN         CHAIN     WMPHP
     C                   IF        %found
     C                   eval      schdgrp = %editc(phfnd:'X')
     C                   select
     C                   when      phtype = 'E'
     C                   eval      schdprtp = 'Commission Earnings        '
     C                   when      phtype = 'R'
     C                   eval      schdprtp = 'Commission Checks Requested'
     C                   when      phtype = 'P'
     C                   eval      schdprtp = 'Commission Payments        '
     C                   when      phtype = 'A'
     C                   eval      schdprtp = 'Commission Adjustments     '
     C                   endsl
     C                   eval      schdrndt = %editc(dtmdyy(phdt):'Y')
     C                   eval      schdrntm = %editw(phtime:'  :  :  ')
     C                   eval      schduser = phuser
     C                   endif
     C                   if        %found(wmphp)
      *
     C                   select
     C                   when      phtype = 'E'
     C                   eval      sc1lbl = 'From date . . . . . . . :'
     C                   eval      sc2lbl = 'To date . . . . . . . . :'
      /Free
      /If Defined (WEB)
       sc1lbl = 'From date               :';
       sc2lbl = 'To date                 :';
      /Endif
      /End-Free
     C                   eval      sc3lbl = 'Commission Earning code :'
     C                   eval      sc1val = %editc(phfrdt:'Y')
     C                   eval      sc2val = %editc(phtodt:'Y')
     C                   eval      sc3val = phcocd
      *
     C                   when      phtype = 'R'
     C                   eval      sc1lbl = 'Commission ending date  :'
     C                   eval      sc2lbl = 'Commission payment code :'
     C                   eval      sc1val = %editc(phtodt:'Y')
     C                   eval      sc2val = phcocd
      *
     C                   when      phtype = 'P'
     C                   eval      sc1lbl = 'Policy year . . . . . . :'
     C                   eval      sc2lbl = 'Execution date  . . . . :'
     C                   eval      sc3lbl = 'Starting check# . . . . :'
      /Free
      /If Defined (WEB)
       sc1lbl = 'Policy year             :';
       sc2lbl = 'Execution date          :';
       sc3lbl = 'Starting check#         :';
      /Endif
      /End-Free
     C                   eval      sc1val = %editc(phfyr:'X')
     C                   eval      sc2val = %editc(dtmdyy(phidt):'Y')
     C                   eval      sc3val = %trim(%editc(PHCHK#:'Z'))
      *
     C                   when      phtype = 'A'
     C                   eval      sc1lbl = 'Policy year . . . . . . :'
     C                   eval      sc2lbl = 'Post date . . . . . . . :'
     C                   eval      sc3lbl = 'Agent#  . . . . . . . . :'
     C                   eval      sc4lbl = 'Agent name  . . . . . . :'
     C                   eval      sc5lbl = 'Employer# . . . . . . . :'
     C                   eval      sc6lbl = 'Employer name . . . . . :'
     C                   eval      sc7lbl = 'Division  . . . . . . . :'
     C                   eval      sc8lbl = 'Adjustment code . . . . :'
     C                   eval      sc9lbl = 'Adjustment type . . . . :'
     C                   eval      sc10lbl = 'Amount  . . . . . . . . :'
     C                   eval      sc11lbl ='Check#  . . . . . . . . :'
     C                   eval      sc12lbl ='Collected premium . . . :'
     C                   eval      sc13lbl ='Rate  . . . . . . . . . :'
     C                   eval      sc14lbl ='Producer  . . . . . . . :'
     C                   eval      sc15lbl ='Description . . . . . . :'
      /Free
      /If Defined (WEB)
       sc1lbl  = 'Policy year             :';
       sc2lbl  = 'Post date               :';
       sc3lbl  = 'Agent#                  :';
       sc4lbl  = 'Agent name              :';
       sc5lbl  = 'Employer#               :';
       sc6lbl  = 'Employer name           :';
       sc7lbl  = 'Division                :';
       sc8lbl  = 'Adjustment code         :';
       sc9lbl  = 'Adjustment type         :';
       sc10lbl = 'Amount                  :';
       sc11lbl = 'Check#                  :';
       sc12lbl = 'Collected premium       :';
       sc13lbl = 'Rate                    :';
       sc14lbl = 'Producer                :';
       sc15lbl = 'Description             :';
      /Endif
      /End-Free
     C                   eval      sc1val = %char(PHFYR)
     C                   eval      sc2val = %EDITC(DTMDYY(PHPODT):'Y')
     C                   eval      sc3val = %trim(%editc(PHAGT#:'Z'))
     C                   eval      sc4val = PHAGNM
     C                   eval      sc5val = %trim(%editc(PHEMP#:'Z'))
     C                   eval      sc6val = PHEMNM
     C                   eval      sc7val = %trim(%editc(PHDIV:'Z'))
     C                   eval      sc8val = PHCOCD
     C                   eval      sc9val = PHCOTP
     C                   eval      sc10val =  %trim(%editc(PHAMT:'J'))
     C                   eval      sc11val = %trim(%editc(PHCHK#:'Z'))
     C                   eval      sc12val = %trim(%editc(PHPREM:'J'))
     C                   eval      sc13val = %trim(%editc(PHRATE:'J'))
     C                   eval      sc14val = %trim(%editc(PHPROD:'Z'))
     C                   eval      sc15val = PHCODS
      *
     C                   endsl
     C                   endif

     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
     C                   eval      *in09 = *off
     C                   eval      *in40 = *off
     c                   endsr

     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C                   endsr

     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   endsr

