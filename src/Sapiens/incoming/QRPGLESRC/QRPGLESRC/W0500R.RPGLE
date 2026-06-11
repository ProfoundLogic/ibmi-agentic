     Hcopyright('Fiserv, Inc., 2003-2008')                                                         *
     H dftactgrp(*no)
     H/TITLE W0500R - Work with return premium batches.
      **********************************************************************************************
      *  CALLS TO: W0500R02 (Premium Batch Information), W0501R (Batcjh Detail)                    *
      *            W0500C18 (Edit Report), W0500C25 (Generate Check Requests)                      *
      *                                                                                            *
      *  CREATED: 03/12/2003  C:142/C:136                                                          *
      **********************************************************************************************
      *  REVISIONS:                                                                                *
      *                                                                        *
      *  04/24/24 C:300 Project: WC2676                                        *
      *                 This program is no longer in use.                      *
      *                 Pass-through this program to new screen.               *
      *                 Add extra entry parm to avoid Pass-through of old screen
      *                                                                        *
      *  11/26/08 C:220 Project: 81WZY                                         *
      *                 Return Funds - Batch Edit Sort User Defined            *
      *                 Controls.                                              *
      *                                                                                            *
      *  06/19/08 C:136 Support: WA081 63023                                                       *
      *                 Release lock on PREMCHK if a lock doesn't exist. The lock is going to be   *
      *                 moved 'downstream' to the processes that actually need the lock.           *
      *                                                                                            *
      *  12/26/07 C:183  Project: 81WZB                                                            *
      *                  Added the extra column "Surplus Dividend" for                             *
      *                  client 81.                                                                *
      *                                                                                            *
      *  07/18/07 C:136  Project: WC1491 (Independent Cash Distribution)                           *
      *                 -Don't populate dates if no date is available                              *
      *                                                                                            *
      *                                                                                            *
      *  07/14/05 C:136  Support: WA081 48064                                                      *
      *                  Corrected verbiage on window message to indicate Fiserv ISD instead of    *
      *                  PDS.                                                                      *
      *                                                                                            *
      *  11/05/04  C:33  Project: WC1033                                                           *
      *                  Include calculations for Agency Billing.                                  *
     **********************************************************************************************
     FW0500F    CF   E             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     Sfile(scr01s:sfrrn1)
     F                                     Sfile(scr02s:sfrrn2)
     F                                     Infds(fids01)
     *  Physical file definition (for refreshing subfile record)
     Fwxrnp     if   e             disk
     F                                     Rename(Prn01:Pfrcdf)
     *  Primary logical file definition (for displaying items)
     Fwxrnl     if   e           k disk
     F                                     Rename(Prn01:Dbrcdf)
     F                                     Infds(Dbfds)
     F***  Begin Add  ***  11/05/04  **************************************
     Fwxrtl     if   e           k disk    rename(prt01:prt00)
     F***  Begin Add  ***  11/05/04  **************************************
     Fwmupl     if   e           k disk
     Fswcpf     if   e           k disk
     D hd              S             78    dim(1) ctdata perrcd(1)
     D***  Begin Chg  ***  11/05/04  **************************************
     D*of*******       S              1    dim(12) ctdata perrcd(1)
     D*ofd******       S             35    dim(12) alt(of)
     D #OptFunc        c                   15
     D of              S              1    dim(#OptFunc) ctdata perrcd(1)
     D ofd             S             35    dim(#OptFunc) alt(of)
     D***  End   Chg  ***  11/05/04  **************************************
     *================================================================
     *  /COPY required constants
     *================================================================
     D Msg18           C                   'Batch Edit Report'
     D Msg25           C                   'Generate Check Request'
     D optnm           C                   'Return Fund Batches'
     D slmsgi          C                   'WC39998'
     D slmsgf          C                   'WMESGG'
     D acdpgm          C                   'W0500R02'
     D comapi          C                   'WA001R'
     D uspapi          C                   'W0006R'
     D infomx          C                   1
     D viewmx          C                   1
     D sf1pag          C                   12
     D Datusa          S               d   Datfmt(*Usa)
     D dlya            ds            80
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D***  Begin Add  ***  11/05/04  **************************************
     D ##call          s                   like(@@call) inz
     D***  End   Add  ***  11/05/04  **************************************
     D xxdt6           s              6  0
     D kkco#           s                   like(rnco#)
     D kkbat#          s                   like(rnbat#)
      *** beg add ***** 07/18/07 ******************************************
     D xxmamt          s             14
      *** end add ***** 07/18/07 ******************************************
     D
     D view1           ds            74
     D   v1bat#                1      9
     D   v1fgrp               15     17
     D   v1dash               19     19    inz('-')
     D   v1tgrp               21     23
     D   v1frdt               28     35
     D   v1todt               38     45
      *** beg chg ***** 07/18/07 ******************************************
     D***v1mamt********       49     62
     D   v1mamt               49     58
      *** end chg ***** 07/18/07 ******************************************
     D***  Begin Add  ***  11/05/04  **************************************
      *** beg chg ***** 07/18/07 ******************************************
     D***v1acfl********       70     70
     d   v1mtyp               62     62
     d   v1btyp               68     68
     d   v1desc               15     45
      *** end chg ***** 07/18/07 ******************************************
     D*** BEG ADD ***** 12/26/07 ******************************************
     D   V1SDFL               73     73
     D*** END ADD ***** 12/26/07 ******************************************
     D*
     DAgencyBill       pr             1
     D co#                                 like(RTCO#)
     D bat#                                like(RTBAT#)
     D***  Begin Add  ***  11/05/04  **************************************

     C/COPY SCOPYWW01C

     C     *dtaara       define    premchk       dlya
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm                    @@call
      **** Begin Add **** 04/24/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************

     *
     *  Add/change/display program parameter list
     *
     C     acdpl         plist
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn             9 0
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##outq
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *================================================================
     *  KLIST definitions
     *================================================================
     C     key1          klist
     C                   kfld                    kkco#

     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#

     C     upky          klist
     C                   kfld                    q1user
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
      *
     C                   if              (scsel =  '4'  or
     C                                    scsel =  '8'  or
     C***  Begin Add  ***  11/05/04  **************************************
     C                                    scsel =  '9'  or
     C***  End   Add  ***  11/05/04  **************************************
     C                                    scsel = '18'  or
     C                                    scsel = '25') and
     C                                    rnbpf =  'N'
     c                   seton                                        71  90
     C                   endif

     C  n90              if        scsel =  '25'
     C     *lock         in        dlya                                 72
     C                   if        *in72
     C                   seton                                        90
      *** beg add ***** 06/19/08 ******************************************
     c                   else
     c                   out       dlya
      *** end add ***** 06/19/08 ******************************************
     C                   endif
     C                   endif

     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for the subset screen
     *================================================================
     C     errck2        begsr
     C***  BEG ADD ******* 12/26/07 ***************************************
     C                   IF        SSMTYP <> ' ' AND SSMTYP <> 'A'
     C                             AND SSMTYP <> 'M'
     C                   SETON                                        36  90
     C                   ENDIF                                                  SSMTYP <> ' '
      *
     C                   IF        SSBTYP <> ' ' AND SSBTYP <> 'A'
     C                             AND SSBTYP <> 'D' AND SSBTYP <> 'I'
     C                             AND SSBTYP <> 'P'
     C                   SETON                                        37  90
     C                   ENDIF                                                  SSBTYP <> ' '
      *
     C                   IF        SSSDFL <> 'Y' AND SSSDFL <> 'N' AND
     C                             SSSDFL <> ' ' AND CLTID = '081'
     C                   SETON                                        38  90
     C                   ENDIF                                                  SSSDFL <> 'Y'
     C***  END ADD ******* 12/26/07 ***************************************
     C
     C                   endsr
     *================================================================
     * INZSST - Initialize the subset fields
     *================================================================
     C     inzsst        begsr
     C                   endsr
     *================================================================
     * KEYINZ - Initialize key values for program start
     *================================================================
     C     keyinz        begsr
     c                   eval      kkco# = ##co#
     c                   eval      kkbat# = 1
     C                   endsr
     *================================================================
     * KEYSET - Set key values when doing "position to"
     *================================================================
     C     keyset        begsr
     C                   move      scpst1        kkbat#
     C                   endsr
     *================================================================
     * KEYSFL - Set key values from subfile records for paging
     *================================================================
     C     keysfl        begsr
     C                   eval      kkbat# = scbat#
     C                   endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr
      *
      *** beg add ***** 07/18/07 **************************************
     c                   clear                   view1
      *** end add ***** 07/18/07 **************************************
     C                   eval      scbat# = RNBAT#
     C                   eval      v1bat# = %editc(RNBAT#:'Z')
     C                   eval      v1fgrp = %editc(RNFGRP:'Z')
     C                   eval      v1tgrp = %editc(RNTGRP:'Z')
      *
      *** beg add ***** 07/18/07 **************************************
     c                   if        RNFAUD <> 0
      *** end add ***** 07/18/07 **************************************
     C                   z-add     RNFAUD        srdti
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         xxdt6
      *
     C     *MDY          move      xxdt6         datusa
     C     *MDY          move      datusa        v1frdt
      *** beg add ***** 07/18/07 **************************************
     c                   endif
      *** end add ***** 07/18/07 **************************************
      *
      *** beg add ***** 07/18/07 **************************************
     c                   if        RNTAUD <> 0
      *** end add ***** 07/18/07 **************************************
     C                   z-add     RNTAUD        srdti
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         xxdt6
     C     *MDY          move      xxdt6         datusa
     C     *MDY          move      datusa        v1todt
      *** beg add ***** 07/18/07 **************************************
     c                   endif

     c                   if               RNMTYP ='M'
     c                             and v1desc = *blanks

     c                   if        RNBTYP = 'I'
     c                   eval      v1desc = 'Independent Cash Distribution'
     c                   else
     c                   eval      v1desc = 'Manual Batch'
     c                   endif

     c                   else
     c                   eval      v1dash = '-'
     c                   endif
      *** end add ***** 07/18/07 **************************************

      *** beg chg ***** 07/18/07 ******************************************
     C***********        eval      v1mamt = %editc(RNMIN:'4')
     C                   eval      xxmamt = %editc(RNMIN:'4')
     c                   move      xxmamt        v1mamt
      *** end chg ***** 07/18/07 ******************************************
     C***  Begin Add  ***  11/05/04  **************************************
     C*** beg del ***** 07/18/07 ******************************************
     C*****************  eval      v1acfl = AgencyBill(RNCO#:RNBAT#)
     C*** end del ***** 07/18/07 ******************************************
     C***  End   Add  ***  11/05/04  **************************************
     C*** beg add ***** 07/18/07 ******************************************
     c                   eval      v1mtyp = RNMTYP
     c                   eval      v1btyp = RNBTYP
     C*** end add ***** 07/18/07 ******************************************
     C*** BEG ADD ***** 12/26/07 ******************************************
      *
     C                   IF        CLTID = '081'
     C                   EVAL      V1SDFL = RNSDFL
     C                   ENDIF                                                  CLTID = '081'
      *
     C*** END ADD ***** 12/26/07 ******************************************
     C                   eval      scbpf  = RNBPF
      *
     C                   movel     view1         scdata
      *
     C                   endsr
     *================================================================
     * OTHFKS - Process other function keys
     *================================================================
     C     othfks        begsr
     C                   endsr
     *================================================================
     * OTHOPT - Process other options
     *================================================================
     C     othopt        begsr
      *
     C     scsel         caseq     '8'           opt08
     C***  Begin Add  ***  11/05/04  **************************************
     C     scsel         caseq     '9'           opt09
     C***  End   Add  ***  11/05/04  **************************************
     C     scsel         caseq     '18'          opt18
     C     scsel         caseq     '25'          opt25
     C                   endcs
      *
     C                   endsr
     *================================================================
     * Option 08 processing
     *================================================================
     C     opt08         begsr
      *
     C                   call      'W0501R'
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         parm                    @@kc              1
     C**********         parm                    @@kl              1
     C**********         parm                    @@call
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C                   parm                    ##call
     C***  End   Chg  ***  11/05/04  **************************************
     C                   parm      scdbrr        @@rrn             9 0
     C***  Begin Add  ***  11/05/04  **************************************
     C*** beg del ***** 07/18/07 *** **************************************
     C*****************  parm      'E'           @@mode            1
     C*** end del ***** 07/18/07 *** **************************************
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   endsr
     C***  Begin Add  ***  11/05/04  **************************************
     *================================================================
     * Option 09 processing
     *================================================================
     C     opt09         begsr
      *
     C                   call      'W0501R'
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    @@kl              1
     C                   parm                    ##call
     C                   parm      scdbrr        @@rrn             9 0
     C                   parm      'A'           @@mode            1
      *
     C                   endsr
     C***  End   Add  ***  11/05/04  **************************************
     *================================================================
     * Option 18 processing
     *================================================================
     C     opt18         begsr
      *
     C                   move      scbat#        ##bat#
     C                   move      kkco#         #1co#
     C     upky          chain     pup01
      *
     C***  Beg   Add  ***  11/26/08  **************************************
     C                   move(p)   msg18         wd4msg
     C                   exfmt     wdw04
     C***  End   Add  ***  11/26/08  **************************************
      *
     C                   call      'W0500C18'
     C                   parm                    #1co#             3
     C                   parm                    ##bat#            9
     C                   parm      upoutq        ##outq
      *
     C***  Beg   Del  ***  11/26/08  **************************************
     C**************     move(p)   msg18         wd4msg
     C**************     exfmt     wdw04
     C***  End   Del  ***  11/26/08  **************************************
      *
     C                   endsr
     *================================================================
     * Option 25 processing
     *================================================================
     C     opt25         begsr
      *
      * change value of RNBPF to 'N'
      *
     C                   eval      ##mode = 'N'
     C                   call      acdpgm        acdpl
      *
      * generate check request records
      *
     C                   move      RNBAT#        ##bat#
     C                   move      ##co#         #1co#             3
     C***  Beg   Add  ***  11/26/08  **************************************
     C                   move(p)   msg25         wd4msg
     C                   exfmt     wdw04
     C***  End   Add  ***  11/26/08  **************************************
     C                   call      'W0500C25'
     C                   parm                    #1co#
     C                   parm                    ##bat#
      *
     C***  Beg   Del  ***  11/26/08  **************************************
     C*********          move(p)   msg25         wd4msg
     C*********          exfmt     wdw04
     C***  End   Del  ***  11/26/08  **************************************
      *
     C                   endsr
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if 2nd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 2
     c                   Call      'WTRFBTCH'
     c                   parm                    @@rrn2            9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   if        %parms >= 1
     C                   eval      ##call = @@call
     C                   endif
     C***  End   Add  ***  11/05/04  **************************************
      *
      * Display window if return premium module not installed
      *
     C                   read      swcpf
     C                   if        %eof or WCFL25 <> 'Y'
     C                   exfmt     rpinstwdw
     C                   seton                                        lr
     C                   return
     C                   endif
     C*** BEG ADD ***** 12/26/07 **************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   IF        CLTID = '081'
     C                   EVAL      *IN31 = *ON
     C                   EVAL      HD(1) = %trimr(HD(1)) + '  Div'
     C                   ENDIF                                                  CLTID = '081'
      *
     C*** END ADD ***** 12/26/07 **************************************

     C                   endsr
     *================================================================
     * READ   - Read next record from file
     *================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key1          reade(N)  dbrcdf                                 95
     C                   endif
     C                   endsr
     *================================================================
     * READP  - Read previous record from file
     *================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key1          readpe(N) dbrcdf                                 95
     C                   endif
     C                   endsr
     *================================================================
     * SETGT  - Set greater than on file
     *================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key2          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
     *================================================================
     * SETLL  - Set lower limits on file
     *================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key2          setll     dbrcdf                             96
     C                   endif
     C                   endsr
     *================================================================
     * SF1UPD - Update subfile record when item is deleted
     *================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '                     ***Deleted***'
     C                   endsr
     *================================================================
     * SLCTRD - Move fields for selected record to parameters
     *================================================================
     C     slctrd        begsr
     C                   endsr
     *================================================================
     * SUBSET - Determine if record should be put in subfile
     *================================================================
     C     subset        begsr

      *** beg add ***** 07/18/07 **************************************
     c                   if        ssmtyp <> *blank and
     c                             RNMTYP <> ssmtyp
     c                   eval      meetss = *off
     c                   endif
     c                   if        ssbtyp <> *blank and
     c                             RNBTYP <> ssbtyp
     c                   eval      meetss = *off
     c                   endif
      *** end add ***** 07/18/07 **************************************
     C*** BEG ADD ***** 12/26/07 **************************************
     C                   IF        CLTID  = '081'
      *
     C                   IF        RNSDFL = *BLANK
     C                   MOVE      'N'           @RNSDFL           1
     C                   ELSE
     C                   MOVE      RNSDFL        @RNSDFL           1
     C                   ENDIF                                                  RNSDFL = *BLANK
      *
     C                   IF        (SSSDFL <> *BLANK AND
     C                             SSSDFL <> @RNSDFL)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSSDFL <> *BLANK
      *
     C                   ENDIF                                                  CLTID = '081'
      *
     C*** END ADD ***** 12/26/07 **************************************

     C                   endsr
     *================================================================
     P***  Begin Add  ***  11/05/04  **************************************
    P*====================================================================
    P* AgencyBill - Determine if any agency bill was processed in batch
    P*====================================================================
     PAgencyBill       b
    P*
     DAgencyBill       pi             1
     D co#                                 like(RTCO#)
     D bat#                                like(RTBAT#)

     C     keyWXRTL      klist
     C                   kfld                    co#
     C                   kfld                    bat#

     C                   clear                   RTACFL

     C     keyWXRTL      setll     wxrtl

     C                   dou       %eof
     C     keyWXRTL      reade     wxrtl
     C                   if        RTACFL = 'Y'
     C                   return    'Y'
     C                   endif
     C                   enddo

     C                   return    ' '

     PAgencyBill       e
     P***  End   Add  ***  11/05/04  **************************************





** Column headings
   Batch#     Grp Range    Date      Date        Min Amt    Typ   Typ
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1088=Batch detail           YNY
11818=Batch edit report     YNY
12525=Generate check requestYNY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Build return batch    YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
