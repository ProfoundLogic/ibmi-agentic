     H* Copyright('(c) 2022 Sapiens. All Rights Reserved.')
     H*
     H/TITLE W7352R - WCSTAT Submit Data                                                   *
     h option(*nodebugio: *srcstmt) bnddir('STBNDDIR')
     h debug(*yes)
     H**************************************************************************************
     H*  Description:  Though user interaction, a file to tape transfer is set up and      *
     H*                performed for submission to state reporting agency.                 *
     H*                                                                                    *
     H*  Called from: W7350R                                                               *
     H*  Calls to: W7352CA, W7352C                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 07/24/98    Programmer C:87    Project:WC591                             *
     H**************************************************************************************
     H* Revised:                                                                           *
     H*                                                                                    *
     H* 03/11/22 C:340 Project: WC2843                                                     *
     H*                Start using new Logical - WMTML1                                    *
     H*                Use Carrier specific Data provider and FEIN                         *
     H*                                                                                    *
     H* 12/03/15 C:65  Project: 45WKP                                                      *
     H*                Add checkbox to allow selection of pre-2016 data for LCTA which     *
     H*                uses old Carrier #.                                                 *
     H*                                                                                    *
     H*  05/11/09  C:61  Project: 93WHW                                                    *
     H*                  Added experience rating selection for clt093
     H*                                                                                    *
     H*                                                                                    *
     H*  04/07/09  C:65  Project: 139WKB                                                   *
     H*                  Remove the group/report date fields from tape screen as the       *
     H*                  process is now run for multiple records.  Change the WMSSP        *
     H*                  file update to read over the work file WXWSP to update all        *
     H*                  selected extract records. The RRN parameter now holds the time.   *
      *
      * 07/05/07  C:61  support: 58995 WC1272
      *                 Initialize zip code fields.
      *
      * 06/11/07  C:61  Project:
      *                 Default SCR02 values.
      *                 Correct program title on SCR02.
     H*                                                                                    *
     H*  02/01/05  C:65  Project: WC1272                                                   *
     H*                  Add address information for the contact screen.    record.        *
     H*                                                                                    *
     H*  01/18/05  C:65  Project: WC1253                                                   *
     H*                  Add screen for contact information for transmittal record.        *
     H*                                                                                    *
     H*  05/07/03  C:15  Project: 114WBJ                                                   *
     H*                  Added code to allow the user to select a specific state for a     *
     H*                  submission.                                                       *
     H*                                                                                    *
     H*  09/24/99  C:87  Project: supp WA033 #19343                                        *
     H*                  When passing parms longer than 30 characters to a CL with a       *
     H*                  submit job, need to insure the last char of the parm is not blank *
     H*                                                                                    *
     H**************************************************************************************
     H*  Indicators:                                                                       *
     H*                                                                                    *
     H*  60 - General chain                                                                *
     H*  90 - Error on screen input                                                        *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7352F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F***  Begin chg  ***  04/07/09  **************************************
     F*****WMSSP     UF   E             DISK
     FWMSSL     UF   E           K DISK
     F***  End chg ******  04/07/09  **************************************
     F*
     FWMFNL     IF   E           K DISK
     F*
     FSSYLF     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*** Begin add *** 05/07/03 ***********************************************************
     FSTSTL3    IF   E           K DISK
     F*** End add ***** 05/07/03 ***********************************************************
      ******* beg add ****** 06/11/07 *********************************

      * WCSTAT Transmittal Information Default
     F*** Beg   Add *** 03/11/22 ***********************************************************
     F****wmtml     if   e           k disk
     FWMTML1    IF   E           K Disk
     FWDF2L     IF   E           K disk
     F*** End   Add *** 03/11/22 ***********************************************************
      ******* end add ****** 06/11/07 *********************************
     F***  Begin add  ***  04/07/09  **************************************
     FWXWSL1    IF   E           K DISK
     F***  End add ******  04/07/09  **************************************

     D*  Density table
     D dens_tbl        S             10A   dim(23) Ctdata PerRcd(2)

     D*  Date/time data structure
     D                 DS
     D dsdttm                        12  0
     D  dstime                        6  0 Overlay(dsdttm)
     D  dsdate                        6  0 Overlay(dsdttm:7)

     D***  begin add  ***  09/24/99  ***************************************
     D                 DS
     D dummy                         40
     D  dum40                         1    Overlay(dummy:40)
     D***  end   add  ***  09/24/99  ***************************************
     d*** begin add ***  01/18/05  *****************************************
     D                 DS
     D xxphn                         10  0
     D  scphna                        3  0 Overlay(xxphn)
     D  scphn1                        3  0 Overlay(xxphn:4)
     D  scphn2                        4  0 Overlay(xxphn:7)

     D                 DS
     D xxfax                         10  0
     D  scfaxa                        3  0 Overlay(xxfax)
     D  scfax1                        3  0 Overlay(xxfax:4)
     D  scfax2                        4  0 Overlay(xxfax:7)

     d*** end add *****  01/18/05  *****************************************
      ******* beg add ****** 06/11/07 *********************************
     d                 ds
     d sczip                          9  0
     d  sczip1                        5  0 overlay(sczip:1)
     d  sczip2                        4  0 overlay(sczip:6)
      ******* end add ****** 06/11/07 *********************************
     D*** BEGIN ADD ***  02/01/05  ************************************
     D                 DS
     D xxadr                        101
     D  xxstrt                       60    Overlay(xxadr)
     D  xxcity                       30    Overlay(xxadr:61)
     D  xxst                          2    Overlay(xxadr:91)
     D  xxzip1                        5    Overlay(xxadr:93)
     D  xxzip2                        4    Overlay(xxadr:98)
     D*** END ADD *****  02/01/05  ************************************

     D*  Key list field definitions
     D kkco#           S                   Like(COCO#)
     D kkfnd           S                   Like(SSFND)
     D*** Begin add *** 05/07/03 ***********************************************************
     D kkkey           S                   Like(STKEY)
     D qqstcd          S                   Like(STNST)
     D*** End add ***** 05/07/03 ***********************************************************
     D***  Begin add  ***  04/07/09  **************************************
     D kktime          S                   Like(WSTIME)
     D kkuser          S                   Like(WSUSER)
     D***  End add ******  04/07/09  **************************************

     D qqdev           S             10
     D qqden           S             10
     D qqerr           S              1
     D qqtype          S              7
     D qqrrn           S              9  0

     DPSDS           ESDS                  Extname(SZQ1P)
     d/copy sprUsrPrms
     C*---------------
     C*  Key lists
     C*---------------
      ******* beg add ****** 06/11/07 *********************************

      * WCSTAT Transmittal Information Default

     C     tmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkflag            1

      ******* end add ****** 06/11/07 *********************************
     C*    Company file
     C     coky          klist
     C                   kfld                    kkco#
     C*
     C*    Fund master file
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C*** Begin add *** 05/07/03 ***********************************************************
     C*  State code table file
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*** End add ***** 05/07/03 ***********************************************************
     C***  Begin add  ***  04/07/09  **************************************
     C     wsky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkuser
     C                   kfld                    kktime
     C*
     C     ssky          klist
     C                   kfld                    kkco#
     C                   kfld                    wsfnd
     C                   kfld                    wsrpmy
     C*
     C***  End add ******  04/07/09  **************************************

     C*
     C     *entry        plist
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@mode            1
     C                   parm                    @@rrn             9 0
     C                   parm                    @@que            10
     C*
     C*  Transfer to tape program parameter list
     C     tttpl         plist
     C                   parm                    qqrrn
     C                   parm                    qqdev
     C                   parm                    qqden
     C                   parm                    qqrnam           40
     C                   parm                    qqadd1           40
     C                   parm                    qqadd2           40
     C                   parm                    qqmode            1
     C                   parm                    qqque            10
     C                   parm                    qqxtap            1
     C*** Begin add *** 05/07/03 ***********************************************************
     C                   parm                    qqstcd
     C*** End add ***** 05/07/03 ***********************************************************
     C*** begin add ***  01/18/05  ****************************************
     C                   parm                    qqmail           40
     C                   parm                    qqcnam           25
     C                   parm                    qqphn            10
     C                   parm                    qqext             6
     C                   parm                    qqfax            10
     C*** end add *****  01/18/05  ****************************************
     C*** BEGIN ADD ***  02/01/05  ************************************
     C                   parm                    qqadr           101
     C*** END ADD *****  02/01/05  ************************************
     C*** BEG ADD *** 05/11/09 ***************************************
     C                   parm                    qqer              1
     C*** END ADD *** 05/11/09 ***************************************
      *** begin add ***  12/03/15  *************************************************
     C                   PARM                    QQOldCar          1
      *** end add *****  12/03/15  *************************************************
      *** begin add ***  03/11/22  *************************************************
     C                   PARM                    qqDtaPvdr         1
     C                   PARM                    qqFein            9
      *** End   add ***  03/11/22  *************************************************

     C*  Date conversion parms
     C*
     C     datchk        plist
     C                   parm                    srdat_in          8 0
     C                   parm      *blanks       srfr_fmt          8
     C                   parm      *blanks       srto_fmt          8
     C                   parm                    srdat_out        10
     C                   parm                    srerror           1
     C*
     C     dtmdyy        plist
     C                   parm                    srdat_in
     C                   parm      '*CYMD'       srfr_fmt
     C                   parm      *blanks       srto_fmt
     C                   parm                    srdat_out
     C                   parm                    srerror
     C*
     C     dtmdy         plist
     C                   parm                    srdat_in
     C                   parm      '*CYMD'       srfr_fmt
     C                   parm      '*MDY'        srto_fmt
     C                   parm                    srdat_out
     C                   parm                    srerror

     C*** BEG ADD *** 05/11/09 ***************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3
      *
     C     cltid         comp      '093'                                  93
     C*** END ADD *** 05/11/09 ***************************************
      *** begin add ***  12/03/15  *************************************************
     C                   Eval      clt045 = *off
     C
     C                   If        cltID = '045'
     C                   Eval      clt045 = *on
     C                   Endif
      *** end add *****  12/03/15  *************************************************
     C*
     C                   time                    dsdttm
      ******* beg add ****** 06/11/07 *********************************
     C                   move      'Y'           default02         1
      ******* end add ****** 06/11/07 *********************************
      ******** beg add ******* 07/05/07 ****************************************************
     c                   z-add     0             sczip1
     c                   z-add     0             sczip2
      ******** end add ******* 07/05/07 ****************************************************
     C*
     C*  Chain to data extraction file and assign values to screen
     C***  begin chg ****  04/07/09  **************************************
     C*****@@rrn         chain(n)  PSS01                              60
     C**********         eval       SCFND = SSFND
     C**********         z-add     SSRPMY        srdat_in
     C**********         call      'SRCVTMY'     dtmdyy
     C**********         movel     srdat_out     SCRPMY
     c*
     c                   z-add     @@rrn         kktime
     C                   eval      kkuser = q1user
     c                   z-add     0             kkco#
     c*
     c                   read      pws01                                  60
     c     *in60         doweq     *off
     c                   if        wsuser = kkuser and wstime = kktime
     C***  Beg chg ******  03/11/22  **************************************
     c***                z-add     wsco#         kkco#
     C***  End chg ******  03/11/22  **************************************
     c                   leave
     c                   endif
     c                   read      pws01                                  60
     c                   enddo
     c*
      *** Begin add ***  03/11/22  *************************************************
          kkCo# = GetUserInfo(q1user);
      *** End   add ***  03/11/22  *************************************************
     C***  end chg ******  04/07/09  **************************************
     C*
     C*  Get default value for tape device from SMNU/SDPRF (system control file)
     C     'SC'          chain     SDR01                              60
     C                   if        *in60 = *off
     C                   movel     DRTDNM        SCDEV
     C                   else
     C                   eval       SCDEV = ' '
     C                   endif
     C*
     C                   eval      SCDENS = '*CTGTYPE'
     C*
     C*  Get company name
     C***  begin del ****  04/07/09  **************************************
     C**********         eval       kkco# = SSCO#
     C***  end del ******  04/07/09  **************************************
     C*
     C     coky          chain     PCO01                              60
     C                   if        *in60 = *off
     C                   eval      srname = %TRIML(CONAME)
     C                   call      'SRCENTER'
     C                   parm                    srname           80
     C                   parm      40            srlen             3 0
     C*
     C                   movel     srname        SCCONM
     C*
     C                   else
     C*
     C                   eval       SCCONM = ' '
     C                   endif                                                  *in60=*off
     C*
     C*  Get fund name
     C***  begin del ****  04/07/09  **************************************
     C**********         eval       kkfnd = SSFND
     C*
     C*****fnky*         chain     PFN01                              60
     C**********         if        *in60 = *off
     C**********         movel(p)  FNNAME        SCFNDN
     C**********         else
     C**********         eval       SCFNDN = ' '
     C**********         end
     C***  end del ******  04/07/09  **************************************
     C*
     C*  Load user name
     C                   eval       SCUSER = q1user
     C*
     C*  Load default name and address of rating organization
     C                   movel(p)  'NCCI'        SCRNAM
     C                   eval      SCADD1 = '777 Yamato Road, Suite 200/400'
     C                   eval      SCADD2 = 'Boca Raton, FL 33431          '
     C*
     C*  Display 'Transfer to tape' prompt
     C*** Begin add *** 05/07/03 ***********************************************************
     C                   clear                   scst
     C*** End add ***** 05/07/03 ***********************************************************
     C                   eval      scxtap = 'N'
     C                   dou       *in90 = *off
     C*
     C                   setoff                                           90
     C                   exfmt     WDW02
     C*
     C*** Begin add *** 05/07/03 ***********************************************************
     C                   if        *INKL = *on
     C                   return
     C                   endif
     C*** End add ***** 05/07/03 ***********************************************************
     C*
     C                   if        (SCXTAP <> 'Y') AND (SCXTAP <> 'N')
     C                   seton                                        89  90
     C                   endif
     C*
     C*** Begin add *** 05/07/03 ***********************************************************
     C                   clear                   qqstcd
     C                   if        SCST <> '  '
     C*
     C                   movel(p)  SCST          kkkey
     C     stky          chain     PST01                              88
     C*
     C  n88              move      stnst         qqstcd
     C   88              seton                                            90
     C*
     C                   endif
     C*** End add ***** 05/07/03 ***********************************************************
     C*** BEG ADD *** 05/11/09 ***************************************
     C  n90              if        (SCER<>' ') AND (SCER<>'1') AND (SCER<>'3')
     C                   seton                                        75  90
     C                   endif
     C*** END ADD *** 05/11/09 ***************************************
     C*
     C                   enddo                                                  *in90=*off
     C*

     C*--------------------
     C*  Main processing
     C*--------------------
     C                   if        SCXTAP = 'Y'
     C                   dou       *in90 = *off
     C*
     C                   setoff                                       90
     C*  Output screen 1
     C                   exfmt     SCR01
     C*
     C*  Process command keys
     C                   exsr      Func_keys
     C*
     C*  Validate data entered
     C                   exsr      Err_chk
     C*
     C                   enddo                                                  *in90=*off
     C                   endif                                                  SCXTAP='Y'
     C*** begin add ***  01/18/05  ****************************************
     c                   z-add     0             xxphn
     c                   z-add     0             xxfax
     C                   if        SCST <> '  '
     C*
     C                   dou       *in90 = *off
     C*
     C                   setoff                                           90
      ******* beg add ****** 06/11/07 *********************************

     c                   if        default02 = 'Y'
     c                   exsr      srdef02
     c                   endif

      ******* end add ****** 06/11/07 *********************************
     C                   exfmt     SCR02
     C*
     C*  Process command keys
     C                   exsr      Func_keys
     C*
     C  N90              if        (SCMAIL = *blanks)
     C                   seton                                        33  90
     C                   endif
     C*
     C  N90              if        (sccnam = *blanks)
     C                   seton                                        34  90
     C                   endif
     C*
     C  N90              if        (scphna = *zero)
     C                   seton                                        35  90
     C                   endif
     C*
     C  N90              if        (scphn1 = *zero)
     C                   seton                                        36  90
     C                   endif
     C*
     C  N90              if        (scphn2 = *zero)
     C                   seton                                        37  90
     C                   endif
     C*
     C  N90              if        (scext < *zero)
     C                   seton                                        38  90
     C                   endif
     C*
     C  N90              if        (scfaxa = *zero)
     C                   seton                                        39  90
     C                   endif
     C*
     C  N90              if        (scfax1 = *zero)
     C                   seton                                        40  90
     C                   endif
     C*
     C  N90              if        (scfax2 = *zero)
     C                   seton                                        41  90
     C                   endif
     c*** begin add ***  02/01/05  ************************************
     C*
     C* STREET NAME CANNOT BE BLANK.
     C*
     C*
     C  N90              if        (scstrt = *blanks)
     C                   seton                                        42  90
     C                   endif
     C*
     C* CITY NAME CANNOT BE BLANK.
     C*
     C  N90              if        (sccity = *blanks)
     C                   seton                                        43  90
     C                   endif
     C*
     C* STATE MUST BE VALID.
     C*
     C                   movel(p)  SCST2         kkkey
     C  N90stky          chain     PST01                              44
     C   44              seton                                            90
     C*
     C* ZIP CODE 1 CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C  N90              if        (sczip1 <= *zero)
     C                   seton                                        45  90
     C                   endif
     C*
     C* ZIP CODE 2 CANNOT BE LESS THAN ZERO.
     C*
     C  N90              if        (sczip2 < *zero)
     C                   seton                                        46  90
     C                   endif
     D*** end add *****  02/01/05  ************************************
     C*
     C*** Beg   Add *** 03/11/22 ***********************************************************
      // If Data Provider is Third Party, then FEIN is required

         If *In90 = *Off and scDtaPvdr = 'T' and scTFien = 0;
            *In90 = *On;
            *In47 = *On;
         EndIf;

     C*** End   Add *** 03/11/22 ***********************************************************
     C                   enddo                                                  *in90=*off
     C*
     C                   endif
     C*
     C                   move      *blanks       qqmail
     C                   move      *blanks       qqcnam
     C                   move      *blanks       qqphn
     C                   move      *blanks       qqext
     C                   move      *blanks       qqfax
     C*
     c                   movel     scmail        qqmail
     c                   movel     sccnam        qqcnam
     c                   move      xxphn         qqphn
     c                   move      scext         qqext
     c                   move      xxfax         qqfax
     C*** BEGIN ADD ***  02/01/05  ************************************
     c                   move      *blanks       qqadr
     c                   move      *blanks       xxadr
     C                   MOVE      SCSTRT        XXSTRT
     C                   MOVE      SCCITY        XXCITY
     C                   MOVE      SCST2         XXST
     C                   MOVE      SCZIP1        XXZIP1
     C                   MOVE      SCZIP2        XXZIP2
     C                   MOVE      XXADR         QQADR
     C*** END ADD *****  02/01/05  ************************************
     C*
     C*    Check parms for last char blank (parms longer than 30 char)
     C                   move(p)   qqmail        dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqmail
     C                   endif
     C*
     C*** end add *****  01/18/05  ****************************************
     C*
     C*  Call W7352C to submit transfer process to batch
     C                   eval       qqrnam = SCRNAM
     C                   eval       qqadd1 = SCADD1
     C                   eval       qqadd2 = SCADD2
     C                   eval       qqrrn = @@rrn
     C                   eval       qqmode = @@mode
     C                   eval       qqque = @@que
     C                   eval       qqxtap = SCXTAP
     C*** BEG ADD *** 05/11/09 ***************************************
     C                   eval       qqer = SCER
     C*** END ADD *** 05/11/09 ***************************************
     C*
     C***  begin add  ***  09/24/99  ***************************************
     C*    Check parms for last char blank (parms longer than 30 char)
     C                   move(p)   qqrnam        dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqrnam
     C                   endif
     C*
     C                   move(p)   qqadd1        dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqadd1
     C                   endif
     C*
     C                   move(p)   qqadd2        dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqadd2
     C                   endif
     C***  end   add  ***  09/24/99  ***************************************
      *** begin add ***  12/03/15  *************************************************
     C                   Move      scOldCar      QQOldCar
      *** end add *****  12/03/15  *************************************************
      *** Beg   add ***  03/11/22  *************************************************
                         qqDtaPvdr = scDtaPvdr ;
                         qqFein = %Char(scTFien);
      *** End   add ***  03/11/22  *************************************************
     C*
     C                   call      'W7352C'      tttpl
     C*
     C                   exsr      Pgmend
     C*
     C                   eval      *inlr = *on
     C                   Return

     C*============================================================================
     C*  Func_keys - Subroutine to process function keys pressed
     C*============================================================================
     C     Func_keys     begsr
     C*
     C                   if        *in11 = *on
     C*
     C                   if        *inkc = *off and *inkl = *off
     C*
     C*--> process search for device and density values?
     C*
     C                   else                                                   f3,f12 pressed
     C                   Return
     C*
     C                   endif                                                  *inkc=*off
     C*
     C                   endif                                                  *in11=*on
     C*
     C                   endsr                                                  Func_keys
     C*============================================================================
     C*  Err_chk - Subroutine to validate screen values
     C*============================================================================
     C     Err_chk       begsr
     C*
     C                   movel(p)  SCDEV         qqdev
     C                   movel(p)  '*DEVD'       qqtype
     C                   eval      qqerr = ' '
     C*
     C*  Check that device name is valid
     C                   call      'SRCHKOBJ'
     C                   parm                    qqdev
     C                   parm                    qqtype
     C                   parm                    qqerr
     C*
     C                   if        qqerr = *on
     C                   seton                                        71  90
     C                   endif
     C*
     C                   if        *in90 = *off
     C*
     C*  Check density from screen versus density table values
     C     SCDENS        lookup    dens_tbl                               70
     C*
     C                   if        *in70 = *off
     C                   seton                                        72  90
     C*
     C                   else
     C*
     C*  Call CL to initialize tape (and validate density in the process)
     C*
     C*    Display msg window to continue with transfer
     C                   exfmt     WDW01
     C*
     C                   if        *inkl=*on
     C                   seton                                            90
     C*
     C                   else
     C*
     C                   movel(p)  SCDENS        qqden
     C*
     C                   call      'W7352CA'
     C                   parm                    qqdev
     C                   parm                    qqden
     C                   parm                    qqerr
     C*
     C                   if        qqerr = *on
     C                   seton                                        73  90
     C                   endif
     C*
     C*  Check name and address fields not blank
     C                   if        *in90 = *off
     C                   if        SCRNAM=' ' or SCADD1=' ' or SCADD2=' '
     C                   seton                                        74  90
     C                   endif
     C                   endif
     C*
     C                   endif                                                  *inkl=*on
     C*
     C                   endif                                                  *in70=*off
     C*
     C                   endif                                                  *in90=*off
     C*
     C                   endsr                                                  Err_chk
     C*============================================================================
     C*  Pgmend - subroutine to perform some end of program processes
     C*============================================================================
     C     Pgmend        begsr
     C*
     C*  Protect stat submission file (wmssp) while writing to tape
     C***  begin chg ****  04/07/09  **************************************
     C*****@@rrn         chain     PSS01                              60
     C**********         eval      SSXTRC = 'Y'
     C*
     C**********         update    PSS01
     c     wsky          chain     pws01                              60
     c     *in60         doweq     *off
     c*
     c     ssky          chain     pss01                              60
     c                   if        *in60 = *off
     C                   eval      SSXTRC = 'Y'
     c                   update    pss01
     c                   endif
     c*
     c     wsky          reade     pws01                                  60
     c                   enddo
     C***  end chg ******  04/07/09  **************************************
     C*
     C*  Display 'processing submission screen'
     C************       call      'WA008R'
     C*
     C                   endsr                                                  Pgmend
     C*============================================================================
      ******* beg add ****** 06/11/07 *********************************


      *============================================================================
      *  SRDEF02 - Default SCR02 Values
      *============================================================================
     c     SRDEF02       begsr
     c                   eval      default02 = 'N'
     c*** Beg   Change - 03/11/22   ****************************************************************
     c***                eval      kkflag = 'S'
     c***  tmky          chain     ptm01

     c***                if        %found
     c***                eval      scmail = tmmail
     c***                eval      sccnam = tmname
     c***                eval      xxphn  = tmphn#
     c***                eval      scext  = tmext#
     c***                eval      xxfax  = tmfax#
     c***                eval      scstrt = tmadr1
     c***                eval      sccity = tmcty
     c***                eval      scst2  = tmst
     c***                eval      sczip  = tmzip

           Chain(n) (kkco# : WSfnd) WDF2L ;
           Chain(n) (kkco# : F2car#) WMTML1;
           If Not %Found(WMTML1);
              Chain(n) (kkco# : 0) WMTML1;
           EndIf;
           If %Found(WMTML1);
              scmail    = tmSmail ;
              sccnam    = tmSname ;
              xxphn     = tmSphn# ;
              scext     = tmSext# ;
              xxfax     = tmSfax# ;
              scstrt    = tmSadr1 ;
              sccity    = tmScty  ;
              scst2     = tmSst   ;
              sczip     = tmSzip  ;
              scDtaPvdr = tmSDType;
              scTFien   = tmSFein ;

     c*** End   Change - 03/11/22   ****************************************************************
     c                   endif

     c                   endsr                                                  Pgmend


      ******* end add ****** 06/11/07 *********************************
**Ctdata dens_tbl
*DEVTYPE  38000
*CTGTYPE  *FMT3490E
*QIC120   *FMT3570
10000     *FMT3570E
*QIC525   *FMT3590
16000     1600
*QIC10000 3200
*QIC2GB   6250
*QIC3040  *FMT2GB
*QIC5010  43200
*FMT3480  *FNT5GB
*FMT7GB
